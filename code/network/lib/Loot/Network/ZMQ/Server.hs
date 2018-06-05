{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Server-side logic.

module Loot.Network.ZMQ.Server () where
{-

    ( ZTNetServEnv
    , createNetServEnv
    , ZTCliId
    , ZTListenerEnv
    , regListener
    , lAccept
    , lRespond
    , lPublish
    ) where
-}


import Codec.Serialise (serialise)
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Concurrent.STM.TVar (modifyTVar)
import Control.Lens (at, makeLenses, _Just)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.STM (retry)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

import qualified Data.Restricted as Z
import qualified System.ZMQ4 as Z

import Loot.Network.Class
import Loot.Network.Utils (HasLens (..), HasLens', whileM)
import Loot.Network.ZMQ.Adapter
import Loot.Network.ZMQ.Common (ZTGlobalEnv (..), ZTNodeId (..), ZmqTcp, ztContext,
                                ztNodeConnectionId, ztNodeIdPub, ztNodeIdRouter)


----------------------------------------------------------------------------
-- Internal communication
----------------------------------------------------------------------------

data InternalRequest =
    IRRegister ListenerId [MsgType] ZTListenerEnv

newtype ServRequestQueue = ServRequestQueue { unServRequestQueue :: TQueue InternalRequest }

----------------------------------------------------------------------------
-- Methods
----------------------------------------------------------------------------

-- | Client id, as seen from the server side.
data ZTCliId = ZTCliId { unZtCliId :: ByteString }

type ZTServSendMsg = ServSendMsg ZTCliId

type ZTListenerEnv = BiTQueue (ZTCliId, MsgType, Content) ZTServSendMsg

-- | A context for the broker, essentially.
data ZTNetServEnv = ZTNetServEnv
    { ztOurNodeId        :: ZTNodeId
      -- ^ Our identifier, in case we need it to send someone
      -- explicitly (e.g. when PUBlishing).
    , ztServFront        :: Z.Socket Z.Router
      -- ^ Frontend which is talking to the outer network. Other
      -- nodes/clients connect to it and send requests.
    , ztServPub          :: Z.Socket Z.Pub
      -- ^ Publishing socket. For publishing.

    , ztListeners        :: TVar (Map ListenerId ZTListenerEnv)
      -- ^ Information about listeners, map from id to info. Id
      -- inside info must match the map key.
    , ztMsgTypes         :: TVar (Map MsgType ListenerId)
      -- ^ Income message types listeners work with.

    , ztServRequestQueue :: ServRequestQueue
      -- ^ Request queue for server.
    }

createNetServEnv :: MonadIO m => ZTGlobalEnv -> ZTNodeId -> m ZTNetServEnv
createNetServEnv (ZTGlobalEnv ctx) ztOurNodeId = liftIO $ do
    ztServFront <- Z.socket ctx Z.Router
    Z.setIdentity (Z.restrict $ ztNodeConnectionId ztOurNodeId) ztServFront
    Z.bind ztServFront (ztNodeIdRouter ztOurNodeId)

    ztServPub <- Z.socket ctx Z.Pub
    Z.bind ztServPub (ztNodeIdPub ztOurNodeId)

    ztListeners <- newTVarIO mempty
    ztMsgTypes <- newTVarIO mempty
    ztServRequestQueue <- ServRequestQueue <$> TQ.newTQueueIO

    pure ZTNetServEnv {..}

data ServBrokerStmRes
    = SBListener ListenerId ZTServSendMsg
    | SBFront
    | SBRequest InternalRequest

runBroker :: (MonadReader r m, HasLens' r ZTNetServEnv, MonadIO m, MonadMask m) => m ()
runBroker = do
    ZTNetServEnv{..} <- view $ lensOf @ZTNetServEnv

    let processReq (IRRegister listenerId msgTypes lEnv) = do
            res <- atomically $ runExceptT $ do
                listenerRegistered <- Map.member listenerId <$> lift (readTVar ztListeners)
                when listenerRegistered $ throwError "listener is already registered"

                forM_ msgTypes $ \msgT -> do
                    msgTClash <- Map.member msgT <$> lift (readTVar ztMsgTypes)
                    when msgTClash $ throwError "msgT clashes"

                lift $ modifyTVar ztListeners $ Map.insert listenerId lEnv
                forM_ msgTypes $ \msgT ->
                    lift $ modifyTVar ztMsgTypes $ Map.insert msgT listenerId

            either (\e -> error $ "Server IRRegister: " <> e) (const pass) res

    let processMsg = \case
            Reply cId msgT msg ->
                Z.sendMulti ztServFront $
                NE.fromList $ [unZtCliId cId,"",msgT] ++ msg
            Publish k v ->
                Z.sendMulti ztServPub $
                NE.fromList $ [k,ztNodeConnectionId ztOurNodeId] ++ v

    let frontToListener = \case
            (cId:"":msgT:msg) -> do
                ztEnv <- atomically $ runMaybeT $ do
                    lId <- MaybeT $ Map.lookup msgT <$> readTVar ztMsgTypes
                    MaybeT $ Map.lookup lId <$> readTVar ztListeners
                case ztEnv of
                  Nothing  -> putText "frontToListener: can't resolve msgT"
                  Just biQ ->
                      atomically $ TQ.writeTQueue (bReceiveQ biQ) (ZTCliId cId, msgT, msg)
            _ -> putText "frontToListener: wrong format"

    (frontStm, frontDestroy) <- socketWaitReadSTMLong ztServFront
    -- TODO add heartbeating trigger action (or worker?).
    let action = liftIO $ do
            res <- atomically $ do
                lMap <- readTVar ztListeners
                let readReq = SBRequest <$> TQ.readTQueue (unServRequestQueue ztServRequestQueue)
                let readListener =
                        map (\(listId,biq) -> do content <- TQ.readTQueue (bSendQ biq)
                                                 pure $ SBListener listId content)
                            (Map.toList lMap)
                orElseMulti $ NE.fromList $ [ readReq
                                            , SBFront <$ frontStm ] ++ readListener
            case res of
                SBRequest r         -> processReq r
                SBListener _lId msg -> processMsg msg
                SBFront             -> whileM (canReceive ztServFront) $
                                       Z.receiveMulti ztServFront >>= frontToListener
    forever action `finally` frontDestroy

{-
-- | Listener's environment. What is _actually_ needed is REP with
-- unrestricted send/receive pattern and one peer (bucket)
-- connected. So DEALER suits well.
data ZTListenerEnv = ZTListenerEnv { ztListSock :: Z.Socket Z.Dealer }

regListener ::
       (MonadReader r m, HasLens' r ZTGlobalEnv, MonadIO m)
    => ListenerId -> ByteString -> m ZTListenerEnv
regListener lName msgType = do
    ctx <- view $ lensOf @ZTGlobalEnv . ztContext
    liftIO $ do
        lSocket <- Z.socket ctx Z.Dealer
        let lName' = fromMaybe (error $ "regList: lname is malformed: " <> show lName) $
                     Z.toRestricted lName
        Z.setIdentity lName' lSocket
        Z.connect lSocket endpointBrokerServer

        Z.sendMulti lSocket $ NE.fromList $ ["","register",msgType]
        Z.receiveMulti lSocket >>= \case
            ["","registered"] -> pass
            other -> error $ "registerClient: malformed reply from broker: " <> show other

        pure $ ZTListenerEnv lSocket

-- | Accept on a listener side
lAccept :: MonadIO m => ZTListenerEnv -> Bool -> m (ZTCliId, Content)
lAccept (ZTListenerEnv {..}) marker = liftIO $ do
    when marker $ Z.sendMulti ztListSock $ NE.fromList $ ["", "ready"]
    Z.receiveMulti ztListSock >>= \case
        "":clientId:"":msg -> pure (ZTCliId clientId, msg)
        other -> error $ "onRequest: unknown format: " <> show other

lRespond :: MonadIO m => ZTListenerEnv -> ZTCliId -> Content -> m ()
lRespond (ZTListenerEnv {..}) (ZTCliId clientId) msg =
    liftIO $ Z.sendMulti ztListSock $ NE.fromList $
    ["", "send", clientId, ""] ++ msg

lPublish :: MonadIO m => ZTListenerEnv -> (ByteString,Content) -> m ()
lPublish (ZTListenerEnv {..}) (k,content) =
    liftIO $ Z.sendMulti ztListSock $ NE.fromList $
    ["", "pub", k] ++ content

-- This class should be moved somewhere else or
-- disappear. Constrainted methods allow us to provide instance for
-- any class -- abstracted, transformer, etc. They also allow us to
-- create caps. So Server.hs should provide methods only, not instances.
instance ( MonadReader r m
         , HasLens' r ZTGlobalEnv
         , HasLens' r ZTNetServEnv
         , MonadIO m) => NetworkingServ ZmqTcp m where

    type CliId ZmqTcp = ZTCliId

    runServer = runBroker

    type ListenerEnv ZmqTcp = ZTListenerEnv

    registerListener = regListener
    accept = lAccept
    respond = lRespond
    publish = lPublish

-}
