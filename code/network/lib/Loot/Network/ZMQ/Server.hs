{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Server-side logic.

module Loot.Network.ZMQ.Server
       ( ZTCliId(..)
       , ZTNetServEnv
       , createNetServEnv
       , runBroker
       , registerListener
       ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async as A
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Concurrent.STM.TVar (modifyTVar)
import Control.Lens (lens)
import Control.Monad.Except (runExceptT, throwError)
import Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

import qualified Data.Restricted as Z
import qualified System.ZMQ4 as Z

import Loot.Network.Class hiding (registerListener)
import Loot.Network.Utils (HasLens (..), HasLens', whileM)
import Loot.Network.ZMQ.Adapter
import Loot.Network.ZMQ.Common (ZTGlobalEnv (..), ZTNodeId (..), heartbeatSubscription,
                                ztNodeConnectionId, ztNodeIdPub, ztNodeIdRouter)


----------------------------------------------------------------------------
-- Internal communication
----------------------------------------------------------------------------

data InternalRequest
    = IRRegister ListenerId (Set MsgType) ZTListenerEnv
    | IRHeartBeat

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

instance HasLens ZTNetServEnv r ZTNetServEnv =>
         HasLens ServRequestQueue r ServRequestQueue where
    lensOf =
        (lensOf @ZTNetServEnv) .
        (lens ztServRequestQueue (\ztce rq2 -> ztce {ztServRequestQueue = rq2}))

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

    let publish k v =
            Z.sendMulti ztServPub $
            NE.fromList $ [k,ztNodeConnectionId ztOurNodeId] ++ v

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
        processReq IRHeartBeat = publish heartbeatSubscription []

    let processMsg = \case
            Reply cId msgT msg ->
                Z.sendMulti ztServFront $
                NE.fromList $ [unZtCliId cId,"",msgT] ++ msg
            Publish k v -> publish k v

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

    hbWorker <- liftIO $ A.async $ forever $ do
        let heartbeatInterval = 300000 -- 300 ms
        threadDelay heartbeatInterval
        atomically $ TQ.writeTQueue (unServRequestQueue ztServRequestQueue) IRHeartBeat

    (_, frontStmTry, frontDestroy) <- socketWaitReadSTMLong ztServFront
    -- TODO add heartbeating trigger worker sending new type SBRequest.
    let action = liftIO $ do
            results <- atomically $ do
                lMap <- readTVar ztListeners
                let readReq =
                        fmap SBRequest <$>
                        TQ.tryReadTQueue (unServRequestQueue ztServRequestQueue)
                let readListeners :: [STM (Maybe ServBrokerStmRes)]
                    readListeners =
                        map (\(listId,biq) ->
                               fmap (\content -> SBListener listId content) <$>
                               TQ.tryReadTQueue (bSendQ biq))
                            (Map.toList lMap)
                atLeastOne $ NE.fromList $ [ readReq
                                           , (bool Nothing (Just SBFront)) <$> frontStmTry ]
                                           ++ readListeners
            forM_ results $ \case
                SBRequest r         -> processReq r
                SBListener _lId msg -> processMsg msg
                SBFront             -> whileM (canReceive ztServFront) $
                                       Z.receiveMulti ztServFront >>= frontToListener
    forever action `finally` (frontDestroy >> liftIO (A.cancel hbWorker))

registerListener ::
       (MonadReader r m, HasLens' r ServRequestQueue, MonadIO m)
    => ListenerId -> Set MsgType -> m ZTListenerEnv
registerListener lName msgTypes = do
    servRequestQueue <- unServRequestQueue <$> view (lensOf @ServRequestQueue)
    liftIO $ do
        biTQueue <- BiTQueue <$> TQ.newTQueueIO <*> TQ.newTQueueIO

        atomically $ TQ.writeTQueue servRequestQueue $ IRRegister lName msgTypes biTQueue
        pure biTQueue
