{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Server-side logic.

module Loot.Network.ZMQ.Server
       ( ZTNetServEnv (..)
       , createNetServEnv

       , ZTListenerEnv
       , ServRequestQueue
       , ZTCliId (..)
       , termNetServEnv
       , runBroker
       , registerListener
       ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Concurrent.STM.TVar (modifyTVar)
import Control.Monad.Except (runExceptT, throwError)
import Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Restricted as Z
import Fmt (hexF, (+|), (+||), (|+), (||+))
import Loot.Base.HasLens (HasGetter, getterOf)
import Loot.Log (MonadLogging, logDebug, logWarning)
import qualified System.ZMQ4 as Z
import qualified Text.Show as T
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async as A

import Loot.Network.BiTQueue (newBtq)
import Loot.Network.Class hiding (registerListener)
import Loot.Network.Utils (whileM)
import Loot.Network.ZMQ.Adapter
import Loot.Network.ZMQ.Common (ZTGlobalEnv (..), ZTNodeId (..), heartbeatSubscription,
                                ztNodeConnectionId, ztNodeIdPub, ztNodeIdRouter)


----------------------------------------------------------------------------
-- Internal communication
----------------------------------------------------------------------------

data InternalRequest
    = IRRegister ListenerId (Set MsgType) ZTListenerEnv
    | IRHeartBeat

instance T.Show InternalRequest where
    show IRHeartBeat          = "IRHeartBeat"
    show (IRRegister lId _ _) = "IRRegister " <> show lId

newtype ServRequestQueue = ServRequestQueue { unServRequestQueue :: TQueue InternalRequest }

----------------------------------------------------------------------------
-- Methods
----------------------------------------------------------------------------

-- | Client id, as seen from the server side.
newtype ZTCliId = ZTCliId { unZtCliId :: ByteString } deriving (Eq,Ord,Show,Generic)

type ZTServSendMsg = ServSendMsg ZTCliId

type ZTListenerEnv = BiTQueue (ZTCliId, MsgType, Content) ZTServSendMsg

-- | A context for the broker, essentially.
data ZTNetServEnv = ZTNetServEnv
    { ztOurNodeId        :: !ZTNodeId
      -- ^ Our identifier, in case we need it to send someone
      -- explicitly (e.g. when PUBlishing).
    , ztServFront        :: !(Z.Socket Z.Router)
      -- ^ Frontend which is talking to the outer network. Other
      -- nodes/clients connect to it and send requests.
    , ztServFrontAdapter :: !SocketAdapter
      -- ^ Front socket adapter.
    , ztServPub          :: !(Z.Socket Z.Pub)
      -- ^ Publishing socket. For publishing.
    , ztServPubAdapter   :: !SocketAdapter
      -- ^ Pub socket adapter.

    , ztListeners        :: !(TVar (Map ListenerId ZTListenerEnv))
      -- ^ Information about listeners, map from id to info. Id
      -- inside info must match the map key.
    , ztMsgTypes         :: !(TVar (Map MsgType ListenerId))
      -- ^ Income message types listeners work with.

    , ztServRequestQueue :: !ServRequestQueue
      -- ^ Request queue for server.
    }

-- | Creates server environment.
createNetServEnv :: MonadIO m => ZTGlobalEnv -> ZTNodeId -> m ZTNetServEnv
createNetServEnv (ZTGlobalEnv ctx) ztOurNodeId = liftIO $ do
    ztServFront <- Z.socket ctx Z.Router
    ztServFrontAdapter <- newSocketAdapter ztServFront
    Z.setIdentity (Z.restrict $ ztNodeConnectionId ztOurNodeId) ztServFront
    Z.bind ztServFront (ztNodeIdRouter ztOurNodeId)

    ztServPub <- Z.socket ctx Z.Pub
    ztServPubAdapter <- newSocketAdapter ztServPub
    Z.bind ztServPub (ztNodeIdPub ztOurNodeId)

    ztListeners <- newTVarIO mempty
    ztMsgTypes <- newTVarIO mempty
    ztServRequestQueue <- ServRequestQueue <$> TQ.newTQueueIO

    pure ZTNetServEnv {..}

-- | Terminates server environment.
termNetServEnv :: MonadIO m => ZTNetServEnv -> m ()
termNetServEnv ZTNetServEnv{..} = liftIO $ do
    adapterRelease ztServFrontAdapter
    Z.close ztServFront
    adapterRelease ztServPubAdapter
    Z.close ztServPub

data ServBrokerStmRes
    = SBListener ListenerId ZTServSendMsg
    | SBFront
    | SBRequest InternalRequest
    deriving (Show)

runBroker
    :: ( MonadReader r m
       , HasGetter r ZTNetServEnv
       , MonadUnliftIO m
       , MonadMask m
       , MonadLogging m
       ) => m ()
runBroker = do
    ZTNetServEnv{..} <- view $ getterOf @ZTNetServEnv

    let publish :: MonadIO m => Subscription -> [ByteString] -> m ()
        publish k v =
            liftIO $ Z.sendMulti ztServPub
                   $ NE.fromList
                   $ [unSubscription k,ztNodeConnectionId ztOurNodeId] ++ v

    let processReq :: (MonadIO m, MonadLogging m) => InternalRequest -> m ()
        processReq (IRRegister listenerId msgTypes lEnv) = do
            res <- atomically $ runExceptT $ do
                listenerRegistered <- Map.member listenerId <$> lift (readTVar ztListeners)
                when listenerRegistered $ throwError "listener is already registered"

                forM_ msgTypes $ \msgT -> do
                    msgTClash <- Map.member msgT <$> lift (readTVar ztMsgTypes)
                    when msgTClash $ throwError "msgT clashes"

                lift $ modifyTVar ztListeners $ Map.insert listenerId lEnv
                forM_ msgTypes $ \msgT ->
                    lift $ modifyTVar ztMsgTypes $ Map.insert msgT listenerId

            whenLeft res $ \e -> error $ "Server IRRegister: " <> e
            logDebug $ "Registered listener "+|hexF listenerId|+""

        processReq IRHeartBeat = publish heartbeatSubscription []

    let processMsg :: MonadIO m => ServSendMsg ZTCliId -> m ()
        processMsg = \case
            Reply cId msgT msg ->
                liftIO $ Z.sendMulti ztServFront
                       $ NE.fromList
                       $ [unZtCliId cId,"",unMsgType msgT] ++ msg
            Publish k v -> publish k v

    let frontToListener :: (MonadIO m, MonadLogging m) => [ByteString] -> m ()
        frontToListener = \case
            (cId:"":msgT:msg) -> do
                ztEnv <- atomically $ runMaybeT $ do
                    lId <- MaybeT $ Map.lookup (MsgType msgT) <$> readTVar ztMsgTypes
                    MaybeT $ Map.lookup lId <$> readTVar ztListeners
                case ztEnv of
                  Nothing  -> logWarning "frontToListener: can't resolve msgT"
                  Just biQ ->
                      atomically $ TQ.writeTQueue (bReceiveQ biQ)
                                                  (ZTCliId cId, MsgType msgT, msg)
            _ -> logWarning "frontToListener: wrong format"

    let hbWorker = forever $ do
            let heartbeatInterval = 300000 -- 300 ms
            threadDelay heartbeatInterval
            atomically $ TQ.writeTQueue (unServRequestQueue ztServRequestQueue) IRHeartBeat

    A.withAsync (liftIO hbWorker) $ const $ do
        let action :: (MonadIO m, MonadLogging m) => m ()
            action = do
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
                  atLeastOne $ NE.fromList $
                      [ readReq
                      , (bool Nothing (Just SBFront)) <$> adapterTry ztServFrontAdapter ]
                                             ++ readListeners
              forM_ results $ \case
                  SBRequest r         -> processReq r
                  SBListener _lId msg -> processMsg msg
                  SBFront             -> whileM (canReceive ztServFront) $
                                             liftIO (Z.receiveMulti ztServFront) >>= frontToListener
        forever action `catchAny`
            (\e -> logWarning $ "Server broker exited: "+||e||+"")

registerListener ::
       (MonadReader r m, MonadIO m)
    => ServRequestQueue -> ListenerId -> Set MsgType -> m ZTListenerEnv
registerListener queue lName msgTypes = do
    let servRequestQueue = unServRequestQueue queue
    liftIO $ do
        biTQueue <- newBtq

        atomically $ TQ.writeTQueue servRequestQueue $ IRRegister lName msgTypes biTQueue

        pure biTQueue
