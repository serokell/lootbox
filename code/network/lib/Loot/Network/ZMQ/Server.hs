{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Server-side logic.

module Loot.Network.ZMQ.Server
       ( ZTServSettings (..)
       , ZTNetServEnv
       , createNetServEnv

       , ZTListenerEnv
       , ServRequestQueue
       , ZTCliId (..)
       , termNetServEnv
       , runBroker
       , registerListener
       ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async as A
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Concurrent.STM.TVar (modifyTVar)
import Control.Monad.Except (runExceptT, throwError)
import Data.ByteString (ByteString)
import Data.Default (Default (def))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Text.Show as T

import qualified Data.Restricted as Z
import qualified System.ZMQ4 as Z

import Loot.Base.HasLens (HasLens (..), HasLens')
import Loot.Log.Internal (Severity (..), Logging (..), NameSelector (..), logNameSelL)
import Loot.Network.BiTQueue (newBtq)
import Loot.Network.Class hiding (registerListener)
import Loot.Network.Utils (whileM)
import Loot.Network.ZMQ.Adapter
import Loot.Network.ZMQ.Common


----------------------------------------------------------------------------
-- Server side settings
----------------------------------------------------------------------------

-- | Server configuration options.
data ZTServSettings = ZTServSettings
    { zsHeartbeatsInterval  :: !Integer
      -- ^ How much time to wait between sending heartbeats, in ms.
    }

instance Default ZTServSettings where
    def = ZTServSettings {zsHeartbeatsInterval = 300}

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

-- | A context for the broker, mustn't be modified manually.
data ZTNetServEnv = ZTNetServEnv
    { ztOurNodeId        :: !ZTInternalId
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

    , ztServLogging      :: !(Logging IO)
      -- ^ Logging function from global context.

    , ztServSettings     :: !ZTServSettings
      -- ^ Server settings.
    }

-- | Creates server environment. Accepts only the host/ports to bind
-- on, with the exceptin that "localhost" is turned into "127.0.0.1".
createNetServEnv :: MonadIO m => ZTGlobalEnv -> ZTServSettings -> ZTNodeId -> m ZTNetServEnv
createNetServEnv ZTGlobalEnv{..} ztServSettings ztBindOn0 = liftIO $ do
    ztOurNodeId <- randomZTInternalId

    ztServFront <- Z.socket ztContext Z.Router
    Z.setIdentity (Z.restrict $ unZTInternalId ztOurNodeId) ztServFront
    Z.bind ztServFront $ ztNodeIdRouter ztBindOn
    ztServFrontAdapter <- newSocketAdapter ztServFront

    ztServPub <- Z.socket ztContext Z.Pub
    Z.bind ztServPub $ ztNodeIdPub ztBindOn
    ztServPubAdapter <- newSocketAdapter ztServPub

    ztListeners <- newTVarIO mempty
    ztMsgTypes <- newTVarIO mempty
    ztServRequestQueue <- ServRequestQueue <$> TQ.newTQueueIO

    let modGivenName (GivenName x) = GivenName $ x <> "serv"
        modGivenName x             = x
    let ztServLogging = ztLogging & logNameSelL %~ modGivenName
    ztLog ztServLogging Debug $ "Set identity: " <> show ztOurNodeId
    pure ZTNetServEnv {..}
  where
    ztBindOn = unLocalHost ztBindOn0
    unLocalHost nId
        | ztIdHost nId == "localhost" = nId { ztIdHost = "127.0.0.1" }
        | otherwise = nId

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


runBroker :: (MonadReader r m, HasLens' r ZTNetServEnv, MonadIO m, MonadMask m) => m ()
runBroker = do
    ZTNetServEnv{..} <- view $ lensOf @ZTNetServEnv

    let publish k v = do
            --ztLog ztServLogging Debug "Publishing"
            Z.sendMulti ztServPub $
              NE.fromList $ [unSubscription k,unZTInternalId ztOurNodeId] ++ v
            --ztLog ztServLogging Debug "Published"

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

            whenLeft res $ \e -> error $ "Server IRRegister: " <> e
            ztLog ztServLogging Debug $ "Registered listener " <> show listenerId

        processReq IRHeartBeat = publish heartbeatSubscription []

    let processMsg = \case
            Reply cId msgT msg ->
                Z.sendMulti ztServFront $
                NE.fromList $ [unZtCliId cId,"",unMsgType msgT] ++ msg
            Publish k v -> publish k v

    let frontToListener = \case
            [cId,"",t] | t == tag_getId -> do
                Z.sendMulti ztServFront $
                  NE.fromList [cId,"",unZTInternalId ztOurNodeId]
                ztLog ztServLogging Debug "Received request connection, replied with our id"
            (cId:"":t:msgT:msg) | t == tag_normal -> do
                ztEnv <- atomically $ runMaybeT $ do
                    lId <- MaybeT $ Map.lookup (MsgType msgT) <$> readTVar ztMsgTypes
                    MaybeT $ Map.lookup lId <$> readTVar ztListeners
                case ztEnv of
                  Nothing  -> ztLog ztServLogging Warning "frontToListener: can't resolve msgT"
                  Just biQ ->
                      atomically $ TQ.writeTQueue (bReceiveQ biQ)
                                                  (ZTCliId cId, MsgType msgT, msg)
            _ -> ztLog ztServLogging Warning "frontToListener: wrong format"

    let hbWorker = forever $ do
            threadDelay $
                (fromIntegral $ zsHeartbeatsInterval ztServSettings) * 1000
            atomically $
                TQ.writeTQueue (unServRequestQueue ztServRequestQueue) IRHeartBeat

    liftIO $ A.withAsync hbWorker $ const $ do
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
                  atLeastOne $ NE.fromList $
                      [ readReq
                      , (bool Nothing (Just SBFront)) <$> adapterTry ztServFrontAdapter ]
                                             ++ readListeners
              forM_ results $ \case
                  SBRequest r         -> processReq r
                  SBListener _lId msg -> processMsg msg
                  SBFront             -> whileM (canReceive ztServFront) $
                                         Z.receiveMulti ztServFront >>= frontToListener

      forever $
          (forever action)
          `catchAny`
          (\e -> do ztLog ztServLogging Error $
                        "Server broker exited, restarting in 2s: " <> show e
                    threadDelay 2000000)

registerListener ::
       (MonadReader r m, HasLens' r ZTNetServEnv, MonadIO m)
    => ListenerId -> Set MsgType -> m ZTListenerEnv
registerListener lName msgTypes = do
    queue <- ztServRequestQueue <$> view (lensOf @ZTNetServEnv)
    let servRequestQueue = unServRequestQueue queue
    liftIO $ do
        biTQueue <- newBtq

        atomically $ TQ.writeTQueue servRequestQueue $ IRRegister lName msgTypes biTQueue

        pure biTQueue
