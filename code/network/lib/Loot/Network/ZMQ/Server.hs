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
import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Concurrent.STM.TVar (modifyTVar)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.STM (retry)
import Data.ByteString (ByteString)
import Data.Default (Default (def))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Text.Show as T

import qualified System.ZMQ4 as Z

import Loot.Base.HasLens (HasLens (..), HasLens')
import Loot.Log.Internal (Logging (..), NameSelector (..), Severity (..), logNameSelL)
import Loot.Network.BiTQueue (newBtq)
import Loot.Network.Class hiding (registerListener)
import Loot.Network.Utils (whileM)
import Loot.Network.ZMQ.Common
import Loot.Network.ZMQ.Internal
import Loot.Network.ZMQ.InternalQueue

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

type ServRequestQueue = InternalQueue InternalRequest

----------------------------------------------------------------------------
-- Listeners to broker
----------------------------------------------------------------------------

type ListenersQueue = InternalQueue ZTServSendMsg

listenersToBrokerWorker :: ZTNetServEnv -> IO ()
listenersToBrokerWorker ZTNetServEnv { ztListeners, ztListenersQueue, ztServLogging} =
      forever $ (forever action) `catchAny` handler
  where
    action = do
        (results :: NE.NonEmpty ZTServSendMsg) <-
           atomically $ do
            (listenerEnvs :: [ZTListenerEnv]) <- Map.elems <$> readTVar ztListeners
            when (null listenerEnvs) retry
            atLeastOne $ map (TQ.tryReadTQueue . bSendQ) $ NE.fromList listenerEnvs
        forM_ results (iqSend ztListenersQueue)
    handler e = do
        ztLog ztServLogging Error $
            "listenersToBroker worker exited, restarting in 2s: " <> show e
        threadDelay 2000000

----------------------------------------------------------------------------
-- Methods
----------------------------------------------------------------------------

-- | Client id, as seen from the server side.
newtype ZTCliId = ZTCliId { unZtCliId :: ByteString } deriving (Eq,Ord,Show,Generic)

type ZTServSendMsg = ServSendMsg ZTCliId

type ZTListenerEnv = BiTQueue (ZTCliId, MsgType, Content) ZTServSendMsg

-- | A context for the broker, mustn't be modified manually.
data ZTNetServEnv = ZTNetServEnv
    {
      ztServFront        :: !(Z.Socket Z.Router)
      -- ^ Frontend which is talking to the outer network. Other
      -- nodes/clients connect to it and send requests.
    , ztServPub          :: !(Z.Socket Z.Pub)
      -- ^ Publishing socket. For publishing.

    , ztListeners        :: !(TVar (Map ListenerId ZTListenerEnv))
      -- ^ Information about listeners, map from id to info. Id
      -- inside info must match the map key.
    , ztListenersQueue   :: !ListenersQueue
      -- ^ Listeners' queue
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
    ztServFront <- Z.socket ztContext Z.Router
    Z.bind ztServFront $ ztNodeIdRouter ztBindOn

    ztServPub <- Z.socket ztContext Z.Pub
    Z.bind ztServPub $ ztNodeIdPub ztBindOn

    ztListeners <- newTVarIO mempty
    ztListenersQueue <- newInternalQueue ztContext
    ztMsgTypes <- newTVarIO mempty
    ztServRequestQueue <- newInternalQueue ztContext

    let modGivenName (GivenName x) = GivenName $ x <> "serv"
        modGivenName x             = x
    let ztServLogging = ztLogging & logNameSelL %~ modGivenName
    pure ZTNetServEnv {..}
  where
    ztBindOn = unLocalHost ztBindOn0
    unLocalHost nId
        | ztIdHost nId == "localhost" = nId { ztIdHost = "127.0.0.1" }
        | otherwise = nId

-- | Terminates server environment.
termNetServEnv :: MonadIO m => ZTNetServEnv -> m ()
termNetServEnv ZTNetServEnv{..} = liftIO $ do
    Z.close ztServFront
    Z.close ztServPub
    releaseInternalQueue ztListenersQueue
    releaseInternalQueue ztServRequestQueue

data ServBrokerStmRes
    = SBListener ListenerId ZTServSendMsg
    | SBFront
    | SBRequest InternalRequest
    deriving (Show)

-- | We poll sockets in @runBroker@.
-- There are three types of sockets:
-- * ROUTER socket, frontend socket where all clients connect to
-- * PAIR socket to handle internal requests to the broker
-- * PAIR socket to handle replies from listeners
data SocketType
    = FrontendSocket
    | ReqSocket
    | ListenersSocket

resolveSocketIndex :: Int -> SocketType
resolveSocketIndex i
    | i == 0 = FrontendSocket
    | i == 1 = ReqSocket
    | i == 2 = ListenersSocket
    | otherwise = error "couldn't resolve polling socket index"

runBroker :: (MonadReader r m, HasLens' r ZTNetServEnv, MonadIO m) => m ()
runBroker = do
    sEnv@ZTNetServEnv{..} <- view $ lensOf @ZTNetServEnv

    let publish k v =
            Z.sendMulti ztServPub $ NE.fromList $ [unSubscription k] ++ v

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
            (cId:"":msgT:msg) -> do
                ztEnv <- atomically $ runMaybeT $ do
                    lId <- MaybeT $ Map.lookup (MsgType msgT) <$> readTVar ztMsgTypes
                    MaybeT $ Map.lookup lId <$> readTVar ztListeners
                case ztEnv of
                  Nothing  -> ztLog ztServLogging Warning $ "frontToListener: can't resolve msgT: " <> show msgT
                  Just biQ ->
                      atomically $ TQ.writeTQueue (bReceiveQ biQ)
                                                  (ZTCliId cId, MsgType msgT, msg)
            _ -> ztLog ztServLogging Warning "frontToListener: wrong format"

    let hbWorker = forever $ do
            threadDelay $
                (fromIntegral $ zsHeartbeatsInterval ztServSettings) * 1000
            iqSend ztServRequestQueue IRHeartBeat

    liftIO $ A.withAsync hbWorker $ const $
             A.withAsync (listenersToBrokerWorker sEnv) $ const $ do

      let action = liftIO $ do
              let toPoll sock = Z.Sock sock [Z.In] Nothing
              let spdPolls =
                      [ toPoll ztServFront
                      , toPoll (iqOut ztServRequestQueue)
                      , toPoll (iqOut ztListenersQueue)
                      ]

              events <- Z.poll (-1) spdPolls
              forM_ (events `zip` [(0::Int)..]) $ \(e,i) ->
                  unless (null e) $ case resolveSocketIndex i of
                      FrontendSocket ->
                          whileM (canReceive ztServFront) $
                              Z.receiveMulti ztServFront >>= frontToListener
                      ReqSocket -> do
                          req <- iqReceive ztServRequestQueue
                          whenJust req processReq
                      ListenersSocket -> do
                          req <- iqReceive ztListenersQueue
                          whenJust req processMsg

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
    servRequestQueue <- ztServRequestQueue <$> view (lensOf @ZTNetServEnv)
    liftIO $ do
        biTQueue <- newBtq
        iqSend servRequestQueue $ IRRegister lName msgTypes biTQueue

        pure biTQueue
