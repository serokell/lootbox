{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

-- | ZMQ Client implementation.

module Loot.Network.ZMQ.Client
    ( ZTNetCliEnv (..)
    , createNetCliEnv
    , termNetCliEnv
    , ZTClientEnv
    , CliRequestQueue
    , ZTUpdatePeersReq
    , runBroker
    , getPeers
    , registerClient
    , updatePeers
    ) where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as A
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Concurrent.STM.TVar (modifyTVar)
import Control.Lens (at, makeLenses, (-~))
import Control.Monad.Except (runExceptT, throwError)
import Data.ByteString (ByteString)
import Data.Default (def)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (randomIO)
import qualified Text.Show as T

import qualified System.ZMQ4 as Z

import Loot.Base.HasLens (HasLens (..), HasLens')
import Loot.Log.Internal (Level (..), Logging (..), logNameSelL, _GivenName)
import Loot.Network.Class hiding (NetworkingCli (..), NetworkingServ (..))
import Loot.Network.Utils (whileM)
import Loot.Network.ZMQ.Adapter
import Loot.Network.ZMQ.Common (ZTGlobalEnv (..), ZTNodeId (..), heartbeatSubscription, ztLog,
                                ztNodeConnectionIdUnsafe, ztNodeIdPub, ztNodeIdRouter)

----------------------------------------------------------------------------
-- Heartbeats
----------------------------------------------------------------------------

-- these should be configurable
hbLivenessMax, hbIntervalMin, hbIntervalMax :: Integer
hbLivenessMax = 5
hbIntervalMin = 2000
hbIntervalMax = 32000

-- I use integer arithmetic here instead of UTCTime manipulation
-- because it is all scoped (not an external API) and also (possibly)
-- faster.

data HBState = HBState
    { _hbInterval :: Integer -- ^ Milliseconds.
    , _hbLiveness :: Integer -- ^ Attempts before we reset the connection.
    , _hbNextPoll :: Integer -- ^ POSIX milliseconds, when to decrease liveliness.
    , _hbInactive :: Bool    -- ^ This flag makes node invisible to
                             -- heartbeat worker, which is needed to
                             -- deal with the fact that reconnects and
                             -- disconnects take time.
    }

makeLenses ''HBState

-- | Gets current time in POSIX ms.
getCurrentTimeMS :: IO Integer
getCurrentTimeMS = floor . (*1000) <$> getPOSIXTime

updateHeartbeat :: TVar (Map ZTNodeId HBState) -> ZTNodeId -> IO ()
updateHeartbeat states nodeId = do
    let modAction Nothing    = error "updateHeartbeat: uninitalised"
        modAction (Just hbs) = Just $ hbs & hbLiveness .~ hbLivenessMax
                                          & hbInterval .~ hbIntervalMin
    atomically $ modifyTVar states $ at nodeId %~ modAction

heartbeatWorker :: ZTNetCliEnv -> IO ()
heartbeatWorker cliEnv = do
    forever $ do
        threadDelay 50000
        curTime <- getCurrentTimeMS
        let modMap :: Map ZTNodeId HBState -> (Set ZTNodeId, Map ZTNodeId HBState)
            modMap hbMap =
                let foo :: (ZTNodeId, HBState) -> (Bool, (ZTNodeId, HBState))
                    foo x@(nodeId, hbs)
                        | not (hbs ^. hbInactive) && hbs ^. hbNextPoll < curTime =
                              if hbs ^. hbLiveness == 1
                              then (True, (nodeId, hbs & hbInactive .~ True))
                              else (False, (nodeId, hbs & hbLiveness -~ 1
                                                        & hbNextPoll .~
                                                          (curTime + hbs ^. hbInterval)))
                        | otherwise = (False, x)
                    mapped = map foo (Map.toList hbMap)
                    toReconnect = map (fst . snd) $ filter fst mapped
                in (Set.fromList toReconnect, Map.fromList $ map snd mapped)
        atomically $ do
            hbMap <- readTVar (ztHeartbeatInfo cliEnv)
            let (toReconnect,hbMap') = modMap hbMap
            writeTVar (ztHeartbeatInfo cliEnv) hbMap'
            unless (Set.null toReconnect) $
                TQ.writeTQueue (unCliRequestQueue $ ztCliRequestQueue cliEnv)
                               (IRReconnect toReconnect)

----------------------------------------------------------------------------
-- Internal requests
----------------------------------------------------------------------------

type ZTUpdatePeersReq = UpdatePeersReq ZTNodeId

data InternalRequest
    = IRUpdatePeers ZTUpdatePeersReq
    | IRRegister ClientId (Set MsgType) (Set Subscription) ZTClientEnv
    | IRReconnect (Set ZTNodeId)

instance T.Show InternalRequest where
    show (IRUpdatePeers r)      = "IRUpdatePeers: " <> show r
    show (IRRegister cId _ _ _) = "IRRegister " <> show cId
    show (IRReconnect nids)     = "IRReconnect" <> show nids

applyUpdatePeers ::
       Set ZTNodeId -> ZTUpdatePeersReq -> (Set ZTNodeId, Set ZTNodeId)
applyUpdatePeers peers (UpdatePeersReq {..}) = do
    let both = _uprDel `Set.intersection` _uprAdd
    let add' = (_uprAdd `Set.difference` both) `Set.difference` peers
    let del' = (_uprDel `Set.difference` both) `Set.intersection` peers
    (add',del')

newtype CliRequestQueue = CliRequestQueue { unCliRequestQueue :: TQueue InternalRequest }

----------------------------------------------------------------------------
-- Context
----------------------------------------------------------------------------

type ZTClientEnv = BiTQueue (ZTNodeId, CliRecvMsg) (Maybe ZTNodeId, (MsgType, Content))

-- | Client environment state. Is to be used by the one thread only
-- (main client worker).
data ZTNetCliEnv = ZTNetCliEnv
    {
      ztCliBack         :: Z.Socket Z.Router
      -- ^ Backend which receives data from the network and routes it
      -- to client workers.
    , ztCliSub          :: Z.Socket Z.Sub
      -- ^ Subscriber socket which is listening to other nodes'
      -- updates. It also sends data to clients .

    , ztPeers           :: TVar (Set ZTNodeId)
      -- ^ List of peers we are connected to. Is to be updated by the
      -- main thread only (broker worker).
    , ztHeartbeatInfo   :: TVar (Map ZTNodeId HBState)
      -- ^ Information about connection and current heartbeating
      -- state.

    , ztClients         :: TVar (Map ClientId ZTClientEnv)
      -- ^ Channels binding broker to clients, map from client name to.
    , ztSubscriptions   :: TVar (Map Subscription (Set ClientId))
      -- ^ Map from subscription key to clents' identifiers.
    , ztMsgTypes        :: TVar (Map MsgType ClientId)
      -- ^ Map from msg type key to clents' identifiers.

    , ztCliRequestQueue :: CliRequestQueue
      -- ^ Queue to read (internal, administrative) client requests
      -- from, like updating peers or resetting connection.

    , ztCliLogging      :: Logging IO
      -- ^ Logging function from global context.
    }

-- | Creates client environment.
createNetCliEnv :: MonadIO m => ZTGlobalEnv -> Set ZTNodeId -> m ZTNetCliEnv
createNetCliEnv (ZTGlobalEnv ctx ztLogging) peers = liftIO $ do
    ztCliBack <- Z.socket ctx Z.Router
    Z.setLinger (Z.restrict 0) ztCliBack
    forM_ peers $ Z.connect ztCliBack . ztNodeIdRouter

    ztCliSub <- Z.socket ctx Z.Sub
    Z.subscribe ztCliSub (unSubscription heartbeatSubscription)
    forM_ peers $ Z.connect ztCliSub . ztNodeIdPub

    ztClients <- newTVarIO mempty
    ztPeers <- newTVarIO mempty
    ztHeartbeatInfo <- newTVarIO mempty -- initialise it later
    ztSubscriptions <- newTVarIO mempty
    ztMsgTypes <- newTVarIO mempty
    ztCliRequestQueue <- CliRequestQueue <$> TQ.newTQueueIO

    let ztCliLogging = ztLogging & logNameSelL . _GivenName %~ (<> "cli")
    let cliEnv = ZTNetCliEnv {..}
    changePeers cliEnv $ def & uprAdd .~ peers
    pure cliEnv

-- | Terminates client environment.
termNetCliEnv :: MonadIO m => ZTNetCliEnv -> m ()
termNetCliEnv ZTNetCliEnv{..} = liftIO $ do
    Z.close ztCliBack
    Z.close ztCliSub

changePeers :: MonadIO m => ZTNetCliEnv -> ZTUpdatePeersReq -> m ()
changePeers ZTNetCliEnv{..} req = liftIO $ do
    curTime <- getCurrentTimeMS
    (toConnect,toDisconnect) <- atomically $ do
        peers <- readTVar ztPeers
        let (toAdd,toDel) = applyUpdatePeers peers req
        let peers' = (peers `Set.difference` toDel) `Set.union` toAdd
        writeTVar ztPeers $! peers'
        -- 2 seconds heuristical delay before heartbeating worker
        -- starts noticing these nodes.
        let initHbs = HBState hbIntervalMin hbLivenessMax (curTime + 2000) False
        modifyTVar ztHeartbeatInfo $
            foldr (.) id (map (\nId -> at nId .~ Nothing) (Set.toList toDel)) .
            foldr (.) id (map (\nId -> at nId .~ Just initHbs) (Set.toList toAdd))
        pure (toAdd,toDel)
    forM_ toDisconnect $ \z -> do
        Z.disconnect ztCliBack $ ztNodeIdRouter z
        Z.disconnect ztCliSub $ ztNodeIdPub z
    forM_ toConnect $ \z -> do
        Z.connect ztCliBack $ ztNodeIdRouter z
        Z.connect ztCliSub $ ztNodeIdPub z

reconnectPeers :: MonadIO m => ZTNetCliEnv -> Set ZTNodeId -> m ()
reconnectPeers ZTNetCliEnv{..} nIds = liftIO $ do
    ztLog ztCliLogging Info $ "Reconnecting peers: " <> show nIds

    forM_ nIds $ \nId -> do
        Z.disconnect ztCliBack (ztNodeIdRouter nId)
        Z.connect ztCliBack (ztNodeIdRouter nId)
        Z.disconnect ztCliSub (ztNodeIdPub nId)
        Z.connect ztCliSub (ztNodeIdPub nId)
    curTime <- getCurrentTimeMS
    atomically $ do
        let mulTwoMaybe x | x >= hbIntervalMax = x
                          | otherwise = 2 * x
        let upd Nothing = error "reconnectPeers: can't upd, got nothing"
            upd (Just hbs) =
                let newInterval = mulTwoMaybe (hbs ^. hbInterval)
                in Just $ hbs & hbInactive .~ False
                              & hbInterval .~ newInterval
                              & hbNextPoll .~ (curTime + newInterval)
        modifyTVar ztHeartbeatInfo $ \hbInfo ->
            foldl' (\m nId -> m & at nId %~ upd) hbInfo nIds

----------------------------------------------------------------------------
-- Methods
----------------------------------------------------------------------------

data CliBrokerStmRes
    = CBClient ClientId (Maybe ZTNodeId) (MsgType, Content)
    | CBBack
    | CBSub
    | CBRequest InternalRequest deriving Show

runBroker :: (MonadReader r m, HasLens' r ZTNetCliEnv, MonadIO m, MonadMask m) => m ()
runBroker = do
    cEnv@ZTNetCliEnv{..} <- view $ lensOf @ZTNetCliEnv

    let ztCliLog = ztLog ztCliLogging
    -- This function may do something creative (LRU! Lowest ping!),
    -- but instead (for now) it'll just choose a random peer.
    let choosePeer = do
            -- Yes, modulo bias. It shouldn't really matter.
            i <- abs <$> randomIO
            atomically $ do
                l <- Set.toList <$> readTVar ztPeers
                pure $ l L.!! (i `mod` length l)

    let resolvePeer :: MonadIO m => ByteString -> m (Maybe ZTNodeId)
        resolvePeer nodeid = do
            atomically $ find ((== nodeid) . ztNodeConnectionIdUnsafe) <$> readTVar ztPeers

    let sendToClient clientId (nId :: ZTNodeId, content :: CliRecvMsg) = do
            res <- atomically $ do
                cMap <- readTVar ztClients
                let toWrite = bReceiveQ <$> Map.lookup clientId cMap
                whenJust toWrite $ \tq -> TQ.writeTQueue tq (nId,content)
                pure $ isJust toWrite
            unless res $ ztCliLog Warning $ "sendToClient: cId doesn't exist: " <> show clientId

    let onHeartbeat addr = liftIO $ updateHeartbeat ztHeartbeatInfo addr

    let processReq = \case
            IRUpdatePeers req -> changePeers cEnv req
            IRReconnect nIds -> reconnectPeers cEnv nIds
            IRRegister clientId msgTs subs biQ -> do
                res <- atomically $ runExceptT $ do
                    -- check predicates
                    clientRegistered <- Map.member clientId <$> lift (readTVar ztClients)
                    when clientRegistered $ throwError "client is already registered"

                    forM_ msgTs $ \msgT -> do
                        msgTClash <- Map.member msgT <$> lift (readTVar ztMsgTypes)
                        when msgTClash $ throwError "msgT clashes"

                    lift $ modifyTVar ztClients $ Map.insert clientId biQ
                    forM_ msgTs $ \msgT ->
                        lift $ modifyTVar ztMsgTypes $ Map.insert msgT clientId

                    -- subscriptions
                    currentSubs <- Set.fromList . Map.keys <$> lift (readTVar ztSubscriptions)
                    let newSubs = subs `Set.difference` currentSubs
                    lift $ modifyTVar ztSubscriptions $ \s ->
                        let insertClientId Nothing          = Just (Set.singleton clientId)
                            insertClientId (Just clientIds) = Just $ Set.insert clientId clientIds
                        in foldl' (\prevMap sub -> Map.alter insertClientId sub prevMap) s subs

                    pure newSubs
                case res of
                    Left e        -> error $ "Client IRRegister: " <> e
                    Right newSubs -> forM_ newSubs $ Z.subscribe ztCliSub . unSubscription
                ztCliLog Debug $ "Registered client " <> show clientId

    let clientToBackend (nodeIdM :: Maybe ZTNodeId) (msgT, msg) = do
            nodeId <- maybe choosePeer pure nodeIdM

            -- this warning can be removed if speed is crucial
            present <- atomically $ Set.member nodeId <$> readTVar ztPeers
            unless present $ do
                ztCliLog Warning $ "Sending message with type " <> show msgT <>
                                   ", but client " <> show nodeId <> " is not our peer"

            Z.sendMulti ztCliBack $ NE.fromList $
                [ztNodeConnectionIdUnsafe nodeId, "", unMsgType msgT] ++ msg

    let backendToClients = \case
            (addr:"":msgT:msg) -> resolvePeer addr >>= \case
                Nothing -> ztCliLog Warning $ "client btc: couldn't resolve peer: " <> show addr
                Just nodeId -> do
                    onHeartbeat nodeId
                    clientIdM <-
                        atomically $ Map.lookup (MsgType msgT) <$> readTVar ztMsgTypes
                    maybe (ztCliLog Warning $ "backendToClients: couldn't find client " <>
                                              "with this message type")
                          (\clientId -> sendToClient clientId (nodeId, Response (MsgType msgT) msg))
                          clientIdM
            other -> ztCliLog Warning $ "backendToClients: wrong format: " <> show other

    let subToClients = \case
            (k:addr:content) -> resolvePeer addr >>= \case
                Nothing -> ztCliLog Warning $ "subToClients: couldn't resolve peer: " <> show addr
                Just nodeId -> do
                    let k' = Subscription k
                    onHeartbeat nodeId
                    unless (k' == heartbeatSubscription) $ do
                        cids <-
                            atomically $
                            fromMaybe mempty . Map.lookup k' <$>
                            readTVar ztSubscriptions
                        forM_ cids $ \clientId ->
                          sendToClient clientId (nodeId, Update k' content)
                        when (null cids) $
                            -- It shouldn't be alright, since it means
                            -- that our clients records are broken
                            -- (we're subscribed to something no
                            -- client needs).
                            error $ "subToClients: Nobody got the subscription " <>
                                    "message for key " <> show k
            other -> ztCliLog Warning $ "subToClients: wrong format: " <> show other

    hbWorker <- liftIO $ A.async $ heartbeatWorker cEnv
    (_, backStmTry, backDestroy) <- socketWaitReadSTMLong ztCliBack
    (_, subStmTry, subDestroy) <- socketWaitReadSTMLong ztCliSub
    let action = liftIO $ do
            results <- atomically $ do
                cMap <- readTVar ztClients
                let readReq =
                        fmap CBRequest <$>
                        TQ.tryReadTQueue (unCliRequestQueue ztCliRequestQueue)
                let readClient :: [STM (Maybe CliBrokerStmRes)]
                    readClient =
                        map (\(cliId,biq) ->
                                fmap (\(nodeIdM,content) -> CBClient cliId nodeIdM content) <$>
                                TQ.tryReadTQueue (bSendQ biq))
                            (Map.toList cMap)
                let boolToMaybe t = bool Nothing (Just t)
                atLeastOne $ NE.fromList $ [ readReq
                                           , boolToMaybe CBBack <$> backStmTry
                                           , boolToMaybe CBSub <$> subStmTry ] ++ readClient
            forM_ results $ \case
                CBRequest r             -> processReq r
                CBClient _cId nIdM cont -> clientToBackend nIdM cont
                CBBack                  -> whileM (canReceive ztCliBack) $
                                           Z.receiveMulti ztCliBack >>= backendToClients
                CBSub                   -> whileM (canReceive ztCliSub) $
                                           Z.receiveMulti ztCliSub >>= subToClients
    forever action `finally` (backDestroy >> subDestroy >> liftIO (A.cancel hbWorker))

----------------------------------------------------------------------------
-- Methods
----------------------------------------------------------------------------

-- | Retrieve peers we're connected to.
getPeers ::
       (MonadReader r m, HasLens' r ZTNetCliEnv, MonadIO m) => m (Set ZTNodeId)
getPeers = readTVarIO =<< (ztPeers <$> view (lensOf @ZTNetCliEnv))

-- | Register a new client.
registerClient ::
       (MonadReader r m, MonadIO m)
    => CliRequestQueue
    -> ClientId
    -> Set MsgType
    -> Set Subscription
    -> m ZTClientEnv
registerClient (unCliRequestQueue -> cliRequestQueue) clientId msgTs subs = liftIO $ do
    biTQueue <- BiTQueue <$> TQ.newTQueueIO <*> TQ.newTQueueIO
    atomically $ TQ.writeTQueue cliRequestQueue $ IRRegister clientId msgTs subs biTQueue
    pure biTQueue

-- | Updates peers.
updatePeers ::
       (MonadReader r m, MonadIO m)
    => CliRequestQueue
    -> ZTUpdatePeersReq
    -> m ()
updatePeers (unCliRequestQueue -> cliRequestQueue) req =
    atomically $ TQ.writeTQueue cliRequestQueue $ IRUpdatePeers req
