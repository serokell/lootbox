{-# LANGUAGE UndecidableInstances #-}

-- | ZMQ Client implementation.

module Loot.Network.ZMQ.Client
    ( ZTCliSettings (..)
    , ZTNetCliEnv (..)
    , createNetCliEnv
    , termNetCliEnv
    , withNetCliEnv
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
import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Concurrent.STM.TVar (modifyTVar)
import qualified Control.Exception.Safe as E
import Control.Lens (at, makeLenses, (-~))
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.STM (retry)
import Data.Default (Default (def))
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Random (randomIO)
import qualified Text.Show as T

import qualified System.ZMQ4 as Z

import Loot.Base.HasLens (HasLens (..), HasLens')
import Loot.Log.Internal (Logging (..), NameSelector (..), Severity (..), logNameSelL)
import Loot.Network.Class hiding (NetworkingCli (..), NetworkingServ (..))
import Loot.Network.Utils (TimeDurationMs (..), getCurrentTimeMs, whileM)
import Loot.Network.ZMQ.Common
import Loot.Network.ZMQ.Internal
import Loot.Network.ZMQ.InternalQueue

----------------------------------------------------------------------------
-- Settings
----------------------------------------------------------------------------

-- | Client configuration options.
data ZTCliSettings = ZTCliSettings
    { zsHBLivenessMax     :: !Integer
      -- ^ Heartbeating maximum liveliness.
    , zsHBIntervalMin     :: !TimeDurationMs
      -- ^ Heartbeating minimum time interval (the one we start from),
      -- in ms.
    , zsHBIntervalMax     :: !TimeDurationMs
      -- ^ Heartbeating maximum interval (the one we end up with --
      -- after exponential backoff).
    , zsConnectingTimeout :: !TimeDurationMs
      -- ^ Connecting timeout in ms. We'll stop trying to connect to
      -- the node after this amount of time. -1 means "no timeout".
    }

instance Default ZTCliSettings where
    def =
        ZTCliSettings
            { zsHBLivenessMax = 5
            , zsHBIntervalMin = 2000
            , zsHBIntervalMax = 32000
            , zsConnectingTimeout = -1
            }

----------------------------------------------------------------------------
-- Manipulating peers
----------------------------------------------------------------------------

-- | Peer resources: the pair of sockets -- DEALER to send/receive
-- requests, SUB to receive updates. And their adapters.
data PeerResource = PeerResource
    { prBack :: !(Z.Socket Z.Dealer)
      -- ^ Backend which receives data from the network and routes it
      -- to client workers.
    , prSub  :: !(Z.Socket Z.Sub)
      -- ^ Subscriber socket which is listening to other nodes'
      -- updates. It also sends data to clients.
    }

-- | Initialise peer resources.
newPeerResource :: Z.Context -> IO PeerResource
newPeerResource ztContext = do
    prBack <- Z.socket ztContext Z.Dealer
    Z.setLinger (Z.restrict (0 :: Integer)) prBack

    prSub <- Z.socket ztContext Z.Sub
    Z.subscribe prSub (unSubscription heartbeatSubscription)

    pure PeerResource{..}

-- | Release the sockets and adapters of PeerResource.
releasePeerResource :: PeerResource -> IO ()
releasePeerResource PeerResource{..} = do
    Z.close prBack
    Z.close prSub

-- | Peers info variable. Each peer is mapped to its resources.
type PeersInfoVar = TVar (HashMap ZTNodeId PeerResource)

----------------------------------------------------------------------------
-- Heartbeats
----------------------------------------------------------------------------

data HeartbeatState = HeartbeatState
    { _hbInterval :: TimeDurationMs
       -- ^ Heartbeating interval.
    , _hbLiveness :: Integer
       -- ^ Attempts before we reset the connection.
    , _hbNextPoll :: TimeDurationMs
       -- ^ Timestamp: when to decrease liveliness.
    , _hbInactive :: Bool
       -- ^ This flag makes node invisible to heartbeat worker, which
       -- is needed to deal with the fact that reconnects and
       -- disconnects take time.
    }

makeLenses ''HeartbeatState

updateHeartbeat ::
       ZTCliSettings -> TVar (Map ZTNodeId HeartbeatState) -> ZTNodeId -> IO ()
updateHeartbeat ZTCliSettings{..} states nodeId = do
    let modAction Nothing    = error "updateHeartbeat: uninitalised"
        modAction (Just hbs) = Just $ hbs & hbLiveness .~ zsHBLivenessMax
                                          & hbInterval .~ zsHBIntervalMin
    atomically $ modifyTVar states $ at nodeId %~ modAction

-- Heartbeating worker. It regularly checks whether we had proper ping
-- messages from others, otherwise asks broker to reconnect these
-- peers. It decreases liveness every interval. When it becomes zero,
-- it asks broker to reconnect the node, the broker will then double
-- the interval.
heartbeatWorker ::
       ZTCliSettings -> TVar (Map ZTNodeId HeartbeatState) -> CliRequestQueue -> IO ()
heartbeatWorker ZTCliSettings{..} heartbeatInfo internalQueue = forever $ do
    threadDelay 50000
    curTime <- getCurrentTimeMs
    let modMap :: [(ZTNodeId, HeartbeatState)]
               -> (Set ZTNodeId, Map ZTNodeId HeartbeatState)
        modMap hbMap =
            -- Takes nodeId and its hbstate. First element of the
            -- result is whether we should reconnect the node (which
            -- happens if liveness is going to become 0).
            let checkPollTime :: (ZTNodeId, HeartbeatState)
                              -> (Bool, (ZTNodeId, HeartbeatState))
                checkPollTime x@(nodeId, hbs)
                    -- We ignore inactive nodes and active nodes for
                    -- which poll time is not timeouted.
                    | hbs ^. hbInactive ||
                      hbs ^. hbNextPoll > curTime = (False, x)
                    | otherwise =
                          if hbs ^. hbLiveness == 1
                          then (True, (nodeId, hbs & hbInactive .~ True))
                          else (False, (nodeId, hbs & hbLiveness -~ 1
                                                    & hbNextPoll .~
                                                      (curTime + hbs ^. hbInterval)))
                (toReconnect,toLeave) =
                    bimap (map snd) (map snd) $
                    L.partition fst $ map checkPollTime hbMap
            in (Set.fromList (map fst toReconnect), Map.fromList toLeave)
    toReconnect <- atomically $ do
        hbMap <- readTVar heartbeatInfo
        let (toReconnect,hbMap') = modMap (Map.toList hbMap)
        writeTVar heartbeatInfo hbMap'
        pure toReconnect
    unless (Set.null toReconnect) $
        iqSend internalQueue (IRReconnect toReconnect)

----------------------------------------------------------------------------
-- Internal queue requests
----------------------------------------------------------------------------

type ZTUpdatePeersReq = UpdatePeersReq ZTNodeId

data InternalRequest
    = IRUpdatePeers ZTUpdatePeersReq
    | IRRegister ClientId (Set MsgType) (Set Subscription) ZTClientEnv
    | IRReconnect (Set ZTNodeId)
    deriving (Generic)

instance T.Show InternalRequest where
    show (IRUpdatePeers r)      = "IRUpdatePeers: " <> show r
    show (IRRegister cId _ _ _) = "IRRegister " <> show cId
    show (IRReconnect nids)     = "IRReconnect" <> show nids

type CliRequestQueue = InternalQueue InternalRequest

----------------------------------------------------------------------------
-- Polling data
----------------------------------------------------------------------------

data CliPollData = CliPollData
    { cpdPolls      :: ![Z.Poll Z.Socket IO]
    , cpdN          :: !Int
    , cpdResolveMap :: !(HashMap Int (ZTNodeId, PeerResource))
    }

-- | We poll sockets from @cpdPolls@.
-- There are four types of sockets:
-- * DEALER socket for each connected peer
-- * SUB socket for each connected peer
-- * PAIR socket to handle requests from client workers
-- * PAIR socket to handle internal requests to the broker
-- Each consructor corresponds to one of these types.
data SocketType
    = BackendSocket (ZTNodeId, Z.Socket Z.Dealer)
    | SubSocket (ZTNodeId, Z.Socket Z.Sub)
    | CliSocket
    | ReqSocket

resolveSocketIndex ::
    Int -> CliPollData -> SocketType
resolveSocketIndex i CliPollData{..}
    | i < cpdN          = BackendSocket $ prBack <$> resolve i
    | i < 2 * cpdN      = SubSocket $ prSub <$> resolve (i - cpdN)
    | i == 2 * cpdN     = CliSocket
    | i == 2 * cpdN + 1 = ReqSocket
    | otherwise         = error "couldn't resolve polling socket index"
  where
    resolve ix =
      fromMaybe (error $ "lookupUnsafe " <> show (ix,cpdN)) $
      HMap.lookup ix cpdResolveMap

recreateCliPollData ::
       PeersInfoVar -> CliRequestQueue -> ClientsQueue -> STM CliPollData
recreateCliPollData piv reqQueue cQueue = do
    peersInfo <- HMap.toList <$> readTVar piv
    let cpdResolveMap :: HashMap Int (ZTNodeId, PeerResource)
        cpdResolveMap = HMap.fromList $ [0..] `zip` peersInfo
    let cpdN = HMap.size cpdResolveMap
    let resources = map snd peersInfo
    let toPoll sock = Z.Sock sock [Z.In] Nothing
    let backPolls = map (toPoll . prBack) resources
    let subPolls = map (toPoll . prSub) resources
    let cliPolls = [toPoll (iqOut cQueue)]
    let reqPolls = [toPoll (iqOut reqQueue)]
    let cpdPolls = backPolls ++ subPolls ++ cliPolls ++ reqPolls
    pure CliPollData{..}

-- | This queue is used to transfer messages from clients to the main broker.
type ClientsQueue = InternalQueue (Maybe ZTNodeId, (MsgType, Content))

-- | This worker reads from clients' bitqueues and forwards these
-- messages to one socket that is then processed by broker in one big
-- zmq poll.
clientsToBrokerWorker :: ZTNetCliEnv -> IO ()
clientsToBrokerWorker ZTNetCliEnv { ztClientsQueue, ztClients, ztCliLogging } =
      forever $ (forever action) `catchAny` handler
  where
    action = do
        (results :: NE.NonEmpty (Maybe ZTNodeId, (MsgType, Content))) <-
           atomically $ do
            (clientEnvs :: [ZTClientEnv]) <- Map.elems <$> readTVar ztClients
            when (null clientEnvs) retry
            atLeastOne $ map (TQ.tryReadTQueue . bSendQ) $ NE.fromList clientEnvs
        forM_ results (iqSend ztClientsQueue)
    handler e = do
        ztLog ztCliLogging Error $
            "clientsToBroker worker exited, restarting in 2s: " <> show e
        threadDelay 2000000

----------------------------------------------------------------------------
-- Context
----------------------------------------------------------------------------

type ZTClientEnv = BiTQueue (ZTNodeId, CliRecvMsg) (Maybe ZTNodeId, (MsgType, Content))


-- | Client environment state. Is to be used by the one thread only
-- (main client worker).
data ZTNetCliEnv = ZTNetCliEnv
    {
      ztPeers           :: !PeersInfoVar
      -- ^ Peers and their resources.
    , ztCliPollData     :: !(TVar CliPollData)
      -- ^ Data needed for speeding up the polling process.
    , ztHeartbeatInfo   :: !(TVar (Map ZTNodeId HeartbeatState))
      -- ^ Information about connection and current heartbeating
      -- state.
    , ztClientsQueue    :: !ClientsQueue
      -- ^ Clients queue.
    , ztClients         :: !(TVar (Map ClientId ZTClientEnv))
      -- ^ Channels binding broker to clients, map from client name to.
    , ztSubscriptions   :: !(TVar (Map Subscription (Set ClientId)))
      -- ^ Map from subscription key to clents' identifiers.
    , ztMsgTypes        :: !(TVar (Map MsgType ClientId))
      -- ^ Map from msg type key to clents' identifiers.

    , ztCliRequestQueue :: !CliRequestQueue
      -- ^ Queue to read (internal, administrative) client requests
      -- from, like updating peers or resetting connection.

    , ztCliLogging      :: !(Logging IO)
      -- ^ Logging function from global context.
    , ztCliSettings     :: !ZTCliSettings
      -- ^ Client settings.
    , ztCliTerminated   :: !(MVar ())
      -- ^ Broker running flag, empty if broker is running.
    }

-- | Creates client environment.
createNetCliEnv :: MonadIO m => ZTGlobalEnv -> ZTCliSettings -> [ZTNodeId] -> m ZTNetCliEnv
createNetCliEnv globalEnv@ZTGlobalEnv{..} ztCliSettings peers = liftIO $ do
    ztClients <- newTVarIO mempty
    ztPeers <- newTVarIO mempty
    ztHeartbeatInfo <- newTVarIO mempty
    ztClientsQueue <- newInternalQueue ztContext
    ztSubscriptions <- newTVarIO mempty
    ztMsgTypes <- newTVarIO mempty
    ztCliRequestQueue <- newInternalQueue ztContext
    ztCliPollData <- newTVarIO $ CliPollData mempty 0 mempty
    ztCliTerminated <- liftIO $ newMVar ()

    let modGivenName (GivenName x) = GivenName $ x <> "cli"
        modGivenName x             = x
    let ztCliLogging = ztLogging & logNameSelL %~ modGivenName
    let ztCliEnv = ZTNetCliEnv {..}

    changePeers globalEnv ztCliEnv $ def & uprAdd .~ peers

    pure ztCliEnv

-- | Terminates client environment.
termNetCliEnv :: MonadIO m => ZTNetCliEnv -> m ()
termNetCliEnv ZTNetCliEnv{..} = liftIO $ do
    -- Wait until broker exits.
    () <- takeMVar ztCliTerminated

    -- Free the dealer sockets/adapters for peers we're currently
    -- connecting to.
    peersResources <- atomically $ HMap.elems <$> readTVar ztPeers
    forM_ peersResources releasePeerResource

    releaseInternalQueue ztClientsQueue
    releaseInternalQueue ztCliRequestQueue

withNetCliEnv ::
       (MonadIO m, MonadMask m)
    => ZTGlobalEnv
    -> ZTCliSettings
    -> [ZTNodeId]
    -> (ZTNetCliEnv -> m a)
    -> m a
withNetCliEnv global settings peers action =
    bracket (createNetCliEnv global settings peers) termNetCliEnv action

-- Connects/disconnects peers at request.
changePeers :: MonadIO m => ZTGlobalEnv -> ZTNetCliEnv -> ZTUpdatePeersReq -> m ()
changePeers ZTGlobalEnv{..} ZTNetCliEnv{..} req = liftIO $ do
    curTime <- getCurrentTimeMs

    -- We pre-create resources to use them in the STM. The number may
    -- be slightly more than the real number used (due to the possibly
    -- incorrect request trying to connect already connected peers),
    -- but we'll cleanup right after. We do it because we want to
    -- process everything in one atomically block (with one view over
    -- current peers).
    resources <- replicateM (length $ req ^. uprAdd) (newPeerResource ztContext)

    (toConnect,toRelease,allSubs) <- atomically $ do
        -- Process request.
        (toAdd,toDel) <- applyUpdatePeers ztPeers req

        (toRelease :: [(ZTNodeId,PeerResource)]) <-
            (\p -> mapMaybe (\d -> (d,) <$> HMap.lookup d p) toDel) <$> readTVar ztPeers
        let (toAddFinal :: HashMap ZTNodeId PeerResource) =
                HMap.fromList (toAdd `zip` resources)

        -- Modify actual variables content.
        modifyTVar ztPeers $! \p ->
            p `HMap.difference` HMap.fromList (map (,()) toDel)
        modifyTVar ztPeers $ (`HMap.union` toAddFinal)
        -- Remove them from the heartbeating worker.

        -- Update heartbeat worker state
        let initHbs = HeartbeatState
                (zsHBIntervalMin ztCliSettings)
                (zsHBLivenessMax ztCliSettings)
                curTime
                False
        modifyTVar ztHeartbeatInfo $
            foldr (.) id (map (\nId -> at nId .~ Nothing) toDel) .
            foldr (.) id (map (\nId -> at nId .~ Just initHbs) toAdd)

        allSubscriptions <- Map.keys <$> readTVar ztSubscriptions

        -- Update polling data
        newPollData <-
            recreateCliPollData ztPeers ztCliRequestQueue ztClientsQueue
        writeTVar ztCliPollData newPollData

        pure (toAdd,toRelease,allSubscriptions)

    forM_ toRelease $ \(_,res) -> releasePeerResource res
    unless (null toRelease) $
        ztLog ztCliLogging Info $ "changePeers: disconnected " <> show (map fst toRelease)

    unless (null toConnect) $ do
        -- We connect all dealer sockets and send a connection request
        -- message.
        forM_ (toConnect `zip` resources) $ \(z,PeerResource{..}) -> do
            Z.connect prBack $ ztNodeIdRouter z
            Z.connect prSub $ ztNodeIdPub z
            forM_ allSubs $ \(Subscription s) -> Z.subscribe prSub s
        ztLog ztCliLogging Debug $
            "changePeers: connecting to " <> show toConnect

    -- Release resources that were not used.
    let resourcesLeft = drop (length toConnect) resources
    forM_ resourcesLeft releasePeerResource
  where
    -- Reformulates peers update request in terms of current peer info
    -- var, returning the set of add/del nodes. Doesn't change PIV itself.
    applyUpdatePeers ::
           PeersInfoVar
        -> ZTUpdatePeersReq
        -> STM ([ZTNodeId], [ZTNodeId])
    applyUpdatePeers piv UpdatePeersReq{..} = do
        -- Normalising the request
        let both = L.nub $ _uprDel `L.intersect` _uprAdd
        let add' = L.nub _uprAdd L.\\ both
        let del' = L.nub _uprDel L.\\ both

        -- Get hashmap keys as a hashset.
        let hmKeys :: HashMap k a -> HashSet k
            hmKeys = HSet.fromMap . HMap.map (const ())

        currentPeers <- HSet.toList . hmKeys <$> readTVar piv

        -- We're adding all peers asked which were not connected.
        -- And deleting all who are not connected.
        let toAdd = add' L.\\ currentPeers
        let toDel = del' `L.intersect` currentPeers
        let res = (toAdd, toDel)
        when (toAdd `L.intersect` toDel /= []) $
            error $ "applyUpdatePeers: malformed response: " <> show res
        pure res

-- A part of heartbeating routine.
-- Given the set of nodeIDs it disconnects them and connects them
-- back, doubling their heartbeating interval.
reconnectPeers :: MonadIO m => ZTNetCliEnv -> Set ZTNodeId -> m ()
reconnectPeers ZTNetCliEnv{..} nIds = liftIO $ do
    curTime <- getCurrentTimeMs
    toReconnect <- atomically $ do
        -- NodeIds that we really need to reconnect -- after the HB
        -- requested a reconnect some of the peers could be
        -- disconnected (for instance, at a user's request).
        allConnected <- readTVar ztPeers
        let toReconnect = Set.filter (`HMap.member` allConnected) nIds
        let resources =
                mapMaybe (\n -> (n,) <$> HMap.lookup n allConnected) $
                Set.toList toReconnect

        -- Multiply by two if we don't exceed the max limit, otherwise
        -- set to the max limit.
        let mulTwoMaybe x
                | 2 * x > zsHBIntervalMax ztCliSettings =
                  zsHBIntervalMax ztCliSettings
                | otherwise = 2 * x
        let upd Nothing = error "reconnectPeers: can't upd, got nothing"
            upd (Just hbs) =
                let newInterval = mulTwoMaybe (hbs ^. hbInterval)
                in Just $ hbs & hbInactive .~ False
                              & hbInterval .~ newInterval
                              & hbNextPoll .~ (curTime + newInterval)

        modifyTVar ztHeartbeatInfo $ \hbInfo ->
            foldl' (\m nId -> m & at nId %~ upd) hbInfo toReconnect
        pure resources

    unless (null toReconnect) $
        ztLog ztCliLogging Warning $ "Reconnecting peers: " <> show nIds
    forM_ toReconnect $ \(nId, PeerResource{..}) -> do
        Z.disconnect prBack (ztNodeIdRouter nId)
        Z.connect prBack (ztNodeIdRouter nId)
        Z.disconnect prSub (ztNodeIdPub nId)
        Z.connect prSub (ztNodeIdPub nId)

----------------------------------------------------------------------------
-- Methods
----------------------------------------------------------------------------

data CliBrokerStmRes
    = CBClient ClientId (Maybe ZTNodeId) (MsgType, Content)
    | CBBack (Z.Socket Z.Dealer) ZTNodeId
    | CBSub (Z.Socket Z.Sub) ZTNodeId
    | CBRequest InternalRequest

instance Show CliBrokerStmRes where
    show (CBClient _ _ _) = "CBClient"
    show (CBBack _ _)     = "CBBack"
    show (CBSub _ _)      = "CBSub"
    show (CBRequest _)    = "CBRequest"

runBroker ::
       ( MonadReader r m
       , HasLens' r ZTNetCliEnv
       , HasLens' r ZTGlobalEnv
       , MonadIO m
       )
    => m ()
runBroker = do
    gEnv@ZTGlobalEnv{..} <- view $ lensOf @ZTGlobalEnv
    cEnv@ZTNetCliEnv{..} <- view $ lensOf @ZTNetCliEnv

    let ztCliLog = ztLog ztCliLogging

    -- This function may do something creative (LRU! Lowest ping!),
    -- but instead (for now) it'll just choose a random peer.
    let choosePeer :: IO (Maybe ZTNodeId)
        choosePeer = do
            -- Yes, modulo bias. It shouldn't really matter.
            i <- abs <$> randomIO
            atomically $ do
                -- We only choose a peer from "connected" list.
                l <- HMap.keys <$> readTVar ztPeers
                pure $ case l of
                    [] -> Nothing
                    _  -> Just $ l L.!! (i `mod` length l)

    let sendToClient clientId (nId :: ZTNodeId, content :: CliRecvMsg) = do
            res <- atomically $ do
                cMap <- readTVar ztClients
                let toWrite = bReceiveQ <$> Map.lookup clientId cMap
                whenJust toWrite $ \tq -> TQ.writeTQueue tq (nId,content)
                pure $ isJust toWrite
            unless res $ ztCliLog Warning $ "sendToClient: cId doesn't exist: " <> show clientId

    let onHeartbeat addr = liftIO $ updateHeartbeat ztCliSettings ztHeartbeatInfo addr

    let processReq = \case
            IRUpdatePeers req -> changePeers gEnv cEnv req
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

                    allSubSockets <-
                        lift $ (map prSub) . HMap.elems <$> readTVar ztPeers

                    pure (newSubs,allSubSockets)
                let (newSubs,allSubSockets) =
                        either (\e -> error $ "Client IRRegister: " <> e) identity res
                forM_ allSubSockets $ \subSocket ->
                    forM_ newSubs $ Z.subscribe subSocket . unSubscription
                ztCliLog Debug $ "Registered client " <> show clientId <> " subs " <> show newSubs

    let clientToBackend (nodeIdM :: Maybe ZTNodeId) (msgT, msg) = do

            -- Getting internal identifier of the node provided (if provided)
            nodeId' <- maybe choosePeer (pure . Just) nodeIdM
            whenJust nodeId' $ \nodeId ->
                atomically (HMap.lookup nodeId <$> readTVar ztPeers) >>= \case
                    Nothing ->
                        ztCliLog Warning $
                        "Coludn't send to peer, can't resolve: " <> show nodeId
                    Just PeerResource{ prBack } -> do
                        Z.sendMulti prBack $ NE.fromList $
                            ["", unMsgType msgT] ++ msg

    let dealersToClients nodeId (MsgType -> msgT) msg = do
            onHeartbeat nodeId
            clientIdM <- atomically $ Map.lookup msgT <$> readTVar ztMsgTypes
            maybe (ztCliLog Warning $ "dealersToClients: couldn't find client " <>
                                      "with this message type")
                  (\clientId ->
                    sendToClient clientId (nodeId, Response msgT msg))
                  clientIdM

    let subToClients nodeId (Subscription -> subK) content = do
            onHeartbeat nodeId
            unless (subK == heartbeatSubscription) $ do
                cids <-
                    atomically $
                    fromMaybe mempty . Map.lookup subK <$>
                    readTVar ztSubscriptions
                forM_ cids $ \clientId ->
                    sendToClient clientId (nodeId, Update subK content)

    -- Receiving functions

    let receiveBack back nodeId =
            whileM (canReceive back) $ Z.receiveMulti back >>= \case
                ("":msgT:msg) -> dealersToClients nodeId msgT msg
                _ -> ztCliLog Warning $
                    "receiveBack: malformed message from " <> show nodeId

    let receiveSub sub nodeId =
            whileM (canReceive sub) $ Z.receiveMulti sub >>= \case
                (subK:content) -> subToClients nodeId subK content
                _ -> ztCliLog Warning $
                    "receiveSub: malformed message from " <> show nodeId

    -- Runner
    let withWorkers action =
             A.withAsync (heartbeatWorker ztCliSettings ztHeartbeatInfo ztCliRequestQueue) $ const $
             A.withAsync (clientsToBrokerWorker cEnv) $ const $
             action

    let pollAndProcess = do
            cpd@CliPollData{..} <- atomically $ readTVar ztCliPollData
            events <- Z.poll (-1) cpdPolls
            forM_ (events `zip` [0..]) $ \(e,i) ->
                unless (null e) $ case resolveSocketIndex i cpd of
                    BackendSocket (peerId, backSock) -> receiveBack backSock peerId
                    SubSocket (peerId, subSock)      -> receiveSub subSock peerId
                    CliSocket -> do
                        req <- iqReceive ztClientsQueue
                        whenJust req $ uncurry clientToBackend
                    ReqSocket -> do
                        req <- iqReceive ztCliRequestQueue
                        whenJust req processReq

    let runAll = do
            x <- tryTakeMVar ztCliTerminated
            whenNothing x $
                error $ "Couldn't start client broker: " <>
                        "it's either already running, or not initialised"
            withWorkers $ forever $
                (forever pollAndProcess)
                `catchAny`
                (\e -> do ztCliLog Error $
                              "Client broker exited, restarting in 2s: " <> show e
                          threadDelay 2000000)

    liftIO $ runAll `finally` (tryPutMVar ztCliTerminated ())

----------------------------------------------------------------------------
-- Methods
----------------------------------------------------------------------------

-- | Retrieve peers we're connected to.
getPeers ::
       (MonadReader r m, HasLens' r ZTNetCliEnv, MonadIO m) => m (Set ZTNodeId)
getPeers = do
    peersVar <- ztPeers <$> view (lensOf @ZTNetCliEnv)
    Set.fromList . HMap.keys <$> readTVarIO peersVar

-- | Register a new client.
registerClient ::
       (MonadIO m)
    => CliRequestQueue
    -> ClientId
    -> Set MsgType
    -> Set Subscription
    -> m ZTClientEnv
registerClient internalQueue clientId msgTs subs = liftIO $ do
    biTQueue <- BiTQueue <$> TQ.newTQueueIO <*> TQ.newTQueueIO
    iqSend internalQueue $ IRRegister clientId msgTs subs biTQueue
    pure biTQueue

-- | Updates peers.
updatePeers ::
       (MonadIO m)
    => CliRequestQueue
    -> ZTUpdatePeersReq
    -> m ()
updatePeers internalQueue req =
    liftIO $ iqSend internalQueue $ IRUpdatePeers req
