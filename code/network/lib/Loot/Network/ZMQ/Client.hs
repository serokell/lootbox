{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE UndecidableInstances #-}

-- | ZMQ Client implementation.

module Loot.Network.ZMQ.Client
    ( ZTCliSettings (..)
    , ZTNetCliEnv (..)
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
import Loot.Log.Internal (Severity (..), Logging (..), NameSelector (..), logNameSelL)
import Loot.Network.Class hiding (NetworkingCli (..), NetworkingServ (..))
import Loot.Network.Utils (TimeDurationMs (..), getCurrentTimeMs, whileM)
import Loot.Network.ZMQ.Adapter
import Loot.Network.ZMQ.Common

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

-- | Peers info variable. Peer can be either in the connected state
-- which is what you expect usually, or in a transitive "connecting"
-- state. The latter one has the timestamp when the connection was
-- tried to be established so we can abort if the connection takes too
-- long.
--
-- Also, as 'ZTInternalId's are used to index peers when sending info
-- to them, they must be all distinct as well.
data PeersInfoVar = PeersInfoVar
    { _pivConnected  :: TVar (HashMap ZTNodeId ZTInternalId)
    -- ^ Peers we're connected to.
    , _pivConnecting :: TVar (HashMap ZTNodeId (TimeDurationMs, Z.Socket Z.Dealer, SocketAdapter))
    -- ^ Argument is POSIX milliseconds representation of connection
    -- attempt time.
    }

makeLenses ''PeersInfoVar

-- TODO it's defined here b/c for performance reasons it'd be better
-- to have it in peersInfoVar too and not build from scratch every
-- time.
-- | Build a reverse peers map (for peer resolving).
peersRevMap :: PeersInfoVar -> STM (HashMap ZTInternalId ZTNodeId)
peersRevMap piv = do
    l <- map swap . HMap.toList <$> readTVar (piv ^. pivConnected)
    let res = HMap.fromList l

    when (HMap.size res /= length l) $
        error $ "peersRevMap: values collision" <> show l
    pure res

----------------------------------------------------------------------------
-- Heartbeats
----------------------------------------------------------------------------

-- Integer arithmetic is used here instead of UTCTime manipulation
-- because it is all scoped (not an external API) and also (possibly)
-- faster.

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
heartbeatWorker ZTCliSettings{..} heartbeatInfo requestQueue = forever $ do
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
    atomically $ do
        hbMap <- readTVar heartbeatInfo
        let (toReconnect,hbMap') = modMap (Map.toList hbMap)
        writeTVar heartbeatInfo hbMap'
        unless (Set.null toReconnect) $
            TQ.writeTQueue (unCliRequestQueue requestQueue) (IRReconnect toReconnect)

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

-- Get hashmap keys as a hashset.
hmKeys :: HashMap k a -> HashSet k
hmKeys = HSet.fromMap . HMap.map (const ())

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

    -- Processing
    nowConnected <- readTVar $ piv ^. pivConnected
    nowConnecting <- readTVar $ piv ^. pivConnecting
    let allConnected = HSet.toList $ hmKeys nowConnected `HSet.union` hmKeys nowConnecting
    -- We're adding all peers asked which were not connect(ed/ing)
    let toAdd = add' L.\\ allConnected

    -- And deleting all who are not connected. Another semantics would
    -- be to disconnect only those who are connected, but we also
    -- disconnect connectING peers as well.
    let toDel = del' L.\\ allConnected

    let res = (toAdd, toDel)
    when (toAdd `L.intersect` toDel /= []) $
        error $ "applyUpdatePeers: malformed response: " <> show res
    pure res

-- | Wrapper over client request queue. CLient broker listens to this.
newtype CliRequestQueue = CliRequestQueue
    { unCliRequestQueue :: TQueue InternalRequest
    }

----------------------------------------------------------------------------
-- Context
----------------------------------------------------------------------------

type ZTClientEnv = BiTQueue (ZTNodeId, CliRecvMsg) (Maybe ZTNodeId, (MsgType, Content))

-- | Client environment state. Is to be used by the one thread only
-- (main client worker).
data ZTNetCliEnv = ZTNetCliEnv
    {
      ztCliBack         :: !(Z.Socket Z.Router)
      -- ^ Backend which receives data from the network and routes it
      -- to client workers.
    , ztCliBackAdapter  :: !SocketAdapter
      -- ^ Socket adapter for backend socket.
    , ztCliSub          :: !(Z.Socket Z.Sub)
      -- ^ Subscriber socket which is listening to other nodes'
      -- updates. It also sends data to clients.
    , ztCliSubAdapter   :: !SocketAdapter
      -- ^ Socket adapter for backend socket.

    , ztPeers           :: !PeersInfoVar
      -- ^ Information about peers.
    , ztHeartbeatInfo   :: !(TVar (Map ZTNodeId HeartbeatState))
      -- ^ Information about connection and current heartbeating
      -- state.

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
    }

-- | Creates client environment.
createNetCliEnv :: MonadIO m => ZTGlobalEnv -> ZTCliSettings -> [ZTNodeId] -> m ZTNetCliEnv
createNetCliEnv globalEnv@ZTGlobalEnv{..} ztCliSettings peers = liftIO $ do
    ztCliBack <- Z.socket ztContext Z.Router
    ztCliBackAdapter <- newSocketAdapter ztCliBack
    Z.setLinger (Z.restrict (0 :: Integer)) ztCliBack

    ztCliSub <- Z.socket ztContext Z.Sub
    ztCliSubAdapter <- newSocketAdapter ztCliSub
    Z.subscribe ztCliSub (unSubscription heartbeatSubscription)

    ztClients <- newTVarIO mempty
    ztPeers <- PeersInfoVar <$> newTVarIO HMap.empty
                            <*> newTVarIO HMap.empty
    ztHeartbeatInfo <- newTVarIO mempty
    ztSubscriptions <- newTVarIO mempty
    ztMsgTypes <- newTVarIO mempty
    ztCliRequestQueue <- CliRequestQueue <$> TQ.newTQueueIO

    let modGivenName (GivenName x) = GivenName $ x <> "cli"
        modGivenName x             = x
    let ztCliLogging = ztLogging & logNameSelL %~ modGivenName
    let ztCliEnv = ZTNetCliEnv {..}

    changePeers globalEnv ztCliEnv $ def & uprAdd .~ peers

    pure ztCliEnv

-- | Terminates client environment.
termNetCliEnv :: MonadIO m => ZTNetCliEnv -> m ()
termNetCliEnv ZTNetCliEnv{..} = liftIO $ do
    adapterRelease ztCliBackAdapter
    Z.close ztCliBack
    adapterRelease ztCliSubAdapter
    Z.close ztCliSub
    -- Free the dealer sockets/adapters for peers we're currently
    -- connecting to.
    peersResources <-
        atomically $ HMap.elems <$> readTVar (ztPeers ^. pivConnecting)
    forM_ peersResources $ \(_,sock,adapter) ->
        Z.close sock >> adapterRelease adapter

-- Connects/disconnects peers at request.
changePeers :: MonadIO m => ZTGlobalEnv -> ZTNetCliEnv -> ZTUpdatePeersReq -> m ()
changePeers ZTGlobalEnv{..} ZTNetCliEnv{..} req = liftIO $ do
    curTime <- getCurrentTimeMs

    -- We pre-create sockets to use them in the STM. The number may be
    -- slightly more than the real number used (due to the possibly
    -- incorrect request), but we'll cleanup right after.
    sockets <- replicateM (length $ req ^. uprAdd) (Z.socket ztContext Z.Dealer)
    adapters <- mapM newSocketAdapter sockets

    (toConnect,toDisconnect) <- atomically $ do
        -- Process request
        (toAdd,toDel) <- applyUpdatePeers ztPeers req

        -- Modify actual variables content.
        modifyTVar (ztPeers ^. pivConnected) $! \p ->
            p `HMap.difference` HMap.fromList (map (,()) toDel)
        let addInfo = zipWith (\s a -> (curTime,s,a)) sockets adapters
        modifyTVar (ztPeers ^. pivConnecting) $! \p ->
            p `HMap.union` HMap.fromList (toAdd `zip` addInfo)

        -- Remove them from the heartbeating worker.
        modifyTVar ztHeartbeatInfo $
            foldr (.) id (map (\nId -> at nId .~ Nothing) toDel)

        -- Initialise related heartbeating context
        pure (toAdd,toDel)

    forM_ toDisconnect $ \z -> do
        Z.disconnect ztCliBack $ ztNodeIdRouter z
        Z.disconnect ztCliSub $ ztNodeIdPub z
    unless (null toDisconnect) $
        ztLog ztCliLogging Info $ "changePeers: disconnected " <> show toDisconnect
    unless (null toConnect) $ do
        -- We connect all dealer sockets and send a connection request
        -- message.
        forM_ (toConnect `zip` sockets) $ \(z,d) -> do
            Z.connect d $ ztNodeIdRouter z
            Z.sendMulti d $ NE.fromList ["",tag_getId]
        ztLog ztCliLogging Debug $
            "changePeers: preconnecting (sending req) to " <> show toConnect

    -- Release sockes that were not used.
    let dataLeft = drop (length toConnect) $ sockets `zip` adapters
    forM_ dataLeft $ \(sock,adapter) -> Z.close sock >> adapterRelease adapter

-- Continues connection request (moves peer from "connecting" to
-- "connected" state).
contConnectionReq ::
       ZTGlobalEnv -> ZTNetCliEnv -> ZTNodeId -> ZTInternalId -> IO ()
contConnectionReq ZTGlobalEnv{..} ZTNetCliEnv{..} nodeId iId = do
    curTime <- getCurrentTimeMs
    resourcesM <- atomically $ do
        clash <- HMap.member iId <$> peersRevMap ztPeers
        if clash
        then pure Nothing
        else do
            (_,sock,adapter) <-
                fromMaybe (error $ "contConnectionReq: cant find by nodeId") .
                HMap.lookup nodeId <$>
                readTVar (ztPeers ^. pivConnecting)
            modifyTVar (ztPeers ^. pivConnected) $ HMap.insert nodeId iId
            modifyTVar (ztPeers ^. pivConnecting) $ HMap.delete nodeId

            let initHbs = HeartbeatState
                    (zsHBIntervalMin ztCliSettings)
                    (zsHBLivenessMax ztCliSettings)
                    curTime
                    False
            modifyTVar ztHeartbeatInfo $ at nodeId .~ Just initHbs

            pure $ Just (sock,adapter)

    let onNothing =
            ztLog ztCliLogging Warning $
                "contConnectionReq: internal identity " <>
                "collision, didn't connect peer with iid" <> show nodeId
    let onJust (sock,adapter) = do
            Z.close sock
            adapterRelease adapter
            Z.connect ztCliBack $ ztNodeIdRouter nodeId
            Z.connect ztCliSub $ ztNodeIdPub nodeId
            ztLog ztCliLogging Debug $
                "contConnectionReq: successfully connected to " <>
                show nodeId <> ", " <> show iId
    maybe onNothing onJust resourcesM

-- Given the set of nodeIDs it disconnects them and connects them
-- back, doubling their heartbeating interval.
reconnectPeers :: MonadIO m => ZTNetCliEnv -> Set ZTNodeId -> m ()
reconnectPeers ZTNetCliEnv{..} nIds = liftIO $ do
    curTime <- getCurrentTimeMs
    toReconnect <- atomically $ do
        -- NodeIds that we really need to reconnect -- after the HB
        -- requested a reconnect some of the peers could be
        -- disconnected (for instance, at a user's request).
        allConnected <- readTVar (ztPeers ^. pivConnected)
        let toReconnect = Set.filter (`HMap.member` allConnected) nIds

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
        pure toReconnect

    unless (null toReconnect) $
        ztLog ztCliLogging Warning $ "Reconnecting peers: " <> show nIds
    forM_ toReconnect $ \nId -> do
        Z.disconnect ztCliBack (ztNodeIdRouter nId)
        Z.connect ztCliBack (ztNodeIdRouter nId)
        Z.disconnect ztCliSub (ztNodeIdPub nId)
        Z.connect ztCliSub (ztNodeIdPub nId)

-- This worker cleans up connection attempts that were stuck.
connectionsWorker :: ZTNetCliEnv -> IO ()
connectionsWorker ZTNetCliEnv{..} =
    forever $ action `catchAny` handler
  where
    action = forever $ do
        threadDelay 500000 -- 0.5 sec
        curTime <- getCurrentTimeMs
        let timeout = zsConnectingTimeout ztCliSettings
        let leaveNode (_,(created,_,_))
                | timeout == -1 = True
                | otherwise = created + timeout > curTime
        (disconnected,notConnSockets) <- atomically $ do
            prev <- readTVar (ztPeers ^. pivConnecting)
            let (stay,leave) = L.partition leaveNode (HMap.toList prev)
            writeTVar (ztPeers ^. pivConnecting) (HMap.fromList stay)
            pure (leave,map (\(_, (_,sock,_)) -> sock) stay)
        -- Sending messages until connected/disconnected. This
        -- prevents the situation when the server starts later then
        -- the client, in which server doesn't receive our first
        -- handshake request.
        forM_ notConnSockets $ \dSock ->
            Z.sendMulti dSock $ NE.fromList ["",tag_getId]
        unless (null disconnected) $ do
            ztLog ztCliLogging Warning $
                "Connecting attempt timeouted for peers: " <>
                show (map fst disconnected)
            forM_ disconnected $ \(_,(_,sock,adapter)) ->
                Z.close sock >> adapterRelease adapter
    handler e = do
        ztLog ztCliLogging Error $
            "Connections timeout worker failed: " <> show e <>
            ", restarting in 2s"
        threadDelay 2000000

----------------------------------------------------------------------------
-- Methods
----------------------------------------------------------------------------

data CliBrokerStmRes
    = CBClient ClientId (Maybe ZTNodeId) (MsgType, Content)
    | CBBack
    | CBSub
    | CBDealer ZTNodeId (Z.Socket Z.Dealer)
    | CBRequest InternalRequest

instance Show CliBrokerStmRes where
    show (CBClient _ _ _) = "CBClient"
    show CBBack           = "CBBack"
    show CBSub            = "CBSub"
    show (CBDealer _ _)   = "CBDealer"
    show (CBRequest _)    = "CBRequest"

runBroker ::
       ( MonadReader r m
       , HasLens' r ZTNetCliEnv
       , HasLens' r ZTGlobalEnv
       , MonadIO m
       , MonadMask m
       )
    => m ()
runBroker = do
    gEnv@ZTGlobalEnv{..} <- view $ lensOf @ZTGlobalEnv
    cEnv@ZTNetCliEnv{..} <- view $ lensOf @ZTNetCliEnv

    let ztCliLog = ztLog ztCliLogging

    -- This function may do something creative (LRU! Lowest ping!),
    -- but instead (for now) it'll just choose a random peer.
    let choosePeer :: IO (Maybe (ZTNodeId, ZTInternalId))
        choosePeer = do
            -- Yes, modulo bias. It shouldn't really matter.
            i <- abs <$> randomIO
            atomically $ do
                -- We only choose a peer from "connected" list.
                l <- HMap.toList <$> readTVar (ztPeers ^. pivConnected)
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

                    pure newSubs
                let newSubs = either (\e -> error $ "Client IRRegister: " <> e) identity res
                forM_ newSubs $ Z.subscribe ztCliSub . unSubscription
                ztCliLog Debug $ "Registered client " <> show clientId <> " subs " <> show newSubs

    let clientToBackend (nodeIdM :: Maybe ZTNodeId) (msgT, msg) = do

            -- Getting internal identifier of the node provided (if provided)
            iIdM <- runMaybeT $ do
                nodeId <- MaybeT $ pure nodeIdM
                MaybeT $ HMap.lookup nodeId <$>
                    atomically (readTVar (ztPeers ^. pivConnected))

            if isJust nodeIdM && isNothing iIdM
            then ztCliLog Warning $ "Coludn't send to peer, can't resolve: " <> show nodeIdM
            else do
                -- At this moment either both are nothing or both are just.

                -- Choosing at random if not provided (both are nothing).
                (iIdFinal :: Maybe ZTInternalId) <-
                    maybe (fmap snd <$> choosePeer) (pure . Just) iIdM


                -- If it's nothing it means that we couldn't choose peer
                -- at random -- the peer list is empty, so we just silently skip.
                whenJust iIdFinal $ \(unZTInternalId -> iId) ->
                    Z.sendMulti ztCliBack $ NE.fromList $
                    [iId, "", tag_normal, unMsgType msgT] ++ msg

    let routerToClients (ZTInternalId -> iId) (MsgType -> msgT) msg =
          atomically (HMap.lookup iId <$> peersRevMap ztPeers) >>= \case
            Nothing ->
                ztCliLog Warning $ "routerToClients: couldn't resolve the node"
            Just nodeId -> do
                onHeartbeat nodeId
                clientIdM <- atomically $ Map.lookup msgT <$> readTVar ztMsgTypes
                maybe (ztCliLog Warning $ "routerToClients: couldn't find client " <>
                                          "with this message type")
                      (\clientId ->
                        sendToClient clientId (nodeId, Response msgT msg))
                      clientIdM

    let subToClients (Subscription -> subK) (ZTInternalId -> iId) content =
            atomically (HMap.lookup iId <$> peersRevMap ztPeers) >>= \case
              Just nodeId -> do
                  onHeartbeat nodeId
                  unless (subK == heartbeatSubscription) $ do
                      cids <-
                          atomically $
                          fromMaybe mempty . Map.lookup subK <$>
                          readTVar ztSubscriptions
                      forM_ cids $ \clientId ->
                          sendToClient clientId (nodeId, Update subK content)
              Nothing -> ztCliLog Warning $ "subToClients: can't resolve iId" <> show iId

    -- Receiving functions

    let receiveBack =
            whileM (canReceive ztCliBack) $ Z.receiveMulti ztCliBack >>= \case
                (addr:"":msgT:msg) -> routerToClients addr msgT msg
                _ -> ztCliLog Warning $ "receiveBack: malformed message"

    let receiveSub =
          whileM (canReceive ztCliSub) $ Z.receiveMulti ztCliSub >>= \case
              (subK:iId:content) -> subToClients subK iId content
              _ -> ztCliLog Warning $ "receiveSub: malformed message"

    let receiveDealer nId d =
            -- We receive once only b/c if we succeed, this dealer
            -- socket becomes closed.
            whenM (canReceive d) $ Z.receiveMulti d >>= \case
                ["",iId] -> contConnectionReq gEnv cEnv nId (ZTInternalId iId)
                _ -> ztCliLog Warning $ "receiveDealer: malformed message"

    -- Runner
    let withWorkers action =
             A.withAsync (heartbeatWorker ztCliSettings ztHeartbeatInfo ztCliRequestQueue) $ const $
             A.withAsync (connectionsWorker cEnv) $ const $
             action

    liftIO $ withWorkers $ do
      let action = do
              results <- atomically $ do
                  cMap <- readTVar ztClients
                  connecting <-
                      HMap.toList <$> readTVar (ztPeers ^. pivConnecting)
                  let boolToMaybe t = bool Nothing (Just t)
                  let readReq =
                          fmap CBRequest <$>
                          TQ.tryReadTQueue (unCliRequestQueue ztCliRequestQueue)
                  let readClient :: [STM (Maybe CliBrokerStmRes)]
                      readClient =
                          map (\(cliId,biq) ->
                                  fmap (\(nodeIdM,content) -> CBClient cliId nodeIdM content) <$>
                                  TQ.tryReadTQueue (bSendQ biq))
                              (Map.toList cMap)
                  let readDealers :: [STM (Maybe CliBrokerStmRes)]
                      readDealers =
                          map (\(nId, (_,sock,adapter)) ->
                                boolToMaybe (CBDealer nId sock) <$>
                                adapterTry adapter)
                              connecting
                  atLeastOne $ NE.fromList $
                      [ readReq
                      , boolToMaybe CBSub <$> adapterTry ztCliSubAdapter
                      , boolToMaybe CBBack <$> adapterTry ztCliBackAdapter
                      ] ++ readClient ++ readDealers
              forM_ results $ \case
                  CBRequest r             -> processReq r
                  CBClient _cId nIdM cont -> clientToBackend nIdM cont
                  CBBack                  -> receiveBack
                  CBSub                   -> receiveSub
                  CBDealer n d            -> receiveDealer n d

      -- DSCP-177 Sockets must be processed at least once in the beginning
      -- for 'threadWaitRead' to function correctly later. This is not a
      -- solution, but a workaround -- I haven't managed to find the
      -- explanatian of why does it happen (@volhovm).
      receiveBack
      receiveSub

      forever $
          (forever action)
          `catchAny`
          (\e -> do ztCliLog Error $
                        "Client broker exited, restarting in 2s: " <> show e
                    threadDelay 2000000)

----------------------------------------------------------------------------
-- Methods
----------------------------------------------------------------------------

-- | Retrieve peers we're connected to.
getPeers ::
       (MonadReader r m, HasLens' r ZTNetCliEnv, MonadIO m) => m (Set ZTNodeId)
getPeers = do
    peersVar <- ztPeers <$> view (lensOf @ZTNetCliEnv)
    Set.fromList . HMap.keys <$> readTVarIO (peersVar ^. pivConnected)

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
