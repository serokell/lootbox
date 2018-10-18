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
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Random (randomIO)
import qualified Text.Show as T

import qualified System.ZMQ4 as Z

import Loot.Base.HasLens (HasLens (..), HasLens')
import Loot.Log.Internal (Level (..), Logging (..), NameSelector (..), logNameSelL)
import Loot.Network.Class hiding (NetworkingCli (..), NetworkingServ (..))
import Loot.Network.Utils (whileM)
import Loot.Network.ZMQ.Adapter
import Loot.Network.ZMQ.Common


----------------------------------------------------------------------------
-- Manipulating peers
----------------------------------------------------------------------------

-- | Peers info variable, shared between client and server. It
-- represents a single united map, so an implicit predicate is that
-- the same node id can't be the key in more than one field of the
-- record.
--
-- Peer can be either in the connected state which is what you
-- expect usually, or in a transitive "connecting" state. The latter
-- one has the tag when the connection was tried to be established so
-- we can abort if the connection takes too long.
--
-- Also, as 'ZTInternalId's are used to index peers when sending info
-- to them, they must be all distinct as well.
data PeersInfoVar = PeersInfoVar
    { _pivConnected  :: TVar (HashMap ZTNodeId ZTInternalId)
    -- ^ Peers we're connected to.
    , _pivConnecting :: TVar (HashMap ZTNodeId (Integer, Z.Socket Z.Dealer, SocketAdapter))
    -- ^ Argument is POSIX representation of connection attempt time.
    }

makeLenses ''PeersInfoVar

-- | Returns the hashset of peers we send data to.
peersToSend :: PeersInfoVar -> STM (HashSet ZTInternalId)
peersToSend piv = do
    plist <- HMap.elems <$> readTVar (piv ^. pivConnected)
    let p = HSet.fromList plist
    when (HSet.size p /= length plist) $
        error $ "peersToSend: values collision: " <> show plist
    pure p

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

-- | Get hashmap keys as a hashset.
hmKeys :: HashMap k a -> HashSet k
hmKeys = HSet.fromMap . fmap (const ())

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
      -- updates. It also sends data to clients .
    , ztCliSubAdapter   :: !SocketAdapter
      -- ^ Socket adapter for backend socket.

    , ztPeers           :: !PeersInfoVar
      -- ^ Information about peers.
    , ztHeartbeatInfo   :: !(TVar (Map ZTNodeId HBState))
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
    }

-- | Creates client environment.
createNetCliEnv :: MonadIO m => ZTGlobalEnv -> [ZTNodeId] -> m ZTNetCliEnv
createNetCliEnv ztGlobalEnv@ZTGlobalEnv{..} peers = liftIO $ do
    ztCliBack <- Z.socket ztContext Z.Router
    ztCliBackAdapter <- newSocketAdapter ztCliBack
    Z.setLinger (Z.restrict (0 :: Integer)) ztCliBack

    ztCliSub <- Z.socket ztContext Z.Sub
    ztCliSubAdapter <- newSocketAdapter ztCliSub
    Z.subscribe ztCliSub (unSubscription heartbeatSubscription)

    ztClients <- newTVarIO mempty
    ztPeers <- PeersInfoVar <$> newTVarIO HMap.empty
                            <*> newTVarIO HMap.empty
    ztHeartbeatInfo <- newTVarIO mempty -- initialise it later
    ztSubscriptions <- newTVarIO mempty
    ztMsgTypes <- newTVarIO mempty
    ztCliRequestQueue <- CliRequestQueue <$> TQ.newTQueueIO

    let modGivenName (GivenName x) = GivenName $ x <> "cli"
        modGivenName x             = x
    let ztCliLogging = ztLogging & logNameSelL %~ modGivenName
    let ztCliEnv = ZTNetCliEnv {..}

    changePeers ztGlobalEnv ztCliEnv $ def & uprAdd .~ peers

    pure ztCliEnv

-- | Terminates client environment.
termNetCliEnv :: MonadIO m => ZTNetCliEnv -> m ()
termNetCliEnv ZTNetCliEnv{..} = liftIO $ do
    adapterRelease ztCliBackAdapter
    Z.close ztCliBack
    adapterRelease ztCliSubAdapter
    Z.close ztCliSub

changePeers :: MonadIO m => ZTGlobalEnv -> ZTNetCliEnv -> ZTUpdatePeersReq -> m ()
changePeers ZTGlobalEnv{..} ZTNetCliEnv{..} req = liftIO $ do
    curTime <- getCurrentTimeMS

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
        let addInfo = map (\(s,a) -> (curTime,s,a)) $ sockets `zip` adapters
        modifyTVar (ztPeers ^. pivConnecting) $! \p ->
            p `HMap.union` HMap.fromList (toAdd `zip` addInfo)

        -- Remove them from the heartbeating worker.
        modifyTVar ztHeartbeatInfo $
            foldr (.) id (map (\nId -> at nId .~ Nothing) toDel)

        -- Initialise related heartbeating context
        pure (toAdd,toDel)

    forM_ toDisconnect $ \z -> Z.disconnect ztCliBack $ ztNodeIdRouter z
    unless (null toDisconnect) $
        ztLog ztCliLogging Info $ "changePeers: disconnected " <> show toDisconnect
    unless (null toConnect) $ do
        -- We connect all dealer sockets and send a connection request
        -- message.
        forM_ (toConnect `zip` sockets) $ \(z,d) -> do
            Z.connect ztCliSub $ ztNodeIdRouter z
            Z.connect d $ ztNodeIdRouter z
            Z.sendMulti d $ NE.fromList ["","getid"]
        ztLog ztCliLogging Debug $
            "changePeers: preconnecting (sending req) to " <> show toConnect

    -- Release sockes that were not used.
    let dataLeft = drop (length toConnect) $ sockets `zip` adapters
    forM_ dataLeft $ \(sock,adapter) -> Z.close sock >> adapterRelease adapter

-- | Continue connection request.
contConnectionReq :: ZTGlobalEnv -> ZTNetCliEnv -> ZTNodeId -> ZTInternalId -> IO ()
contConnectionReq ZTGlobalEnv{..} ZTNetCliEnv{..} nodeId iId = do
    curTime <- getCurrentTimeMS
    resourcesM <- atomically $ do
        connected <- HMap.toList <$> readTVar (ztPeers ^. pivConnected)
        let clash = isJust $ find (\(_,iId') -> iId' == iId) connected
        if clash
        then pure Nothing
        else do
            (_,sock,adapter) <-
                fromMaybe (error $ "contConnectionReq: cant find by nodeId") .
                HMap.lookup nodeId <$>
                readTVar (ztPeers ^. pivConnecting)
            modifyTVar (ztPeers ^. pivConnected) $ HMap.insert nodeId iId
            modifyTVar (ztPeers ^. pivConnecting) $ HMap.delete nodeId

            -- 2 seconds heuristical delay before heartbeating worker
            -- starts noticing these nodes.
            let initHbs = HBState hbIntervalMin hbLivenessMax (curTime + 2000) False
            modifyTVar ztHeartbeatInfo $ at nodeId .~ Just initHbs

            pure $ Just (sock,adapter)

    let onNothing =
            ztLog ztCliLogging Warning $
                "contConnectionReq: internal identity " <>
                "collision, didn't connect peer with iid" <> show nodeId
    let onJust (sock,adapter) = do
            Z.close sock
            adapterRelease adapter
            ztLog ztCliLogging Debug $
                "contConnectionReq: successfully connected to :" <> show nodeId
    maybe onNothing onJust resourcesM

-- | Given the set of nodeIDs it disconnects them and connects them
-- back, doubling their heartbeating interval.
reconnectPeers :: MonadIO m => ZTNetCliEnv -> Set ZTNodeId -> m ()
reconnectPeers ZTNetCliEnv{..} nIds = liftIO $ do
    ztLog ztCliLogging Warning $ "Reconnecting peers: " <> show nIds

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

-- | This worker cleans up connection attempts that were stuck.
connectionsTimeoutWorker :: Logging IO -> PeersInfoVar -> IO ()
connectionsTimeoutWorker logging piv =
    forever $ action `catchAny` handler
  where
    -- TODO this must be configurable.
    connectionTimeoutMS = 10000000
    action = forever $ do
        threadDelay 50000
        curTime <- getCurrentTimeMS
        let predicate (_,(created,_,_)) = created + connectionTimeoutMS < curTime
        disconnected <- atomically $ do
            prev <- readTVar (piv ^. pivConnecting)
            let (stay,leave) = L.partition predicate (HMap.toList prev)
            writeTVar (piv ^. pivConnecting) (HMap.fromList stay)
            pure leave
        unless (null disconnected) $ do
            ztLog logging Warning $
                "Connecting attemp timeouted for peers: " <>
                show (map fst disconnected)
            forM_ disconnected $ \(_,(_,sock,adapter)) ->
                Z.close sock >> adapterRelease adapter
    handler e = do
        ztLog logging Error $
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
    | CBRequest InternalRequest deriving Show

runBroker :: (MonadReader r m, HasLens' r ZTNetCliEnv, MonadIO m, MonadMask m) => m ()
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
                let newSubs = either (\e -> error $ "Client IRRegister: " <> e) identity res
                forM_ newSubs $ Z.subscribe ztCliSub . unSubscription
                ztCliLog Debug $ "Registered client " <> show clientId <> " subs " <> show newSubs

    let clientToBackend (nodeIdM :: Maybe ZTNodeId) (msgT, msg) = do
            (nodeIdFinal :: Maybe ZTNodeId) <-
                maybe choosePeer (pure . Just) nodeIdM

            whenJust nodeIdFinal $ \nodeId -> do
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

    liftIO $ A.withAsync (heartbeatWorker cEnv) $ const $ do
      let receiveBack = whileM (canReceive ztCliBack) $
                        Z.receiveMulti ztCliBack >>= backendToClients
      let receiveSub  = whileM (canReceive ztCliSub) $
                        Z.receiveMulti ztCliSub >>= subToClients
      let action = do
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
                  atLeastOne $ NE.fromList $
                      [ readReq
                      , boolToMaybe CBSub <$> adapterTry ztCliSubAdapter
                      , boolToMaybe CBBack <$> adapterTry ztCliBackAdapter
                      ] ++ readClient
              forM_ results $ \case
                  CBRequest r             -> processReq r
                  CBClient _cId nIdM cont -> clientToBackend nIdM cont
                  CBBack                  -> receiveBack
                  CBSub                   -> receiveSub

      -- DSCP-177 Sockets must be processed at least once in the beginning
      -- for 'threadWaitRead' to function correctly later. This is not a
      -- solution, but a workaround -- I haven't managed to find the
      -- explanatian of why does it happen (@volhovm).
      receiveBack
      receiveSub

      forever action `catchAny`
          (\e -> ztCliLog Warning $ "Client broker exited: " <> show e)

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
