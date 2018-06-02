{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- method naming is HORRIBLE, i hope to fix it later
-- | ZMQ Client implementation.

module Loot.Network.ZMQ.Client where
--    ( ZTNetCliEnv
--    , createNetCliEnv
--    , ZTClientEnv
--    , runBroker
--    , ztGetPeers
--    , updPeers
--    , regClient
--    , cSend
--    , cBroadcast
--    , cReceive
--    ) where

import Codec.Serialise (Serialise, deserialise, deserialiseOrFail, serialise)
import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Concurrent.STM.TVar (modifyTVar)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Random (randomIO)

import qualified Data.Restricted as Z
import qualified System.ZMQ4 as Z

import Loot.Network.Class
import Loot.Network.Utils (HasLens (..), HasLens', whileM)
import Loot.Network.ZMQ.Adapter
import Loot.Network.ZMQ.Common (ZTGlobalEnv (..), ZTNodeId (..), ZmqTcp, ztContext,
                                ztNodeConnectionId, ztNodeIdRouter)

----------------------------------------------------------------------------
-- Internal requests
----------------------------------------------------------------------------

data UpdatePeersReq = UpdatePeersReq
    { purAdd :: Set ZTNodeId
    , purDel :: Set ZTNodeId
    } deriving (Show, Generic)

data InternalRequest
    = IRUpdatePeers UpdatePeersReq
    | IRSubscribe ClientId [Subscription]
    deriving (Show)


instance Serialise UpdatePeersReq

applyUpdatePeers ::
       Set ZTNodeId -> UpdatePeersReq -> (Set ZTNodeId, Set ZTNodeId)
applyUpdatePeers peers (UpdatePeersReq {..}) = do
    let both = purDel `Set.intersection` purAdd
    let add' = (purAdd `Set.difference` both) `Set.difference` peers
    let del' = (purDel `Set.difference` both) `Set.intersection` peers
    (add',del')

----------------------------------------------------------------------------
-- Context
----------------------------------------------------------------------------

type ZTClientEnv = BiTQueue (ZTNodeId, CliRecvMsg) (Maybe ZTNodeId, (MsgType, Content))

-- | Client environment state. Is to be used by the one thread only
-- (main client worker).
data ZTNetCliEnv = ZTNetCliEnv
    {
      ztCliBack       :: Z.Socket Z.Router
      -- ^ Backend which receives data from the network and routes it
      -- to client workers.
    , ztCliSub        :: Z.Socket Z.Sub
      -- ^ Subscriber socket which is listening to other nodes'
      -- updates. It also sends data to clients .

    , ztPeers         :: TVar (Set ZTNodeId)
      -- ^ List of peers we are connected to. Is to be updated by the
      -- main thread only (broker worker).

    , ztClients       :: TVar (Map ClientId ZTClientEnv)
      -- ^ Channels binding broker to clients, map from client name to.
    , ztSubscriptions :: TVar (Map Subscription [ClientId])
      -- ^ Map from subscription key to clents' identifiers.
    , ztMsgTypes      :: TVar (Map MsgType ClientId)
      -- ^ Map from msg type key to clents' identifiers.

    , ztRequestQueue  :: TQueue InternalRequest
      -- ^ Queue to read (internal, administrative) client requests
      -- from, like updating peers or resetting connection.
    }

createNetCliEnv :: MonadIO m => ZTGlobalEnv -> Set ZTNodeId -> m ZTNetCliEnv
createNetCliEnv (ZTGlobalEnv ctx) peers = liftIO $ do
    -- I guess it's alright to connect ROUTER instead of binding it.
    -- https://stackoverflow.com/questions/16109139/zmq-when-to-use-zmq-bind-or-zmq-connect
    ztCliBack <- Z.socket ctx Z.Router
    forM_ peers $ Z.connect ztCliBack . ztNodeIdRouter

    ztCliSub <- Z.socket ctx Z.Sub

    ztClients <- newTVarIO mempty
    ztPeers <- newTVarIO peers
    ztSubscriptions <- newTVarIO mempty
    ztMsgTypes <- newTVarIO mempty
    ztRequestQueue <- TQ.newTQueueIO

    pure ZTNetCliEnv {..}

----------------------------------------------------------------------------
-- Methods
----------------------------------------------------------------------------

data BrokerStmRes
    = BSClient ClientId (Maybe ZTNodeId) (MsgType, Content)
    | BSBack
    | BSSub
    | BSRequest InternalRequest deriving Show

runBroker :: (MonadReader r m, HasLens' r ZTNetCliEnv, MonadIO m, MonadMask m) => m ()
runBroker = do
    ZTNetCliEnv{..} <- view $ lensOf @ZTNetCliEnv
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
            atomically $ find ((== nodeid) . ztNodeConnectionId) <$> readTVar ztPeers

    let sendToClient clientId (nId :: ZTNodeId, content :: CliRecvMsg) = do
            res <- atomically $ do
                cMap <- readTVar ztClients
                let toWrite = bReceiveQ <$> Map.lookup clientId cMap
                maybe pass (\tq -> TQ.writeTQueue tq (nId,content)) toWrite
                pure $ isJust toWrite
            unless res $ putText $ "sendToClient: cId doesn't exist: " <> show clientId

        -- Processing internal requests.
    let processReq = \case
            IRUpdatePeers req -> do
                (toConnect,toDisconnect) <- atomically $ do
                    peers <- readTVar ztPeers
                    let (toAdd,toDel) = applyUpdatePeers peers req
                    let peers' = (peers `Set.difference` toDel) `Set.union` toAdd
                    writeTVar ztPeers $! peers'
                    pure (toAdd,toDel)
                liftIO $ do
                    forM_ toDisconnect $ Z.disconnect ztCliBack . ztNodeIdRouter
                    forM_ toConnect $ Z.connect ztCliBack . ztNodeIdRouter
            IRSubscribe clientId subs -> do
                newSubs <- atomically $ do
                    currentSubs <-
                        Set.fromList . concat . Map.elems <$> readTVar ztSubscriptions
                    let newSubs = Set.fromList subs `Set.difference` currentSubs
                    modifyTVar ztSubscriptions $ \s ->
                        let insertClientId Nothing          = Just [clientId]
                            insertClientId (Just clientIds) = Just $ L.nub $ clientId : clientIds
                        in foldl' (\prevMap sub -> Map.alter insertClientId sub prevMap) s subs
                    pure newSubs
                forM_ newSubs $ Z.subscribe ztCliSub

        -- Clients to backend.
    let ctb clientId (nodeIdM :: Maybe ZTNodeId) (msgT, msg) = do
            nodeId <- ztNodeConnectionId <$> maybe choosePeer pure nodeIdM
            Z.sendMulti ztCliBack $ NE.fromList $ [nodeId, "", msgT] ++ msg

        -- Backend (ROUTER) to clients.
    let btc = \case
            (addr:"":msgT:msg) -> resolvePeer addr >>= \case
                Nothing -> putText $ "client btc: couldn't resolve peer: " <> show addr
                Just nodeId -> do
                    clientIdM <- atomically $ Map.lookup msgT <$> readTVar ztMsgTypes
                    maybe (putText "client btc: couldn't find client with this message type")
                          (\clientId -> sendToClient clientId (nodeId, Response msgT msg))
                          clientIdM
            other -> putText $ "client btf: wrong format: " <> show other

        -- SUB to clients.
    let stc = \case
            m@(k:addr:content) -> resolvePeer addr >>= \case
                Nothing -> putText $ "client stc: couldn't resolve peer: " <> show addr
                Just nodeId -> do
                    cids <- atomically $ fromMaybe [] . Map.lookup k <$> readTVar ztSubscriptions
                    forM_ cids $ \clientId -> sendToClient clientId (nodeId, Update k content)
                    when (null cids) $
                        -- It shouldn't be alright, since it means that our clients
                        -- records are broken (we're subscribed to something no client needs).
                        error $ "stf: Nobody got the subscription message for key " <> show k
            other -> putText $ "Client stc: wrong format: " <> show other
    (backStm, backDestroy) <- socketWaitReadSTMLong ztCliBack
    (subStm, subDestroy) <- socketWaitReadSTMLong ztCliSub
    let action = liftIO $ do
            res <- atomically $ do
                cMap <- readTVar ztClients
                let readReq = BSRequest <$> TQ.readTQueue ztRequestQueue
                let readClient =
                        map (\(cliId,biq) -> do (nodeIdM,content) <- TQ.readTQueue (bSendQ biq)
                                                pure $ BSClient cliId nodeIdM content)
                            (Map.toList cMap)
                orElseMulti $ NE.fromList $ [ readReq
                                            , BSBack <$ backStm
                                            , BSSub <$ subStm ] ++ readClient
            case res of
                BSRequest r            -> processReq r
                BSClient cId nIdM cont -> ctb cId nIdM cont
                BSBack                 -> whileM (canReceive ztCliBack) $
                                          Z.receiveMulti ztCliBack >>= btc
                BSSub                  -> whileM (canReceive ztCliSub) $
                                          Z.receiveMulti ztCliSub >>= stc
    forever action `finally` (backDestroy >> subDestroy)

----------------------------------------------------------------------------
-- Methods
----------------------------------------------------------------------------

{-
-- | Retrieve peers we're connected to.
ztGetPeers ::
       (MonadReader r m, HasLens' r ZTNetCliEnv, MonadIO m) => m (Set ZTNodeId)
ztGetPeers = readTVarIO =<< (ztPeers <$> view (lensOf @ZTNetCliEnv))

-- | Register a new client.
regClient ::
       (MonadReader r m, HasLens' r ZTGlobalEnv, MonadIO m)
    => ByteString -> Subscriptions -> m ZTClientEnv
regClient cid subs = do
    ctx <- view $ lensOf @ZTGlobalEnv . ztContext
    liftIO $ do
        workerSocket <- Z.socket ctx Z.Dealer
        let cid' = fromMaybe (error $ "registerClient: cid is malformed: " <> show cid) $
                   Z.toRestricted cid
        Z.setIdentity cid' workerSocket
        Z.connect workerSocket endpointBrokerClient
        unless (null subs) $
            Z.sendMulti workerSocket $ NE.fromList $ ["", "subscribe", cid] ++ subs
        pure $ ZTClientEnv workerSocket

updPeers ::
       (MonadReader r m, HasLens' r ZTNetCliEnv, MonadIO m)
    => ZTClientEnv
    -> Set ZTNodeId
    -> Set ZTNodeId
    -> m ()
updPeers (ZTClientEnv sock) toAdd toDel =
    liftIO $ Z.sendMulti sock $
    "modpeers" :| [BSL.toStrict $ serialise (UpdatePeersReq toAdd toDel)]

cSend :: MonadIO m => ZTClientEnv -> Maybe ZTNodeId -> Content -> m ZTNodeId
cSend (ZTClientEnv sock) to msg = liftIO $ do
    Z.sendMulti sock $ NE.fromList $
        ["", "send"] ++ maybeToList (ztNodeConnectionId <$> to) ++ ("":msg)
    let recvPeer = Z.receiveMulti sock >>= \case
            ["",sent,nodeId] -> pure (deserialise $ BSL.fromStrict nodeId)
            o -> error $ "cSend: broker didn't reply with 'sent': " <> show o
    maybe recvPeer pure to

cBroadcast :: MonadIO m => ZTClientEnv -> Content -> m ()
cBroadcast (ZTClientEnv sock) msg =
    liftIO $ Z.sendMulti sock $ NE.fromList $ ["", "broadcast"] ++ ("":msg)

cReceive :: MonadIO m => ZTClientEnv -> m (ZTNodeId, ReceiveRes)
cReceive (ZTClientEnv sock) = liftIO $ Z.receiveMulti sock >>= \case
    ("":"resp":a:msg) -> pure $ (decodeNodeId a, Response msg)
    ("":"upd":k:a:msg) -> pure $ (decodeNodeId a, Update k msg)
    other -> error $ "Dealer receive: message malformed: " <> show other
  where
    decodeNodeId = deserialise . BSL.fromStrict

instance ( MonadReader r m
         , HasLens' r ZTGlobalEnv
         , HasLens' r ZTNetCliEnv
         , MonadIO m) => NetworkingCli ZmqTcp m where

    type NodeId ZmqTcp = ZTNodeId

    runClient = runBroker
    getPeers = ztGetPeers
-}
--    registerClient = regClient
--    updatePeers = updPeers
