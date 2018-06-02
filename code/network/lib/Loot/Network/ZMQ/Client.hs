{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- method naming is HORRIBLE, i hope to fix it later
-- | ZMQ Client implementation.

module Loot.Network.ZMQ.Client
    ( ZTNetCliEnv
    , createNetCliEnv
    , ZTClientEnv
    , RequestQueue
    , runBroker
    , getPeers
    , registerClient
    , updatePeers
    ) where

import Control.Concurrent.STM.TQueue (TQueue)
import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Concurrent.STM.TVar (modifyTVar)
import Control.Monad.Except (runExceptT, throwError)
import Data.ByteString (ByteString)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.Random (randomIO)

import qualified System.ZMQ4 as Z

import Loot.Network.Class hiding (NetworkingCli (..), NetworkingServ (..))
import Loot.Network.Utils (HasLens (..), HasLens', whileM)
import Loot.Network.ZMQ.Adapter
import Loot.Network.ZMQ.Common (ZTGlobalEnv (..), ZTNodeId (..), ztNodeConnectionId, ztNodeIdRouter)

----------------------------------------------------------------------------
-- Internal requests
----------------------------------------------------------------------------

data UpdatePeersReq = UpdatePeersReq
    { purAdd :: Set ZTNodeId
    , purDel :: Set ZTNodeId
    } deriving (Show, Generic)

data InternalRequest
    = IRUpdatePeers UpdatePeersReq
    | IRRegister ClientId [MsgType] [Subscription] ZTClientEnv

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

newtype RequestQueue = RequestQueue { unRequestQueue :: TQueue InternalRequest }

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

    , ztRequestQueue  :: RequestQueue
      -- ^ Queue to read (internal, administrative) client requests
      -- from, like updating peers or resetting connection.
    }

--instance HasLens' r ZTNetCliEnv => HasLens RequestQueue r RequestQueue where
--    lensOf = lensOf . to ztRequestQueue

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
    ztRequestQueue <- RequestQueue <$> TQ.newTQueueIO

    pure ZTNetCliEnv {..}

----------------------------------------------------------------------------
-- Methods
----------------------------------------------------------------------------

data BrokerStmRes
    = BSClient ClientId (Maybe ZTNodeId) (MsgType, Content)
    | BSBack
    | BSSub
    | BSRequest InternalRequest

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
                    currentSubs <-
                        Set.fromList . concat . Map.elems <$> lift (readTVar ztSubscriptions)
                    let newSubs = Set.fromList subs `Set.difference` currentSubs
                    lift $ modifyTVar ztSubscriptions $ \s ->
                        let insertClientId Nothing          = Just [clientId]
                            insertClientId (Just clientIds) = Just $ L.nub $ clientId : clientIds
                        in foldl' (\prevMap sub -> Map.alter insertClientId sub prevMap) s subs
                    pure newSubs
                case res of
                    Left e -> error $ "IRRegister: " <> e
                    Right newSubs -> do
                        forM_ newSubs $ Z.subscribe ztCliSub

        -- Clients to backend.
    let ctb (nodeIdM :: Maybe ZTNodeId) (msgT, msg) = do
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
            (k:addr:content) -> resolvePeer addr >>= \case
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
                let readReq = BSRequest <$> TQ.readTQueue (unRequestQueue ztRequestQueue)
                let readClient =
                        map (\(cliId,biq) -> do (nodeIdM,content) <- TQ.readTQueue (bSendQ biq)
                                                pure $ BSClient cliId nodeIdM content)
                            (Map.toList cMap)
                orElseMulti $ NE.fromList $ [ readReq
                                            , BSBack <$ backStm
                                            , BSSub <$ subStm ] ++ readClient
            case res of
                BSRequest r             -> processReq r
                BSClient _cId nIdM cont -> ctb  nIdM cont
                BSBack                  -> whileM (canReceive ztCliBack) $
                                           Z.receiveMulti ztCliBack >>= btc
                BSSub                   -> whileM (canReceive ztCliSub) $
                                           Z.receiveMulti ztCliSub >>= stc
    forever action `finally` (backDestroy >> subDestroy)

----------------------------------------------------------------------------
-- Methods
----------------------------------------------------------------------------

-- | Retrieve peers we're connected to.
getPeers ::
       (MonadReader r m, HasLens' r ZTNetCliEnv, MonadIO m) => m (Set ZTNodeId)
getPeers = readTVarIO =<< (ztPeers <$> view (lensOf @ZTNetCliEnv))

-- | Register a new client.
registerClient ::
       (MonadReader r m, HasLens' r RequestQueue, MonadIO m)
    => ClientId -> [MsgType] -> [Subscription] -> m ZTClientEnv
registerClient clientId msgTs subs = do
    requestQueue <- unRequestQueue <$> view (lensOf @RequestQueue)
    liftIO $ do
        biTQueue <- BiTQueue <$> TQ.newTQueueIO <*> TQ.newTQueueIO
        atomically $ TQ.writeTQueue requestQueue $ IRRegister clientId msgTs subs biTQueue
        pure biTQueue

-- | Updates peers.
updatePeers ::
       (MonadReader r m, HasLens' r RequestQueue, MonadIO m)
    => Set ZTNodeId
    -> Set ZTNodeId
    -> m ()
updatePeers toAdd toDel = do
    requestQueue <- unRequestQueue <$> view (lensOf @RequestQueue)
    atomically $ TQ.writeTQueue requestQueue $ IRUpdatePeers (UpdatePeersReq toAdd toDel)
