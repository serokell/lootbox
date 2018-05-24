{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | ZMQ Client implementation.

module Loot.Network.ZMQ.Client
    ( ZTNetCliEnv
    , createNetCliEnv
    ) where

import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
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

import Loot.Network.Class (NetworkingCli (..), ReceiveRes (..))
import Loot.Network.Utils (HasLens (..), HasLens')
import Loot.Network.ZMQ.Common (ZTGlobalEnv (..), ZTNodeId (..), ZmqTcp, ztContext,
                                ztNodeConnectionId, ztNodeIdRouter)



-- | Address of the client proxy's frontend (ROUTER), to which clients
-- are connected.
endpointProxyClient :: String
endpointProxyClient = "inproc://proxy-client"


-- | Client environment state. Is to be used by the one thread only
-- (main client worker).
data ZTNetCliEnv = ZTNetCliEnv
    { ztCliProxyFront :: Z.Socket Z.Router
      -- ^ Front proxy which is talking to client workers. It accepts
      -- requests and sends them to the outer network through backend.
    , ztCliProxyBack  :: Z.Socket Z.Router
      -- ^ Backend proxy which receives data from the network and
      -- routes it to workers through frontend.
    , ztCliSub        :: Z.Socket Z.Sub
      -- ^ Subscriber socket which is listening to other nodes'
      -- updates. It also sends data to clients through the frontend.
    , ztPeers         :: TVar (Set (NodeId ZmqTcp))
      -- ^ List of peers we are connected to. Is to be updated by the
      -- main thread only (proxy worker).
    , ztSubscriptions :: TVar (Map ByteString [ByteString])
      -- ^ Map from subscription key to clents' identifiers.
    }

createNetCliEnv :: MonadIO m => ZTGlobalEnv -> Set (NodeId ZmqTcp) -> m ZTNetCliEnv
createNetCliEnv (ZTGlobalEnv ctx) peers = liftIO $ do
    front <- Z.socket ctx Z.Router
    Z.bind front endpointProxyClient

    -- I guess it's alright to connect ROUTER instead of binding it.
    -- https://stackoverflow.com/questions/16109139/zmq-when-to-use-zmq-bind-or-zmq-connect
    back <- Z.socket ctx Z.Router
    forM_ peers $ Z.connect back . ztNodeIdRouter

    sub <- Z.socket ctx Z.Sub

    peersVar <- newTVarIO peers

    subsVar <- newTVarIO mempty

    pure (ZTNetCliEnv front back sub peersVar subsVar)

data PeersUpdateReq = PeersUpdateReq
    { purAdd :: Set (NodeId ZmqTcp)
    , purDel :: Set (NodeId ZmqTcp)
    } deriving (Show, Generic)

instance Serialise PeersUpdateReq

applyPeersUpdate ::
       Set (NodeId ZmqTcp)
    -> PeersUpdateReq
    -> (Set (NodeId ZmqTcp), Set (NodeId ZmqTcp))
applyPeersUpdate peers (PeersUpdateReq {..}) = do
    let both = purDel `Set.intersection` purAdd
    let add' = (purAdd `Set.difference` both) `Set.difference` peers
    let del' = (purDel `Set.difference` both) `Set.intersection` peers
    (add',del')

instance ( MonadReader r m
         , HasLens' r ZTGlobalEnv
         , HasLens' r ZTNetCliEnv
         , MonadIO m) => NetworkingCli ZmqTcp m where

    type NodeId ZmqTcp = ZTNodeId

    runClient = do
        ZTNetCliEnv{..} <- view $ lensOf @ZTNetCliEnv
        let front = ztCliProxyFront
        let back = ztCliProxyBack
        -- This function may do something creative (LRU! Lowest ping!),
        -- but instead (for now) it'll just choose a random peer.
        let choosePeer = do
                -- Yes, modulo bias. It shouldn't really matter.
                i <- abs <$> randomIO
                atomically $ do
                    l <- Set.toList <$> readTVar ztPeers
                    pure $ l L.!! (i `mod` length l)
        let ftb _ = Z.receiveMulti front >>= \case
                [_,"","modpeers",xs] -> case deserialiseOrFail (BSL.fromStrict xs) of
                    Right (peersUpdate :: PeersUpdateReq) -> do
                        (toConnect,toDisconnect) <- atomically $ do
                            peers <- readTVar ztPeers
                            let (toAdd,toDel) = applyPeersUpdate peers peersUpdate
                            let peers' = (peers `Set.difference` toDel) `Set.union` toAdd
                            writeTVar ztPeers $! peers'
                            pure (toAdd,toDel)
                        liftIO $ do
                            forM_ toDisconnect $ Z.disconnect back . ztNodeIdRouter
                            forM_ toConnect $ Z.connect back . ztNodeIdRouter
                    Left e                          ->
                        error $ "Ftb client proxy, modpeers parsing failed: " <> show e
                (_:"":"subscribe":cid:subs) -> do
                    newSubs <- atomically $ do
                        currentSubs <-
                            Set.fromList . concat . Map.elems <$> readTVar ztSubscriptions
                        let newSubs = Set.fromList subs `Set.difference` currentSubs
                        modifyTVar ztSubscriptions $ \s ->
                            let insertCid Nothing     = Just [cid]
                                insertCid (Just cids) = Just $ L.nub $ cid : cids
                            in foldl' (\prevMap sub -> Map.alter insertCid sub prevMap) s subs
                        pure newSubs
                    forM_ newSubs $ Z.subscribe ztCliSub
                (_clientId:"":"send":xs) -> do
                    (peer,msg) <- case xs of
                                    (peer:"":msg) -> pure (peer,msg)
                                    ("":msg)      -> (,msg) . ztNodeConnectionId <$> choosePeer
                                    other -> error $ "ftb wrong 'send' request: " <> show other
                    Z.sendMulti back $ peer :| msg
                (_clientId:"":"broadcast":msg) -> do
                    (peers :: Set ZTNodeId) <- readTVarIO ztPeers
                    forM_ peers $ \zid -> Z.sendMulti back $ ztNodeConnectionId zid :| msg
                other -> error $ "ftb wrong request format: " <> show other
            btf _ = Z.receiveMulti back >>= \case
                -- TODO ignoring peerAddr is WEIRD. Should we send it with a message? Or
                -- remember inside the map? Why do we expect node to send us clientId?..
                -- TODO conversations!
                (_peerAddr:"":clientId:"":msg) ->
                    Z.sendMulti front $ NE.fromList $ [clientId,"","resp"] ++ msg
                other -> putText $ "Client btf: wrong format: " <> show other
            stf _ = Z.receiveMulti ztCliSub >>= \case
                m@(k:_) -> do
                    cids <- atomically $ fromMaybe [] . Map.lookup k <$> readTVar ztSubscriptions
                    forM_ cids $ \clientId -> Z.sendMulti front $ NE.fromList $ [clientId, ""] ++ m
                    when (null cids) $
                        -- It shouldn't be alright, since it means that our clients
                        -- records are broken (we're subscribed to something no client needs).
                        error $ "stf: Nobody got the subscription message for key " <> show k
                other -> putText $ "Client stf: wrong format: " <> show other
        forever $ liftIO $
            Z.poll (-1) [ Z.Sock front [Z.In] $ Just ftb
                        , Z.Sock back [Z.In] $ Just btf
                        , Z.Sock ztCliSub [Z.In] $ Just stf ]

    getPeers = readTVarIO =<< (ztPeers <$> view (lensOf @ZTNetCliEnv))

    data ClientEnv ZmqTcp = ZTClientEnv (Z.Socket Z.Dealer)

    registerClient cid subs = do
        ctx <- view $ lensOf @ZTGlobalEnv . ztContext
        liftIO $ do
            workerSocket <- Z.socket ctx Z.Dealer
            let cid' = fromMaybe (error $ "registerClient: cid is malformed: " <> show cid) $
                       Z.toRestricted cid
            Z.setIdentity cid' workerSocket
            Z.connect workerSocket endpointProxyClient
            unless (null subs) $
                Z.sendMulti workerSocket $ NE.fromList $ ["", "subscribe", cid] ++ subs
            pure $ ZTClientEnv workerSocket

    updatePeers (ZTClientEnv sock) toAdd toDel =
        liftIO $ Z.sendMulti sock $
        "modpeers" :| [BSL.toStrict $ serialise (PeersUpdateReq toAdd toDel)]

    send (ZTClientEnv sock) to msg =
        liftIO $ Z.sendMulti sock $ NE.fromList $
            ["", "send"] ++ maybeToList (ztNodeConnectionId <$> to) ++ ("":msg)

    broadcastCli (ZTClientEnv sock) msg = do
        liftIO $ Z.sendMulti sock $ NE.fromList $ ["", "broadcast"] ++ ("":msg)

    receive (ZTClientEnv sock) = liftIO $ Z.receiveMulti sock >>= \case
        ("":"resp":msg) -> pure $ Response msg
        ("":"upd":k:msg) -> pure $ Update k msg
        other -> error $ "Dealer receive: message malformed: " <> show other
