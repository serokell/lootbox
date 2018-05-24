{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Server-side logic.

module Loot.Network.ZMQ.Server
    ( ZTNetServEnv
    , createNetServEnv
    ) where

import Control.Concurrent.STM.TVar (modifyTVar)
import Control.Lens (at, makeLenses, _Just)
import Control.Monad.STM (retry)
import Data.ByteString (ByteString)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map

import qualified Data.Restricted as Z
import qualified System.ZMQ4 as Z

import Loot.Network.Class (NetworkingServ (..))
import Loot.Network.Utils (HasLens (..), HasLens')
import Loot.Network.ZMQ.Common (ZTGlobalEnv (..), ZTNodeId (..), ZmqTcp, ztContext,
                                ztNodeConnectionId, ztNodeIdPub, ztNodeIdRouter)


-- | Address of the server proxy's backend (ROUTER), to which listeners
-- are connected.
endpointProxyServer :: String
endpointProxyServer = "inproc://proxy-client"

data ZTCliId = ZTCliId ByteString

data ZTListenerState = ZTLBusy | ZTLReady deriving (Eq,Show)

-- | Information about listener available to the proxy.
data ZTListenerInfo = ZTListenerInfo
    { _ztlId      :: ByteString
    , _ztlMsgType :: ByteString
    , _ztlState   :: ZTListenerState
    }

makeLenses ''ZTListenerInfo

data ZTNetServEnv = ZTNetServEnv
    { ztServProxyFront :: Z.Socket Z.Router
      -- ^ Front proxy which is talking to the outer network. Other
      -- nodes/clients connect to it and send requests.
    , ztServProxyBack  :: Z.Socket Z.Router
      -- ^ Backend proxy which distributes requests from the frontend
      -- to the local listeners. It is connected to local listeners
      -- via inproc. It's also router because we want to be choose
      -- which listener to send request to. It also does the load
      -- balancing. Hopefully.
    , ztServPub        :: Z.Socket Z.Pub
      -- ^ Publishing socket. For publishing.
    , ztListeners      :: TVar (Map ByteString ZTListenerInfo)
      -- Can be optimized even more by holding another map msgType -> info.
      -- ^ Information about listeners, map from id to info. Id
      -- inside info must match the map key.
    }

createNetServEnv :: MonadIO m => ZTGlobalEnv -> ZTNodeId -> m ZTNetServEnv
createNetServEnv (ZTGlobalEnv ctx) zid = liftIO $ do
    front <- Z.socket ctx Z.Router
    Z.setIdentity (Z.restrict $ ztNodeConnectionId zid) front
    Z.bind front (ztNodeIdRouter zid)

    back <- Z.socket ctx Z.Router
    Z.bind back endpointProxyServer

    pub <- Z.socket ctx Z.Pub
    Z.bind pub (ztNodeIdPub zid)

    listenersVar <- newTVarIO mempty

    pure (ZTNetServEnv front back pub listenersVar)

instance ( MonadReader r m
         , HasLens' r ZTGlobalEnv
         , HasLens' r ZTNetServEnv
         , MonadIO m) => NetworkingServ ZmqTcp m where

    type CliId ZmqTcp = ZTCliId

    runServer = do
        ZTNetServEnv{..} <- view $ lensOf @ZTNetServEnv
        let front = ztServProxyFront
        let back = ztServProxyBack

        let ftb _ = Z.receiveMulti front >>= \case
                m@(_clientId:"":msgT:_msg) -> do
                    -- TODO we block until at least one listener that can process this
                    -- message is ready. This is highly inefficient as there may be
                    -- other messages with other types that we can already process.
                    -- Current implementation is just a scratch.
                    (ztli :: ZTListenerInfo) <- atomically $ do
                        listeners <- readTVar ztListeners
                        let res = find (\l -> l ^. ztlMsgType == msgT &&
                                              l ^. ztlState == ZTLReady)
                                       (Map.elems listeners)
                        let onSuccess ztli = do
                                modifyTVar ztListeners $
                                    at (ztli ^. ztlId) . _Just . ztlState .~ ZTLBusy
                                pure ztli
                        maybe retry onSuccess res
                    Z.sendMulti back $ NE.fromList $ [ztli ^. ztlId, ""] ++ m
                o -> putText $ "ZmqTcp server ftb: bad format, skipping: " <> show o
            btf _ = Z.receiveMulti back >>= \case
                listenerId:"":d -> case d of
                    "send":m  -> Z.sendMulti front $ NE.fromList m
                    "pub":m   -> Z.sendMulti ztServPub $ NE.fromList m
                    ["ready"] ->
                        atomically $ modifyTVar ztListeners $
                            at listenerId . _Just . ztlState .~ ZTLReady
                    ["register",msgType] -> do
                        atomically $ modifyTVar ztListeners $
                            at listenerId .~ Just (ZTListenerInfo listenerId msgType ZTLBusy)
                        Z.sendMulti back $ NE.fromList [listenerId,"","registered"]
                    other         -> error $ "ZmqTcp server btf: unknown command: " <> show other
                other -> error $ "ZmqTcp server btf: malformed request: " <> show other
        -- We could poll with timeout and then publish a heartbeat.
        forever $ liftIO $
            Z.poll (-1) [ Z.Sock front [Z.In] $ Just ftb
                        , Z.Sock back [Z.In] $ Just btf ]

    -- What is needed is Rep with unrestricted send/receive pattern and
    -- one peer (bucket) connected. It could be DEALER too, but ROUTER to DEALER
    -- seems like a better option, even though ROUTER has only client.
    data ListenerEnv ZmqTcp = ZTListenerEnv { ztListSock :: Z.Socket Z.Dealer }

    registerListener lName msgType = do
        ctx <- view $ lensOf @ZTGlobalEnv . ztContext
        liftIO $ do
            lSocket <- Z.socket ctx Z.Dealer
            let lName' = fromMaybe (error $ "regList: lname is malformed: " <> show lName) $
                         Z.toRestricted lName
            Z.setIdentity lName' lSocket
            Z.connect lSocket endpointProxyServer

            Z.sendMulti lSocket $ NE.fromList $ ["","register",msgType]
            Z.receiveMulti lSocket >>= \case
                ["","registered"] -> pass
                other -> error $ "registerClient: malformed reply from proxy: " <> show other

            pure $ ZTListenerEnv lSocket

    accept (ZTListenerEnv {..}) marker = liftIO $ do
        when marker $ Z.sendMulti ztListSock $ NE.fromList $ ["", "ready"]
        Z.receiveMulti ztListSock >>= \case
            "":clientId:"":msg -> pure (ZTCliId clientId, msg)
            other -> error $ "onRequest: unknown format: " <> show other

    respond (ZTListenerEnv {..}) (ZTCliId clientId) msg =
        liftIO $ Z.sendMulti ztListSock $ NE.fromList $
        ["", "send", clientId, ""] ++ msg

    publish (ZTListenerEnv {..}) (k,content) =
        liftIO $ Z.sendMulti ztListSock $ NE.fromList $
        ["", "pub", k] ++ content
