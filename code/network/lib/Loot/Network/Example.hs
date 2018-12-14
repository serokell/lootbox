{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Playground example of how to use the framework.

module Loot.Network.Example () where

import Prelude hiding (log)

import Control.Concurrent (threadDelay, withMVar)
import qualified Control.Concurrent.Async.Lifted as A
import Control.Lens (makeLenses)
import Data.Default (def)
import qualified Data.Set as Set
import Loot.Log.Internal (Name, Severity (Debug))
import System.IO.Unsafe (unsafePerformIO)

import Loot.Base.HasLens (HasLens (..))
import Loot.Log.Internal (Logging (..), Message (..), NameSelector (GivenName))
import Loot.Network.BiTQueue (recvBtq, sendBtq)
import Loot.Network.Class
import Loot.Network.ZMQ
import qualified Loot.Network.ZMQ.Instance as I

----------------------------------------------------------------------------
-- State
----------------------------------------------------------------------------

data BigState = BigState
    { _bsCli  :: ZTNetCliEnv
    , _bsServ :: ZTNetServEnv
    , _bsCtx  :: ZTGlobalEnv
    }

makeLenses ''BigState


instance HasLens ZTNetCliEnv BigState ZTNetCliEnv where
    lensOf = bsCli

instance HasLens ZTNetServEnv BigState ZTNetServEnv where
    lensOf = bsServ

instance HasLens ZTGlobalEnv BigState ZTGlobalEnv where
    lensOf = bsCtx

type Env a = ReaderT BigState IO a

instance NetworkingCli ZmqTcp (ReaderT BigState IO) where
    type NodeId ZmqTcp = I.ZTNodeId
    runClient = I.runClientDefault
    getPeers = I.getPeersDefault
    updatePeers = I.updatePeersDefault
    registerClient = I.registerClientDefault

instance NetworkingServ ZmqTcp (ReaderT BigState IO) where
    type CliId ZmqTcp = I.ZTCliId
    runServer = I.runServerDefault
    registerListener = I.registerListenerDefault

----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

lMVar :: MVar ()
lMVar = unsafePerformIO $ newMVar ()

log :: MonadIO m => Text -> m ()
log x = liftIO $ withMVar lMVar $ \() -> putTextLn x >> pure ()

withZMQ :: Name -> ZTNodeId -> [ZTNodeId] -> Env () -> Env () -> IO ()
withZMQ name nId peers server action = do
    let logFoo (Message l n t) = log $ "[" <> show l <> "] " <> show n <> ": " <> t
    let logging = Logging logFoo (pure $ GivenName name) (pure Debug)
    withZTGlobalEnv logging $ \ztEnv -> do
        cliEnv <- createNetCliEnv ztEnv def peers
        servEnv <- createNetServEnv ztEnv def nId
        let execute = flip runReaderT (BigState cliEnv servEnv ztEnv) $
                      void $ A.withAsync server $ const $
                      void $ A.withAsync runClient $ const $
                      action
        execute `finally` (termNetCliEnv cliEnv >> termNetServEnv servEnv)

hang :: MonadIO m => m ()
hang = liftIO $ threadDelay 1000000000

minTest :: IO ()
minTest = do
    let n1 = ZTNodeId "127.0.0.1" 8001 8002
    let n2 = ZTNodeId "127.0.0.1" 8003 8004
    let node1 = withZMQ "n1" n1 mempty runServer hang
    let node2 = withZMQ "n2" n2 [n1] runServer hang
    void $ A.withAsync node1 $ const $ do
        threadDelay 1000000
        node2

testZmq :: IO ()
testZmq = do
    let n1 = ZTNodeId "127.0.0.1" 8001 8002
    let n2 = ZTNodeId "127.0.0.1" 8003 8004
    print n1
    print n2
    let node1 = do
            let runPonger biQ = forever $
                    atomically $ do
                        (cId, msgT, content) <- recvBtq biQ
                        when (msgT == "ping" && content == [""]) $
                            sendBtq biQ (Reply cId "pong" [""])
            let runPublisher biQ = forM_ [(1::Int)..] $ \i -> do
                    liftIO $ threadDelay 2000000
                    atomically $
                        sendBtq biQ (Publish (Subscription "block") ["noblock: " <> show i])
            let servWithCancel = do
                    s1 <- A.async $ runServer @ZmqTcp
                    liftIO $ do
                        threadDelay 10000000
                        log "server: *killing*"
                        A.cancel s1
                        log "server: *killed, waiting*"
                        threadDelay 10000000
                        log "server: *restarting*"
                    runServer @ZmqTcp
            withZMQ "n1" n1 mempty servWithCancel $ do
                log "server: *starting*"
                biQ1 <- registerListener @ZmqTcp "ponger" (Set.fromList ["ping"])
                biQ2 <- registerListener @ZmqTcp "publisher" mempty
                void $ A.concurrently_ (runPonger biQ1) (runPublisher biQ2)
    let node2 = do
            let runPinger biQ = forever $ do
                    liftIO $ threadDelay 1000000
                    log "pinger: sending"
                    atomically $ sendBtq biQ (Just n1, ("ping",[""]))
                    log "pinger: sent ping, waiting for the reply"
                    doUnlessM $ do
                        (nId,msg) <- atomically $ recvBtq biQ
                        if nId == n1 && msg == Response "pong" [""]
                            then True <$ log "pinger: got correct response"
                            else False <$ log ("pinger: got something else, probably " <>
                                              "subscription: " <> show msg)
            let runSubreader biQ = forever $ do
                    x <- atomically $ recvBtq biQ
                    log $ "subreader: got " <> show x

            withZMQ "n2" n2 [n1] runServer $ do
                log "client: *starting*"
                updatePeers @ZmqTcp $ def & uprAdd .~ [n1]
                -- biq2 is also subscribed to blocks but will discard them, just to test
                -- that subs are propagated properly
                biQ1 <- registerClient @ZmqTcp "pinger" (Set.fromList ["pong"])
                            (Set.singleton (Subscription "block"))
                biQ2 <- registerClient @ZmqTcp
                            "subreader"
                            mempty
                            (Set.singleton (Subscription "block"))
                void $ A.concurrently_ (runPinger biQ1) (runSubreader biQ2)
    void $ A.withAsync node1 $ const $ do
        threadDelay 1000000
        node2
  where
    doUnlessM action = ifM action pass (doUnlessM action)
