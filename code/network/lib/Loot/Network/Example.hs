{-# LANGUAGE TypeFamilies #-}

-- | Playground example of how to use the framework.

module Loot.Network.Example (testZmq) where

import Prelude hiding (log)

import Control.Concurrent (threadDelay, withMVar)
import qualified Control.Concurrent.Async.Lifted as A
import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Lens (makeLenses)
import Data.Default (def)
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)

import Loot.Base.HasLens (HasLens (..))
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
log x = do
    liftIO $ withMVar lMVar $ \() -> putTextLn x >> pure ()

runZMQ :: ZTNodeId -> Env () -> Env () -> IO ()
runZMQ nId server client = do
    let logFoo l t = putTextLn $ "[" <> show l <> "]: " <> t
    withZTGlobalEnv logFoo $ \ztEnv -> do
        cliEnv <- createNetCliEnv ztEnv mempty
        servEnv <- createNetServEnv ztEnv nId
        flip runReaderT (BigState cliEnv servEnv ztEnv) $ do
            void $ A.concurrently server client

testZmq :: IO ()
testZmq = do
    let n1 = (ZTNodeId "127.0.0.1" 8001 8002)
    let n2 = (ZTNodeId "127.0.0.1" 8003 8004)
    let node1 = do
            let runPonger biQ = forever $ do
                    atomically $ do
                        (cId, msgT, content) <- TQ.readTQueue (bReceiveQ biQ)
                        when (msgT == "ping" && content == [""]) $
                            TQ.writeTQueue (bSendQ biQ) (Reply cId "pong" [""])
            let runPublisher biQ = forM_ [(1::Int)..] $ \i -> do
                    liftIO $ threadDelay 2000000
                    atomically $ TQ.writeTQueue
                        (bSendQ biQ)
                        (Publish (Subscription "block") ["noblock: " <> show i])
            let servWithCancel = do
                    s1 <- A.async $ runServer @ZmqTcp
                    liftIO $ do
                        threadDelay 5000000
                        log "server: *killing*"
                        A.cancel s1
                        log "server: *killed, waiting*"
                        threadDelay 10000000
                        log "server: *restarting*"
                    runServer @ZmqTcp
            let server = do
                    biQ1 <- registerListener @ZmqTcp "ponger" (Set.fromList ["ping"])
                    biQ2 <- registerListener @ZmqTcp "publisher" mempty
                    void $ A.concurrently_ (runPonger biQ1) $
                           A.concurrently_ (runPublisher biQ2) $
                           servWithCancel
            liftIO $ threadDelay 100000
            log "server: *starting*"
            runZMQ n1 server pass
    let node2 = do
            let runPinger biQ = forever $ do

                    liftIO $ threadDelay 1000000
                    log "pinger: sending"
                    atomically $ TQ.writeTQueue (bSendQ biQ) (Just n1, ("ping",[""]))
                    liftIO $ threadDelay 50000
                    log "pinger: sent ping, waiting for reply"
                    doUnlessM $ do
                        (nId,msg) <- atomically $ TQ.readTQueue (bReceiveQ biQ)
                        if nId == n1 && msg == Response "pong" [""]
                            then True <$ log "pinger: got correct response"
                            else False <$ log ("pinger: got something else, probably " <>
                                              "subscription: " <> show msg)
                    liftIO $ threadDelay 50000
            let runSubreader biQ = forever $ do
                    x <- atomically $ TQ.readTQueue (bReceiveQ biQ)
                    log $ "subreader: got " <> show x
            let client = do
                    -- biq2 is also subscribed to blocks but will discard them, just to test
                    -- that subs are propagated properly
                    biQ1 <- registerClient @ZmqTcp "pinger" (Set.fromList ["pong"])
                                (Set.singleton (Subscription "block"))
                    biQ2 <- registerClient @ZmqTcp
                                "subreader"
                                mempty
                                (Set.singleton (Subscription "block"))
                    updatePeers @ZmqTcp $ def & uprAdd .~ (Set.singleton n1)
                    void $ A.concurrently_ (runPinger biQ1) $
                           A.concurrently_ (runSubreader biQ2) $
                           (runClient @ZmqTcp)
            liftIO $ threadDelay 200000
            log "client: *starting*"
            runZMQ n2 pass client
    void $ A.concurrently node1 node2
  where
    doUnlessM action = ifM action pass (doUnlessM action)
