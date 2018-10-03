{-# LANGUAGE TypeFamilies #-}

-- | Playground example of how to use the framework.

module Loot.Network.Example (testZmq) where

import Prelude hiding (log)

import Control.Concurrent (threadDelay, withMVar)
import qualified Control.Concurrent.Async.Lifted as A
import Control.Lens (makeLenses)
import Data.Default (def)
import qualified Data.Set as Set
import qualified Data.Text as T
import Fmt ((+|), (|+))
import Loot.Base.HasLens (HasTaggedGetter (..), HasTaggedLens (..))
import Loot.Log (LogContext, LoggingImpl, event, level, tags, withLogTag)
import Loot.Log.Rio ()
import System.IO.Unsafe (unsafePerformIO)

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

    , _bsLoggingImpl:: LoggingImpl IO
    , _bsLogCtx     :: LogContext
    }

makeLenses ''BigState


instance HasTaggedGetter ZTNetCliEnv BigState ZTNetCliEnv where
    getterOf = bsCli

instance HasTaggedGetter ZTNetServEnv BigState ZTNetServEnv where
    getterOf = bsServ

instance HasTaggedGetter ZTGlobalEnv BigState ZTGlobalEnv where
    getterOf = bsCtx

instance HasTaggedGetter (LoggingImpl IO) BigState (LoggingImpl IO) where
    getterOf = bsLoggingImpl

instance HasTaggedGetter LogContext BigState LogContext
instance HasTaggedLens LogContext BigState BigState LogContext LogContext where
    lensOf = bsLogCtx

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
log x = liftIO $
    withMVar lMVar $ \() -> putTextLn x

loggingImpl :: MonadIO m => LoggingImpl m
loggingImpl ctx r = log msg
  where
    msg = "["+|r^.level|+"] "+|T.intercalate ":" (ctx^.tags)|+": "+|r^.event|+""

withZMQ :: ZTNodeId -> Set ZTNodeId -> Env () -> Env () -> IO ()
withZMQ nId peers server action = withZTGlobalEnv $ \ztEnv -> do
    cliEnv <- createNetCliEnv ztEnv peers
    servEnv <- createNetServEnv ztEnv nId
    let execute = flip runReaderT (BigState cliEnv servEnv ztEnv loggingImpl mempty) $
                      void $ A.withAsync server $ const $
                      void $ A.withAsync runClient $ const $
                      action
    execute `finally` (termNetCliEnv cliEnv >> termNetServEnv servEnv)

testZmq :: IO ()
testZmq = do
    let n1 = (ZTNodeId "127.0.0.1" 8001 8002)
    let n2 = (ZTNodeId "127.0.0.1" 8003 8004)
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
                        threadDelay 5000000
                        log "server: *killing*"
                        A.cancel s1
                        log "server: *killed, waiting*"
                        threadDelay 10000000
                        log "server: *restarting*"
                    runServer @ZmqTcp
            withZMQ n1 mempty servWithCancel $ withLogTag "n1" $ do
                log "server: *starting*"
                biQ1 <- registerListener @ZmqTcp "ponger" (Set.fromList ["ping"])
                biQ2 <- registerListener @ZmqTcp "publisher" mempty
                void $ A.concurrently_ (runPonger biQ1) (runPublisher biQ2)
    let node2 = do
            let runPinger biQ = forever $ do
                    log "pinger: sending"
                    atomically $ sendBtq biQ (Just n1, ("ping",[""]))
                    log "pinger: sent ping, waiting for reply"
                    doUnlessM $ do
                        (nId,msg) <- atomically $ recvBtq biQ
                        if nId == n1 && msg == Response "pong" [""]
                            then True <$ log "pinger: got correct response"
                            else False <$ log ("pinger: got something else, probably " <>
                                              "subscription: " <> show msg)
                    liftIO $ threadDelay 1500000
            let runSubreader biQ = forever $ do
                    x <- atomically $ recvBtq biQ
                    log $ "subreader: got " <> show x

            withZMQ n2 (Set.singleton n1) runServer $ withLogTag "n2" $ do
                log "client: *starting*"
                -- biq2 is also subscribed to blocks but will discard them, just to test
                -- that subs are propagated properly
                biQ1 <- registerClient @ZmqTcp "pinger" (Set.fromList ["pong"])
                            (Set.singleton (Subscription "block"))
                biQ2 <- registerClient @ZmqTcp
                            "subreader"
                            mempty
                            (Set.singleton (Subscription "block"))
                updatePeers @ZmqTcp $ def & uprAdd .~ (Set.singleton n1)
                void $ A.concurrently_ (runPinger biQ1) (runSubreader biQ2)
    void $ A.withAsync node1 $ const $ do
        threadDelay 1000000
        node2
  where
    doUnlessM action = ifM action pass (doUnlessM action)
