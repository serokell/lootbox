-- | Swalka.

module Loot.Network.Testzmq (testZmq) where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async.Lifted as A
import qualified Control.Concurrent.STM.TQueue as TQ
import Control.Lens (makeLenses)
import Data.Default (def)
import qualified Data.Set as Set

import Loot.Network.Class
import Loot.Network.Utils (HasLens (..))
import qualified Loot.Network.ZMQ.Client as ZC
import Loot.Network.ZMQ.Common
import Loot.Network.ZMQ.Instance ()
import qualified Loot.Network.ZMQ.Server as ZS

----------------------------------------------------------------------------
-- State
----------------------------------------------------------------------------

data BigState = BigState
    { _bsCli  :: ZC.ZTNetCliEnv
    , _bsServ :: ZS.ZTNetServEnv
    , _bsCtx  :: ZTGlobalEnv
    }

makeLenses ''BigState


instance HasLens ZC.ZTNetCliEnv BigState ZC.ZTNetCliEnv where
    lensOf = bsCli

instance HasLens ZS.ZTNetServEnv BigState ZS.ZTNetServEnv where
    lensOf = bsServ

instance HasLens ZTGlobalEnv BigState ZTGlobalEnv where
    lensOf = bsCtx

type Env a = ReaderT BigState IO a

----------------------------------------------------------------------------
-- Runners
----------------------------------------------------------------------------

runZMQ :: ZTNodeId -> Env () -> Env () -> IO ()
runZMQ nId server client = do
    withZTGlobalEnv $ \ztEnv -> do
        cliEnv <- ZC.createNetCliEnv ztEnv mempty
        servEnv <- ZS.createNetServEnv ztEnv nId
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
            let servWithCancel = do
                    s1 <- A.async $ runServer @ZmqTcp
                    liftIO $ do
                        threadDelay 5000000
                        putTextLn "------Killing"
                        A.cancel s1
                        putTextLn "------Killed, waiting"
                        threadDelay 10000000
                        putTextLn "------Restarting"
                    runServer @ZmqTcp
            let server = do
                    biQ <- registerListener @ZmqTcp "ponger" (Set.fromList ["ping"])
                    void $ A.concurrently (runPonger biQ) servWithCancel
            liftIO $ threadDelay 100000
            putTextLn "starting server"
            runZMQ n1 server pass
    let node2 = do
            let runPinger biQ = forever $ do
                    -- disabled for now
                    liftIO $ threadDelay 1000000000000

                    liftIO $ threadDelay 1000000
                    putTextLn "sending ping"
                    atomically $ TQ.writeTQueue (bSendQ biQ) (Just n1, ("ping",[""]))
                    liftIO $ threadDelay 50000
                    putTextLn "sent ping, waiting for reply"
                    (nId,msg) <- atomically $ TQ.readTQueue (bReceiveQ biQ)
                    putTextLn $
                        if nId == n1 && msg == Response "pong" [""]
                        then "alright!"
                        else "error :("
                    liftIO $ threadDelay 50000
            let client = do
                    biQ <- registerClient @ZmqTcp "pinger" (Set.fromList ["pong"]) mempty
                    updatePeers @ZmqTcp $ def & uprAdd .~ (Set.singleton n1)
                    void $ A.concurrently (runPinger biQ) (runClient @ZmqTcp)
            liftIO $ threadDelay 200000
            putTextLn "starting client"
            runZMQ n2 pass client
    void $ A.concurrently node1 node2
