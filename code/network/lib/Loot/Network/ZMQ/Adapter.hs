{-# LANGUAGE NoApplicativeDo #-}

-- Enabling ApplicativeDo leads to https://ghc.haskell.org/trac/ghc/ticket/14105

-- | Provides a poll-like methods to compose STM actions and polls on
-- ZMQ sockets.

module Loot.Network.ZMQ.Adapter
       ( atLeastOne
       , threadWaitReadSTMLong
       , socketWaitReadSTMLong
       , canReceive
       ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async as A
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, newTMVarIO, putTMVar, readTMVar,
                                     swapTMVar, takeTMVar, tryReadTMVar)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, readTQueue, writeTQueue)
import Control.Monad.STM (STM, orElse, retry)
import Data.List.NonEmpty as NE
import qualified GHC.Conc.IO as G
import System.Posix.Types (Fd)
import qualified System.ZMQ4 as Z

import Loot.Network.Utils (tMeasureIO, whileM)

----------------------------------------------------------------------------
-- Functions to export
----------------------------------------------------------------------------

-- | Given a set of STM actions, returns all that succeed if at least
-- one does.
atLeastOne :: NonEmpty (STM (Maybe a)) -> STM (NonEmpty a)
atLeastOne l = fmap catMaybes (sequence (NE.toList l)) >>= \case
    [] -> retry
    x:xs -> pure $ x :| xs

-- | Produces stm action corresponding to the action "lock until it is
-- possible to read from the socket". It doesn't guarantee that there
-- is actually any data to read from it.
threadWaitReadSTMLong :: Fd -> IO (STM (), STM Bool, IO ())
threadWaitReadSTMLong fd = do
    -- Bool value inside indicates who should take action now. If it's
    -- false, it's updater thread, if it's true it's main wait thread.
    m <- newTMVarIO False
    -- Fork updater thread.
    t <- A.async $ forever $ do
        -- Take the content of the TMVar if it's False, otherwise skip.
        atomically $ do
            v <- readTMVar m
            if v == False
            then void (takeTMVar m)
            else retry
        G.threadWaitRead fd
        atomically $ putTMVar m True
    let waitAction = do
            v <- readTMVar m
            if v == True
            then void (swapTMVar m False) -- We now pass flag to another thread
            else retry                    -- Another thread haven't yet taken the lock
    let tryAction =
            tryReadTMVar m >>= \case
                Nothing -> pure False -- updater thread is blocking
                Just False -> pure False -- content has already been taken
                Just True -> True <$ void (swapTMVar m False)
    let killAction = A.cancel t
    return (waitAction, tryAction, killAction)

-- | 'threadWaitReadSTMLong' adapted for sockets.
socketWaitReadSTMLong :: (MonadIO m) => Z.Socket t -> m (STM (), STM Bool, m ())
socketWaitReadSTMLong s = liftIO $ do
    fd <- Z.fileDescriptor s
    (stmWait, stmTry, destroy) <- threadWaitReadSTMLong fd
    pure (stmWait, stmTry, liftIO destroy)

-- | Checks if data can be received from the socket. Use @whileM
-- canReceive process@ pattern after the STM action on the needed
-- socket.
canReceive :: Z.Socket t -> IO Bool
canReceive sock = elem Z.In <$> Z.events sock


{-
http://api.zeromq.org/master:zmq-getsockopt#toc30

The combination of a file descriptor returned by the ZMQ_FD option
being ready for reading but no actual events returned by a subsequent
retrieval of the ZMQ_EVENTS option is valid; applications should
simply ignore this case and restart their polling operation/event
loop.

Issues:
https://github.com/zeromq/libzmq/issues/1434
https://github.com/zeromq/czmq/issues/1487

Mans:
https://funcptr.net/2013/04/20/embedding-zeromq-in-the-libev-event-loop/
https://funcptr.net/2012/09/10/zeromq---edge-triggered-notification/

-}

----------------------------------------------------------------------------
-- Tests TODO move to benches (?)
----------------------------------------------------------------------------

_testWait :: Z.Socket Z.Router -> IO ()
_testWait sock = do
    putTextLn "testWait: starting"
    fd <- Z.fileDescriptor sock
    let action = do
            putTextLn "testWait: waiting"
            G.threadWaitRead fd
            putTextLn "testWait: waiting exited"
            whileM (canReceive sock) $ do
              putTextLn "testWait: receiving"
              d <- Z.receiveMulti sock
              putTextLn $ "testWait: " <> show d
    forever action

_testWaitSTM :: Z.Socket Z.Router -> IO ()
_testWaitSTM sock = do
    putTextLn "testWaitStm: waiting"
    fd <- Z.fileDescriptor sock
    let action = do
            putTextLn "testWaitStm: waiting"
            (waitAct,afterWaitAct) <- G.threadWaitReadSTM fd
            atomically waitAct `finally` afterWaitAct
            putTextLn "testWaitStm: waiting exited"
            whileM (canReceive sock) $ do
              putTextLn "testWaitStm: receiving"
              d <- Z.receiveMulti sock
              putTextLn $ "testWaitStm: " <> show d
    forever action


type Msg = NonEmpty ByteString

data WaitRes  = ReadSock | ReadQueue Msg deriving Show

-- | Sequential 'orElse' for nonempty list of STM actions.
orElseMulti :: NonEmpty (STM a) -> STM a
orElseMulti = foldr1 orElse

pollBoth :: Z.Socket Z.Dealer -> TQueue Msg -> TQueue Msg -> IO ()
pollBoth sock qPut qGrab = do
    --putTextLn "pollBoth starting"
    fd <- Z.fileDescriptor sock
    (waitForSocket,_, closeSocketListener) <- threadWaitReadSTMLong fd
    let action = do
            --putTextLn "pollBoth: round"
            --putTextLn "pollBoth: running stm"
            let waitAction =
                    orElseMulti $ NE.fromList $
                        [ ReadQueue <$> readTQueue qGrab
                        , waitForSocket >> pure ReadSock ]
            s <- atomically waitAction
            --putTextLn $ "pollBoth: continuing with " <> show s
            case s of
              ReadSock  -> do
                  whileM (canReceive sock) $ do
                    --putText "pollboth: whoop"
                    Z.receiveMulti sock >>= \case
                      ("":x:xs) -> do
                          --putTextLn "pollBoth whoop, received"
                          atomically $ writeTQueue qPut (x:|xs)
                      _ -> putText "pollboth: mda"
              ReadQueue (x:|xs) -> Z.sendMulti sock ("":|(x:xs))
    forever action `finally` closeSocketListener

data ATL = V1 | V2 | V3 | V4 deriving Show

{-
Prints:
V2 :| [V4]
V1 :| [V3]
V1 :| [V2,V3,V4]
-}
_testAtLeastOne :: IO ()
_testAtLeastOne = do
    [(v1 :: TMVar ()),v2,v3,v4] <- replicateM 4 newEmptyTMVarIO
    let wait1 = threadDelay 1000000
    void $ forkIO $ forever $ do
        wait1
        results <- atomically $ do
            let cast t v = maybe (pure Nothing) (\_ -> Just t <$ takeTMVar v) =<< tryReadTMVar v
            atLeastOne $ NE.fromList [cast V1 v1, cast V2 v2, cast V3 v3, cast V4 v4]
        print results
    let putVars (l :: [TMVar ()]) = atomically $ forM_ l $ \i -> putTMVar i ()
    threadDelay 500000
    putVars [v2,v4]
    wait1
    putVars [v1,v3]
    wait1
    putVars [v1,v2,v3,v4]
    wait1

_test :: IO ()
_test = do
    let servAddr = "inproc://mda"
    Z.withContext $ \ctx -> do
        void $ forkIO $ Z.withSocket ctx Z.Rep $ \sock -> do
            Z.bind sock servAddr
            threadDelay 1000000
            forever $ do
                _val <- Z.receiveMulti sock
                --putText $ "Server: got something: " <> show val
                Z.sendMulti sock $ "OK" :| []

--        forkIO $ Z.withSocket ctx Z.Router $ \sock -> do
--            Z.bind sock servAddr
--            testWaitSTM sock

        qPut <- newTQueueIO
        qGrab <- newTQueueIO

        void $ forkIO $ Z.withSocket ctx Z.Dealer $ \sock -> do
            Z.connect sock servAddr
            --forever $ do
            --    threadDelay 2000000
            --    putTextLn "Zap! Sending"
            --    Z.sendMulti sock $ NE.fromList ["", "meme"]
            pollBoth sock qPut qGrab

        void $ forM_ [(1 :: Int)..1000] $ \i -> do
            threadDelay 500000
            putTextLn "Client: sending"

            res <- tMeasureIO "send>recv" $ do
                atomically $ writeTQueue qGrab (show i:|[])
                atomically $ readTQueue qPut

            putTextLn $ "Client: received: " <> show res

        threadDelay 200000000
