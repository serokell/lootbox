{-# LANGUAGE BangPatterns #-}

-- | Provides a poll-like methods to compose STM actions and polls on
-- ZMQ sockets.

module Loot.Network.ZMQ.Adapter
       ( orElseMulti
       , threadWaitReadSTMLong
       , socketWaitReadSTMLong
       , canReceive
       ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, readTQueue, writeTQueue)
import Control.Monad.STM (STM, orElse, retry)
import Data.List.NonEmpty as NE
import qualified GHC.Conc.IO as G
import qualified GHC.Conc.Sync as Sync
import System.Posix.Types (Fd)
import qualified System.ZMQ4 as Z

import Loot.Network.Utils (tMeasureIO, whileM)

----------------------------------------------------------------------------
-- Functions to export
----------------------------------------------------------------------------

-- | Sequential 'orElse' for nonempty list of STM actions.
orElseMulti :: NonEmpty (STM a) -> STM a
orElseMulti (x :| [])     = x
orElseMulti (x :| (y:xs)) = x `orElse` (orElseMulti $ y :| xs)

-- | Produces stm action corresponding to the action "lock until it is
-- possible to read from the socket". It doesn't guarantee that there
-- is actually any data to read from it.
threadWaitReadSTMLong :: Fd -> IO (STM (), IO ())
threadWaitReadSTMLong fd = do
    m <- Sync.newTVarIO False
    t <- Sync.forkIO $ forever $ do
        G.threadWaitRead fd
        Sync.atomically $ Sync.writeTVar m True
    let waitAction = do b <- Sync.readTVar m
                        if b then Sync.writeTVar m False else retry
    let killAction = Sync.killThread t
    return (waitAction, killAction)

-- | 'threadWaitReadSTMLong' adapted for sockets.
socketWaitReadSTMLong :: (MonadIO m) => Z.Socket t -> m (STM (), m ())
socketWaitReadSTMLong s = liftIO $ do
    fd <- Z.fileDescriptor s
    (stm, destroy) <- threadWaitReadSTMLong fd
    pure (stm, liftIO destroy)

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

pollBoth :: Z.Socket Z.Dealer -> TQueue Msg -> TQueue Msg -> IO ()
pollBoth sock qPut qGrab = do
    --putTextLn "pollBoth starting"
    fd <- Z.fileDescriptor sock
    (waitForSocket,closeSocketListener) <- threadWaitReadSTMLong fd
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
