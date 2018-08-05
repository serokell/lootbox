{-# LANGUAGE NoApplicativeDo #-}

-- Enabling ApplicativeDo leads to https://ghc.haskell.org/trac/ghc/ticket/14105

-- | Provides a poll-like methods to compose STM actions and polls on
-- ZMQ sockets.

module Loot.Network.ZMQ.Adapter
       ( SocketAdapter
       , newSocketAdapter
       , adapterRelease
       , adapterWait
       , adapterTry

       , atLeastOne
       , canReceive
       ) where

import Control.Concurrent.Async as A
import Control.Concurrent.STM.TMVar (TMVar, newTMVarIO, putTMVar, readTMVar, swapTMVar, takeTMVar,
                                     tryReadTMVar)
import Control.Monad.STM (retry)
import Data.List.NonEmpty as NE
import qualified GHC.Conc.IO as G
import System.Posix.Types (Fd)
import qualified System.ZMQ4 as Z

-- | Socket-related type that adapts it to STM-style polling.
data SocketAdapter = SocketAdapter (TMVar Bool) (A.Async ())

-- | Produces stm action corresponding to the action "lock until it is
-- possible to read from the socket". It doesn't guarantee that there
-- is actually any data to read from it.
threadWaitReadSTMLong :: Fd -> IO (TMVar Bool, A.Async ())
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
    pure (m, t)

-- | Create new socket adapter.
newSocketAdapter :: MonadIO m => Z.Socket t -> m SocketAdapter
newSocketAdapter s = liftIO $ do
    fd <- Z.fileDescriptor s
    (m, t) <- threadWaitReadSTMLong fd
    pure $ SocketAdapter m t

-- | Release socket adapter.
adapterRelease :: MonadIO m => SocketAdapter -> m ()
adapterRelease (SocketAdapter _ t) = liftIO $ A.cancel t

-- | Wait until any message comes to the socket.
adapterWait :: SocketAdapter -> STM ()
adapterWait (SocketAdapter m _) = do
    v <- readTMVar m
    if v == True
    then void (swapTMVar m False) -- We now pass flag to another thread
    else retry                    -- Another thread haven't yet taken the lock

-- | Check if there are any messages can be read from socket.
adapterTry :: SocketAdapter -> STM Bool
adapterTry (SocketAdapter m _) =
    tryReadTMVar m >>= \case
        Nothing -> pure False -- updater thread is blocking
        Just False -> pure False -- content has already been taken
        Just True -> True <$ void (swapTMVar m False)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- | Given a set of STM actions, returns all that succeed if at least
-- one does.
atLeastOne :: NonEmpty (STM (Maybe a)) -> STM (NonEmpty a)
atLeastOne l = fmap catMaybes (sequence (NE.toList l)) >>= \case
    [] -> retry
    x:xs -> pure $ x :| xs


-- | Checks if data can be received from the socket. Use @whileM
-- canReceive process@ pattern after the STM action on the needed
-- socket.
canReceive :: Z.Socket t -> IO Bool
canReceive sock = elem Z.In <$> Z.events sock


{-
Related:

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
