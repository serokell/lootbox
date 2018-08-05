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
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Control.Monad.STM (retry)
import Data.List.NonEmpty as NE
import qualified GHC.Conc.IO as G
import System.Posix.Types (Fd)
import qualified System.ZMQ4 as Z

-- | Internal socket adapter state. It is manipulated by several
-- threads, atomically.
--
-- The main worker thread switches it.
--
-- notInitialised | justRead → waiting   and blocks on threadWaitRead
-- waiting                   → canRead   and repeats
--
-- Other thread waits for the "canRead" state and then atomically
-- switches it to "justRead" which resumes the main worker. After this
-- "other thread" does the swap, it must read from the socket, because
-- sockets are edge triggered and threadWaitRead won't unblock in the
-- worker thread otherwise.
data SocketState
    = SSNotInitialised -- ^ Worker hasn't yet reached blocking section.
    | SSWaiting        -- ^ Currently waiting for messages from socket.
    | SSCanRead        -- ^ Data can be read from socket.
    | SSJustRead       -- ^ Data was just read from socket.
    deriving (Eq)

-- | Socket-related type that adapts it to STM-style polling.
data SocketAdapter = SocketAdapter (TVar SocketState) (A.Async ())

-- | Produces stm action corresponding to the action "lock until it is
-- possible to read from the socket". It doesn't guarantee that there
-- is actually any data to read from it.
threadWaitReadSTMLong :: Fd -> IO (TVar SocketState, A.Async ())
threadWaitReadSTMLong fd = do
    m <- newTVarIO SSNotInitialised

    -- Fork updater thread.
    t <- A.async $ forever $ do
        -- Section 1
        atomically $ readTVar m >>= \case
            SSWaiting        -> error "threadWaitReadSTMLong: SSWaiting"
            SSCanRead        -> retry
            SSNotInitialised -> writeTVar m SSWaiting
            SSJustRead       -> writeTVar m SSWaiting
        -- Section 2
        G.threadWaitRead fd
        -- Section 3
        atomically $ writeTVar m SSCanRead

    -- Wait until async action passes section 1.
    atomically $ do
        v <- readTVar m
        if v == SSNotInitialised then retry else pass

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
    readTVar m >>= \case
        SSCanRead -> writeTVar m SSJustRead
        _         -> retry

-- | Check if there are any messages can be read from socket.
adapterTry :: SocketAdapter -> STM Bool
adapterTry (SocketAdapter m _) =
    readTVar m >>= \case
        SSCanRead -> True <$ writeTVar m SSJustRead
        _ -> pure False

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
