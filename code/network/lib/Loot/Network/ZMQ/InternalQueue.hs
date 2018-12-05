-- | Internal request queue -- used in both client and server brokers.

module Loot.Network.ZMQ.InternalQueue
    ( InternalQueue (..)
    , newInternalQueue
    , iqSend
    , iqReceive
    ) where

import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, tryReadTQueue, writeTQueue)
import qualified Data.List.NonEmpty as NE
import System.Random (randomRIO)
import qualified System.ZMQ4 as Z

-- | Internal queue is basically a unidirectional channel. We use it
-- instead of STM-based channels/queues (and similar things) because
-- we want to use ZMQ's poll. On the other hand, we do not always
-- want/can to serialise data we send through the queue, so we send
-- data through the TQueue, and only "ping" through the socket pair.
--
-- Important: the fact one receives "pong" doesn't mean the message is
-- actually in the queue. Queue must be "tried" instead of blocking.
data InternalQueue t = InternalQueue
    { iqIn     :: !(Z.Socket Z.Pair)
      -- ^ The socket we send/write pings to.
    , iqOut    :: !(Z.Socket Z.Pair)
      -- ^ The socket we read pings from.
    , iqTQueue :: !(TQueue t)
      -- ^ Queue that is actually used to transfer the data.
    }

-- | Creates a new internal queue.
newInternalQueue :: Z.Context -> IO (InternalQueue t)
newInternalQueue ztContext = do
    iqOutHost <- ("inproc://" <>) <$> replicateM 15 (randomRIO ('a','z'))

    iqIn <- Z.socket ztContext Z.Pair
    iqOut <- Z.socket ztContext Z.Pair

    Z.bind iqOut iqOutHost
    Z.connect iqIn iqOutHost

    iqTQueue <- newTQueueIO

    pure InternalQueue{..}

-- | Sends message to the internal queue.
iqSend :: InternalQueue t -> t -> IO ()
iqSend InternalQueue {iqIn, iqTQueue} msg = do
    atomically $ writeTQueue iqTQueue msg
    Z.sendMulti iqIn $ NE.fromList [""]

-- | Tries to receive something from the queue. This method should be
-- called after "poll" on "out" socket have succeeded.
iqReceive :: InternalQueue t -> IO (Maybe t)
iqReceive InternalQueue{iqOut, iqTQueue} = do
    _ <- Z.receive iqOut
    atomically $ tryReadTQueue iqTQueue
