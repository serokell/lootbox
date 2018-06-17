-- | Bidirectional 'TQueue' is a main communication interface between
-- clients/listeners and outer world. It is fairly simple -- record
-- with two queues -- one for writing, one for reading. This module
-- provides some utilities to work with bitqueues. Everything is
-- trivial, though it's a convenient wrapper.

module Loot.Network.BiTQueue
    (
      BiTQueue (..)
    , newBtq
    , recvBtq
    , tryRecvBtq
    , sendBtq
    ) where

import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, readTQueue, tryReadTQueue, writeTQueue)

-- | Two-ended queue, first parameter is messages user receives,
-- second -- messages user sends.
data BiTQueue r s = BiTQueue
    { bReceiveQ :: TQueue r
      -- ^ Queue to receive messages from.
    , bSendQ    :: TQueue s
      -- ^ Queue to send messages to.
    }

-- | Creates an empty tqueue.
newBtq :: MonadIO m => m (BiTQueue r s)
newBtq = liftIO $ BiTQueue <$> newTQueueIO <*> newTQueueIO

-- | Read from the queue, blocks.
recvBtq :: BiTQueue r s -> STM r
recvBtq = readTQueue . bReceiveQ

-- | Read from the queue, returns Nothing if nothing can be read.
tryRecvBtq :: BiTQueue r s -> STM (Maybe r)
tryRecvBtq = tryReadTQueue . bReceiveQ

-- | Put an element to the queue.
sendBtq :: BiTQueue r s -> s -> STM ()
sendBtq btq = writeTQueue (bSendQ btq)
