module Loot.Async
       ( -- * Running forever
         WorkerFailed (WorkerFailed)
       , withWorker
       ) where

import Universum

import Control.Concurrent (myThreadId)
import Control.Exception (asyncExceptionFromException, asyncExceptionToException)
import Control.Exception.Safe (Exception (..), finally, throwTo, tryAsync)
import Data.Void (absurd)
import GHC.Stack (CallStack, HasCallStack, callStack, getCallStack, prettySrcLoc)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (withAsync)

import qualified GHC.Show as Show (Show (show))


-----------------------
-- Running forever
-----------------------

-- | Asynchronous exception thrown to the main thread if the worker crashes.
data WorkerFailed = WorkerFailed CallStack SomeException

instance Show WorkerFailed where
    show (WorkerFailed s e) = toString $ "Worker at " <> loc
        <> " failed: " <> show e
      where
        loc = case getCallStack s of
            (_, srcLoc) : _ -> prettySrcLoc srcLoc
            _               -> "<unknown location>"

instance Exception WorkerFailed where
    toException   = asyncExceptionToException
    fromException = asyncExceptionFromException

-- | Like 'withAsync', but makes sure that the worker thread will
-- not exit, neither by returning, nor by throwing an exception.

-- * The universal type guarantees that the worker cannot return.
-- * If the worker crashes, 'WorkerFailed' exception will be thrown in the parent thread.
withWorker :: (HasCallStack, MonadUnliftIO m, MonadMask m)
           => (forall void. m void)   -- ^ Action performed by the worker
           -> m b                     -- ^ Action performed in current thread
           -> m b
withWorker worker go = do
    tid <- liftIO $ myThreadId
    mainDone <- newIORef False
    let worker' =
            tryAsync worker >>= \case
                Right v -> absurd v
                Left  e ->
                    unlessM (readIORef mainDone) $
                        throwTo tid (WorkerFailed callStack e)
    withAsync worker' $ \_ -> go `finally` atomicWriteIORef mainDone True
