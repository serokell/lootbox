{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Loot.Log.Rio.NoContext where

import Loot.Base.Test (thisLine)
import Loot.Log (Level (..), LoggingImpl, logInfo)
import Loot.Log.Internal.Context (LogContext)
import Loot.Log.Internal.Core (LogEvent (..), LogRecord (..))
import Loot.Log.Internal.Location (Location (..))
import Loot.Log.Rio.NoContext ()

import Test.Tasty.HUnit (Assertion, (@=?))


newtype RIO ctx a = RIO { runRIO :: ReaderT ctx IO a }
    deriving (Functor, Applicative, Monad, MonadReader ctx, MonadIO)

withRIO :: ctx -> RIO ctx a -> IO a
withRIO rctx (RIO r) = runReaderT r rctx


type LogRef = IORef [(LogContext, LogRecord)]

-- | A logging implementation that stores logs in an 'IORef'.
mkIORefLoggingImpl
    :: IO (LoggingImpl IO, LogRef)
mkIORefLoggingImpl = do
    ref <- newIORef [] :: IO LogRef
    let loggingImpl ctx r = atomicModifyIORef' ref (\xs -> ((ctx, r) : xs, ()))
    pure (loggingImpl, ref)


type ReaderCtx = LoggingImpl IO


unit_logNoCtx :: Assertion
unit_logNoCtx = do
    (logImpl, ref) <- mkIORefLoggingImpl
    withRIO (logImpl) $ do
        logInfo "This is a test"
    result <- readIORef ref
    [(mempty, LogRecord{..})] @=? result
  where
    _event = LogEvent "This is a test"
    _level = Info
    _position = Just $ Location
        { lPackage = "main"
        , lModule = "Test.Loot.Log.Rio.NoContext"
        , lLine = thisLine - 9
        }
