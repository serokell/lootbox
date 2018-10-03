{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}

module Test.Loot.Log.Rio where

import Loot.Base.HasLens (HasTaggedGetter (..), HasTaggedLens (..))
import Loot.Base.Test (thisLine)
import Loot.Log (Level (..), LoggingImpl, localLogCtx, logInfo, tags)
import Loot.Log.Internal.Context (LogContext (..))
import Loot.Log.Internal.Core (LogEvent (..), LogRecord (..))
import Loot.Log.Internal.Location (Location (..))
import Loot.Log.Rio ()

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


type ReaderCtx = (LoggingImpl IO, LogContext)

instance HasTaggedGetter (LoggingImpl IO) ReaderCtx (LoggingImpl IO) where
    getterOf = _1

instance HasTaggedGetter LogContext ReaderCtx LogContext
instance HasTaggedLens LogContext ReaderCtx ReaderCtx LogContext LogContext where
    lensOf = _2


unit_log :: Assertion
unit_log = do
    (logImpl, ref) <- mkIORefLoggingImpl
    withRIO (logImpl, mempty :: LogContext) $ do
        logInfo "This is a test"
    result <- readIORef ref
    [(mempty, LogRecord{..})] @=? result
  where
    _event = LogEvent "This is a test"
    _level = Info
    _position = Just $ Location
        { lPackage = "main"
        , lModule = "Test.Loot.Log.Rio"
        , lLine = thisLine - 9
        }

unit_logWithCtx :: Assertion
unit_logWithCtx = do
    (logImpl, ref) <- mkIORefLoggingImpl
    withRIO (logImpl, mempty :: LogContext) $ do
        logInfo "Empty context"
        localLogCtx (tags %~ ("tag1" :)) $
            logInfo "Nonempty context"
    result <- readIORef ref
    [(ctx2, r2), (ctx1, r1)] @=? result
  where
    ctx1 = LogContext []
    r1 = LogRecord {..}
      where
        _event = LogEvent "Empty context"
        _level = Info
        _position = Just $ Location
            { lPackage = "main"
            , lModule = "Test.Loot.Log.Rio"
            , lLine = thisLine - 14
            }
    ctx2 = LogContext ["tag1"]
    r2 = LogRecord {..}
      where
        _event = LogEvent "Nonempty context"
        _level = Info
        _position = Just $ Location
            { lPackage = "main"
            , lModule = "Test.Loot.Log.Rio"
            , lLine = thisLine - 22
            }
