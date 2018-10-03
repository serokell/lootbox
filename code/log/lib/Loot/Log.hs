{-# LANGUAGE DataKinds #-}

-- | Usable logging abstraction.
--
-- To use @log*@ functions you need to be inside a monad that imlements 'MonadLogging'.
-- There is a number of ways to achieve this:
--
-- 1. If you are using the 'caps' library, then see ''Loot.Log.Caps''.
-- 2. If you are following the RIO approach to monad stacks, then see ''Loot.Log.Rio''.
module Loot.Log
       ( Level (..)

       , LogEvent

       , LogRecord
       , event
       , level
       , position

       , LogContext
       , tags

       , Logging (..)
       , MonadLogging (..)
       , hoistLogging

       , HasLogContext (..)
       , MonadHasLogContext (..)

       , MonadLoggingWithContext

       , logDebug
       , logInfo
       , logNotice
       , logWarning
       , logError

       , LoggingImpl

       , withLogTag

       , module Fmt
       ) where

import Fmt ((+|), (|+), (+||), (||+))
import GHC.Stack (HasCallStack, callStack)

import Loot.Log.Internal.Context (LogContext, HasLogContext (..), MonadHasLogContext (..),
                                  tags)
import Loot.Log.Internal.Core (Level (..), LogEvent, LogRecord, Logging (..), MonadLogging (..),
                               MonadLogging, logWith, MonadLoggingWithContext, event, level, position)


-- | Hoist 'Logging' implementation.
hoistLogging :: (forall a. m a -> n a) -> Logging m -> Logging n
hoistLogging hst logging = Logging
    { _emit = \r -> hst (_emit logging r)
    }


-- | Log an event with the 'Debug' level.
logDebug :: (HasCallStack, Monad m, MonadLogging m) => LogEvent -> m ()
logDebug = logWith Debug callStack

-- | Log an event with the 'Info' level.
logInfo :: (HasCallStack, Monad m, MonadLogging m) => LogEvent -> m ()
logInfo = logWith Info callStack

-- | Log an event with the 'Notice' level.
logNotice :: (HasCallStack, Monad m, MonadLogging m) => LogEvent -> m ()
logNotice = logWith Notice callStack

-- | Log an event with the 'Warning' level.
logWarning :: (HasCallStack, Monad m, MonadLogging m) => LogEvent -> m ()
logWarning = logWith Warning callStack

-- | Log an event with the 'Error' level.
logError :: (HasCallStack, Monad m, MonadLogging m) => LogEvent -> m ()
logError = logWith Error callStack


-- | Logging implementation.
--
-- This is what the initialisation function of your logging backend of choice
-- will return.
type LoggingImpl m = LogContext -> LogRecord -> m ()


-- | Modify 'LogContext' by adding a tag to it.
withLogTag :: MonadHasLogContext m => Text -> m a -> m a
withLogTag t = localLogCtx (tags %~ (t :))
