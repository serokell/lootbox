{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards      #-}

-- | Internals of logging.
module Loot.Log.Internal.Logging
       ( LogEvent

       , Logging (..)
       , hoistLogging
       , MonadLogging (..)

       , logNameSelL

       , logDebug
       , logInfo
       , logNotice
       , logWarning
       , logError
       , logEmergency
       , logAlert
       , logCritical

       , ModifyLogName (..)
       , WithLogging
       , modifyLogName

       , toLogAction
       , fromLogAction
         -- re-exports
       , Severity (..)
       ) where

import Prelude hiding (toList)

import Loot.Log.Internal.Name
import Loot.Log.Internal.Message

import Fmt.Internal (FromBuilder (fromBuilder))
import GHC.Stack (callStack)
import Lens.Micro (ASetter', sets)
import Monad.Capabilities (makeCap)

import Colog.Core.Action (LogAction(..))
import Colog.Syslog.Priority (Severity (..))

-- | An event that gets logged (in most cases just text).
newtype LogEvent = LogEvent { getLogEvent :: Text }

instance IsString LogEvent where
    fromString = LogEvent . fromString

instance FromBuilder LogEvent where
    fromBuilder = LogEvent . fromBuilder

-- | Logging capability.
data Logging m = Logging
    { _log     :: Message -> m ()
    , _logName :: m NameSelector
    }

makeCap ''Logging

toLogAction :: Logging m -> LogAction m Message
toLogAction = LogAction . _log

fromLogAction :: (Monad m) => NameSelector -> LogAction m Message -> Logging m
fromLogAction nameSel logAction = Logging
    { _log = unLogAction logAction
    , _logName = return nameSel
    }

hoistLogging :: (forall a. m a -> n a) -> Logging m -> Logging n
hoistLogging hst logging =
    logging { _log = \msg -> hst (_log logging msg)
            , _logName = hst (_logName logging)
            }

logNameSelL :: Functor m => ASetter' (Logging m) NameSelector
logNameSelL = sets $ \f l -> l{ _logName = fmap f (_logName l) }

-- | Helper function for use 'logDebug' and family.
logWith :: (Monad m, MonadLogging m) => Severity -> CallStack -> LogEvent -> m ()
logWith msgSeverity cs ev = do
    msgName <- selectLogName cs <$> logName
    let msgContent = getLogEvent ev
    log $ Message {..}

logDebug :: (HasCallStack, Monad m, MonadLogging m) => LogEvent -> m ()
logDebug = logWith Debug callStack

logInfo :: (HasCallStack, Monad m, MonadLogging m) => LogEvent -> m ()
logInfo = logWith Info callStack

logNotice :: (HasCallStack, Monad m, MonadLogging m) => LogEvent -> m ()
logNotice = logWith Notice callStack

logWarning :: (HasCallStack, Monad m, MonadLogging m) => LogEvent -> m ()
logWarning = logWith Warning callStack

logError :: (HasCallStack, Monad m, MonadLogging m) => LogEvent -> m ()
logError = logWith Error callStack

logEmergency :: (HasCallStack, Monad m, MonadLogging m) => LogEvent -> m ()
logEmergency = logWith Emergency callStack

logAlert :: (HasCallStack, Monad m, MonadLogging m) => LogEvent -> m ()
logAlert = logWith Alert callStack

logCritical :: (HasCallStack, Monad m, MonadLogging m) => LogEvent -> m ()
logCritical = logWith Critical callStack

-- | Allows to manipulate with logger name.
class MonadLogging m => ModifyLogName m where
    modifyLogNameSel :: (NameSelector -> NameSelector) -> m a -> m a

type WithLogging m = (MonadLogging m, ModifyLogName m)

modifyLogName :: ModifyLogName m => (Name -> Name) -> m a -> m a
modifyLogName f = modifyLogNameSel (GivenName . f . selectLogName callStack)
