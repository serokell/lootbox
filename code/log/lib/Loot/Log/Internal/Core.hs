{-# LANGUAGE ConstraintKinds #-}

module Loot.Log.Internal.Core
       ( LogEvent (..)

       , LogRecord (..)
       , event
       , level
       , position

       , Level (..)

       , Logging (..)
       , MonadLogging (..)
       , logWith

       , MonadLoggingWithContext
       ) where

import Fmt (Buildable (build), (+||), (||+))
import Fmt.Internal (FromBuilder (fromBuilder))
import Lens.Micro.TH (makeLenses)

import Loot.Log.Internal.Context (MonadHasLogContext)
import Loot.Log.Internal.Location (Location, locationFromStack)


-- | An event that gets logged (in most cases just text).
newtype LogEvent = LogEvent { getLogEvent :: Text }
  deriving (Buildable, Eq, Show, Semigroup)

instance IsString LogEvent where
    fromString = LogEvent . fromString

instance FromBuilder LogEvent where
    fromBuilder = LogEvent . fromBuilder


-- | Logging level.
data Level
    = Debug     -- ^ Things nobody should see unless it's explicitly stated.
    | Info      -- ^ Regular information for user.
    | Notice    -- ^ Something that should be more noticable than 'Info'.
    | Warning   -- ^ Suspicious warning conditions.
    | Error     -- ^ Errors.
    deriving (Eq, Generic, Show)

instance Buildable Level where
    build level = ""+||level||+""


-- | 'LogEvent' extended with additional metadata about it.
data LogRecord = LogRecord
    { _event    :: LogEvent
    , _level    :: Level
    , _position :: Maybe Location
    } deriving (Eq, Show)
makeLenses ''LogRecord


-- | Logging capability.
--
-- Providing this capability for a monad @m@ means that you can emit log
-- message in this monad and they will be somehow recorded in it.
-- E.g. in the 'IO' monad they can be written to the console or to a file
-- depending on the configuration.
data Logging m = Logging
    { _emit :: LogRecord -> m ()
    }

-- | Class of monads capable of logging.
class MonadLogging m where
    emit :: LogRecord -> m ()

logWith :: (Monad m, MonadLogging m) => Level -> CallStack -> LogEvent -> m ()
logWith l cs ev = emit $ LogRecord ev l (locationFromStack cs)


-- Being capable of logging with context means being capable of 'Logging' and also
-- keeping track of 'LoggingContext'.
type MonadLoggingWithContext m = (MonadLogging m, MonadHasLogContext m)
