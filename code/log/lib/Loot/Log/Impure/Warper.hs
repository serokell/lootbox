{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | @log-warper@ backend implementation.
--
-- Please be warned that this backend is impure as @log-warper@ stores
-- its configuration in a global variable.
module Loot.Log.Impure.Warper
       ( mkLoggingImpl

       , LW.LoggerConfig
       ) where

import Prelude hiding (toList)

import qualified Data.Text as T
import GHC.Exts (toList)
import Fmt ((+|), (|+))

import Loot.Log (LoggingImpl)
import Loot.Log.Internal.Core (Level (..), LogRecord (..), LogEvent (..))
import Loot.Log.Internal.Context (LogContext (..))
import Loot.Log.Internal.Location (Location (..))

import qualified System.Wlog as LW


-- | Initialise @log-warper@ and return a logging implementation.
mkLoggingImpl
    :: forall m n. (MonadIO m, LW.CanLog n)
    => LW.LoggerConfig
    -> m (LoggingImpl n)
mkLoggingImpl userCfg = do
    LW.setupLogging Nothing $ defaultLogCfg <> userCfg

    let loggingImpl LogContext{_tags} LogRecord{_event, _level, _position} =
            LW.dispatchMessage (LW.LoggerName name') lvl' txt'
          where
            name' = formatName _tags _position
            lvl'  = mapLevel (_level)
            txt'  = getLogEvent _event
    return loggingImpl
  where
    formatName tags pos = T.intercalate "." $ reverse tags ++ (locationToList pos)

    locationToList Nothing = ["<unknown location>"]
    locationToList (Just Location{lPackage, lModule, lLine}) =
        lPackage : toList lModule ++ ["line#"+|lLine|+""]


-- | Map @loot-log@ severity level to @log-warper@ severity.
mapLevel :: Level -> LW.Severity
mapLevel Debug   = LW.Debug
mapLevel Info    = LW.Info
mapLevel Notice  = LW.Notice
mapLevel Warning = LW.Warning
mapLevel Error   = LW.Error

-- | Reasonable defaults for 'LoggerConfig'.
defaultLogCfg :: LW.LoggerConfig
defaultLogCfg = LW.productionB & LW.lcTermSeverityOut .~ Just mempty
                               & LW.lcTermSeverityErr .~ Just LW.allSeverities
                               & LW.lcTree .~ defaultTree
  where
    defaultTree = mempty & LW.ltSeverity .~ Just LW.infoPlus
