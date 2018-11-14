{-# LANGUAGE RecordWildCards #-}
module Loot.Log.Component
       ( allocateLogging
       , prepareLogging
       ) where

import Loot.Log.Actions
import Loot.Log.Config
import Loot.Log.Internal

import Colog.Core.Action (LogAction (..), cfilter)
import Colog.Syslog (mkSyslogHandler, shClose)
import Control.Monad.Component (ComponentM, buildComponent)
import System.IO (hClose)

-- | Builds a 'Logging' 'ComponentM' with the name "logging" from a 'LogConfig'
allocateLogging
    :: MonadIO m
    => LogConfig
    -> NameSelector
    -> ComponentM (Logging m)
allocateLogging config nameSel = do
    (logging, clean) <- liftIO $ prepareLogging config nameSel
    buildComponent "logging" (pure logging) (const clean)

-- | Creates a 'Logging' from a `LogConfig` and the function to free its
-- resources. Mostly to give a replacement for "prepareLogWarper" and "prepareSyslog"
prepareLogging :: MonadIO m => LogConfig -> NameSelector -> IO (Logging m, IO ())
prepareLogging LogConfig {..} nameSel = do
    (logAction, clean) <- liftIO $
        foldrM combineActionAndCleaner (mempty, pass) backends
    let predicate msg = msgSeverity msg >= minSeverity
        logging = fromLogAction nameSel $ cfilter predicate logAction
    return (logging, clean)

-- | Creates a 'LogAction' and a cleaning IO function (if needed) from a
-- 'BackendConfig' and combines these to an existing action-cleaner tuple.
combineActionAndCleaner
    :: MonadIO m
    => BackendConfig
    -> (LogAction m Message, IO ())
    -> IO (LogAction m Message, IO ())
combineActionAndCleaner backConfig (logAction, clean) = case backConfig of
    StdOut -> pure (logAction <> logMessageStdout, clean)
    StdErr -> pure (logAction <> logMessageStderr, clean)
    File path -> do
        handle <- openFile path AppendMode
        return (logAction <> logMessageFile handle, hClose handle >> clean)
    Syslog syslogConfig -> do
        handler <- mkSyslogHandler syslogConfig
        return (logAction <> logMessageSyslog handler, shClose handler >> clean)
