{-# LANGUAGE RecordWildCards #-}
module Loot.Log.Component
       ( allocateLogging
       ) where

import Loot.Log.Actions
import Loot.Log.Config
import Loot.Log.Internal

import Colog.Core.Action (LogAction (..), cfilter)
import Colog.Syslog (mkSyslogHandler, shClose)
import Control.Monad.Component (ComponentM, buildComponent, buildComponent_)
import Fmt ((+|), (|+))
import System.IO (hClose)

-- | Builds a 'Logging' 'ComponentM' with the name "logging" from a 'LogConfig'
allocateLogging
    :: MonadIO m
    => LogConfig
    -> NameSelector
    -> ComponentM (Logging m)
allocateLogging LogConfig {..} nameSel = do
    actionList <- zipWithM allocateBackend [1..] backends 
    let predicate msg = msgSeverity msg >= minSeverity
        logAction = cfilter predicate $ mconcat actionList
    buildComponent_ "logging" . pure $ fromLogAction nameSel logAction

allocateBackend :: MonadIO m => Int -> BackendConfig -> ComponentM (LogAction m Message)
allocateBackend n = \case
    StdOut -> buildComponent_ (buildName "stdout") $ pure logMessageStdout
    StdErr -> buildComponent_ (buildName "stderr") $ pure logMessageStderr
    File path -> logMessageFile <$>
        buildComponent (buildName "file") (openFile path AppendMode) hClose
    Syslog syslogConfig -> logMessageSyslog <$>
        buildComponent (buildName "syslog") (mkSyslogHandler syslogConfig) shClose
  where
    buildName :: Text -> Text
    buildName backendType = "logging backend #"+|n|+": "+|backendType|+""
