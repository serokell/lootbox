{- This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.
 -}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Yaml (decodeFileEither)
import Fmt (listF, (+|), (|+))
import Monad.Capabilities (emptyCaps)

import Loot.Config (finalise, option, sub)
import Loot.Demo.Config (ConfigPart, defaultConfig)
import Loot.Log.Caps (logDebug, logError, logInfo, withLogging)
import Loot.Log.Impure.Warper (mkLoggingImpl)


configPath :: FilePath
configPath = "code/demo/app/config.yaml"


main :: IO ()
main = usingReaderT emptyCaps $ do
    cfgPart <- loadConfig
    case finalise (defaultConfig <> cfgPart) of
        Left os -> do
            -- Config could not be loaded, so we initialise default logging
            -- configuration, because it is better than nothing.
            loggingImpl <- mkLoggingImpl mempty
            withLogging loggingImpl $ do
                logError $ "Missing mandatory options: "+|listF os|+""
                exitFailure
        Right cfg -> do
            loggingImpl <- mkLoggingImpl (cfg ^. option #logging)
            withLogging loggingImpl $ do
                logDebug "Config loaded"
                logInfo $ "Timeout: "+|cfg ^. option #timeout|+""
                logInfo $ "Hostname: "+|cfg ^. sub #server . option #host|+""
                logInfo $ "Port: "+|cfg ^. sub #server . option #port|+""
  where
    loadConfig :: MonadIO m => m ConfigPart
    loadConfig = liftIO $
        decodeFileEither configPath >>= \case
            Left e        -> print e >> exitFailure
            Right cfgPart -> pure cfgPart
