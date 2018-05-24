{- This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.
 -}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Yaml (decodeFileEither)

import Loot.Config (finalise, option, sub, (?~))
import Loot.Demo.Config (ConfigPart)


configPath :: FilePath
configPath = "code/demo/app/config.yaml"


main :: IO ()
main = do
    cfgPart <- loadConfig
    case finalise (cfgPart & option #timeout ?~ 10) of
        Left os -> do
            putStrLn $
                "Missing mandatory options: "
             <> intercalate ", " os
            exitFailure
        Right cfg -> do
            putTextLn "Config loaded!"
            putTextLn $ "Timeout: " <> show (cfg ^. option #timeout)
            putTextLn $ "Hostname: " <> show (cfg ^. sub #server . option #host)
            putTextLn $ "Port: " <> show (cfg ^. sub #server . option #port)
  where
    loadConfig :: IO ConfigPart
    loadConfig = decodeFileEither configPath >>= \case
        Left e        -> print e >> exitFailure
        Right cfgPart -> pure cfgPart
