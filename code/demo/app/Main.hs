{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Universum

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
