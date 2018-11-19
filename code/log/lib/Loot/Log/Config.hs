{-# LANGUAGE RecordWildCards #-}
module Loot.Log.Config
       ( -- * Configurations
         LogConfig (..)
       , BackendConfig (..)
       , basicConfig
         -- * Loading functions
       , loadConfigSafe
       , loadConfigDefault
       ) where

import Loot.Log.Internal

import Colog.Syslog.Config

import Data.Aeson (FromJSON (..), ToJSON (..), withObject, (.:), (.:?), (.!=),
    object, (.=))
import Data.Aeson.Types (Value (Object))
import Data.Yaml (decodeFileEither)
import Fmt ((+|), (|+))

data LogConfig = LogConfig
    { backends    :: [BackendConfig]
    , minSeverity :: Severity
    } deriving (Show, Eq)

data BackendConfig
    = StdOut
    | StdErr
    | File FilePath
    | Syslog SyslogConfig
    deriving (Show, Eq)

-- | Basic configuration: just prints log messages to stdout, without filtering
basicConfig :: LogConfig
basicConfig = LogConfig [StdOut] Debug

-- | Loads a 'LogConfig' from a Yaml file. Returns 'basicConfig' is something
-- goes wrong. It's equivalent to do: 'loadConfigDefault' 'basicConfig'
loadConfigSafe :: FilePath -> IO LogConfig
loadConfigSafe = loadConfigDefault basicConfig

-- | Loads a 'LogConfig' from a Yaml file. Returns the default config in case
-- something goes wrong
loadConfigDefault
    :: LogConfig     -- ^ Default config, to use in case of failed loading
    -> FilePath      -- ^ FilePath to the Yaml file containing the config
    -> IO LogConfig  -- ^ Resulting configuration
loadConfigDefault defaultConfig filePath =
    fromRight defaultConfig <$> decodeFileEither filePath

-- JSON instances

instance FromJSON LogConfig where
    parseJSON = withObject "LogConfig" $ \v -> LogConfig
        <$> v .:? "backends"     .!= [StdOut]
        <*> v .:? "min-severity" .!= Debug

instance ToJSON LogConfig where
    toJSON LogConfig {..} = object
        [ "backends"     .= backends
        , "min-severity" .= minSeverity
        ]

instance FromJSON BackendConfig where
    parseJSON = withObject "BackendConfig" $ \v -> do
        destType <- v .: "type"
        case destType :: Text of
            "stdout" -> return StdOut
            "stderr" -> return StdErr
            "file" -> File <$> v .: "path"
            "syslog" -> Syslog <$> parseJSON (Object v) -- short-circuiting: it will 
                        -- avoid adding a "config" key and parse SyslogConfig directly
            t -> fail $ "Parsing BackendConfig failed: unknown backend type: \""+|t|+"\""

instance ToJSON BackendConfig where
    toJSON = \case
        StdOut -> object
            [ "type" .= ("stdout" :: Text)
            ]
        StdErr -> object
            [ "type" .= ("stderr" :: Text)
            ]
        File path -> object
            [ "type" .= ("file" :: Text)
            , "path" .= path
            ]
        Syslog SyslogConfig {..} -> object
            [ "type"      .= ("syslog" :: Text)
            -- short-circuiting: the next 3 pairs belong to the 'SyslogConfig'
            , "collector" .= collector
            , "facility"  .= facility
            , "app-name"  .= appName
            ]

-- Semigroup and Monoid instance

instance Semigroup LogConfig where
    lc1 <> lc2 = LogConfig
        { backends    = backends lc1 <> backends lc2
        , minSeverity = min (minSeverity lc1) (minSeverity lc2)
        }

instance Monoid LogConfig where
    mempty = LogConfig
        { backends    = []
        , minSeverity = Emergency
        -- ^ the highest severity, so that the 'Monoid' laws are satisfied
        }
    mappend = (<>)
