-- | Utilities for locating files according to the XDG Base Directory Specification.
--
-- A good way to reuse this module is to create a module called @Paths@ in your
-- project and define in it:
--
-- @
-- appName :: String
-- appName = "\<your app name\>"
--
-- getDataDir :: MonadIO m => m FilePath
-- getDataDir = 'getDataDirFor' appName
--
-- getConfigDir :: MonadIO m => m FilePath
-- getConfigDir = 'getConfigDirFor' appName
--
-- getCacheDir :: MonadIO m => m FilePath
-- getCacheDir = 'getCacheDirFor' appName
-- @
--
-- And then make sure your application places files in the right directories.
module Loot.Bin.Paths
       ( getDataDirFor
       , getConfigDirFor
       , getCacheDirFor
       ) where

import System.Directory (XdgDirectory (XdgCache, XdgConfig, XdgData), getXdgDirectory)
import System.IO (FilePath)


-- | Returns the path to the data directory for your application.
getDataDirFor :: MonadIO m => String -> m FilePath
getDataDirFor = liftIO . getXdgDirectory XdgData

-- | Returns the path to the config directory for your application.
getConfigDirFor :: MonadIO m => String -> m FilePath
getConfigDirFor = liftIO . getXdgDirectory XdgConfig

-- | Returns the path to the cache directory for your application.
getCacheDirFor :: MonadIO m => String -> m FilePath
getCacheDirFor = liftIO . getXdgDirectory XdgCache
