module Loot.Tools.Paths
       (
        getDataDir
       ) where

import System.Directory (XdgDirectory (XdgData), getXdgDirectory)

-- | Path to the directory used by Ale to store data.
getDataDir :: MonadIO m => m FilePath
getDataDir = liftIO $ getXdgDirectory XdgData dataDirName
  where
    dataDirName :: String
    dataDirName = "loot"
