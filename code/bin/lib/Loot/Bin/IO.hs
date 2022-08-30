-- | Utilities for performing IO
module Loot.Bin.IO
       ( openFileOrStdin
       , withFileOrStdin
       ) where

import Control.Exception.Safe (bracket)
import System.IO (Handle, IOMode (ReadMode), hClose, stdin)


-- | Like 'openFile' but if the filename is @-@ returns 'stdin'.
--
-- Always opens the file in 'ReadMode'.
openFileOrStdin :: MonadIO m => FilePath -> m Handle
openFileOrStdin "-"  = pure stdin
openFileOrStdin name = openFile name ReadMode

-- | Like 'withFile', but also like 'openFileOrStdin'.
withFileOrStdin :: (MonadIO m, MonadMask m) => FilePath -> (Handle -> m r) -> m r
withFileOrStdin "-"  cont = cont stdin
withFileOrStdin name cont = bracket (openFile name ReadMode) (liftIO . hClose) cont
