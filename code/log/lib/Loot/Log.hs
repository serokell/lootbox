{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Usable logging abstraction.
module Loot.Log
       ( Level (..)
       , Name

       , Logging (..)
       , MonadLogging
       , log
       , logName

       , logDebug
       , logInfo
       , logNotice
       , logWarning
       , logError

       , HasLogName (..)
       , WithLogging
       , modifyLogName
       ) where

import Loot.Log.Internal