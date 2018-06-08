{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Usable logging abstraction.
module Loot.Log
       ( Level (..)
       , Name

       , Logging (..)
       , MonadLogging
       , log

       , logDebug
       , logInfo
       , logNotice
       , logWarning
       , logError
       ) where

import Loot.Log.Internal
