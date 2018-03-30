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
       , logWarning
       , logError
       , logCritical
       ) where

import Loot.Log.Internal
