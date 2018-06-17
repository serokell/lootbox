{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- | Usable logging abstraction.
module Loot.Log
       ( Level (..)
       , Name
       , NameSelector (..)
       , _GivenName

       , Logging (..)
       , MonadLogging
       , log
       , logName
       , hoistLogging

       , logDebug
       , logInfo
       , logNotice
       , logWarning
       , logError

       , ModifyLogName (..)
       , WithLogging
       , modifyLogName
       ) where

import Loot.Log.Internal