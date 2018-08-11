{-# LANGUAGE CPP #-}

-- | Down to earth logging for the lazy ones
--
-- No configuration, just use functions from this module instead of @putStrLn@
-- and they will do the rest. All messages above @Debug@ are printed to @stderr@.
-- Compile with @-DDEBUG@ to get the ones from @Debug@ as well.
module Loot.Log.Rustic
       ( Level (..)

       , logDebug
       , logInfo
       , logNotice
       , logWarning
       , logError
       ) where

import Prelude hiding (log)

import Data.Text.IO (hPutStrLn)
import Data.Time (formatTime, defaultTimeLocale, getZonedTime)
import Fmt ((+||), (||+), (+|), (|+))
import System.IO (stderr)

import Loot.Log.Internal (Level (..), LogEvent (..), nameFromStack)


logWith :: (MonadIO m) => Level -> CallStack -> LogEvent -> m ()
logWith level cs ev =
    #ifdef DEBUG
    log
    #else
    case level of
        Debug -> pure ()
        _     -> log
    #endif
  where
    name = nameFromStack cs
    msg = do
        time <- formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%z" <$> liftIO getZonedTime
        pure $ ""+|time|+" ["+||level||+"] "+|name|+": "+|getLogEvent ev|+""
    log = msg >>= liftIO . hPutStrLn stderr

-- | Log an event with the @Debug@ level
logDebug :: (HasCallStack, MonadIO m) => LogEvent -> m ()
logDebug = logWith Debug callStack

-- | Log an event with the @Info@ level
logInfo :: (HasCallStack, MonadIO m) => LogEvent -> m ()
logInfo = logWith Info callStack

-- | Log an event with the @Notice@ level
logNotice :: (HasCallStack, MonadIO m) => LogEvent -> m ()
logNotice = logWith Notice callStack

-- | Log an event with the @Warning@ level
logWarning :: (HasCallStack, MonadIO m) => LogEvent -> m ()
logWarning = logWith Warning callStack

-- | Log an event with the @Error@ level
logError :: (HasCallStack, MonadIO m) => LogEvent -> m ()
logError = logWith Error callStack
