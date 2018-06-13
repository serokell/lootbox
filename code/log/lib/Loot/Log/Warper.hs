{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | @log-warper@ backend implementation.
module Loot.Log.Warper
       ( prepareLogWarper
       , withLogWarper
       , LW.LoggerConfig
       ) where

import Control.Monad.Reader (withReaderT)
import Data.Aeson (encode)
import Fmt ((+|), (|+))
import Monad.Capabilities (CapImpl (CapImpl), CapsT, HasNoCap, addCap)

import Loot.Log.Internal (Level (..), Logging (..), NameSelector (..), hoistLogging, logDebug)

import qualified System.Wlog as LW

-- | Create 'Logging' using log-warper.
-- Given config may be expanded - result of that is returned as well.
prepareLogWarper
    :: (MonadIO m, MonadIO n)
    => LW.LoggerConfig -> NameSelector -> m (LW.LoggerConfig, Logging n)
prepareLogWarper logCfg nameSel = do
    let finalCfg = logCfg <> defaultLogCfg
    LW.setupLogging Nothing finalCfg
    let logging = Logging
            { _log = \lvl name text ->
                let name' = show name
                    lvl' = mapLevel lvl
                in liftIO $ LW.dispatchMessage (LW.LoggerName name') lvl' text
            , _logName = pure nameSel
            }
    return (finalCfg, logging)

-- | Add 'Logging' capability.
withLogWarper :: forall m caps a. (MonadIO m, HasNoCap Logging caps)
              => LW.LoggerConfig  -- ^ @log-warper@ configuration
              -> CapsT (Logging ': caps) m a -> CapsT caps m a
withLogWarper logCfg cont = do
    (finalCfg, logging) <- prepareLogWarper logCfg CallstackName
    let loggingImpl :: CapImpl Logging '[] m
        loggingImpl = CapImpl $ hoistLogging liftIO logging
    withReaderT (addCap loggingImpl) $ do
        logDebug "Logging started"
        let cfgText = decodeUtf8 (encode finalCfg) :: Text
        logDebug $ "Logging config: "+|cfgText|+""
        cont


-- | Map @loot-log@ severity level to @log-warper@ severity.
mapLevel :: Level -> LW.Severity
mapLevel Debug   = LW.Debug
mapLevel Info    = LW.Info
mapLevel Notice  = LW.Notice
mapLevel Warning = LW.Warning
mapLevel Error   = LW.Error

-- | Reasonable defaults for 'LoggerConfig'.
defaultLogCfg :: LW.LoggerConfig
defaultLogCfg = LW.productionB & LW.lcTermSeverityOut .~ Just mempty
                               & LW.lcTermSeverityErr .~ Just LW.allSeverities
                               & LW.lcTree .~ defaultTree
  where
    defaultTree = mempty & LW.ltSeverity .~ Just LW.infoPlus
