{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
module Loot.Log.CPS
       ( -- * Continuation-passing style function for 'Logging' capability 
         withLogging
       , withConfigAction
       , withLogCombined
       ) where

import Loot.Log.Actions
import Loot.Log.Config (LogConfig (..), BackendConfig (..))
import Loot.Log.Internal

import Colog.Core.Action (LogAction(..), cfilter)
import Control.Monad.Cont
import Control.Monad.Reader (withReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Fmt ((+||), (||+))
import Monad.Capabilities (CapImpl(..), HasNoCap, CapsT, addCap)

-- | Adds 'Logging' capability from a 'LogConfig'
withLogging
    :: forall m caps a. (MonadBaseControl IO m, MonadIO m, HasNoCap Logging caps)
    => LogConfig
    -> NameSelector
    -> CapsT (Logging ': caps) m a
    -> CapsT caps m a
withLogging config nameSel foll =
    withConfigAction config $ \logAction -> do
        let loggingImpl :: CapImpl Logging '[] m
            loggingImpl = CapImpl $ hoistLogging liftIO $
                fromLogAction nameSel logAction
        withReaderT (addCap loggingImpl) $ do
            logDebug $ "Logging started, with config: "+||config||+""
            foll

-- | Creates a 'LogAction', in continuation-passing style, from a 'LogConfig'
withConfigAction
    :: (MonadBaseControl IO n, MonadIO m)
    => LogConfig
    -> (LogAction m Message -> n r) -> n r
withConfigAction LogConfig {..} = runCont $ 
    cfilter predicate <$> cont (withLogCombined $ map makeContAction backends)
  where
    makeContAction = \case
        StdErr -> withLogMessageStderr
        File path -> withLogMessageFile path
        Syslog syslogConfig -> withLogMessageSyslog syslogConfig
    predicate msg = msgSeverity msg >= minSeverity

-- | Combines multiple continuation-passing style 'LogAction's in one
withLogCombined
    :: (Monad m)
    => [(LogAction m msg -> n r) -> n r]
    -> (LogAction m msg -> n r) -> n r
withLogCombined lst = runCont $ (mconcat <$> mapM cont lst)
