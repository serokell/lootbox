-- | Helpers for defining instances in "ReaderT over IO" approach to monad
-- stack.
{-# LANGUAGE RecordWildCards #-}

module Loot.Log.Rio
    ( LoggingIO
    , defaultLog
    , defaultLogName
    , defaultModifyLogNameSel
    ) where

import Lens.Micro (to)
import Loot.Base.HasLens (HasLens, lensOf)

import Loot.Log.Internal (Logging (..), Message (..), NameSelector, logNameSelL)

-- | We provide default implementations for @LoggingIO@ because it facilitates
-- movement of logging capability between different monads (and also we will
-- hardly have logging which requires something more complex than 'IO').
-- Use 'hoistLogging' to lift this to required monad.
type LoggingIO = Logging IO

-- | Default implementation of 'MonadLogging.log' (generated with 'makeCap').
defaultLog
    :: forall m ctx.
       (HasLens ctx LoggingIO, MonadReader ctx m, MonadIO m)
    => Message -> m ()
defaultLog msg = do
    lg <- view (lensOf . to _log) <$> ask
    liftIO $ lg msg

-- | Default implementation of 'MonadLogging.logName' (generated with
-- 'makeCap').
defaultLogName
    :: forall m ctx.
       (HasLens ctx LoggingIO, MonadReader ctx m, MonadIO m)
    => m NameSelector
defaultLogName = view lensOf >>= liftIO . _logName

-- | Default implementation of 'modifyLogNameSel' method.
defaultModifyLogNameSel
    :: forall m ctx a.
       (HasLens ctx LoggingIO, MonadReader ctx m)
    => (NameSelector -> NameSelector) -> m a -> m a
defaultModifyLogNameSel f =
    local (lensOf @_ @LoggingIO . logNameSelL %~ f)
