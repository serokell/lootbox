-- | Helpers for defining instances in "ReaderT over IO" approach to monad
-- stack.

module Loot.Log.Rio
    ( LoggingIO
    , defaultLog
    , defaultLogName
    , defaultModifyLogNameSel
    ) where

import Control.Lens (views)
import Ether.Internal (HasLens (..))

import Loot.Log.Internal (Level, Logging (..), Name, NameSelector, logNameSelL)

-- | We provide default implementations for @LoggingIO@ because it facilitates
-- movement of logging capability between different monads (and also we will
-- hardly have logging which requires something more complex than 'IO').
-- Use 'hoistLogging' to lift this to required monad.
type LoggingIO = Logging IO

-- | Default implementation of 'MonadLogging.log' (generated with 'makeCap').
defaultLog
    :: forall m ctx.
       (HasLens LoggingIO ctx LoggingIO, MonadReader ctx m, MonadIO m)
    => Level -> Name -> Text -> m ()
defaultLog l n t = do
    lg <- views (lensOf @LoggingIO) _log
    liftIO $ lg l n t

-- | Default implementation of 'MonadLogging.logName' (generated with
-- 'makeCap').
defaultLogName
    :: forall m ctx.
       (HasLens LoggingIO ctx LoggingIO, MonadReader ctx m, MonadIO m)
    => m NameSelector
defaultLogName = view (lensOf @LoggingIO) >>= liftIO . _logName

-- | Default implementation of 'modifyLogNameSel' stack.
defaultModifyLogNameSel
    :: forall m ctx a.
       (HasLens LoggingIO ctx LoggingIO, MonadReader ctx m)
    => (NameSelector -> NameSelector) -> m a -> m a
defaultModifyLogNameSel f =
    local (lensOf @LoggingIO . logNameSelL %~ f)