module Loot.Log.Rio.Internal
       ( rioEmit
       , rioEmitEmptyCtx

       , rioAskLogCtx
       , rioLocalLogCtx
       ) where

import Loot.Base.HasLens (HasGetter, HasLens', getterOf, lensOf)

import Loot.Log
import Loot.Log.Internal.Context (LogContext)
import Loot.Log.Internal.Core (LogRecord (..))


-- | RIO-style implementation of 'MonadLogging.emit'.
--
-- For you to use it your monad @m@ should be isomorphic to @ReaderT ctx IO@ and
-- @ctx@ needs to contain @'LoggingImpl' IO@ (this is what backend initialisation
-- functions return) and 'LogContext' somewhere in it.
rioEmit
    :: forall m ctx.
       ( HasGetter ctx (LoggingImpl IO)
       , HasGetter ctx LogContext
       , MonadReader ctx m
       , MonadIO m
       )
    => LogRecord
    -> m ()
rioEmit r = do
    loggingImpl <- view (getterOf @(LoggingImpl IO))
    ctx <- view (getterOf @LogContext)
    liftIO $ loggingImpl ctx r

-- | RIO-style implementation of 'MonadLogging.emit' without access to 'LogContext'.
--
-- For you to use it your monad @m@ should be isomorphic to @ReaderT ctx IO@ and
-- @ctx@ needs to contain @'LoggingImpl' IO@ (this is what backend initialisation
-- functions return)somewhere in it.
rioEmitEmptyCtx
    :: forall m ctx.
       ( HasGetter ctx (LoggingImpl IO)
       , MonadReader ctx m
       , MonadIO m
       )
    => LogRecord
    -> m ()
rioEmitEmptyCtx r = do
    loggingImpl <- view (getterOf @(LoggingImpl IO))
    liftIO $ loggingImpl mempty r


-- | RIO-style implementation of 'MonadLoggingWithContext.askLoggingCtx'.
--
-- For you to use it your monad @m@ should be isomorphic to @ReaderT ctx n@
-- and @ctx@ needs to contain 'LogContext' somewhere in it.
rioAskLogCtx
    :: forall m ctx. (HasGetter ctx LogContext, MonadReader ctx m)
    => m LogContext
rioAskLogCtx = view (getterOf @LogContext)

-- | RIO-style implementation of 'MonadLoggingWithContext.localLoggingCtx'.
--
-- For you to use it your monad @m@ should be isomorphic to @ReaderT ctx n@
-- and @ctx@ needs to contain 'LogContext' somewhere in it.
rioLocalLogCtx
    :: forall m ctx a. (HasLens' ctx LogContext, MonadReader ctx m)
    => (LogContext -> LogContext)
    -> (m a -> m a)
rioLocalLogCtx f = local (lensOf @LogContext %~ f)
