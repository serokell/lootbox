{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Logging interface for RIO-style monad stacks.
--
-- If your monad @m@ is isomorphic to @ReaderT ctx IO@, you can obtain an instance
-- of 'MonadLoggingWithContext'. For this you need to:
--
-- 1. Make sure that your @ctx@ contains @'LoggingImpl' IO@.
-- 2. Provide a 'HasTaggedGetter' instance for locating the @LoggingImpl@ in your @ctx@.
-- 3. Provide a 'HasTaggedLens' instance for locating @LogContext@ in your @ctx@.
--
-- In case there is no place in your monad stack to store the logging context,
-- you can (while it is not advised) use ''Loot.Log.Rio.NoContext'' instead.
module Loot.Log.Rio
    ( rioEmit

    , rioAskLogCtx
    , rioLocalLogCtx

    , module Loot.Log
    ) where

import Loot.Base.HasLens (HasGetter, HasLens')

import Loot.Log
import Loot.Log.Internal.Context (LogContext, MonadHasLogContext (..))
import Loot.Log.Internal.Core (MonadLogging (..))
import Loot.Log.Rio.Internal (rioAskLogCtx, rioEmit, rioLocalLogCtx)


--
-- Orphan instances for RIO
--

instance ( HasGetter ctx (LoggingImpl IO)
         , HasGetter ctx LogContext
         , MonadReader ctx m
         , MonadIO m
         ) => MonadLogging m where
    emit = rioEmit

instance ( HasLens' ctx LogContext
         , MonadReader ctx m
         ) => MonadHasLogContext m where
    askLogCtx = rioAskLogCtx
    localLogCtx = rioLocalLogCtx
