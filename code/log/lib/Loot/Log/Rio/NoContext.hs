{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Logging interface for RIO-style monad stacks without 'LoggingContext'.
--
-- If your monad @m@ is isomorphic to @ReaderT ctx IO@, you can obtain an instance
-- of 'MonadLogging'. For this you need to:
--
-- 1. Make sure that your @ctx@ contains @'LoggingImpl' IO@.
-- 2. Provide a 'HasTaggedGetter' instance for locating the @LoggingImpl@ in your @ctx@.
--
-- Using this module is not recommended. Please, consider using ''Loot.Log.Rio''.
module Loot.Log.Rio.NoContext
    ( rioEmitEmptyCtx

    , module Loot.Log
    ) where

import Loot.Base.HasLens (HasGetter)

import Loot.Log
import Loot.Log.Internal.Core (MonadLogging (..))
import Loot.Log.Rio.Internal (rioEmitEmptyCtx)


--
-- Orphan instances for RIO
--

instance ( HasGetter ctx (LoggingImpl IO)
         , MonadReader ctx m
         , MonadIO m
         ) => MonadLogging m where
    emit = rioEmitEmptyCtx
