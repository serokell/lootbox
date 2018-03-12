-- | Capabilities helpers.

module Lootbox.Capabilities
       ( CapImplIODB
       ) where

import Universum

import Control.Monad.Base (MonadBase)
import Monad.Capabilities (CapImpl)

-- | Implementation of a capability that works in arbitrary monad that
-- has an instance of 'MonadIO' and 'MonadMask'.
type CapImplIODB cap icaps = forall m. (MonadIO m, MonadMask m, MonadBase IO m) => CapImpl cap icaps m
