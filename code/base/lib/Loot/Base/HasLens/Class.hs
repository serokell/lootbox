{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Basic "has" lenses, extracted from the 'ether' package.
module Loot.Base.HasLens.Class
    ( HasLens(..)
    , HasLens'
    , HasLenses
    , HasCtx
    ) where

import Data.Coerce (coerce)
import Data.Tagged (Tagged (..))

class HasLens tag outer inner | tag outer -> inner where
    lensOf :: Lens' outer inner

instance HasLens a a a where
    lensOf = id

instance HasLens t (Tagged t a) a where
    lensOf = \f -> fmap coerce . f . coerce

type HasLens' s a = HasLens a s a

type family HasLenses s as :: Constraint where
    HasLenses s '[] = ()
    HasLenses s (a : as) = (HasLens' s a, HasLenses s as)

type HasCtx ctx m subs = (MonadReader ctx m, HasLenses ctx subs)
