{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}

-- | Basic "has" lenses, extracted from the 'ether' package.
module Loot.Base.HasLens
    ( HasLens(..)
    , HasLens'
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
