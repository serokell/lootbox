{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}

-- | Utilities for the network. Move them to some lootbox-swalka
-- later.

module Loot.Network.Utils
    ( LensLike
    , HasLens (..)
    , HasLens'
    , findM
    ) where

import Data.Coerce (coerce)
import Data.Tagged (Tagged (..))

type LensLike f s t a b = (a -> f b) -> s -> f t

class HasLens tag outer inner | tag outer -> inner where
    lensOf :: Lens' outer inner

instance HasLens a a a where
    lensOf = id

instance HasLens t (Tagged t a) a where
    lensOf = \f -> fmap coerce . f . coerce

type HasLens' s a = HasLens a s a

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ []     = pure Nothing
findM p (x:xs) = ifM (p x) (pure $ Just x) (findM p xs)
