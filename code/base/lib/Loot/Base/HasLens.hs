{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Basic "has" lenses, extracted from the 'ether' package.
module Loot.Base.HasLens
    ( HasTaggedGetter (getterOf)
    , HasGetter

    , HasTaggedLens (lensOf)
    , HasLens
    , HasLens'

    , HasLenses'

    , HasCtx'
    ) where

import Data.Coerce (coerce)
import Data.Tagged (Tagged (..))
import Lens.Micro (SimpleGetter)


-- | Class for those @s@ that contain read-only @a@ in them.
--
-- You can use the default implementation of this class if
-- you implement 'HasLens'.
class HasTaggedGetter tag s a | tag s -> a where
    getterOf :: SimpleGetter s a

    default getterOf :: HasTaggedLens tag s s a a => SimpleGetter s a
    getterOf = lensOf @tag

instance HasTaggedGetter a a a where
    getterOf = id

instance HasTaggedGetter t (Tagged t a) a where
    getterOf = \f -> fmap coerce . f . coerce

type HasGetter s a = HasTaggedGetter a s a


-- | Class for those @s@ that contain a modifiable @a@ in them.
class HasTaggedGetter tag s a => HasTaggedLens tag s t a b | tag s b -> t a where
    lensOf :: Lens s t a b

instance HasTaggedLens a a a a a where
    lensOf = id

instance HasTaggedLens t (Tagged t a) (Tagged t b) a b where
    lensOf = \f -> fmap coerce . f . coerce

type HasLens s t a b = HasTaggedLens a s t a b

type HasLens' s a = HasLens s s a a


type family HasLenses' s as :: Constraint where
    HasLenses' s '[] = ()
    HasLenses' s (a : as) = (HasLens' s a, HasLenses' s as)


type HasCtx' ctx m subs = (MonadReader ctx m, HasLenses' ctx subs)
