{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- | Basic "has" lenses, extracted from the 'ether' package.
module Loot.Base.HasLens
    ( HasLens (lensOf)
    , HasLenses
    , HasCtx
    , glensOf
    ) where

-- | Class for those @s@ that contain a modifiable @a@ in them.
class HasLens s a where
    lensOf :: Lens' s a

instance HasLens a a where
    lensOf = id

type family HasLenses s as :: Constraint where
    HasLenses s '[] = ()
    HasLenses s (a : as) = (HasLens s a, HasLenses s as)

type HasCtx ctx m subs = (MonadReader ctx m, HasLenses ctx subs)

-- | A flexible variation of 'lensOf' parametrised in a phantom type.
glensOf :: forall tag c ctx. HasLens ctx (c tag) => Lens' ctx (c tag)
glensOf = lensOf
