{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module provides orphan instance HasLens for config.
module Loot.Config.HasLens  where

import Data.Vinyl (Label (..))
import GHC.TypeLits (ErrorMessage (Text), Symbol, TypeError)

import Loot.Base.HasLens (HasLens (..))
import Loot.Config.Record

----------------------------------------------------------------------------
-- Constraints
----------------------------------------------------------------------------

-- Because there is no publicly availble type family for list concatenation.
type family (++) (as :: [k]) (bs :: [k]) :: [k] where
    (++) a '[] = a
    (++) '[] b = b
    (++) (a ': as) bs = a ': (as ++ bs)

type family RevList (a :: [k]) :: [k] where
    RevList '[] = '[]
    RevList (a ': b) = RevList b ++ '[a]

type family DoesntHaveType f is :: Constraint where
    DoesntHaveType _ '[]       = ()

    DoesntHaveType f ((_ ::: f)  ': is) = TypeError ('Text "DoesntHaveType failed (item type not unique?)")
    DoesntHaveType f ((_ ::: _)  ': is) = DoesntHaveType f is
    DoesntHaveType f ((_ ::< us) ': is) = (DoesntHaveType f us, DoesntHaveType f is)

type family ItemTypeUnique (f :: Type) (is :: [ItemKind]) :: Constraint where
    ItemTypeUnique _ '[]       = ()
    ItemTypeUnique f ((_ ::: f) ': is) = DoesntHaveType f is
    ItemTypeUnique f ((_ ::: _) ': is) = ItemTypeUnique f is
    ItemTypeUnique f ((_ ::< us) ': is) = ItemTypeUnique f (us ++ is)


data SymPathElem = SOption Symbol | SSub Symbol

type family LabelOfType (f :: Type) labels cont (is :: [ItemKind]) :: [SymPathElem] where

    LabelOfType _ _ '[] '[] = TypeError ('Text "PathToType wat?")

    LabelOfType f (_ ': prevLabels) (cont ': prevCont) '[] =
        LabelOfType f prevLabels prevCont cont

    LabelOfType f labels _ ((l ::: f) ': _) =
        'SOption l ': labels

    LabelOfType f labels cont ((l ::: g) ': is) =
        LabelOfType f labels cont is

    LabelOfType f labels cont ((l ::< us) ': is) =
        LabelOfType f ('SSub l ': labels) (is ': cont) us

type family LabelOfTypeS f is where
    LabelOfTypeS f is = RevList (LabelOfType f '[] '[] is)

----------------------------------------------------------------------------
-- Lenses for records
----------------------------------------------------------------------------

class HierarchyLens (path :: [SymPathElem]) is v where
    hlens :: Proxy path -> Lens' (ConfigRec 'Final is) v

instance (HasOption l is v) =>
         HierarchyLens ('SOption l ': lx) is v where

    hlens _ = option (Label :: Label l)

instance (HasSub l is us, HierarchyLens lx us v) =>
         HierarchyLens ('SSub l ': lx) is v where

    hlens _ = sub (Label :: Label l) .
              (hlens (Proxy :: Proxy lx) :: Lens' (ConfigRec 'Final us) v)

----------------------------------------------------------------------------
-- HasLens Instance
----------------------------------------------------------------------------

instance
         ( ItemTypeUnique v is
         , HierarchyLens (LabelOfTypeS v is) is v
         ) =>
         HasLens v (ConfigRec 'Final is) v where
    lensOf = hlens (Proxy :: Proxy (LabelOfTypeS v is))
