{- SPDX-License-Identifier: MPL-2.0 -}

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
{-# LANGUAGE TypeOperators  #-}

module Loot.Config.Define
       ( (=::)
       , chooseBranch
       ) where

import Data.Vinyl.Core (Rec (..))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Vinyl.Derived (Label)

import Loot.Config.Record

-- | Building a record from tuple.
-- This thing appears only in vinyl-0.9, so reimplementing it here.
class RecFromTuple r where
    type IsoRecTuple r :: *
    recFromTuple :: IsoRecTuple r -> r

instance RecFromTuple (Rec (f :: u -> *) '[]) where
    type IsoRecTuple (Rec f '[]) = ()
    recFromTuple () = RNil

instance RecFromTuple (Rec (f :: u -> *) '[a]) where
    type IsoRecTuple (Rec f '[a]) = f a
    recFromTuple a = a :& RNil

instance RecFromTuple (Rec (f :: u -> *) '[a, b]) where
    type IsoRecTuple (Rec f '[a, b]) = (f a, f b)
    recFromTuple (a, b) = a :& b :& RNil

instance RecFromTuple (Rec (f :: u -> *) '[a, b, c]) where
    type IsoRecTuple (Rec f '[a, b, c]) = (f a, f b, f c)
    recFromTuple (a, b, c) = a :& b :& c :& RNil

instance RecFromTuple (Rec (f :: u -> *) '[a, b, c, d]) where
    type IsoRecTuple (Rec f '[a, b, c, d]) = (f a, f b, f c, f d)
    recFromTuple (a, b, c, d) = a :& b :& c :& d :& RNil

instance RecFromTuple (Rec (f :: u -> *) '[a, b, c, d, e]) where
    type IsoRecTuple (Rec f '[a, b, c, d, e]) = (f a, f b, f c, f d, f e)
    recFromTuple (a, b, c, d, e) = a :& b :& c :& d :& e :& RNil

instance RecFromTuple (Rec (f :: u -> *) '[a, b, c, d, e, g]) where
    type IsoRecTuple (Rec f '[a, b, c, d, e, g]) = (f a, f b, f c, f d, f e, f g)
    recFromTuple (a, b, c, d, e, g) = a :& b :& c :& d :& e :& g :& RNil

instance RecFromTuple (Rec (f :: u -> *) '[a, b, c, d, e, g, h]) where
    type IsoRecTuple (Rec f '[a, b, c, d, e, g, h]) = (f a, f b, f c, f d, f e, f g, f h)
    recFromTuple (a, b, c, d, e, g, h) = a :& b :& c :& d :& e :& g :& h :& RNil


-- | Helper typeclass used to define configurations.
-- This is a safer way than just filling empty configuration via lenses
-- since no field may remain unitialized.
class DefineItem (k :: ConfigKind) (i :: ItemKind) where
    -- | Values user provide.
    type ManualItemDefinition k i :: *
    type ManualItemDefinition k i = Item' k i

    -- | Manually define config option.
    (=::) :: Label (ItemLabel i) -> ManualItemDefinition k i -> Item k i
    infixr 0 =::

instance DefineItem 'Final (l ::: t) where
    _ =:: t = ItemOptionF t

instance DefineItem 'Partial (l ::: t) where
    _ =:: t = ItemOptionP t

instance RecFromTuple (ConfigRec k is) =>
         DefineItem k (l ::< is) where
    _ =:: is = ItemSub is

instance ( RecFromTuple (ConfigRec 'Final (SumSelection l ': is))
         , KnownSymbol l
         ) =>
         DefineItem 'Final (l ::+ is) where
    type ManualItemDefinition 'Final (l ::+ is) =
        Rec (Item 'Final) is
    _ =:: is = ItemSum $ ItemOptionF labelName :& is
      where
        labelName = symbolVal (Proxy @l)

-- | Indicates that branch with a given label does not exist.
data BranchNotFound

-- | Find value of branch with the given label.
type family SelectedItem (l :: Symbol) (is :: [ItemKind]) :: * where
    SelectedItem l '[] = BranchNotFound
    SelectedItem l ((l ::- isi) ': is) = ConfigRec 'Final isi
    SelectedItem l ((l0 ::- isi) ': is) = SelectedItem l is

-- | Fill branch items under some sum item.
class DefineItemsSum (l :: Symbol) (is :: [ItemKind]) where
    -- | Fill the branch having a given label with a given value, initialise
    -- other branches as 'Nothing'.
    fillBranch :: Label l -> SelectedItem l is -> ConfigRec 'Final is

instance DefineItemsSum l '[] where
    fillBranch _ _ = RNil

instance ( DefineItemsSum l is
         , SelectedItem l ((l0 ::- isi) ': is) ~ SelectedItem l is
         ) =>
         DefineItemsSum l ((l0 ::- isi) ': is) where
    fillBranch l v = ItemBranchF Nothing :& fillBranch l v

instance {-# OVERLAPPING #-}
         (LabelsKnown is, EmptyBranch is, DefineItemsSum l is) =>
         DefineItemsSum l ((l ::- isi) ': is) where
    fillBranch _ v = ItemBranchF (Just v) :& emptyBranch

-- | Expect all elements to be final branch items and fill them with 'Nothing'.
class EmptyBranch (is :: [ItemKind]) where
    emptyBranch :: ConfigRec 'Final is

instance EmptyBranch '[] where
    emptyBranch = RNil

instance EmptyBranch is => EmptyBranch ((l ::- iss) ': is) where
    emptyBranch = ItemBranchF Nothing :& emptyBranch

-- | Initialise the branch having a given label with a given value.
chooseBranch
    :: (DefineItemsSum l is, RecFromTuple (SelectedItem l is))
    => Label l -> IsoRecTuple (SelectedItem l is) -> ConfigRec 'Final is
chooseBranch l d = fillBranch l (recFromTuple d)

-- | Start defining your config with this function.
define :: (ConfigRec k d ~ c, RecFromTuple c) => IsoRecTuple c -> c
define = recFromTuple

-----------------------
-- Test sample
-----------------------

type SampleConfig =
    [ "a" ::: Int
    , "b" ::<
       '[ "c" ::: Double
        , "d" ::: Text
        ]
    , "e" ::<
       '[]
    , "f" ::+
        [ "f1" ::-
           '[ "g" ::: Int
            , "h" ::: ()
            ]
        , "f2" ::- '[]
        ]
    ]

_sampleConfig :: ConfigRec 'Final SampleConfig
_sampleConfig = define
    ( #a =:: 5
    , #b =:: define
       ( #c =:: 1
       , #d =:: ""
       )
    , #e =:: define ()
    , #f =:: chooseBranch #f1
       ( #g =:: 0
       , #h =:: ()
       )
    )
