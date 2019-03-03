{- SPDX-License-Identifier: MPL-2.0 -}

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

-- | Vinyl records for configuration.
module Loot.Config.Record
       ( ConfigKind (Partial, Final)

       , ItemKind
       , (:::)
       , (::<)
       , (::+)
       , (::-)

       , SumSelection

       , ConfigRec

       , Item'
       , Item (..)

       , ItemType

       , ItemLabel

       , LabelsKnown

       , finalise
       , finaliseDeferredUnsafe
       , complement
       , upcast

       , HasOption
       , option

       , HasSub
       , sub

       , HasSum
       , tree

       , HasBranch
       , branch

       , selection
       ) where

import Data.Default (Default (..))
import Data.Validation (Validation (Failure, Success), toEither)
import Data.Vinyl (Label, Rec ((:&), RNil))
import Data.Vinyl.Lens (type (<:), RecElem, rlens, rreplace)
import Data.Vinyl.TypeLevel (RIndex)
import GHC.TypeLits (AppendSymbol, ErrorMessage ((:<>:), ShowType, Text), KnownSymbol, Symbol,
                     TypeError, symbolVal)

import qualified Text.Show (Show (show))


data ConfigKind
    = Partial
    -- ^ In a partial configuration some of the fields migh be missing, in case
    -- they will be initialised later.
    | Final
    -- ^ A final configuratoin must have all its fields initialised.


-- | Closed kind for items that can be stored in a config.
data ItemKind
    = OptionType Symbol Type
    | SubsectionType Symbol [ItemKind]
    | SumType Symbol [ItemKind]
    | BranchType Symbol [ItemKind]

-- | Type for ordinary configuration options.
--
-- Example: @"timeout" ::: Int@
type (:::) = 'OptionType

-- | Type for configurations subsections.
--
-- Example: @"constants" ::< '[ ... ]@
type (::<) = 'SubsectionType

-- | Type for a tree of configuration possibilities (like a sum-type)
--
-- Example: @"connection" ::+ '[ ... ]@
type (::+) = 'SumType

-- | Type for a branch of a configuration tree (like a sum-type constructor)
--
-- Example: @"ftpConnection" ::- '[ ... ]@
type (::-) = 'BranchType

-- | Type of configuration records of 'ConfigKind' @k@.
type ConfigRec k = Rec (Item k)


-- | Type family that interprets configuration items for vinyl records.
type family Item' (k :: ConfigKind) (i :: ItemKind) where
    Item' 'Partial (l ::: t)  = Maybe t
    Item' 'Final   (l ::: t)  = t
    Item' k        (l ::< is) = ConfigRec k is
    Item' k        (l ::+ is) = ConfigRec k (SumSelection l : is)
    Item' 'Partial (l ::- is) = ConfigRec 'Partial is
    Item' 'Final   (l ::- is) = Maybe (ConfigRec 'Final is)

-- | Defines the tree selection option label from the tree label
type SumSelectionLabel l = AppendSymbol l "Type"

-- | Defines the tree selection option from the tree label
type SumSelection l = SumSelectionLabel l ::: String

-- | Technical first-class wrapper around the type family to make it possible
-- to pattern-match on its constructors.
data Item (k :: ConfigKind) (i :: ItemKind) where
    ItemOptionP   :: Item' 'Partial (l ::: t)  -> Item 'Partial (l ::: t)
    ItemOptionF   :: Item' 'Final   (l ::: t)  -> Item 'Final   (l ::: t)
    ItemSub       :: Item' k        (l ::< is) -> Item k        (l ::< is)
    ItemSum       :: Item' k        (l ::+ is) -> Item k        (l ::+ is)
    ItemBranchP   :: Item' 'Partial (l ::- is) -> Item 'Partial (l ::- is)
    ItemBranchF   :: Item' 'Final   (l ::- is) -> Item 'Final   (l ::- is)

-- | Lens to focus onto the data actually stored inside 'Item'.
cfgItem :: Functor f => (Item' k d -> f (Item' k d)) -> Item k d -> f (Item k d)
cfgItem f (ItemOptionP x)   = ItemOptionP <$> f x
cfgItem f (ItemOptionF x)   = ItemOptionF <$> f x
cfgItem f (ItemSub rec)     = ItemSub <$> f rec
cfgItem f (ItemSum rec)     = ItemSum <$> f rec
cfgItem f (ItemBranchP rec) = ItemBranchP <$> f rec
cfgItem f (ItemBranchF rec) = ItemBranchF <$> f rec


-- | Type family to obtain a label from an 'ItemKind'
type family ItemLabel (i :: ItemKind) where
    ItemLabel (l ::: _) = l
    ItemLabel (l ::< _) = l
    ItemLabel (l ::+ _) = l
    ItemLabel (l ::- _) = l

-- | Internal helper used to get the name of an item given the item.
itemLabel :: forall k i. KnownSymbol (ItemLabel i) => Item k i -> String
itemLabel _ = symbolVal (Proxy :: Proxy (ItemLabel i))


-- | Require that all labels in the configuration are known.
type family LabelsKnown is :: Constraint where
    LabelsKnown '[]       = ()
    LabelsKnown ((l ::: _)  ': is) = (KnownSymbol l, LabelsKnown is)
    LabelsKnown ((l ::< us) ': is) = (KnownSymbol l, LabelsKnown us, LabelsKnown is)
    LabelsKnown ((l ::+ us) ': is) = (KnownSymbol l, LabelsKnown us, LabelsKnown is,
                                      KnownSymbol (SumSelectionLabel l))
    LabelsKnown ((l ::- us) ': is) = (KnownSymbol l, LabelsKnown us, LabelsKnown is)

-- | Require that all types of options satisfy a constraint.
type family ValuesConstrained c is :: Constraint where
    ValuesConstrained _ '[]       = ()
    ValuesConstrained c ((_ ::: v)  ': is) = (c v, ValuesConstrained c is)
    ValuesConstrained c ((_ ::< us) ': is) =
        ( ValuesConstrained c us
        , ValuesConstrained c is
        )
    ValuesConstrained c ((_ ::+ us) ': is) =
        ( ValuesConstrained c us
        , ValuesConstrained c is
        )
    ValuesConstrained c ((_ ::- us) ': is) =
        ( ValuesConstrained c us
        , ValuesConstrained c is
        )

-- | Technical constraint that is needed for built-int instances for vinyl records
-- that are defined recursively using instances for indivifual fields.
-- Almost always it is satisfied automatically but needs to be listed nevertheless.
type family SubRecsConstrained c k is :: Constraint where
    SubRecsConstrained _ _ '[]       = ()
    SubRecsConstrained c k ((_ ::: _)  ': is) = SubRecsConstrained c k is
    SubRecsConstrained c k ((_ ::< us) ': is) =
        ( c (ConfigRec k us)
        , SubRecsConstrained c k us
        , SubRecsConstrained c k is
        )
    SubRecsConstrained c k ((l ::+ us) ': is) =
        ( c (ConfigRec k (SumSelection l : us))
        , SubRecsConstrained c k us
        , SubRecsConstrained c k is
        )
    SubRecsConstrained c k ((_ ::- us) ': is) =
        ( c (ConfigRec k us)
        , SubRecsConstrained c k us
        , SubRecsConstrained c k is
        )


-----------------------
-- Finalisation
-----------------------

-- | Make sure that all options in the configuration have values
-- and if not, return the list of missing options.
finalise :: forall is. LabelsKnown is
         => ConfigRec 'Partial is
         -> Either [String] (ConfigRec 'Final is)
finalise = toEither . finalise' "" Nothing

-- | This function essentially traverses the configuration, but it is not natural
-- as a tranformation and it also keeps track of the prefix and branch finding.
finalise' :: forall gs. LabelsKnown gs
          => String                 -- ^ Option name prefix
          -> Maybe String           -- ^ Name of a branch we are looking for (if any)
          -> ConfigRec 'Partial gs
          -> Validation [String] (ConfigRec 'Final gs)
finalise' _ Nothing RNil = pure RNil
finalise' prf (Just sel) RNil = Failure [prf <> sel]
finalise' prf brc (ItemOptionP (Just x) :& xs)
    = (:&)
    <$> Success (ItemOptionF x)
    <*> finalise' prf brc xs
finalise' prf brc (item@(ItemOptionP Nothing) :& xs)
    = (:&)
    <$> Failure [prf <> itemLabel item]
    <*> finalise' prf brc xs
finalise' prf brc (item@(ItemSub rec) :& xs)
    = (:&)
    <$> (ItemSub <$> finalise' (prf <> itemLabel item <> ".") Nothing rec)
    <*> finalise' prf brc xs
finalise' prf brc (item@(ItemSum rec) :& xs)
    = (:&)
    <$> (ItemSum <$> finaliseSum (prf <> itemLabel item <> ".") rec)
    <*> finalise' prf brc xs
finalise' prf (Just sel) (item@(ItemBranchP rec) :& xs) =
    if sel == itemLabel item
    then (:&)
        <$> (ItemBranchF . Just <$> finalise' (prf <> itemLabel item <> ".") Nothing rec)
        <*> finalise' prf Nothing xs
    else (:&)
        <$> Success (ItemBranchF Nothing)
        <*> finalise' prf (Just sel) xs
finalise' prf Nothing (ItemBranchP _ :& xs)
    = (:&)
    <$> Success (ItemBranchF Nothing)
    <*> finalise' prf Nothing xs

-- | This function traverses a sum-type configuration, it essentially uses
-- the selection option to call 'finaliseBranches' and keeps track of the prefix.
finaliseSum
    :: forall gs l ms. (LabelsKnown gs, gs ~ (SumSelection l : ms))
    => String                 -- ^ Option name prefix
    -> ConfigRec 'Partial gs
    -> Validation [String] (ConfigRec 'Final gs)
finaliseSum prf (item :& xs) = case item of
    ItemOptionP (Just x) -> (:&)
        <$> Success (ItemOptionF x)
        <*> finalise' prf (Just x) xs
    ItemOptionP Nothing -> (:&)
        <$> Failure [prf <> itemLabel item]
        <*> finalise' prf Nothing xs

-- | Similar to 'finalise', but does not instantly fail if some options are
-- missing, attempt to force them will fail instead.
finaliseDeferredUnsafe :: forall is. LabelsKnown is
                      => ConfigRec 'Partial is -> ConfigRec 'Final is
finaliseDeferredUnsafe = finaliseDeferredUnsafe' Nothing

-- | Internal function that does the work of 'finaliseDeferredUnsafe'
finaliseDeferredUnsafe'
    :: forall gs. LabelsKnown gs
    => Maybe String          -- ^ Name of a branch we are looking for (if any)
    -> ConfigRec 'Partial gs
    -> ConfigRec 'Final gs
finaliseDeferredUnsafe' Nothing RNil = RNil
finaliseDeferredUnsafe' (Just sel) RNil =
    error $ toText $ "Branch was not found: " <> sel
finaliseDeferredUnsafe' brc (item@(ItemOptionP opt) :& ps) =
    let failureMsg = toText $ "Undefined config item: " <> itemLabel item
    in ItemOptionF (opt ?: error failureMsg) :& finaliseDeferredUnsafe' brc ps
finaliseDeferredUnsafe' brc (ItemSub part :& ps) =
    ItemSub (finaliseDeferredUnsafe' Nothing part) :& finaliseDeferredUnsafe' brc ps
finaliseDeferredUnsafe' brc (ItemSum part :& ps) =
    ItemSum (finaliseDeferredUnsafeSum part) :& finaliseDeferredUnsafe' brc ps
finaliseDeferredUnsafe' Nothing (ItemBranchP _ :& ps) =
    ItemBranchF Nothing :& finaliseDeferredUnsafe' Nothing ps
finaliseDeferredUnsafe' (Just sel) (item@(ItemBranchP rec) :& ps) =
    if sel == itemLabel item
    then (ItemBranchF . Just $ finaliseDeferredUnsafe' Nothing rec) :&
         finaliseDeferredUnsafe' Nothing ps
    else ItemBranchF Nothing :& finaliseDeferredUnsafe' (Just sel) ps

-- | This is to 'finaliseDeferredUnsafe' what 'finaliseSum' is to 'finalise'
finaliseDeferredUnsafeSum
    :: forall gs l ms. (LabelsKnown gs, gs ~ (SumSelection l : ms))
    => ConfigRec 'Partial gs
    -> ConfigRec 'Final gs
finaliseDeferredUnsafeSum (item :& xs) = case item of
    ItemOptionP (Just x) -> ItemOptionF x :& finaliseDeferredUnsafe' (Just x) xs
    ItemOptionP Nothing ->
        error . toText $ "Undefined branch selection item: " <> itemLabel item

-- | Fill values absent in one config with values from another config.
-- Useful when total config of default values exists.
-- NOTE: A Sum-Type selected branch cannot be swapped for another one using this
complement
    :: ConfigRec 'Partial is
    -> ConfigRec 'Final is
    -> ConfigRec 'Final is
complement RNil RNil
    = RNil
complement (ItemOptionP opt :& ps) (ItemOptionF sup :& fs)
    = ItemOptionF (fromMaybe sup opt) :& complement ps fs
complement (ItemSub part :& ps) (ItemSub final :& fs)
    = ItemSub (complement part final) :& complement ps fs
complement (ItemSum (_ :& part) :& ps) (ItemSum (f :& final) :& fs)
    = ItemSum (f :& complement part final) :& complement ps fs
complement (ItemBranchP part :& ps) (ItemBranchF final :& fs)
    = ItemBranchF (complement part <$> final) :& complement ps fs

-- | Cast partial config to another partial config which is
-- a superset of the former.
upcast
    :: (Monoid (ConfigRec 'Partial xs), ys <: xs)
    => ConfigRec 'Partial ys
    -> ConfigRec 'Partial xs
upcast ys = rreplace ys mempty

-----------------------
-- Configuration lenses
-----------------------

-- | Get the type of the item by its label.
type family ItemType l is where
      ItemType l '[] = TypeError
          ( 'Text "Cannot find label " ':<>: 'ShowType l
            ':<>: 'Text " in config items"
          )
      ItemType l ((l ::: v)  ': _) = l ::: v
      ItemType l ((l ::< us) ': _) = l ::< us
      ItemType l ((l ::+ us) ': _) = l ::+ us
      ItemType l ((l ::- us) ': _) = l ::- us
      ItemType l (_  ': is) = ItemType l is


-- | Check whether a configuration of kind @k@ contains an item of type @l ::: v@.
type HasOption l is v =
    ( RecElem Rec (l ::: v) is (RIndex (l ::: v) is)
    , ItemType l is ~ (l ::: v)
    )

-- | Lens that focuses on the configuration option with the given label.
option :: forall k l v g is a. (Functor g, a ~ Item' k (l ::: v), HasOption l is v)
    => Label l
    -> (a -> g a)
    -> ConfigRec k is
    -> g (ConfigRec k is)
option _ = rlens (Proxy :: Proxy (l ::: v)) . cfgItem


-- | Check whether the configuration has the subsection.
type HasSub l is us =
    ( RecElem Rec (l ::< us) is (RIndex (l ::< us) is)
    , ItemType l is ~ (l ::< us)
    )

-- | Lens that focuses on the subsection option with the given label.
sub :: forall k l us g is a. (Functor g, a ~ Item' k (l ::< us), HasSub l is us)
    => Label l
    -> (a -> g a)
    -> ConfigRec k is
    -> g (ConfigRec k is)
sub _ = rlens (Proxy :: Proxy (l ::< us)) . cfgItem

-- | Check whether the configuration has the sum-type.
type HasSum l is us =
    ( RecElem Rec (l ::+ us) is (RIndex (l ::+ us) is)
    , ItemType l is ~ (l ::+ us)
    )

-- | Lens that focuses on the sum-type option with the given label.
tree :: forall k l us g is a. (Functor g, a ~ Item' k (l ::+ us), HasSum l is us)
    => Label l
    -> (a -> g a)
    -> ConfigRec k is
    -> g (ConfigRec k is)
tree _ = rlens (Proxy :: Proxy (l ::+ us)) . cfgItem

-- | Check whether the configuration has the branch.
type HasBranch l is us =
    ( RecElem Rec (l ::- us) is (RIndex (l ::- us) is)
    , ItemType l is ~ (l ::- us)
    )

-- | Lens that focuses on the branch option with the given label.
branch :: forall k l us g is a. (Functor g, a ~ Item' k (l ::- us), HasBranch l is us)
    => Label l
    -> (a -> g a)
    -> ConfigRec k is
    -> g (ConfigRec k is)
branch _ = rlens (Proxy :: Proxy (l ::- us)) . cfgItem

-- | Lens that focuses on the selection option of a tree/sum-type
selection
    :: forall k l v g is a lp ms.
        ( Functor g
        , a ~ Item' k (l ::: v)
        , HasOption l is v
        , is ~ (SumSelection lp : ms)
        , l ~ SumSelectionLabel lp
        )
    => (a -> g a)
    -> ConfigRec k is
    -> g (ConfigRec k is)
selection = rlens (Proxy :: Proxy (l ::: v)) . cfgItem

-----------------------
-- Basic instances
-----------------------

deriving instance
    ( ValuesConstrained Eq '[i]
    , SubRecsConstrained Eq k '[i]
    ) => Eq (Item k i)


instance
    ( LabelsKnown '[i]
    , ValuesConstrained Show '[i]
    , SubRecsConstrained Show k '[i]
    )
    => Show (Item k i)
  where
    show item@(ItemOptionP (Just x))   = itemLabel item ++ " =: " ++ show x
    show item@(ItemOptionP Nothing)    = itemLabel item ++ " <unset>"
    show item@(ItemOptionF x)          = itemLabel item ++ " =: " ++ show x
    show item@(ItemSub rec)            = itemLabel item ++ " =< " ++ show rec
    show item@(ItemSum rec)            = itemLabel item ++ " =+ " ++ show rec
    show item@(ItemBranchP rec)        = itemLabel item ++ " =- " ++ show rec
    show item@(ItemBranchF (Just rec)) = itemLabel item ++ " =- " ++ show rec
    show item@(ItemBranchF Nothing)    = itemLabel item ++ " <unselected>"

instance
    ( SubRecsConstrained Semigroup 'Partial '[i]
    ) => Semigroup (Item 'Partial i) where
    ItemOptionP x1 <> ItemOptionP x2 = ItemOptionP . getLast $ Last x1 <> Last x2
    ItemSub r1 <> ItemSub r2 = ItemSub $ r1 <> r2
    ItemSum r1 <> ItemSum r2 = ItemSum $ r1 <> r2
    ItemBranchP r1 <> ItemBranchP r2 = ItemBranchP $ r1 <> r2


instance Monoid (Item 'Partial (l ::: t)) where
    mempty = ItemOptionP Nothing
    mappend = (<>)

instance
    ( SubRecsConstrained Semigroup 'Partial '[l ::< is]
    , SubRecsConstrained Monoid 'Partial '[l ::< is]
    ) => Monoid (Item 'Partial (l ::< is))
  where
    mempty = ItemSub mempty
    mappend = (<>)

instance
    ( SubRecsConstrained Semigroup 'Partial '[l ::+ is]
    , SubRecsConstrained Monoid 'Partial '[l ::+ is]
    ) => Monoid (Item 'Partial (l ::+ is))
  where
    mempty = ItemSum mempty
    mappend = (<>)

instance
    ( SubRecsConstrained Semigroup 'Partial '[l ::- is]
    , SubRecsConstrained Monoid 'Partial '[l ::- is]
    ) => Monoid (Item 'Partial (l ::- is))
  where
    mempty = ItemBranchP mempty
    mappend = (<>)


instance Default (ConfigRec k '[]) where
    def = RNil

-- | Values are missing by default.
instance
    ( Default (ConfigRec 'Partial is)
    ) => Default (ConfigRec 'Partial ((i ::: t) : is)) where
    def = ItemOptionP Nothing :& def

instance
    ( Default (ConfigRec k t)
    , Default (ConfigRec k is)
    ) => Default (ConfigRec k ((i ::< t) : is)) where
    def = ItemSub def :& def

instance
    ( Default (ConfigRec 'Partial is)
    , Default (ConfigRec 'Partial t)
    ) => Default (ConfigRec 'Partial ((i ::+ t) : is)) where
    def = ItemSum def :& def

instance
    ( Default (ConfigRec 'Partial t)
    , Default (ConfigRec 'Partial is)
    ) => Default (ConfigRec 'Partial ((i ::- t) : is)) where
    def = ItemBranchP def :& def
