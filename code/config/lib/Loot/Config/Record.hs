{- This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.
 -}

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

       , ConfigRec

       , Item'
       , Item (..)

       , ItemType

       , finalise
       , complement

       , HasOption
       , option

       , HasSub
       , sub
       ) where

import Data.Validation (Validation (Failure, Success), toEither)
import Data.Default (Default (..))
import Data.Vinyl (Label, Rec ((:&), RNil))
import Data.Vinyl.Lens (RecElem, rlens)
import Data.Vinyl.TypeLevel (RIndex)
import GHC.TypeLits (ErrorMessage ((:<>:), ShowType, Text), KnownSymbol, Symbol, TypeError,
                     symbolVal)

import qualified Text.Show (Show (show))


data ConfigKind
    = Partial
    -- ^ In a partial configuration some of the fields migh be missing, in case
    -- they will be initialised later.
    | Final
    -- ^ A final configuratoin must have all its fields initialised.


-- | Closed kind for items that can be stored in a config.
data ItemKind = OptionType Symbol Type | SubsectionType Symbol [ItemKind]

-- | Type for ordinary configuration options.
--
-- Example: @"timeout" ::: Int@
type (:::) = 'OptionType

-- | Type for configurations subsections.
--
-- Example: @"constants" ::< '[ ... ]@
type (::<) = 'SubsectionType


-- | Type of configuration records of 'ConfigKind' @k@.
type ConfigRec k = Rec (Item k)


-- | Type family that interprets configuration items for vinyl records.
type family Item' (k :: ConfigKind) (i :: ItemKind) where
    Item' 'Partial (l ::: t)  = Maybe t
    Item' 'Final   (l ::: t)  = t
    Item' k        (l ::< is) = ConfigRec k is

-- | Technical first-class wrapper around the type family to make it possible
-- to pattern-match on its constructors.
data Item (k :: ConfigKind) (i :: ItemKind) where
    ItemOptionP :: Item' 'Partial (l ::: t)  -> Item 'Partial (l ::: t)
    ItemOptionF :: Item' 'Final   (l ::: t)  -> Item 'Final   (l ::: t)
    ItemSub     :: Item' k        (l ::< is) -> Item k        (l ::< is)

-- | Lens to focus onto the data actually stored inside 'Item'.
cfgItem :: Functor f => (Item' k d -> f (Item' k d)) -> Item k d -> f (Item k d)
cfgItem f (ItemOptionP x) = ItemOptionP <$> f x
cfgItem f (ItemOptionF x) = ItemOptionF <$> f x
cfgItem f (ItemSub rec)   = ItemSub <$> f rec


-- | Internal helper used to get the name of an option given the option.
itemOptionLabel :: forall k l t. KnownSymbol l => Item k (l ::: t) -> String
itemOptionLabel _ = symbolVal (Proxy :: Proxy l)

-- | Internal helper used to get the name of a subsection given the subsection.
itemSubLabel :: forall k l is. KnownSymbol l => Item k (l ::< is) -> String
itemSubLabel _ = symbolVal (Proxy :: Proxy l)


-- | Require that all labels in the configuration are known.
type family LabelsKnown is :: Constraint where
    LabelsKnown '[]       = ()
    LabelsKnown ((l ::: _)  ': is) = (KnownSymbol l, LabelsKnown is)
    LabelsKnown ((l ::< us) ': is) = (KnownSymbol l, LabelsKnown us, LabelsKnown is)

-- | Require that all types of options satisfy a constraint.
type family ValuesConstrained c is :: Constraint where
    ValuesConstrained _ '[]       = ()
    ValuesConstrained c ((_ ::: v)  ': is) = (c v, ValuesConstrained c is)
    ValuesConstrained c ((_ ::< us) ': is) =
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


-----------------------
-- Finalisation
-----------------------

-- | Make sure that all options in the configuration have values
-- and if not, return the list of missing options.
finalise :: forall is. LabelsKnown is
         => ConfigRec 'Partial is
         -> Either [String] (ConfigRec 'Final is)
finalise = toEither . finalise' ""
  where
    -- | This function essentially traverses the configuration, but it is not natural
    -- as a tranformation and it also keeps track of the prefix.
    finalise' :: forall gs. LabelsKnown gs
              => String                 -- ^ Option name prefix
              -> ConfigRec 'Partial gs
              -> Validation [String] (ConfigRec 'Final gs)
    finalise' _ RNil = pure RNil
    finalise' prf (ItemOptionP (Just x) :& xs)
        = (:&)
      <$> Success (ItemOptionF x)
      <*> finalise' prf xs
    finalise' prf (item@(ItemOptionP Nothing) :& xs)
        = (:&)
      <$> Failure [prf <> itemOptionLabel item]
      <*> finalise' prf xs
    finalise' prf (item@(ItemSub rec) :& xs)
        = (:&)
      <$> (ItemSub <$> finalise' (prf <> itemSubLabel item <> ".") rec)
      <*> finalise' prf xs

-- | Fill values absent in one config with values from another config.
-- Useful when total config of default values exists.
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

-----------------------
-- Configuration lenses
-----------------------

-- | Get the type of the item by its label.
type family ItemType l is where
      ItemType l '[] = TypeError
          ( 'Text "Cannot find label " ':<>: 'ShowType l
            ':<>: 'Text " in config items"
          )
      ItemType l ((l  ::: v)  ': _) = l ::: v
      ItemType l ((l  ::< us) ': _) = l ::< us
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
    show item@(ItemOptionP (Just x)) = itemOptionLabel item ++ " =: " ++ show x
    show item@(ItemOptionP Nothing)  = itemOptionLabel item ++ " <unset>"
    show item@(ItemOptionF x)        = itemOptionLabel item ++ " =: " ++ show x
    show item@(ItemSub rec)          = itemSubLabel item    ++ " =< " ++ show rec


instance
    ( SubRecsConstrained Semigroup 'Partial '[i]
    ) => Semigroup (Item 'Partial i) where
    ItemOptionP x1 <> ItemOptionP x2 = ItemOptionP . getLast $ Last x1 <> Last x2
    ItemSub r1 <> ItemSub r2 = ItemSub $ r1 <> r2


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
