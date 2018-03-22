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

       , (:::)
       , (::<)

       , ConfigRec

       , Item'
       , Item (..)

       , ItemType

       , finalise

       , HasOption
       , option

       , HasSub
       , sub
       ) where

import Universum

import Data.Validation (AccValidation (AccFailure, AccSuccess), toEither)
import Data.Vinyl (Label, Rec ((:&), RNil))
import Data.Vinyl.Lens (RecElem, rlens)
import Data.Vinyl.TypeLevel (AllConstrained, RIndex)
import GHC.TypeLits (ErrorMessage ((:<>:), ShowType, Text), KnownSymbol, TypeError, symbolVal)
import Lens.Micro.TH (makeLenses)

import qualified Text.Show (Show (show))


data ConfigKind
    = Partial
    -- ^ In a partial configuration some of the fields migh be missing, in case
    -- they will be initialised later.
    | Final
    -- ^ A final configuratoin must have all its fields initialised.


-- | Type for ordinary configuration options.
--
-- Example: @"timeout" ::: Int@
data (:::) l t

-- | Type for configurations subsections.
--
-- Example: @"constants" ::< '[ ... ]@
data (::<) l t


-- | Type of configuration records of 'ConfigKind' @k@.
type ConfigRec k = Rec (Item k)


-- | Type family that interprets configuration items for vinyl records.
type family Item' (k :: ConfigKind) f where
    Item' 'Partial (l ::: t)  = Maybe (Item' 'Final (l ::: t))
    Item' 'Final   (l ::: t)  = t
    Item' k        (l ::< fs) = ConfigRec k fs

-- | Wrapper for the type family (because type families are not first-class).
newtype Item k f = Item { _cfgItem :: Item' k f }
makeLenses ''Item


-----------------------
-- Finalisation
-----------------------

-- TODO: What is happening here is completely crazy.
-- As far as I understand, singletons should be used instead of this ad-hoc
-- class and 'Proxy' magic, but I am too dumb to sort this out.

class Finalisable f where
    finaliseItem :: Proxy f  -- ^ It is needed to resolve ambiguity
                 -> String   -- ^ Item label prefix
                 -> Item' 'Partial f
                 -> AccValidation [String] (Item' 'Final f)

instance KnownSymbol l => Finalisable (l ::: t) where
    finaliseItem _ _   (Just x) = AccSuccess x
    finaliseItem _ prf Nothing  = AccFailure [prf <> symbolVal (Proxy :: Proxy l)]

instance (KnownSymbol l, AllConstrained Finalisable fs) => Finalisable (l ::< fs) where
    finaliseItem _ prf = finalise' prf'
      where
        prf' = prf <> symbolVal (Proxy :: Proxy l) <> "."

-- | Make sure that all options in the configuration have values
-- and if not, return the list of missing options.
finalise :: forall fs. AllConstrained Finalisable fs
         => ConfigRec 'Partial fs
         -> Either [String] (ConfigRec 'Final fs)
finalise = toEither . finalise' ""

-- | This function is essentially 'rtraverse' except that 'rtraverse' can’t
-- be used here because it needs a natural transformation and our transformation
-- is not natural due to the type family being involved.
finalise' :: forall fs. AllConstrained Finalisable fs
          => String                 -- ^ Label prefix
          -> ConfigRec 'Partial fs  -- ^ Partial config
          -> AccValidation [String] (ConfigRec 'Final fs)
finalise' _ RNil = pure RNil
finalise' prf rec@(_ :& _) = finaliseCons rec
  where
    -- | The magic of this function is that it captures @g@ to create the proxy
    -- and disambiguate the expected type of the option.
    finaliseCons :: forall g gs. fs ~ (g ': gs)
                 => ConfigRec 'Partial (g ': gs)
                 -> AccValidation [String] (ConfigRec 'Final (g ': gs))
    finaliseCons (Item x :& fs) = (:&)
                              <$> (Item <$> finaliseItem (Proxy :: Proxy g) prf x)
                              <*> finalise' prf fs


-----------------------
-- Configuration lenses
-----------------------

-- | Get the type of the item by its label.
type family ItemType l fs where
      ItemType l '[] = TypeError
          ( 'Text "Cannot find label " ':<>: 'ShowType l
            ':<>: 'Text " in config items"
          )
      ItemType l ((l  ::: v)  ': _) = l ::: v
      ItemType l ((l  ::< us) ': _) = l ::< us
      ItemType l (_  ': fs) = ItemType l fs


-- | Check whether a configuration of kind @k@ contains an item of type @l ::: v@.
type HasOption k l fs v =
    ( RecElem Rec (l ::: v) fs (RIndex (l ::: v) fs)
    , ItemType l fs ~ (l ::: v)
    )

-- | Lens that focuses on the configuration option with the given label.
option :: forall k l v g fs a. (Functor g, a ~ Item' k (l ::: v), HasOption k l fs v)
    => Label l
    -> (a -> g a)
    -> ConfigRec k fs
    -> g (ConfigRec k fs)
option _ = rlens (Proxy :: Proxy (l ::: v)) . cfgItem


-- | Check whether the configuration has the subsection.
type HasSub k l fs us =
    ( RecElem Rec (l ::< us) fs (RIndex (l ::< us) fs)
    , Item' k (ItemType l fs) ~ ConfigRec k us
    )

-- | Lens that focuses on the subsection option with the given label.
sub :: forall k l us g fs a. (Functor g, a ~ Item' k (l ::< us), HasSub k l fs us)
    => Label l
    -> (a -> g a)
    -> ConfigRec k fs
    -> g (ConfigRec k fs)
sub _ = rlens (Proxy :: Proxy (l ::< us)) . cfgItem


-----------------------
-- Basic instances
-----------------------

deriving instance Eq (Item' k f) => Eq (Item k f)


instance (KnownSymbol l, Show t) => Show (Item 'Partial (l ::: t)) where
    show (Item (Just x)) = symbolVal (Proxy :: Proxy l) ++ " =: " ++ show x
    show (Item Nothing)  = symbolVal (Proxy :: Proxy l) ++ " <unset>"

instance (KnownSymbol l, Show t) => Show (Item 'Final (l ::: t)) where
    show (Item x) = symbolVal (Proxy :: Proxy l) ++ " =: " ++ show x

instance (KnownSymbol l, Show (ConfigRec k fs)) => Show (Item k (l ::< fs)) where
    show (Item rec) = symbolVal (Proxy :: Proxy l) ++ " =< " ++ show rec


instance Semigroup (Item 'Partial (l ::: (t :: *))) where
    Item f1 <> Item f2 = Item . getLast $ Last f1 <> Last f2

instance Semigroup (ConfigRec 'Partial fs) => Semigroup (Item 'Partial (l ::< fs)) where
    Item r1 <> Item r2 = Item $ r1 <> r2


instance Monoid (Item 'Partial (l ::: (t :: *))) where
    mempty = Item Nothing
    mappend = (<>)

instance
    ( Semigroup (ConfigRec 'Partial fs)
    , Monoid (ConfigRec 'Partial fs)
    )
    => Monoid (Item 'Partial (l ::< fs))
  where
    mempty = Item mempty
    mappend = (<>)
