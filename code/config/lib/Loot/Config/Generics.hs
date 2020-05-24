{- SPDX-License-Identifier: MPL-2.0 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Utilities for converting between configuration and ADT.

Example: let's assume you have the following datatype:

@
data DBConfig = DBConfig
  { username :: Text
  , password :: Text
  , port :: Word16
  } deriving Generic
@

Then it can be included into configuration as follows:

@
type Config =
  '[ "myservice" ::<
      '[ "db" ::< 'FromData' DBConfig
       ]
   -- This unfolds into
   -- '[ "db" ::<
   --     [ "username" ::: Text
   --     , "password" ::: Text
   --     , "port" ::: Word16
   --     ]
   --  ]
   ]

dbConfig :: ConfigRec 'Final Config -> DBConfig
dbConfig cfg = cfg ^. sub #myservice . sub #db . 'asData'
@

This works only for product types, it does not handle nested datatypes nor
sum types. We may add this functionality later, see [LTB-67].

-}
module Loot.Config.Generics
       ( FromData
       , toData
       , fromData
       , asData

         -- * Internal helpers
       , KnownPrimitiveConfigKind
       ) where

import Data.Vinyl (Rec (RNil, (:&)), (<+>))
import Data.Vinyl.TypeLevel (type (++))
import GHC.Generics ((:*:) (..), (:+:))
import qualified GHC.Generics as G
import GHC.TypeLits (ErrorMessage (..), TypeError)

import Loot.Config.Record (ConfigKind (Final, Partial), ConfigRec, Item (ItemOptionF, ItemOptionP),
                           ItemKind, (:::))

-- | Internal typeclass
class KnownPrimitiveConfigKind (k :: ConfigKind) where
    -- | Lift to config item.
    toItem :: t -> Item k (l ::: t)

instance KnownPrimitiveConfigKind 'Final where
    toItem = ItemOptionF

instance KnownPrimitiveConfigKind 'Partial where
    toItem = ItemOptionP . Just

-- | Internal typeclass which splits 'Rec' into two specified pieces.
class RSplit (l :: [k]) where
    -- | The opposite to '<+>'.
    rsplit :: Rec f (l ++ r) -> (Rec f l, Rec f r)

instance RSplit '[] where
    rsplit = (RNil, )

instance RSplit ls => RSplit (l ': ls) where
    rsplit (l :& xs) = let (ls, rs) = rsplit xs in (l :& ls, rs)

-- | Internal typeclass which implements conversion between generic
-- representation of a type and the respective config.
class GDataIsoItem (x :: Type -> Type) where

    -- | Configuration associated with given datatype.
    type GFromData x :: [ItemKind]

    -- | Converts datatype to config.
    gToData :: KnownPrimitiveConfigKind k => x p -> ConfigRec k (GFromData x)

    -- | Converts datatype from the associated config.
    gFromData :: ConfigRec 'Final (GFromData x) -> x p

instance GDataIsoItem x => GDataIsoItem (G.D1 i x) where
    type GFromData (G.D1 i x) = GFromData x
    gToData = gToData . G.unM1
    gFromData = G.M1 . gFromData

instance GDataIsoItem x => GDataIsoItem (G.C1 i x) where
    type GFromData (G.C1 i x) = GFromData x
    gToData = gToData . G.unM1
    gFromData = G.M1 . gFromData

instance GDataIsoItem (G.S1 ('G.MetaSel ('Just name) _1 _2 _3) (G.Rec0 a)) where
    type GFromData (G.S1 ('G.MetaSel ('Just name) _1 _2 _3) (G.Rec0 a)) =
        '[name ::: a]
    gToData (G.M1 (G.K1 a)) = toItem a :& RNil
    gFromData (ItemOptionF a :& RNil) = G.M1 (G.K1 a)

instance TypeError ('Text "Datatype contains field without records") =>
         GDataIsoItem (G.S1 ('G.MetaSel 'Nothing _1 _2 _3) x) where
    type GFromData (G.S1 ('G.MetaSel 'Nothing _1 _2 _3) x) = '[]
    gToData = error "impossible"
    gFromData = error "impossible"

instance ( GDataIsoItem x, GDataIsoItem y
         , RSplit (GFromData x)
         ) => GDataIsoItem (x :*: y) where
    type GFromData (x :*: y) = GFromData x ++ GFromData y
    gToData (x :*: y) = gToData x <+> gToData y
    gFromData xs =
      let (ls, rs) = rsplit @_ @(GFromData x) xs
      in gFromData ls :*: gFromData rs

instance GDataIsoItem G.U1 where
    type GFromData G.U1 = '[]
    gToData G.U1 = RNil
    gFromData RNil = G.U1

instance TypeError ('Text "Sum types are not yet supported") =>
         GDataIsoItem (x :+: y) where
    type GFromData (x :+: y) = '[]
    gToData = error "impossible"
    gFromData = error "impossible"

instance TypeError ('Text "Empty variants configs are not allowed") =>
         GDataIsoItem G.V1 where
    type GFromData G.V1 = '[]
    gToData = error "impossible"
    gFromData = error "impossible"


-- | Requires type to have a config associated with it.
type DataIsoItem dt = (Generic dt, GDataIsoItem (G.Rep dt))

-- | Derive configuration format corresponding to the given datatype.
--
-- Produced config is always flat: currently only product types are supported.
-- Each field is represented in config as-is (no deep inspection is performed).
type FromData dt = GFromData (G.Rep dt)

-- | Convert configuration to the datatype.
toData :: DataIsoItem dt => ConfigRec 'Final (FromData dt) -> dt
toData = G.to . gFromData

-- | Convert datatype to configuration.
--
-- In case of conversion to partial configuration all fields will be initialised.
fromData :: (DataIsoItem dt, KnownPrimitiveConfigKind k) => dt -> ConfigRec k (FromData dt)
fromData = gToData . G.from

-- | Lens that converts between datatype and the associated configuration section.
asData
    :: (Functor f, DataIsoItem dt)
    => (dt -> f dt)
    -> ConfigRec 'Final (FromData dt)
    -> f (ConfigRec 'Final (FromData dt))
asData f = fmap fromData . f . toData
