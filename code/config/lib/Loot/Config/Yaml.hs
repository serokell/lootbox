{- SPDX-License-Identifier: MPL-2.0 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Utilities for parsing and serializing configuration with Yaml/JSON.
module Loot.Config.Yaml
       ( OptionsFromJson (..)
       , OptionsToJson (..)
       ) where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object))
import Data.Aeson.BetterErrors (Parse, fromAesonParser, keyMay, keyOrDefault, toAesonParser')
import Data.Aeson.Types (Object)
import Data.Vinyl (Rec (RNil, (:&)))
import GHC.TypeLits (KnownSymbol, symbolVal)

import qualified Data.HashMap.Strict as HM

import Loot.Config.Record ((:::), (::<), (::+), (::-), ConfigKind (..),
                           ConfigRec, Item (..), ItemKind, SumSelection)

-- | Helper function to get config key from type level
getConfigKey :: forall l . KnownSymbol l => Text
getConfigKey = fromString $ symbolVal (Proxy :: Proxy l)

-- | This class is almost like 'FromJSON' but uses @aeson-better-errors@.
class OptionsFromJson (is :: [ItemKind]) where
    -- | Parser for configuration.
    configParser :: Parse e (ConfigRec 'Partial is)

instance OptionsFromJson '[] where
    configParser = pure RNil

instance
    forall l v is.
        ( KnownSymbol l
        , FromJSON v
        , OptionsFromJson is)
    => OptionsFromJson ((l ::: v) ': is)
  where
    configParser = (:&) <$> fmap ItemOptionP parseOption <*> configParser
      where
        parseOption :: Parse e (Maybe v)
        parseOption = keyMay (getConfigKey @l) fromAesonParser

instance
    forall l us is.
        ( KnownSymbol l
        , Monoid (ConfigRec 'Partial us)
        , OptionsFromJson us
        , OptionsFromJson is
        )
    => OptionsFromJson ((l ::< us) ': is)
  where
    configParser = (:&)
        <$> fmap ItemSub (parseMulti @l)
        <*> configParser

instance
    forall l us is.
        ( KnownSymbol l
        , Monoid (ConfigRec 'Partial (SumSelection l : us))
        , OptionsFromJson (SumSelection l : us)
        , OptionsFromJson is
        )
    => OptionsFromJson ((l ::+ us) ': is)
  where
    configParser = (:&)
        <$> fmap ItemSumP (parseMulti @l)
        <*> configParser

instance
    forall l us is.
        ( KnownSymbol l
        , Monoid (ConfigRec 'Partial us)
        , OptionsFromJson us
        , OptionsFromJson is
        )
    => OptionsFromJson ((l ::- us) ': is)
  where
    configParser = (:&)
        <$> fmap ItemBranchP (parseMulti @l)
        <*> configParser

-- | Internal function to parse multiple 'Item's
parseMulti
    :: forall l us e.
        ( KnownSymbol l
        , Monoid (ConfigRec 'Partial us)
        , OptionsFromJson us
        )
    => Parse e (ConfigRec 'Partial us)
parseMulti = keyOrDefault (getConfigKey @l) mempty configParser

instance OptionsFromJson is => FromJSON (ConfigRec 'Partial is) where
    parseJSON = toAesonParser' configParser

-- Helper function to insert values in a JSON 'Object' by config key.
insert' :: forall l v . (KnownSymbol l, ToJSON v) => v -> Object -> Object
insert' value = HM.insert (getConfigKey @l) (toJSON value)

-- | This class is helper which converts config to object
class OptionsToJson (k :: ConfigKind) (is :: [ItemKind]) where
  -- | Convert 'ConfigRec' to 'Object'
  configToObject :: ConfigRec k is -> Object

instance OptionsToJson (k :: ConfigKind) '[] where
  configToObject RNil = HM.empty

instance
  forall l v is k
  . ( KnownSymbol l
    , ToJSON v
    , OptionsToJson k is
    )
  => OptionsToJson k ((l ::: v) ': is) where
  configToObject (ItemOptionP Nothing :& vs) = configToObject vs
  configToObject (ItemOptionP (Just v) :& vs) = insert' @l v $ configToObject vs
  configToObject (ItemOptionF v :& vs) = insert' @l v $ configToObject vs

instance
  forall l us is k
  . ( KnownSymbol l
    , Monoid (ConfigRec 'Partial us)
    , OptionsToJson k us
    , OptionsToJson k is
    )
  => OptionsToJson k ((l ::< us) ': is) where
  configToObject (ItemSub v :& vs) = insert' @l v $ configToObject vs

instance
  forall l us is k
  . ( KnownSymbol l
    , Monoid (ConfigRec 'Partial (SumSelection l : us))
    , OptionsToJson k (SumSelection l : us)
    , OptionsToJson k is
    )
  => OptionsToJson k ((l ::+ us) ': is) where
  configToObject (ItemSumP v :& vs) = insert' @l v $ configToObject vs
  configToObject (ItemSumF v :& vs) = insert' @l v $ configToObject vs

instance
  forall l us is k
  . ( KnownSymbol l
    , Monoid (ConfigRec 'Partial us)
    , OptionsToJson k us
    , OptionsToJson k is
    )
  => OptionsToJson k ((l ::- us) ': is) where
  configToObject (ItemBranchP v :& vs) = insert' @l v $ configToObject vs
  configToObject (ItemBranchF Nothing :& vs) = configToObject vs
  configToObject (ItemBranchF (Just v) :& vs) = insert' @l v $ configToObject vs

instance OptionsToJson k is => ToJSON (ConfigRec k is) where
  toJSON = Object . configToObject
