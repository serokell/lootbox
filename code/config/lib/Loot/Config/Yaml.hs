{- SPDX-License-Identifier: MPL-2.0 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds #-}

-- | Utilities for reading configuration from a file.
module Loot.Config.Yaml
       ( configParser
       ) where

import Data.Aeson (FromJSON (parseJSON))
import Data.Aeson.BetterErrors (Parse, fromAesonParser, keyMay, keyOrDefault, toAesonParser')
import Data.Vinyl (Rec ((:&), RNil))
import GHC.TypeLits (KnownSymbol, symbolVal)

import Loot.Config.Record ((:::), (::<), (::+), (::-), ConfigKind (Partial),
                           ConfigRec, Item (ItemOptionP, ItemSub, ItemSumP, ItemBranchP), 
                           ItemKind, SumSelection, ToBranches)


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
        parseOption = keyMay (fromString $ symbolVal (Proxy :: Proxy l)) fromAesonParser

instance
    forall l us is.
        ( KnownSymbol l
        , Monoid (ConfigRec 'Partial us)
        , OptionsFromJson us
        , OptionsFromJson is
        )
    => OptionsFromJson ((l ::< us) ': is)
  where
    configParser = (:&) <$> fmap ItemSub parseSub <*> configParser
      where
        parseSub :: Parse e (ConfigRec 'Partial us)
        parseSub = keyOrDefault (fromString $ symbolVal (Proxy :: Proxy l)) mempty configParser

instance
    forall l us is.
        ( KnownSymbol l
        , Monoid (ConfigRec 'Partial (SumSelection l : ToBranches us))
        , OptionsFromJson (SumSelection l : ToBranches us)
        , OptionsFromJson is
        )
    => OptionsFromJson ((l ::+ us) ': is)
  where
    configParser = (:&) <$> fmap ItemSumP parseSum <*> configParser
      where
        parseSum :: Parse e (ConfigRec 'Partial (SumSelection l : ToBranches us))
        parseSum = keyOrDefault (fromString $ symbolVal (Proxy :: Proxy l)) mempty configParser

instance
    forall l us is.
        ( KnownSymbol l
        , Monoid (ConfigRec 'Partial us)
        , OptionsFromJson us
        , OptionsFromJson is
        )
    => OptionsFromJson ((l ::- us) ': is)
  where
    configParser = (:&) <$> fmap ItemBranchP parseBranch <*> configParser
      where
        parseBranch :: Parse e (ConfigRec 'Partial us)
        parseBranch = keyOrDefault (fromString $ symbolVal (Proxy :: Proxy l)) mempty configParser

instance OptionsFromJson is => FromJSON (ConfigRec 'Partial is) where
    parseJSON = toAesonParser' configParser
