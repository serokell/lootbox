{- SPDX-License-Identifier: MPL-2.0 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds       #-}

-- | Utilities for reading configuration from a file.
module Loot.Config.Yaml
       ( configParser
       , OptionsFromJson
       ) where

import Data.Aeson (FromJSON (parseJSON))
import Data.Aeson.BetterErrors (Parse, fromAesonParser, keyMay, keyOrDefault, toAesonParser')
import Data.Vinyl (Rec ((:&), RNil))
import GHC.TypeLits (KnownSymbol, symbolVal)

import Loot.Config.Record ((:::), (::<), (::+), (::-), ConfigKind (Partial),
                           ConfigRec, Item (ItemOptionP, ItemSub, ItemSumP, ItemBranchP),
                           ItemKind, SumSelection)


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
    configParser = (:&)
        <$> fmap ItemSub (parseMulti (Proxy :: Proxy l))
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
        <$> fmap ItemSumP (parseMulti (Proxy :: Proxy l))
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
        <$> fmap ItemBranchP (parseMulti (Proxy :: Proxy l))
        <*> configParser

-- | Internal function to parse multiple 'Item's
parseMulti
    :: forall l us e.
        ( KnownSymbol l
        , Monoid (ConfigRec 'Partial us)
        , OptionsFromJson us
        )
    => Proxy l
    -> Parse e (ConfigRec 'Partial us)
parseMulti p = keyOrDefault (fromString $ symbolVal p) mempty configParser

instance OptionsFromJson is => FromJSON (ConfigRec 'Partial is) where
    parseJSON = toAesonParser' configParser
