{- SPDX-License-Identifier: MPL-2.0 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE GADTs #-}

-- | Utilities for reading configuration from a file.
module Loot.Config.Yaml
       ( configParser
       ) where

import qualified Data.Aeson.Types as A

import Data.Aeson (FromJSON (parseJSON), ToJSON(toJSON))
import Data.Aeson.BetterErrors (Parse, fromAesonParser, keyMay, keyOrDefault, toAesonParser')
import Data.Vinyl (Rec ((:&), RNil))
import GHC.TypeLits (KnownSymbol, symbolVal)

import Loot.Config.Record ((:::), (::<), (::+), (::-), ConfigKind (Partial),
                           ConfigRec, Item (ItemOptionP, ItemOptionF, ItemSub, ItemSumP, ItemSumF, ItemBranchP, ItemBranchF),
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

class OptionsToJsonList (k :: ConfigKind) (is :: [ItemKind]) where
  configToJsonList :: ConfigRec k is -> [A.Pair]

instance OptionsToJsonList (k :: ConfigKind) '[] where
  configToJsonList RNil = []

instance
  forall l v is k.
        ( KnownSymbol l
        , ToJSON v
        , OptionsToJsonList k is)
    => OptionsToJsonList k ((l ::: v) ': is)
  where
    configToJsonList (itemOption :& rest) =
      let label = fromString $ symbolVal (Proxy :: Proxy l)
          value = case itemOption of
            ItemOptionP mv -> toJSON <$> mv
            ItemOptionF v -> Just $ toJSON v
          restJson = configToJsonList @k @is rest
      in case value of
        Just v -> (label, v) : restJson
        Nothing -> restJson

instance
    forall l us is k.
        ( KnownSymbol l
        , OptionsToJsonList k us
        , OptionsToJsonList k is
        )
    => OptionsToJsonList k ((l ::< us) ': is)
  where
    configToJsonList (ItemSub inner :& rest) =
      ( fromString $ symbolVal (Proxy :: Proxy l)
      , A.object $ configToJsonList inner
      ) : configToJsonList rest

instance
    forall l us is k.
        ( KnownSymbol l
        , OptionsToJsonList k  (SumSelection l : us)
        , OptionsToJsonList k is
        )
    => OptionsToJsonList k ((l ::+ us) ': is)
  where
    configToJsonList (itemSum :& rest) =
      let value = case itemSum of
            ItemSumP inner -> A.object $ configToJsonList inner
            ItemSumF inner -> A.object $ configToJsonList inner
      in (fromString $ symbolVal (Proxy :: Proxy l), value) : configToJsonList rest

instance
    forall l us is k.
        ( KnownSymbol l
        , OptionsToJsonList k us
        , OptionsToJsonList k is
        )
    => OptionsToJsonList k ((l ::- us) ': is)
  where
    configToJsonList (itemBranch :& rest) =
      let value = case itemBranch of
            ItemBranchP inner -> A.object $ configToJsonList inner
            ItemBranchF (Just inner) -> A.object $ configToJsonList inner
            ItemBranchF Nothing -> A.Null
      in (fromString $ symbolVal (Proxy :: Proxy l), value) : configToJsonList rest

instance OptionsToJsonList k is => ToJSON (ConfigRec k is) where
  toJSON config = A.object $ configToJsonList config


