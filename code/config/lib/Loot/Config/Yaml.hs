{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators  #-}

-- | Utilities for reading configuration from a file.
module Loot.Config.Yaml
       ( configParser
       ) where

import Universum

import Data.Aeson (FromJSON (parseJSON))
import Data.Aeson.BetterErrors (Parse, fromAesonParser, keyMay, keyOrDefault, toAesonParser')
import Data.Vinyl (Rec ((:&), RNil))
import GHC.TypeLits (KnownSymbol, symbolVal)

import Loot.Config.Record ((:::), (::<), ConfigKind (Partial), ConfigRec, Item (Item))


-- | This class is almost like 'FromJSON' but uses @aeson-better-errors@.
class OptionsFromJson (fs :: [*]) where
    -- | Parser for configuration.
    configParser :: Parse e (ConfigRec 'Partial fs)

instance OptionsFromJson '[] where
    configParser = pure RNil

instance
    forall l v fs.
        ( KnownSymbol l
        , FromJSON v
        , OptionsFromJson fs)
    => OptionsFromJson ((l ::: v) ': fs)
  where
    configParser = (:&) <$> fmap Item parseOption <*> configParser
      where
        parseOption :: Parse e (Maybe v)
        parseOption = keyMay (fromString $ symbolVal (Proxy :: Proxy l)) fromAesonParser

instance
    forall l us fs.
        ( KnownSymbol l
        , Monoid (ConfigRec 'Partial us)
        , OptionsFromJson us
        , OptionsFromJson fs
        )
    => OptionsFromJson ((l ::< us) ': fs)
  where
    configParser = (:&) <$> fmap Item parseSub <*> configParser
      where
        parseSub :: Parse e (ConfigRec 'Partial us)
        parseSub = keyOrDefault (fromString $ symbolVal (Proxy :: Proxy l)) mempty configParser


instance OptionsFromJson fs => FromJSON (ConfigRec 'Partial fs) where
    parseJSON = toAesonParser' configParser
