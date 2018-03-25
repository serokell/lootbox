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

import Loot.Config.Record ((:::), (::<), ConfigKind (Partial), ConfigRec,
                           Item (ItemOptionP, ItemSub), ItemKind)


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


instance OptionsFromJson is => FromJSON (ConfigRec 'Partial is) where
    parseJSON = toAesonParser' configParser
