{- SPDX-License-Identifier: MPL-2.0 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module Loot.Config.Buildable
  (
  ) where

import Data.Vinyl (Rec (RNil, (:&)))
import Fmt (Buildable (..), Builder, blockMapF)
import GHC.TypeLits (KnownSymbol)

import Loot.Config.Record (ConfigKind, ConfigRec, Item (..), ItemKind,
                           SumSelection, getConfigKey, (::+), (::-), (:::),
                           (::<))

class OptionsToBuilderList (k :: ConfigKind) (is :: [ItemKind]) where
  configToBuilderList :: ConfigRec k is -> [(Text, Builder)]

instance OptionsToBuilderList (k :: ConfigKind) '[] where
  configToBuilderList RNil = []

undefinedBuilder :: Builder
undefinedBuilder = build @Text "<undefined>"

instance
  ( KnownSymbol l
  , Buildable v
  , OptionsToBuilderList k is
  ) => OptionsToBuilderList k ((l ::: v) ': is)
  where
    configToBuilderList (itemOption :& rest) =
      let label = getConfigKey @l
          value = case itemOption of
            ItemOptionP (Just v) -> build v
            ItemOptionP Nothing -> undefinedBuilder
            ItemOptionF v -> build v
      in (label, value) : configToBuilderList @k @is rest

instance
  ( KnownSymbol l
  , OptionsToBuilderList k us
  , OptionsToBuilderList k is
  ) => OptionsToBuilderList k ((l ::< us) ': is)
  where
    configToBuilderList (ItemSub inner :& rest) =
      let label = getConfigKey @l
          value = blockMapF $ configToBuilderList inner
      in (label, value) : configToBuilderList rest

instance
  ( KnownSymbol l
  , OptionsToBuilderList k  (SumSelection l : us)
  , OptionsToBuilderList k is
  ) => OptionsToBuilderList k ((l ::+ us) ': is)
  where
    configToBuilderList (itemSum :& rest) =
      let label = getConfigKey @l
          value = case itemSum of
            ItemSumP inner -> blockMapF $ configToBuilderList inner
            ItemSumF inner -> blockMapF $ configToBuilderList inner
      in (label, value) : configToBuilderList rest

instance
  ( KnownSymbol l
  , OptionsToBuilderList k us
  , OptionsToBuilderList k is
  ) => OptionsToBuilderList k ((l ::- us) ': is)
  where
    configToBuilderList (itemBranch :& rest) =
      let label = getConfigKey @l
          value = case itemBranch of
            ItemBranchP inner -> blockMapF $ configToBuilderList inner
            ItemBranchF (Just inner) -> blockMapF $ configToBuilderList inner
            ItemBranchF Nothing -> undefinedBuilder
      in (label, value) : configToBuilderList rest

instance OptionsToBuilderList k is => Buildable (ConfigRec k is) where
  build config = blockMapF $ configToBuilderList config
