{- SPDX-License-Identifier: MPL-2.0 -}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Flexible and convenient configuration framework.
module Loot.Config
       ( module Loot.Config.Record
       , module Loot.Config.Generics
       , module Loot.Config.Lens
       , module Loot.Config.CLI

       , Config
       , PartialConfig

       , module Lens.Micro
       ) where

import Lens.Micro ((?~))

import Loot.Config.CLI
import Loot.Config.Generics
import Loot.Config.Lens
import Loot.Config.Record ((::+), (::-), (:::), (::<), ConfigKind (Final, Partial), ConfigRec,
                           branch, complement, finalise, finaliseDeferredUnsafe, option, selection,
                           sub, tree, upcast)
import Loot.Config.Yaml ()


type Config = ConfigRec 'Final

type PartialConfig = ConfigRec 'Partial
