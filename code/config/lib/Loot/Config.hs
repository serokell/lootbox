{- This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.
 -}

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Flexible and convenient configuration framework.
module Loot.Config
       ( module Loot.Config.Record
       , module Loot.Config.Lens

       , Config
       , PartialConfig

       , module Lens.Micro
       ) where

import Lens.Micro ((?~))

import Loot.Config.Lens
import Loot.Config.Record ((:::), (::<), ConfigKind (Final, Partial), ConfigRec, finalise, option,
                           sub)
import Loot.Config.Yaml ()


type Config = ConfigRec 'Final

type PartialConfig = ConfigRec 'Partial
