{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Flexible and convenient configuration framework.
module Loot.Config
       ( module Loot.Config.Record

       , Config
       , PartialConfig

       , module Lens.Micro
       ) where

import Lens.Micro ((?~))

import Loot.Config.Record ((:::), (::<), ConfigKind (Final, Partial), ConfigRec, finalise, option,
                           sub)
import Loot.Config.Yaml ()


type Config = ConfigRec 'Final

type PartialConfig = ConfigRec 'Partial
