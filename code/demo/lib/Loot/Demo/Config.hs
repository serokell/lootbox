{- This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.
 -}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Loot.Demo.Config
       ( Config
       , ConfigPart

       , defaultConfig
       ) where

import Loot.Config ((:::), (::<), ConfigKind (Final, Partial), ConfigRec, option, (?~))
import Loot.Log.Warper (LoggerConfig)


-- | Our configuration contains:
--
-- 1. @timeout@ of type 'Int'
--
-- 2. Subsection @server@ consisting of:
--
--     1. Server hostname of type 'String'
--
--     2. Server port of type 'Word16'
--
-- 3. Logger config
type Options =
    '[ "timeout" ::: Int
     , "server"  ::<
        '[ "host" ::: String
         , "port" ::: Word16
         ]
     , "logging" ::: LoggerConfig
     ]

-- | Type of partial configurations.
-- Any option can be missing until the configuration is finalised.
type ConfigPart = ConfigRec 'Partial Options

-- | Type of configurations.
-- All options are guaranteed to exist.
--
-- In order to finalise a configuration, use the 'finalise' function,
-- whose type essentially is @:: ConfigPart -> Either [String] Config@.
type Config = ConfigRec 'Final Options


defaultConfig :: ConfigPart
defaultConfig = mempty & option #timeout ?~ 10
                       & option #logging ?~ mempty
