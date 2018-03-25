{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Loot.Demo.Config
       ( Config
       , ConfigPart
       ) where

import Universum

import Loot.Config ((:::), (::<), ConfigKind (Final, Partial), ConfigRec)


-- | Our configuration contains:
--
-- 1. @timeout@ of type 'Int'
--
-- 2. Subsection @server@ consisting of:
--
--     1. Server hostname of type 'String'
--
--     2. Server port of type 'Word16'
type Options =
    '[ "timeout" ::: Int
     , "server"  ::<
        '[ "host" ::: String
         , "port" ::: Word16
         ]
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
