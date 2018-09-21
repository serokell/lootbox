{- SPDX-License-Identifier: MPL-2.0 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE TypeFamilies #-}

-- | Utilities for reading configuration from command-line parameters.
module Loot.Config.CLI
       ( OptParser
       , (.::)
       , (.:<)
       , (.<>)
       ) where

import Data.Vinyl (Label, Rec ((:&), RNil), type (<:))
import Options.Applicative (Parser, auto, long, strOption, switch, value)
import qualified Options.Applicative as Opt
import Data.Default (Default (def))

import Loot.Config.Record ((:::), (::<), ConfigKind (Partial), ConfigRec, HasOption, HasSub,
                           Item (ItemOptionP, ItemSub), ItemKind, option, sub)


-- | Type alias for options parser
type OptParser cfg = Parser (ConfigRec 'Partial cfg)

-- | Combinator which declares a config parser which parses one config
-- option, leaving other options empty.
(.::)
    :: forall l is v. (HasOption l is v, Default (ConfigRec 'Partial is))
    => Label l
    -> Parser v
    -> OptParser is
l .:: p = (\v -> def & option l .~ Just v) <$> p
infixr 6 .::

-- | Combinator which declares a config parser which parses one
-- subsection, leaving other options empty.
(.:<)
    :: forall l is us. (HasSub l is us, Default (ConfigRec 'Partial is))
    => Label l
    -> OptParser us
    -> OptParser is
l .:< p = (\us -> def & sub l .~ us) <$> p
infixr 6 .:<

{-|
Combines two partial config parsers.
It is intended to use with @OverloadedLabels@ and combinators
'.::' and '.:<' to build partial CLI parsers for config records.

Example: here is the config type:

@
type Config =
  '[ "mda" ::: Int
   , "heh" ::: Bool
   , "taks" ::<
       '[ "chto" ::: String
        , "tut" ::: Integer
        , "u_nas" ::: Bool
        ]
   ]
@

And here is a valid parser for this config:

@
cfgParser :: OptParser Config
cfgParser =
    #heh  .:: switch (long "heh") .<>
    #taks .:<
       (#chto  .:: strOption (long "chto") .<>
        #u_nas .:: switch (long "u_nas"))
@

Note that options "mda" and "taks.tut" are omitted
from parser's definition. This means that these options
will not be included in the partial config produced by the parser.
-}
(.<>)
    :: Semigroup (ConfigRec 'Partial is)
    => OptParser is -> OptParser is -> OptParser is
p .<> q = (<>) <$> p <*> q
infixr 5 .<>
