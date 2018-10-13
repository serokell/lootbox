 -- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

-- | Utilities for reading configuration from command-line parameters.
module Loot.Config.CLI
       ( ModParser
       , OptModParser
       , modifying
       , setP
       , (<.>)
       , (.::)
       , (%::)
       , (.:<)
       ) where

import Data.Vinyl (Label)
import Lens.Micro (ASetter')
import Options.Applicative (Parser, optional)

import Loot.Config.Record (ConfigKind (Partial), ConfigRec, HasOption, HasSub, option, sub)

-- | Type for a parser which yields a modifier function instead of a
-- value
type ModParser a = Parser (a -> a)

{-|
Combines two modifier parsers.
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
cfgParser :: OptModParser Config
cfgParser =
    #heh  .:: switch (long "heh") <.>
    #taks .:<
       (#chto  .:: strOption (long "chto") <.>
        #u_nas .:: switch (long "u_nas"))
@

Note that options "mda" and "taks.tut" are omitted
from parser's definition. This means that these options
will not be included in the partial config produced by the parser.
-}
(<.>) :: ModParser a -> ModParser a -> ModParser a
p <.> q = (.) <$> p <*> q
infixl 5 <.>

-- | Creates a trivial modifying parser which just replaces original
-- value with given one.
modifying :: Parser a -> ModParser a
modifying = fmap const

-- | Creates a parser which sets a given value using a `Setter` (usually a
-- `Lens` to another type).
setP :: ASetter' a b -> Parser b -> ModParser a
setP = fmap . set

-- | Type alias for a parser which yields config modifier
type OptModParser cfg = ModParser (ConfigRec 'Partial cfg)

-- | Combinator which declares a config parser which parses one config
-- option, leaving other options empty.
(.::)
    :: forall l is v. HasOption l is v
    => Label l
    -> Parser v
    -> OptModParser is
l .:: p = (\mv -> option l %~ (mv <|>)) <$> optional p
infixr 6 .::

-- | Combinator which declares a config parser which modifies one config
-- option, not touching other options.
(%::)
    :: forall l is v. HasOption l is v
    => Label l
    -> ModParser v
    -> OptModParser is
l %:: p = (\f -> option l %~ fmap f) <$> p

-- | Combinator which declares a config parser which parses one
-- subsection, leaving other options empty.
(.:<)
    :: forall l is us. (HasSub l is us)
    => Label l
    -> OptModParser us
    -> OptModParser is
l .:< p = (\uf cfg -> cfg & sub l %~ uf) <$> p
infixr 6 .:<
