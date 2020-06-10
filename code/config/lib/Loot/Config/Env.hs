{- SPDX-License-Identifier: MPL-2.0 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Utilities for reading configuration from environmental variables.

Imagine you have the following configuration:

@
type Options =
    '[ "appname" ::: Text
     , "db" ::<
        '[ "username" ::: Text
         , "password" ::: Text
         ]
     ]
@

Then running 'parseEnv' will fill the it with the following
environmental variables:

* APPNAME
* DB_USERNAME
* DB_PASSWORD
-}
module Loot.Config.Env
       ( parseEnv
       , parseEnvPure
       , requiredVars

         -- * Parsing individual values
       , FromEnv (..)
       , Parser
       , noValue
       , withPresent
       , parseStringEnvValue
       , autoParseEnvValue
       , parseBoundedNumEnvValue

         -- * Customized parsing
       , ParseOptions (..)
       , defaultOptions
       , simpleKeyBuilder
       , parseEnvWith
       , parseEnvPureWith
       , requiredVarsWith

         -- * Types
       , EnvParseError (..)
       , parseErrorPretty
       ) where

import Control.Monad.Except (Except, runExcept, throwError)
import qualified Data.Aeson as Aeson
import Data.Char (toLower)
import qualified Data.Fixed as Fixed
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Vinyl (Rec ((:&), RNil))
import Fmt (Buildable (..), pretty, (+|), (|+))
import GHC.TypeLits (KnownSymbol, symbolVal)
import System.Environment (getEnvironment)

import Loot.Config.Record ((::+), (::-), (:::), (::<), ConfigKind (Partial), ConfigRec,
                           Item (ItemBranchP, ItemOptionP, ItemSub, ItemSumP), ItemKind,
                           SumSelection)

-- | A complete description of parsing error.
data EnvParseError = EnvParseError
    { errKey     :: Text
    , errValue   :: Maybe String
    , errMessage :: Text
    } deriving (Show, Eq)

instance Buildable EnvParseError where
    build EnvParseError{..} =
        -- TODO: can it be that value is secret and should not be displayed?
        -- In all scenarios which I have encountered if a value have been failed
        -- to parse, then it was too incorrect and worth throwing away, though
        -- maybe there are cases when it is mostly not :thinking:
        -- My idea is to add "isSecure" flag to 'EnvValue' typelass to resolve
        -- this problem.
        "Failed to parse an environmental variable \
        \"+|errKey|+"="+|maybe "-" build errValue|+"\
        \: "+|errMessage|+""

-- | Pretty-print a 'EnvParseError'.
parseErrorPretty :: EnvParseError -> String
parseErrorPretty = pretty

instance Exception EnvParseError where
    displayException = pretty

-- | Parser for an environment variable.
--
-- Use 'fail' to report parsing errors, and 'noValue' to indicate
-- that value is left uninitialized.
newtype Parser a = Parser
    { runParser :: MaybeT (Except Text) a
      -- ^ Parsing result, Maybe layer designates result presence, while Except
      -- layer contains parsing errors.
    } deriving (Functor, Applicative, Monad)

instance MonadFail Parser where
    fail = Parser . lift . throwError . fromString

-- | Leave value uninitialized in config.
noValue :: Parser a
noValue = Parser mzero

-- | Describes a way to parse an item appearing in config.
class FromEnv a where
    -- | Parse a variable value.
    parseEnvValue :: Maybe String -> Parser a

-- | Apply the given parser to an environmental value if it present,
-- otherwise leave the configuration option uninitialized.
--
-- This is what most parsers usually do.
withPresent :: (String -> Parser a) -> Maybe String -> Parser a
withPresent parser = \case
    Nothing -> noValue
    Just val -> parser val

-- | Value parser based on 'IsString' instance.
parseStringEnvValue :: IsString a => Maybe String -> Parser a
parseStringEnvValue = withPresent $ pure . fromString

-- | Value parser based on 'Read' instance.
autoParseEnvValue :: Read a => Maybe String -> Parser a
autoParseEnvValue = withPresent $ \arg -> case reads arg of
    -- This is similar to what e.g. optparse-applicative does,
    -- the errors from 'eitherRead' are not too beautiful
    [(r, "")] -> return r
    [(_, s)]  -> fail $ "Extra trailing input '" <> s <> "'"
    []        -> fail "Failed to read value"
    _         -> fail "Value is ambigous"

-- | Value parser for numbers with overflow checks.
parseBoundedNumEnvValue
    :: forall a. (Bounded a, Integral a)
    => Maybe String -> Parser a
parseBoundedNumEnvValue val = do
    int <- autoParseEnvValue @Integer val
    if | int < fromIntegral (minBound @a) -> fail "Numeric underflow"
       | int > fromIntegral (maxBound @a) -> fail "Numeric overflow"
       | otherwise -> pure (fromIntegral int)

-- | Value parser based on 'Aeson.FromJSON' instance.
aesonParseEnvValue :: Aeson.FromJSON a => Maybe String -> Parser a
aesonParseEnvValue =
    withPresent $ either fail pure . Aeson.eitherDecode . encodeUtf8

-- | Options which define the expected format of environmental variables.
data ParseOptions = ParseOptions
    { keyBuilder :: NonEmpty Text -> Text
      -- ^ Given a full path to the current configuration option in top-down order,
      --   construct expected name of the associated environmental variable.
      --
      --   For default behavior see 'simpleKeyBuilder'.
    }

-- | Sensible default options for parsing.
defaultOptions :: ParseOptions
defaultOptions = ParseOptions
    { keyBuilder = simpleKeyBuilder
    }

-- | The default implementation for 'keyBuilder'. It behaves as follows:
--
--   * @["myservice", "db", "username"]@ is associated with
--     @MYSERVICE_DB_USERNAME@ variable name;
--
--   * @["db", "_username"]@ is associated with @DB_USERNAME@,
--     stripping underscore is convenient for configs derived from
--     datatypes. If you need the extra underscore to appear in env
--     variable name, prepend the field with @"__"@.
simpleKeyBuilder :: NonEmpty Text -> Text
simpleKeyBuilder =
  let stripUnderscore t = T.stripPrefix "_" t ?: t
  in mconcat . toList . NE.intersperse "_" . map (T.toUpper . stripUnderscore)

-- | Internal type which represents a path to a config variable in __bottom-up__
-- order.
type Path = [Text]

-- | Implements config traversal and parsing of each individual env variable.
class OptionsFromEnv (is :: [ItemKind]) where
    -- | Internal method which recursively parses env variables.
    envParser
        :: ParseOptions
        -> Map Text String
        -> Path
        -> Either EnvParseError (ConfigRec 'Partial is)

    -- | Internal method which collects names of expected env variables.
    -- Returns hand-crafted DList.
    gatherRequired :: ParseOptions -> Path -> Proxy is -> Endo [Text]

instance OptionsFromEnv '[] where
    envParser _ _ _ = pure RNil
    gatherRequired _ _ = mempty

instance
    forall l v is.
        ( KnownSymbol l
        , FromEnv v
        , OptionsFromEnv is)
    => OptionsFromEnv ((l ::: v) ': is)
  where
    envParser options env path =
        (:&) <$> fmap ItemOptionP parseOption <*> envParser options env path
      where
        parseOption :: Either EnvParseError (Maybe v)
        parseOption =
            let key = mkEnvKey @l options path
                mvalue = Map.lookup key env
            in first (EnvParseError key mvalue) $
                runExcept . runMaybeT $
                runParser (parseEnvValue mvalue)

    gatherRequired options path _ =
        Endo (mkEnvKey @l options path :) <>
        gatherRequired options path (Proxy @is)

-- | Internal method which constructs a env key for config item.
mkEnvKey :: forall l. KnownSymbol l => ParseOptions -> Path -> Text
mkEnvKey options path =
    keyBuilder options . NE.fromList $ reverse (symbolValT @l : path)

instance
    forall l us is.
        ( KnownSymbol l
        , OptionsFromEnv us
        , OptionsFromEnv is
        )
    => OptionsFromEnv ((l ::< us) ': is)
  where
    envParser options env path = (:&)
        <$> fmap ItemSub (envParser options env $ symbolValT @l : path)
        <*> envParser options env path
    gatherRequired options path _ =
        gatherRequired options (symbolValT @l : path) (Proxy @us) <>
        gatherRequired options path (Proxy @is)

instance
    forall l us is.
        ( KnownSymbol l
        , OptionsFromEnv (SumSelection l : us)
        , OptionsFromEnv is
        )
    => OptionsFromEnv ((l ::+ us) ': is)
  where
    envParser options env path = (:&)
        <$> fmap ItemSumP (envParser options env $ symbolValT @l : path)
        <*> envParser options env path
    gatherRequired options path _ =
        gatherRequired options (symbolValT @l : path) usp <>
        gatherRequired options path (Proxy @is)
      where
        usp = Proxy @(SumSelection l : us)

instance
    forall l us is.
        ( KnownSymbol l
        , OptionsFromEnv us
        , OptionsFromEnv is
        )
    => OptionsFromEnv ((l ::- us) ': is)
  where
    envParser options env path = (:&)
        <$> fmap ItemBranchP (envParser options env $ symbolValT @l : path)
        <*> envParser options env path
    gatherRequired options path _ =
        gatherRequired options (symbolValT @l : path) (Proxy @us) <>
        gatherRequired options path (Proxy @is)

-- | Internal helper for demoting type-level text.
symbolValT :: forall l. KnownSymbol l => Text
symbolValT = fromString $ symbolVal (Proxy @l)

-- | Parses configuration from environmental variables.
--
-- They will be parsed with respect to 'EnvValue'.
-- Expected variable names are constructed following the rules defined
-- in 'ParseOptions'.
parseEnv
    :: (MonadIO m, OptionsFromEnv is)
    => m (Either EnvParseError (ConfigRec 'Partial is))
parseEnv = parseEnvWith defaultOptions

-- | Version of 'parseEnv' which accepts the environment explicitly.
--
-- This can be used along with 'getEnvironment', methods from @dotenv@ package,
-- or combination of both.
--
-- When duplicated keys are present, the right-most values are preferred.
parseEnvPure
    :: OptionsFromEnv is
    => [(String, String)] -> Either EnvParseError (ConfigRec 'Partial is)
parseEnvPure = parseEnvPureWith defaultOptions

-- | Version of 'parseEnv' which allows specifying parsing options.
parseEnvWith
    :: (MonadIO m, OptionsFromEnv is)
    => ParseOptions
    -> m (Either EnvParseError (ConfigRec 'Partial is))
parseEnvWith options = parseEnvPureWith options <$> liftIO getEnvironment

-- | Version of 'parseEnv' which allows specifying parsing options and
-- accepts the environment explicitly.
parseEnvPureWith
    :: OptionsFromEnv is
    => ParseOptions
    -> [(String, String)]
    -> Either EnvParseError (ConfigRec 'Partial is)
parseEnvPureWith options env =
    envParser options (Map.fromList $ first fromString <$> env) []

-- | Returns names of all environmental variables which the given configuration
-- is going to read.
requiredVars :: OptionsFromEnv is => Proxy is -> [Text]
requiredVars = requiredVarsWith defaultOptions

-- | Version of 'requiredVars' which accepts custom options.
requiredVarsWith :: OptionsFromEnv is => ParseOptions -> Proxy is -> [Text]
requiredVarsWith options p = appEndo (gatherRequired options [] p) []

----------------------------------------------------------------------------
-- Instances
----------------------------------------------------------------------------

instance {-# OVERLAPPING #-} FromEnv String where
    parseEnvValue = parseStringEnvValue
instance FromEnv Text where
    parseEnvValue = parseStringEnvValue
instance FromEnv LText where
    parseEnvValue = parseStringEnvValue

instance FromEnv Int where
    parseEnvValue = parseBoundedNumEnvValue
instance FromEnv Int8 where
    parseEnvValue = parseBoundedNumEnvValue
instance FromEnv Int16 where
    parseEnvValue = parseBoundedNumEnvValue
instance FromEnv Int32 where
    parseEnvValue = parseBoundedNumEnvValue
instance FromEnv Int64 where
    parseEnvValue = parseBoundedNumEnvValue
instance FromEnv Integer where
    parseEnvValue = autoParseEnvValue
instance FromEnv Word where
    parseEnvValue = parseBoundedNumEnvValue
instance FromEnv Word8 where
    parseEnvValue = parseBoundedNumEnvValue
instance FromEnv Word16 where
    parseEnvValue = parseBoundedNumEnvValue
instance FromEnv Word32 where
    parseEnvValue = parseBoundedNumEnvValue
instance FromEnv Word64 where
    parseEnvValue = parseBoundedNumEnvValue
instance FromEnv Natural where
    parseEnvValue val = do
        int <- autoParseEnvValue @Integer val
        if int < 0
        then fail "Negative number"
        else pure (fromIntegral int)

instance FromEnv Float where
    parseEnvValue = autoParseEnvValue
instance FromEnv Double where
    parseEnvValue = autoParseEnvValue
instance Fixed.HasResolution a => FromEnv (Fixed.Fixed a) where
    parseEnvValue = autoParseEnvValue

instance FromEnv Bool where
    parseEnvValue = withPresent $ \case
      "0" -> pure False
      "1" -> pure True
      (map toLower -> "false") -> pure False
      (map toLower -> "true") -> pure True
      _ -> fail "Invalid boolean"

-- | Parses to @Nothing@ when value is not provided.
-- Never leaves config value uninitialized.
--
-- Note that if env variable is defined but empty, it will be parsed anyway.
instance FromEnv a => FromEnv (Maybe a) where
    parseEnvValue = \case
        Nothing -> pure Nothing
        Just val -> parseEnvValue (Just val)

instance FromEnv Aeson.Value where
    parseEnvValue = aesonParseEnvValue
