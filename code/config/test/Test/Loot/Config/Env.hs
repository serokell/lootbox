{- This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.
 -}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Test.Loot.Config.Env where

import Data.Aeson (FromJSON)
import qualified Data.List as L
import Fmt (build, fmt)

import Loot.Config
import Loot.Config.Env hiding (Parser)

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))

newtype Option1 = Option1 String
  deriving (Eq, Ord, Show, IsString, Generic, FromJSON)

instance FromEnv Option1 where
    parseEnvValue val =
        maybe (fail "Wrong prefix") (pure . Option1) $
        L.stripPrefix "Mem " val

type Fields =
   '[ "int" ::: Int
    , "sub" ::< SubFields
    ]

type SubFields =
   '[ "bool" ::: Bool
    , "myStr" ::: Text
    , "option1" ::: Option1
    ]

test_envParsing :: [TestTree]
test_envParsing =
    [ testCase "Can parse simple config successfully" $ do
          let cfg =
                either (error . show) id . finalise $
                either (error . fmt . build) id $
                parseEnvPure @SubFields
                    [ ("MYSTR", "nyan")
                    , ("BOOL", "0")
                    , ("OPTION1", "Mem text")
                    ]
          cfg ^. option #bool @?= False
          cfg ^. option #myStr @?= "nyan"
          cfg ^. option #option1 @?= "text"

    , testCase "Parsing errors works" $
          parseEnvPure @SubFields [("OPTION1", "text")]
              @?= Left EnvParseError
                  { errKey = "OPTION1", errValue = "text"
                  , errMessage = "Wrong prefix" }

    , testCase "Number parser does not allow overflow" $
          (parseEnvPure @Fields [("INT", replicate 20 '9')]
              & first errMessage)
              @?= Left "Numeric overflow"

    , testCase "Absent value is not present in the config" $ do
          let cfg =
                either (error . fmt . build) id $
                parseEnvPure @Fields []
          cfg ^. option #int @?= Nothing
    ]

unit_requiredEnvVars :: Assertion
unit_requiredEnvVars = do
    requiredVars (Proxy @Fields) @?=
        ["INT", "SUB_BOOL", "SUB_MYSTR", "SUB_OPTION1"]
