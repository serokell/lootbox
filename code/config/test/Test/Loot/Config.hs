{- This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.
 -}

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Test.Loot.Config where

import Data.Aeson (FromJSON, eitherDecode, encode, ToJSON)
import Fmt (Buildable, build, fmt)
import Loot.Base.HasLens (lensOf)
import Options.Applicative (Parser, auto, defaultPrefs, execParserPure, getParseResult, info, long)
import qualified Options.Applicative as O

import Loot.Config

import Hedgehog (Property, forAll, property, (===), Gen)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, (@=?))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

newtype SomeKek = SomeKek Integer deriving (Eq,Ord,Show,Read,Generic,FromJSON, ToJSON, Buildable)
newtype SomeMem = SomeMem String deriving (Eq,Ord,Show,IsString,Generic,FromJSON, ToJSON, Buildable)

type Fields = '[ "str" ::: Text
               , "int" ::: Int
               , "sub" ::< SubFields
               , "kek" ::: SomeKek
               , SumTree
               ]

type SubFields = '[ "int2" ::: Int
                  , "bool" ::: Bool
                  , "sub2" ::< Sub2Fields
                  ]

type Sub2Fields = '[ "str2" ::: Text
                   , "mem"  ::: SomeMem
                   ]

type SumTree = "tre" ::+ TreeFields

type TreeFields = '[ "str3" ::: Text
                   , "brc1" ::- BranchFields
                   , "brc2" ::- Branch2Fields
                   ]

type BranchFields = '[ "int3" ::: Int
                     ]

type Branch2Fields = '[ "str4" ::: Text
                      , "sub3" ::< Sub3Fields
                      ]

type Sub3Fields = '[ "int4" ::: Int ]

cfg :: PartialConfig Fields
cfg = mempty

cfgOptionPartial :: Gen (PartialConfig Fields)
cfgOptionPartial = do 
    text <- Gen.text (Range.linear 0 10) Gen.enumBounded
    int <- Gen.int Range.constantBounded
    pure $ cfg
      & option #str ?~ text
      & option #int ?~ int
      
fullConfig :: ConfigRec 'Partial Fields
fullConfig =
    cfg & option #str ?~ "hey"
        & option #int ?~ 12345
        & option #kek ?~ (SomeKek 999)
        & sub #sub . option #bool ?~ False
        & sub #sub . option #int2 ?~ 13579
        & sub #sub . sub #sub2 . option #str2 ?~ ""
        & sub #sub . sub #sub2 . option #mem ?~ (SomeMem "bye")
        & tree #tre . selection ?~ "brc1"
        & tree #tre . option #str3 ?~ "lemon"
        & tree #tre . branch #brc1 . option #int3 ?~ 54321


unit_emptyPartial :: Assertion
unit_emptyPartial = do
    let s :: Text
        s = "{str <unset>, int <unset>, sub =< {int2 <unset>, bool <unset>, \
            \sub2 =< {str2 <unset>, mem <unset>}}, kek <unset>, \
            \tre =+ {treType <unset>, str3 <unset>, brc1 =- {int3 <unset>}, \
            \brc2 =- {str4 <unset>, sub3 =< {int4 <unset>}}}}"
    s @=? show cfg


unit_lensesEmptyPartial :: Assertion
unit_lensesEmptyPartial = do
    cfg ^. option #str @=? Nothing
    cfg ^. option #int @=? Nothing
    cfg ^. sub #sub @=? (mempty :: PartialConfig SubFields)
    cfg ^. option #kek @=? Nothing
    cfg ^. tree #tre @=? (mempty :: PartialConfig '[SumTree]) ^. tree #tre

    cfg ^. sub #sub . option #int2 @=? Nothing
    cfg ^. sub #sub . option #bool @=? Nothing
    cfg ^. sub #sub . sub #sub2 @=? (mempty :: PartialConfig Sub2Fields)

    cfg ^. sub #sub . sub #sub2 . option #str2 @=? Nothing

    cfg ^. tree #tre . option #treType @=? Nothing
    cfg ^. tree #tre . selection @=? Nothing
    cfg ^. tree #tre . option #str3 @=? Nothing
    cfg ^. tree #tre . branch #brc1 @=? (mempty :: PartialConfig BranchFields)
    cfg ^. tree #tre . branch #brc2 @=? (mempty :: PartialConfig Branch2Fields)

    cfg ^. tree #tre . branch #brc1 . option #int3 @=? Nothing

    cfg ^. tree #tre . branch #brc2 . option #str4 @=? Nothing
    cfg ^. tree #tre . branch #brc2 . sub #sub3 @=? (mempty :: PartialConfig Sub3Fields)

    cfg ^. tree #tre . branch #brc2 . sub #sub3 . option #int4 @=? Nothing

hprop_lensOptionPartial :: Property
hprop_lensOptionPartial = property $ do
    text <- forAll $ Gen.text (Range.linear 0 10) Gen.enumBounded
    let cfg1 = cfg & option #str ?~ text
    cfg1 ^. option #str === Just text

    int <- forAll $ Gen.int Range.constantBounded
    let cfg2 = cfg1 & option #int ?~ int
    cfg2 ^. option #str === Just text
    cfg2 ^. option #int === Just int

    let cfg3 = cfg1 & option #int .~ Nothing
    cfg3 ^. option #str === Just text
    cfg3 ^. option #int === Nothing

hprop_lensSubOptionPartial :: Property
hprop_lensSubOptionPartial = property $ do
    int <- forAll $ Gen.int Range.constantBounded
    let cfg1 = cfg & sub #sub . option #int2 ?~ int
    cfg1 ^. sub #sub . option #int2 === Just int

    text <- forAll $ Gen.text (Range.linear 0 10) Gen.enumBounded
    let cfg2 = cfg1 & sub #sub . sub #sub2 . option #str2 ?~ text
    cfg2 ^. sub #sub . option #int2 === Just int
    cfg2 ^. sub #sub . sub #sub2 . option #str2 === Just text

hprop_lensTreeOptionPartial :: Property
hprop_lensTreeOptionPartial = property $ do
    text <- forAll $ Gen.text (Range.linear 0 10) Gen.enumBounded
    let cfg1 = cfg & tree #tre . option #str3 ?~ text
    cfg1 ^. tree #tre . option #str3 === Just text

    int <- forAll $ Gen.int Range.constantBounded
    let cfg2 = cfg1 & tree #tre . branch #brc1 . option #int3 ?~ int
    cfg2 ^. tree #tre . option #str3 === Just text
    cfg2 ^. tree #tre . branch #brc1 . option #int3 === Just int

    text2 <- forAll $ Gen.text (Range.linear 0 10) Gen.enumBounded
    let cfg3 = cfg2 & tree #tre . branch #brc2 . option #str4 ?~ text2
    cfg3 ^. tree #tre . branch #brc2 . option #str4 === Just text2
    cfg3 ^. tree #tre . option #str3 === Just text
    cfg3 ^. tree #tre . branch #brc1 . option #int3 === Just int


hprop_mappendPartial :: Property
hprop_mappendPartial = property $ do
    text1 <- forAll $ Gen.text (Range.linear 0 10) Gen.enumBounded
    let cfg1 = cfg & option #str ?~ text1

    let cfg01 = cfg <> cfg1
    cfg01 ^. option #str === Just text1
    cfg01 ^. option #int === Nothing

    text2 <- forAll $ Gen.text (Range.linear 0 10) Gen.enumBounded
    int <- forAll $ Gen.int Range.constantBounded
    let cfg2 = cfg & option #str ?~ text2
                   & option #int ?~ int

    let cfg02 = cfg <> cfg2
    cfg02 ^. option #str === Just text2
    cfg02 ^. option #int === Just int

    let cfg12 = cfg1 <> cfg2
    cfg12 === cfg02

    text3 <- forAll $ Gen.text (Range.linear 0 10) Gen.enumBounded
    let cfg3 = cfg & sub #sub . sub #sub2 . option #str2 ?~ text3

    let cfg123 = cfg1 <> cfg2 <> cfg3
    cfg123 ^. option #str === Just text2
    cfg123 ^. option #int === Just int
    cfg123 ^. sub #sub . sub #sub2 . option #str2 === Just text3

    text4 <- forAll $ Gen.text (Range.linear 0 10) Gen.enumBounded
    int2 <- forAll $ Gen.int Range.constantBounded
    let cfg4 = cfg & tree #tre . option #str3 ?~ text4
                   & tree #tre . branch #brc1 . option #int3 ?~ int2
    cfg4 ^. tree #tre . option #str3 === Just text4
    cfg4 ^. tree #tre . branch #brc1 . option #int3 === Just int2

    text5 <- forAll $ Gen.text (Range.linear 0 10) Gen.enumBounded
    text6 <- forAll $ Gen.text (Range.linear 0 10) Gen.enumBounded
    let cfg5 = cfg & tree #tre . branch #brc2 . option #str4 ?~ text5
                   & tree #tre . option #str3 ?~ text6

    let cgf12345 = mconcat [cfg1, cfg2, cfg3, cfg4, cfg5]
    cgf12345 ^. option #str === Just text2
    cgf12345 ^. option #int === Just int
    cgf12345 ^. sub #sub . sub #sub2 . option #str2 === Just text3
    cgf12345 ^. tree #tre . option #str3 === Just text6
    cgf12345 ^. tree #tre . branch #brc1 . option #int3 === Just int2
    cgf12345 ^. tree #tre . branch #brc2 . option #str4 === Just text5

-- | Helper for testing JSON decoding.
testDecode :: String -> PartialConfig Fields -> Assertion
testDecode str expected = Right expected @=? eitherDecode (fromString str)

unit_parseJsonEmpty :: Assertion
unit_parseJsonEmpty = testDecode "{}" cfg

unit_parseJson1 :: Assertion
unit_parseJson1 =
    testDecode "{ \"str\": \"hi\" }" $
        cfg & option #str ?~ "hi"

unit_parseJson2 :: Assertion
unit_parseJson2 =
    testDecode "{ \"str\": \"hi\", \"int\": 4 }" $
        cfg & option #str ?~ "hi"
            & option #int ?~ 4

unit_parseJsonSubEmpty :: Assertion
unit_parseJsonSubEmpty =
    testDecode "{ \"str\": \"hi\", \"sub\": {} }" $
        cfg & option #str ?~ "hi"

unit_parseJsonSub :: Assertion
unit_parseJsonSub =
    testDecode "{ \"str\": \"hi\", \"sub\": { \"bool\": true } }" $
        cfg & option #str ?~ "hi"
            & sub #sub . option #bool ?~ True

unit_parseJsonSubSub :: Assertion
unit_parseJsonSubSub =
    testDecode "{ \"sub\": { \"sub2\": { \"str2\": \"hi\" } } }" $
        cfg & sub #sub . sub #sub2 . option #str2 ?~ "hi"

unit_parseJsonTreeEmpty :: Assertion
unit_parseJsonTreeEmpty =
    testDecode "{ \"str\": \"hi\", \"tre\": {} }" $
        cfg & option #str ?~ "hi"

unit_parseJsonTree1 :: Assertion
unit_parseJsonTree1 =
    testDecode "{ \"tre\": { \"str3\": \"common\", \"brc1\": { \"int3\": 15 } } }" $
        cfg & tree #tre . option #str3 ?~ "common"
            & tree #tre . branch #brc1 . option #int3 ?~ 15

unit_parseJsonTree2 :: Assertion
unit_parseJsonTree2 =
    testDecode "{ \"tre\": { \"brc2\": { \"sub3\": { \"int4\": 20 } } } }" $
        cfg & tree #tre . branch #brc2 . sub #sub3 . option #int4 ?~ 20

unit_parseJsonTree3 :: Assertion
unit_parseJsonTree3 =
    testDecode "{ \"tre\": { \"treType\": \"brc1\", \"brc1\": { \"int3\": 10 } } }" $
        cfg & tree #tre . selection ?~ "brc1"
            & tree #tre . branch #brc1 . option #int3 ?~ 10

-- | Helper for testing JSON roundtrip.
testRoundtrip :: PartialConfig Fields -> Assertion
testRoundtrip config = Right config @=? (eitherDecode . encode) config

unit_jsonRoundtripEmptyPartial :: Assertion
unit_jsonRoundtripEmptyPartial = testRoundtrip cfg

hprop_jsonRoundtripOptionPartial :: Property
hprop_jsonRoundtripOptionPartial = property $ do
  config <- forAll cfgOptionPartial
  (eitherDecode . encode) config === Right config

unit_jsonRoundtripFullConfig :: Assertion
unit_jsonRoundtripFullConfig = testRoundtrip fullConfig

-- | Helper for testing Fmt.Buildable instance.
testBuild :: PartialConfig Fields -> [Text] -> Assertion
testBuild config expected = unlines expected @=? (fmt . build) config

unit_fmtBuildableEmptyConfig :: Assertion
unit_fmtBuildableEmptyConfig = testBuild cfg expected
  where
    expected =
      [ "str: <undefined>"
      , "int: <undefined>"
      , "sub:"
      , "  int2: <undefined>"
      , "  bool: <undefined>"
      , "  sub2:"
      , "    str2: <undefined>"
      , "    mem: <undefined>"
      , "kek: <undefined>"
      , "tre:"
      , "  treType: <undefined>"
      , "  str3: <undefined>"
      , "  brc1: int3: <undefined>"
      , "  brc2:"
      , "    str4: <undefined>"
      , "    sub3: int4: <undefined>"
      ]
  
  
-----------------------
-- Finalisation
-----------------------

unit_finaliseEmpty :: Assertion
unit_finaliseEmpty =
    Left missing @=? finalise cfg
  where
    missing =
        [ "str", "int"
        , "sub.int2", "sub.bool"
        , "sub.sub2.str2"
        , "sub.sub2.mem"
        , "kek"
        , "tre.treType", "tre.str3"
        ]

unit_finaliseSome :: Assertion
unit_finaliseSome = do
    let cfg1 = cfg & option #str ?~ "hi"
                   & sub #sub . option #bool ?~ False
                   & sub #sub . sub #sub2 . option #str2 ?~ ""
                   & option #kek ?~ (SomeKek 1)
                   & tree #tre . selection ?~ "brc1"
    Left missing @=? finalise cfg1
  where
    missing =
        [ "int"
        , "sub.int2"
        , "sub.sub2.mem"
        , "tre.str3"
        , "tre.brc1.int3"
        ]

unit_finalise :: Assertion
unit_finalise = do
    let cfg1 = fullConfig
    let efinalCfg = finalise cfg1

    case efinalCfg of
        Left _ -> assertFailure "Valid config was not finalised properly"
        Right finalCfg -> do
            "hey"           @=? finalCfg ^. option #str
            12345           @=? finalCfg ^. option #int
            (SomeKek 999)   @=? finalCfg ^. option #kek
            False           @=? finalCfg ^. sub #sub . option #bool
            13579           @=? finalCfg ^. sub #sub . option #int2
            ""              @=? finalCfg ^. sub #sub . sub #sub2 . option #str2
            (SomeMem "bye") @=? finalCfg ^. sub #sub . sub #sub2 . option #mem
            "brc1"          @=? finalCfg ^. tree #tre . selection
            "lemon"         @=? finalCfg ^. tree #tre . option #str3
            case finalCfg ^. tree #tre . branch #brc1 of
                Nothing -> assertFailure "Valid config was not finalised properly"
                Just brc ->
                    54321   @=? brc ^. option #int3

            finalCfg ^. (lensOf @SomeKek) @=? (SomeKek 999)
            finalCfg ^. (lensOf @SomeMem) @=? (SomeMem "bye")
            finalCfg ^. (lensOfC @('["kek"])) @=? (SomeKek 999)
            finalCfg ^. (lensOfC @('["sub", "sub2", "mem"])) @=? (SomeMem "bye")

----------------------
-- CLI modifications
----------------------

runCliArgs :: Parser a -> [String] -> Maybe a
runCliArgs p = getParseResult . execParserPure defaultPrefs (info p mempty)

fieldsParser :: OptModParser Fields
fieldsParser =
    #str .:: (O.strOption $ long "str") <*<
    #int .:: (O.option auto $ long "int") <*<
    #sub .:<
        (#int2 .:: (O.option auto $ long "int2") <*<
         #bool .:: (O.flag' True $ long "bool") <*<
         #sub2 .:<
              (#str2 .:: (O.strOption $ long "str2") <*<
               #mem .:: (O.strOption $ long "mem"))
        ) <*<
    #kek .:: (O.option auto $ long "kek") <*<
    #tre .:+ 
        (#treType .:: (O.strOption $ long "treType") <*<
         #str3 .:: (O.strOption $ long "str3") <*<
         #brc1 .:-
            (#int3 .:: (O.option auto $ long "int3")
            ) <*<
         #brc2 .:-
            (#str4 .:: (O.strOption $ long "str4") <*<
             #sub3 .:<
                (#int4 .:: (O.option auto $ long "int4")
                )
            )
        )

unit_cliOverrideEmptyId :: Assertion
unit_cliOverrideEmptyId = do
    noMod <- maybe (assertFailure "Config parser fails on empty arguments") pure $
             runCliArgs fieldsParser []
    noMod cfg @=? cfg
    noMod fullConfig @=? fullConfig

someArgs :: [String]
someArgs =
    [ "--int", "228"
    , "--mem", "hi"
    , "--bool"
    , "--kek", "SomeKek 777"
    , "--int4", "12321"
    ]

unit_cliOverrideSetNew :: Assertion
unit_cliOverrideSetNew = do
    someMod <- maybe (assertFailure "Config parser failed") pure $
               runCliArgs fieldsParser someArgs
    let cfg1 = someMod cfg
    assertEqual "CLI parser modifies empty config incorrectly" cfg1 $
        cfg & option #int ?~ 228
            & sub #sub . option #bool ?~ True
            & sub #sub . sub #sub2 . option #mem ?~ (SomeMem "hi")
            & option #kek ?~ (SomeKek 777)
            & tree #tre . branch #brc2 . sub #sub3 . option #int4 ?~ 12321

unit_cliOverrideModExisting :: Assertion
unit_cliOverrideModExisting = do
    someMod <- maybe (assertFailure "Config parser failed") pure $
               runCliArgs fieldsParser someArgs
    let cfg1 = someMod fullConfig
    assertEqual "CLI parser modifies non-empty config incorrectly" cfg1 $
        fullConfig & option #int ?~ 228
                   & sub #sub . option #bool ?~ True
                   & sub #sub . sub #sub2 . option #mem ?~ (SomeMem "hi")
                   & option #kek ?~ (SomeKek 777)
                   & tree #tre . branch #brc2 . sub #sub3 . option #int4 ?~ 12321
