{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Test.Loot.Config where

import Universum

import Data.Aeson (eitherDecode)

import Loot.Config

import Hedgehog (Property, forAll, property, (===))
import Test.Tasty.HUnit (Assertion, assertFailure, (@=?))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


type Fields = '[ "str" ::: String
               , "int" ::: Int
               , "sub" ::< SubFields
               ]

type SubFields = '[ "int2" ::: Int
                  , "bool" ::: Bool
                  , "sub2" ::< Sub2Fields
                  ]

type Sub2Fields = '[ "str2" ::: String
                   ]

cfg :: PartialConfig Fields
cfg = mempty


unit_emptyPartial :: Assertion
unit_emptyPartial = do
    let s :: Text
        s = "{str <unset>, int <unset>, sub =< {int2 <unset>, bool <unset>, sub2 =< {str2 <unset>}}}"
    s @=? show cfg


unit_lensesEmptyPartial :: Assertion
unit_lensesEmptyPartial = do
    cfg ^. option #str @=? Nothing
    cfg ^. option #int @=? Nothing
    cfg ^. sub #sub @=? (mempty :: PartialConfig SubFields)

    cfg ^. sub #sub . option #int2 @=? Nothing
    cfg ^. sub #sub . option #bool @=? Nothing
    cfg ^. sub #sub . sub #sub2 @=? (mempty :: PartialConfig Sub2Fields)

    cfg ^. sub #sub . sub #sub2 . option #str2 @=? Nothing

hprop_lensOptionPartial :: Property
hprop_lensOptionPartial = property $ do
    str <- forAll $ Gen.string (Range.linear 0 10) Gen.enumBounded
    let cfg1 = cfg & option #str ?~ str
    cfg1 ^. option #str === Just str

    int <- forAll $ Gen.int Range.constantBounded
    let cfg2 = cfg1 & option #int ?~ int
    cfg2 ^. option #str === Just str
    cfg2 ^. option #int === Just int

    let cfg3 = cfg1 & option #int .~ Nothing
    cfg3 ^. option #str === Just str
    cfg3 ^. option #int === Nothing

hprop_lensSubOptionPartial :: Property
hprop_lensSubOptionPartial = property $ do
    int <- forAll $ Gen.int Range.constantBounded
    let cfg1 = cfg & sub #sub . option #int2 ?~ int
    cfg1 ^. sub #sub . option #int2 === Just int

    str <- forAll $ Gen.string (Range.linear 0 10) Gen.enumBounded
    let cfg2 = cfg1 & sub #sub . sub #sub2 . option #str2 ?~ str
    cfg2 ^. sub #sub . option #int2 === Just int
    cfg2 ^. sub #sub . sub #sub2 . option #str2 === Just str


hprop_mappendPartial :: Property
hprop_mappendPartial = property $ do
    str1 <- forAll $ Gen.string (Range.linear 0 10) Gen.enumBounded
    let cfg1 = cfg & option #str ?~ str1

    let cfg01 = cfg <> cfg1
    cfg01 ^. option #str === Just str1
    cfg01 ^. option #int === Nothing

    str2 <- forAll $ Gen.string (Range.linear 0 10) Gen.enumBounded
    int <- forAll $ Gen.int Range.constantBounded
    let cfg2 = cfg & option #str ?~ str2
                   & option #int ?~ int

    let cfg02 = cfg <> cfg2
    cfg02 ^. option #str === Just str2
    cfg02 ^. option #int === Just int

    let cfg12 = cfg1 <> cfg2
    cfg12 === cfg02

    str3 <- forAll $ Gen.string (Range.linear 0 10) Gen.enumBounded
    let cfg3 = cfg & sub #sub . sub #sub2 . option #str2 ?~ str3

    let cfg123 = cfg1 <> cfg2 <> cfg3
    cfg123 ^. option #str === Just str2
    cfg123 ^. option #int === Just int
    cfg123 ^. sub #sub . sub #sub2 . option #str2 === Just str3


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
        ]

unit_finaliseSome :: Assertion
unit_finaliseSome = do
    let cfg1 = cfg & option #str ?~ "hi"
                   & sub #sub . option #bool ?~ False
                   & sub #sub . sub #sub2 . option #str2 ?~ ""
    Left missing @=? finalise cfg1
  where
    missing =
        [ "int"
        , "sub.int2"
        ]

unit_finalise :: Assertion
unit_finalise = do
    let cfg1 = cfg & option #str ?~ "hey"
                   & option #int ?~ 12345
                   & sub #sub . option #bool ?~ False
                   & sub #sub . option #int2 ?~ 13579
                   & sub #sub . sub #sub2 . option #str2 ?~ ""
    let efinalCfg = finalise cfg1

    case efinalCfg of
        Left _ -> assertFailure "Valid config was not finalised properly"
        Right finalCfg -> do
            "hey" @=? finalCfg ^. option #str
            12345 @=? finalCfg ^. option #int
            False @=? finalCfg ^. sub #sub . option #bool
            13579 @=? finalCfg ^. sub #sub . option #int2
            ""    @=? finalCfg ^. sub #sub . sub #sub2 . option #str2
