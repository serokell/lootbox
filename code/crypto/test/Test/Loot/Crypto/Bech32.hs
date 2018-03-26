module Test.Loot.Crypto.Bech32 where

import Universum

import Test.Tasty
import Test.Tasty.HUnit

import qualified Loot.Crypto.Bech32 as B32

--------------------------------------------
---- Test data
--------------------------------------------

validB32Strings :: [String]
validB32Strings =
    [
            "A12UEL5L"
          , "a12uel5l"
          , "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs"
          , "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw"
          , "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j"
          , "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w"
    ]

b32WithInvalidHRP :: [String]
b32WithInvalidHRP =
    [
          "1qzzfhee"
        , "10a06t8"
        , "1pzry9x0s0muk"
        , "pzry9x0s0muk"
    ]

b32WithExceededLength :: [String]
b32WithExceededLength =
    [
        "an84characterslonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1569pvx"
    ]

b32WithInvalidChecksum :: [String]
b32WithInvalidChecksum =
    [
          "A1G7SGD8"
        , "a1fuel5l"
        , "an83characterlonghumanreadablepartthatcontainsthenumber1fndtheexcludedcharactersbio1tt5tgs"
        , "abcdef1fpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw"
        , "11fqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j"
        , "split1fheckupstagehandshakeupstreamerranterredcaperred2y9e3w"
    ]

b32WithInvalidCharsetMap :: [String]
b32WithInvalidCharsetMap =
    [
          "a1bbel5l"
        , "a1ibel5l"
        , "a1obel5l"
    ]

b32WithCaseInconsistency :: [String]
b32WithCaseInconsistency =
    [
          "A12UeL5L"
        , "a12uFl5l"
        , "An83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs"
        , "Abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw"
        , "11Qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j"
        , "Split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w"
    ]

b32WithTooShortDataPart :: [String]
b32WithTooShortDataPart =
    [

          "a1f"
        , "a1ff"
        , "a1fff"
        , "a1ffff"
        , "a1fffff"
    ]

--------------------------------------------
--------------------------------------------
--------------------------------------------

-----------------------
-- Decoding
-----------------------


unit_validB32Strings :: Assertion
unit_validB32Strings = assertDecodeSuccess validB32Strings

unit_invalidHRP :: Assertion
unit_invalidHRP = assertDecodeError B32.InvalidHRP comment b32WithInvalidHRP
  where comment = "has invlaid human readable part."

unit_exceededLength :: Assertion
unit_exceededLength = assertDecodeError B32.Bech32StringLengthExceeded comment b32WithExceededLength
  where comment = "has exceeded length"

unit_invalidChecksums :: Assertion
unit_invalidChecksums = assertDecodeError B32.ChecksumVerificationFail comment b32WithInvalidChecksum
  where comment = "has invalid checksum"

unit_invalidCharsetMap :: Assertion
unit_invalidCharsetMap = assertDecodeError B32.InvalidCharsetMap comment b32WithInvalidCharsetMap
  where comment = "has invalid charset map"

unit_caseInconsistency :: Assertion
unit_caseInconsistency = assertDecodeError B32.CaseInconsistency comment b32WithCaseInconsistency
  where comment = "has case inconsitency"

unit_tooShortDataPart :: Assertion
unit_tooShortDataPart = assertDecodeError B32.TooShortDataPart comment b32WithTooShortDataPart
  where comment = "has too short data part"
-----------------------
-- Helpers
-----------------------

assertDecodeSuccess :: [String] -> Assertion
assertDecodeSuccess = assertDecode isRight
    "Is valid and should be decoded successfully."

assertDecodeError :: B32.DecodeError -> String -> [String] -> Assertion
assertDecodeError err comment = assertDecode (isError err) (comment ++ " and therefore is invalid.")

assertDecode :: (Either B32.DecodeError (B32.HumanReadablePart, ByteString) -> Bool)
    -> String
    -> [String]
    -> Assertion
assertDecode pred comment inp = forM_ inp validadteDecoding
  where
    validadteDecoding b32str = assert' . pred . B32.decode $ b32str
      where
        assert' = assertBool $ (show b32str)++" "++comment

isError :: Eq a => a -> Either a b -> Bool
isError e' (Left e) = e == e'
isError _ _         = False
