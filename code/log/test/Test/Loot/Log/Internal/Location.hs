{-# LANGUAGE OverloadedLists #-}

module Test.Loot.Log.Internal.Location where

import qualified Data.Text as T
import Fmt ((+|), (|+))
import GHC.Exts (fromList)

import Loot.Log.Internal.Location

import Hedgehog (Property, discard, forAll, property, (===))
import Test.Tasty.HUnit (Assertion, (@=?))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


unit_ModulePath_root_fromString :: Assertion
unit_ModulePath_root_fromString =
    ([] :: ModulePath) @=? ("" :: ModulePath)

unit_ModulePath_root_build :: Assertion
unit_ModulePath_root_build =
    ("" :: String) @=? (""+|([] :: ModulePath)|+"")

unit_ModulePath_build_single_empty :: Assertion
unit_ModulePath_build_single_empty =
    ("<empty>" :: String) @=? (""+|([""] :: ModulePath)|+"")

hprop_ModulePath_fromString :: Property
hprop_ModulePath_fromString = property $ do
    names <- forAll $
        Gen.list (Range.linear 1 20) (Gen.text (Range.linear 0 10) Gen.unicode)
    when (names == [""]) discard  -- There is a separate test for this

    let name = fromString (toString (T.intercalate "." names)) :: ModulePath

    fromList names === name
    concatModulePaths names === (""+|name|+"")
  where
    concatModulePaths :: [Text] -> Text
    concatModulePaths = T.intercalate "." . map fixEmpty

    fixEmpty :: Text -> Text
    fixEmpty name = if null name then "<empty>" else name

hprop_ModulePath_build :: Property
hprop_ModulePath_build = property $ do
    names <- forAll $
        Gen.list (Range.linear 1 20) (Gen.text (Range.linear 0 10) Gen.unicode)

    let name = fromList names :: ModulePath

    concatModulePaths names === (""+|name|+"")
  where
    concatModulePaths :: [Text] -> Text
    concatModulePaths = T.intercalate "." . map fixEmpty

    fixEmpty :: Text -> Text
    fixEmpty name = if null name then "<empty>" else name


unit_pkgModulePath_regular :: Assertion
unit_pkgModulePath_regular =
    "loot-log" @=? pkgModulePath "loot-log-0.0.0.0-IOX6snPmWg7HmNhr8VmwDb"

unit_pkgModulePath_single :: Assertion
unit_pkgModulePath_single =
    "main" @=? pkgModulePath "main"
