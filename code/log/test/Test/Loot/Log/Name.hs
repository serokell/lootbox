{-# LANGUAGE OverloadedLists #-}

module Test.Loot.Log.Name where

import Fmt ((+|), (|+))

import Loot.Log.Internal

import Hedgehog (Property, discard, forAll, property, (===))
import Test.Tasty.HUnit (Assertion, (@=?))

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Data.Text as T
import GHC.Exts (IsList (..))

unit_Name_root_fromString :: Assertion
unit_Name_root_fromString =
    ([] :: Name) @=? ("" :: Name)

unit_Name_root_build :: Assertion
unit_Name_root_build =
    ("" :: String) @=? (""+|([] :: Name)|+"")

unit_Name_build_single_empty :: Assertion
unit_Name_build_single_empty =
    ("<empty>" :: String) @=? (""+|([""] :: Name)|+"")

hprop_Name_fromString :: Property
hprop_Name_fromString = property $ do
    names <- forAll $
        Gen.list (Range.linear 1 20) (Gen.text (Range.linear 0 10) Gen.unicode)
    when (names == [""]) discard  -- There is a separate test for this

    let name = fromString (toString (T.intercalate "." names)) :: Name

    GHC.Exts.fromList names === name
    concatNames names === (""+|name|+"")
  where
    concatNames :: [Text] -> Text
    concatNames = T.intercalate "." . map fixEmpty

    fixEmpty :: Text -> Text
    fixEmpty name = if null name then "<empty>" else name

hprop_Name_build :: Property
hprop_Name_build = property $ do
    names <- forAll $
        Gen.list (Range.linear 1 20) (Gen.text (Range.linear 0 10) Gen.unicode)

    let name = GHC.Exts.fromList names :: Name

    concatNames names === (""+|name|+"")
  where
    concatNames :: [Text] -> Text
    concatNames = T.intercalate "." . map fixEmpty

    fixEmpty :: Text -> Text
    fixEmpty name = if null name then "<empty>" else name


unit_pkgName_regular :: Assertion
unit_pkgName_regular =
    "loot-log" @=? pkgName "loot-log-0.0.0.0-IOX6snPmWg7HmNhr8VmwDb"

unit_pkgName_single :: Assertion
unit_pkgName_single =
    "main" @=? pkgName "main"
