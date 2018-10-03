{-# LANGUAGE TypeFamilies #-}

module Loot.Log.Internal.Location
       ( ModulePath

       , Location (..)
       , locationFromSrcLoc
       , locationFromStack

       , pkgModulePath
       ) where

import qualified Data.Text as T
import Fmt (Buildable (build), fmt)
import GHC.Exts (IsList (Item, fromList, toList), IsString (fromString))
import GHC.Stack (CallStack, SrcLoc (SrcLoc, srcLocModule, srcLocPackage, srcLocStartLine),
                  getCallStack)
import Text.Show (Show (show))


-- | Location module path.
newtype ModulePath = ModulePath { unModulePath :: [Text] }
    deriving (Eq, Semigroup, Monoid)

instance IsList ModulePath where
    type Item ModulePath = Text
    fromList = ModulePath
    toList = unModulePath

-- | Split a dot-separated string.
-- Empty string turns into a 'ModulePath' with zero components.
instance IsString ModulePath where
    fromString s  = ModulePath $ case s of
        "" -> []
        s' -> T.splitOn "." (fromString s')

instance Buildable ModulePath where
    build = mconcat . intersperse "." . map build'. unModulePath
      where
        build' "" = "<empty>"
        build' s  = build s

instance Show ModulePath where
    show = fmt . build


-- | Logging event location information.
data Location = Location
    { lPackage :: Text
    , lModule  :: ModulePath
    , lLine    :: Int
    } deriving (Eq, Show)


-- | Turn 'SrcLoc' into more usable 'Location'.
locationFromSrcLoc :: SrcLoc -> Location
locationFromSrcLoc SrcLoc{srcLocPackage, srcLocModule, srcLocStartLine} = Location
    { lPackage = pkgModulePath srcLocPackage
    , lModule  = fromString srcLocModule
    , lLine = srcLocStartLine
    }

-- | Extract just the name of the package from the crazy identifier that GHC gives us.
pkgModulePath :: String -> Text
pkgModulePath full = extract . T.breakOnAll "-" . toText $ full
  where
    -- | Take the penultimate component.
    -- In cases when there are less than 2 components, do something.
    extract :: [(Text, Text)] -> Text
    extract []             = toText full
    extract [(n, _)]       = n
    extract ((n, _) : [_]) = n
    extract (_ : ns)       = extract ns

-- | Turn the location of the last item on the call stack into 'Location'.
locationFromStack :: CallStack -> Maybe Location
locationFromStack = fmap (locationFromSrcLoc . snd) . listToMaybe . getCallStack
