{-# LANGUAGE TypeFamilies #-}
module Loot.Log.Internal.Name
       ( Name (Name, unName)
       , fromList
       , fromString

       , nameFromStack
       , pkgName

       , NameSelector (..)
       , selectLogName
       ) where

import Prelude hiding (toList)

import Data.Aeson (FromJSON (..), ToJSON(..), Value (String))
import Data.DList (DList)
import Fmt (Buildable (build), fmt, (+|), (|+))
import GHC.Exts (IsList (Item, fromList, toList), IsString (fromString))
import GHC.Stack (CallStack, HasCallStack, getCallStack,
                  SrcLoc (SrcLoc, srcLocModule, srcLocPackage, srcLocStartLine))
import Text.Show (Show (show))

import qualified Data.Text as T

-- | Logger name (namespace).
newtype Name = Name { unName :: DList Text }
    deriving (Eq, Semigroup, Monoid)

instance IsList Name where
    type Item Name = Text
    fromList = Name . fromList
    toList = toList . unName

-- | Split a dot-separated string.
-- Empty string turns into a 'Name' with zero components.
instance IsString Name where
    fromString s  = fromList $ case s of
        "" -> []
        s' -> T.splitOn "." (fromString s')

instance Buildable Name where
    build = mconcat . intersperse "." . map build'. toList . unName
      where
        build' "" = "<empty>"
        build' s  = build s

instance Show Name where
    show = fmt . build

instance FromJSON Name where
    parseJSON = fmap fromString . parseJSON

instance ToJSON Name where
    toJSON = String . fmt . build

-- | Turn the location of the last item on the call stack into 'Name'
nameFromStack :: CallStack -> Name
nameFromStack stack = case getCallStack stack of
    (_, SrcLoc {srcLocPackage, srcLocModule, srcLocStartLine}) : _ -> fromString $
        ""+|pkgName srcLocPackage|+"."+|srcLocModule|+".line#"+|srcLocStartLine|+""
    _ -> "<unknown location>"

-- | Extract just the name of the package from the crazy identifier that GHC gives us.
pkgName :: String -> Text
pkgName full = extract . T.breakOnAll "-" . toText $ full
  where
    -- | Take the penultimate component.
    -- In cases when there are less than 2 components, do something.
    extract :: [(Text, Text)] -> Text
    extract []             = toText full
    extract [(n, _)]       = n
    extract ((n, _) : [_]) = n
    extract (_ : ns)       = extract ns

-- | One may wish to use names derived from callstack
-- or manually specified ones.
data NameSelector
    = CallstackName
    | GivenName Name

-- | Take a 'Name' according to 'NameSelector'.
selectLogName :: HasCallStack => CallStack -> NameSelector -> Name
selectLogName cs CallstackName = nameFromStack cs
selectLogName _ (GivenName n)  = n
