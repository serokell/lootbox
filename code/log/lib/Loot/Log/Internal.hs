{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Internals of logging.
module Loot.Log.Internal
       ( LogEvent

       , Level (..)

       , Name (Name, unName)
       , fromList
       , fromString

       , nameFromStack
       , pkgName

       , Logging (..)
       , MonadLogging (..)

       , logDebug
       , logInfo
       , logNotice
       , logWarning
       , logError
       ) where

import Prelude hiding (log, toList)

import Data.DList (DList)
import Fmt (Buildable (build), Builder, fmt, (+|), (|+))
import Fmt.Internal (FromBuilder (fromBuilder))
import GHC.Exts (IsList (Item, fromList, toList), IsString (fromString))
import GHC.Stack (CallStack, HasCallStack,
                  SrcLoc (SrcLoc, srcLocModule, srcLocPackage, srcLocStartLine), callStack,
                  getCallStack)
import Monad.Capabilities (makeCap)
import Text.Show (Show (show))

import qualified Data.Text as T


-- | An event that gets logged (in most cases just text).
newtype LogEvent = LogEvent { getLogEvent :: Text }

instance IsString LogEvent where
    fromString = LogEvent . fromString

instance FromBuilder LogEvent where
    fromBuilder = LogEvent . fromBuilder


-- | Logging level.
data Level
    = Debug     -- ^ Things nobody should see unless it's explicitly stated.
    | Info      -- ^ Regular information for user.
    | Notice    -- ^ Something that should be more noticable than 'Info'.
    | Warning   -- ^ Suspicious warning conditions.
    | Error     -- ^ Errors.
    deriving (Eq, Generic, Show)


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


-- | Logging capability.
data Logging m = Logging
    { _log :: Level -> Name -> Text -> m ()
    }
makeCap ''Logging

logDebug :: (HasCallStack, MonadLogging m) => LogEvent -> m ()
logDebug = log Debug (nameFromStack callStack) . getLogEvent

logInfo :: (HasCallStack, MonadLogging m) => LogEvent -> m ()
logInfo = log Info (nameFromStack callStack) . getLogEvent

logNotice :: (HasCallStack, MonadLogging m) => LogEvent -> m ()
logNotice = log Notice (nameFromStack callStack) . getLogEvent

logWarning :: (HasCallStack, MonadLogging m) => LogEvent -> m ()
logWarning = log Warning (nameFromStack callStack) . getLogEvent

logError :: (HasCallStack, MonadLogging m) => LogEvent -> m ()
logError = log Error (nameFromStack callStack) . getLogEvent
