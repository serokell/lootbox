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

       , NameSelector (..)
       , Logging (..)
       , hoistLogging
       , MonadLogging (..)

       , defaultLog
       , defaultLogName
       , logNameSelectorL
       , logNameL

       , logDebug
       , logInfo
       , logNotice
       , logWarning
       , logError
       ) where

import Prelude hiding (log, toList)

import Control.Lens (Setter', makePrisms, sets, views)
import Data.DList (DList)
import Ether.Internal (HasLens (..))
import Fmt (Buildable (build), fmt, (+|), (|+))
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

-- | One may wish to use names derived from callstack
-- or manually specified ones.
data NameSelector
    = CallstackName
    | GivenName Name

makePrisms ''NameSelector

-- | Take a 'Name' according to 'NameSelector'.
selectLogName :: HasCallStack => CallStack -> NameSelector -> Name
selectLogName cs CallstackName = nameFromStack cs
selectLogName _ (GivenName n)  = n

-- | Logging capability.
data Logging m = Logging
    { _log     :: Level -> Name -> Text -> m ()
    , _logName :: m NameSelector
    }
makeCap ''Logging

-- | Default implementation of 'MonadLogging.log' (generated with 'makeCap')
-- for 'ReaderT ctx IO' monad stack.
defaultLog
    :: forall m ctx.
       (HasLens (Logging m) ctx (Logging m), MonadReader ctx m)
    => Level -> Name -> Text -> m ()
defaultLog l n t = do
    lg <- views (lensOf @(Logging m)) _log
    lg l n t

-- | Default implementation of 'MonadLogging.logName' (generated with 'makeCap')
-- for 'ReaderT ctx IO' monad stack.
defaultLogName
    :: forall m ctx.
       (HasLens NameSelector ctx NameSelector, MonadReader ctx m)
    => m NameSelector
defaultLogName = view (lensOf @NameSelector)

logNameSelectorL :: Functor m => Setter' (Logging m) NameSelector
logNameSelectorL = sets $ \f l -> l{ _logName = fmap f (_logName l) }

logNameL :: Functor m => Setter' (Logging m) Name
logNameL = logNameSelectorL . _GivenName

-- | Helper function for use 'logDebug' and family.
logWith :: (Monad m, MonadLogging m) => Level -> CallStack -> LogEvent -> m ()
logWith level cs ev = do
    name <- selectLogName cs <$> logName
    log level name (getLogEvent ev)

logDebug :: (HasCallStack, Monad m, MonadLogging m) => LogEvent -> m ()
logDebug = logWith Debug callStack

logInfo :: (HasCallStack, Monad m, MonadLogging m) => LogEvent -> m ()
logInfo = logWith Info callStack

logNotice :: (HasCallStack, Monad m, MonadLogging m) => LogEvent -> m ()
logNotice = logWith Notice callStack

logWarning :: (HasCallStack, Monad m, MonadLogging m) => LogEvent -> m ()
logWarning = logWith Warning callStack

logError :: (HasCallStack, Monad m, MonadLogging m) => LogEvent -> m ()
logError = logWith Error callStack

hoistLogging :: (forall a. m a -> n a) -> Logging m -> Logging n
hoistLogging hst logging =
    logging{ _log = \l n t -> hst (_log logging l n t)
           , _logName = hst (_logName logging)
           }
