{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE RecordWildCards #-}

-- | Syslog backend implementation.
module Loot.Log.Syslog
       ( -- * configs
         SyslogConfig (..)
       , SysloggerConfig (..)
       , SyslogDestination (..)
       , defaultLogConfig
       , defaultLoggerConfig
       , syslogConfigFromFile
         -- * running
       , prepareSyslog
       , withSysLogging
       , stopLoggers
         -- * lenses
       , loggersTree
       , destination
       , loggerName
       , unitName
       , withPID
       , withPERROR
       , facility
       , minLevel
       , subLoggers
         -- * re-exports
       , SL.Facility (..)
       ) where

import Loot.Log.Internal

import Control.Monad.Reader (withReaderT)
import Data.Yaml (ToJSON(..), FromJSON (..), Parser, Value (..), (.!=), (.:),
    (.:?), (.=), withObject, withText, withScientific, ParseException, decodeFileEither, object)
import Fmt ((+|), (|+))
import Lens.Micro.TH (makeLenses)
import Monad.Capabilities (CapImpl (CapImpl), CapsT, HasNoCap, addCap)
import Network.Socket (Family(AF_INET), HostName, PortNumber)
import Text.Read (read)

import qualified Data.HashMap.Strict as H
import qualified Data.DList as DL
import qualified Data.Text as T

import qualified System.Log as SL
import qualified System.Log.Handler.Syslog as SL
import qualified System.Log.Logger as SL

-- | Syslog configuration
data SyslogConfig = SyslogConfig
    { _loggersTree :: [SysloggerConfig]
    } deriving Show

-- | Configuration for a single Syslog logger
data SysloggerConfig = SysloggerConfig
    { _destination :: SyslogDestination -- ^ Select the receiving Server
    , _loggerName  :: Text              -- ^ This logger name in the hierarchy
    , _unitName    :: Maybe Text        -- ^ Optional Unit name
    , _withPID     :: Bool              -- ^ Use process ID automatically
    , _withPERROR  :: Bool              -- ^ Output messages to stderr as well
    , _facility    :: SL.Facility       -- ^ Facility to communicate to the system
    , _minLevel    :: Level             -- ^ Minimum Level, everything below is ignored
    , _subLoggers  :: [SysloggerConfig] -- ^ Config list for this logger's children
    } deriving Show

-- | Syslog destinations possibilities
data SyslogDestination
    = AutoLocal    -- ^ Automatic local syslog destination
    | Local String -- ^ Path to local Unix FIFO. Not supported under Windows.
    | Remote
        Family     -- ^ Network Address family (usually AF_INET or AF_INET6)
        HostName   -- ^ Remote hostname (can also be localhost)
        PortNumber -- ^ Port number, for syslog is usually 514
    deriving Show

-- | Reasonable defaults for 'SyslogConfig'.
defaultLogConfig :: SyslogConfig
defaultLogConfig = SyslogConfig [defaultLoggerConfig]

-- | Reasonable defaults for 'SysloggerConfig'.
defaultLoggerConfig :: SysloggerConfig
defaultLoggerConfig = SysloggerConfig{..}
  where
    _destination = AutoLocal
    _loggerName  = toText SL.rootLoggerName
    _unitName    = Nothing
    _withPID     = True
    _withPERROR  = False
    _facility    = SL.USER
    _minLevel    = Warning
    _subLoggers  = []

-- | Safely load a 'SyslogConfig' from a file, returns an 'Either' with the result
syslogConfigFromFile
    :: MonadIO m
    => FilePath
    -> m (Either ParseException SyslogConfig)
syslogConfigFromFile = liftIO . decodeFileEither

-- | Create 'Logging' using Syslog.
prepareSyslog
    :: (MonadIO m, MonadIO n)
    => SyslogConfig
    -> NameSelector
    -> m (Logging n)
prepareSyslog SyslogConfig{..} nameSel = do
    liftIO $ do
        -- Remove the default handler (stderr on all Warnings) from the root logger
        SL.updateGlobalLogger SL.rootLoggerName SL.removeHandler
        -- Then use the config to create the logger hierarchy
        mapM_ (addLoggerTree (fromList [])) _loggersTree
    return $ Logging
        { _log = \lvl name text -> liftIO $
            SL.logM (show name) (toPriority lvl) (toString text)
        , _logName = pure nameSel
        }

-- | Add Syslog 'Logging' capability.
withSysLogging
    :: forall m caps a. (MonadIO m, HasNoCap Logging caps)
    => SyslogConfig
    -> CapsT (Logging ': caps) m a
    -> CapsT caps m a
withSysLogging config cont = do
    logging <- prepareSyslog config CallstackName
    let loggingImpl :: CapImpl Logging '[] m
        loggingImpl = CapImpl $ hoistLogging liftIO logging
    withReaderT (addCap loggingImpl) $ do
        logDebug "Logging started"
        let cfgText = T.pack $ show config
        logDebug $ "Logging config: "+|cfgText|+""
        cont

-- | Uses a 'SyslogConfig' to start a @hslogger@ syslogger
addLoggerTree
    :: Name
    -> SysloggerConfig
    -> IO ()
addLoggerTree prevName SysloggerConfig {..} = do
    -- Give the correct handler to the logger, also set the logger priority to debug
    -- (if this happens to be too low, the handler will receive nothing)
    -- NOTE: hslogger will create a new logger or override an existing one
    logHandler <- openLog unitString outOptions _facility priority
    SL.updateGlobalLogger loggerString $
        SL.setLevel SL.DEBUG . SL.addHandler logHandler
    mapM_ (addLoggerTree newName) _subLoggers
  where
    newName = Name . (`DL.snoc` _loggerName) $ unName prevName
    loggerString = show newName
    unitString = maybe loggerString toString _unitName
    outOptions = [SL.PID | _withPID] ++ [SL.PERROR | _withPERROR]
    priority = toPriority _minLevel
    openLog = case _destination of
        AutoLocal                  -> SL.openlog
        Local fifoPath             -> SL.openlog_local fifoPath
        Remote family host portNum -> SL.openlog_remote family host portNum

-- | Utility function to stop all active loggers
stopLoggers :: MonadIO m => m ()
stopLoggers = liftIO SL.removeAllHandlers

-- | Maps @loot-log@ severity level to @hslogger@ Priority.
toPriority :: Level -> SL.Priority
toPriority = \case
    Debug   -> SL.DEBUG
    Info    -> SL.INFO
    Notice  -> SL.NOTICE
    Warning -> SL.WARNING
    Error   -> SL.ERROR

-- SyslogConfig and SysloggerConfig Lenses
makeLenses ''SyslogConfig

makeLenses ''SysloggerConfig

-- 'FromJSON' and "ToJSON" instances for configs
instance FromJSON SyslogConfig where
    parseJSON = withObject "SyslogConfig" $ \v -> SyslogConfig
        <$> v .: "loggers-tree"

instance ToJSON SyslogConfig where
    toJSON SyslogConfig {..} = object ["loggers-tree" .= _loggersTree]

instance FromJSON SysloggerConfig where
    parseJSON = withObject "SysloggerConfig" $ \v -> SysloggerConfig
        <$> v .:? "destination" .!= _destination defaultLoggerConfig
        <*> v .:  "name"        -- it would not make sense not to require this
        <*> v .:? "unit-name"   .!= _unitName defaultLoggerConfig
        <*> v .:? "withPID"     .!= _withPID defaultLoggerConfig
        <*> v .:? "withPERROR"  .!= _withPERROR defaultLoggerConfig
        <*> v .:? "facility"    .!= _facility defaultLoggerConfig
        <*> v .:? "min-level"   .!= _minLevel defaultLoggerConfig
        <*> v .:? "sub-loggers" .!= _subLoggers defaultLoggerConfig
    -- | Special parsing for a list of 'SysloggerConfig': this allows the key for
    -- a 'SysloggerConfig' Object to be used as the "name" key of that same object
    parseJSONList = withObject "SysloggerConfigList" $
        mapM (\(name, val) -> parseKeyAsName name val) . H.toList
      where
        parseKeyAsName :: Text -> Value -> Parser SysloggerConfig
        parseKeyAsName k = withObject "SysloggerConfig" $
            parseJSON . Object . H.insert "name" (String k)

instance ToJSON SysloggerConfig where
    toJSON SysloggerConfig {..} = object
        [ "destination" .= _destination
        , "name"        .= _loggerName
        , "unit-name"   .= _unitName
        , "withPID"     .= _withPID
        , "withPERROR"  .= _withPERROR
        , "facility"    .= _facility
        , "min-level"   .= _minLevel
        , "sub-loggers" .= _subLoggers
        ]
    toJSONList = Object . H.fromList . map useNameAsKey
      where
        useNameAsKey :: SysloggerConfig -> (Text, Value)
        useNameAsKey config = case toJSON config of
            Object obj -> (_loggerName config, Object $ H.delete "name" obj)
            val        -> (_loggerName config, val) -- should be impossible

instance FromJSON SyslogDestination where
    parseJSON = withObject "SyslogDestination" $ \v -> do
        destType <- v .: "destination-type"
        case destType :: Text of
            "auto" -> return AutoLocal
            "local" -> Local
                <$> v .:? "fifo-path" .!= "/dev/log"
            "remote" -> Remote
                <$> v .:? "family"      .!= AF_INET
                <*> v .:? "hostname"    .!= "localhost"
                <*> v .:? "port-number" .!= 514
            _ -> fail "Parsing SyslogDestination value failed: unknown destination-type"

instance ToJSON SyslogDestination where
    toJSON = \case
        AutoLocal -> object
            [ "destination-type" .= ("auto" :: Text)
            ]
        Local fifoPath -> object
            [ "destination-type" .= ("local" :: Text)
            , "fifo-path"        .= fifoPath
            ]
        Remote family hostname portNum -> object
            [ "destination-type" .= ("remote" :: Text)
            , "family"           .= family
            , "hostname"         .= hostname
            , "port-number"      .= portNum
            ]


-- Orphan instances:
instance FromJSON SL.Facility where
    parseJSON = withText "Facility" $ pure . read . toString

instance ToJSON SL.Facility where
    toJSON = String . show

instance FromJSON Family where
    parseJSON = withText "Family" $ pure . read . toString

instance ToJSON Family where
    toJSON = String . show

instance FromJSON PortNumber where
    parseJSON = withScientific "PortNumber" $ pure . round

instance ToJSON PortNumber where
    toJSON = Number . fromIntegral
