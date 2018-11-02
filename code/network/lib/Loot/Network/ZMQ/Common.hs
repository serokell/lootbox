{-# LANGUAGE RecordWildCards #-}

-- | Common ZMQ TCP types and functions.

module Loot.Network.ZMQ.Common
    (
      -- | Misc
      ZmqTcp
    , ztLog
    , endpointTcp

      -- | Node identities
    , ZTNodeId(..)
    , parseZTNodeId
    , ztNodeIdRouter
    , ztNodeIdPub
    , ZTInternalId (..)
    , randomZTInternalId

      -- | Global environment
    , ZTGlobalEnv(..)
    , ztGlobalEnv
    , ztGlobalEnvRelease
    , withZTGlobalEnv

      -- | Internal messages
    , heartbeatSubscription
    , tag_getId
    , tag_normal
    ) where

import Prelude hiding (log)

import Codec.Serialise (Serialise)
import qualified Data.ByteString as BS
import qualified Data.List as L
import GHC.Stack (HasCallStack, callStack)
import System.Random (randomIO, randomRIO)
import qualified System.ZMQ4 as Z

import Loot.Log.Internal (Level, Logging (..), selectLogName)
import Loot.Network.Class (Subscription (..))

----------------------------------------------------------------------------
-- Common functions
----------------------------------------------------------------------------

-- | Networking tag type for ZMQ over TCP.
data ZmqTcp

-- | Logging function for zmq -- doesn't require any monad, uses
-- 'Logging IO' directly.
ztLog :: HasCallStack => Logging IO -> Level -> Text -> IO ()
ztLog Logging{..} level t = do
    name <- selectLogName callStack <$> _logName
    _log level name t

-- | Generic tcp address creation helper.
endpointTcp :: String -> Integer -> String
endpointTcp h p = "tcp://" <> h <> ":" <> show p

----------------------------------------------------------------------------
-- Node identifiers
----------------------------------------------------------------------------

-- | NodeId as seen in ZMQ TCP.
data ZTNodeId = ZTNodeId
    { ztIdHost       :: !String  -- ^ Host, can be domain name or ip address.
                                 -- This is what user specifies and it's used for
                                 -- logging only. Internal code uses internal id.
    , ztIdRouterPort :: !Integer -- ^ Port for ROUTER socket.
    , ztIdPubPort    :: !Integer -- ^ Port for PUB socket.
    } deriving (Eq, Ord, Show, Generic)

instance Serialise ZTNodeId
instance Hashable ZTNodeId

-- | Parser of 'ZTNodeId' in form of "host:port1:port2".
parseZTNodeId :: String -> Either String ZTNodeId
parseZTNodeId s = case splitBy ':' s of
    [ztHost,p1,p2] ->
        case (readMaybe p1, readMaybe p2) of
            (Just port1, Just port2) -> Right $ ZTNodeId ztHost port1 port2
            _                        -> Left "Can't parse either of the ports"
    _            -> Left "String should have expactly two columns"
  where
    splitBy :: Eq a => a -> [a] -> [[a]]
    splitBy _ [] = []
    splitBy d s' = x : splitBy d (drop 1 y) where (x,y) = L.span (/= d) s'

-- | Address of the server's ROUTER/frontend socket.
ztNodeIdRouter :: ZTNodeId -> String
ztNodeIdRouter ZTNodeId{..} = endpointTcp ztIdHost ztIdRouterPort

-- | Address of the server's PUB socket.
ztNodeIdPub :: ZTNodeId -> String
ztNodeIdPub ZTNodeId{..} = endpointTcp ztIdHost ztIdPubPort

-- | The internal zmq identifier we use to address nodes in ROUTER.
newtype ZTInternalId = ZTInternalId
    { unZTInternalId :: ByteString
    } deriving (Eq, Ord, Show, Generic)

instance Hashable ZTInternalId

-- | Generates a random zeromq identity. It's the same as in ZMQ by
-- default -- 5 bytes long, first bit is not zero.
randomZTInternalId :: IO ZTInternalId
randomZTInternalId = do
    -- First bit may not be 0 according to ZMQ
    h <- randomRIO (32,63)
    t <- replicateM 4 randomIO
    pure $ ZTInternalId $ BS.pack $ h:t

----------------------------------------------------------------------------
-- Zeromq global environment
----------------------------------------------------------------------------

-- | Global environment needed for client/server initialisation.
data ZTGlobalEnv = ZTGlobalEnv
    { ztContext :: Z.Context
    , ztLogging :: Logging IO
    }

-- | Acquire 'ZTGlobalEnv'.
ztGlobalEnv :: MonadIO m => Logging IO -> m ZTGlobalEnv
ztGlobalEnv ztLogging = do
    ztContext <- liftIO Z.context
    pure $ ZTGlobalEnv{..}

-- | Release 'ZTGlobalEnv'.
ztGlobalEnvRelease :: MonadIO m => ZTGlobalEnv -> m ()
ztGlobalEnvRelease = liftIO . Z.term . ztContext

-- | Bracket for 'ZTGlobalEnv'
withZTGlobalEnv ::
       (MonadMask m, MonadIO m)
    => Logging IO
    -> (ZTGlobalEnv -> m a)
    -> m a
withZTGlobalEnv logFunc action =
    bracket (ztGlobalEnv logFunc) ztGlobalEnvRelease action

----------------------------------------------------------------------------
-- Internal messages and commands
----------------------------------------------------------------------------

-- | Key for heartbeat subscription.
heartbeatSubscription :: Subscription
heartbeatSubscription = Subscription "_hb"

-- | This indicates that the request on ROUTER is just a request for
-- the node id.
tag_getId :: ByteString
tag_getId = "getId"

-- | Any other (normal) request except for getId.
tag_normal :: ByteString
tag_normal = "n"
