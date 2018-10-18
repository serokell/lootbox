{-# LANGUAGE RecordWildCards #-}

-- | Common ZMQ TCP types and functions.

module Loot.Network.ZMQ.Common
    ( ZmqTcp
    , ztLog
    , heartbeatSubscription
    , endpointTcp

    , ZTNodeId(..)
    , parseZTNodeId
    , ZTInternalId (..)

    , PeersInfoVar
    , pivConnected
    , pivConnecting
    , peersToSend
    , peersRevMap

    , ZTGlobalEnv(..)
    , ztContext
    , ztLogging
    , ztGlobalEnv
    , ztGlobalEnvRelease
    , withZTGlobalEnv
    ) where

import Prelude hiding (log)

import Codec.Serialise (Serialise)
import Control.Lens (makeLenses)
import qualified Data.HashMap.Strict as HMap
import qualified Data.HashSet as HSet
import qualified Data.List as L
import GHC.Stack (HasCallStack, callStack)
import qualified System.ZMQ4 as Z

import Loot.Log.Internal (Level, Logging (..), selectLogName)
import Loot.Network.Class (Subscription (..))
import Loot.Network.ZMQ.Adapter (SocketAdapter)


----------------------------------------------------------------------------
-- Common functions
----------------------------------------------------------------------------

-- | Networking tag type for ZMQ over TCP.
data ZmqTcp

-- | Key for heartbeat subscription.
heartbeatSubscription :: Subscription
heartbeatSubscription = Subscription "_hb"

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

-- | The internal zmq identifier we use to address nodes in ROUTER.
newtype ZTInternalId = ZTInternalId
    { unZTInternalId :: ByteString
    } deriving (Eq, Ord, Show, Generic)

instance Hashable ZTInternalId

----------------------------------------------------------------------------
-- Manipulating peers
----------------------------------------------------------------------------

-- | Peers info variable, shared between client and server. It
-- represents a single united map, so an implicit predicate is that
-- the same node id can't be the key in more than one field of the
-- record.
--
-- Peer can be either in the connected state which is what you
-- expect usually, or in a transitive "connecting" state. The latter
-- one has the tag when the connection was tried to be established so
-- we can abort if the connection takes too long.
--
-- Also, as 'ZTInternalId's are used to index peers when sending info
-- to them, they must be all distinct as well.
data PeersInfoVar = PeersInfoVar
    { _pivConnected  :: TVar (HashMap ZTNodeId ZTInternalId)
    -- ^ Peers we're connected to.
    , _pivConnecting :: TVar (HashMap ZTNodeId (Integer, Z.Socket Z.Dealer, SocketAdapter))
    -- ^ Argument is POSIX representation of connection attempt time.
    }

makeLenses ''PeersInfoVar

-- | Returns the hashset of peers we send data to.
peersToSend :: PeersInfoVar -> STM (HashSet ZTInternalId)
peersToSend piv = do
    plist <- HMap.elems <$> readTVar (piv ^. pivConnected)
    let p = HSet.fromList plist
    when (HSet.size p /= length plist) $
        error $ "peersToSend: values collision: " <> show plist
    pure p

-- TODO it's defined here b/c for performance reasons it'd be better
-- to have it in peersInfoVar too and not build from scratch every
-- time.
-- | Build a reverse peers map (for peer resolving).
peersRevMap :: PeersInfoVar -> STM (HashMap ZTInternalId ZTNodeId)
peersRevMap piv = do
    l <- map swap . HMap.toList <$> readTVar (piv ^. pivConnected)
    let res = HMap.fromList l

    when (HMap.size res /= length l) $
        error $ "peersRevMap: values collision" <> show l
    pure res

----------------------------------------------------------------------------
-- Zeromq global context
----------------------------------------------------------------------------

-- | Global environment needed for client/server initialisation.
data ZTGlobalEnv = ZTGlobalEnv
    { _ztContext :: Z.Context
    , _ztLogging :: Logging IO
    }

makeLenses ''ZTGlobalEnv


-- | Acquire 'ZTGlobalEnv'.
ztGlobalEnv :: MonadIO m => Logging IO -> m ZTGlobalEnv
ztGlobalEnv _ztLogging = do
    _ztContext <- liftIO Z.context
    pure $ ZTGlobalEnv{..}

-- | Release 'ZTGlobalEnv'.
ztGlobalEnvRelease :: MonadIO m => ZTGlobalEnv -> m ()
ztGlobalEnvRelease = liftIO . Z.term . _ztContext

-- | Bracket for 'ZTGlobalEnv'
withZTGlobalEnv ::
       (MonadMask m, MonadIO m)
    => Logging IO
    -> (ZTGlobalEnv -> m a)
    -> m a
withZTGlobalEnv logFunc action =
    bracket (ztGlobalEnv logFunc) ztGlobalEnvRelease action
