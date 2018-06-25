{-# LANGUAGE RecordWildCards #-}

-- | Common ZMQ TCP types and functions.

module Loot.Network.ZMQ.Common
    ( ZmqTcp

    , ZTGlobalEnv(..)
    , ztContext
    , ztLogging
    , ztLog

    , ztGlobalEnv
    , ztGlobalEnvRelease
    , withZTGlobalEnv

    , endpointTcp
    , ZTNodeId(..)
    , parseZTNodeId

    , ztNodeIdRouter
    , ztNodeIdPub
    , ztNodeConnectionId
    , ztNodeConnectionIdUnsafe

    , heartbeatSubscription
    ) where

import Prelude hiding (log)

import Codec.Serialise (Serialise)
import Control.Lens (makeLenses)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.List as L
import qualified Data.Restricted as Z
import GHC.Stack (HasCallStack, callStack)
import qualified System.ZMQ4 as Z

import Loot.Log.Internal (Level, Logging (..), selectLogName)
import Loot.Network.Class (Subscription (..))


-- | Networking tag type for ZMQ over TCP.
data ZmqTcp

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

-- | Logging function for zmq -- doesn't require any monad, uses
-- 'Logging IO' directly.
ztLog :: HasCallStack => Logging IO -> Level -> Text -> IO ()
ztLog Logging{..} level t = do
    name <- selectLogName callStack <$> _logName
    _log level name t

-- | Generic tcp address creation helper.
endpointTcp :: String -> Integer -> String
endpointTcp h p = "tcp://" <> h <> ":" <> show p

-- | NodeId as seen in ZMQ TCP.
data ZTNodeId = ZTNodeId
    { ztIdHost       :: !String  -- ^ Host.
    , ztIdRouterPort :: !Integer -- ^ Port for ROUTER socket.
    , ztIdPubPort    :: !Integer -- ^ Port for PUB socket.
    } deriving (Eq, Ord, Show, Generic)

instance Serialise ZTNodeId

-- | Parser of 'ZTNodeId' in form of "host:port1:port2".
parseZTNodeId :: String -> Either String ZTNodeId
parseZTNodeId s = case splitBy ':' s of
    [ztIdHost,p1,p2] ->
        case (readMaybe p1, readMaybe p2) of
            (Just ztIdRouterPort, Just ztIdPubPort) -> Right $ ZTNodeId {..}
            _                                       -> Left "Can't parse either of the ports"
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

-- TODO Make unsafe version of this function maybe.
-- | Agreed standard of server identities public nodes must set on
-- their ROUTER frontend. Identifiers are size limited -- this
-- function errors if it's not possible to create a valid id from the
-- given ZTNodeId. You can wrap it again using 'Z.restrict'.
ztNodeConnectionId :: ZTNodeId -> ByteString
ztNodeConnectionId zId = -- ZTNodeId{..} =
    let sid = ztNodeConnectionIdUnsafe zId
    in Z.rvalue $
       fromMaybe (error $ "ztNodeConnectionId: restriction check failed " <> show sid) $
       (Z.toRestricted sid :: Maybe (Z.Restricted (Z.N1, Z.N254) ByteString))

-- | Unsafe variant of 'ztNodeConnectionId' which doesn't check
-- whether string is empty or too long.
ztNodeConnectionIdUnsafe :: ZTNodeId -> ByteString
ztNodeConnectionIdUnsafe ZTNodeId{..} =
    -- Yes, we use host:frontendPort, it doesn't seem to have
    -- any downsides.
    BS8.pack $ endpointTcp ztIdHost ztIdRouterPort

-- | Key for heartbeat subscription.
heartbeatSubscription :: Subscription
heartbeatSubscription = Subscription "_hb"
