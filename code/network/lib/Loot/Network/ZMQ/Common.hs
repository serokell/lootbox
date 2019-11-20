{-# LANGUAGE RecordWildCards #-}

-- | Common ZMQ TCP types and functions.

module Loot.Network.ZMQ.Common
    (
      -- | Common functions
      ZmqTcp
    , endpointTcp

      -- | Node identities
    , ZTNodeId(..)
    , parseZTNodeId
    , ztNodeIdRouter
    , ztNodeIdPub

      -- | Global environment
    , ZTGlobalEnv(..)
    , ztGlobalEnv
    , ztGlobalEnvRelease
    , withZTGlobalEnv
    ) where

import Codec.Serialise (Serialise)
import qualified Data.List as L
import qualified System.ZMQ4 as Z

import Loot.Log.Internal (Logging (..), Severity (..))
import Loot.Network.ZMQ.Internal (ztLog)

----------------------------------------------------------------------------
-- Common functions
----------------------------------------------------------------------------

-- | Networking tag type for ZMQ over TCP.
data ZmqTcp

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
ztGlobalEnvRelease ZTGlobalEnv{..} = liftIO $ do
    ztLog ztLogging Debug "Terminating global env"
    Z.term ztContext
    ztLog ztLogging Debug "Terminating global env done"

-- | Bracket for 'ZTGlobalEnv'
withZTGlobalEnv ::
       (MonadMask m, MonadIO m)
    => Logging IO
    -> (ZTGlobalEnv -> m a)
    -> m a
withZTGlobalEnv logFunc action =
    bracket (ztGlobalEnv logFunc) ztGlobalEnvRelease action
