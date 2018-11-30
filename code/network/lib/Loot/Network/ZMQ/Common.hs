{-# LANGUAGE RecordWildCards #-}

-- | Common ZMQ TCP types and functions.

module Loot.Network.ZMQ.Common
    (
      -- | Misc
      ZmqTcp
    , ztLog
    , endpointTcp
    , canReceive
    , atLeastOne

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

      -- | Internal messages
    , heartbeatSubscription
    ) where

import Prelude hiding (log)

import Codec.Serialise (Serialise)
import Control.Monad.STM (retry)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import GHC.Stack (HasCallStack, callStack)
import qualified System.ZMQ4 as Z

import Loot.Log.Internal (Logging (..), Message (..), Severity, selectLogName)
import Loot.Network.Class (Subscription (..))

----------------------------------------------------------------------------
-- Common functions
----------------------------------------------------------------------------

-- | Networking tag type for ZMQ over TCP.
data ZmqTcp

-- | Logging function for zmq -- doesn't require any monad, uses
-- 'Logging IO' directly.
ztLog :: HasCallStack => Logging IO -> Severity -> Text -> IO ()
ztLog Logging{..} msgSeverity msgContent = do
    msgName <- selectLogName callStack <$> _logName
    _log $ Message {..}

-- | Generic tcp address creation helper.
endpointTcp :: String -> Integer -> String
endpointTcp h p = "tcp://" <> h <> ":" <> show p

-- | Checks if data can be received from the socket. Use @whileM
-- canReceive process@ pattern after the STM action on the needed
-- socket.
canReceive :: Z.Socket t -> IO Bool
canReceive sock = elem Z.In <$> Z.events sock

-- | Given a set of STM actions, returns all that succeed if at least
-- one does.
atLeastOne :: NonEmpty (STM (Maybe a)) -> STM (NonEmpty a)
atLeastOne l = fmap catMaybes (sequence (NE.toList l)) >>= \case
    [] -> retry
    x:xs -> pure $ x :| xs


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

-- | Key for the heartbeat subscription.
heartbeatSubscription :: Subscription
heartbeatSubscription = Subscription "_hb"
