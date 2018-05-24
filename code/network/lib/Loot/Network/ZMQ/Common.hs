{-# LANGUAGE RecordWildCards #-}

-- | Common types and functions.

module Loot.Network.ZMQ.Common
    ( ZmqTcp
    , ZTGlobalEnv(..)
    , ztContext
    , withZTGlobalEnv
    , endpointTcp
    , ZTNodeId(..)
    , ztNodeIdRouter
    , ztNodeIdPub
    , ztNodeConnectionId
    ) where


import Codec.Serialise (Serialise)
import Control.Lens (makeLenses)
import qualified Data.ByteString.Char8 as BS8

import qualified Data.Restricted as Z
import qualified System.ZMQ4 as Z



-- | Networking tag type for ZMQ over TCP.
data ZmqTcp

data ZTGlobalEnv = ZTGlobalEnv
    { _ztContext :: Z.Context
    }

makeLenses ''ZTGlobalEnv

withZTGlobalEnv :: (MonadMask m, MonadIO m) => (ZTGlobalEnv -> m a) -> m a
withZTGlobalEnv action =
    bracket (liftIO Z.context) (liftIO . Z.term) $ action . ZTGlobalEnv

-- | Generic tcp address creation helper.
endpointTcp :: String -> Integer -> String
endpointTcp h p = "tcp://" <> h <> ":" <> show p

data ZTNodeId = ZTNodeId
    { ztIdHost       :: String
    , ztIdRouterPort :: Integer
    , ztIdPubPort    :: Integer
    } deriving (Eq, Ord, Show, Generic)

instance Serialise ZTNodeId

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
ztNodeConnectionId ZTNodeId{..} =
    -- Yes, we use host:frontendPort, it doesn't seem to have
    -- any downsides.
    let sid = endpointTcp ztIdHost ztIdRouterPort
    in Z.rvalue $
       fromMaybe (error $ "ztNodeConnectionId: restriction check failed " <> fromString sid) $
       (Z.toRestricted (BS8.pack sid) :: Maybe (Z.Restricted (Z.N1, Z.N254) ByteString))
