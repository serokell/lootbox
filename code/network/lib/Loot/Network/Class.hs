{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}


-- This is a module containing almost all the documentation on the
-- networking for now.

{- |

Networking prototype. This module aims to provide a networking API and
its ZMQ instantiation.

Requirements
============

Two basic interfaces -- client and server. Two usecases: full nodes
and client nodes. Two APIs: client nodes use client API, full nodes
use both. Server-only node doesn't make a lot of sense, since full
node should talk to other full nodes (get updates etc), but it is
possible to launch it.

Client API should:
* Receive updates from server nodes (subscriptions, (k,v) style).
* Receive heartbeats from servers.
* Asynchronously talk with server nodes (any node or a particular one).
* Maintain list of peers (disconnect/connect) so we can later extend clients
  with a discovery mechanism.
* Talk with server nodes in separate "client" (worker) threads.
Not required:
* Sending heartbeats to server (heartbeating server -> client is sufficient,
  all full nodes are servers).

Server API should:
* Respond to users' requests asynchronously.
* Responses should be done in separate "listener" threads, each typically
  blocking until some request comes and then replying in a free manner until
  it decides to block again.
* Publish any (k,v) data, such as transactions, headers or blocks.
* Send heartbeats to clients.
Not required:
* Maintaining list of clients and replying first (without request), that's
  what "publishing" is for.

-}

module Loot.Network.Class
    ( Content
    , Subscription (..)
    , MsgType (..)
    , BiTQueue (..)

    , CliRecvMsg(..)
    , ClientId
    , ClientEnv
    , UpdatePeersReq (..)
    , uprAdd
    , uprDel
    , NetworkingCli (..)

    , ListenerId
    , ListenerEnv
    , ServSendMsg (..)
    , NetworkingServ (..)
    ) where

import Control.Lens (makeLenses)
import Data.Default (Default (..))

import Loot.Network.BiTQueue (BiTQueue (..))

----------------------------------------------------------------------------
-- Common
----------------------------------------------------------------------------

-- | We send and receive lists of strict bytestring. ZMQ
-- implementation operates in frames, so user can use this extra
-- functionlaity (for example, attach message numbers in the fisrt
-- frame and data in the second one). If it is not needed, your
-- protocol design can state that one or zero BSs are sent.
type Content = [ByteString]

-- | Bytestring describing the type of the subscription. The key.
newtype Subscription = Subscription { unSubscription :: ByteString } deriving (Eq,Ord,Show,IsString)

-- | Message type is characterized as a bytestring.
newtype MsgType = MsgType { unMsgType :: ByteString } deriving (Eq,Ord,Show,IsString)

----------------------------------------------------------------------------
-- Client
----------------------------------------------------------------------------

-- | Request to update peers
data UpdatePeersReq nId = UpdatePeersReq
    { _uprAdd :: Set nId -- ^ Peers to add/connect.
    , _uprDel :: Set nId -- ^ Peers to delete/disconnect.
    } deriving (Show, Generic)

makeLenses ''UpdatePeersReq

instance (Ord s) => Default (UpdatePeersReq s) where
    def = UpdatePeersReq mempty mempty

-- | Either a response from some server or a subscription update (key
-- and data).
data CliRecvMsg
    = Response MsgType Content
    | Update Subscription Content
    deriving (Eq,Ord,Show)

-- | Client worker environment. It receives 'CliRecvMsg' from a
-- particular node. We send pair of content with its message type to
-- some particular node, or just to some arbitrary one.
type ClientEnv t = BiTQueue (NodeId t, CliRecvMsg) (Maybe (NodeId t), (MsgType, Content))

-- | Client worker identifier.
type ClientId = ByteString

-- | Client-side networking interface.
class (Monad m, Ord (NodeId t)) => NetworkingCli t m where
    -- | Full-size identities -- ports/hosts. Something we can connect to.
    type NodeId t

    -- | Runs client routine. This must be run in the same thread
    -- client context was created.
    runClient :: m ()

    -- | Returns the list of current peers connected.
    getPeers :: m (Set (NodeId t))

    -- | First arguments -- peers to connect to, second -- to disconnect.
    updatePeers :: UpdatePeersReq (NodeId t) -> m ()

    -- | Register client worker. Client identifier, msgtype set should
    -- be unique for a client (to implement proper
    -- routing). Subscription sets of clients can intersect (several
    -- clients can subscribe to the same key)..
    registerClient :: ClientId -> Set MsgType -> Set Subscription -> m (ClientEnv t)

----------------------------------------------------------------------------
-- Server
----------------------------------------------------------------------------

-- | Things server sends -- either replies (to the requested node) or
-- publishing content.
data ServSendMsg cliId
    = Reply cliId MsgType Content
    | Publish Subscription Content
    deriving (Eq,Ord,Show)

-- | Listener environment. It sends 'ServSendMsg', receives content
-- with its message type from some client id.
type ListenerEnv t = BiTQueue (CliId t, MsgType, Content) (ServSendMsg (CliId t))

-- | Listener identifier.
type ListenerId = ByteString

-- | Server-side networking interface.
class (Monad m, Ord (CliId t)) => NetworkingServ t m where
    -- | Client identifier as seen from the server part. Clients do
    -- not have publicly recognized ids unlike servers, so it may be
    -- any bytestring.
    type CliId t

    -- | Runs server routine. This must be run in the same thread
    -- server context was created.
    runServer :: m ()

    -- | Register a new listener with a given name and message type he
    -- is subscribed to. All listener ids must be distinct, as well as
    -- message type sets.
    registerListener :: ListenerId -> Set MsgType -> m (ListenerEnv t)
