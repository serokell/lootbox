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


Implementation
==============

Implementation follows ZMQ Guide (http://zguide.zeromq.org/page:all#toc111)
and, in particular, some relevant patterns:
* "Nasty freelancer" pattern with ids equal to hosts (tcp://something:dealerPort)
  for ROUTER (cli back) <-> ROUTER (serv front) connection. Implies that
  clients don't have ID.
  https://rfc.zeromq.org/spec:10/FLP/
* "Pub/Sub Message Envelopes". Messages are three+ frames, first one is
  key, second is address, third+ is data.
  http://zguide.zeromq.org/page:all#toc49
* One-way heartbeating using PUB/SUB (because it's a recommended way)

TODO <Picture here>

Some other important things that will be well-documented in future:
1. Server binds to two ports: ROUTER and PUSH
2. It runs "server broker" in a main thread, which routes requests from
   frontend (ROUTER) to listeners. In other direction, it propagates
   replies and publications from listeners to the outer world.
3. Client has ROUTER backend to talk to servers and PULL to receive updates.
4. It also has broker called "client broker" which connects a number
   of client threads (that want to talk to network) to the broker
   frontend (ROUTER). Broker gets messages from the frontend (worker requests)
   and propagates them to backend, which sends them to the network.
   Broker also gets messages from the PULL and propagates them to
   a worker that "subscribed" to this kind of update.

Server: broker (ROUTER front/ROUTER back/PUB publisher) + listener workers (BiTQueue)
Client broker (ROUTER front/ROUTER back/SUB subscriber) + client workers (BiTQueue)

Tasklist
========

Done:
  * Subscriptions
  * Client-worker by tag/type (Router on server back instead of Dealer)
  * Choose node when sending a message from client (Router, not Dealer)
  * TODO Client-side message routing (msgType-based).

Basic features:
  * TODO Heartbeating
  * TODO Exceptions! safe-exceptions/async. Replace all "error" with
    something well-thought, like MonadThrow/Catch and Maybes/Eithers.

Then test all features and debug.

Pro features:
  * TODO Server side monitoring of incoming connections (for stats?).
  * TODO Smart heartbeating? (different peers -- different frequency
    that we should agree on beforehand
  * TODO Discovery
  * TODO Load balancing on server backend. TODO Read "majordomo" pattern again.
  * TODO Pub/Sub envelopes with publisher's address.
  * TODO Smart broadcasting (client -> [set of peers] instead of PUB)
  * TODO Limits
  * TODO Listeners supporting multiple message types. This is not hard
    todo, but is it really needed?
  * TODO Heartbeating between broker and workers (ZMQ's PPP), to handle
    workers/broker failures.
  * TODO Load balancing on client backend -- choosing peer to connect based
    on its ping/average response speed/etc.

-}

module Loot.Network.Class
    ( Content
    , Subscription
    , MsgType
    , CliRecvMsg(..)
    , ClientId
    , BiTQueue(..)
    , ClientEnv
    , NetworkingCli(..)
    , ListenerId
    , ListenerEnv
    , ServSendMsg(..)
    , NetworkingServ(..)
    ) where

import Control.Concurrent.STM.TQueue (TQueue)

----------------------------------------------------------------------------
-- Classes
----------------------------------------------------------------------------

-- | We send and receive lists of strict bytestring. ZMQ
-- implementation operates in frames, so user can use this extra
-- functionlaity (for example, attach message numbers in the fisrt
-- frame and data in the second one). If it is not needed, your
-- protocol design can state that one or zero BSs are sent.
type Content = [ByteString]

-- | Bytestring describing the type of the subscription. The key.
type Subscription = ByteString

-- | Message type is characterized as a bytestring.
type MsgType = ByteString

data BiTQueue r s = BiTQueue
    { bReceiveQ :: TQueue r
      -- ^ Queue to receive messages.
    , bSendQ    :: TQueue s
      -- ^ Queue to send messages.
    }

----------------------------------------------------------------------------
-- Client
----------------------------------------------------------------------------

-- | Either a response from some server or a subscription update (key
-- and data).
data CliRecvMsg = Response MsgType Content | Update Subscription Content deriving (Eq,Ord,Show)

-- | Client worker environment.
type ClientEnv t = BiTQueue (NodeId t, CliRecvMsg) (Maybe (NodeId t), (MsgType, Content))

-- | Client worker identifier.
type ClientId = ByteString

class NetworkingCli t m where
    -- | Full-size identities -- ports/hosts. Something we can connect to.
    type NodeId t

    -- | Runs client routine. This must be run in the same thread
    -- client context was created.
    runClient :: m ()

    -- | Returns the list of current peers connected.
    getPeers :: m (Set (NodeId t))

    -- | First arguments -- peers to connect to, second -- to disconnect.
    updatePeers :: Set (NodeId t) -> Set (NodeId t) -> m ()

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
data ServSendMsg cliId = Reply cliId MsgType Content | Publish Subscription Content deriving (Eq,Ord,Show)

-- | Listener environment.
type ListenerEnv t = BiTQueue (CliId t, MsgType, Content) (ServSendMsg (CliId t))

-- | Listener identifier.
type ListenerId = ByteString

class NetworkingServ t m where
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
