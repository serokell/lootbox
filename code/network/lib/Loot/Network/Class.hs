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
* One-way heartbeating using PUB/SUB (because it's a recommended way)
* "Nasty freelancer" pattern with ids equal to hosts (tcp://something:dealerPort)
  for ROUTER (cli back) <-> ROUTER (serv front) connection. Implies that
  clients don't have ID.
  https://rfc.zeromq.org/spec:10/FLP/
* "Pub/Sub Message Envelopes". Messages are three+ frames, first one is
  key, second is address, third+ is data.
  http://zguide.zeromq.org/page:all#toc49
* "Load balancing" on the server side.
  * Message type is represented with a Bytestring and represents listener's
    capability to handle this type of message.
  * Each listener is DEALER connected to a server backend ROUTER.
  * Listener can receive, reply, publish. All communication is done through
    the server broker, so reply/publish difference is made by sending extra
    flag to the broker.
  * When broker receives a message from the frontend, it determines its message
    type and sends to the active listener that supports this message type.
  * Listeners can be in "busy" or "ready" state. Broker sets listener's state
    to "busy" when broker sends it a new task. After finishing the task,
    listener tells broker it is "ready".

  So now we don't spawn a thread per connection, instead we can do a
  nice load balancing: create more threads of this type if needed (on
  the start or in the runtime). Also we can spawn lots of these
  threads beforehand to be sure that we don't run out of them -- and
  it should be alright, since inproc is fast.


TODO <Picture here>

Some other important things that will be well-documented in future:
1. Server binds to two ports: ROUTER and PUSH
2. It runs "server broker" in a main thread, which routes requests from
   frontend (ROUTER) to listeners (DEALER). In other direction, it propagates
   replies and publications from listeners to outer world.
3. Client has ROUTER backend to talk to servers and PULL to receive updates.
4. It also has broker called "client broker" which connects a number
   of client threads (that want to talk to network) to the broker
   frontend (ROUTER). Broker gets messages from the frontend (worker requests)
   and propagates them to backend, which sends them to the network.
   Broker also gets messages from the PULL and propagates them to
   a worker that "subscribed" to this kind of update.

Server: broker (ROUTER front/ROUTER back/PUB publisher) + listener workers (DEALER)
Client broker (ROUTER front/ROUTER back/SUB subscriber) + client workers (DEALER)


MDP Protocol (which we somewhat follow).
https://rfc.zeromq.org/spec:7/MDP

Tasklist
========

Done:
  * Subscriptions
  * Client-worker by tag/type (Router on server back instead of Dealer)
  * Choose node when sending a message from client (Router, not Dealer)

Basic features:
  * TODO Conversations ...
  * TODO Load balancing on server backend. TODO Read "majordomo" pattern again.
  * TODO Heartbeating
  * TODO "Receive" timeouts (just poll before of receiveMulti)
  * TODO Exceptions! safe-exceptions/async. Replace all "error" with
    something well-thought, like MonadThrow/Catch and Maybes/Eithers.

Then test and debug.

Pro features:
  * TODO Server side monitoring of incoming connections (for stats?).
  * TODO Smart heartbeating? (different peers -- different frequency
    that we should agree on beforehand
  * TODO Discovery
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
    , Subscriptions
    , ReceiveRes(..)
    , NetworkingCli(..)
    , ListenerId
    , NetworkingServ(..)
    ) where

----------------------------------------------------------------------------
-- Classes
----------------------------------------------------------------------------

-- Disclaimer on how to run this: (maybe put it into the header?)
-- ZMQ kindly asks users not to share sockets between threads
-- and the library design allows it to be so. The only thing that
-- library user should do is _always_ initialize context and run
-- worker in the same thread.
--
-- So if you want to run client, you fork once, then create a
-- client context, then execute runClient which blocks forever.
-- You also would like to add some workers, so I would recommend
-- forking them before you do runClient. And again, you first fork,
-- then invoke registerClient and then use ClientEnv in the same thread.
--
-- Same applies to the server.

-- | We send and receive lists of strict bytestring. ZMQ implementation
-- operates in frames, so user can use this extra functionlaity. If
-- it is not needed, your protocol design can state that one or zero
-- BSs are sent.
type Content = [ByteString]

-- | List of tags client is subscribed to.
type Subscriptions = [ByteString]

-- | Either a response from some server or a subscription update (key
-- and data).
data ReceiveRes = Response Content | Update ByteString Content

class NetworkingCli t m where
    type NodeId t
    -- ^ Full-size identities -- ports/hosts. Something we can connect to.
    runClient :: m ()
    -- ^ Runs client routine. This must be run in the same thread
    -- client context was created.

    getPeers :: m (Set (NodeId t))
    -- ^ Returns the list of current peers connected.

    type ClientEnv t
    registerClient :: ByteString -> Subscriptions -> m (ClientEnv t)
    -- ^ Register client worker -- it is expected then then main is
    -- forked and child thread uses his environment. The first
    -- argument is the (unique) client identity. The second argument
    -- is list of tags client is "subscribed" for.  It's better not to
    -- mix "send and receive" and "get update and process"
    -- functionality in a single client, but still -- 'receive' return
    -- type will make it possible to distinguish.
    updatePeers :: ClientEnv t -> Set (NodeId t) -> Set (NodeId t) -> m ()
    -- ^ First arguments -- peers to connect to, second -- to disconnect.
    send :: ClientEnv t -> Maybe (NodeId t) -> Content -> m (NodeId t)
    -- ^ Send single message to a particular peer if specified, or to
    -- any, if not. Returns the node message was sent.
    broadcast :: ClientEnv t -> Content -> m ()
    -- ^ Send message to all the peers using the client socket.
    receive :: ClientEnv t -> m (NodeId t, ReceiveRes)
    -- ^ Receive a message. It is either a reply from a node we did
    -- the last send to, or a subscription update.

type ListenerId = ByteString

class NetworkingServ t m where
    type CliId t
    -- | Runs server routine. This must be run in the same thread
    -- client context was created.
    runServer :: m ()

    type ListenerEnv t
    -- | Register a new listener with a given name and message type he
    -- is subscribed to. All listener ids must be distinct. Listeners
    -- with the same message type are considered the same.
    registerListener :: ListenerId -> ByteString -> m (ListenerEnv t)
    -- | Receive. Block until listener gets any data. Second parameter
    -- stands for "waiting for another conversation round". Since
    -- listener can do several accepts and this communicate with peer
    -- for some time, this marker is needed to indicate that worker is
    -- busy. Set it to true if you've finished the previous
    -- conversation.
    --
    -- Naming is questionable, but both cli and serv have "receive"s, so
    -- wat do?
    accept :: ListenerEnv t -> Bool -> m (CliId t, Content)
    -- | Respond to a client's request.
    respond :: ListenerEnv t -> CliId t -> Content -> m ()
    -- | Publish some data -- key and content.
    publish :: ListenerEnv t -> (ByteString,Content) -> m ()