{- |

Reexports from ZMQ* modules, also documentation.

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
  * Client-side message routing (msgType-based).
  * Heartbeating

Basic features:
  * TODO Exceptions! safe-exceptions/async. Replace all "error" with
    something well-thought, like MonadThrow/Catch and Maybes/Eithers.
  * Also logging?

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

module Loot.Network.ZMQ
       ( module Loot.Network.ZMQ.Common
       , module Loot.Network.ZMQ.Client
       , module Loot.Network.ZMQ.Server
       ) where


import Loot.Network.ZMQ.Client hiding (getPeers, registerClient, runBroker, updatePeers)
import Loot.Network.ZMQ.Common
import Loot.Network.ZMQ.Server hiding (registerListener, runBroker)
