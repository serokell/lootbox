{- |

Reexports from ZMQ* modules, also providing the documentation.

Implementation
==============

Implementation follows ZMQ Guide (http://zguide.zeromq.org/page:all#toc111)
and, in particular, some relevant patterns:
* For direct communication: ROUTER to ROUTER. Instead of suggested
  "nasty freelancer" pattern our nodes exchange do the handshake where
  server communicates its identity to the client.
* For subscriptions we use "Pub/Sub Message Envelopes". Messages are
  three+ frames, first one is key, second is address, third+ is data.
  http://zguide.zeromq.org/page:all#toc49
* One-way (server to client) heartbeating using PUB/SUB, see the
  "heartbeating for paranoid pirate":
  http://zguide.zeromq.org/page:all#Heartbeating-for-Paranoid-Pirate

Server:
* Binds to two ports: ROUTER and PUB.
* Runs "server broker" in a main thread, which routes requests from
  frontend (ROUTER) to listeners. In other direction, it propagates
  replies and publications from listeners to the outer world.

Client:
* Has ROUTER backend to talk to servers and PULL to receive updates.
* It also has broker called "client broker" which connects a number
  of client threads (that want to talk to network) to the broker
  frontend (ROUTER). Broker gets messages from the frontend (worker requests)
  and propagates them to backend, which sends them to the network.
  Broker also gets messages from the PULL and propagates them to
  a worker that "subscribed" to this kind of update.
* Every time we need to connect to some server, we open a DEALER socket,
  ask server's ROUTER for his zmq identity, disconnect and destroy this
  DEALER and use ROUTER <-> ROUTER communication.

Tasklist
========

  * Configurable parameters (polling periods etc).
  * Discovery.
  * Application-level load balancing.
  * Message limits.
  * Server side monitoring of incoming connections (for stats?).
  * Load balancing on client backend -- choosing peer to connect based
    on its ping/average response speed/etc.
  * Smart heartbeating? (different peers -- different frequency
    that we should agree on beforehand.
-}

module Loot.Network.ZMQ
       ( module Loot.Network.ZMQ.Common
       , module Loot.Network.ZMQ.Client
       , module Loot.Network.ZMQ.Server
       ) where


import Loot.Network.ZMQ.Client (ZTNetCliEnv)
import Loot.Network.ZMQ.Client hiding (ZTNetCliEnv (..), getPeers, registerClient, runBroker,
                                updatePeers)
import Loot.Network.ZMQ.Common
import Loot.Network.ZMQ.Server (ZTNetServEnv)
import Loot.Network.ZMQ.Server hiding (registerListener, runBroker)
