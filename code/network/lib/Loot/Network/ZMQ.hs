{- |

Reexports from ZMQ* modules, also providing the documentation.

Implementation
==============

Implementation follows ZMQ Guide (http://zguide.zeromq.org/page:all#toc111)
and, in particular, some relevant patterns:
* For direct communication: DEALER / ROUTER.
* For subscriptions: PUB / SUB.
* For subscriptions we also use "Pub/Sub Message Envelopes". Messages
  are two+ frames, first one is the key, second+ is data.
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
* Has n DEALER backends to talk to servers and n SUB sockets to
  receive updates. We store a map "ztNodeId -> peerResources", where
  resources are these two sockets.
* It also has broker called "client broker" which connects a number
  of client threads (that want to talk to the network) to the broker
  backends (DEALERs). Broker gets messages from the frontend (worker
  requests through STM) and propagates them to backends (DEALERs),
  which sends them to the network. Broker also gets messages from
  the backends (DEALERs/SUBs) and propagates them to workers that
  "subscribed" to this kind of update (SUB case) or can receive the
  message of msgt (DEALER case).

Tasklist
========

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
import Loot.Network.ZMQ.Server hiding (registerListener, runBroker)
