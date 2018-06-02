{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | ZMQ TCP instance for Network.Class, constraint-based.

module Loot.Network.ZMQ.Instance () where

import qualified Loot.Network.Class as C
import Loot.Network.Utils (HasLens')
import qualified Loot.Network.ZMQ.Client as ZC
import Loot.Network.ZMQ.Common (ZTGlobalEnv, ZTNodeId, ZmqTcp)

instance ( MonadReader r m
         , HasLens' r ZTGlobalEnv
         , HasLens' r ZC.ZTNetCliEnv
         , MonadIO m
         , MonadMask m
         ) =>
         C.NetworkingCli ZmqTcp m where
    type NodeId ZmqTcp = ZTNodeId
    runClient = ZC.runBroker
    getPeers = ZC.getPeers
    updatePeers = ZC.updatePeers
    registerClient = ZC.registerClient
