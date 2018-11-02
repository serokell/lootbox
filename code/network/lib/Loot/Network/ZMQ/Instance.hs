{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | ZMQ TCP instance for Network.Class, constraint-based.

module Loot.Network.ZMQ.Instance
    (
      ZTNodeId
    , runClientDefault
    , getPeersDefault
    , updatePeersDefault
    , registerClientDefault

    , ZS.ZTCliId
    , runServerDefault
    , registerListenerDefault
    ) where

import Loot.Base.HasLens (HasLens (..), HasLens')
import qualified Loot.Network.Class as C
import qualified Loot.Network.ZMQ.Client as ZC
import Loot.Network.ZMQ.Common (ZTGlobalEnv, ZTNodeId)
import qualified Loot.Network.ZMQ.Server as ZS

type ReaderIOM r m = (MonadReader r m, MonadIO m)

----------------------------------------------------------------------------
-- Client
----------------------------------------------------------------------------

runClientDefault :: (ReaderIOM r m, HasLens' r ZC.ZTNetCliEnv, HasLens' r ZTGlobalEnv, MonadMask m) => m ()
runClientDefault = ZC.runBroker

getPeersDefault :: (ReaderIOM r m, HasLens' r ZC.ZTNetCliEnv) => m (Set ZTNodeId)
getPeersDefault = ZC.getPeers

updatePeersDefault :: (ReaderIOM r m, HasLens' r ZC.ZTNetCliEnv) => ZC.ZTUpdatePeersReq -> m ()
updatePeersDefault x = do
    q <- ZC.ztCliRequestQueue <$> view (lensOf @ZC.ZTNetCliEnv)
    ZC.updatePeers q x

registerClientDefault ::
       (ReaderIOM r m, HasLens' r ZC.ZTNetCliEnv)
    => C.ClientId
    -> Set C.MsgType
    -> Set C.Subscription
    -> m ZC.ZTClientEnv
registerClientDefault c m s = do
    q <- ZC.ztCliRequestQueue <$> view (lensOf @ZC.ZTNetCliEnv)
    ZC.registerClient q c m s

----------------------------------------------------------------------------
-- Server
----------------------------------------------------------------------------

runServerDefault :: (ReaderIOM r m, HasLens' r ZS.ZTNetServEnv, MonadMask m) => m ()
runServerDefault = ZS.runBroker

registerListenerDefault ::
       (ReaderIOM r m, HasLens' r ZS.ZTNetServEnv)
    => C.ListenerId
    -> Set C.MsgType
    -> m ZS.ZTListenerEnv
registerListenerDefault = ZS.registerListener
