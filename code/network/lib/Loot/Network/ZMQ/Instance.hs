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

import Loot.Base.HasLens (HasGetter, getterOf)
import Loot.Log (MonadLogging)
import UnliftIO (MonadUnliftIO)

import qualified Loot.Network.Class as C
import qualified Loot.Network.ZMQ.Client as ZC
import Loot.Network.ZMQ.Common (ZTNodeId)
import qualified Loot.Network.ZMQ.Server as ZS

type ReaderIOLogM r m = (MonadReader r m, MonadUnliftIO m, MonadLogging m)

----------------------------------------------------------------------------
-- Client
----------------------------------------------------------------------------

runClientDefault :: (ReaderIOLogM r m, HasGetter r ZC.ZTNetCliEnv, MonadMask m) => m ()
runClientDefault = ZC.runBroker

getPeersDefault :: (ReaderIOLogM r m, HasGetter r ZC.ZTNetCliEnv) => m (Set ZTNodeId)
getPeersDefault = ZC.getPeers

updatePeersDefault :: (ReaderIOLogM r m, HasGetter r ZC.ZTNetCliEnv) => ZC.ZTUpdatePeersReq -> m ()
updatePeersDefault x = do
    q <- ZC.ztCliRequestQueue <$> view (getterOf @ZC.ZTNetCliEnv)
    ZC.updatePeers q x

registerClientDefault ::
       (ReaderIOLogM r m, HasGetter r ZC.ZTNetCliEnv)
    => C.ClientId
    -> Set C.MsgType
    -> Set C.Subscription
    -> m ZC.ZTClientEnv
registerClientDefault c m s = do
    q <- ZC.ztCliRequestQueue <$> view (getterOf @ZC.ZTNetCliEnv)
    ZC.registerClient q c m s

----------------------------------------------------------------------------
-- Server
----------------------------------------------------------------------------

runServerDefault :: (ReaderIOLogM r m, HasGetter r ZS.ZTNetServEnv, MonadMask m) => m ()
runServerDefault = ZS.runBroker

registerListenerDefault ::
       (ReaderIOLogM r m, HasGetter r ZS.ZTNetServEnv)
    => C.ListenerId
    -> Set C.MsgType
    -> m ZS.ZTListenerEnv
registerListenerDefault l m = do
    q <- ZS.ztServRequestQueue <$> view (getterOf @ZS.ZTNetServEnv)
    ZS.registerListener q l m
