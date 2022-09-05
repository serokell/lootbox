-- | Internal ZMQ-related utilities.

module Loot.Network.ZMQ.Internal
    ( ztLog
    , canReceive
    , atLeastOne
    , heartbeatSubscription
    ) where

import Control.Monad.STM (retry)
import qualified Data.List.NonEmpty as NE
import qualified System.ZMQ4 as Z

import Loot.Log.Internal (Logging (..), Message (..), Severity, selectLogName)
import Loot.Network.Class (Subscription (..))


-- | Logging function for zmq -- doesn't require any monad, uses
-- 'Logging IO' directly.
ztLog :: HasCallStack => Logging IO -> Severity -> Text -> IO ()
ztLog Logging{..} msgSeverity msgContent = do
    msgName <- selectLogName callStack <$> _logName
    _log $ Message {..}

-- | Checks if data can be received from the socket. Use @whileM
-- canReceive process@ pattern after the STM action on the needed
-- socket.
canReceive :: Z.Socket t -> IO Bool
canReceive sock = elem Z.In <$> Z.events sock

-- | Given a set of STM actions, returns all that succeed if at least
-- one does.
atLeastOne :: NonEmpty (STM (Maybe a)) -> STM (NonEmpty a)
atLeastOne l = fmap catMaybes (sequence (NE.toList l)) >>= \case
    [] -> retry
    x:xs -> pure $ x :| xs

-- | Key for the heartbeat subscription.
heartbeatSubscription :: Subscription
heartbeatSubscription = Subscription "_hb"
