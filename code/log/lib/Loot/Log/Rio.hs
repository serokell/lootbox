-- | Helpers for defining instances in "ReaderT over IO" approach to monad
-- stack.

module Loot.Log.Rio
    ( defaultLog
    , defaultLogName
    , defaultAskLogNameSel
    , defaultModifyLogNameSel
    ) where

import Control.Lens (views)
import Ether.Internal (HasLens (..))

import Loot.Log.Internal (Level, Logging (..), Name, NameSelector, logNameSelL)

-- | Default implementation of 'MonadLogging.log' (generated with 'makeCap').
defaultLog
    :: forall m ctx.
       (HasLens (Logging m) ctx (Logging m), MonadReader ctx m)
    => Level -> Name -> Text -> m ()
defaultLog l n t = do
    lg <- views (lensOf @(Logging m)) _log
    lg l n t

-- | Default implementation of 'MonadLogging.logName' (generated with
-- 'makeCap').
defaultLogName
    :: forall m ctx.
       (HasLens NameSelector ctx NameSelector, MonadReader ctx m)
    => m NameSelector
defaultLogName = view (lensOf @NameSelector)

-- | Default implementation of 'modifyLogNameSel' stack.
defaultModifyLogNameSel
    :: forall m ctx a.
       (HasLens (Logging m) ctx (Logging m), MonadReader ctx m)
    => (NameSelector -> NameSelector) -> m a -> m a
defaultModifyLogNameSel f =
    local (lensOf @(Logging m) . logNameSelL %~ f)

-- | Default implementation of 'askLogNameSel'.
defaultAskLogNameSel
    :: forall m ctx.
       (HasLens (Logging m) ctx (Logging m), MonadReader ctx m)
    => m NameSelector
defaultAskLogNameSel = view (lensOf @(Logging m)) >>= _logName