-- | Logging context is additional runtime information attached to every
-- log record.
--
-- It is dynamic (as opposed to static) because the same logging call
-- in a function can produce log records with different context, since
-- the context depends on how the function itself was called.
module Loot.Log.Internal.Context
       ( LogContext (..)
       , tags

       , HasLogContext (..)
       , MonadHasLogContext (..)
       ) where

import Generics.Deriving.Monoid (mappenddefault, memptydefault)
import Lens.Micro.TH (makeLenses)


-- | Dynamic logging context.
data LogContext = LogContext
    { _tags :: [Text]
    } deriving (Eq, Generic, Show)
makeLenses ''LogContext

instance Semigroup LogContext where
    (<>) = mappenddefault

instance Monoid LogContext where
    mempty = memptydefault
    mappend = (<>)


-- | Capability of keeping track of a 'LogContext'.
--
-- 'LogContext' provides additional metadata that is attached to logging
-- events. This metadata is optional, as it needs to be stored somewhere in the
-- monad and not all monads are capable of storing something in them.
--
-- This capability essentially means that your monad can act as a
-- @'MonadReader' 'LogContext'@.
data HasLogContext m = HasLogContext
    { _askLoggingCtx   :: m LogContext
    , _localLoggingCtx :: (LogContext -> LogContext) -> (m () -> m ())
    }

-- | Class of monads keeping track of 'LogContext'.
--
-- Isomorphic to @MonadReader LogContext@.
class MonadHasLogContext m where
    askLogCtx   :: m LogContext
    localLogCtx :: (LogContext -> LogContext) -> (m a -> m a)
