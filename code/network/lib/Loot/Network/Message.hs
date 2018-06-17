{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}

-- | Messages and message-dependent callbacks matching.

module Loot.Network.Message
       ( Message (..)
       , Callback (..)
       , CallbackWrapper (..)
       , handler
       , handlerDecoded
       , runCallbacks
       , runCallbacksInt
       ) where

import Codec.Serialise (DeserialiseFailure, Serialise (..), deserialiseOrFail)
import qualified Data.ByteString.Lazy as BSL
import Data.Dependent.Map (DSum ((:=>)))
import qualified Data.Dependent.Map as D
import Data.GADT.Compare ((:~:) (Refl), GEq (..), GOrdering (..))
import Data.Reflection (reifyNat)
import Data.Singletons.TypeLits hiding (natVal)
import Data.Tagged (Tagged (..))
import Data.Type.Equality (testEquality)

-- todo require typeable?
-- | Message is a type that has unique message type (expressed by
-- 'Nat' type family instance).
class (KnownNat (MsgTag d), Serialise d) => Message d where
    type family MsgTag d = (n :: Nat) | n -> d

-- | Action called on a particular type (can be either parse error or type itself).
data Callback m a n =
    forall d. (Message d, MsgTag d ~ n) =>
    Callback { unCallback :: Tagged d ByteString -> m a }

-- | Packed callback.
data CallbackWrapper m a =
    forall n. KnownNat n => CallbackWrapper { unCallbackWrapper :: Callback m a n }

-- | Convenient helper to create callback wrappers (from raw bytestring input).
handler :: Message d => (Tagged d ByteString -> m a) -> CallbackWrapper m a
handler = CallbackWrapper . Callback

-- | Callback wrapper from function processing parsed value.
handlerDecoded ::
       forall d m a. Message d
    => (Either DeserialiseFailure d -> m a)
    -> CallbackWrapper m a
handlerDecoded action =
    handler $ \(Tagged bs :: Tagged d ByteString) ->
                action (deserialiseOrFail $ BSL.fromStrict bs)

instance GEq (Sing :: Nat -> *) where
    geq = testEquality

instance D.GCompare (Sing :: Nat -> *) where
    gcompare s1@(SNat :: SNat n) s2@(SNat :: SNat m) =
        case testEquality s1 s2 of
          Just Refl -> GEQ
          Nothing ->
            case compare (natVal (Proxy @n)) (natVal (Proxy @m)) of
              LT -> GLT
              GT -> GGT
              EQ -> error "GCompare Sing: 'testEquality' and 'compare' are inconsistent"

-- Creates a dmap from callbacks. If more than one callback with the
-- same incoming message typewill be in the list, only first will be
-- put in.
createDMap :: forall m a. [CallbackWrapper m a] -> D.DMap Sing (Callback m a)
createDMap callbacks =
    D.fromList $
    map (\(CallbackWrapper c@(_ :: Callback m a n)) -> ((SNat :: SNat n) :=> c))
        callbacks

-- | Executes a callback given a dmap, switching integer and BS. User
-- must ensure that there is a callback that can be called, otherwise
-- (in case of lookup failure) this function will "error".
runCallbacks :: [CallbackWrapper m a] -> SNat n -> ByteString -> m a
runCallbacks cbs sn bs =
    case D.lookup sn dmap of
      Just (Callback x) -> x (Tagged bs)
      Nothing           -> error "runCallback: lookup in DMap failed"
  where
    dmap = createDMap cbs

-- | Same as 'runCallback', but accepts value-level integer.
runCallbacksInt :: [CallbackWrapper m a] -> Integer -> ByteString -> m a
runCallbacksInt cbs i bs =
    reifyNat i $ \(Proxy :: Proxy n) -> runCallbacks cbs (SNat :: SNat n) bs
