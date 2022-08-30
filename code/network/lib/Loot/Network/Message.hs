{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}

-- | Messages and message-dependent callbacks matching.

module Loot.Network.Message
       ( Message (..)
       , getMsgTag
       , Callback (..)
       , CallbackWrapper (..)
       , handler
       , handlerDecoded
       , runCallbacks
       , runCallbacksInt
       ) where

import Codec.Serialise (DeserialiseFailure, Serialise (..), deserialiseOrFail)
import qualified Data.ByteString.Lazy as BSL
import Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.Dependent.Map as D
import Data.GADT.Compare (GCompare (..), GEq (..), GOrdering (..))
import Data.Reflection (reifyNat)
import Data.Singletons (Sing)
import Data.Tagged (Tagged (..))
import Data.Type.Equality ((:~:) (Refl))
import GHC.TypeLits.Singletons (SNat (..))
import GHC.TypeNats (sameNat)

-- todo require typeable?
-- | Message is a type that has unique message type (expressed by
-- 'Nat' type family instance). Parameter k is a kind to create
-- non-intersecting families of messages.
class (KnownNat (MsgTag k d), Serialise d) => Message k d | d -> k where
    type family MsgTag k d = (n :: Nat) | n -> k d

-- | Returns message tag.
getMsgTag :: forall k d. Message k d => Natural
getMsgTag = natVal (Proxy @(MsgTag k d))

-- | Action called on a particular type (can be either parse error or
-- type itself). Callback's main feature is that it links the message
-- type with a bytestring using 'Tagged'. Nevertheless, user might
-- wath real callback function to have other argumets, and that's what
-- "ex" parameter does. In networking, the real message is passed as a
-- tagged bytestring and 'ex' can contain sender address or anything
-- else.
data Callback ex m a n =
    forall k d. (Message k d, MsgTag k d ~ n) =>
    Callback { unCallback :: ex -> Tagged (k,d) ByteString -> m a }

-- | Packed callback.
data CallbackWrapper ex m a =
    forall n. KnownNat n => CallbackWrapper { unCallbackWrapper :: Callback ex m a n }

-- | Convenient helper to create callback wrappers (from raw bytestring input).
handler :: Message k d => (ex -> Tagged (k,d) ByteString -> m a) -> CallbackWrapper ex m a
handler = CallbackWrapper . Callback

-- | Callback wrapper from function processing parsed value.
handlerDecoded ::
       forall ex k d m a. Message k d
    => (ex -> Either DeserialiseFailure d -> m a)
    -> CallbackWrapper ex m a
handlerDecoded action =
    handler $ \ex (Tagged bs :: Tagged (k,d) ByteString) ->
                action ex (deserialiseOrFail $ BSL.fromStrict bs)

instance GEq SNat where
    geq (SNat :: SNat n) (SNat :: SNat m) =
        sameNat (Proxy :: Proxy n) (Proxy :: Proxy m)

instance GCompare SNat where
    gcompare s1@(SNat :: SNat n) s2@(SNat :: SNat m) =
        case geq s1 s2 of
          Just Refl -> GEQ
          Nothing ->
            case compare (natVal (Proxy @n)) (natVal (Proxy @m)) of
              LT -> GLT
              GT -> GGT
              EQ -> error "GCompare Sing: 'testEquality' and 'compare' are inconsistent"

-- Creates a dmap from callbacks. If more than one callback with the
-- same incoming message typewill be in the list, only first will be
-- put in.
createDMap :: forall ex m a. [CallbackWrapper ex m a] -> D.DMap Sing (Callback ex m a)
createDMap callbacks =
    D.fromList $
    map (\(CallbackWrapper c@(_ :: Callback ex m a n)) -> ((SNat :: SNat n) :=> c))
        callbacks

-- | Executes a callback given a dmap, message tag and BS. User must
-- ensure that there is a callback that can be called, otherwise (in
-- case of lookup failure) this function will return Nothing.
runCallbacks :: Monad m => [CallbackWrapper ex m a] -> SNat n -> ByteString -> ex -> m (Maybe a)
runCallbacks cbs sn bs ex =
    case D.lookup sn dmap of
      Just (Callback x) -> Just <$> x ex (Tagged bs)
      Nothing           -> pure Nothing
  where
    dmap = createDMap cbs

-- | Same as 'runCallback', but accepts value-level natural number.
runCallbacksInt :: Monad m => [CallbackWrapper ex m a] -> Natural -> ByteString -> ex -> m (Maybe a)
runCallbacksInt cbs i bs ex =
    reifyNat (toInteger i) $ \(Proxy :: Proxy n) -> runCallbacks cbs (SNat :: SNat n) bs ex
