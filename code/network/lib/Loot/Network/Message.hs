{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeFamilyDependencies    #-}

-- | Networking utilities. Playground for now.

module Loot.Network.Message
       ( Message (..)
       , Callback (..)
       , CallbackWrapper (..)
       , handler
       , createDMap
       , runCallback
       , runCallbackInt
       ) where

import Data.Dependent.Map (DSum ((:=>)))
import qualified Data.Dependent.Map as D
import Data.GADT.Compare ((:~:) (Refl), GEq (..), GOrdering (..))
import Data.Reflection (reifyNat)
import Data.Singletons.TypeLits hiding (natVal)
import Unsafe.Coerce (unsafeCoerce)

class (KnownNat (MsgTag d), Binary d) => Message d where
    type family MsgTag d = (n :: Nat) | n -> d

-- | Action called on a particular type.
data Callback m a n =
    forall d. (Message d, MsgTag d ~ n) =>
    Callback { unCallback :: d -> m a }

-- | Packed callback.
data CallbackWrapper m a =
    forall n. KnownNat n => CallbackWrapper { unCallbackWrapper :: Callback m a n }

-- | Convenient helper to create callback wrappers.
handler :: Message d => (d -> m a) -> CallbackWrapper m a
handler = CallbackWrapper . Callback

-- Is there a better way to do this w/o using unsafeCoerce?
instance GEq (Sing :: Nat -> *) where
    geq (SNat :: SNat n) (SNat :: SNat m) =
        if natVal (Proxy @n) /= natVal (Proxy @m)
        then Nothing
        else Just (unsafeCoerce Refl)

instance D.GCompare (Sing :: Nat -> *) where
    gcompare (SNat :: SNat n) (SNat :: SNat m) =
        case compare (natVal (Proxy @n)) (natVal (Proxy @m)) of
          LT -> GLT
          EQ -> unsafeCoerce GEQ
          GT -> GGT

-- | Creates a dmap from callbacks. If more than one callback with the
-- same incoming message typewill be in the list, only first will be
-- put in.
createDMap :: forall m a. [CallbackWrapper m a] -> D.DMap Sing (Callback m a)
createDMap callbacks =
    D.fromList $
    map (\(CallbackWrapper c@(_ :: Callback m a n)) -> ((SNat :: SNat n) :=> c))
        callbacks

-- | Executes a callback given a dmap, switching integer and BS.
runCallback :: D.DMap Sing (Callback m a) -> SNat n -> ByteString -> m a
runCallback dmap sn bs = case dmap D.! sn of (Callback x) -> x (parse bs)

-- | Same as 'runCallback', but accepts value-level integer.
runCallbackInt :: D.DMap Sing (Callback m a) -> Integer -> ByteString -> m a
runCallbackInt dmap i bs =
    reifyNat i $ \(Proxy :: Proxy n) -> runCallback dmap (SNat :: SNat n) bs



----------------------------------------------------------------------------
-- Testing
----------------------------------------------------------------------------

data Msg1 = Msg1 String
data Msg2 = Msg2 Integer
data Msg3 = Msg3 Double

class Binary b where
    parse :: ByteString -> b

instance Binary Msg1 where
    parse _ = Msg1 "wow,parsed)))"
instance Binary Msg2 where
    parse _ = Msg2 10
instance Binary Msg3 where
    parse _ = Msg3 1.2345


instance Message Msg1 where
    type MsgTag Msg1 = 1
instance Message Msg2 where
    type MsgTag Msg2 = 2
instance Message Msg3 where
    type MsgTag Msg3 = 3 -- it's an error if you put "2" here,
                               -- thanks to injective type families

_test :: IO ()
_test = do
    let r1 = handler $ \(Msg1 s) -> putStrLn s
    let r2 = handler $ \(Msg2 i) -> putText "wat?" >> print (i + 1)
    let dmap = createDMap [r1,r2]
    runCallback dmap (SNat :: SNat 3) "aoeu"