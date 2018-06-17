{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

module Message where

import Codec.Serialise (Serialise (..))

import Loot.Network.Message

import Test.Hspec


data Msg1 = Msg1 String
data Msg2 = Msg2 Integer
data Msg3 = Msg3 Double

instance Serialise Msg1 where
    encode = error "test"
    decode = pure $ Msg1 "wow,parsed)))"
instance Serialise Msg2 where
    encode = error "test"
    decode = pure $ Msg2 10
instance Serialise Msg3 where
    encode = error "test"
    decode = pure $ Msg3 1.2345

instance Message Msg1 where type MsgTag Msg1 = 1
instance Message Msg2 where type MsgTag Msg2 = 2
-- it's an error if you put "2" here, thanks to injective type families
instance Message Msg3 where type MsgTag Msg3 = 3

testCallbacks :: IO [Integer]
testCallbacks = do
    let create foo = handlerDecoded $ \() -> either (const $ pure 0) foo
    let r1 = create $ \(Msg1 _) -> pure 1
    let r2 = create $ \(Msg2 _) -> pure 2
    e1 <- runCallbacksInt [r1,r2] 1 "aoeu" ()
    e2 <- runCallbacksInt [r1,r2] 2 "aoeu" ()
    pure [e1,e2]

spec_runCallbacks :: Spec
spec_runCallbacks = describe "message dispatcher" $ do
    res <- runIO testCallbacks
    it "should return expected result" $
        res `shouldBe` [1,2]
