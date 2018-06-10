{-# LANGUAGE NoApplicativeDo #-}

-- Enabling ApplicativeDo leads to https://ghc.haskell.org/trac/ghc/ticket/14105

-- | Tests for Adapter.

module Adapter where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, putTMVar, takeTMVar, tryReadTMVar)
import Control.Concurrent.STM.TVar (modifyTVar, readTVar)
import Data.List.NonEmpty as NE
import Hedgehog
import Test.Hspec

import Loot.Network.ZMQ.Adapter

data ATL = V1 | V2 | V3 | V4 deriving (Eq,Show)

_testAtLeastOne :: IO [NonEmpty ATL]
_testAtLeastOne = do
    resVar <- newTVarIO []
    [(v1 :: TMVar ()),v2,v3,v4] <- replicateM 4 newEmptyTMVarIO
    let wait = threadDelay 20000
    let readerW = forever $ do
            results <- atomically $ do
                let cast t v = maybe (pure Nothing) (\_ -> Just t <$ takeTMVar v) =<< tryReadTMVar v
                atLeastOne $ NE.fromList [cast V1 v1, cast V2 v2, cast V3 v3, cast V4 v4]
            -- Here we process results. In our case we will just send them to the variable.
            atomically $ modifyTVar resVar $ (:) results
    withAsync readerW $ \_ -> do
        let putVars (l :: [TMVar ()]) = atomically $ forM_ l $ \i -> putTMVar i ()
        threadDelay 5000
        putVars [v2,v4]
        wait
        putVars [v1,v3]
        wait
        putVars [v1,v2,v3,v4]
        wait
        atomically $ readTVar resVar

spec_testAtLeastOne :: Spec
spec_testAtLeastOne = describe "adapter" $ do
    res <- runIO _testAtLeastOne
    it "should return expected result" $
        res `shouldBe` [ V1 :| [V2,V3,V4]
                       , V1 :| [V3]
                       , V2 :| [V4]
                       ]

-- Bug! It should work 10ms, not 1s. Still repeats 100 times, no
-- matter what I set.
hprop_shouldRunOnce :: Property
hprop_shouldRunOnce = withTests 1 $ property $ do
    liftIO $ threadDelay 10000
    success
