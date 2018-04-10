module Test.Loot.Network.Internal where

import Universum

import Loot.Network.Internal

import Test.HUnit (Test (..), runTestTT)

import UnliftIO.Async (async)

main = runTestTT $ TestCase $ setupLogging "test" $
    withServer "127.0.0.1" 12345 ["test"] 1000000 $ \sCtx ->
        withClient "127.0.0.1" 12345 ["test"] 1000000 $ \cCtx -> do
            let ([cc], [sc]) = (ccComponents cCtx, scComponents sCtx)
            void $ async $ forever $ serverReply sc $ \_ resp -> resp ""
            clientRequest cc ""
