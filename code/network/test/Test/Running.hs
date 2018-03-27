module Test.Running where

import Universum

import Internal (ClientContext (..), ServerContext (..), clientRequest, serverReply, setupLogging,
                 withClient, withServer)
import Test.HUnit (Test (..), runTestTT)
import UnliftIO.Async (async)

main = runTestTT $ TestCase $ setupLogging "test" $
    withServer "127.0.0.1" 12345 ["test"] 1000000 $ \sCtx ->
        withClient "127.0.0.1" 12345 ["test"] 1000000 $ \cCtx -> do
            let ([cc], [sc]) = (ccComponents cCtx, scComponents sCtx)
            void $ async $ forever $ serverReply sc $ \_ resp -> resp ""
            clientRequest cc ""
