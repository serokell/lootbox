module Test.Running where

import Universum

import Internal (ClientContext (..), ServerContext (..), clientRequest, serverReply, withClient,
                 withServer)
import Log (asyncLog, setupAleLoggingWithName)
import System.Wlog (LoggerNameBox, WithLoggerIO, logError)
import Test.HUnit (Test (..), runTestTT)

main = runTestTT $ TestCase $ setupAleLoggingWithName "test" $
    withServer "127.0.0.1" 12345 ["test-server"] 1000000 $ \sCtx ->
        withClient "127.0.0.1" 12345 ["test-client"] 1000000 $ \cCtx -> do
            let ([cc], [sc]) = (ccComponents cCtx, scComponents sCtx)
            void $ asyncLog $ forever $ serverReply sc $ \_ resp -> resp ""
            clientRequest cc ""
