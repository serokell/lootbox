{-# LANGUAGE RecordWildCards #-}

module Test.Loot.Async where

import Universum

import Control.Concurrent (killThread, myThreadId, threadDelay)
import Control.Exception (AsyncException (ThreadKilled), SomeException (SomeException))
import Control.Exception.Safe (catchAny, handleAsync, isAsyncException, throwIO, throwTo, tryAsync)
import GHC.Stack (emptyCallStack)

import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, (@?), (@?=))

import Loot.Async


data TestException = TestException
    deriving Show

instance Exception TestException

-----------------------
-- withWorker
-----------------------

data WorkerTestResult e = WorkerTestResult
    { wtrReturn :: Either e ()
    , wtrGoDone :: Bool
    }

-- | This function starts a worker action and a main action, augmenting them
-- with additional singaling.
--
-- It asserts that in the end the worker thread is terminated, checks whether
-- the main thread exits without an exception and returns the overall result.
withWorker_test :: Exception e
                => (forall void. IO void)  -- ^ Worker action
                -> IO ()                   -- ^ Main action
                -> IO (WorkerTestResult e)
withWorker_test worker go = do
    workerReady <- newEmptyMVar
    workerDied  <- newEmptyMVar
    goDone      <- newIORef False

    r <- tryAsync $ withWorker
        ((putMVar workerReady () >> worker) `finally` putMVar workerDied ())
        (takeMVar workerReady >> go >> writeIORef goDone True)

    -- If the worker does not die eventually, this line will block forever
    -- (which is not ideal) or, hopefully, throw an exception.
    -- In any case, the test will not succeeed :).
    () <- takeMVar workerDied

    WorkerTestResult r <$> readIORef goDone

assertWtr :: Bool -> Bool -> WorkerTestResult WorkerFailed -> Assertion
assertWtr expectRight expectGoDone WorkerTestResult{..} = do
    assertEqual "return right" expectRight (isRight wtrReturn)
    assertEqual "go done"      expectGoDone wtrGoDone


--------
-- First, some sanity checks.
--------

testResult :: WorkerFailed
testResult = WorkerFailed emptyCallStack (SomeException TestException)

unit_withWorker_exception_is_async :: Assertion
unit_withWorker_exception_is_async = do
    isAsyncException testResult @? "isAsync"

    me <- myThreadId
    tryAsync (catchAny (throwTo me testResult) (const pass)) >>= \case
        Right _                  -> assertFailure $ "caught by safe `catchAny`"
        Left (_ :: WorkerFailed) -> pass


unit_withWorker_exception_can_be_caught :: Assertion
unit_withWorker_exception_can_be_caught = do
    me <- myThreadId
    handleAsync (\(_ :: WorkerFailed) -> pass) $ throwTo me testResult

-- | Make sure there is location information.
unit_location :: Assertion
unit_location = do
    --
    -- Achtung!
    --
    -- This test is sensitive to its location in the source code!
    -- Please, update the values in the @where@ clause if it starts failing
    -- due to changes to the lines above it or the name of the file.
    --
    r <- tryAsync $ withWorker
        (throwIO TestException)
        (forever $ threadDelay 1000)
    case r of
        Right _                  -> assertFailure "Exception expected"
        Left (e :: WorkerFailed) -> show e @?= expectedMessage
  where
    expectedMessage = "Worker at " <> loc <> " failed: TestException" :: String
    loc = "test/Test/Loot/Async.hs:" <> show lineNo <> ":21 in main:Test.Loot.Async"
    lineNo = 94 :: Int

--------
-- Now actual tests.
--------

-- | Main thread exits -> worker is killed.
unit_withWorker_main_exit :: Assertion
unit_withWorker_main_exit =
    withWorker_test (forever $ threadDelay 1000) (threadDelay 1000) >>=
    assertWtr True True

-- | Main crashes -> worker is killed.
unit_withWorker_main_crash :: Assertion
unit_withWorker_main_crash = do
    -- In this test the main thread dies, so it is a little different from the rest.
    -- In particular, tests using 'assertWtr' catch 'WorkerFailed' exception, while
    -- this test catches 'TestException' used to kill the main thread.
    withWorker_test (forever $ threadDelay 1000) (throwIO TestException) >>= \case
        WorkerTestResult _ True -> assertFailure "main should not exit"
        WorkerTestResult (Right _) _ -> assertFailure "Exception expected"
        WorkerTestResult (Left (_ :: TestException)) _ -> pure ()

-- | Worker crashes -> main thread is killed.
unit_withWorker_worker_crash :: Assertion
unit_withWorker_worker_crash =
    withWorker_test (throwIO TestException) (threadDelay 1000) >>=
    assertWtr False False

-- | Worker receives an async exception -> main thread is killed.
unit_withWorker_worker_crash_async :: Assertion
unit_withWorker_worker_crash_async =
    withWorker_test worker (threadDelay 1000) >>=
    assertWtr False False
  where
    worker = myThreadId >>= \me -> throwTo me TestException >> error "unreachable"

-- | Worker receives ThreadKilled -> main thread is killed.
unit_withWorker_worker_crash_ThreadKilled :: Assertion
unit_withWorker_worker_crash_ThreadKilled =
    withWorker_test worker (threadDelay 1000) >>=
    assertWtr False False
  where
    worker = myThreadId >>= \me -> throwTo me ThreadKilled >> error "unreachable"

-- | Main thread kills worker -> main thread is killed.
unit_withWorker_main_kills_worker :: Assertion
unit_withWorker_main_kills_worker = do
    v <- newEmptyMVar
    wtr <- withWorker_test
        (myThreadId >>= putMVar v  >> forever (threadDelay 1000))
        (takeMVar v >>= killThread >> threadDelay 1000)
    assertWtr False False wtr
