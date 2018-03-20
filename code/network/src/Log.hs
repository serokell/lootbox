{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Stuff for logging

module Log
       ( setupAleLoggingWithName
       , asyncLog
       , withAsyncLog
       , liftLogIO2
       ) where

import Universum

import Control.Concurrent.Async (Async, async, uninterruptibleCancel)
import Control.Monad.IO.Unlift (MonadUnliftIO (..), UnliftIO (..), withUnliftIO)
import Crypto.Random.Types (MonadRandom, getRandomBytes)
import System.Wlog (LoggerName, LoggerNameBox (..), WithLoggerIO, askLoggerName,
                    buildAndSetupYamlLogging, liftLogIO, productionB, usingLoggerName)

-- | Logging configurations file.
logConfigFile :: FilePath
logConfigFile = "log-config.yaml"

-- | Sets up logging configurations using logging conigurations file.
setupAleLogging :: MonadIO m => m ()
setupAleLogging = buildAndSetupYamlLogging productionB logConfigFile

-- | Sets up logging configurations using logging conigurations file
-- and uses given logger name.
setupAleLoggingWithName :: MonadIO m => LoggerName -> LoggerNameBox m a -> m a
setupAleLoggingWithName logName action = do
    setupAleLogging
    usingLoggerName logName action

asyncLog :: WithLoggerIO m => LoggerNameBox IO a -> m (Async a)
asyncLog = liftLogIO async

-- | Similar to 'liftLogIO' but lifts function with two arguments.
liftLogIO2 :: WithLoggerIO m
           => (IO a -> IO b -> IO c)
           -> LoggerNameBox IO a
           -> LoggerNameBox IO b
           -> m c
liftLogIO2 f a b = do
    logger <- askLoggerName
    liftIO $ f (usingLoggerName logger a)
               (usingLoggerName logger b)

-- | This is 'withAsync' lifted from 'IO' to 'IO'-with-logging.  This
-- is a re-implementation of 'withAsync' because it's impossible to
-- end up in arbitrary monad @WithLoggerIO m => m@ while using default
-- 'withAsync'.
withAsyncLog :: (MonadMask m, WithLoggerIO m)
             => LoggerNameBox IO a -> (Async a -> m b) -> m b
withAsyncLog a = bracket (asyncLog a) (liftIO . uninterruptibleCancel)


instance MonadRandom m => MonadRandom (LoggerNameBox m) where
    getRandomBytes = lift . getRandomBytes

instance MonadFail m => MonadFail (LoggerNameBox m) where
    fail = lift . fail

instance MonadUnliftIO m => MonadUnliftIO (LoggerNameBox m) where
    askUnliftIO = LoggerNameBox $ ReaderT $ \r ->
                  withUnliftIO $ \u ->
                  return (UnliftIO (unliftIO u . flip runReaderT r . loggerNameBoxEntry))
