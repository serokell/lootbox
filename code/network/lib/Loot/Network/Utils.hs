{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}

-- | Utilities for the network. Move them to some lootbox-swalka
-- later.

module Loot.Network.Utils
    ( findM
    , tMeasure
    , tMeasureIO
    , whileM
    , TimeDurationMs (..)
    , getCurrentTimeMs
    ) where

import qualified Data.Time.Clock as Tm
import Data.Time.Clock.POSIX (getPOSIXTime)
import Numeric as N

-- | Monadic version of 'find'.
findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ []     = pure Nothing
findM p (x:xs) = ifM (p x) (pure $ Just x) (findM p xs)

tMeasureIO :: (MonadIO m) => Text -> m a -> m a
tMeasureIO = tMeasure putTextLn

-- | Takes the first time sample, executes action (forcing its
-- result), takes the second time sample, logs it.
tMeasure :: (MonadIO m) => (Text -> m ()) -> Text -> m a -> m a
tMeasure logAction label action = do
    before <- liftIO Tm.getCurrentTime
    !x <- action
    after <- liftIO Tm.getCurrentTime
    let d0 :: Integer
        d0 = round $ 100000 * toRational (after `Tm.diffUTCTime` before)
    let d1 :: Double = fromIntegral d0 / 100
    logAction $ "tMeasure " <> label <> ": " <> formatFloatN d1 3 <> " ms"
    pure x
  where
    formatFloatN n numOfDecimals = fromString $ N.showFFloat (Just numOfDecimals) n ""

-- | Execute "pred >>= action" while predicate returns true.
whileM :: (Monad m) => m Bool -> m () -> m ()
whileM predicate action = whenM predicate (action >> whileM predicate action)

-- Time interval represented in
newtype TimeDurationMs = TimeDurationMs Integer deriving (Eq,Ord,Num,Show)

-- Gets current time in POSIX ms.
getCurrentTimeMs :: IO TimeDurationMs
getCurrentTimeMs = TimeDurationMs . floor . (*1000) <$> getPOSIXTime
