{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}

-- | Utilities for the network. Move them to some lootbox-swalka
-- later.

module Loot.Network.Utils
    ( LensLike
    , HasLens (..)
    , HasLens'
    , findM
    , tMeasure
    , tMeasureIO
    , whileM
    ) where

import Data.Coerce (coerce)
import Data.Tagged (Tagged (..))
import qualified Data.Time.Clock as Tm
import Numeric as N

type LensLike f s t a b = (a -> f b) -> s -> f t

class HasLens tag outer inner | tag outer -> inner where
    lensOf :: Lens' outer inner

instance HasLens a a a where
    lensOf = id

instance HasLens t (Tagged t a) a where
    lensOf = \f -> fmap coerce . f . coerce

type HasLens' s a = HasLens a s a

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

whileM :: (Monad m) => m Bool -> m () -> m ()
whileM predicate action = whenM predicate (action >> whileM predicate action)
