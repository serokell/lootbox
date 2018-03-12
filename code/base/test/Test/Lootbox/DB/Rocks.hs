{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Lootbox.DB.Rocks where

import Universum

import Test.HUnit (Assertion, (@=?), (@?))

import Control.Monad.Base (MonadBase)
import Data.Conduit (runConduitRes, (.|))
import Data.Conduit.List (consume)
import Monad.Capabilities (CapabilitiesBuilder (NoCaps), CapsT, initCaps)
import System.IO.Temp (withSystemTempDirectory)

import Lootbox.DB
import Lootbox.DB.Rocks (withDBImpl)


unit_putGet :: Assertion
unit_putGet = do
    x <- runWithTestDB $ do
        putDB' "hi" (25 :: Word32)
        getDB' "hi"
    Right (25 :: Word32) @=? x

unit_getNonExistent :: Assertion
unit_getNonExistent = do
    x <- runWithTestDB $ do
        getDB' @Word32 "hi"
    Left DBKeyNotFound @=? x

unit_del :: Assertion
unit_del = do
    x <- runWithTestDB $ do
        putDB' "hi" (25 :: Word32)
        delDB' (Proxy :: Proxy Word32) "hi"
        getDB' @Word32 "hi"
    Left DBKeyNotFound @=? x


col :: Collection (Int, Word32) '[Int] Word32
col = Collection "collection" fst snd (,)

unit_collection :: Assertion
unit_collection = do
    xs <- runWithTestDB $ do
        putDBCollection col (2, 20)
        putDBCollection col (1, 10)
        putDBCollection col (3, 30)
        iterDBCollection col >>= \src -> runConduitRes (src .| consume)
    [(1, 10), (2, 20), (3, 30)] @=? xs

unit_collectionWipe :: Assertion
unit_collectionWipe = do
    xs <- runWithTestDB $ do
        putDBCollection col (2, 20)
        putDBCollection col (1, 10)
        putDBCollection col (3, 30)
        wipeDBCollection col
        iterDBCollection col >>= \src -> runConduitRes (src .| consume)
    null xs @? "Not empty: " <> show xs

unit_collectionDel :: Assertion
unit_collectionDel = do
    xs <- runWithTestDB $ do
        putDBCollection col (2, 20)
        putDBCollection col (1, 10)
        putDBCollection col (3, 30)
        delDBCollection col (2, 20)
        iterDBCollection col >>= \src -> runConduitRes (src .| consume)
    [(1, 10), (3, 30)] @=? xs

col3 :: Collection (Word32, Word32, Word32, Word32) '[Word32, Word32, Word32] Word32
col3 = Collection
    { cName = "test3"
    , cToKeys = \(x1, x2, x3, _x4) -> (x1, (x2, x3))
    , cToItem = \(_x1, _x2, _x3, x4) -> x4
    , cRecollect = \(x1, (x2, x3)) x4 -> (x1, x2, x3, x4)
    }

unit_SubCollectionsIter :: Assertion
unit_SubCollectionsIter = do
    let items = [(i, 2 * j, 3 * k, i + j + k) | i <- [1..10], j <- [1..10], k <- [1..10]]
    xs <- runWithTestDB $ do
        forM_ items $ putDBCollection col3
        iterDBCollectionPrefix (CollectionPrefix @('S 'Z) col3 5) >>= \src -> runConduitRes (src .| consume)
    xs2 <- runWithTestDB $ do
        forM_ items $ putDBCollection col3
        iterDBCollectionPrefix (CollectionPrefix @('S ('S 'Z)) col3 (5, 2)) >>= \src -> runConduitRes (src .| consume)
    sort (filter (\t -> t^._1 == 5) items) @=? sort xs
    sort (filter (\t -> t^._1 == 5 && t^._2 == 2) items) @=? sort xs2

runWithTestDB :: (MonadIO m, MonadMask m, MonadBase IO m) => CapsT '[DB] m a -> m a
runWithTestDB x =
    withSystemTempDirectory "loot-db-test" $ \dbPath ->
        usingReaderT (initCaps NoCaps) $
            withDBImpl dbPath $ \_ -> x
