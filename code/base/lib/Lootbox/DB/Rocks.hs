{-# LANGUAGE TypeOperators #-}

-- | This module provides RocksDB support.  It is later used to
-- instantiate 'MonadRocksDB'.
module Lootbox.DB.Rocks
       ( withDBImpl

       , RocksDB
       , withRocksDB
       , getRocksDBRaw
       , putRocksDBRaw
       , delRocksDBRaw
       ) where

import Universum

import Codec.Serialise (deserialiseOrFail)
import Control.Monad.Base (MonadBase)
import Control.Monad.Reader (withReaderT)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit (Source, bracketP, yield)
import Data.Default (def)
import Monad.Capabilities (CapImpl (CapImpl), CapsT, HasNoCap, addCap)
import System.Directory (createDirectoryIfMissing)

import Lootbox.Capabilities (CapImplIODB)
import Lootbox.DB (DB (..), DBError (..), EntryOperations (..), Persistable (..), PrefixTag,
                   StoragePrefix (..), rawKey)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Database.RocksDB as Rocks

data RocksDB = RocksDB
    { rocksDB           :: Rocks.DB
    , rocksReadOptions  :: Rocks.ReadOptions
    , rocksWriteOptions :: Rocks.WriteOptions
    }

openRocksDB :: MonadIO m => FilePath -> m RocksDB
openRocksDB path = do
    liftIO $ createDirectoryIfMissing True path
    db <- Rocks.open path def { Rocks.createIfMissing = True }
    pure $ RocksDB db def def

closeRocksDB :: MonadIO m => RocksDB -> m ()
closeRocksDB db = Rocks.close $ rocksDB db

withRocksDB :: (MonadIO m, MonadMask m) => FilePath -> (RocksDB -> m a) -> m a
withRocksDB dbPath = bracket (openRocksDB dbPath) closeRocksDB


entryName :: StoragePrefix value -> ByteString -> ByteString
entryName pref k = unStoragePrefix pref <> "/" <> k


getRocksDBRaw :: MonadIO m
              => RocksDB -> PrefixTag -> ByteString -> m (Either DBError ByteString)
getRocksDBRaw RocksDB{..} pref k = do
    val <- Rocks.get rocksDB rocksReadOptions (rawKey pref k)
    pure $ maybeToRight DBKeyNotFound val

putRocksDBRaw :: MonadIO m
              => RocksDB -> PrefixTag -> ByteString -> ByteString -> m ()
putRocksDBRaw RocksDB{..} pref k v =
    Rocks.put rocksDB rocksWriteOptions (rawKey pref k) v
{-# ANN putRocksDBRaw ("HLint: ignore Eta reduce" :: Text) #-}

delRocksDBRaw :: MonadIO m => RocksDB -> PrefixTag -> ByteString -> m ()
delRocksDBRaw RocksDB{..} pref k = Rocks.delete rocksDB rocksWriteOptions (rawKey pref k)

withRocksDBIterator :: forall m value a.
                       (MonadIO m, MonadMask m, Persistable value, MonadBase IO m)
                    => RocksDB
                    -> ByteString
                    -> Int
                    -> (EntryOperations Rocks.Iterator value m -> Source (ResourceT m) Rocks.Iterator -> m a)
                    -> m a
withRocksDBIterator db iprefix idropLength cont = cont ops (bracketP openIter closeIter produce)
  where
    openIter  = Rocks.createIter (rocksDB db) def
    closeIter = Rocks.releaseIter

    prefix = entryName (storagePrefix (Proxy :: Proxy value)) iprefix
    dropLength = BS.length prefix - BS.length iprefix + idropLength

    -- When implementing these we know that the iterator is valid
    -- and its key has prefix which we expect.
    ops = EntryOperations
        { eoGet = \it -> do
            Just (k, v) <- Rocks.iterEntry it
            let k' = BS.drop dropLength k
            let v' = case deserialiseOrFail $ BSL.fromStrict v of
                  Left _  -> Nothing
                  Right x -> Just x
            pure (k', v')
        , eoDelete = \it -> do
            Just k <- Rocks.iterKey it
            let k' = BS.drop dropLength k
            delRocksDBRaw db (BS.init prefix) k'
        }

    produce :: Rocks.Iterator -> Source (ResourceT m) Rocks.Iterator
    produce iter = Rocks.iterSeek iter prefix >> loop
      where
        loop :: Source (ResourceT m) Rocks.Iterator
        loop = whenM (checkPrefix iter) $ do
            -- ^ We stop when the key no longer starts with 'prefix'
            yield iter
            Rocks.iterNext iter
            loop

    checkPrefix iter = checkKey <$> Rocks.iterKey iter
      where
        checkKey (Just k) = prefix `BS.isPrefixOf` k
        checkKey Nothing  = False


-- | An implementation of the 'DB' capability which uses a Rocks database.
withDBImpl :: (MonadIO m, MonadMask m, MonadBase IO m,
               HasNoCap DB caps)
           => FilePath
           -> (CapImplIODB DB '[] -> CapsT (DB : caps) m a)
           -> CapsT caps m a
withDBImpl dbPath cont = withRocksDB dbPath $ \db ->
    let dbImpl :: CapImplIODB DB '[]
        dbImpl = CapImpl DB
            { _getDBRaw = getRocksDBRaw db
            , _putDBRaw = putRocksDBRaw db
            , _delDBRaw = delRocksDBRaw db
            , _withDBIterator = withRocksDBIterator db
            }
    in withReaderT (addCap dbImpl) (cont dbImpl)
