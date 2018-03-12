{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | Database capability.

module Lootbox.DB.Internal
       ( PrefixTag
       , StoragePrefix (unStoragePrefix)
       , Persistable (..)

       , DB (..)
       , EntryOperations (..)
       , DBError (..)
       , MonadDB(..)

       , getDB
       , putDB
       , delDB

       , getDB'
       , putDB'
       , delDB'

       , Collection (..)
       , iterDBCollection
       , listDBCollection
       , putDBCollection
       , delDBCollection
       , wipeDBCollection

       , MyNat (..)
       , CollectionPrefix (..)
       , iterDBCollectionPrefix
       , listDBCollectionPrefix

       , rawKey
       ) where

import Universum

import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Control.Monad.Trans.Resource (MonadBaseControl, ResourceT)
import Data.Conduit (Source, runConduitRes, (.|))
import Data.Typeable (typeRep)
import Monad.Capabilities (makeCap)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit.List as CL
import qualified Serokell.Util.Base16 as B16

-- | A wrapper used to provide a storage prefix in the 'Persistable'
-- class that adds a necessary phantom type.
newtype StoragePrefix a = StoragePrefix { unStoragePrefix :: PrefixTag }
    deriving (Show, IsString)

-- | Convenient prefix type alias.
type PrefixTag = ByteString

-- | The class for things that can be stored in some database.
class Serialise a => Persistable a where
    storagePrefix :: Proxy a -> StoragePrefix a

    default storagePrefix :: Typeable a => Proxy a -> StoragePrefix a
    storagePrefix proxy = show $ typeRep proxy

instance Persistable Word32 where
    storagePrefix _ = "i32"

-- | Things that can go wrong when accessing a database
data DBError = DBKeyNotFound
             | DBCorruptedValue Text
    deriving (Eq, Show)

-- | Operations that can be performed on a database-specific entry.
data EntryOperations entry v m = EntryOperations
    { eoGet    :: Persistable v => entry -> ResourceT m (ByteString, Maybe v)
    , eoDelete :: entry -> ResourceT m ()
    }

-- | 'DB' 'Capability' allows one to store data in a database.
data DB m = DB
    { _getDBRaw :: PrefixTag -> ByteString -> m (Either DBError ByteString)
    , _putDBRaw :: PrefixTag -> ByteString -> ByteString -> m ()
    , _delDBRaw :: PrefixTag -> ByteString -> m ()

    , _withDBIterator :: forall v a. Persistable v
                      => ByteString
                      -- ^ Prefix to iterate through
                      -> Int
                      -- ^ Number of bytes to drop from key.  Can
                      -- differ from length of prefix when
                      -- sub-collections are iterated.
                      -> (forall entry. EntryOperations entry v m -> Source (ResourceT m) entry -> m a)
                      -- ^ Continuation
                      -> m a
    }
makeCap ''DB

getDB' :: forall v m . (Monad m, MonadDB m, Persistable v)
       => ByteString -> m (Either DBError v)
getDB' k = do
    val <- getDBRaw (getPrefix (Proxy :: Proxy v)) k
    pure $ case val of
        Right val' -> first (DBCorruptedValue . show) $ deserialiseOrFail $ BSL.fromStrict val'
        Left e     -> Left e

putDB' :: forall v m . (MonadDB m, Persistable v) => ByteString -> v -> m ()
putDB' k val = putDBRaw (getPrefix (Proxy :: Proxy v)) k (strictSer val)

delDB' :: forall v m . (MonadDB m, Persistable v) => Proxy v -> ByteString -> m ()
delDB' proxy = delDBRaw (getPrefix proxy)

-- | We can use an arbitrary serialisable type as a key.  Base16 (hex)
-- was chosen to preserve lexicographic ordering of encoded values.
-- Base64 encoding is not lexicographic.
toKey :: Serialise k => k -> ByteString
toKey = encodeUtf8 . B16.encode . BSL.toStrict . serialise

fromKey :: Serialise k => ByteString -> Either Text k
fromKey bs = do
    bu <- first show $ decodeUtf8Strict bs
    b <- B16.decode bu
    first show $ deserialiseOrFail $ BSL.fromStrict b

-- | DB key from prefix and raw key.
rawKey :: PrefixTag -> ByteString -> ByteString
rawKey pref k = pref <> "/" <> k

strictSer :: Serialise a => a -> ByteString
strictSer = BSL.toStrict . serialise

getPrefix :: forall value . Persistable value => Proxy value -> ByteString
getPrefix proxy = unStoragePrefix $ storagePrefix proxy

-- | Like 'getDB\'' but the key can be anything serialisable
getDB :: (Monad m, MonadDB m, Serialise k, Persistable v)
      => k
      -> m (Either DBError v)
getDB = getDB' . toKey

-- | Like 'putDB\'' but the key can be anything serialisable
putDB :: (MonadDB m, Serialise k, Persistable v)
      => k
      -> v
      -> m ()
putDB = putDB' . toKey

-- | Like 'delDB\'' but the key can be anything serialisable
delDB :: forall v k m . (MonadDB m, Serialise k, Persistable v)
      => k
      -> Proxy v
      -> m ()
delDB k proxy = delDB' proxy $ toKey k

-- | Helper type to represent keys for many-leveled 'Collection's.
-- Keys for @Collection e '[k1, k2, k3, k4] i@ is represented as @(k1,
-- (k2, (k3, k4)))@.  'Collection's should have at least one element
-- as a key.
type family ColKeyRep (ks :: [Type]) where
    ColKeyRep '[k] = k
    ColKeyRep (k : ks) = (k, ColKeyRep ks)

-- | A collection is something like a set of items stored in a database.
-- Each item need a key to be put into the db at and this class explains
-- how to turn a value into a key. Sometimes the key cannot be deried from
-- the item itself and additional information is required, hence this class.
data Collection e (ks :: [Type]) i = Collection
    { cName      :: ByteString
    -- ^ Name of this collection.

    , cToKeys    :: e -> ColKeyRep ks
    -- ^ When we store an element to a database, we need a key which we
    -- will store it at. This function has to be as close to an injection
    -- as possible.
    --
    -- Keep in mind that iteration over the items of the collection will happen
    -- in the lexicographic order of the keys returned by this funtion.

    , cToItem    :: e -> i
    -- ^ Strip information that is used only to derive the key.

    , cRecollect :: ColKeyRep ks -> i -> e
    -- ^ Given a key and an item restore the entire wrapped collection element.
    }



-- | Add a new item to the collection.
putDBCollection :: forall m e k ks i. (Collectible k ks, MonadDB m, Persistable i)
                => Collection e (k:ks) i
                -> e
                -> m ()
putDBCollection c e = putDB' (collectionKey c e) (cToItem c e)

-- | Delete an item to the collection.
delDBCollection :: forall m e k ks i. (Collectible k ks, MonadDB m, Persistable i)
                => Collection e (k:ks) i
                -> e
                -> m ()
delDBCollection c e = delDB' (Proxy :: Proxy i) (collectionKey c e)

-- | Iterate over all items in the collection.
-- The order of iteration is determined by 'toCollectionKey'.
iterDBCollection :: forall m e k ks i.
                    (Monad m, MonadDB m, Collectible k ks, Persistable i)
                 => Collection e (k:ks) i
                 -> m (Source (ResourceT m) e)
iterDBCollection c = iterDBCollectionPrefix (CollectionPrefix @'Z c ())

listDBCollection :: forall m e k ks i.
                    (MonadBaseControl IO m, MonadDB m, Collectible k ks, Persistable i)
                 => Collection e (k:ks) i
                 -> m [e]
listDBCollection c = listDBCollectionPrefix (CollectionPrefix @'Z c ())

-- | Remove all items in this collection.
wipeDBCollection :: forall m e k ks i.
                    (MonadBaseControl IO m, MonadDB m, Collectible k ks, Persistable i)
                 => Collection e (k:ks) i
                 -> m ()
wipeDBCollection c = withDBIterator name (BS.length name) wipe
  where
    name = cName c <> "/"
    wipe :: EntryOperations entry i m
         -> Source (ResourceT m) entry
         -> m ()
    wipe ops src = runConduitRes $ src .| CL.mapM_ (eoDelete ops)

-- | Basically, it's a synonym for @Each '[Serialise] (k:ks)@ but I
-- failed to convince compiler in that.
type Collectible k ks = CollectibleOps k ks

-- | This class contains everything one need to do differently for
-- one-element and many-element 'Collection's.
class CollectibleOps k ks where
    collectionKey' :: ColKeyRep (k:ks) -> ByteString
    -- ^ Compute key that will be used in DB.
    decode' :: [ByteString] -> Either DBError (ColKeyRep (k:ks))
    -- ^ Parse key from DB representation back to 'ColKeyRep'.

instance Serialise k => CollectibleOps k '[] where
    collectionKey' key = toKey key
    decode' [key'] = first (DBCorruptedValue . show) $ fromKey key'
    decode' _      = Left $ DBCorruptedValue "Too many keys"

instance (Each '[Serialise] (k1:k2:ks), CollectibleOps k2 ks) => CollectibleOps k1 (k2:ks) where
    collectionKey' (key, keysRest) = toKey key <> "/" <> collectionKey' @k2 @ks keysRest
    decode' []          = Left $ DBCorruptedValue "Too few keys"
    decode' (key':rest) = do
        key <- first (DBCorruptedValue . show) $ fromKey key'
        keysTail <- decode' @k2 @ks rest
        pure (key, keysTail)

collectionKey :: forall e k ks i. Collectible k ks
   => Collection e (k:ks) i
   -> e
   -> ByteString
collectionKey c e = cName c <> "/" <> collectionKey' @k @ks (cToKeys c e)


-- | We need own type for numbers because standard GHC 'Nat' does not
-- support pattern-matching.
data MyNat = Z | S MyNat

-- | Partially traversed collection query type.  Encoding is similar
-- to 'ColKeyRep'.
type family ColPreRep (n :: MyNat) (ks :: [Type]) where
    ColPreRep 'Z ks = ()
    ColPreRep ('S 'Z) (k:ks) = k
    ColPreRep ('S ('S n)) (k1:k2:ks) = (k1, ColPreRep ('S n) (k2:ks))

-- | Datatype that represents "sub-collection".
data CollectionPrefix (n :: MyNat) e (ks :: [Type]) i = CollectionPrefix
    { cpColl :: Collection e ks i
    , cpKeys :: ColPreRep n ks
    }

class ColPreOps (n :: MyNat) (ks :: [Type]) where
    cpoFormat :: ColPreRep n ks -> ByteString

instance ColPreOps 'Z ks where
    cpoFormat _ = ""

instance Serialise k => ColPreOps ('S 'Z) (k:ks) where
    cpoFormat x = toKey x

instance (Each '[Serialise] (k1:k2:ks), ColPreOps ('S n) (k2:ks)) => ColPreOps ('S ('S n)) (k1:k2:ks) where
    cpoFormat (key, keysRest) = toKey key <> "/" <> cpoFormat @('S n) @(k2:ks) keysRest

-- | Iterate over all items in the sub-collection.
-- The order of iteration is determined by 'toCollectionKey'.
iterDBCollectionPrefix :: forall m n e k ks i.
                    (Monad m, MonadDB m, Collectible k ks, ColPreOps n (k:ks), Persistable i)
                 => CollectionPrefix n e (k:ks) i
                 -> m (Source (ResourceT m) e)
iterDBCollectionPrefix cp = withDBIterator prefix (BS.length name) $ \ops src -> pure $
    src .| CL.mapM (eoGet ops) .| CL.mapMaybe decode .| CL.map (uncurry $ cRecollect $ cpColl cp)
  where
    name, prefix :: ByteString
    name = cName (cpColl cp) <> "/"
    prefix = name <> (cpoFormat @n @(k:ks) $ cpKeys cp)
    decode :: (ByteString, Maybe i) -> Maybe (ColKeyRep (k:ks), i)
    decode (k, mv) = case (decode' @k @ks (BS.split (BS.head "/") k), mv) of
        (Right k', Just v) -> Just (k', v)
        _                  -> Nothing  -- TODO: db is corrupted?

listDBCollectionPrefix :: forall m n e k ks i.
                          ( MonadBaseControl IO m, MonadDB m, Collectible k ks
                          , ColPreOps n (k:ks), Persistable i
                          )
                       => CollectionPrefix n e (k:ks) i
                       -> m [e]
listDBCollectionPrefix cp =
    iterDBCollectionPrefix cp >>= \src -> runConduitRes (src .| CL.consume)
