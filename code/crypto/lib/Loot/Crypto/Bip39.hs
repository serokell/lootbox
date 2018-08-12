{- This Source Code Form is subject to the terms of the Mozilla Public
 - License, v. 2.0. If a copy of the MPL was not distributed with this
 - file, You can obtain one at http://mozilla.org/MPL/2.0/.
 -}

-- | BIP 39 compatible implementation of mnemonics for secret keys.
module Loot.Crypto.Bip39
       (
       -- * Generating the mnemonic
         entropyToMnemonic
       , EncodingError (..)

       -- * From mnemonic to seed
       , mnemonicToSeed

       -- * Internal helpers
       , bytesToIndices
       ) where

import Control.Exception.Safe (impureThrow)
import Control.Monad.Except (MonadError (throwError), runExcept)
import Crypto.Hash (hashWith)
import Crypto.Hash.Algorithms (SHA256 (SHA256))
import Crypto.KDF.PBKDF2 (Parameters (Parameters), fastPBKDF2_SHA512)
import Data.Bits (Bits, shiftL, shiftR, zeroBits, (.|.))
import Data.ByteArray (convert, takeView)
import Data.Text.ICU.Normalize (NormalizationMode (NFKD), normalize)

import Loot.Crypto.Bip39.Wordlist (words_en)

import qualified Data.Array as A
import qualified Data.ByteString as BS


-----------------------
-- Generating the mnemonic.
-----------------------

-- | Things that can go bad when turning entropy into words.
data EncodingError
    = LengthOutOfBounds { actualLength :: Int }
    | NotDivisibleBy4 { actualRemainder :: Int }
  deriving Show

instance Exception EncodingError


-- | Turn entropy (pure bytes) into a mnemonic.
entropyToMnemonic :: HasCallStack => ByteString -> [Text]
entropyToMnemonic ent = either impureThrow id . runExcept $ do
    unless (4 <= entBytes && entBytes <= 8) $
        throwError (LengthOutOfBounds entBytes)
    unless (remainder == 0) $ throwError (NotDivisibleBy4 remainder)
    pure $ map (words_en A.!) indices
  where
    -- | Number of bytes of entropy in the input.
    entBytes :: Int
    entBytes = BS.length ent

    -- | Used to check that the number of bits was divisible by 32.
    -- (Which is the same as checking the number of bytes is divisible by 4.)
    remainder :: Int
    remainder = entBytes `rem` 4  -- 'rem' is probably faster than 'mod'

    -- WARNING: What comes next relies on the fact that there cannot be
    -- more than 256 bits of input entropy (according to the specification).
    -- So we skip all the calculations, simply take the first byte of the hash,
    -- stick it to the end and then split the resulting byte string into groups
    -- of 11 bits discarding any extra bits that remain.
    -- As long as there are no more than 256 bits of entropy, we will need only
    -- the first byte of the hash.

    -- | Checksum bits plus some unused bits to fill the last byte.
    csWithExtra :: ByteString
    csWithExtra = convert $ takeView (hashWith SHA256 ent) 1

    indices :: [Word16]
    indices = bytesToIndices . BS.unpack $ ent `BS.append` csWithExtra


-----------------------
-- From mnemonic to seed.
-----------------------

-- | Create a binary seed from a mnemonic.
--
-- The mnemonic can be anything, not only one produced by 'entropyToMnemonic'.
mnemonicToSeed :: Text  -- ^ Mnemonic
               -> Text  -- ^ Passphrase
               -> ByteString
mnemonicToSeed mnem pph = fastPBKDF2_SHA512 params mnem' ("mnemonic" <> pph')
  where
    params :: Parameters
    params = Parameters 2048 64

    mnem' :: ByteString
    mnem' = encodeUtf8 . normalize NFKD $ mnem

    pph' :: ByteString
    pph' = encodeUtf8 . normalize NFKD $ pph


-----------------------
-- Internal stuff.
-----------------------

-- | Split bytes into groups of 11 bits (discarding leftovers).
bytesToIndices :: [Word8] -> [Word16]
bytesToIndices = go 0 zeroBits
  where
    go :: Int        -- ^ n: How many bits we already have; n < 11
       -> Word16     -- ^ Five zeros, n bits we already have, rest are zeros
       -> [Word8]
       -> [Word16]
    go _ _ [] = []
    go n c (b : bs) =
        let -- | This many bits we still need.
            needBits = 11 - n
            -- | This many we can take from the next byte.
            takeBits = min needBits 8
            -- | This many will remain because we don't need them.
            dropBits = 8 - takeBits
            -- | This many we will have after this step.
            haveBits = n + takeBits

            -- | Add new bits to existing ones.
            c' = c .|. (fromIntegral b `shiftR` dropBits `shiftL` (needBits - takeBits))
        in
            if haveBits == 11
            then
                let newC = fromIntegral (b `clearL` takeBits) `shiftL` (3 + takeBits)
                in c' : go dropBits newC bs
            else
                go haveBits c' bs


-- | Clear leftmost bits.
clearL :: Bits a => a -> Int -> a
clearL x i = x `shiftL` i `shiftR` i
