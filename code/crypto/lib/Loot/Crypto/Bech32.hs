module Loot.Crypto.Bech32
    (
          HumanReadablePart
        , encode
        , decode
    )
    where

import Universum

import qualified Codec.Binary.Bech32 as B32
import qualified Data.ByteString as BS

type HumanReadablePart = ByteString

data Bech32EncodeError = LengthExceedError
data Bech32DecodeError = InvalidBech32String | InvalidBitPadding

encode :: ConvertUtf8 t ByteString =>
    HumanReadablePart
    -> ByteString
    -> Either Bech32EncodeError t
encode hrp d=
    let encoded = B32.bech32Encode hrp . B32.toBase32 . BS.unpack $ d
    in  decodeUtf8 <$> fromEncoded encoded
  where
    fromEncoded :: Maybe ByteString -> Either Bech32EncodeError ByteString
    fromEncoded (Just bs) = Right bs
    fromEncoded Nothing   = Left LengthExceedError

decode :: ConvertUtf8 t ByteString => t -> Either Bech32DecodeError t
decode t = decodeUtf8 . BS.pack <$> (snd <$> base32FromText t >>= toBase256)

base32FromText :: ConvertUtf8 t ByteString =>
    t -> Either Bech32DecodeError (HumanReadablePart, [B32.Word5])
base32FromText t = case B32.bech32Decode . encodeUtf8 $ t of
    Just b32 -> Right b32
    Nothing  -> Left InvalidBech32String

toBase256 :: [B32.Word5] -> Either Bech32DecodeError [Word8]
toBase256 d = case B32.toBase256 d of
    Just res -> Right res
    Nothing  -> Left InvalidBitPadding
