module Loot.Crypto.Bech32
    (     DecodeError (..)
        , EncodeError (..)
        , HumanReadablePart
        , encode
        , decode
    )
    where

import Universum

import Codec.Binary.Bech32 (DecodeError (..), EncodeError (..), bech32Decode, bech32Encode,
                            fromWord5, word5)

import qualified Data.ByteString as BS

type HumanReadablePart = ByteString

encode :: ConvertUtf8 t ByteString =>
    HumanReadablePart
    -> ByteString
    -> Either EncodeError t
encode hrp = fmap decodeUtf8 . bech32Encode hrp . fmap word5 . BS.unpack

decode :: ConvertUtf8 t ByteString => t -> Either DecodeError (HumanReadablePart, ByteString)
decode =  fmap (fmap (BS.pack . (fmap fromWord5))). bech32Decode . encodeUtf8
