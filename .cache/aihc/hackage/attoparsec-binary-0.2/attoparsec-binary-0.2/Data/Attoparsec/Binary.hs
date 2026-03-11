-- |Binary processing extensions to Attoparsec.
module Data.Attoparsec.Binary
       (
         anyWord16be
       , anyWord16le
       , anyWord32be
       , anyWord32le
       , anyWord64be
       , anyWord64le
       , word16be
       , word16le
       , word32be
       , word32le
       , word64be
       , word64le
       ) where

import Data.Attoparsec
import Data.Bits
import qualified Data.ByteString as B
import Data.Word

byteSize :: (Bits a) => a -> Int
byteSize = (`div` 8) . bitSize

pack :: (Bits a, Num a) => B.ByteString -> a
pack = B.foldl' (\n h -> (n `shiftL` 8) .|. fromIntegral h) 0

anyWordN :: (Bits a) => (B.ByteString -> a) -> Parser a
anyWordN = anyWordN' undefined
  where anyWordN' :: (Bits a) => a -> (B.ByteString -> a) -> Parser a
        anyWordN' d = flip fmap $ Data.Attoparsec.take $ byteSize d

-- |Match any 16-bit big-endian word.
anyWord16be :: Parser Word16
anyWord16be = anyWordN pack

-- |Match any 16-bit little-endian word.
anyWord16le :: Parser Word16
anyWord16le = anyWordN $ pack . B.reverse

-- |Match any 32-bit big-endian word.
anyWord32be :: Parser Word32
anyWord32be = anyWordN pack

-- |Match any 32-bit little-endian word.
anyWord32le :: Parser Word32
anyWord32le = anyWordN $ pack . B.reverse

-- |Match any 64-bit big-endian word.
anyWord64be :: Parser Word64
anyWord64be = anyWordN pack

-- |Match any 64-bit little-endian word.
anyWord64le :: Parser Word64
anyWord64le = anyWordN $ pack . B.reverse

unpack :: (Bits a, Integral a) => a -> B.ByteString
unpack x = B.pack $ map f $ reverse [0..byteSize x - 1]
  where f s = fromIntegral $ shiftR x (8 * s)

wordN :: (Bits a) => (a -> B.ByteString) -> a -> Parser a
wordN u w = string (u w) >> return w

-- |Match a specific 16-bit big-endian word.
word16be :: Word16 -> Parser Word16
word16be = wordN unpack

-- |Match a specific 16-bit little-endian word.
word16le :: Word16 -> Parser Word16
word16le = wordN $ B.reverse . unpack

-- |Match a specific 32-bit big-endian word.
word32be :: Word32 -> Parser Word32
word32be = wordN unpack

-- |Match a specific 32-bit little-endian word.
word32le :: Word32 -> Parser Word32
word32le = wordN $ B.reverse . unpack

-- |Match a specific 64-bit big-endian word.
word64be :: Word64 -> Parser Word64
word64be = wordN unpack

-- |Match a specific 64-bit little-endian word.
word64le :: Word64 -> Parser Word64
word64le = wordN $ B.reverse . unpack
