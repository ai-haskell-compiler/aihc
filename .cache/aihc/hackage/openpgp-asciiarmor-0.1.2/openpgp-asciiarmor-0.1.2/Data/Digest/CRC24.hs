-- CRC24.hs: OpenPGP (RFC4880) CRC24 implementation
-- Copyright © 2012-2019 Clint Adams
-- This software is released under the terms of the Expat license.
-- (See the LICENSE file).

module Data.Digest.CRC24 (
   crc24
 , crc24Lazy
) where

import Data.Bits (shiftL, (.&.), xor)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8, Word32)

crc24Init :: Word32
crc24Init = 0xB704CE

crc24Poly :: Word32
crc24Poly = 0x1864CFB

crc24Update :: Word32 -> Word8 -> Word32
crc24Update c b = (last . take 9 $ iterate (\x -> if shiftL x 1 .&. 0x1000000 == 0x1000000 then shiftL x 1 `xor` crc24Poly else shiftL x 1) (c `xor` shiftL (fromIntegral b) 16)) .&. 0xFFFFFF

crc24 :: B.ByteString -> Word32
crc24 bs = crc24Lazy . BL.fromChunks $ [bs]

crc24Lazy :: ByteString -> Word32
crc24Lazy = BL.foldl' crc24Update crc24Init
