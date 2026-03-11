module Bytezap.Poke.Derived where

import Bytezap.Poke

import Data.ByteString.Short qualified as SBS
import Data.Text.Internal qualified as T
--import Data.Array.Byte qualified as A
-- ^ text-2.1 and above
import Data.Text.Array qualified as A
import Data.Word
import GHC.Int
import Data.Char ( ord )
import Data.Bits ( shiftR, (.&.) )
import GHC.Exts ( sizeofByteArray# )

-- | Poke a 'SBS.ShortByteString'.
shortByteString :: SBS.ShortByteString -> Poke s
shortByteString (SBS.SBS ba#) = byteArray# ba# 0# (sizeofByteArray# ba#)

-- | Poke a 'T.Text'.
text :: T.Text -> Poke s
text (T.Text (A.ByteArray ba#) (I# os#) (I# len#)) = byteArray# ba# os# len#

-- | Poke a 'Char'.
--
-- Adapted from utf8-string.
char :: Char -> Poke s
char = go . ord
 where
  w8 :: Word8 -> Poke s
  w8 = prim
  go oc
   | oc <= 0x7f       = w8 $ fromIntegral oc

   | oc <= 0x7ff      =    w8 (fromIntegral (0xc0 + (oc `shiftR` 6)))
                        <> w8 (fromIntegral (0x80 + oc .&. 0x3f))

   | oc <= 0xffff     =    w8 (fromIntegral (0xe0 + (oc `shiftR` 12)))
                        <> w8 (fromIntegral (0x80 + ((oc `shiftR` 6) .&. 0x3f)))
                        <> w8 (fromIntegral (0x80 + oc .&. 0x3f))
   | otherwise        =    w8 (fromIntegral (0xf0 + (oc `shiftR` 18)))
                        <> w8 (fromIntegral (0x80 + ((oc `shiftR` 12) .&. 0x3f)))
                        <> w8 (fromIntegral (0x80 + ((oc `shiftR` 6) .&. 0x3f)))
                        <> w8 (fromIntegral (0x80 + oc .&. 0x3f))

-- v TODO maybe not needed any more thanks to removing Pokeable class
-- | @unsafePokeIndexed pokeAt off n@ performs @n@ indexed pokes starting from
--   @off@.
--
-- Does not check bounds. Largely intended for bytewise pokes where some work
-- needs to be performed for each byte (e.g. escaping text and poking inline).
unsafePokeIndexed :: (Int -> Poke s) -> Int -> Int -> Poke s
-- no tail recursive option since it'd require binding a ptr, which we can't do
-- due to TYPE rr.
-- if you simply expand the monoidal ops here, you get the tail call ver. but
-- like, will GHC be able to do that?
-- hoping INLINE is good enough. but maybe need to check strictness.
unsafePokeIndexed pokeAt off n = go off
  where
    go i | i >= iend = mempty
         | otherwise = pokeAt i <> go (i+1)
    iend = off + n
