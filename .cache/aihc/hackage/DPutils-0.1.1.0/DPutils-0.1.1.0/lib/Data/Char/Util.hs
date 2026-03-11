
-- | Convert between @Word8@ and @Char@. Mostly for attoparsec's bytestring
-- module.

module Data.Char.Util where

import Data.Word (Word8)

c2w8 :: Char -> Word8
c2w8 = fromIntegral . fromEnum
{-# Inline c2w8 #-}

w82c :: Word8 -> Char
w82c = toEnum . fromIntegral
{-# Inline w82c #-}

