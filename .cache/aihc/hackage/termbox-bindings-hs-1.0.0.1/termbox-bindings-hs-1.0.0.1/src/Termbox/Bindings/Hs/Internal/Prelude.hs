module Termbox.Bindings.Hs.Internal.Prelude
  ( charToWord32,
    cintToInt,
    intToCInt,
    word32ToChar,
  )
where

import qualified Data.Char as Char
import Data.Word (Word32)
import Foreign.C.Types (CInt)

charToWord32 :: Char -> Word32
charToWord32 =
  fromIntegral @Int @Word32 . Char.ord
{-# INLINE charToWord32 #-}

word32ToChar :: Word32 -> Char
word32ToChar =
  Char.chr . fromIntegral @Word32 @Int
{-# INLINE word32ToChar #-}

cintToInt :: CInt -> Int
cintToInt =
  fromIntegral
{-# INLINE cintToInt #-}

intToCInt :: Int -> CInt
intToCInt =
  fromIntegral
{-# INLINE intToCInt #-}
