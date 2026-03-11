{-# Language CPP, MagicHash #-}

{- |
  Module      :  Codec.Binary.UTF8.Light.Helper
  Copyright   :  (c) Francesco Ariis 2022
  License     :  BSD3
  Maintainer  :  Francesco Ariis <fa-ml@ariis.it>
  Stability   :  provisional
  Portability :  portable

  Lightweight UTF8 handling, helper functions.
-}

module Codec.Binary.UTF8.Light.Helper (
            c2w, w2c, i2w, w2i, cwrd, wh, toW8
) where

import GHC.Exts (Int(I#), Char(C#))
import GHC.Word
  (Word8(W8#), Word32(W32#))
import GHC.Prim
  (Word#, Word32#
  ,ord#,chr#,word2Int#,int2Word#
  ,intToInt32#,int2Word#,int32ToInt#,int32ToWord32#
  ,wordToWord8#,wordToWord32#,word32ToWord#,word32ToInt32#)

-----------------------------------------------------------------------------
-- Helper functions for GHC ≥ 9.2

{-# INLINE w2c #-}
w2c :: Word32 -> Char
w2c (W32# w) = C#(chr#(word2Int#(word32ToWord# w)))

{-# INLINE c2w #-}
c2w :: Char -> Word32
c2w (C# c) = W32#(int32ToWord32#(intToInt32#(ord# c)))

{-# INLINE i2w #-}
i2w :: Int -> Word32
i2w (I# i) = W32#(wordToWord32#(int2Word# i))

{-# INLINE w2i #-}
w2i :: Word32 -> Int
w2i (W32# w) = I#(int32ToInt#(word32ToInt32# w))

{-# INLINE wh #-}
wh :: Word32# -> Word8
wh w = W8#(wordToWord8#(word32ToWord# w))

{-# INLINE toW8 #-}
toW8 :: Word# -> Word8
toW8 w = W8#((wordToWord8# w))

{-# INLINE cwrd #-}
cwrd :: Word32# -> Word#
cwrd w = word32ToWord# w
