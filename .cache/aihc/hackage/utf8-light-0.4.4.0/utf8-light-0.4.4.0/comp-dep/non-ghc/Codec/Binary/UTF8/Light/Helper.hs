{-# LANGUAGE CPP, MagicHash, TypeSynonymInstances, FlexibleInstances #-}

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

import Data.Word
  (Word,Word8,Word16,Word32)
import Data.Int(Int32)

fi :: (Num b, Integral a) => a -> b
fi = fromIntegral

{-# INLINE w2c #-}
w2c :: Word32 -> Char
w2c = unsafeChr . fromIntegral

{-# INLINE c2w #-}
c2w :: Char -> Word32
c2w = fi . ord

{-# INLINE i2w #-}
i2w :: Int -> Word32
i2w = fi

{-# INLINE w2i #-}
w2i :: Word32 -> Int
w2i = fi

{-# INLINE wh #-}
wh w = W8# w

{-# INLINE toW8 #-}
toW8 w = W8# w

{-# INLINE cwrd #-}
cwrd :: Word# -> Word#
cwrd w = w

