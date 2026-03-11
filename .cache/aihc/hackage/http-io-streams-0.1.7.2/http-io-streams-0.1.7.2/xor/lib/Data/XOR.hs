{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: © 2020  Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- Compat-layer providing (slow) fallbacks when compiled w/o
-- https://hackage.haskell.org/package/xor
--
module Data.XOR
    ( xor32StrictByteString'
    , xor32LazyByteString
    ) where

import           Data.Bits                     (rotateL, xor)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BL (ByteString (..))
import           Data.Word                     (Word32, Word8)

xor32StrictByteString' :: Word32 -> ByteString -> (Word32,ByteString)
xor32StrictByteString' 0    = (,) 0
xor32StrictByteString' msk0 = BS.mapAccumL go msk0
  where
    go :: Word32 -> Word8 -> (Word32,Word8)
    go msk b = let b'   = fromIntegral msk' `xor` b
                   msk' = rotateL msk 8
               in b' `seq` (msk',b')

xor32LazyByteString :: Word32 -> BL.ByteString -> BL.ByteString
xor32LazyByteString 0    = id
xor32LazyByteString msk0 = go msk0
  where
    go _ BL.Empty          = BL.Empty
    go msk (BL.Chunk x xs) = BL.Chunk x' (go msk' xs)
      where
        (msk',x') = xor32StrictByteString' msk x
