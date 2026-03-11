{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Word.Crc32 (

	C, fromWord, toWord, step, initial, complement

	) where

import Control.Arrow
import Data.Bits hiding (complement)
import Data.Bits qualified as Bits
import Data.Bits.ToolsYj
import Data.Array
import Data.Bool
import Data.Word

newtype C = C { unC :: Word32 }
	deriving (Show, Eq, Bits, FiniteBits)

fromWord :: Word32 -> C
fromWord = C

toWord :: C -> Word32
toWord = unC

crc1 :: Word32 -> Word32
crc1 = uncurry (bool id (`xor` 0xedb88320)) . popBit

crc8 :: Word8 -> Word32
crc8 n = iterate crc1 (fromIntegral n) !! 8

table :: Array Word8 Word32
table = listArray (0, 255) $ map crc8 [0 .. 255]

popByte :: (Integral a, Bits a) => a -> (Word8, a)
popByte n = (fromIntegral n, n `shiftR` 8)

step :: C -> Word8 -> C
step (C n) b = C . uncurry xor . (first $ (table !) . (`xor` b)) $ popByte n

initial :: C
initial = C 0xffffffff

complement :: C -> C
complement = C . Bits.complement . unC
