{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Word.Word8 where

import Data.MonoTraversable
import Data.Bits
import Data.Bits.ToolsYj
import Data.Bool
import Data.Word

toBits :: (MonoFoldable mono, Element mono ~ Word8, Bits b) => mono -> b
toBits = ofoldr (\b s -> bitsToBits 8 b .|. s `shiftL` 8) zeroBits

toBits' :: forall b mono .
	(MonoFoldable mono, Element mono ~ Word8, FiniteBits b) =>
	mono -> Maybe b
toBits' bs = bool
	Nothing
	(Just $ ofoldr (\b s -> bitsToBits 8 b .|. s `shiftL` 8) zeroBits bs)
	(8 * olength bs <= finiteBitSize @b undefined)

toBitsBE :: (MonoFoldable mono, Element mono ~ Word8, Bits b) => mono -> b
toBitsBE = ofoldl' (\s b -> bitsToBits 8 b .|. s `shiftL` 8) zeroBits
