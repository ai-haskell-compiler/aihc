{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Sequence.Word8 (

	-- * FROM/TO BITS

	fromBits, fromBits', toBits, toBits', fromBitsBE', toBitsBE

	) where

import Data.Bits
import Data.Bits.ToolsYj
import Data.Bool
import Data.Word
import Data.Sequence qualified as Seq
import Data.Sequence.ToolsYj qualified as Seq

fromBits :: Bits b => b -> Seq.Seq Word8
fromBits = Seq.unfoldr \b ->
	bool Nothing (Just (bitsToBits 8 b, b `shiftR` 8)) (b /= zeroBits)

fromBits' :: FiniteBits b => b -> Seq.Seq Word8
fromBits' b0 = go (finiteBitSize b0 `div` 8) b0
	where
	go 0 _ = Seq.Empty
	go n b = bitsToBits 8 b `Seq.cons` go (n - 1) (b `shiftR` 8)

toBits :: Bits b => Seq.Seq Word8 -> b
toBits = foldr (\b s -> bitsToBits 8 b .|. s `shiftL` 8) zeroBits

toBits' :: forall b . FiniteBits b => Seq.Seq Word8 -> Maybe b
toBits' bs = bool
	Nothing
	(Just $ foldr (\b s -> bitsToBits 8 b .|. s `shiftL` 8) zeroBits bs)
	(8 * length bs <= finiteBitSize @b undefined)

fromBitsBE' :: FiniteBits b => b -> Seq.Seq Word8
fromBitsBE' b0 = go (finiteBitSize b0 `div` 8) b0
	where
	go 0 _ = Seq.Empty
	go n b = go (n - 1) (b `shiftR` 8) `Seq.snoc` bitsToBits 8 b

toBitsBE :: Bits b => Seq.Seq Word8 -> b
toBitsBE = foldl (\s b -> bitsToBits 8 b .|. s `shiftL` 8) zeroBits
