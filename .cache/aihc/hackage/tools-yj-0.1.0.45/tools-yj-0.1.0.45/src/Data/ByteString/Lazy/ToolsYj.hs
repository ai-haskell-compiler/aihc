{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.ByteString.Lazy.ToolsYj (

	-- * FROM/TO BITS

	fromBits, fromBits', toBits, toBits',

	-- * FROM/TO BITS -- BIG ENDIEN

	fromBitsBE', toBitsBE,

	-- * SPLIT AT

	splitAt'

	) where

import Data.Bits
import Data.Bits.ToolsYj
import Data.Bool
import Data.Int
import Data.ByteString.Lazy qualified as LBS

fromBits :: Bits b => b -> LBS.ByteString
fromBits = LBS.unfoldr \b ->
	bool Nothing (Just (bitsToBits 8 b, b `shiftR` 8)) (b /= zeroBits)

fromBits' :: FiniteBits b => b -> LBS.ByteString
fromBits' b0 = go (finiteBitSize b0 `div` 8) b0
	where
	go 0 _ = ""
	go n b = bitsToBits 8 b `LBS.cons` go (n - 1) (b `shiftR` 8)

toBits :: Bits b => LBS.ByteString -> b
toBits = LBS.foldr (\b s -> bitsToBits 8 b .|. s `shiftL` 8) zeroBits

toBits' :: forall b . FiniteBits b => LBS.ByteString -> Maybe b
toBits' bs = bool
	Nothing
	(Just $ LBS.foldr (\b s -> bitsToBits 8 b .|. s `shiftL` 8) zeroBits bs)
	(8 * LBS.length bs <= fromIntegral (finiteBitSize @b undefined))

toBitsBE :: Bits b => LBS.ByteString -> b
toBitsBE = LBS.foldl (\s b -> bitsToBits 8 b .|. s `shiftL` 8) zeroBits

fromBitsBE' :: FiniteBits b => b -> LBS.ByteString
fromBitsBE' b0 = go (finiteBitSize b0 `div` 8) b0
	where
	go 0 _ = ""
	go n b = go (n - 1) (b `shiftR` 8) `LBS.snoc` bitsToBits 8 b

splitAt' :: Int64 -> LBS.ByteString -> Maybe (LBS.ByteString, LBS.ByteString)
splitAt' n bs
	| LBS.length bs < n = Nothing
	| otherwise = Just $ LBS.splitAt n bs
