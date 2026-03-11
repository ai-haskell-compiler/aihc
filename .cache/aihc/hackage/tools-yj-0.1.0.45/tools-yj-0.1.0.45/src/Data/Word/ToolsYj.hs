{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Word.ToolsYj (

	Word4

	) where

import Control.Arrow
import Data.Bits
import Data.Word

newtype Word4 = Word4_ { unWord4 :: Word8 }

instance Show Word4 where
	showsPrec p x = showsPrec p (fromIntegral x :: Int)

instance Read Word4 where
	readsPrec p s = [(fromIntegral (x :: Int), r) | (x, r) <- readsPrec p s]

instance Bits Word4 where
	Word4_ w1 .&. Word4_ w2 = Word4_ $ w1 .&. w2
	Word4_ w1 .|. Word4_ w2 = Word4_ $ w1 .|. w2
	Word4_ w1 `xor` Word4_ w2 = Word4_ $ w1 `xor` w2
	complement = Word4_ . (.&. 0xf) . complement . unWord4
	shift (Word4_ w) i = Word4_ . (.&. 0xf) $ shift w i
	rotate (Word4_ w) i = Word4_ $ shiftR w' 4 .|. (w' .&. 0xf)
		where
		w' = (`rotate` i) $ (.&. 0xf) w
	bitSize _ = 4; bitSizeMaybe _ = Just 4; isSigned _ = False
	Word4_ w `testBit` i = w `testBit` i
	bit i = Word4_ . (.&. 0xf) $ bit i
	popCount (Word4_ w) = popCount $ (.&. 0xf) w

instance FiniteBits Word4 where
	finiteBitSize _ = 4

instance Bounded Word4 where minBound = 0; maxBound = 0xf

instance Enum Word4 where
	toEnum i
		| 0 <= i && i <= 0xf = fromIntegral i
		| otherwise = error $
			"Enum.toEnum{Word4}: tag " ++
			show i ++ " is outside of bounds (0,15)"
	fromEnum = fromIntegral

instance Num Word4 where
	Word4_ w1 + Word4_ w2 = Word4_ . (.&. 0xf) $ w1 + w2
	Word4_ w1 * Word4_ w2 = Word4_ . (.&. 0xf) $ w1 * w2
	abs = id
	signum _ = 1
	negate = Word4_ . (.&. 0xf) . negate . unWord4
	fromInteger = Word4_ . (.&. 0xf) . fromInteger

instance Real Word4 where
	toRational = toRational . (.&. 0xf) . unWord4

instance Integral Word4 where
	toInteger = toInteger . (.&. 0xf) . unWord4
	Word4_ w1 `quotRem` Word4_ w2 =
		(Word4_ . (.&. 0xf) *** Word4_ . (.&. 0xf)) $ w1 `quotRem` w2

instance Eq Word4 where Word4_ w1 == Word4_ w2 = (w1 .&. 0xf) == (w2 .&. 0xf)
instance Ord Word4 where Word4_ w1 <= Word4_ w2 = (.&. 0xf) w1 <= (.&. 0xf) w2
