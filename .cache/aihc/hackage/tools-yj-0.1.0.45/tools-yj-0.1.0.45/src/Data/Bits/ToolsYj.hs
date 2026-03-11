{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Bits.ToolsYj (

	bitsToBits, checkBits, bitsList,

	popBit

	) where

import Control.Arrow
import Data.Bits

checkBits :: Bits bs => bs -> bs -> Bool
checkBits wnt = (== wnt) . (.&. wnt)

bitsList :: FiniteBits bs => bs -> [bs]
bitsList bs =
	filter (/= zeroBits) $ map ((bs .&.) . bit) [0 .. finiteBitSize bs - 1]

bitsToBits :: (Bits a, Bits b) => Int -> a -> b
bitsToBits n b = foldl setBit zeroBits
	. map fst . filter snd $ map (id &&& testBit b) [0 .. n - 1]

popBit :: Bits b => b -> (Bool, b)
popBit n = (n `testBit` 0, n `shiftR` 1)
