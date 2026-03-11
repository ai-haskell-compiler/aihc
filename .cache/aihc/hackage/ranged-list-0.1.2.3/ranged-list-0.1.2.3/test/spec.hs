{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Data.Bits
import Data.Word

import Data.List.Length
import Data.List.Range

main :: IO ()
main = do
	print $ foo 123; putStrLn ""
	print $ bar 123; putStrLn ""
	print baz; putStrLn ""
	print hoge; putStrLn ""

foo :: Word32 -> LengthL 32 Bool
foo = unfoldr \w -> (w `testBit` 0, w `shiftR` 1)

bar :: Word32 -> LengthR 32 Bool
bar = unfoldl \w -> (w `shiftR` 1, w `testBit` 0)

baz :: (RangeL 2 3 (Integer, Integer), RangeL 1 6 Integer)
baz = zipWithL (,)
	(1 :. 2 :. 3 :.. NilL :: RangeL 2 3 Integer)
	(1 :. 2 :. 3 :. 4 :. 5 :.. NilL :: RangeL 4 8 Integer)

hoge :: (RangeR 1 6 Integer, RangeR 2 3 (Integer, Integer))
hoge = zipWithR (,)
	(NilR :++ 1 :+ 2 :+ 3 :+ 4 :+ 5 :: RangeR 4 8 Integer)
	(NilR :++ 1 :+ 2 :+ 3 :: RangeR 2 3 Integer)
