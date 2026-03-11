{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import GHC.TypeNats
import Data.Foldable
import Data.List.Length
import Data.List.Range
import Data.Bits
import Data.Word
import Numeric

takeL :: (LoosenLMax 0 (n - 1) n, Unfoldr 0 n n, ListToLengthL n) =>
	a -> [a] -> LengthL n a
takeL d = either ((`fillL` d) . loosenLMax) fst . splitL

data Bit = O | I deriving Show

boolToBit :: Bool -> Bit
boolToBit = \case False -> O; True -> I

bitToNum63 :: (Num n, Bits n) => Bit -> n
bitToNum63 = \case O -> 0; I -> 1 `shiftL` 63

bitsToWord64 :: LengthL 64 Bit -> Word64
bitsToWord64 = foldl' (\w b -> w `shiftR` 1 .|. bitToNum63 b) 0

takeWord64 :: String -> Word64
takeWord64 = bitsToWord64 . takeL O . (boolToBit . (== '*') <$>)

main :: IO ()
main = do
	putStrLn $ takeWord64 sample1 `showHex` ""
	putStrLn $ takeWord64 sample2 `showHex` ""

sample1, sample2 :: String
sample1 = "...*..*..*...........*...**********...*************............******"
sample2 = "...*..*..*...........*.."
