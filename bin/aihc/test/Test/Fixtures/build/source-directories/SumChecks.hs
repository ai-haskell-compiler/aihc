{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

module SumChecks (sumChecks) where

import GHC.Exts (Int (I#), Int#, (+#))
import SumProducer (choose, produce)

consume :: Int# -> Int
consume n = case produce n of
  (# (# #) | #) -> 0
  (# | (# first, second #) #) -> I# (first +# second)

selected :: Bool -> a -> Maybe a
selected condition value = case choose condition value of
  (# (# #) | #) -> Nothing
  (# | result #) -> Just result

sumChecks :: Bool
sumChecks = consume 0# == 0
  && consume 20# == 41
  && selected False True == Nothing
  && selected True True == Just True
