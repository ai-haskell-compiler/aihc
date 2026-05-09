{- ORACLE_TEST pass -}
module RatioIntParenthesizedCompare where

enumFromTo n m = takeWhile (<= m + 1 / 2) (enumFrom n)
