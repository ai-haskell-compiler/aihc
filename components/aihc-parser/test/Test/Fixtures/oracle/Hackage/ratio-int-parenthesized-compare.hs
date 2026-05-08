{- ORACLE_TEST xfail Guard roundtrip due to unnecessary parentheses in rhs of <= -}
module RatioIntParenthesizedCompare where

enumFromTo n m = takeWhile (<= m + 1 / 2) (enumFrom n)

