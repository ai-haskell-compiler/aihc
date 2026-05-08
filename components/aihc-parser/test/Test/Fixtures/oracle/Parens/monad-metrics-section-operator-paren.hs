{- ORACLE_TEST xfail monad-metrics-style section application emits extra parens around rhs precedence -}
module ParenSectionExponent where

nsToUs = (/ 10^(3 :: Int))
