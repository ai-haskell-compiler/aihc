{- ORACLE_TEST xfail mathexpr let-bound signature before rhs -}
module LetBindingSignature where

f = let x :: Double = 1 in x
