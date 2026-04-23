{- ORACLE_TEST xfail NonEmpty pattern associativity mismatch -}
{-# LANGUAGE GHC2021 #-}
module NonEmptyPattern where

import Data.List.NonEmpty (NonEmpty(..))

f (x :| y : ys) = undefined
f _ = undefined
