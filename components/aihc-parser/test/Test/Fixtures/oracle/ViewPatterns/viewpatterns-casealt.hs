{- ORACLE_TEST pass -}
{-# LANGUAGE ViewPatterns #-}

module ViewPatternsCaseAlt where

project :: a -> a
project x = x

useCase :: a -> a
useCase input =
  case input of
    (project -> x) -> x