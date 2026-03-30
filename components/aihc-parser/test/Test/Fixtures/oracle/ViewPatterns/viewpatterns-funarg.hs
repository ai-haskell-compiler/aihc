{- ORACLE_TEST pass -}
{-# LANGUAGE ViewPatterns #-}

module ViewPatternsFunArg where

project :: a -> a
project x = x

useView :: a -> a
useView (project -> x) = x