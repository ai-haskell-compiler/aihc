{- ORACLE_TEST pass -}
{-# LANGUAGE PartialTypeSignatures #-}
module MultiConstraint where

x :: (Enum a, _) => a -> String
x = show
