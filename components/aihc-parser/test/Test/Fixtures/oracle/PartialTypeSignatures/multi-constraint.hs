{- ORACLE_TEST xfail wildcard in multi-constraint -}
{-# LANGUAGE PartialTypeSignatures #-}
module MultiConstraint where

x :: (Enum a, _) => a -> String
x = show
