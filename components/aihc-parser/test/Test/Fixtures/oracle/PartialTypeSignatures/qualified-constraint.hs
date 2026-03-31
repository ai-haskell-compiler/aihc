{- ORACLE_TEST xfail wildcard in qualified constraint -}
{-# LANGUAGE PartialTypeSignatures #-}
module QualifiedConstraint where

x :: _ => a -> String
x = show
