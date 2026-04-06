{- ORACLE_TEST pass -}
{-# LANGUAGE PartialTypeSignatures #-}
module QualifiedConstraint where

x :: _ => a -> String
x = show
