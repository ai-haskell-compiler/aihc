{- ORACLE_TEST pass -}
{-# LANGUAGE PartialTypeSignatures #-}
module PatternWildcard where

foo :: _
foo (x :: _) = (x :: _)
