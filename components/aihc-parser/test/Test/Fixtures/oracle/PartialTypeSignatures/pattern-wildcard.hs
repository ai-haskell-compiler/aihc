{- ORACLE_TEST xfail wildcard in pattern -}
{-# LANGUAGE PartialTypeSignatures #-}
module PatternWildcard where

foo :: _
foo (x :: _) = (x :: _)
