{- ORACLE_TEST xfail anonymous wildcard type -}
{-# LANGUAGE PartialTypeSignatures #-}
module Wildcard where

f :: _ -> Int
f x = 42