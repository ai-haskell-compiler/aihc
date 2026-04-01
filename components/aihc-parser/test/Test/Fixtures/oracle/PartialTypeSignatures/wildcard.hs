{- ORACLE_TEST pass -}
{-# LANGUAGE PartialTypeSignatures #-}
module Wildcard where

f :: _ -> Int
f x = 42