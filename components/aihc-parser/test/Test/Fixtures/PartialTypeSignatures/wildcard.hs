{- ORACLE_TEST
id: wildcard
category: types
expected: xfail
reason: anonymous wildcard type
-}
{-# LANGUAGE PartialTypeSignatures #-}
module Wildcard where

f :: _ -> Int
f x = 42
