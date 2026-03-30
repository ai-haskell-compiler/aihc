{- ORACLE_TEST
id: promoted-tuple
category: types
expected: pass
-}
{-# LANGUAGE DataKinds #-}
module PromotedTuple where

type T = '(Int, String)
