{- ORACLE_TEST
id: modules-language-pragma-singleline-above-below
category: modules
expected: pass
-}
-- a single-line comment before pragma
{-# LANGUAGE ForeignFunctionInterface #-}
-- a single-line comment below pragma
module DemoSingleLineAboveBelow where
x = 1
