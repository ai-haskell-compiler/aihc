{- ORACLE_TEST
id: modules-language-pragma-mixed-multi-above-single-below
category: modules
expected: pass
-}
{- a
   multi-line comment before pragma -}
{-# LANGUAGE ForeignFunctionInterface #-}
-- a single-line comment below pragma
module DemoMixedMultiAboveSingleBelow where
x = 1
