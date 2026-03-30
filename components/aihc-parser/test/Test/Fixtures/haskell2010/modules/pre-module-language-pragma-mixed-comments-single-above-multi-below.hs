{- ORACLE_TEST
id: modules-language-pragma-mixed-single-above-multi-below
category: modules
expected: pass
-}
-- a single-line comment before pragma
{-# LANGUAGE ForeignFunctionInterface #-}
{- a
   multi-line comment below pragma -}
module DemoMixedSingleAboveMultiBelow where
x = 1
