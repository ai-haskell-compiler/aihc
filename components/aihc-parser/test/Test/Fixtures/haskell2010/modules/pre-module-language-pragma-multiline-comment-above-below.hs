{- ORACLE_TEST
id: modules-language-pragma-multiline-above-below
category: modules
expected: pass
-}
{- a
   multi-line comment before pragma -}
{-# LANGUAGE ForeignFunctionInterface #-}
{- a
   multi-line comment below pragma -}
module DemoMultiLineAboveBelow where
x = 1
