{- ORACLE_TEST
id: modules-language-pragma-multiline-above
category: modules
expected: pass
-}
{- a
   multi-line comment before pragma -}
{-# LANGUAGE ForeignFunctionInterface #-}
module DemoMultiLineAbove where
x = 1
