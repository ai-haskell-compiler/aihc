{- ORACLE_TEST
id: modules-language-pragma-multiline-below
category: modules
expected: pass
-}
{-# LANGUAGE ForeignFunctionInterface #-}
{- a
   multi-line comment below pragma -}
module DemoMultiLineBelow where
x = 1
