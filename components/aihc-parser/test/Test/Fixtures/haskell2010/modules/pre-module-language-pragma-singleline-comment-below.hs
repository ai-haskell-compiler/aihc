{- ORACLE_TEST
id: modules-language-pragma-singleline-below
category: modules
expected: pass
-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- a single-line comment below pragma
module DemoSingleLineBelow where
x = 1
