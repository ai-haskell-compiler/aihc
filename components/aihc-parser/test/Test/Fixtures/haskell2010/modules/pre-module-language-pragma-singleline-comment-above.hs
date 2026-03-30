{- ORACLE_TEST
id: modules-language-pragma-singleline-above
category: modules
expected: pass
-}
-- a single-line comment before pragma
{-# LANGUAGE ForeignFunctionInterface #-}
module DemoSingleLineAbove where
x = 1
