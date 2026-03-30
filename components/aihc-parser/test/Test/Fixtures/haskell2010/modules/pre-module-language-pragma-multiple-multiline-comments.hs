{- ORACLE_TEST
id: modules-language-pragma-multiple-multiline-comments
category: modules
expected: pass
-}
{- before first pragma -}
{-# LANGUAGE ForeignFunctionInterface #-}
{- between pragmas -}
{-# LANGUAGE ScopedTypeVariables #-}
{- after second pragma -}
module DemoMultiplePragmasMultiLineComments where
x = 1
