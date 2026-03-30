{- ORACLE_TEST
id: modules-language-pragma-multiple-singleline-comments
category: modules
expected: pass
-}
-- before first pragma
{-# LANGUAGE ForeignFunctionInterface #-}
-- between pragmas
{-# LANGUAGE ScopedTypeVariables #-}
-- after second pragma
module DemoMultiplePragmasSingleLineComments where
x = 1
