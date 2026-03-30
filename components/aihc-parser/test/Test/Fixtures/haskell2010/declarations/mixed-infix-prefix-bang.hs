{- ORACLE_TEST
id: decls-mixed-infix-prefix-bang
category: declarations
expected: pass
-}
{-# LANGUAGE BangPatterns #-}
module MixedInfixPrefixBang where
a ! b = a + b
f !x = x
