{- ORACLE_TEST pass -}
{-# LANGUAGE BangPatterns #-}
module MixedInfixPrefixBang where
a ! b = a + b
f !x = x