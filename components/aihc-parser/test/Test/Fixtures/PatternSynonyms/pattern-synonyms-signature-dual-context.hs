{- ORACLE_TEST
id: pattern-synonyms-signature-dual-context
category: declarations
expected: xfail
reason: parser support pending
-}
{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsSignatureDualContext where

pattern HeadEq :: Eq a => () => a -> [a]
pattern HeadEq x <- (x : _)

isHead :: Eq a => a -> [a] -> Bool
isHead y (HeadEq x) = x == y
isHead _ [] = False
