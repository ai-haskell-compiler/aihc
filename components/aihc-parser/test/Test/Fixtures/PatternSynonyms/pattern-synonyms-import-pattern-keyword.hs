{- ORACLE_TEST
id: pattern-synonyms-import-pattern-keyword
category: modules
expected: xfail
reason: parser support pending
-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}

module PatternSynonymsImportPatternKeyword where

import PatternSynonymsSource (pattern Zero, pattern Succ)

buildZero = Zero
buildOne = Succ Zero
