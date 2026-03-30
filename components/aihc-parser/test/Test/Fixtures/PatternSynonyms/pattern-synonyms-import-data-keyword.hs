{- ORACLE_TEST
id: pattern-synonyms-import-data-keyword
category: modules
expected: xfail
reason: parser support pending
-}
{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsImportDataKeyword where

import PatternSynonymsSource (pattern Zero, pattern Succ)

buildZero = Zero
buildOne = Succ Zero
