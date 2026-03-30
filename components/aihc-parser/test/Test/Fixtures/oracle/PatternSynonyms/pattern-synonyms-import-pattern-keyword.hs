{- ORACLE_TEST xfail parser support pending -}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ExplicitNamespaces #-}

module PatternSynonymsImportPatternKeyword where

import PatternSynonymsSource (pattern Zero, pattern Succ)

buildZero = Zero
buildOne = Succ Zero