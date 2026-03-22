{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsImportPatternKeyword where

import PatternSynonymsSource (pattern Succ, pattern Zero)

buildZero = Zero

buildOne = Succ Zero
