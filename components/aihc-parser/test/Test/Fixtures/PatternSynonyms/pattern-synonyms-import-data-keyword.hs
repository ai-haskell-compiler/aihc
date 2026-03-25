{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsImportDataKeyword where

import PatternSynonymsSource (pattern Zero, pattern Succ)

buildZero = Zero
buildOne = Succ Zero
