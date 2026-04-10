{- ORACLE_TEST pass -}
module SpecializeImportedUse where

import SpecializeImportedDef (lookupLike)

data Tag = TagA | TagB deriving (Eq)

{-# SPECIALIZE lookupLike :: [(Tag, b)] -> Tag -> Maybe b #-}

findTagA :: [(Tag, b)] -> Maybe b
findTagA xs = lookupLike xs TagA
