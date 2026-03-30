{- ORACLE_TEST
id: unicode-rank-n
category: types
expected: pass
-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}

module UnicodeSyntaxRankN where

-- Rank-N type with Unicode
runST ∷ (∀ s . ST s a) → a
runST = undefined

data ST s a = MkST
