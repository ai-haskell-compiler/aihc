{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}

module UnicodeSyntaxRankN where

-- Rank-N type with Unicode
runST ∷ (∀ s . ST s a) → a
runST = undefined

data ST s a = MkST
