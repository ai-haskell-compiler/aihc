{- ORACLE_TEST pass -}
{-# LANGUAGE ParallelListComp #-}
{- ORACLE_TEST pass -}
{-# LANGUAGE ViewPatterns #-}

module ViewPatternsCompParallel where

-- View pattern inside a parallel list comprehension generator.
-- The first | separates the comprehension body from qualifiers;
-- the second | starts a parallel qualifier group.
x e y = [e | (() -> r) <- [e] | x <- y]
