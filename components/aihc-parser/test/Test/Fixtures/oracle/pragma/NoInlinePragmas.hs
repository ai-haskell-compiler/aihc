{- ORACLE_TEST pass -}
module NoInlinePragmas where

neverInline :: Int -> Int
neverInline x = x + 100
{-# NOINLINE neverInline #-}

notInlineSynonym :: Int -> Int
notInlineSynonym x = x + 200
{-# NOTINLINE notInlineSynonym #-}

noinlinePhase :: Int -> Int
noinlinePhase x = x * 3
{-# NOINLINE [1] noinlinePhase #-}

noinlineUntilPhase :: Int -> Int
noinlineUntilPhase x = x * 4
{-# NOINLINE [~1] noinlineUntilPhase #-}

ruleCheap :: Int -> Int
ruleCheap x = x + 1
{-# INLINE CONLIKE ruleCheap #-}
