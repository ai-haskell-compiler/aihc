{- ORACLE_TEST xfail INLINE pragmas not preserved in pretty-printer roundtrip -}
module InlinePragmas where

inlineTop :: Int -> Int
inlineTop x = x + 1
{-# INLINE inlineTop #-}

inlinePhase :: Int -> Int
inlinePhase x = x * 2
{-# INLINE [1] inlinePhase #-}

inlineUntilPhase :: Int -> Int
inlineUntilPhase x = x - 1
{-# INLINE [~1] inlineUntilPhase #-}

localInline :: Int -> Int
localInline x =
  let helper :: Int -> Int
      helper y = y + 10
      {-# INLINE helper #-}
  in helper x

class DefaultInline a where
  opDefault :: a -> a
  opDefault x = x
  {-# INLINE opDefault #-}

newtype Box = Box Int

instance DefaultInline Box where
  {-# INLINE opDefault #-}
  opDefault (Box n) = Box (n + 1)
