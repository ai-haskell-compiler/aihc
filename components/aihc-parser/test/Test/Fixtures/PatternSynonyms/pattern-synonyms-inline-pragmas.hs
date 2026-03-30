{- ORACLE_TEST
id: pattern-synonyms-inline-pragmas
category: declarations
expected: xfail
reason: parser support pending
-}
{-# LANGUAGE PatternSynonyms #-}

module PatternSynonymsInlinePragmas where

data Wrapped = Wrapped Int

pattern Wrap :: Int -> Wrapped
pattern Wrap n = Wrapped n

{-# INLINE Wrap #-}
{-# NOINLINE [0] Wrap #-}

build :: Int -> Wrapped
build = Wrap
