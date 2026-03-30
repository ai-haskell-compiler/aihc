{- ORACLE_TEST
id: hashable-mix-cpp-if
category: corpus
expected: pass
reason: from hashable/src/Data/Hashable/Mix.hs; parser now accepts this preprocessed case
-}
module X where

#if WORD_SIZE_IN_BITS == 64
x :: Int
x = 64
#else
x :: Int
x = 32
#endif
