{- ORACLE_TEST
id: hashable-lowlevel-cpp-ifdef
category: corpus
expected: pass
reason: from hashable/src/Data/Hashable/LowLevel.hs; parser now accepts this preprocessed case
-}
module X where

#ifdef HASHABLE_RANDOM_SEED
x :: Int
x = 1
#else
x :: Int
x = 2
#endif
