{- ORACLE_TEST
id: hashable-regress-cpp-have-mmap
category: corpus
expected: pass
reason: from hashable/tests/Regress.hs; covered by parser-test CPP preprocessing
-}
module X where

#ifdef HAVE_MMAP
x :: Int
x = 1
#endif
