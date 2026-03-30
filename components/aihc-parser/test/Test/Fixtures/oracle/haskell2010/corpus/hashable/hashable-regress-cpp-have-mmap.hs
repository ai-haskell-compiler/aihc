{- ORACLE_TEST pass -}
module X where

#ifdef HAVE_MMAP
x :: Int
x = 1
#endif