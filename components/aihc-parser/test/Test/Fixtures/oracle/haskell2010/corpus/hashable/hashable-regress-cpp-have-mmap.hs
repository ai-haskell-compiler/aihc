{- ORACLE_TEST pass -}
{-# LANGUAGE CPP #-}
module X where

#ifdef HAVE_MMAP
x :: Int
x = 1
#endif