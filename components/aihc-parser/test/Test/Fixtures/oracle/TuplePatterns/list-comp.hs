{- ORACLE_TEST xfail tuple constructor pattern in list comprehension generator -}
{-# LANGUAGE GHC2021 #-}
module TuplePatterns where

f = [(,) x () | (,) x () <- []]
