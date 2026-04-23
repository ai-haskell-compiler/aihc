{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
module TuplePatterns where

f = [(,) x () | (,) x () <- []]
