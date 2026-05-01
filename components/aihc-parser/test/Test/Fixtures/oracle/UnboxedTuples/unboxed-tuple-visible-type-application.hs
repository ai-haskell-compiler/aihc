{- ORACLE_TEST pass -}
{-# LANGUAGE UnboxedTuples #-}

module UnboxedTupleVisibleTypeApplication where

x = (, ) @(# * | 'C #)
