{- ORACLE_TEST pass -}
{-# LANGUAGE ImplicitParams #-}
module KeywordNames where

f :: (?case :: Int, ?let :: Int, ?where :: Int, ?do :: Int) => Int
f = ?case + ?let + ?where + ?do
