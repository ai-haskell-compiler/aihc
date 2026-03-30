{- ORACLE_TEST pass -}
{-# LANGUAGE TupleSections #-}

module TupleSectionLeft where

pairWithOne :: Int -> (Int, Int)
pairWithOne = (1,)