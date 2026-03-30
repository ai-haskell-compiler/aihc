{- ORACLE_TEST pass -}
module DTypeSigCtxMultiline where

memo :: (Ord a)
     => (a -> b)
     -> (a -> b)
memo f = f