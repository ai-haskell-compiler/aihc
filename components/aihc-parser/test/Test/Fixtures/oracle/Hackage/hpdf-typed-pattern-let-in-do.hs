{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}
module HpdfTypedPatternLetInDo where

f xs = do
  let [a, b] :: [Double] = xs
      total = a + b
   in pure total
