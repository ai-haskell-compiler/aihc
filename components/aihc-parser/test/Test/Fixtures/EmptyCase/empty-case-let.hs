{- ORACLE_TEST pass -}
{-# LANGUAGE EmptyCase #-}

module EmptyCaseLet where

data Zero

eliminate :: Zero -> Bool
eliminate x =
  let impossible y = case y of {}
   in impossible x