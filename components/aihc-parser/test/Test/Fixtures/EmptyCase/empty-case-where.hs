{-# LANGUAGE EmptyCase #-}

module EmptyCaseWhere where

data Never

consume :: Never -> Int
consume x =
  whereImpossible x
  where
    whereImpossible y = case y of {}
