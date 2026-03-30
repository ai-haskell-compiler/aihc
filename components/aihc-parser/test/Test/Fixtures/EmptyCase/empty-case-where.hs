{- ORACLE_TEST
id: empty-case-where
category: expressions
expected: pass
reason: parser now supports empty case alternatives
-}
{-# LANGUAGE EmptyCase #-}

module EmptyCaseWhere where

data Never

consume :: Never -> Int
consume x =
  whereImpossible x
  where
    whereImpossible y = case y of {}
