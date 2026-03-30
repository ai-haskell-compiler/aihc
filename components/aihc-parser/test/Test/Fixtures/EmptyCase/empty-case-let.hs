{- ORACLE_TEST
id: empty-case-let
category: expressions
expected: pass
reason: parser now supports empty case alternatives
-}
{-# LANGUAGE EmptyCase #-}

module EmptyCaseLet where

data Zero

eliminate :: Zero -> Bool
eliminate x =
  let impossible y = case y of {}
   in impossible x
