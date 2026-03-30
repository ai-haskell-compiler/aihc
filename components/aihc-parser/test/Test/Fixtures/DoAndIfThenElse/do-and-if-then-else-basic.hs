{- ORACLE_TEST
id: do-and-if-then-else-basic
category: layout
expected: pass
-}
{-# LANGUAGE DoAndIfThenElse #-}

module DoAndIfThenElseBasic where

choose :: Bool -> Maybe Int
choose cond = do
  if cond
  then pure 1
  else pure 2
