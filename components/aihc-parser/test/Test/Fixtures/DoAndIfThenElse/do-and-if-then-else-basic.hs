{- ORACLE_TEST pass -}
{-# LANGUAGE DoAndIfThenElse #-}

module DoAndIfThenElseBasic where

choose :: Bool -> Maybe Int
choose cond = do
  if cond
  then pure 1
  else pure 2