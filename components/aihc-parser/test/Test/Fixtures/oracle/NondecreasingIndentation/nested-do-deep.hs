{- ORACLE_TEST pass -}
{-# LANGUAGE NondecreasingIndentation #-}

-- NondecreasingIndentation: deeply nested do blocks at the same indentation.

module NondecreasingDeepNestedDo where

f = do
  do
    do
      do
      action
  where
    action = undefined
