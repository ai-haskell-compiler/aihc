{- ORACLE_TEST pass -}
{-# LANGUAGE NondecreasingIndentation #-}

-- NondecreasingIndentation: simple nested do at same indentation level.

module NondecreasingNestedDoSimple where

f = do
  do
  action
  where
    action = undefined
