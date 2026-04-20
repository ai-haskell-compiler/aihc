{- ORACLE_TEST pass -}
{-# LANGUAGE DoAndIfThenElse #-}

-- Test case: multiline inner 'if-then-else' as the first statement in an
-- 'else do' layout block. The outer else-do layout must not close on the
-- inner branch markers before the statement finishes.
module DoAndIfThenElseElseDoMultilineInnerIf where

fn =
  if False then
    return True
  else do
      if hidden /= 0 then
        return True
      else
        return False
