{- ORACLE_TEST
id: do-and-if-then-else-at-layout
category: layout
expected: pass
-}
{-# LANGUAGE DoAndIfThenElse #-}

-- Test case: then and else at column of parent do with no inner do.
-- This should NOT close the outer do layout.
module DoAndIfThenElseThenElseAtLayout where

atLayout :: Bool -> IO ()
atLayout cond = do
  if cond
  then putStrLn "true"
  else putStrLn "false"
  putStrLn "done"
