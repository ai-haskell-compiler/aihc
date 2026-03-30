{- ORACLE_TEST
id: do-and-if-then-else-then-do-single-stmt
category: layout
expected: pass
-}
{-# LANGUAGE DoAndIfThenElse #-}

-- Test case: 'then do' and 'else do' with single statements at same indent as 'else'.
-- This tests the parse-error rule for closing implicit layouts before 'else'.
module DoAndIfThenElseThenDoSingleStmt where

getCachedJSONQuery :: IO ()
getCachedJSONQuery = do
  if True
    then do
    error "err"
    else do
    error "blah"
