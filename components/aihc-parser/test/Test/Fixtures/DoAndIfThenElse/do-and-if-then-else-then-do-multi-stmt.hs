{- ORACLE_TEST
id: do-and-if-then-else-then-do-multi-stmt
category: layout
expected: pass
-}
{-# LANGUAGE DoAndIfThenElse #-}

-- Test case: 'then do' and 'else do' where inner do has multiple statements.
-- The 'else' at column 5 should close the inner do at column 5.
module DoAndIfThenElseThenDoMultiStmt where

multiStmt :: IO ()
multiStmt = do
  if True
    then do
    putStrLn "a"
    putStrLn "b"
    else do
    putStrLn "c"
    putStrLn "d"
