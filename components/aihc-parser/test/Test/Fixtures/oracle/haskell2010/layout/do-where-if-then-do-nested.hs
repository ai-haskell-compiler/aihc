{- ORACLE_TEST pass -}
-- Test: do block with where clause containing nested if-then-do with let and nested if
module DoWhereIfThenDoNested where

f = do
    return ()
  where
    g = do
        if not True then do
            s <- return "hello"
            let ls' = s : undefined
            if s == "x" then do
                return ()
            else
                return ()
        else
            return ()
    h = 1
    i = 2