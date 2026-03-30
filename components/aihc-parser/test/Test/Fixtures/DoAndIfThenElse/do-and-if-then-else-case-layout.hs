{- ORACLE_TEST
id: do-and-if-then-else-case-layout
category: layout
expected: pass
-}
{-# LANGUAGE DoAndIfThenElse #-}

-- Test case: 'then do' followed by 'case' expression, then 'else do'.
-- The 'case' block at same column as 'then' should be inside then-do.
-- The 'else' at same column should close then-do and case before else-do.
module DoAndIfThenElseCaseLayout where

toCaseLayout :: IO ()
toCaseLayout = do
  if True
    then do
    case undefined of
      Left err -> error err
      Right obj -> return obj
    else do
    return ()
