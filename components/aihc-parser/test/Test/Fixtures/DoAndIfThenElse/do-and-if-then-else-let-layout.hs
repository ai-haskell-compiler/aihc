{-# LANGUAGE DoAndIfThenElse #-}

-- Test case: 'then do' followed by 'let' expression, then 'else do'.
-- The 'let' block at same column as 'then' should be inside then-do.
-- The 'else' at same column should close then-do and let before else-do.
module DoAndIfThenElseLetLayout where

withLet :: IO ()
withLet = do
  if True
    then do
    let x = 1
        y = 2
    return (x + y)
    else do
    return 0
