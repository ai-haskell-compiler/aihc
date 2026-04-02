{- ORACLE_TEST pass -}
{-# LANGUAGE DoAndIfThenElse #-}

-- Test case: nested if-then-else inside 'then do' and 'else do'.
-- Multiple levels of nesting should work correctly.
module DoAndIfThenElseNestedIf where

nestedIf :: Bool -> Bool -> IO ()
nestedIf a b = do
  if a
    then do
      if b
        then do
          putStrLn "a and b"
        else do
          putStrLn "a and not b"
    else do
      putStrLn "not a"