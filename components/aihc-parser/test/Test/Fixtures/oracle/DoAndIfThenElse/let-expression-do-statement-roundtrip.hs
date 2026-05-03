{- ORACLE_TEST pass -}
{-# LANGUAGE Haskell2010 #-}

module LetExpressionDoStatementRoundtrip where

withLocalFunction :: IO ()
withLocalFunction = do
  let
    loop n =
      if n <= 0
        then return ()
        else loop (n - 1)
   in loop 2

withLocalPattern :: IO ()
withLocalPattern = do
  let
    (x, y) = (1 :: Int, 2 :: Int)
    total = x + y
   in print total
