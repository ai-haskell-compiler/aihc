{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}

module AttoparsecExpr where

rassocP :: Maybe Int
rassocP =
  do
    f <- Just 1
    y <- do
      z <- Just 2
      Just (f + z)
    pure (f + y)
    <|> Just 0
