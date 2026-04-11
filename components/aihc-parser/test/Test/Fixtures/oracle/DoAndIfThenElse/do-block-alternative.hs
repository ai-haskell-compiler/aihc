{- ORACLE_TEST xfail do-block infix continuation with <|> operator -}
{-# LANGUAGE GHC2021 #-}
module AttoparsecExpr where

rassocP :: Maybe Int
rassocP = do
  f <- Just 1
  y <- do
    z <- Just 2
    Just (f + z)
  pure (f + y)
  <|> Just 0
