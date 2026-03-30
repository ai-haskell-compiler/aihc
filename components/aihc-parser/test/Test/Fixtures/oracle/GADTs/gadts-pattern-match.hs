{- ORACLE_TEST pass -}
{-# LANGUAGE GADTs #-}

module GADTSPatternMatch where

data Expr a where
  EInt :: Int -> Expr Int
  EAdd :: Expr Int -> Expr Int -> Expr Int

eval :: Expr Int -> Int
eval e = case e of
  EInt n -> n
  EAdd l r -> eval l + eval r