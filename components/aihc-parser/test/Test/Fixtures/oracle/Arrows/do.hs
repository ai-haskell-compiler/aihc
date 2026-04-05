{- ORACLE_TEST pass -}
{-# LANGUAGE Arrows #-}
module Do where

f g h = proc x -> do
  y <- g -< x
  h -< y