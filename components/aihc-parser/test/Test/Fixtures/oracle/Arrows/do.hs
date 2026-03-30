{- ORACLE_TEST xfail arrow notation with do -}
{-# LANGUAGE Arrows #-}
module Do where

f g h = proc x -> do
  y <- g -< x
  h -< y