{- ORACLE_TEST pass -}
{-# LANGUAGE Haskell2010 #-}
module ParenReservedAtSection where

(@) :: a -> b -> a
(@) x _ = x

test = (@ ())
