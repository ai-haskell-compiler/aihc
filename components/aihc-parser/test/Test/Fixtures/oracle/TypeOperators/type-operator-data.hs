{- ORACLE_TEST pass -}
{-# LANGUAGE GHC2021 #-}

module TypeOperatorData where

data (f :+: g) e = Left' (f e) | Right' (g e)
