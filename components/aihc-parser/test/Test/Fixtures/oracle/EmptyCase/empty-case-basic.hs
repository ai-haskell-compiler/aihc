{- ORACLE_TEST pass -}
{-# LANGUAGE EmptyCase #-}

module EmptyCaseBasic where

data Void

absurd :: Void -> a
absurd v = case v of {}