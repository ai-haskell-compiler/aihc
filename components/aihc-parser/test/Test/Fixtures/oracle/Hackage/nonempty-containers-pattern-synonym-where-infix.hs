{- ORACLE_TEST pass -}
{-# LANGUAGE PatternSynonyms #-}
module A where

data T a = a :<| a

pattern x :> y <- x :<| y
  where
    (a :<| b) :> c = a :<| (b :<| c)
