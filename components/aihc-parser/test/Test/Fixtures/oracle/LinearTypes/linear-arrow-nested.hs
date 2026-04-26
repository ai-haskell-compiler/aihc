{- ORACLE_TEST pass -}
{-# LANGUAGE LinearTypes #-}
module LinearArrowNested where

f :: (a %1 -> b) %1 -> a -> b
f g x = g x
