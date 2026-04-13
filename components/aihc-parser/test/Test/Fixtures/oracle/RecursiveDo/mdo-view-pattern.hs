{- ORACLE_TEST pass -}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ViewPatterns #-}

module MDoViewPattern where

f :: a -> a
f (mdo pure x -> y) = y
