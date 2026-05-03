{- ORACLE_TEST pass -}
{-# LANGUAGE EmptyCase #-}

module EmptyCaseImplicitEof where

data Void

absurdAfterAlt :: Bool -> Void -> a
absurdAfterAlt b v = case b of True -> case v of
