{- ORACLE_TEST pass -}
{-# LANGUAGE EmptyCase #-}

module EmptyCaseImplicitRParen where

data Void

absurdInParens :: Bool -> Void -> a
absurdInParens b v = (case b of True -> case v of)
