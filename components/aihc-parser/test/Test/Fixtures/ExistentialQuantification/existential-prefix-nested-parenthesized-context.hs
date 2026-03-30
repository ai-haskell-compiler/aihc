{- ORACLE_TEST pass -}
{-# LANGUAGE ExistentialQuantification #-}

module ExistentialPrefixNestedParenthesizedContext where

data A = forall a. ((Show a)) => A a
data B = forall a. ((((Show a)))) => B a