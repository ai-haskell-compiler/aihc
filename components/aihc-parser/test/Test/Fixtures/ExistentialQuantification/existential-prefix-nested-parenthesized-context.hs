{- ORACLE_TEST
id: existential-prefix-nested-parenthesized-context
category: declarations
expected: pass
-}
{-# LANGUAGE ExistentialQuantification #-}

module ExistentialPrefixNestedParenthesizedContext where

data A = forall a. ((Show a)) => A a
data B = forall a. ((((Show a)))) => B a
