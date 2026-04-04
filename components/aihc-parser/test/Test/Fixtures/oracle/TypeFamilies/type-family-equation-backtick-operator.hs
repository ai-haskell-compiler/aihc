{- ORACLE_TEST xfail generic-type-asserts backticked type operator in family equation -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TypeFamilyEquationBacktickOperator where

data a :+: b

type family F a where
  F (l :+: r) = l `And` r

type family l `And` r where
  l `And` r = l
