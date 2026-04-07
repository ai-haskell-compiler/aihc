{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module TypeFamilyEquationBacktickOperator where

type family F a where
  F a = And l r

type family And l r where
  And l r = l
