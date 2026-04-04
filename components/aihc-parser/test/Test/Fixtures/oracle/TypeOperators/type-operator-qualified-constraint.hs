{- ORACLE_TEST xfail parenthesized type-operator constraint -}
{-# LANGUAGE DataKinds, TypeOperators #-}
module TypeOperatorQualifiedConstraint where

import GHC.TypeLits

f :: (1 <= 2) => ()
f = ()
