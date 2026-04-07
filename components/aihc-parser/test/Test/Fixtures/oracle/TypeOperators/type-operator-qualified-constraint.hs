{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds, TypeOperators #-}
module TypeOperatorQualifiedConstraint where

import GHC.TypeLits

f :: (1 <= 2) => ()
f = ()
