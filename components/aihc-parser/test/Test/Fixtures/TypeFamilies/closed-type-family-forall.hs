{- ORACLE_TEST
id: closed-type-family-forall
category: declarations
expected: xfail
reason: closed type family with forall
-}
{-# LANGUAGE TypeFamilies, ExplicitForAll #-}
module ClosedTypeFamilyForAll where

type family R a where
  forall t a. R (t a) = [a]
  forall a.   R a     = a
