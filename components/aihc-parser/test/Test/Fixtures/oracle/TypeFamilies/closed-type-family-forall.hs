{- ORACLE_TEST xfail closed type family with forall -}
{-# LANGUAGE TypeFamilies, ExplicitForAll #-}
module ClosedTypeFamilyForAll where

type family R a where
  forall t a. R (t a) = [a]
  forall a.   R a     = a