{- ORACLE_TEST pass -}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE DataKinds #-}
module SbvPromotedStringTypeAppPattern where

import GHC.TypeLits

data Forall (a :: Symbol) = Forall Int

f (Forall @"xs" x) = x
