{- ORACLE_TEST xfail pretty-print yields ambiguous output -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExplicitForAll #-}

module NewtypeMaybeHashUnboxedSum where

newtype Maybe# :: forall (r :: RuntimeRep). TYPE r -> TYPE ('SumRep '[ 'TupleRep '[], r]) where
  Maybe# :: forall (r :: RuntimeRep) (a :: TYPE r). (# (# #) | a #) -> Maybe# @r a
