{- ORACLE_TEST pass -}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}

module ForallBodyKindSigForallSubject where

type T = forall a. forall a. forall a. _ :: _
