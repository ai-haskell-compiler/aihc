{- ORACLE_TEST pass -}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}

module ParenMinimalNestedKindSignature where

import Data.Kind (Constraint, Type)

x :: [_ :: (_ :: Type)]
x = undefined

type T = (() :: Constraint) => forall a. (a :: (Type :: Type))
