{- ORACLE_TEST pass -}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE KindSignatures #-}

module ForallKindedInferredBinder where

import Data.Kind (Type)

f :: forall {a :: Type}. a -> a
f x = x
