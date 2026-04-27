{- ORACLE_TEST pass -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module InfixDataHeadKindedBinders where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

data ListKey a

data (key :: Symbol) := (value :: Type)
  where
    (:=) :: ListKey a -> b -> a := b