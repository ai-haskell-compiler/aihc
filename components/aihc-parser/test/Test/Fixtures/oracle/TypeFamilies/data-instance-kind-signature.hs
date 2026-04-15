{- ORACLE_TEST pass -}
{-# LANGUAGE TypeFamilies #-}

module DataInstanceKindSignature where

import Data.Kind (Type)

data family Fam a :: Type -> Type

data instance Fam () :: Type -> Type where
  F :: Fam () Int
