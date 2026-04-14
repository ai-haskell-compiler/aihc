{- ORACLE_TEST xfail reason="data family instances with inline result kind signatures are not parsed" -}
{-# LANGUAGE TypeFamilies #-}

module DataInstanceKindSignature where

import Data.Kind (Type)

data family Fam a :: Type -> Type

data instance Fam () :: Type -> Type where
  F :: Fam () Int
