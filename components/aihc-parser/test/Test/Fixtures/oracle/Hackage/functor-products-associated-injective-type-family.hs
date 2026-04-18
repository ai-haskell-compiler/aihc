{- ORACLE_TEST pass -}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module M where

import Data.Kind (Type)

class FProd (f :: Type -> Type) where
  type Elem f = (i :: f k -> k -> Type) | i -> f
