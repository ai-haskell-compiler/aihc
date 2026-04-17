{- ORACLE_TEST xfail reason="associated type families with injectivity annotations stop at the bar" -}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module M where

import Data.Kind (Type)

class FProd (f :: Type -> Type) where
  type Elem f = (i :: f k -> k -> Type) | i -> f
