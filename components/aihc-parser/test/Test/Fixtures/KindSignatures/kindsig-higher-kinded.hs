{- ORACLE_TEST
id: kindsig-higher-kinded
category: declarations
expected: pass
-}
{-# LANGUAGE KindSignatures #-}

module KindSignaturesHigherKinded where

import Data.Kind (Type)

data App (f :: Type -> Type) (a :: Type) = App (f a)
