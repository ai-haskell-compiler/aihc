{- ORACLE_TEST xfail reason="type-set uses the type-level application operator $ in type instance heads, which the parser rejects" -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module M where

import Data.Kind (Type)

type a >-> b = (b -> Type) -> a -> Type

type family ($) (f :: a >-> b) (x :: a) :: b

data InsertElse t t' ts :: () >-> k

type instance InsertElse t t' ts $ '() = t
