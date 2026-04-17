{- ORACLE_TEST pass -}
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
