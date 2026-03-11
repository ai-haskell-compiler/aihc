{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}

module Type.Data.Bool
    ( C(switch)
    , Singleton(False, True)
    , singleton
    , True
    , true
    , False
    , false
    , Not
    , not
    , (:&&:)
    , and
    , (:||:)
    , or
    , If
    , if_
    ) where

import Type.Base.Proxy (Proxy(Proxy))
import Data.Typeable (Typeable)

import qualified Prelude


class C bool where switch :: f False -> f True -> f bool
instance C False where switch f _ = f
instance C True where switch _ f = f

data Singleton bool where
    False :: Singleton False
    True :: Singleton True

singleton :: (C bool) => Singleton bool
singleton = switch False True

data True deriving (Typeable)
true :: Proxy True
true = Proxy
instance Prelude.Show True where
    show _ = "True"
data False deriving (Typeable)
false :: Proxy False
false = Proxy
instance Prelude.Show False where
    show _ = "False"

type family Not x
type instance Not False = True
type instance Not True  = False
not :: Proxy x -> Proxy (Not x)
not Proxy = Proxy

type family x :&&: y
type instance False :&&: _x = False
type instance True  :&&: x = x
and :: Proxy x -> Proxy y -> Proxy (x :&&: y)
and Proxy Proxy = Proxy

type family x :||: y
type instance True  :||: _x = True
type instance False :||: x = x
or :: Proxy x -> Proxy y -> Proxy (x :||: y)
or Proxy Proxy = Proxy

type family If x y z
type instance If True y _z = y
type instance If False _y z = z
if_ :: Proxy x -> Proxy y -> Proxy z -> Proxy (If x y z)
if_ Proxy Proxy Proxy = Proxy
