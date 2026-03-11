{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Type.Data.Num
    ( Negate
    , negate
    , IsPositive
    , isPositive
    , IsZero
    , isZero
    , IsNegative
    , isNegative
    , IsNatural
    , isNatural
    , One
    , one
    , Succ
    , succ
    , Pred
    , pred
    , IsEven
    , isEven
    , IsOdd
    , isOdd
    , (:+:)
    , add
    , (:-:)
    , sub
    , (:*:)
    , mul
    , Mul2
    , mul2
    , Pow2
    , pow2
    , Log2Ceil
    , log2Ceil
    , DivMod
    , divMod
    , Div
    , div
    , Mod
    , mod
    , Div2
    , div2
    , Fac
    , fac
    , Singleton(..)
    , Representation (..)
    , Integer (..)
    , Natural
    , Positive
    , Negative
    , integerFromSingleton
    , integralFromSingleton
    , singletonFromProxy
    , integralFromProxy
    , fromInteger
    , reifyPositive
    , reifyNegative
    , reifyNatural
    ) where

import Type.Data.Bool (False, True, Not)
import Type.Base.Proxy (Proxy(Proxy))
import Data.Maybe.HT (toMaybe)

import qualified Prelude as P
import Prelude (Num, Maybe, (.), (<), (>), (>=))


-- | @Negate x@ evaluates to the additive inverse of (i.e., minus) @x@.
type family Negate x
negate :: Proxy x -> Proxy (Negate x)
negate Proxy = Proxy

type family IsPositive x
isPositive :: Proxy x -> Proxy (IsPositive x)
isPositive Proxy = Proxy

type family IsZero x
isZero :: Proxy x -> Proxy (IsZero x)
isZero Proxy = Proxy

type family IsNegative x
isNegative :: Proxy x -> Proxy (IsNegative x)
isNegative Proxy = Proxy

type family IsNatural x
isNatural :: Proxy x -> Proxy (IsNatural x)
isNatural Proxy = Proxy

type family One repr
one :: Proxy repr -> Proxy (One repr)
one Proxy = Proxy

type family Succ x
succ :: Proxy x -> Proxy (Succ x)
succ Proxy = Proxy

type family Pred x
pred :: Proxy x -> Proxy (Pred x)
pred Proxy = Proxy

type family IsEven x
isEven :: Proxy x -> Proxy (IsEven x)
isEven Proxy = Proxy

type family IsOdd x
type instance IsOdd x = Not (IsEven x)
isOdd :: Proxy x -> Proxy (IsOdd x)
isOdd Proxy = Proxy

type family x :+: y
add :: Proxy x -> Proxy y -> Proxy (x :+: y)
add Proxy Proxy = Proxy

type family x :-: y
sub :: Proxy x -> Proxy y -> Proxy (x :-: y)
sub Proxy Proxy = Proxy

type family x :*: y
mul :: Proxy x -> Proxy y -> Proxy (x :*: y)
mul Proxy Proxy = Proxy

type family Mul2 x
mul2 :: Proxy x -> Proxy (Mul2 x)
mul2 Proxy = Proxy

type family DivMod x y
divMod :: Proxy x -> Proxy y -> Proxy (DivMod x y)
divMod Proxy Proxy = Proxy

type family Div x y
div :: Proxy x -> Proxy y -> Proxy (Div x y)
div Proxy Proxy = Proxy

type family Mod x y
mod :: Proxy x -> Proxy y -> Proxy (Mod x y)
mod Proxy Proxy = Proxy

type family Div2 x
div2 :: Proxy x -> Proxy (Div2 x)
div2 Proxy = Proxy

type family Fac x
fac :: Proxy x -> Proxy (Fac x)
fac Proxy = Proxy

type instance Fac x = FacRec x (IsZero x)
type family FacRec x is0
type instance FacRec x True = One (Repr x)
-- peasant multiplication is faster if second factor is small
type instance FacRec x False = Fac (Pred x) :*: x



type family Pow2 x
pow2 :: Proxy x -> Proxy (Pow2 x)
pow2 Proxy = Proxy

type family Log2Ceil x
log2Ceil :: Proxy x -> Proxy (Log2Ceil x)
log2Ceil Proxy = Proxy

class Integer x => Natural x
instance (Integer x, IsNatural x  ~ True) => Natural x
class Integer x => Positive x
instance (Integer x, IsPositive x ~ True) => Positive x
class Integer x => Negative x
instance (Integer x, IsNegative x ~ True) => Negative x

class (Representation (Repr x)) => Integer x where
    singleton :: Singleton x
    type Repr x

class Representation r where
    reifyIntegral ::
        Proxy r -> P.Integer ->
        (forall s. (Integer s, Repr s ~ r) => Proxy s -> a) ->
        a


newtype Singleton d = Singleton P.Integer


integerFromSingleton :: (Integer x) => Singleton x -> P.Integer
integerFromSingleton (Singleton n) = n

integralFromSingleton :: (Integer x, Num y) => Singleton x -> y
integralFromSingleton = P.fromInteger . integerFromSingleton

singletonFromProxy :: (Integer x) => Proxy x -> Singleton x
singletonFromProxy Proxy = singleton

integralFromProxy :: (Integer x, Num y) => Proxy x -> y
integralFromProxy = integralFromSingleton . singletonFromProxy

-- | synonym for 'integralFromProxy', kept for backward compatibility
fromInteger :: (Integer x, Num y) => Proxy x -> y
fromInteger = integralFromProxy


--- positive and negative assertions: unsafe, in a trusted kernel
data AssertPos x
data AssertNeg x
data AssertNat x

assertPos :: Proxy x -> Proxy (AssertPos x)
assertPos Proxy = Proxy

assertNeg :: Proxy x -> Proxy (AssertNeg x)
assertNeg Proxy = Proxy

assertNat :: Proxy x -> Proxy (AssertNat x)
assertNat Proxy = Proxy

type instance IsPositive (AssertPos _x) = True
type instance IsPositive (AssertNeg _x) = False

type instance IsNegative (AssertPos _x) = False
type instance IsNegative (AssertNeg _x) = True
type instance IsNegative (AssertNat _x) = False

type instance IsNatural  (AssertPos _x) = True
type instance IsNatural  (AssertNeg _x) = False
type instance IsNatural  (AssertNat _x) = True

instance Integer x => Integer (AssertPos x) where 
    singleton = case singleton :: Singleton x of Singleton n -> Singleton n
    type Repr (AssertPos x) = Repr x

instance Integer x => Integer (AssertNeg x) where
    singleton = case singleton :: Singleton x of Singleton n -> Singleton n
    type Repr (AssertNeg x) = Repr x

instance Integer x => Integer (AssertNat x) where
    singleton = case singleton :: Singleton x of Singleton n -> Singleton n
    type Repr (AssertNat x) = Repr x

reifyPositive ::
    Representation r =>
    Proxy r -> P.Integer ->
    (forall s. (Positive s, Repr s ~ r) => Proxy s -> a) ->
    Maybe a
reifyPositive r n k =
    toMaybe (n > 0) (reifyIntegral r n (k . assertPos))

reifyNegative ::
    Representation r =>
    Proxy r -> P.Integer ->
    (forall s. (Negative s, Repr s ~ r) => Proxy s -> a) ->
    Maybe a
reifyNegative r n k =
    toMaybe (n < 0) (reifyIntegral r n (k . assertNeg))

reifyNatural ::
    Representation r =>
    Proxy r -> P.Integer ->
    (forall s. (Natural s, Repr s ~ r) => Proxy s -> a) ->
    Maybe a
reifyNatural r n k =
    toMaybe (n >= 0) (reifyIntegral r n (k . assertNat))
