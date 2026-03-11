{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
module Type.Data.Num.Unary (
    Unary, unary, Un, Zero, Succ, zero, succ,
    HeadSingleton(Zero, Succ), headSingleton,
    Singleton(..), singleton, singletonFromProxy,
    integerFromSingleton, integralFromSingleton,
    integralFromProxy,
    Natural(..), Positive(..),
    (:+:), (:*:),
    reifyNatural,
    ) where

import qualified Type.Data.Num as Num
import qualified Type.Base.Proxy as Proxy
import Type.Base.Proxy (Proxy(Proxy))

import Text.Printf (printf)

import Prelude hiding (succ)


-- | Representation name for unary type level numbers.
data Unary

data Un x
data Zero
data Succ x
{-
Negative numbers could be represented by Pred
but this would complicate our proofs.
We would require that a number contains only Succ or Pred.
Alternative:
Int = Zero | Neg Nat | Pos Nat
Nat = None | Succ Nat
-}

zero :: Proxy Zero
zero = Proxy

succ :: Proxy n -> Proxy (Succ n)
succ Proxy = Proxy


class Natural n where
    switchNat ::
        f Zero ->
        (forall m. (Natural m) => f (Succ m)) ->
        f n

instance Natural Zero where switchNat x _ = x
instance Natural n => Natural (Succ n) where switchNat _ x = x


class (Natural n) => Positive n where
    switchPos ::
        (forall m. (Natural m) => f (Succ m)) ->
        f n

instance Natural n => Positive (Succ n) where switchPos x = x


type family x :+: y
type instance x :+: Zero = x
type instance x :+: Succ y = Succ (x :+: y)

type family x :*: y
type instance _x :*: Zero = Zero
type instance x :*: Succ y = x :+: (x :*: y)



data HeadSingleton n where
    Zero :: HeadSingleton Zero
    Succ :: (Natural n) => HeadSingleton (Succ n)

headSingleton :: (Natural n) => HeadSingleton n
headSingleton = switchNat Zero Succ


newtype Singleton n = Singleton Integer

instance (Natural n) => Num.Integer (Un n) where
    singleton = singletonToGeneric singleton
    type Repr (Un n) = Unary

singletonToGeneric :: Singleton n -> Num.Singleton (Un n)
singletonToGeneric (Singleton n) = Num.Singleton n

singleton :: (Natural n) => Singleton n
singleton =
    switchNat
        (Singleton 0)
        (succSingleton singleton)

succSingleton ::
    (Natural n) =>
    Singleton n -> Singleton (Succ n)
succSingleton (Singleton n) = Singleton $ n+1


integerFromSingleton :: (Natural n) => Singleton n -> Integer
integerFromSingleton (Singleton n) = n

integralFromSingleton :: (Natural n, Num a) => Singleton n -> a
integralFromSingleton = fromInteger . integerFromSingleton

singletonFromProxy :: (Natural n) => Proxy n -> Singleton n
singletonFromProxy Proxy = singleton

integralFromProxy :: (Natural n, Num a) => Proxy n -> a
integralFromProxy = integralFromSingleton . singletonFromProxy


instance Num.Representation Unary where
    reifyIntegral _ i k = reifyIntegral i (k . unary)

unary :: Proxy n -> Proxy (Un n)
unary Proxy = Proxy

stripUn :: Proxy (Un n) -> Proxy n
stripUn Proxy = Proxy

reifyIntegral :: Integer -> (forall s. Natural s => Proxy s -> w) -> w
reifyIntegral n f =
    if n < 0
      then error "negative unary numbers not supported so far"
      else reifyNatural n f

reifyNatural :: Integer -> (forall s. Natural s => Proxy s -> w) -> w
reifyNatural n f =
   if n>0
     then reifyNatural (n-1) (f . succ)
     else f zero


type instance Un x Num.:+: Un y = Un (x :+: y)
type instance Un x Num.:*: Un y = Un (x :*: y)


instance Natural a => Proxy.Show (Un a) where
   showsPrec prec =
      (\n -> showParen (prec>10) (showString (printf "unary u%d" n)))
       . integerFromSingleton . singletonFromProxy . stripUn
