{-|
    Module      : Numeric.Modular
    Copyright   : (c) Preetham Gujjula, 2018
    License     : BSD3
    Maintainer  : preetham.gujjula@gmail.com
    Stability   : experimental

    The @'Mod' m@ type represents a Integer modulo m, i.e., a value in ℤ/mℤ,
    which enables type-safe modular arithmetic.

    This library, especially the 'withMod' function, uses ideas from
    /Functional Pearl: Implicit Configurations — or, Type Classes Reflect the/
    /Values of Types/ by Oleg Kiselyov and Chung-chieh Shan, available here:
    <http://okmij.org/ftp/Haskell/tr-15-04.pdf>.

    For example, to perform basic modular computations,

    >>> 10 :: Mod 3
    1
    >>> 15 + 3 :: Mod 7
    4

    Modular reductions are performed implicitly, so modular exponentiation can
    be performed efficiently.

    >>> 60803790666453028877 ^ 88100461154844882932 :: Mod 39127526509442054532
    33479467020524411041

    Compare this to running @(60803790666453028877 ^ 88100461154844882932)
    \``mod`\` 39127526509442054532@, which is much less efficient.

    The modulus can also be specified at runtime without losing any type safety
    or efficiency.

    >>> x = mkMod 10
    >>> y = mkMod 17
    >>> withMod 3 (x + y)
    0
    >>> withMod 10 (x + y)
    7
    >>> a = mkMod 60803790666453028877
    >>> b = 88100461154844882932 :: Integer
    >>> m = 39127526509442054532 :: Integer
    >>> withMod m $ a^b
    33479467020524411041
-}

{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators, GADTs, Rank2Types,
    ScopedTypeVariables, CPP #-}

#ifdef MIN_VERSION_GLASGOW_HASKELL
#if MIN_VERSION_GLASGOW_HASKELL(8,6,1,0)
{-# LANGUAGE NoStarIsType #-}
#endif
#endif

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Numeric.Modular
    ( Mod
    , mkMod
    , withMod
    ) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (Nat, KnownNat, type (*), type (+), natVal)

{-|
    Data type to represent an integer modulo `n`.
-}
data Mod (n :: Nat) = Mod Integer

{-| @mkMod n@ wraps @n@ in type @'Mod' m@. -}
mkMod :: forall m. (KnownNat m) => Integer -> Mod m
mkMod n = Mod (n `mod` (natVal (Proxy :: Proxy m)))

{-|
    In @withMod m a@

        * @m@ is the modulus.
        * @a@ is a value that can take on the type @'Mod' m@ for any @a@.
        * @withMod m a@ equals @a@ interpreted modulo @m@.

    > x = mkMod 17
    > withMod 5 x == 2
-}
withMod :: Integer -> (forall m. (KnownNat m) => Mod m) -> Integer
withMod k m = reifyInteger k (withModProxy m)

{- Given a polymorphic modular value and a proxy for the modulus Nat, resolve
   the modular value using the given modulus.
-}
withModProxy :: forall m. KnownNat m
             => (forall n. (KnownNat n) => Mod n) -> Proxy m -> Integer
withModProxy k modProxy = (getV (k :: Mod m)) `mod` (natVal modProxy)

{- Get the modulus m as a integer of a value of type Mod m. -}
getM :: forall m. (KnownNat m) => Mod m -> Integer
getM _ = natVal (Proxy :: Proxy m)

{- Get the Integer inside a value of type Mod m. -}
getV :: forall m. (KnownNat m) => Mod m -> Integer
getV (Mod k) = k

{- The implementation of "reifyIntegral" from the Implicit Configurations paper
   adapted to the current context.
-}
reifyInteger :: Integer -> (forall n. (KnownNat n) => Proxy n -> w) -> w
reifyInteger 0 f = f (Proxy :: Proxy 0)
reifyInteger n f
    | even n    = reifyInteger
                    (n `div` 2)
                    (\(Proxy :: Proxy n) -> f (Proxy :: Proxy (n * 2)))
    | otherwise = reifyInteger
                    (n - 1)
                    (\(Proxy :: Proxy n) -> f (Proxy :: Proxy (n + 1)))

instance Eq (Mod m) where
    (==) (Mod a) (Mod b) = a == b

instance (KnownNat m) => Show (Mod m) where
    show (Mod a) = show a

instance (KnownNat m) => Num (Mod m) where
    (+)    k@(Mod a) (Mod b) = Mod $ (a + b) `mod` (getM k)
    (*)    k@(Mod a) (Mod b) = Mod $ (a * b) `mod` (getM k)
    (-)    k@(Mod a) (Mod b) = Mod $ (a - b) `mod` (getM k)
    negate k@(Mod a) = Mod $ (negate a) `mod` (getM k)
    abs = id
    signum _ = 1
    fromInteger = mkMod
