{-# LANGUAGE
  UndecidableInstances, ScopedTypeVariables, DataKinds,
  FlexibleInstances, GADTs, TypeFamilies, TemplateHaskell,
  InstanceSigs, TypeOperators, PolyKinds, StandaloneDeriving,
  FlexibleContexts, AllowAmbiguousTypes, CPP, OverloadedStrings,
  EmptyCase, TypeApplications #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#endif
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif

#ifndef MIN_VERSION_singletons_base
# define MIN_VERSION_singletons_base(x,y,z) 0
#endif

module Data.Nat (
    Nat(..)
  , NatPlus
  , NatMul
  , NatMinus
  , NatAbs
  , NatSignum
  , natPlus
  , natMul
  , natMinus
  , natAbs
  , natSignum
  , someNatVal
#if MIN_VERSION_singletons(2,6,0)
  , SNat(..)
  , PreludeSingletons.Sing
#else
  , SNat
  , PreludeSingletons.Sing(SS, SZ)
#endif
  , PreludeSingletons.PNum
  , PreludeSingletons.SNum
  , SSym0(..)
  , SSym1
  , ZSym0
  , Lit
  , LitSym0(..)
  , LitSym1
  , SLit
  , sLit) where

import qualified GHC.TypeLits as Lit

#if MIN_VERSION_singletons(3,0,0)
import Data.Singletons.Base.TH
import Prelude.Singletons as PreludeSingletons
#else
import Data.Singletons.Prelude as PreludeSingletons
import Data.Singletons.TH
#endif

$(singletons [d|
  data Nat = Z | S Nat deriving (Eq, Show, Ord)

  natPlus :: Nat -> Nat -> Nat
  natPlus Z     b = b
  natPlus (S a) b = S (natPlus a b)

  natMul :: Nat -> Nat -> Nat
  natMul Z     _ = Z
  natMul (S a) b = natPlus b (natMul a b)

  natMinus :: Nat -> Nat -> Nat
  natMinus Z       _     = Z
  natMinus (S a)   (S b) = natMinus a b
  natMinus a@(S _) Z     = a

  natAbs :: Nat -> Nat
  natAbs n = n

  natSignum :: Nat -> Nat
  natSignum Z     = Z
  natSignum (S _) = S Z

  instance Num Nat where
    (+) = natPlus
    (-) = natMinus
    (*) = natMul
    abs = natAbs
    signum = natSignum
    fromInteger n
      = if n == 0
           then Z
           else S (fromInteger (n - 1))
  |])

#if !(MIN_VERSION_singletons(2,4,0))
deriving instance Show (SNat n)
#endif

#if !(MIN_VERSION_singletons_base(3,3,0))
instance Eq (SNat n) where
  (==) _ _ = True

instance Ord (SNat n) where
  compare _ _ = EQ
#endif

{-| Converts a runtime 'Integer' to an existentially wrapped 'Nat'. Returns 'Nothing' if
the argument is negative -}
someNatVal :: Integer -> Maybe (SomeSing Nat)
someNatVal n = case Lit.someNatVal n of
  Just (Lit.SomeNat (_ :: Proxy n)) -> Just (SomeSing (sFromInteger (sing :: Sing n)))
  Nothing -> Nothing

{-| Provides a shorthand for 'Nat'-s using "GHC.TypeLits", for example:

>>> :kind! Lit 3
Lit 3 :: Nat
= 'S ('S ('S 'Z))
-}

type family Lit n where
  Lit 0 = Z
  Lit n = S (Lit (n Lit.- 1))
$(genDefunSymbols [''Lit])

type SLit n = Sing (Lit n)

{-| Shorthand for 'SNat' literals using `TypeApplications`.

>>> :set -XTypeApplications
>>> sLit @5
SS (SS (SS (SS (SS SZ))))

-}

sLit :: forall (n :: Lit.Nat). SingI (Lit n) => Sing (Lit n)
sLit = sing
