{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Numeric.QuadraticIrrational.SquareFree
  ( SquareFree()
  , KnownSquareFree(..)
  , SomeSquareFree(..)
  , someSquareFreeVal
  , sameSquareFree
  ) where

import Data.Proxy
import Data.Type.Equality
import GHC.TypeLits
import Math.NumberTheory.ArithmeticFunctions
import Unsafe.Coerce

data SquareFree = Negative Nat | Positive Nat

class KnownSquareFree (c :: SquareFree) where
  squareFreeVal :: Proxy c -> Integer

instance KnownNat n => KnownSquareFree ('Negative n) where
  squareFreeVal (Proxy :: Proxy ('Negative n)) = negate $ natVal (Proxy :: Proxy n)

instance KnownNat n => KnownSquareFree ('Positive n) where
  squareFreeVal (Proxy :: Proxy ('Positive n)) = natVal (Proxy :: Proxy n)

data SomeSquareFree where
  SomeSquareFree :: KnownSquareFree c => Proxy c -> SomeSquareFree

someSquareFreeVal :: Integer -> Maybe SomeSquareFree
someSquareFreeVal 0 = Nothing
someSquareFreeVal n = case moebius n of
  MoebiusZ -> Nothing
  _ -> case someNatVal n of
    Just (SomeNat (Proxy :: Proxy n)) -> Just (SomeSquareFree (Proxy :: Proxy ('Positive n)))
    Nothing -> case someNatVal (negate n) of
      Just (SomeNat (Proxy :: Proxy n)) -> Just (SomeSquareFree (Proxy :: Proxy ('Negative n)))
      Nothing -> Nothing

sameSquareFree :: (KnownSquareFree a, KnownSquareFree b) => Proxy a -> Proxy b -> Maybe (a :~: b)
sameSquareFree x y
  | squareFreeVal x == squareFreeVal y = Just (unsafeCoerce Refl)
  | otherwise = Nothing
