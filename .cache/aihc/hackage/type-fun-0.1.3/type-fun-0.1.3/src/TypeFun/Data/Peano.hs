module TypeFun.Data.Peano
  ( N(..)
  , KnownPeano(..)
  , ToNat
  , FromNat
  , (:+:)
  , (:-:)
  , (:*:)
  ) where

import           Data.Typeable
import           GHC.Generics  (Generic)
import           GHC.TypeLits

data N = Z | S N
         deriving ( Eq, Ord, Read, Show
                  , Generic, Typeable )

-- | @since 0.1.2
class KnownPeano (p :: N) where
  peanoVal :: proxy p -> Integer

instance KnownPeano Z where
  peanoVal _ = 0

instance (KnownPeano n) => KnownPeano (S n) where
  peanoVal _ = succ $ peanoVal (Proxy :: Proxy n)

type family ToNat (a :: N) :: Nat where
  ToNat Z = 0
  ToNat (S a) = 1 + (ToNat a)

type family FromNat (a :: Nat) :: N where
  FromNat 0 = Z
  FromNat a = S (FromNat (a - 1))

type family (:+:) (a :: N) (b :: N) :: N where
  Z :+: b = b
  (S a) :+: b = a :+: (S b)

type family (:-:) (a :: N) (b :: N) :: N where
  a :-: Z = a
  (S a) :-: (S b) = a :-: b

type family (:*:) (a :: N) (b :: N) :: N where
  Z :*: b = Z
  a :*: Z = Z
  (S a) :*: b = b :+: (a :*: b)
