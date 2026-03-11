{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Numeric.QuadraticIrrational.Ring
  ( QuadExt(..)
  ) where

import Data.Proxy
import Numeric.QuadraticIrrational.SquareFree

data QuadExt (c :: SquareFree) t = QuadExt t t
  deriving (Eq)

instance (KnownSquareFree c, Show t) => Show (QuadExt c t) where
  showsPrec p (QuadExt a b) = showParen (p > 10)
    $ showString "QuadExt " . showsPrec 11 a . showChar '+' . showsPrec 11 b . showChar '√' . showsPrec 11 c
    where
      c = squareFreeVal (Proxy :: Proxy c)

instance (KnownSquareFree c, Num t) => Num (QuadExt c t) where
  QuadExt a1 b1 + QuadExt a2 b2 = QuadExt (a1 + a2) (b1 + b2)
  QuadExt a1 b1 - QuadExt a2 b2 = QuadExt (a1 - a2) (b1 - b2)
  negate (QuadExt a b) = QuadExt (negate a) (negate b)
  abs q = q
  signum _ = 1
  fromInteger n = QuadExt (fromInteger n) 0

  QuadExt a1 b1 * QuadExt a2 b2 = QuadExt a b
    where
      a = a1 * a2 + fromInteger c * b1 * b2
      b = a1 * b2 + a2 * b1
      c = squareFreeVal (Proxy :: Proxy c)

instance (KnownSquareFree c, Fractional t) => Fractional (QuadExt c t) where
  fromRational r = QuadExt (fromRational r) 0

  recip (QuadExt a b) = QuadExt (a / d) (negate b / d)
    where
      d = a * a - fromInteger c * b * b
      c = squareFreeVal (Proxy :: Proxy c)

  QuadExt a1 b1 / QuadExt a2 b2 = QuadExt (a / d) (b / d)
    where
      QuadExt a b = QuadExt a1 b1 * QuadExt a2 (negate b2) :: QuadExt c t
      d = a2 * a2 - fromInteger c * b2 * b2
      c = squareFreeVal (Proxy :: Proxy c)
