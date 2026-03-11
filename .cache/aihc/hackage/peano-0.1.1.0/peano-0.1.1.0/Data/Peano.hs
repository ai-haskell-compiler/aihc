-- | See 'Peano'.
{-# LANGUAGE DeriveGeneric #-}
module Data.Peano ( Peano (Zero, Succ), infinity ) where

import Data.Data       ( Data )
import Data.Ix         ( Ix( index, inRange, range, rangeSize ) )
import GHC.Generics    ( Generic )

-- | The natural numbers in (lazy) unary notation.
data Peano
  = Zero
  | Succ Peano
  deriving (Eq, Ord, Data, Read, Show, Generic)

instance Enum Peano where
    succ = Succ

    pred (Succ n) = n
    pred Zero     = error "Data.Peano.pred Zero"

    toEnum   = fromInteger . toEnum

    fromEnum = fromEnum . toInteger

instance Bounded Peano where
    minBound = Zero
    maxBound = infinity

instance Ix Peano where
    range            = uncurry enumFromTo
    index (l, _) n   = fromEnum (n - l)
    inRange (l, u) n = l <= n && u >= n
    rangeSize (l, u) = fromEnum (Succ u - l)

instance Num Peano where
    Zero   + n = n
    Succ m + n = Succ (m + n)

    m      - Zero   = m
    Succ m - Succ n = m - n
    Zero   - _      = error "Data.Peano.(-): underflow (negative)"

    Zero   * _ = Zero
    Succ m * n = n + m * n

    abs = id

    signum Zero = Zero
    signum _    = Succ Zero

    fromInteger n =
      case compare n 0 of
        LT -> error "fromInteger n | n < 0"
        EQ -> Zero
        GT -> Succ (fromInteger (n - 1))

instance Real Peano where
    toRational = toRational . toInteger

instance Integral Peano where
    toInteger Zero     = 0
    toInteger (Succ n) = toInteger n + 1

    Zero `quotRem` Zero   = error "Data.Peano.quotRem: zero divided by zero"
    m    `quotRem` n      =
      case compare m n of
        LT -> (Zero, m)
        _  -> let (q, r) = quotRem (m - n) n in (Succ q, r)

    divMod = quotRem

-- | The infinite number (ω).
infinity :: Peano
infinity = Succ infinity
