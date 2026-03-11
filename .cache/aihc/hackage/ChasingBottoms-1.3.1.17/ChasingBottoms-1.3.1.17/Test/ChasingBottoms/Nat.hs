{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module      :  Test.ChasingBottoms.Nat
-- Copyright   :  (c) Nils Anders Danielsson 2004-2022, 2024-2025
-- License     :  See the file LICENCE.
--
-- Maintainer  :  http://www.cse.chalmers.se/~nad/
-- Stability   :  experimental
-- Portability :  non-portable (GHC-specific)
--
-- A simple implementation of natural numbers on top of 'Integer's.
-- Note that since 'Integer's are used there is no infinite natural
-- number; in other words, 'succ' is strict.

module Test.ChasingBottoms.Nat(Nat, isSucc, fromSucc, natrec, foldN) where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary (CoArbitrary(..))
import qualified Data.Generics as G
import Data.Ratio ((%))
import Data.Typeable

default ()

-- | Natural numbers.
--
-- No 'G.Data' instance is provided, because the implementation should
-- be abstract.

-- Could add 'G.Data' instance based on unary representation of
-- natural numbers, but that would lead to inefficiencies.
newtype Nat = Nat { nat2int :: Integer } deriving (Eq, Ord, Typeable)

-- | @'isSucc' 0 == 'False'@, for other total natural numbers it is 'True'.
isSucc :: Nat -> Bool
isSucc (Nat 0) = False
isSucc _       = True

-- | @'fromSucc' 0 == 'Nothing'@, @'fromSucc' (n+1) == 'Just' n@ for a
-- total natural number @n@.
fromSucc :: Nat -> Maybe Nat
fromSucc (Nat 0) = Nothing
fromSucc n       = Just $ pred n

-- | 'natrec' performs primitive recursion on natural numbers.
natrec :: a -> (Nat -> a -> a) -> Nat -> a
natrec g _ (Nat 0) = g
natrec g h n       = let p = pred n in h p (natrec g h p)

-- | 'foldN' is a fold on natural numbers:
--
-- @
--  'foldN' g h = 'natrec' g ('curry' '$' h . 'snd')
-- @
foldN :: a -> (a -> a) -> Nat -> a
foldN g h = natrec g (curry $ h . snd)

steal :: (Integer -> Integer -> Integer) -> Nat -> Nat -> Nat
steal op x y = fromInteger $ (nat2int x) `op` (nat2int y)

instance Num Nat where
  (+) = steal (+)
  (*) = steal (*)
  x - y = let x' = nat2int x; y' = nat2int y
          in if x' < y' then
            error "Nat: x - y undefined if y > x."
           else
            fromInteger $ x' - y'
  negate = error "Nat: negate undefined."
  signum n = if isSucc n then 1 else 0
  abs = id
  fromInteger i | i < 0     = error "Nat: No negative natural numbers."
                | otherwise = Nat i

instance Real Nat where
  toRational = (% 1) . nat2int

steal2 :: (Integer -> Integer -> (Integer, Integer))
          -> Nat -> Nat -> (Nat, Nat)
steal2 op x y = let (x', y') = (nat2int x) `op` (nat2int y)
                in (fromInteger x', fromInteger y')

instance Integral Nat where
  toInteger = toInteger . fromEnum
  a `quotRem` b = if b == 0 then
    error "Nat: quotRem undefined for zero divisors."
   else
    steal2 quotRem a b
  a `divMod` b = if b == 0 then
    error "Nat: divMod undefined for zero divisors."
   else
    steal2 divMod a b

instance Enum Nat where
  succ = (+ 1)
  pred = subtract 1
  toEnum = fromInteger . toInteger
  fromEnum = fromInteger . nat2int
  -- Add tests for enumFrom and friends if the default definitions are
  -- overridden.

instance Show Nat where
  showsPrec _ = showString . show . nat2int

instance Arbitrary Nat where
  arbitrary = do
    n <- arbitrary :: Gen Integer
    return $ fromInteger $ abs n

  shrink 0 = []
  shrink n = [n - 1]

instance CoArbitrary Nat where
  coarbitrary n = coarbitrary (toInteger n)
