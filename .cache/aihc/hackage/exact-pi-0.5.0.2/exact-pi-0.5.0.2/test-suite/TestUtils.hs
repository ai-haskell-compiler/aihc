{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
module TestUtils
  ( getValue
  , getDigit
  , getDigitBBP
  , E
  ) where

import Data.Proxy   (Proxy)
import Data.List    (foldl')
import Data.Fixed   (mod', HasResolution(..), Fixed)

import GHC.TypeLits (Nat, KnownNat, SomeNat(..), natVal, someNatVal)

import Data.ExactPi

-- E n generalises E2/E3/E6/E12 from Data.Fixed to give more precise
-- fixed-precision arithmetic: Fixed (E 30) has 30 decimal places.
data E (n :: Nat)

instance KnownNat n => HasResolution (E n) where
  resolution _ = 10^natVal (undefined :: E n)

-- this function is not necessarily in general safe but is fine in the cases used here
getValue :: (Eq a, Fractional a) => ExactPi -> a
getValue = getRationalLimit (==) . rationalApproximations

getDigit :: Integer -> Int
getDigit n = case someNatVal d of
               Just (SomeNat (_ :: Proxy m)) -> (floor $ 16^n * (getValue (Exact 1 1) :: Fixed (E m))) `mod` 16
               Nothing -> error "negative digit requested"
             where d = fromInteger $ 4 * n `div` 3 + 1
--------------------------------------------------------------------------------
powModInteger :: Integer -> Integer -> Integer -> Integer
powModInteger a k n = a^k `mod` n

infTerms :: Integer -> Int -> Integer -> Float
infTerms n j k = 16^^(n-k) / (fromIntegral $ 8*k + fromIntegral j)

finiteTerms :: Integer -> Int -> Integer -> Float
finiteTerms n j k = (fromIntegral $ powModInteger 16 (n-k) (8*k + j')) / (fromIntegral $ 8*k + j')
  where j' = fromIntegral j

summation :: Integer -> Int -> Float
summation n j = stabilise $ scanl plus finitePart [infTerms n j k | k <- [n+1..]]
  where finitePart = foldl' plus 0 [finiteTerms n j k | k <- [0..n]]

mod1 :: Float -> Float
mod1 x = mod' x 1

plus :: Float -> Float -> Float
plus x y = mod1 (x + y)

stabilise :: Eq a => [a] -> a
stabilise (x:y:xs)
  | x == y    = x
  | otherwise = stabilise (y:xs)
stabilise _ = error "finite list"

getDigitBBP :: Integer -> Int
getDigitBBP n = floor . (16 *) . mod1 $ 4 * summation n 1 - 2 * summation n 4 - summation n 5 - summation n 6
