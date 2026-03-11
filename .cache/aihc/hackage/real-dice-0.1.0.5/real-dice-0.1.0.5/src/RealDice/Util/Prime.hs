-- | Module for finding the greatest prime number that is less than or equal to
--   a given number
module RealDice.Util.Prime (greatestPrimeNotGreaterThan) where

import Data.Numbers.Primes (isPrime)

-- | Returns the greatest prime number that is not greater than the given number

-- | Returns 1 if called with the number 1

-- | Returns 0 if called with a number lower than 1

-- | See RealDice.Generate.RawData for reasons for use in this package

-- | ==== __Examples__
--   >>> greatestPrimeNotGreaterThan 10
--   7
--   >>> greatestPrimeNotGreaterThan 1024
--   1021
--   >>> greatestPrimeNotGreaterThan 1
--   1
--   >>> greatestPrimeNotGreaterThan 0
--   0
--   >>> greatestPrimeNotGreaterThan (-1024)
--   0
greatestPrimeNotGreaterThan :: Int -> Int
greatestPrimeNotGreaterThan n
  | n < 1 = 0
  | n == 1 = 1
  | isPrime n = n
  | otherwise = greatestPrimeNotGreaterThan (n - 1)
