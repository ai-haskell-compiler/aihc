{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module RealDice.Util.PrimeSpec
  ( result_is_less_than_or_equal_to_input,
    result_is_prime,
    next_largest_prime_is_larger_than_input,
    inputs_lower_than_1_return_0,
    result_equals_result_from_equivalent_function,
  )
where

import Data.Numbers.Primes (isPrime)
import RealDice.Util.Prime (greatestPrimeNotGreaterThan)
import Test.QuickCheck

result_is_prime :: Int -> Property
result_is_prime n = n > 1 ==> isPrime (greatestPrimeNotGreaterThan n)

result_is_less_than_or_equal_to_input :: Int -> Property
result_is_less_than_or_equal_to_input n =
  n > 1 ==> greatestPrimeNotGreaterThan n <= n

next_largest_prime_is_larger_than_input :: Int -> Property
next_largest_prime_is_larger_than_input n = do
  let result = greatestPrimeNotGreaterThan n
  n > 1 ==> n < head (filter isPrime [result + 1, result + 2 ..])

inputs_lower_than_1_return_0 :: Int -> Property
inputs_lower_than_1_return_0 n = n < 1 ==> greatestPrimeNotGreaterThan n == 0

result_equals_result_from_equivalent_function :: Int -> Property
result_equals_result_from_equivalent_function n =
  n > 1 ==>
    greatestPrimeNotGreaterThan n == head (filter isPrime [n, n - 1 ..])
