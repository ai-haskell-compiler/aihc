module Main (main) where

import RealDice.CoinSpec (doesnt_give_same_result_10_times)
import RealDice.DieSpec (returns_many_unique_rolls)
import RealDice.RNGSpec (returns_many_unique_integers)
import RealDice.Util.PrimeSpec (next_largest_prime_is_larger_than_input, result_is_less_than_or_equal_to_input, result_is_prime)
import TestCheck (check)

-- | Run the test suite

-- | In general, functions will either be tested using QuickCheck in this suite
--   or using Docutest in the files themselves, but not both

-- | This suite of QuickCheck tests contains  a guard that will cause the test
--   `suite to fail if any of the individual tests fail
main :: IO ()
main = do
  putStrLn "Running test suite!"

  putStrLn "Testing RealDice.Util.Prime..."

  putStrLn "...greatestPrimeNotGreaterThan..."
  putStrLn "...returns a prime result:"
  check result_is_prime
  putStrLn "...returns a result less than or equal to the input:"
  check result_is_less_than_or_equal_to_input
  putStrLn "...returns the largest possible result:"
  check next_largest_prime_is_larger_than_input

  putStrLn "Testing RealDice.CoinSpec..."

  putStrLn "..flipCoin..."
  putStrLn "...doesn't give the same result 10 times in a row:"
  check doesnt_give_same_result_10_times

  putStrLn "Testing RealDice.RNGSpec..."

  putStrLn "..randomIntR..."
  putStrLn "...returns many unique integers:"
  check returns_many_unique_integers

  putStrLn "Testing RealDice.DieSpec..."

  putStrLn "..roll1d..."
  putStrLn "...returns many unique rolls:"
  check returns_many_unique_rolls

  putStrLn "All tests pass!"
