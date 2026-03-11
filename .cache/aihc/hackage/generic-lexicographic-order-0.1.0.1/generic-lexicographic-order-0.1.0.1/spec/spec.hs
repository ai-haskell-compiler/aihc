{-# LANGUAGE DeriveGeneric #-}

import Data.Lexicographic.Generic

import Control.Monad

main :: IO ()
main = do
  genericBoundedEnumTest "Generic Bounded Enum for Sum Types" $ [A1 x | x <- enum] ++ [A2 y | y <- enum]
  genericBoundedEnumTest "Generic Bounded Enum for Product Types" [B x y | x <- enum, y <- enum]

data A = A1 Bool | A2 Ordering
  deriving (Eq, Ord, Generic)

instance Bounded A where
  minBound = genericMinBound
  maxBound = genericMaxBound

instance Enum A where
  fromEnum = genericFromEnum
  toEnum = genericToEnum

data B = B Bool Ordering
  deriving (Eq, Ord, Generic)

instance Bounded B where
  minBound = genericMinBound
  maxBound = genericMaxBound

instance Enum B where
  fromEnum = genericFromEnum
  toEnum = genericToEnum

genericBoundedEnumTest :: (Ord a, Bounded a, Enum a) => String -> [a] -> IO ()
genericBoundedEnumTest msg expectation = do
  putStr $ msg ++ ":"
  let subject = enum
  assert "lexicographic order" $ pure $ subject == expectation
  assert "coherent with Ord" $ pure $ and $ zipWith (<) subject $ tail subject
  assert "coherent fromEnum" $ pure $ and $ zipWith (==) [0..] $ map fromEnum subject
  assert "coherent toEnum" $ pure $ and $ zipWith (==) subject $ map toEnum [0..]
  assert "from/to round trips" $ pure $ all (\s -> (toEnum . fromEnum) s == s) subject
  putStrLn $ "\n" ++ "PASSED"

assert :: String -> IO Bool -> IO ()
assert msg action = do
  putStr $ "\n\t" ++ msg ++ ":\t"
  success <- action
  if success
    then putStr "OK"
    else putStr "FAIL" >> fail ("FAILURE: " ++ msg)

enum :: (Bounded a, Enum a) => [a]
enum = [minBound .. maxBound]
