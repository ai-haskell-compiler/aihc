{-# LANGUAGE MagicHash #-}

-- Lambdas whose arguments are constructor, tuple, or unboxed patterns.
module Main where

import Data.Coerce (coerce)
import GHC.Exts (Int (I#), (*#), (+#))

newtype Wrapped = Wrapped Int

addUnboxed :: Int -> Int -> Int
addUnboxed = \(I# x#) (I# y#) -> I# (x# +# y#)

scaleWrapped :: Wrapped -> Int -> Int
scaleWrapped = coerce $ \(I# x#) (I# k#) -> I# (x# *# k#)

sumPair :: (Int, Int) -> Int
sumPair = \(a, b) -> a + b

nested :: Maybe (Int, Int) -> Int
nested = \(Just (a, b)) -> a * b

main :: IO ()
main = do
  print (addUnboxed 3 4)
  print (scaleWrapped (Wrapped 6) 7)
  print (sumPair (10, 32))
  print (map (\(Just n) -> n + 1) [Just (1 :: Int), Just 2])
  print (nested (Just (6, 7)))
