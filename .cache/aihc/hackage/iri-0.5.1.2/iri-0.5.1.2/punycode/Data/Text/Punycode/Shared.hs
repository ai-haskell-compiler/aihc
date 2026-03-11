module Data.Text.Punycode.Shared where

import Data.Char (ord)

base :: Int
base = 36

tmin :: Int
tmin = 1

tmax :: Int
tmax = 26

skew :: Int
skew = 38

damp :: Int
damp = 700

initial_bias :: Int
initial_bias = 72

initial_n :: Int
initial_n = 128

adapt :: Int -> Int -> Bool -> Int
adapt delta numpoints firsttime = helper
  where
    helper = loop 0 $ delta' + (delta' `div` numpoints)
      where
        delta'
          | firsttime = delta `div` damp
          | otherwise = delta `div` 2
    loop k delta'
      | delta' > ((base - tmin) * tmax) `div` 2 = loop (k + base) $ delta' `div` (base - tmin)
      | otherwise = k + (((base - tmin + 1) * delta') `div` (delta' + skew))

isBasic :: Char -> Bool
isBasic = (< initial_n) . ord
