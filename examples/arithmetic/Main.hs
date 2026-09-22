{-# LANGUAGE MagicHash #-}

-- Integer shifts through the primitives, arbitrary-precision Integer
-- arithmetic, and floating-point functions.
module Main where

import GHC.Exts
  ( Int (I#),
    iShiftL#,
    iShiftRA#,
    iShiftRL#,
    negateInt#,
    uncheckedIShiftL#,
    uncheckedIShiftRA#,
    uncheckedIShiftRL#,
  )

main :: IO ()
main = do
  shifts
  putStrLn (if integerChecks then "integer ok" else "integer fail")
  putStrLn (if floatingChecks then "floating ok" else "floating fail")

-- The three unchecked shifts of a signed integer lower to the shift
-- operations of the backend: a left shift, an arithmetic shift right that
-- replicates the sign bit, and a logical shift right that fills with zeros.
-- Data.Bits reaches none of them, so only these primitives and the GHC.Base
-- wrappers around them cover the lowering.
shifts :: IO ()
shifts = do
  print (map (\(I# value) -> I# (uncheckedIShiftL# value 4#)) [3, -3])
  print (map (\(I# value) -> I# (uncheckedIShiftRA# value 4#)) [48, -48])
  print (map (\(I# value) -> I# (uncheckedIShiftRL# value 4#)) [48, -48])
  print (I# (uncheckedIShiftL# 1# 63#))
  print (I# (uncheckedIShiftRA# (negateInt# 1#) 63#))
  print (I# (uncheckedIShiftRL# (negateInt# 1#) 60#))
  -- The wrappers guard the shift count, so a shift of the word size or more
  -- never reaches the primitive.
  print (map (\(I# count) -> I# (iShiftL# 3# count)) [4, 64])
  print (map (\(I# count) -> I# (iShiftRA# (negateInt# 48#) count)) [4, 64])
  print (map (\(I# count) -> I# (iShiftRL# 48# count)) [4, 64])

power :: Integer -> Int -> Integer
power value count =
  case count of
    0 -> 1
    _ -> value * power value (count - 1)

integerLow :: Integer -> Int
integerLow = fromInteger

integerChecks :: Bool
integerChecks =
  case power 2 80 of
    large ->
      case large + 12345 of
        left ->
          case large - 6789 of
            right ->
              left + right == large * 2 + 5556
                && large == 1208925819614629174706176
                && 12345 + large == left
                && left - left == 0
                && left * right == right * left
                && integerLow (left * right) == negate 83810205
                && negate left < 0
                && abs (negate left) == left

closeDouble :: Double -> Double -> Bool
closeDouble left right = abs (left - right) < 1.0e-9

closeFloat :: Float -> Float -> Bool
closeFloat left right = abs (left - right) < 1.0e-4

angle :: Double
angle = 0.75

unitCircle :: Double
unitCircle = sin angle * sin angle + cos angle * cos angle

halfFloat :: Float
halfFloat = 0.5

floatingChecks :: Bool
floatingChecks =
  closeDouble (sqrt 2.0 * sqrt 2.0) 2.0
    && closeDouble (exp (log 5.0)) 5.0
    && closeDouble unitCircle 1.0
    && closeDouble (2.0 ** 10.0) 1024.0
    && closeDouble (logBase 2.0 8.0) 3.0
    && closeDouble pi 3.141592653589793
    && closeDouble (1.0 / 4.0) 0.25
    && closeDouble (negate 2.5 + 2.5) 0.0
    && closeDouble 1.25e2 125.0
    && closeDouble (atan 1.0 * 4.0) pi
    && closeFloat (halfFloat + 0.25) 0.75
    && closeFloat (sqrt 2.0) 1.4142135
