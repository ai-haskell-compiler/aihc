module Main (main) where

import Rounding
import GHC.Float
import Data.Word

isNaN' :: Eq a => a -> Bool
isNaN' x = x /= x

f64 :: Word64 -> Double
f64 = castWord64ToDouble
f32 :: Word32 -> Float
f32 = castWord32ToFloat

main :: IO ()
main
  = if and tests
      then putStrLn "testsuite SUCCEDED"
      else error "testsuite FAILED"



tests :: [Bool]
tests = []
  <> sign_bit
  <> arithmic_signum
  <> successor
  <> rounding_variant_tests
  <> operator_tests
  <> conversion_tests



sign_bit :: [Bool]
sign_bit =
  [ True
  , 1 == i32_sign_bit_f64 (-1.0/0.0)
  , 1 == i32_sign_bit_f64 (-1.0)
  , 1 == i32_sign_bit_f64 (-0.0)
  , 0 == i32_sign_bit_f64 (0.0)
  , 0 == i32_sign_bit_f64 (1.0)
  , 0 == i32_sign_bit_f64 (1.0/0.0)
  ]


arithmic_signum :: [Bool]
arithmic_signum =
  [ True
  , 1 == i32_sign_bit_f64 (-1.0/0.0)
  , 1 == i32_sign_bit_f64 (-1.0)
  , 1 == i32_sign_bit_f64 (-0.0)
  , 0 == i32_sign_bit_f64 (0.0)
  , 0 == i32_sign_bit_f64 (1.0)
  , 0 == i32_sign_bit_f64 (1.0/0.0)
  ]


successor :: [Bool]
successor =
  [ True
  , f64_successorIEEE (-1/0) == -1.7976931348623157e308
  , f64_successorIEEE (-0.0) == f64 1
  , f64_successorIEEE (0.0) == f64 1
  , f64_successorIEEE (f64 1) == f64 2
  , f64_successorIEEE (f64 2) == f64 3
  , f64_successorIEEE (1.7976931348623157e308) == 1/0
  ]



rounding_variant_tests :: [Bool]
rounding_variant_tests = []
  <> squareRoot_tests
  <> ceil_tests
  <> floor_tests
  <> truncate_tests



squareRoot_tests :: [Bool]
squareRoot_tests = [ True
  , 0.0 == f32_squareRoot_ceil (0.0)
  , 0.0 == f32_squareRoot_floor (0.0)
  , 0.0 == f32_squareRoot_truncate (0.0)
  , 1.0 == f32_squareRoot_ceil (1.0)
  , 1.0 == f32_squareRoot_floor (1.0)
  , 1.0 == f32_squareRoot_truncate (1.0)
  , 3.7433924e-23 == f32_squareRoot_ceil (1.0e-45)
  , 3.743392e-23 == f32_squareRoot_floor (1.0e-45)
  , 3.743392e-23 == f32_squareRoot_truncate (1.0e-45)
  , 1.8446744e19 == f32_squareRoot_ceil (3.4028235e38)
  , 1.8446743e19 == f32_squareRoot_floor (3.4028235e38)
  , 1.8446743e19 == f32_squareRoot_truncate (3.4028235e38)
  ]




ceil_tests :: [Bool]
ceil_tests =
  [ True

  , 0.0 == f64_add_ceil 1 (-1)

  , (1/0) == f32_add_ceil (3.4028235e38) (3.4028235e38)
  , 0 == f32_add_ceil (-3.4028235e38) (3.4028235e38)
  , (-3.4028235e38) == f32_add_ceil (-3.4028235e38) (-3.4028235e38)

  , 5.0e-324 == f64_multiplicate_ceil 5.0e-324 1
  , 10 == f32_divide_ceil 1 0.1

  ]

floor_tests :: [Bool]
floor_tests =
  [ True
  , (-0.0) == f32_subtract_floor 1 1
  , (-0.0) == f32_add_floor 1 (-1)

  , 1 == f32_add_floor 1.0e-45 1
  , isNaN' $ f32_add_floor (1/0) (-1/0)
  , 1.0e-45 == f32_multiplicate_floor 1.0e-45 1

  , 3.743392e-23 == f32_squareRoot_floor (f32 1)
  , 0 == f64_subtract_floor 1 1
  , (1/0) == f32_add_floor 0 (1/0)
  , (-1/0) == f32_divide_floor 1 (-0)

  , 3.4028235e38 == f32_divide_floor 3.4028235e38 1.1754944e-38
  , 3.4028235e38 == f32_add_floor 3.4028235e38 3.4028235e38
  , (-0) == f32_add_floor (-3.4028235e38) 3.4028235e38

  , (-1/0) == f32_add_floor (-3.4028235e38) (-3.4028235e38)

  , 1 == f32_add_floor 1.0e-45 1
  , 1 == f64_add_floor 5.0e-324 1

  , 0 == f32_multiplicate_floor 5.0e-324 1
  , 5.0e-324 == f64_multiplicate_floor 5.0e-324 1
  , 9.999999 == f32_divide_floor 1 0.1

  ]

truncate_tests :: [Bool]
truncate_tests =
  [ True
  , 3.4028235e38 == f32_add_truncate 1.1754944e-38 3.4028235e38
  , 1 == f32_add_truncate 1 1.1754944e-38
  , (-1) == f32_add_truncate (-1) (-1.1754944e-38)

  , 0 == f64_subtract_truncate 1 1

  , 3.4028235e38 == f32_add_truncate (3.4028235e38) (3.4028235e38)
  , 0 == f32_add_truncate (-3.4028235e38) (3.4028235e38)
  , (-3.4028235e38) == f32_add_truncate (-3.4028235e38) (-3.4028235e38)

  , 1.0 == f32_add_truncate 1.0 1.0e-45
  , (-0.99999994) == f32_add_truncate (-1.0) 1.0e-45

  ]




operator_tests :: [Bool]
operator_tests = []
  <> multiplication_tests
  <> division_tests


multiplication_tests :: [Bool]
multiplication_tests =
  [ True

  , 1.0e18 == f32_multiplicate_floor 1.0e9 1.0e9
  , 1.00000005e18 == f32_multiplicate_ceil 1.0e9 1.0e9

  ,  (1/0) == f32_multiplicate_floor (1/0) 3.4028235e38
  ,  (-1/0) == f32_multiplicate_ceil (1/0) (-1/0)

  ,  3.4028235e38 == f32_multiplicate_floor 3.4028235e38 3.4028235e38
  ,  (1/0) == f32_multiplicate_ceil 3.4028235e38 3.4028235e38

  ]


division_tests :: [Bool]
division_tests =
  [ True
  , f32 1051372202 == f32_divide_floor (f32 1065353216) (f32 1077936128)
  , f32 (1051372202 + 1) == f32_divide_ceil (f32 1065353216) (f32 1077936128)

  , f64 4599676419421066581 == f64_divide_floor (f64 4607182418800017408) (f64 4613937818241073152)
  , f64 (4599676419421066581 + 1) == f64_divide_ceil (f64 4607182418800017408) (f64 4613937818241073152)
  ]





conversion_tests :: [Bool]
conversion_tests =
  [ True

  , 9.2233715e18 == f32_convert_i64_signed_floor 9223372036854775807
  , 9.223372e18 == f32_convert_i64_signed_ceil 9223372036854775807
  , 9.007199254740994e15 == f64_convert_i64_unsigned_floor 9007199254740995

  , f64 4845873199050653697 == f64_convert_i64_unsigned_floor 9007199254740995

  , f32 1 == f32_demote_f64_ceil (f64 3931642474694443008)
  , 0 == f32_demote_f64_floor (f64 3931642474694443008)

  , (f32 1) == f32_demote_f64_ceil (f64 1)
  , (f32 0) == f32_demote_f64_ceil (f64 0)
  , (f32 0) == f32_demote_f64_floor (f64 1)
  , (f32 0) == f32_demote_f64_floor (f64 0)

  ]


