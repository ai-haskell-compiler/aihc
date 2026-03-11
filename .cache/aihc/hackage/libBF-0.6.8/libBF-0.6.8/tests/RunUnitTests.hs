{-# Language BlockArguments #-}
module Main(main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@=?), assertFailure, testCase)

import LibBF


main :: IO ()
main =
  defaultMain $
  testGroup "LibBF tests"
    [ testGroup "bfToString"
        [ testCase "NaN" $
            "NaN" @=? bfToString 16 (showFree Nothing) bfNaN
        ]
    , testGroup "bfFromString"
        [ testCase "Underflow" $
            let (_, status) =
                  bfFromString 10 (expBits 3 <> precBits 2 <> rnd ToZero) "0.001" in
            True @=? statusUnderflow status
        , testCase "Overflow" $
            let (_, status) =
                  bfFromString 10 (expBits 3 <> precBits 2 <> rnd ToZero) "1.0e200" in
            True @=? statusOverflow status
        ]
    , testGroup "bfAdd"
        [ dblTestCase "+" (+) (bfAdd (float64 NearEven)) 1 2
        ]
    , testGroup "bfDiv"
        [ dblTestCase "/" (/) (bfDiv (float64 NearEven)) 1 0
        ]
    , testGroup "bfIsSubnormal (float32 NearEven)"
        (map (\bf -> bfSubnormalTestCase bf False)
             [bfPosZero, bfFromInt 1, bfFromInt 0, bfNaN, bfNegInf, bfPosInf])
    , testGroup "IEEE 754 compare"
      [ testGroup "Comparisons with NaN should always return False"
        [ testCase "NaN > 0" $ False @=? bfNaN > bfPosZero
        , testCase "0 > NaN" $ False @=? bfPosZero > bfNaN
        , testCase "NaN >= 0" $ False @=? bfNaN >= bfPosZero
        , testCase "0 >= NaN" $ False @=? bfPosZero >= bfNaN
        , testCase "NaN < 0" $ False @=? bfNaN < bfPosZero
        , testCase "0 < NaN" $ False @=? bfPosZero < bfNaN
        , testCase "NaN <= 0" $ False @=? bfNaN <= bfPosZero
        , testCase "0 <= NaN" $ False @=? bfPosZero <= bfNaN
        ]
      ]
    ]

statusUnderflow :: Status -> Bool
statusUnderflow Underflow = True
statusUnderflow _         = False

statusOverflow :: Status -> Bool
statusOverflow Overflow = True
statusOverflow _        = False

-- Check that a binary operation over BigFloats returns the same result as the
-- corresponding operation over doubles.
dblTestCase ::
  String ->
  (Double -> Double -> Double) ->
  (BigFloat -> BigFloat -> (BigFloat, Status)) ->
  Double -> Double -> TestTree
dblTestCase op opD opBF x y =
  testCase (unwords [show x, op, show y]) $
  case z1 of
    Left err -> assertFailure ("status: " ++ err)
    Right actual -> expected @=? actual
  where
  expected = opD x y
  z1 = case opBF (bfFromDouble x) (bfFromDouble y) of
        (res,_) ->
          case bfToDouble NearEven res of
            (res1,Ok) -> Right res1
            (_, s)    -> Left ("result: " ++ show s)

-- Check that calling bfIsSubnormal on a BigFloat value returns the expected
-- result.
bfSubnormalTestCase :: BigFloat -> Bool -> TestTree
bfSubnormalTestCase bf expected =
  testCase (show bf) $
  expected @=? bfIsSubnormal (float32 NearEven) bf
