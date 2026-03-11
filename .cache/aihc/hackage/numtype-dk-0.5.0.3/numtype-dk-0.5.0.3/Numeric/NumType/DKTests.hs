{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Numeric.NumType.DKTests where

import Numeric.NumType.DK.Integers
import Prelude hiding ((*), (/), (+), (-), (^), pred, succ, negate, abs, signum)
import qualified Prelude as P ((*), (/), (+), (-), (^), pred, succ, negate, abs, signum)
import Data.Proxy
import Test.HUnit

-- Some “large” NumTypes.
neg12 = pred neg11
neg11 = pred neg10
neg10 = pred neg9

pos10 = succ pos9
pos11 = succ pos10
pos12 = succ pos11

-- | Compares a type level unary function with a value level unary function
  -- by converting 'NumType' to a @Num@. This assumes that the 'toNum'
  -- function is solid.
unaryTest :: (KnownNumType i, KnownNumType i', Num a, Eq a, Show a)
          => (Proxy i -> Proxy i') -> (a -> a) -> Proxy i -> Test
unaryTest f f' x = TestCase $ assertEqual
    "Unary function Num equivalence"
    (f' (toNum x)) (toNum (f x))

-- | 'unaryTest' with @Num a@ fixed to @Integer@. This is needed by
  -- 'testIncrDecr'.
unaryTest' :: (KnownNumType i, KnownNumType i')
          => (Proxy i -> Proxy i') -> (Integer -> Integer) -> Proxy i -> Test
unaryTest' = unaryTest

-- | Compares a type level binary function with a value level binary function
  -- by converting 'NumType' to 'Num'. This assumes that the 'toNum'
  -- function is solid.
binaryTest :: (KnownNumType i, KnownNumType i', KnownNumType i'', Num a, Eq a, Show a)
           => (Proxy i -> Proxy i' -> Proxy i'')
           -> (a -> a -> a)
           -> Proxy i -> Proxy i' -> Test
binaryTest f f' x y = TestCase $ assertEqual
    "Binary function Num equivalence"
    (f' (toNum x) (toNum y)) (toNum (f x y))

binaryTest' :: (KnownNumType i, KnownNumType i', KnownNumType i'', Num a, Eq a, Show a)
           => (Proxy i -> Proxy i' -> Proxy i'')
           -> (Integer -> Integer -> Integer)
           -> Proxy i -> Proxy i' -> Test
binaryTest' = binaryTest


-- | Test that conversion to 'Num a' works as expected. This is sort of a
  -- prerequisite for the other tests.
testAsIntegral = TestLabel "Num equivalence tests" $ TestList
    [ TestCase $ -12 @=? toNum neg12
    , TestCase $ -11 @=? toNum neg11
    , TestCase $ -10 @=? toNum neg10
    , TestCase $  -9 @=? toNum neg9
    , TestCase $  -8 @=? toNum neg8
    , TestCase $  -7 @=? toNum neg7
    , TestCase $  -6 @=? toNum neg6
    , TestCase $  -5 @=? toNum neg5
    , TestCase $  -4 @=? toNum neg4
    , TestCase $  -3 @=? toNum neg3
    , TestCase $  -2 @=? toNum neg2
    , TestCase $  -1 @=? toNum neg1
    , TestCase $   0 @=? toNum zero
    , TestCase $   1 @=? toNum pos1
    , TestCase $   2 @=? toNum pos2
    , TestCase $   3 @=? toNum pos3
    , TestCase $   4 @=? toNum pos4
    , TestCase $   5 @=? toNum pos5
    , TestCase $   6 @=? toNum pos6
    , TestCase $   7 @=? toNum pos7
    , TestCase $   8 @=? toNum pos8
    , TestCase $   9 @=? toNum pos9
    , TestCase $  10 @=? toNum pos10
    , TestCase $  11 @=? toNum pos11
    , TestCase $  12 @=? toNum pos12
    ] -- By induction all other NumTypes should be good if these are.

-- | Test incrementing and decrementing.
testIncrDecr = TestLabel "Increment and decrement tests" $ TestList
    [ t neg12
    , t neg11
    , t neg10
    , t neg9
    , t neg8
    , t neg7
    , t neg6
    , t neg5
    , t neg4
    , t neg3
    , t neg2
    , t neg1
    , t zero
    , t pos1
    , t pos2
    , t pos3
    , t pos4
    , t pos5
    , t pos6
    , t pos7
    , t pos8
    , t pos9
    , t pos10
    , t pos11
    , t pos12
    ] where
        t x = TestList [ unaryTest' (+ pos1) (P.+ 1) x
                       , unaryTest' (+ neg1) (P.- 1) x
                       , unaryTest' (\x -> x - neg1) (P.+ 1) x
                       , unaryTest' (\x -> x - pos1) (P.- 1) x
                       , unaryTest' pred P.pred x
                       , unaryTest' succ P.succ x
                       ]

-- | Test negation.
testNegate = TestLabel "Negation tests" $ TestList
    [ unaryTest negate P.negate neg12
    , unaryTest negate P.negate neg11
    , unaryTest negate P.negate neg10
    , unaryTest negate P.negate neg9
    , unaryTest negate P.negate neg8
    , unaryTest negate P.negate neg7
    , unaryTest negate P.negate neg6
    , unaryTest negate P.negate neg5
    , unaryTest negate P.negate neg4
    , unaryTest negate P.negate neg3
    , unaryTest negate P.negate neg2
    , unaryTest negate P.negate neg1
    , unaryTest negate P.negate zero
    , unaryTest negate P.negate pos1
    , unaryTest negate P.negate pos2
    , unaryTest negate P.negate pos3
    , unaryTest negate P.negate pos4
    , unaryTest negate P.negate pos5
    , unaryTest negate P.negate pos6
    , unaryTest negate P.negate pos7
    , unaryTest negate P.negate pos8
    , unaryTest negate P.negate pos9
    , unaryTest negate P.negate pos10
    , unaryTest negate P.negate pos11
    , unaryTest negate P.negate pos12
    ]

-- | Test absolute value.
testAbs = TestLabel "Absolute value tests" $ TestList
    [ unaryTest abs P.abs neg12
    , unaryTest abs P.abs neg11
    , unaryTest abs P.abs neg10
    , unaryTest abs P.abs neg9
    , unaryTest abs P.abs neg8
    , unaryTest abs P.abs neg7
    , unaryTest abs P.abs neg6
    , unaryTest abs P.abs neg5
    , unaryTest abs P.abs neg4
    , unaryTest abs P.abs neg3
    , unaryTest abs P.abs neg2
    , unaryTest abs P.abs neg1
    , unaryTest abs P.abs zero
    , unaryTest abs P.abs pos1
    , unaryTest abs P.abs pos2
    , unaryTest abs P.abs pos3
    , unaryTest abs P.abs pos4
    , unaryTest abs P.abs pos5
    , unaryTest abs P.abs pos6
    , unaryTest abs P.abs pos7
    , unaryTest abs P.abs pos8
    , unaryTest abs P.abs pos9
    , unaryTest abs P.abs pos10
    , unaryTest abs P.abs pos11
    , unaryTest abs P.abs pos12
    ]

-- | Test signum.
testSignum = TestLabel "Signum tests" $ TestList
    [ unaryTest signum P.signum neg12
    , unaryTest signum P.signum neg11
    , unaryTest signum P.signum neg10
    , unaryTest signum P.signum neg9
    , unaryTest signum P.signum neg8
    , unaryTest signum P.signum neg7
    , unaryTest signum P.signum neg6
    , unaryTest signum P.signum neg5
    , unaryTest signum P.signum neg4
    , unaryTest signum P.signum neg3
    , unaryTest signum P.signum neg2
    , unaryTest signum P.signum neg1
    , unaryTest signum P.signum zero
    , unaryTest signum P.signum pos1
    , unaryTest signum P.signum pos2
    , unaryTest signum P.signum pos3
    , unaryTest signum P.signum pos4
    , unaryTest signum P.signum pos5
    , unaryTest signum P.signum pos6
    , unaryTest signum P.signum pos7
    , unaryTest signum P.signum pos8
    , unaryTest signum P.signum pos9
    , unaryTest signum P.signum pos10
    , unaryTest signum P.signum pos11
    , unaryTest signum P.signum pos12
    ]

-- | Test absolute value.
testAbs = TestLabel "Absolute value tests" $ TestList
    [ unaryTest abs P.abs neg2
    , unaryTest abs P.abs neg1
    , unaryTest abs P.abs zero
    , unaryTest abs P.abs pos1
    , unaryTest abs P.abs pos1
    ]

-- | Test signum.
testSignum = TestLabel "Signum tests" $ TestList
    [ unaryTest signum P.signum neg2
    , unaryTest signum P.signum neg1
    , unaryTest signum P.signum zero
    , unaryTest signum P.signum pos1
    , unaryTest signum P.signum pos1
    ]

-- | Test addition.
testAddition = TestLabel "Addition tests" $ TestList
    [ binaryTest (+) (P.+) pos2 pos5
    , binaryTest (+) (P.+) pos12 pos2
    , binaryTest (+) (P.+) pos11 pos2
    , binaryTest (+) (P.+) pos10 pos2
    , binaryTest (+) (P.+) pos2 pos12
    , binaryTest (+) (P.+) pos2 pos11
    , binaryTest (+) (P.+) pos2 pos10
    , binaryTest (+) (P.+) neg2 pos5
    , binaryTest (+) (P.+) pos2 neg5
    , binaryTest (+) (P.+) neg2 neg5
    , binaryTest (+) (P.+) neg2 neg12
    , binaryTest (+) (P.+) neg2 neg11
    , binaryTest (+) (P.+) neg2 neg10
    , binaryTest (+) (P.+) neg12 neg2
    , binaryTest (+) (P.+) neg11 neg2
    , binaryTest (+) (P.+) neg10 neg2
    , binaryTest (+) (P.+) zero zero
    , binaryTest (+) (P.+) pos2 zero
    , binaryTest (+) (P.+) neg2 zero
    , binaryTest (+) (P.+) zero pos5
    , binaryTest (+) (P.+) zero neg5
    ]

-- | Test subtraction.
testSubtraction = TestLabel "Subtraction tests" $ TestList
    [ binaryTest (-) (P.-) pos2 pos5
    , binaryTest (-) (P.-) pos12 pos2
    , binaryTest (-) (P.-) pos11 pos2
    , binaryTest (-) (P.-) pos10 pos2
    , binaryTest (-) (P.-) pos2 pos12
    , binaryTest (-) (P.-) pos2 pos11
    , binaryTest (-) (P.-) pos2 pos10
    , binaryTest (-) (P.-) neg2 pos5
    , binaryTest (-) (P.-) pos2 neg5
    , binaryTest (-) (P.-) neg2 neg5
    , binaryTest (-) (P.-) neg2 neg12
    , binaryTest (-) (P.-) neg2 neg11
    , binaryTest (-) (P.-) neg2 neg10
    , binaryTest (-) (P.-) neg12 neg2
    , binaryTest (-) (P.-) neg11 neg2
    , binaryTest (-) (P.-) neg10 neg2
    , binaryTest (-) (P.-) zero zero
    , binaryTest (-) (P.-) pos2 zero
    , binaryTest (-) (P.-) neg2 zero
    , binaryTest (-) (P.-) zero pos5
    , binaryTest (-) (P.-) zero neg5
    ]

-- | Test multiplication.
testMultiplication = TestLabel "Multiplication tests" $ TestList
    [ binaryTest (*) (P.*) pos2 pos5
    , binaryTest (*) (P.*) pos12 pos2
    , binaryTest (*) (P.*) pos11 pos2
    , binaryTest (*) (P.*) pos10 pos2
    , binaryTest (*) (P.*) pos2 pos12
    , binaryTest (*) (P.*) pos2 pos11
    , binaryTest (*) (P.*) pos2 pos10
    , binaryTest (*) (P.*) neg2 pos5
    , binaryTest (*) (P.*) pos2 neg5
    , binaryTest (*) (P.*) neg2 neg5
    , binaryTest (*) (P.*) neg2 neg12
    , binaryTest (*) (P.*) neg2 neg11
    , binaryTest (*) (P.*) neg2 neg10
    , binaryTest (*) (P.*) neg12 neg2
    , binaryTest (*) (P.*) neg11 neg2
    , binaryTest (*) (P.*) neg10 neg2
    , binaryTest (*) (P.*) zero zero
    , binaryTest (*) (P.*) pos2 zero
    , binaryTest (*) (P.*) neg2 zero
    , binaryTest (*) (P.*) zero pos5
    , binaryTest (*) (P.*) zero neg5
    -- Probably some duplicates in the below.
    , binaryTest (*) (P.*) pos10 pos2
    , binaryTest (*) (P.*) pos9 pos3
    , binaryTest (*) (P.*) pos8 pos4
    , binaryTest (*) (P.*) pos8 pos2
    , binaryTest (*) (P.*) pos6 pos3
    , binaryTest (*) (P.*) pos6 pos2
    , binaryTest (*) (P.*) pos4 pos2
    , binaryTest (*) (P.*) pos9 neg3
    , binaryTest (*) (P.*) pos8 neg4
    , binaryTest (*) (P.*) pos8 neg2
    , binaryTest (*) (P.*) pos6 neg3
    , binaryTest (*) (P.*) pos6 neg2
    , binaryTest (*) (P.*) pos4 neg2
    , binaryTest (*) (P.*) zero pos5
    , binaryTest (*) (P.*) zero neg3
    , binaryTest (*) (P.*) neg4 pos2
    , binaryTest (*) (P.*) neg6 pos2
    , binaryTest (*) (P.*) neg6 pos3
    , binaryTest (*) (P.*) neg8 pos2
    , binaryTest (*) (P.*) neg8 pos4
    , binaryTest (*) (P.*) neg9 pos3
    , binaryTest (*) (P.*) neg4 neg2
    , binaryTest (*) (P.*) neg6 neg2
    , binaryTest (*) (P.*) neg6 neg3
    , binaryTest (*) (P.*) neg8 neg2
    , binaryTest (*) (P.*) neg8 neg4
    , binaryTest (*) (P.*) neg9 neg3
    , binaryTest (*) (P.*) neg12 pos4
    ]

-- | Test division.
testDivision = TestLabel "Division tests" $ TestList
    [ binaryTest (/) (P./) pos12 pos4
    , binaryTest (/) (P./) pos12 neg4
    , binaryTest (/) (P./) pos10 pos5
    , binaryTest (/) (P./) pos10 pos2
    , binaryTest (/) (P./) pos9 pos3
    , binaryTest (/) (P./) pos8 pos4
    , binaryTest (/) (P./) pos8 pos2
    , binaryTest (/) (P./) pos6 pos3
    , binaryTest (/) (P./) pos6 pos2
    , binaryTest (/) (P./) pos4 pos2
    , binaryTest (/) (P./) pos9 neg3
    , binaryTest (/) (P./) pos8 neg4
    , binaryTest (/) (P./) pos8 neg2
    , binaryTest (/) (P./) pos6 neg3
    , binaryTest (/) (P./) pos6 neg2
    , binaryTest (/) (P./) pos4 neg2
    , binaryTest (/) (P./) zero pos5
    , binaryTest (/) (P./) zero neg3
    , binaryTest (/) (P./) neg4 pos2
    , binaryTest (/) (P./) neg6 pos2
    , binaryTest (/) (P./) neg6 pos3
    , binaryTest (/) (P./) neg8 pos2
    , binaryTest (/) (P./) neg8 pos4
    , binaryTest (/) (P./) neg9 pos3
    , binaryTest (/) (P./) neg4 neg2
    , binaryTest (/) (P./) neg6 neg2
    , binaryTest (/) (P./) neg6 neg3
    , binaryTest (/) (P./) neg8 neg2
    , binaryTest (/) (P./) neg8 neg4
    , binaryTest (/) (P./) neg9 neg3
    , binaryTest (/) (P./) neg12 pos4
    , binaryTest (/) (P./) neg12 neg4
    , binaryTest (/) (P./) neg10 neg5
    , binaryTest (/) (P./) neg10 neg2

    , binaryTest (/) (P./) pos5 pos5
    , binaryTest (/) (P./) neg5 pos5
    , binaryTest (/) (P./) pos5 neg5
    , binaryTest (/) (P./) zero neg5
    , binaryTest (/) (P./) zero pos5
    , binaryTest (/) (P./) pos5 pos1
    , binaryTest (/) (P./) pos5 neg1

    , binaryTest (/) (P./) pos12 pos12
    , binaryTest (/) (P./) neg12 pos12
    , binaryTest (/) (P./) pos12 neg12
    , binaryTest (/) (P./) zero neg12
    , binaryTest (/) (P./) zero pos12
    , binaryTest (/) (P./) pos12 pos1
    , binaryTest (/) (P./) pos12 neg1
    ]

-- | Test exponentiation.
testExponentiation = TestLabel "Exponentiation tests" $ TestList
    [ binaryTest (^) (P.^) pos2 pos3
    , binaryTest (^) (P.^) zero pos5
    , binaryTest (^) (P.^) neg2 pos3

    , binaryTest (^) (P.^) pos3 pos3
    , binaryTest (^) (P.^) pos3 pos2
    , binaryTest (^) (P.^) pos3 pos1
    , binaryTest (^) (P.^) pos2 pos4
    , binaryTest (^) (P.^) pos2 pos3
    , binaryTest (^) (P.^) pos2 pos2
    , binaryTest (^) (P.^) pos2 pos1
    , binaryTest (^) (P.^) pos1 pos12
    , binaryTest (^) (P.^) pos1 pos9
    , binaryTest (^) (P.^) pos1 zero

    , binaryTest (^) (P.^) neg3 pos3
    , binaryTest (^) (P.^) neg3 pos2
    , binaryTest (^) (P.^) neg3 pos1
    , binaryTest (^) (P.^) neg2 pos4
    , binaryTest (^) (P.^) neg2 pos3
    , binaryTest (^) (P.^) neg2 pos2
    , binaryTest (^) (P.^) neg2 pos1
    , binaryTest (^) (P.^) neg1 pos12
    , binaryTest (^) (P.^) neg1 pos9
    , binaryTest (^) (P.^) neg1 zero
    ]

-- | Collect the test cases.
tests = TestList
    [ testAsIntegral
    , testIncrDecr
    , testNegate
    , testAbs
    , testSignum
    , testAddition
    , testSubtraction
    , testMultiplication
    , testDivision
    , testExponentiation
    ]

main = runTestTT tests
