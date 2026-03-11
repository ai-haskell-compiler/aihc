{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid restricted function" #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant negate" #-}

import Prelude hiding (isInfinite)
import Control.DeepSeq
import Control.Exception (SomeException, evaluate, try)
import Data.Maybe
import Data.Ord (Down(..))
import qualified GHC.Real
import System.IO.Unsafe (unsafePerformIO)

import Test.QuickCheck.Function
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty.TH

import Data.ExtendedReal

-- ----------------------------------------------------------------------

instance Arbitrary r => Arbitrary (Extended r) where
  arbitrary =
    oneof
    [ return NegInf
    , return PosInf
    , fmap Finite arbitrary
    ]

eval :: a -> Maybe a
eval a = unsafePerformIO $ do
  ret <- try (evaluate a)
  case ret of
    Left (_::SomeException) -> return Nothing
    Right b -> return $ Just b

isDefined :: a -> Bool
isDefined = isJust . eval

-- ----------------------------------------------------------------------

prop_add_comm :: Property
prop_add_comm =
  forAll arbitrary $ \(a :: Extended Rational) ->
  forAll arbitrary $ \b ->
    eval (a + b) == eval (b + a)

prop_add_assoc :: Property
prop_add_assoc =
  forAll arbitrary $ \(a :: Extended Rational) ->
  forAll arbitrary $ \b ->
  forAll arbitrary $ \c ->
    eval (a + (b + c)) == eval ((a + b) + c)

prop_add_unit :: Property
prop_add_unit =
  forAll arbitrary $ \(a :: Extended Rational) ->
    0 + a == a

prop_add_monotone :: Property
prop_add_monotone =
  forAll arbitrary $ \(a :: Extended Rational) ->
  forAll arbitrary $ \b ->
  forAll arbitrary $ \c ->
    a <= b && isDefined (a+c) && isDefined (b+c)
    ==> a+c <= b+c

prop_mult_comm :: Property
prop_mult_comm =
  forAll arbitrary $ \(a :: Extended Rational) ->
  forAll arbitrary $ \b ->
    a * b == b * a

-- PosInf + NegInf is left undefined
case_add_PosInf_NegInf :: IO ()
case_add_PosInf_NegInf =
  eval (inf + (- inf) :: Extended Rational) @?= Nothing

prop_mult_assoc :: Property
prop_mult_assoc =
  forAll arbitrary $ \(a :: Extended Rational) ->
  forAll arbitrary $ \b ->
  forAll arbitrary $ \c ->
    a * (b * c) == (a * b) * c

prop_mult_unit :: Property
prop_mult_unit =
  forAll arbitrary $ \(a :: Extended Rational) ->
    1 * a == a

prop_mult_dist :: Property
prop_mult_dist =
  forAll arbitrary $ \(a :: Extended Rational) ->
  forAll arbitrary $ \b ->
  forAll arbitrary $ \c ->
    isDefined (a * (b + c)) && isDefined (a * b + a * c)
    ==> eval (a * (b + c)) == eval (a * b + a * c)

prop_mult_zero :: Property
prop_mult_zero =
  forAll arbitrary $ \(a :: Extended Rational) ->
    0 * a == 0

prop_mult_monotone :: Property
prop_mult_monotone =
  forAll arbitrary $ \(a :: Extended Rational) ->
  forAll arbitrary $ \b ->
  forAll arbitrary $ \c ->
    a <= b && c > 0 && isDefined (a*c) && isDefined (b*c)
    ==> a*c <= b*c

prop_mult_down_1 :: Property
prop_mult_down_1 = once $
  fromRealFloat (sqr infinity) === sqr (fromRealFloat infinity)
  where
    infinity :: Down Double
    infinity = Down (1 / 0)

    sqr :: Num a => a -> a
    sqr x = x * x

prop_mult_down_2 :: Property
prop_mult_down_2 = once $
  fromRealFloat (infinity * (-infinity)) === fromRealFloat infinity * fromRealFloat (-infinity)
  where
    infinity :: Down Double
    infinity = Down (1 / 0)

-- We define 0 * PosInf = 0
case_mult_zero_PosInf :: IO ()
case_mult_zero_PosInf =
  0 * inf @?= (0 :: Extended Rational)

-- We define 0 * NegInf = 0
case_mult_zero_NegInf :: IO ()
case_mult_zero_NegInf =
  0 * (- inf) @?= (0 :: Extended Rational)

prop_negate_inverse :: Property
prop_negate_inverse =
  forAll arbitrary $ \(a :: Extended Rational) ->
    negate (negate a) == a

prop_signum_abs :: Property
prop_signum_abs =
  forAll arbitrary $ \(a :: Extended Rational) ->
    signum a * abs a == a

prop_recip_inverse :: Property
prop_recip_inverse =
  forAll arbitrary $ \(a :: Extended Rational) ->
    isFinite a && a /= 0 ==> recip (recip a) == a

case_recip_PosInf :: IO ()
case_recip_PosInf = recip inf @?= (0 :: Extended Rational)

case_recip_NegInf :: IO ()
case_recip_NegInf = recip (- inf) @?= (0 :: Extended Rational)

prop_minBound_smallest :: Property
prop_minBound_smallest =
  forAll arbitrary $ \(a :: Extended Rational) ->
    minBound <= a

prop_maxBound_largest :: Property
prop_maxBound_largest =
  forAll arbitrary $ \(a :: Extended Rational) ->
    a <= maxBound

prop_isFinite_fromRational :: Property
prop_isFinite_fromRational =
  forAll arbitrary $ \a -> isFinite (fromRational a :: Extended Rational)

prop_fromRational_PosInf :: Property
prop_fromRational_PosInf = once $
  fromRational GHC.Real.infinity === (PosInf :: Extended Rational)

prop_fromRational_NegInf :: Property
prop_fromRational_NegInf = once $
  fromRational (-GHC.Real.infinity) === (NegInf :: Extended Rational)

prop_fromRational_NaN :: Property
prop_fromRational_NaN = once $ ioProperty $ do
  let nan :: Extended Double
      nan = fromRational GHC.Real.notANumber
  nan' <- try $ evaluate nan
  pure $ case nan' of
    Left (_ :: SomeException) -> True
    Right _ -> False

prop_isInfinite_PosInf :: Property
prop_isInfinite_PosInf = property $ isInfinite PosInf

prop_isInfinite_NegInf :: Property
prop_isInfinite_NegInf = property $ isInfinite NegInf

-- ----------------------------------------------------------------------
-- Functor

prop_Functor_id :: Property
prop_Functor_id =
  forAll arbitrary $ \(a :: Extended Integer) ->
    fmap id a == a

prop_Functor_comp :: Property
prop_Functor_comp =
  forAll arbitrary $ \(f :: Fun Integer Integer) ->
  forAll arbitrary $ \(g :: Fun Integer Integer) ->
  forAll arbitrary $ \(a :: Extended Integer) ->
    fmap (apply f . apply g) a == fmap (apply f) (fmap (apply g) a)

-- ----------------------------------------------------------------------
-- Show / Read

prop_read_show :: Property
prop_read_show =
  forAll arbitrary $ \(a :: Extended Rational) ->
    read (show a) == a

-- ----------------------------------------------------------------------
-- deepseq

prop_deepseq :: Property
prop_deepseq =
  forAll arbitrary $ \(a :: Extended Rational) ->
    a `deepseq` () == ()

-- ----------------------------------------------------------------------
-- fromRealFloat

prop_fromRealFloat_PosInf :: Property
prop_fromRealFloat_PosInf = once $
  fromRealFloat (1 / 0 :: Double) === PosInf

prop_fromRealFloat_NegInf :: Property
prop_fromRealFloat_NegInf = once $
  fromRealFloat (-(1 / 0) :: Double) === NegInf

prop_fromRealFloat_NaN :: Property
prop_fromRealFloat_NaN = once $ ioProperty $ do
  let nan = fromRealFloat (0 / 0 :: Double)
  nan' <- try $ evaluate nan
  pure $ case nan' of
    Left (_ :: SomeException) -> True
    Right _ -> False

prop_fromRealFloat_Down_NegInf :: Property
prop_fromRealFloat_Down_NegInf = once $
  fromRealFloat (1 / 0 :: Down Double) === NegInf

prop_fromRealFloat_Down_PosInf :: Property
prop_fromRealFloat_Down_PosInf = once $
  fromRealFloat (-(1 / 0) :: Down Double) === PosInf

prop_fromRealFloat_Down_NaN :: Property
prop_fromRealFloat_Down_NaN = once $ ioProperty $ do
  let nan = fromRealFloat (0 / 0 :: Down Double)
  nan' <- try $ evaluate nan
  pure $ case nan' of
    Left (_ :: SomeException) -> True
    Right _ -> False

-- ----------------------------------------------------------------------
-- toRealFloat

prop_toRealFloat_PosInf :: Property
prop_toRealFloat_PosInf = once $
  (1 / 0 :: Double) === toRealFloat PosInf

prop_toRealFloat_NegInf :: Property
prop_toRealFloat_NegInf = once $
  (-(1 / 0) :: Double) === toRealFloat NegInf

prop_toRealFloat_Down_NegInf :: Property
prop_toRealFloat_Down_NegInf = once $
  (1 / 0 :: Down Double) === toRealFloat NegInf

prop_toRealFloat_Down_PosInf :: Property
prop_toRealFloat_Down_PosInf = once $
  (-(1 / 0) :: Down Double) === toRealFloat PosInf

prop_toRealFloat_fromRealFloat :: Double -> Property
prop_toRealFloat_fromRealFloat x =
  toRealFloat (fromRealFloat x) === x

prop_fromRealFloat_toRealFloat :: Extended Double -> Property
prop_fromRealFloat_toRealFloat x =
  fromRealFloat (toRealFloat x) === x

-- ----------------------------------------------------------------------
-- Test harness

main :: IO ()
main = $(defaultMainGenerator)
