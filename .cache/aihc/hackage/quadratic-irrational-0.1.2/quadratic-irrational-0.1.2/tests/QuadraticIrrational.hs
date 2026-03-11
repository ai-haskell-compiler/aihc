{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ViewPatterns         #-}

module QuadraticIrrational (tests) where

import Data.Number.CReal (CReal)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

import Numeric.QuadraticIrrational
import Numeric.QuadraticIrrational.Internal.Lens

-- Slow but precise.
type RefFloat = CReal

instance Arbitrary QI where
  arbitrary = consQI <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    where
     consQI a b (NonNegative c) (NonZero d) = qi a b c d

  shrink (unQI -> ~(a,b,c,d)) =
    [ qi a' b  c  d  | a' <- shrink a ] ++
    [ qi a  b' c  d  | b' <- shrink b ] ++
    [ qi a  b  c' d  | NonNegative c' <- shrink (NonNegative c) ] ++
    [ qi a  b  c  d' | NonZero     d' <- shrink (NonZero     d) ]

tests :: TestTree
tests =
  testGroup "QuadraticIrrational"
    [ testGroup "Construction/destruction/conversion"
      [ testProperty "qi/runQI" $ \a b (NonNegative c) (NonZero d) ->
          runQI (qi a b c d) $ \a' b' c' d' ->
            approxEq' (approxQI a b c d) (approxQI a' b' c' d')

      , testProperty "qi/runQI'" $ \a b (NonNegative c) (NonZero d) ->
          runQI' (qi a b c d) $ \a' b' c' ->
            approxEq' (approxQI a b c d) (approxQI' a' b' c')

      , testProperty "qi'/runQI" $ \a b (NonNegative c) ->
          runQI (qi' a b c) $ \a' b' c' d' ->
            approxEq' (approxQI' a b c) (approxQI a' b' c' d')

      , testProperty "qi'/runQI'" $ \a b (NonNegative c) ->
          runQI' (qi' a b c) $ \a' b' c' ->
            approxEq' (approxQI' a b c) (approxQI' a' b' c')
      ]
    , testGroup "Lenses"
      [ testProperty "_qi" $ \n a' b' (NonNegative c') (NonZero d') ->
          let n'  = over _qi (\(a,b,c,d) -> (a+a',b-b',c*c',d*d')) n
              n'' = runQI n $ \a b c d -> qi (a+a') (b-b') (c*c') (d*d')
          in  approxEq (qiToFloat n') (qiToFloat n'')

      , testProperty "_qi'" $ \n a' b' (NonNegative c') ->
          let n'  = over _qi' (\(a,b,c) -> (a+a',b-b',c*c')) n
              n'' = runQI' n $ \a b c -> qi' (a+a') (b-b') (c*c')
          in  approxEq (qiToFloat n') (qiToFloat n'')

      , testProperty "_qiABD" $ \n a' b' (NonZero d') ->
          let n'  = over _qiABD (\(a,b,d) -> (a+a',b-b',d*d')) n
              n'' = runQI n $ \a b c d -> qi (a+a') (b-b') c (d*d')
          in  approxEq (qiToFloat n') (qiToFloat n'')

      , testProperty "_qiA" $ \n a' ->
          let n'  = over _qiA (+ a') n
              n'' = runQI n $ \a b c d -> qi (a+a') b c d
          in  approxEq (qiToFloat n') (qiToFloat n'')

      , testProperty "_qiB" $ \n b' ->
          let n'  = over _qiB (+ b') n
              n'' = runQI n $ \a b c d -> qi a (b+b') c d
          in  approxEq (qiToFloat n') (qiToFloat n'')

      , testProperty "_qiC" $ \n (NonNegative c') ->
          let n'  = over _qiC (* c') n
              n'' = runQI n $ \a b c d -> qi a b (c*c') d
          in  approxEq (qiToFloat n') (qiToFloat n'')

      , testProperty "_qiD" $ \n (NonZero d') ->
          let n'  = over _qiD (* d') n
              n'' = runQI n $ \a b c d -> qi a b c (d*d')
          in  approxEq (qiToFloat n') (qiToFloat n'')
      ]
    , testGroup "Numerical operations"
      [ testProperty "qiToFloat" $ \a b (NonNegative c) (NonZero d) ->
          approxEq' (qiToFloat (qi a b c d)) (approxQI a b c d)

      , testProperty "compare equals" $ \a ->
          conjoin [ a === a ]
            `const` (a :: QI)

      , testProperty "qiAddI" $ \n x ->
          approxEq' (qiToFloat (qiAddI n x)) (qiToFloat n + fromInteger x)

      , testProperty "qiSubI" $ \n x ->
          approxEq' (qiToFloat (qiSubI n x)) (qiToFloat n - fromInteger x)

      , testProperty "qiMulI" $ \n x ->
          approxEq' (qiToFloat (qiMulI n x)) (qiToFloat n * fromInteger x)

      , testProperty "qiDivI" $ \n x ->
          x /= 0 ==>
            approxEq' (qiToFloat (qiDivI n x)) (qiToFloat n / fromInteger x)

      , testProperty "qiAddR" $ \n x ->
          approxEq' (qiToFloat (qiAddR n x)) (qiToFloat n + fromRational x)

      , testProperty "qiSubR" $ \n x ->
          approxEq' (qiToFloat (qiSubR n x)) (qiToFloat n - fromRational x)

      , testProperty "qiMulR" $ \n x ->
          approxEq' (qiToFloat (qiMulR n x)) (qiToFloat n * fromRational x)

      , testProperty "qiDivR" $ \n x ->
          x /= 0 ==>
            approxEq' (qiToFloat (qiDivR n x)) (qiToFloat n / fromRational x)

      , testProperty "qiNegate" $ \n ->
          approxEq' (qiToFloat (qiNegate n)) (negate (qiToFloat n))

      , testProperty "qiRecip" $ \n ->
          not (approxEq (qiToFloat n) 0)
            ==> let ~(Just nr) = qiRecip n
                in  approxEq' (qiToFloat nr) (recip (qiToFloat n))

      , testProperty "qiAdd" . withCompatiblePair $ \n n' ->
          let ~(Just r) = qiAdd n n'
          in  approxEq' (qiToFloat r) (qiToFloat n + qiToFloat n')

      , testProperty "qiSub" . withCompatiblePair $ \n n' ->
          let ~(Just r) = qiSub n n'
          in  approxEq' (qiToFloat r) (qiToFloat n - qiToFloat n')

      , testProperty "qiMul" . withCompatiblePair $ \n n' ->
          let ~(Just r) = qiMul n n'
          in  approxEq' (qiToFloat r) (qiToFloat n * qiToFloat n')

      , testProperty "qiDiv" . withCompatiblePair $ \n n' ->
          let ~(Just r) = qiDiv n n'
          in  not (approxEq (qiToFloat n') 0)
                ==> approxEq' (qiToFloat r) (qiToFloat n / qiToFloat n')

      , testProperty "qiPow" $ \n (NonNegative p) ->
          -- Limit the power for speed.
          (p <= 10) ==>
            approxEq' (qiToFloat (qiPow n p))
                      -- CReal seems to diverge in 0 ** 1, use (^).
                      (qiToFloat n ^ p)

      , testProperty "qiFloor" $ \n ->
          qiFloor n === floor (qiToFloat n :: RefFloat)
      ]
    , testGroup "Continued fractions"
      [ testProperty "qiToContinuedFraction/continuedFractionToQI" $ \n ->
          let cf@(_, CycList _ xs)  = qiToContinuedFraction n
          -- Limit the length of the periodic part for speed.
          in (length xs <= 100) ==>
              (qiToFloat n :: Double) === qiToFloat (continuedFractionToQI cf)

      , testProperty "continuedFractionApproximate" $ \n ->
          let cf = qiToContinuedFraction n
              n' = continuedFractionApproximate 20 cf
          in  approxEq' (qiToFloat n) (fromRational n')
      ]
    ]

withCompatiblePair :: Testable p
                   => (QI -> QI -> p) -> QI -> QI -> Property
withCompatiblePair f n0_ n1_ =
  counterexample ("n0 = " ++ show n0) . counterexample ("n1 = " ++ show n1) $
    f n0 n1
  where
    n0 = runQI n0_ $ \a b c d ->
           qi a b c d

    n1 = runQI n0_ $ \_ _ c _ -> runQI n1_ $ \a b _ d ->
           qi a b c d

approxQI :: Integer -> Integer -> Integer -> Integer -> RefFloat
approxQI a b c d =
  (fromInteger a + fromInteger b * sqrt (fromInteger c)) / fromInteger d

approxQI' :: Rational -> Rational -> Integer -> RefFloat
approxQI' a b c =
  fromRational a + fromRational b * sqrt (fromInteger c)

approxEq :: RefFloat -> RefFloat -> Bool
approxEq a b = abs (b - a) < 1e-6 * maximum [ 1, abs a, abs b ]

approxEq' :: RefFloat -> RefFloat -> Property
approxEq' a b =
  counterexample (show a ++ " is not approximately " ++ show b ++ " (diff = "
                   ++ show (abs (b - a)) ++ ")")
                 (approxEq a b)
