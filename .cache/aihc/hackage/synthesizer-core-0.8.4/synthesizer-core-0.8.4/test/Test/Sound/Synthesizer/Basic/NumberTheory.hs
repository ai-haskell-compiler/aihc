{-# LANGUAGE NoImplicitPrelude #-}
module Test.Sound.Synthesizer.Basic.NumberTheory (tests) where

import Synthesizer.Basic.NumberTheory (Order(Order), )
import qualified Synthesizer.Basic.NumberTheory as NT

import Control.Applicative ((<$>), )

import qualified Data.List.HT as ListHT
import qualified Data.Set as Set
import qualified Data.Bits as Bit

import qualified Test.QuickCheck as QC
import Test.QuickCheck (Testable, Arbitrary, arbitrary, quickCheck, )

import qualified Algebra.Absolute              as Absolute

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


newtype Cardinal a = Cardinal a
   deriving (Show)

instance (Absolute.C a, Arbitrary a) => Arbitrary (Cardinal a) where
   arbitrary = fmap (Cardinal . abs) arbitrary


newtype Positive a = Positive a
   deriving (Show)

instance (Absolute.C a, Arbitrary a) => Arbitrary (Positive a) where
   arbitrary = fmap (Positive . (1+) . abs) arbitrary


newtype Prime = Prime Integer
   deriving (Show)

instance Arbitrary Prime where
   arbitrary = fmap Prime $ QC.suchThat (QC.choose (2,10000)) NT.isPrime


newtype Big = Big Integer
   deriving (Show)

instance Arbitrary Big where
   arbitrary = do
      digits <- QC.listOf arbitrary
      -- negative digits yield numbers close to the maximum
      let maxi = 10^50
      return $ Big $
         foldl (\acc d -> mod (Bit.shiftL acc 16 + d) maxi) 0 digits


simple ::
   (Testable t, Arbitrary (wrapper Integer), Show (wrapper Integer)) =>
   (wrapper Integer -> t) -> IO ()
simple = quickCheck

singleArgs :: QC.Args
singleArgs = QC.stdArgs {QC.maxSuccess = 1}

tests :: [(String, IO ())]
tests =
   ("multiplicativeGenerator set vs. divisor",
      quickCheck $ \(Prime n) ->
         NT.multiplicativeGeneratorSet n
         ==
         NT.multiplicativeGeneratorDivisors n) :
   ("primitiveRootsOfUnity naive vs. power",
      simple $ \(Cardinal m) order ->
         NT.primitiveRootsOfUnityNaive m order
         ==
         NT.primitiveRootsOfUnityPower m order) :
   ("primitiveRootsOfUnity naive vs. fullorbit",
      simple $ \(Cardinal m) order ->
         NT.primitiveRootsOfUnityNaive m order
         ==
         (Set.toAscList $ Set.fromList $
          NT.primitiveRootsOfUnityFullOrbit m order)) :
   ("Carmichael theorem",
      simple $ \(Positive a) (Positive b) ->
         NT.getOrder (NT.maximumOrderOfPrimitiveRootsOfUnity (lcm a b))
         ==
         lcm
            (NT.getOrder (NT.maximumOrderOfPrimitiveRootsOfUnity a))
            (NT.getOrder (NT.maximumOrderOfPrimitiveRootsOfUnity b))) :
   ("maximumOrderOfPrimitiveRootsOfUnity naive vs. integer",
      simple $ \(Positive m) ->
         NT.maximumOrderOfPrimitiveRootsOfUnityNaive m
         ==
         NT.maximumOrderOfPrimitiveRootsOfUnityInteger m) :
   ("number of rootsOfUnityPower, lcm",
      simple $ \(Positive m) ao@(Order a) bo@(Order b) ->
         let g = length . NT.rootsOfUnityPower m
         in  g (Order $ lcm a b) == lcm (g ao) (g bo)) :
   ("ringsWithPrimitiveRootsOfUnityAndUnits: minimal modulus",
      quickCheck $ \order@(Order expo) ->
         {-
         Often equality holds, but not always.
         Smallest counter-example: expo=80.
         -}
         (head $ NT.ringsWithPrimitiveRootOfUnityAndUnit order)
         >=
         (head $ NT.ringsWithPrimitiveRootsOfUnityAndUnitsNaive
            [order] [expo])) :
   ("combine two rings with primitive roots of certain orders",
      quickCheck $ \m n ->
         let r = lcm
                   (head (NT.ringsWithPrimitiveRootOfUnityAndUnit m))
                   (head (NT.ringsWithPrimitiveRootOfUnityAndUnit n))
         in  NT.hasPrimitiveRootOfUnityInteger r m
             &&
             NT.hasPrimitiveRootOfUnityInteger r n) :
   ("combine many rings with primitive roots of certain orders",
      quickCheck $ QC.forAll (take 3 <$> QC.listOf1 (QC.choose (1,10))) $ \ns ->
         let order = NT.lcmMulti ns
         in  take 3 (NT.ringsWithPrimitiveRootsOfUnityAndUnitsNaive
                       (map Order ns) ns)
             ==
             take 3 (NT.ringsWithPrimitiveRootsOfUnityAndUnitsNaive
                       [Order order] [order])) :
{-
Unfortunately rings with certain units cannot be combined
while maintaining these elements as units.

Counterexample:
   ringsWithPrimitiveRootOfUnityAndUnit 2 = 3:...
   ringsWithPrimitiveRootOfUnityAndUnit 3 = 7:...
   But in Z_{3·7} the number 3 is no unit.

   ("combine rings with certain units",
      quickCheck $ \(Positive m) (Positive n) ->
         let r = fromIntegral $ lcm
                (head (NT.ringsWithPrimitiveRootOfUnityAndUnit m))
                (head (NT.ringsWithPrimitiveRootOfUnityAndUnit n))
         in  PID.coprime r m && PID.coprime r n) :
-}
   ("number of roots of unity lcm",
      quickCheck $ \(Positive n) (Positive k) (Positive l) ->
         let orders = NT.ordersOfRootsOfUnityInteger !! (n-1)
         in  lcm (orders!!(k-1)) (orders!!(l-1))
             ==
             orders !! (lcm k l - 1)) :
   ("number of roots of unity vs. primitive roots",
      quickCheck $ \(Positive n) (Positive k) ->
         (sum $ map snd $
          filter (flip divides k . fst) $
          zip
             [1..]
             (NT.ordersOfPrimitiveRootsOfUnityInteger !! (n-1)))
         ==
         NT.ordersOfRootsOfUnityInteger !! (n-1) !! (k-1)) :
   ("divideByMaximumPower",
      QC.quickCheck $
         QC.forAll (QC.choose (2,10::Integer)) $ \b (Positive n) ->
         NT.divideByMaximumPower b n == NT.divideByMaximumPowerRecursive b n) :
   ("numbers3Smooth",
      QC.quickCheckWith singleArgs $ ListHT.allEqual $ map (take 10000) $
         [NT.numbers3SmoothCorec, NT.numbers3SmoothFoldr, NT.numbers3SmoothSet]) :
   ("numbers5Smooth",
      QC.quickCheckWith singleArgs $ ListHT.allEqual $ map (take 10000) $
         [NT.numbers5SmoothCorec, NT.numbers5SmoothFoldr, NT.numbers5SmoothSet]) :
   ("ceiling3Smooth vs. is3Smooth",
      quickCheck $ \(Positive n) -> NT.is3Smooth $ NT.ceiling3Smooth n) :
   ("ceiling5Smooth vs. is5Smooth",
      quickCheck $ \(Positive n) -> NT.is5Smooth $ NT.ceiling5Smooth n) :
   ("ceiling3Smooth vs. numbers3Smooth",
      quickCheck $ QC.forAll (QC.choose (0,500)) $ \k ->
         let (n0:n1:_) = drop k NT.numbers3Smooth
         in  NT.ceiling3Smooth n0 == n0
             &&
             NT.ceiling3Smooth (n0+1) == n1) :
   ("ceiling5Smooth vs. numbers5Smooth",
      quickCheck $ QC.forAll (QC.choose (0,500)) $ \k ->
         let (n0:n1:_) = drop k NT.numbers5Smooth
         in  NT.ceiling5Smooth n0 == n0
             &&
             NT.ceiling5Smooth (n0+1) == n1) :
   ("ceiling3Smooth naive vs. trace",
      quickCheck $ \(Positive n) ->
         NT.ceiling3SmoothNaive n == NT.ceiling3SmoothTrace n) :
   ("ceiling5Smooth naive vs. trace",
      quickCheck $ \(Positive n) ->
         NT.ceiling5SmoothNaive n == NT.ceiling5SmoothTrace n) :
   ("ceiling3Smooth scan vs. trace",
      quickCheck $ \(Big n) ->
         NT.ceiling3SmoothScan n == NT.ceiling3SmoothTrace n) :
   ("ceiling5Smooth scan vs. trace",
      quickCheck $ \(Big n) ->
         NT.ceiling5SmoothScan n == NT.ceiling5SmoothTrace n) :
   []
