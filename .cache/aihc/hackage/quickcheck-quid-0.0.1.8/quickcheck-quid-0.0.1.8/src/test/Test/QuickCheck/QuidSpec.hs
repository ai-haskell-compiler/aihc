{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.QuickCheck.QuidSpec
    where

import Control.Arrow
    ( (&&&) )
import Control.Monad
    ( replicateM )
import Data.Functor
    ( (<&>) )
import Data.Maybe
    ( fromMaybe )
import Data.Numbers.Primes
    ( primes )
import Data.Ord
    ( Down (..) )
import Data.Set
    ( Set )
import Data.Text.Lazy.Builder
    ( Builder, fromLazyText )
import Fmt
    ( Buildable (..), indentF, padLeftF, pretty, (+|), (|+) )
import Internal.Test.QuickCheck.Quid
    ( Quid
    , arbitraryQuid
    , chooseNatural
    , naturalToQuid
    , quidToNatural
    , shrinkNatural
    )
import Internal.Test.QuickCheck.Quid.Combinators.Size
    ( Size (..) )
import Internal.Test.QuickCheck.Quid.Representations.Decimal
    ( Decimal (..) )
import Numeric.Natural
    ( Natural )
import Test.Hspec
    ( Spec, describe, it, parallel )
import Test.QuickCheck
    ( Arbitrary (..)
    , Fixed (..)
    , Gen
    , Property
    , Testable (..)
    , checkCoverage
    , conjoin
    , counterexample
    , cover
    , forAllBlind
    , label
    , liftShrink2
    , oneof
    , property
    , resize
    , shrinkMapBy
    , withMaxSuccess
    , (.&&.)
    , (===)
    )
import Test.QuickCheck.Classes.Hspec
    ( testLawsMany )
import Text.Pretty.Simple
    ( pShow )

import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Test.QuickCheck.Classes as Laws

spec :: Spec
spec = do

    parallel $ describe "Lawfulness of type class instances" $ do
        testLawsMany @TestQuid
            [ Laws.eqLaws
            , Laws.ordLaws
            ]
        testLawsMany @TestQuid
            [ Laws.eqLaws
            , Laws.ordLaws
            , Laws.showLaws
            , Laws.showReadLaws
            ]

    parallel $ describe "Uniformity" $ do
        it "prop_arbitraryQuid_uniform" $
            property prop_arbitraryQuid_uniform

    parallel $ describe "Uniqueness" $ do
        it "prop_arbitraryQuid_unique" $
            property prop_arbitraryQuid_unique

    parallel $ describe "Shrinkability" $ do
        it "prop_shrinkQuid_lessThan" $
            property prop_shrinkQuid_lessThan
        it "prop_shrinkQuid_minimalElement" $
            property prop_shrinkQuid_minimalElement
        it "prop_shrinkQuid_minimalSet" $
            property prop_shrinkQuid_minimalSet
        it "prop_shrinkQuid_ordered" $
            property prop_shrinkQuid_ordered
        it "prop_shrinkQuid_unique" $
            property prop_shrinkQuid_unique

--------------------------------------------------------------------------------
-- Powers of two
--------------------------------------------------------------------------------

newtype PowerOfTwo = PowerOfTwo {powerOfTwoExponent :: Natural}
    deriving (Eq, Ord, Show)

instance Buildable PowerOfTwo where
    build (PowerOfTwo e) = "2^" <> build (show e)

evalPowerOfTwo :: PowerOfTwo -> Natural
evalPowerOfTwo = (2 ^) . powerOfTwoExponent

genPowerOfTwo :: (Natural, Natural) -> Gen PowerOfTwo
genPowerOfTwo (lo, hi) = PowerOfTwo <$> chooseNatural (lo, hi)

shrinkPowerOfTwo :: PowerOfTwo -> [PowerOfTwo]
shrinkPowerOfTwo = shrinkMapBy PowerOfTwo powerOfTwoExponent shrinkNatural

--------------------------------------------------------------------------------
-- Prime numbers
--------------------------------------------------------------------------------

newtype PrimeNumber = PrimeNumber {primeNumberIndex :: Natural}
    deriving (Eq, Ord, Show)

instance Buildable PrimeNumber where
    build = build . show . evalPrimeNumber

evalPrimeNumber :: PrimeNumber -> Natural
evalPrimeNumber = indexToPrime primes . primeNumberIndex
  where
    indexToPrime (p : ps) i
        | i == 0 = p
        | otherwise = indexToPrime ps (i - 1)
    indexToPrime [] _ =
        error "evalPrimeNumber: Unexpected empty list of prime numbers."

genPrimeNumber :: (Natural, Natural) -> Gen PrimeNumber
genPrimeNumber (lo, hi) = PrimeNumber <$> chooseNatural (lo, hi)

shrinkPrimeNumber :: PrimeNumber -> [PrimeNumber]
shrinkPrimeNumber = shrinkMapBy PrimeNumber primeNumberIndex shrinkNatural

--------------------------------------------------------------------------------
-- Partition functions
--------------------------------------------------------------------------------

data PartitionFunction
    = Div PowerOfTwo
    | Mod PrimeNumber
    deriving (Eq, Ord, Show)

instance Buildable PartitionFunction where
    build = \case
        Div p -> "div " <> padLeftF 5 ' ' p
        Mod p -> "mod " <> padLeftF 5 ' ' p

evalPartitionFunction :: PartitionFunction -> (Natural -> Natural)
evalPartitionFunction = \case
    Div p -> (`div` evalPowerOfTwo  p)
    Mod p -> (`mod` evalPrimeNumber p)

--------------------------------------------------------------------------------
-- Partition contexts
--------------------------------------------------------------------------------

data PartitionContext = PartitionContext
    { sizeExponent :: PowerOfTwo
    , expectedBucketCount :: Natural
    , partitionFunction :: PartitionFunction
    }
    deriving (Eq, Ord, Show)

instance Buildable PartitionContext where
    build c = mconcat
        [ "(size = "
        , padLeftF 5 ' ' (sizeExponent c)
        , ", expected bucket count = "
        , padLeftF 3 ' ' (show $ expectedBucketCount c)
        , ", partition function = "
        , build (partitionFunction c)
        , ")"
        ]

--------------------------------------------------------------------------------
-- Div partitions
--------------------------------------------------------------------------------

data DivPartition = DivPartition
    { divArgument :: PowerOfTwo
    , scaleFactor :: PowerOfTwo
    }
    deriving (Eq, Ord, Show)

evalDivPartition :: DivPartition -> PartitionContext
evalDivPartition DivPartition {divArgument, scaleFactor} =
    PartitionContext
        { sizeExponent = PowerOfTwo
            $ powerOfTwoExponent divArgument
            + powerOfTwoExponent scaleFactor
        , expectedBucketCount = evalPowerOfTwo scaleFactor
        , partitionFunction = Div divArgument
        }

genDivPartition :: Gen DivPartition
genDivPartition = do
    divArgument <- oneof (genPowerOfTwo <$> [(0, 1), (2, 256)])
    scaleFactor <- oneof (genPowerOfTwo <$> [(0, 1), (2,   8)])
    pure DivPartition {divArgument, scaleFactor}

shrinkDivPartition :: DivPartition -> [DivPartition]
shrinkDivPartition = shrinkMapBy unTuple toTuple $
    liftShrink2 shrinkPowerOfTwo shrinkPowerOfTwo
  where
    unTuple (c, s) = (DivPartition c s)
    toTuple (DivPartition c s) = (c, s)

--------------------------------------------------------------------------------
-- Mod partitions
--------------------------------------------------------------------------------

data ModPartition = ModPartition
    { modArgument :: PrimeNumber
    , scaleFactor :: PowerOfTwo
    }
    deriving (Eq, Ord, Show)

evalModPartition :: ModPartition -> PartitionContext
evalModPartition ModPartition {modArgument, scaleFactor} =
    PartitionContext
        { sizeExponent = PowerOfTwo
            $ primeNumberIndex modArgument
            + powerOfTwoExponent scaleFactor
            + 8
        , expectedBucketCount = evalPrimeNumber modArgument
        , partitionFunction = Mod modArgument
        }

genModPartition :: Gen ModPartition
genModPartition = do
    modArgument <- oneof (genPrimeNumber <$> [(0, 1), (2,  32)])
    scaleFactor <- oneof (genPowerOfTwo  <$> [(0, 1), (2, 256)])
    pure ModPartition {modArgument, scaleFactor}

shrinkModPartition :: ModPartition -> [ModPartition]
shrinkModPartition = shrinkMapBy unTuple toTuple $
    liftShrink2 shrinkPrimeNumber shrinkPowerOfTwo
  where
    unTuple (m, s) = (ModPartition m s)
    toTuple (ModPartition m s) = (m, s)

--------------------------------------------------------------------------------
-- Partitions
--------------------------------------------------------------------------------

data Partition
    = DivPartitionOf DivPartition
    | ModPartitionOf ModPartition
    deriving (Eq, Ord, Show)

instance Arbitrary Partition where
    arbitrary = genPartition
    shrink = shrinkPartition

evalPartition :: Partition -> PartitionContext
evalPartition = \case
    DivPartitionOf p -> evalDivPartition p
    ModPartitionOf p -> evalModPartition p

genPartition :: Gen Partition
genPartition = oneof
    [ DivPartitionOf <$> genDivPartition
    , ModPartitionOf <$> genModPartition
    ]

shrinkPartition :: Partition -> [Partition]
shrinkPartition = \case
    DivPartitionOf p -> DivPartitionOf <$> shrinkDivPartition p
    ModPartitionOf p -> ModPartitionOf <$> shrinkModPartition p

--------------------------------------------------------------------------------
-- Uniformity
--------------------------------------------------------------------------------

prop_arbitraryQuid_uniform :: Partition -> Property
prop_arbitraryQuid_uniform p =
    label (pretty partitionContext) $
    forAllBlind arbitraryValues prop
  where
    partitionContext :: PartitionContext
    partitionContext@PartitionContext
        { sizeExponent
        , expectedBucketCount
        , partitionFunction
        } = evalPartition p

    valueToBucket :: Quid -> Natural
    valueToBucket = (evalPartitionFunction partitionFunction) . quidToNatural

    arbitraryValue :: Gen Quid
    arbitraryValue =
        resize (fromIntegral (powerOfTwoExponent sizeExponent)) arbitraryQuid

    arbitraryValues :: Gen [Quid]
    arbitraryValues =
        replicateM (fromIntegral arbitraryValueCount) arbitraryValue

    arbitraryValueCount :: Natural
    arbitraryValueCount = unFrequency expectedFrequency * expectedBucketCount

    expectedFrequency :: Frequency
    expectedFrequency = Frequency 1024

    minimumPermittedFrequency :: Frequency
    minimumPermittedFrequency = expectedFrequency <&> ((* 3) . (`div` 4))

    maximumPermittedFrequency :: Frequency
    maximumPermittedFrequency = expectedFrequency <&> ((* 5) . (`div` 4))

    prop :: [Quid] -> Property
    prop values = reports $ checks $ property True
      where
        reports
            = report sizeExponent
                "size exponent"
            . report arbitraryValueCount
                "arbitrary value count"
            . report expectedBucketCount
                "expected bucket count"
            . report occupiedBucketCount
                "occupied bucket count"
            . report expectedFrequency
                "expected frequency"
            . report minimumObservedFrequency
                "minimum observed frequency"
            . report minimumPermittedFrequency
                "minimum permitted frequency"
            . report maximumObservedFrequency
                "maximum observed frequency"
            . report maximumPermittedFrequency
                "maximum permitted frequency"
        checks
            = check
                (occupiedBucketCount == expectedBucketCount)
                "occupiedBucketCount == expectedBucketCount"
            . check
                (minimumObservedFrequency >= minimumPermittedFrequency)
                "minimumObservedFrequency >= minimumPermittedFrequency"
            . check
                (maximumObservedFrequency <= maximumPermittedFrequency)
                "maximumObservedFrequency <= maximumPermittedFrequency"

        occupiedBuckets :: [Natural]
        occupiedBuckets = valueToBucket <$> values

        occupiedBucketFrequencies :: [(Natural, Frequency)]
        occupiedBucketFrequencies = frequencies occupiedBuckets

        occupiedBucketCount :: Natural
        occupiedBucketCount = fromIntegral $ length occupiedBucketFrequencies

        minimumObservedFrequency :: Frequency
        minimumObservedFrequency =
            snd $ lastNote note occupiedBucketFrequencies
          where
            note = "minimumObservedFrequency: unexpected empty list"

        maximumObservedFrequency :: Frequency
        maximumObservedFrequency =
            snd $ headNote note occupiedBucketFrequencies
          where
            note = "maximumObservedFrequency: unexpected empty list"

--------------------------------------------------------------------------------
-- Uniqueness
--------------------------------------------------------------------------------

prop_arbitraryQuid_unique :: Property
prop_arbitraryQuid_unique =
    withMaxSuccess 1 $
    forAllBlind arbitraryFixedSizeQuids $ \uids ->
        Set.size (Set.fromList uids) === L.length uids
  where
    arbitraryFixedSizeQuids :: Gen [TestQuid]
    arbitraryFixedSizeQuids = fmap (unSize . getFixed) <$>
        replicateM 1_000_000 (arbitrary @(Fixed (Size 256 TestQuid)))

--------------------------------------------------------------------------------
-- Shrinkability
--------------------------------------------------------------------------------

prop_shrinkQuid_lessThan :: Size 256 TestQuid -> Property
prop_shrinkQuid_lessThan (Size q) =
    property $ all (< q) (shrink q)

prop_shrinkQuid_minimalElement :: TestQuid -> Property
prop_shrinkQuid_minimalElement q =
    checkCoverage $
    cover 10 (q /= minimalQuid) "q /= minimalQuid" $
    case shrink q of
        s : _ -> s === minimalQuid
        _     -> q === minimalQuid
  where
    minimalQuid = TestQuid 0

prop_shrinkQuid_minimalSet :: [Size 256 TestQuid] -> Property
prop_shrinkQuid_minimalSet qs =
    label (show $ bucket expectedSize) $
    counterexample (show expectedSize) $
    counterexample (show minimalSet) $
    conjoin
        [ Set.toList minimalSet `L.isPrefixOf` allQuids
        , Set.size minimalSet == expectedSize
        ]
  where
    allQuids :: [TestQuid]
    allQuids = TestQuid . naturalToQuid <$> [0 ..]

    bucket :: Int -> (Int, Int)
    bucket size = (lo, hi)
      where
        lo = size `div` 10 * 10
        hi = lo + 9

    expectedSize :: Int
    expectedSize = L.length qs

    minimalSet :: Set TestQuid
    minimalSet = Set.map unSize $ fromMaybe
        (error "Cannot shrink to minimal set")
        (shrinkWhile ((>= expectedSize) . Set.size) shrink (Set.fromList qs))

prop_shrinkQuid_ordered :: Size 256 TestQuid -> Property
prop_shrinkQuid_ordered (Size q) =
    L.sort shrunkValues === shrunkValues
  where
    shrunkValues = shrink q

prop_shrinkQuid_unique :: Size 256 TestQuid -> Property
prop_shrinkQuid_unique (Size q) =
    Set.size (Set.fromList shrunkValues) === L.length shrunkValues
  where
    shrunkValues = shrink q

--------------------------------------------------------------------------------
-- Shrinking
--------------------------------------------------------------------------------

shrinkWhile :: (a -> Bool) -> (a -> [a]) -> a -> Maybe a
shrinkWhile condition shrinkFn = loop
  where
    loop a
        | condition a =
            case L.find condition (shrinkFn a) of
                Nothing -> Just a
                Just a' -> loop a'
        | otherwise =
            Nothing

--------------------------------------------------------------------------------
-- Frequencies
--------------------------------------------------------------------------------

type Frequency = FrequencyOf Natural

newtype FrequencyOf a = Frequency {unFrequency :: a}
    deriving (Eq, Functor, Ord, Show)

instance Semigroup Frequency where
    Frequency f1 <> Frequency f2 = Frequency (f1 + f2)

instance Monoid Frequency where
    mempty = Frequency 1

frequencies :: (Foldable f, Ord k) => f k -> [(k, Frequency)]
frequencies
    = L.sortOn ((Down . snd) &&& fst)
    . Map.toList
    . L.foldr (flip (Map.insertWith (<>)) mempty) Map.empty

--------------------------------------------------------------------------------
-- Reporting
--------------------------------------------------------------------------------

-- | Adds a named variable to the counterexample output of a property.
--
-- On failure, uses pretty-printing to show the contents of the variable.
--
report :: (Show a, Testable prop) => a -> String -> prop -> Property
report a name = counterexample $
    "" +| name |+ ":\n" +| indentF 4 (pShowBuilder a) |+ ""
  where
    pShowBuilder :: Show a => a -> Builder
    pShowBuilder = fromLazyText . pShow

--------------------------------------------------------------------------------
-- Verification
--------------------------------------------------------------------------------

-- | Adds a named condition to a property.
--
-- On failure, reports the name of the condition that failed.
--
check :: Bool -> String -> Property -> Property
check condition conditionTitle =
    (.&&.) (counterexample counterexampleText $ property condition)
  where
    counterexampleText = "Condition violated: " <> conditionTitle

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

headNote :: String -> [a] -> a
headNote note = g
  where
    g [] = error note
    g (a : _) = a

lastNote :: String -> [a] -> a
lastNote note = g
  where
    g [] = error note
    g [a] = a
    g (_ : as) = g as

--------------------------------------------------------------------------------
-- Test types
--------------------------------------------------------------------------------

newtype TestQuid = TestQuid Quid
    deriving (Read, Show) via (Decimal Quid)
    deriving Arbitrary via Quid
    deriving stock (Eq, Ord)
