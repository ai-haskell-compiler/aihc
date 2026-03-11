{-# LANGUAGE QuasiQuotes #-}

module Data.Time.Clock.DurationSpec (spec) where

import Data.Time.Clock.Duration (s, ms, µs, ns, ps, t)

import Test.Hspec (context, describe, it, shouldBe, Spec)

import Data.Fixed (E0, E1, E2, E3, E6, E9, E12, Fixed)
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Time.Clock (DiffTime, NominalDiffTime)
import Foreign.C.Types (CUSeconds, CSUSeconds)

spec :: Spec
spec = do
    describe "s" $ do
        context "when treated as relative-length time" $ do
            it "represents how long the given duration is in second" $ do
                [s| 1s |] `shouldBe` (1 :: Double)
                [s| 1s |] `shouldBe` (1 :: Fixed E0)
                [s| 1s |] `shouldBe` (1 :: Fixed E1)
                [s| 1s |] `shouldBe` (1 :: Fixed E12)
                [s| 1s |] `shouldBe` (1 :: Fixed E2)
                [s| 1s |] `shouldBe` (1 :: Fixed E3)
                [s| 1s |] `shouldBe` (1 :: Fixed E6)
                [s| 1s |] `shouldBe` (1 :: Fixed E9)
                [s| 1s |] `shouldBe` (1 :: Float)
                [s| 1s |] `shouldBe` (1 :: Int)
                [s| 1s |] `shouldBe` (1 :: Int16)
                [s| 1s |] `shouldBe` (1 :: Int32)
                [s| 1s |] `shouldBe` (1 :: Int64)
                [s| 1s |] `shouldBe` (1 :: Int8)
                [s| 1s |] `shouldBe` (1 :: Integer)
                [s| 1s |] `shouldBe` (1 :: Rational)

        context "when treated as absolute-length time" $ do
            context "when treated as CUSeconds" $ do
                it "converts to microsecond" $ do
                    [s| 1 |] `shouldBe` (1000000 :: CUSeconds)

            context "when treated as CSUSeconds" $ do
                it "converts to microsecond" $ do
                    [s| 1 |] `shouldBe` (1000000 :: CSUSeconds)

            context "when treated as DiffTime" $ do
                it "converts to second" $ do
                    [s| 1 |] `shouldBe` (1 :: DiffTime)

            context "when treated as NominalDiffTime" $ do
                it "converts to second" $ do
                    [s| 1 |] `shouldBe` (1 :: NominalDiffTime)

    describe "ms" $ do
        context "when treated as relative-length time" $ do
            it "represents how long the given duration is in millisecond" $ do
                [ms| 1s |] `shouldBe` (1000 :: Double)
                [ms| 1s |] `shouldBe` (1000 :: Fixed E0)
                [ms| 1s |] `shouldBe` (1000 :: Fixed E1)
                [ms| 1s |] `shouldBe` (1000 :: Fixed E12)
                [ms| 1s |] `shouldBe` (1000 :: Fixed E2)
                [ms| 1s |] `shouldBe` (1000 :: Fixed E3)
                [ms| 1s |] `shouldBe` (1000 :: Fixed E6)
                [ms| 1s |] `shouldBe` (1000 :: Fixed E9)
                [ms| 1s |] `shouldBe` (1000 :: Float)
                [ms| 1s |] `shouldBe` (1000 :: Int)
                [ms| 1s |] `shouldBe` (1000 :: Int8)
                [ms| 1s |] `shouldBe` (1000 :: Int16)
                [ms| 1s |] `shouldBe` (1000 :: Int32)
                [ms| 1s |] `shouldBe` (1000 :: Int64)
                [ms| 1s |] `shouldBe` (1000 :: Integer)
                [ms| 1s |] `shouldBe` (1000 :: Rational)

        context "when treated as absolute-length time" $ do
            context "when treated as CUSeconds" $ do
                it "converts to microsecond" $ do
                    [ms| 1 |] `shouldBe` (1000 :: CUSeconds)

            context "when treated as CSUSeconds" $ do
                it "converts to microsecond" $ do
                    [ms| 1 |] `shouldBe` (1000 :: CSUSeconds)

            context "when treated as DiffTime" $ do
                it "converts to second" $ do
                    [ms| 1 |] `shouldBe` (0.001 :: DiffTime)

            context "when treated as NominalDiffTime" $ do
                it "converts to second" $ do
                    [ms| 1 |] `shouldBe` (0.001 :: NominalDiffTime)

    describe "µs" $ do
        context "when treated as relative-length time" $ do
            it "represents how long the given duration is in microsecond" $ do
                [µs| 1s |] `shouldBe` (1000000 :: Double)
                [µs| 1s |] `shouldBe` (1000000 :: Fixed E0)
                [µs| 1s |] `shouldBe` (1000000 :: Fixed E1)
                [µs| 1s |] `shouldBe` (1000000 :: Fixed E12)
                [µs| 1s |] `shouldBe` (1000000 :: Fixed E2)
                [µs| 1s |] `shouldBe` (1000000 :: Fixed E3)
                [µs| 1s |] `shouldBe` (1000000 :: Fixed E6)
                [µs| 1s |] `shouldBe` (1000000 :: Fixed E9)
                [µs| 1s |] `shouldBe` (1000000 :: Float)
                [µs| 1s |] `shouldBe` (1000000 :: Int)
                [µs| 1s |] `shouldBe` (1000000 :: Int8)
                [µs| 1s |] `shouldBe` (1000000 :: Int16)
                [µs| 1s |] `shouldBe` (1000000 :: Int32)
                [µs| 1s |] `shouldBe` (1000000 :: Int64)
                [µs| 1s |] `shouldBe` (1000000 :: Integer)
                [µs| 1s |] `shouldBe` (1000000 :: Rational)

        context "when treated as absolute-length time" $ do
            context "when treated as CUSeconds" $ do
                it "converts to microsecond" $ do
                    [µs| 1 |] `shouldBe` (1 :: CUSeconds)

            context "when treated as CSUSeconds" $ do
                it "converts to microsecond" $ do
                    [µs| 1 |] `shouldBe` (1 :: CSUSeconds)

            context "when treated as DiffTime" $ do
                it "converts to second" $ do
                    [µs| 1 |] `shouldBe` (0.000001 :: DiffTime)

            context "when treated as NominalDiffTime" $ do
                it "converts to second" $ do
                    [µs| 1 |] `shouldBe` (0.000001 :: NominalDiffTime)

    describe "ns" $ do
        context "when treated as relative-length time" $ do
            it "represents how long the given duration is in nanosecond" $ do
                [ns| 1s |] `shouldBe` (1000000000 :: Double)
                [ns| 1s |] `shouldBe` (1000000000 :: Fixed E0)
                [ns| 1s |] `shouldBe` (1000000000 :: Fixed E1)
                [ns| 1s |] `shouldBe` (1000000000 :: Fixed E12)
                [ns| 1s |] `shouldBe` (1000000000 :: Fixed E2)
                [ns| 1s |] `shouldBe` (1000000000 :: Fixed E3)
                [ns| 1s |] `shouldBe` (1000000000 :: Fixed E6)
                [ns| 1s |] `shouldBe` (1000000000 :: Fixed E9)
                [ns| 1s |] `shouldBe` (1000000000 :: Float)
                [ns| 1s |] `shouldBe` (1000000000 :: Int)
                [ns| 1s |] `shouldBe` (1000000000 :: Int8)
                [ns| 1s |] `shouldBe` (1000000000 :: Int16)
                [ns| 1s |] `shouldBe` (1000000000 :: Int32)
                [ns| 1s |] `shouldBe` (1000000000 :: Int64)
                [ns| 1s |] `shouldBe` (1000000000 :: Integer)
                [ns| 1s |] `shouldBe` (1000000000 :: Rational)

        context "when treated as absolute-length time" $ do
            context "when treated as CUSeconds" $ do
                it "converts to microsecond" $ do
                    [ns| 1 |] `shouldBe` (0 :: CUSeconds)

            context "when treated as CSUSeconds" $ do
                it "converts to microsecond" $ do
                    [ns| 1 |] `shouldBe` (0 :: CSUSeconds)

            context "when treated as DiffTime" $ do
                it "converts to second" $ do
                    [ns| 1 |] `shouldBe` (0.000000001 :: DiffTime)

            context "when treated as NominalDiffTime" $ do
                it "converts to second" $ do
                    [ns| 1 |] `shouldBe` (0.000000001 :: NominalDiffTime)

    describe "ps" $ do
        context "when treated as relative-length time" $ do
            it "represents how long the given duration is in picosecond" $ do
                [ps| 1s |] `shouldBe` (1000000000000 :: Double)
                [ps| 1s |] `shouldBe` (1000000000000 :: Fixed E0)
                [ps| 1s |] `shouldBe` (1000000000000 :: Fixed E1)
                [ps| 1s |] `shouldBe` (1000000000000 :: Fixed E12)
                [ps| 1s |] `shouldBe` (1000000000000 :: Fixed E2)
                [ps| 1s |] `shouldBe` (1000000000000 :: Fixed E3)
                [ps| 1s |] `shouldBe` (1000000000000 :: Fixed E6)
                [ps| 1s |] `shouldBe` (1000000000000 :: Fixed E9)
                [ps| 1s |] `shouldBe` (1000000000000 :: Float)
                [ps| 1s |] `shouldBe` (1000000000000 :: Int)
                [ps| 1s |] `shouldBe` (1000000000000 :: Int8)
                [ps| 1s |] `shouldBe` (1000000000000 :: Int16)
                [ps| 1s |] `shouldBe` (1000000000000 :: Int32)
                [ps| 1s |] `shouldBe` (1000000000000 :: Int64)
                [ps| 1s |] `shouldBe` (1000000000000 :: Integer)
                [ps| 1s |] `shouldBe` (1000000000000 :: Rational)
                [ps| 1s |] `shouldBe` (1000000000000 :: Rational)

        context "when treated as absolute-length time" $ do
            context "when treated as CUSeconds" $ do
                it "converts to microsecond" $ do
                    [ps| 1 |] `shouldBe` (0 :: CUSeconds)

            context "when treated as CSUSeconds" $ do
                it "converts to microsecond" $ do
                    [ps| 1 |] `shouldBe` (0 :: CSUSeconds)

            context "when treated as DiffTime" $ do
                it "converts to second" $ do
                    [ps| 1 |] `shouldBe` (0.000000000001 :: DiffTime)

            context "when treated as NominalDiffTime" $ do
                it "converts to second" $ do
                    [ps| 1 |] `shouldBe` (0.000000000001 :: NominalDiffTime)

    describe "t" $ do
        context "when given a short string" $ do
            it "converts m to DiffTime" $ do
                [t| 1m |] `shouldBe` (60 :: DiffTime)

            it "converts h to DiffTime" $ do
                [t| 1h |] `shouldBe` (3600 :: DiffTime)

            it "converts d to DiffTime" $ do
                [t| 2d |] `shouldBe` (172800 :: DiffTime)

            it "converts w to DiffTime" $ do
                [t| 3w |] `shouldBe` (1814400 :: DiffTime)

            it "converts s to DiffTime" $ do
                [t| 1s |] `shouldBe` (1 :: DiffTime)

            it "converts s to DiffTime" $ do
                [t| 100ms |] `shouldBe` (0.1 :: DiffTime)

            it "works with decimals" $ do
                [t| 1.5h |] `shouldBe` (5400 :: DiffTime)

            it "works with multiple spaces" $ do
                [t| 1   s |] `shouldBe` (1 :: DiffTime)

            it "shoule be case-insensitive" $ do
                [t| 1.5H |] `shouldBe` (5400 :: DiffTime)

            it "works with numbers starting with ." $ do
                [t| .5ms |] `shouldBe` (0.0005 :: DiffTime)

            it "works with negative integers" $ do
                [t| -100ms |] `shouldBe` (-0.1 :: DiffTime)

            it "works with negative decimals" $ do
                [t| -1.5h |] `shouldBe` (-5400 :: DiffTime)

            it "works with negative decimals starting with '.'" $ do
                [t| -.5h |] `shouldBe` (-1800 :: DiffTime)

        context "when given a long string" $ do
            it "converts milliseconds to DiffTime" $ do
                [t| 53 milliseconds |] `shouldBe` (0.053 :: DiffTime)

            it "converts msecs to DiffTime" $ do
                [t| 17 msecs |] `shouldBe` (0.017 :: DiffTime)

            it "converts sec to DiffTime" $ do
                [t| 1 sec |] `shouldBe` (1 :: DiffTime)

            it "converts from min to DiffTime" $ do
                [t| 1 min |] `shouldBe` (60 :: DiffTime)

            it "converts from hr to DiffTime" $ do
                [t| 1 hr |] `shouldBe` (3600 :: DiffTime)

            it "converts days to DiffTime" $ do
                [t| 2 days |] `shouldBe` (172800 :: DiffTime)

            it "works with decimals" $ do
                [t| 1.5 hours |] `shouldBe` (5400 :: DiffTime)

            it "works with negative integers" $ do
                [t| -100 milliseconds |] `shouldBe` (-0.1 :: DiffTime)

            it "works with negative decimals" $ do
                [t| -1.5 hours |] `shouldBe` (-5400 :: DiffTime)

            it "works with negative decimals starting with '.'" $ do
                [t| -.5 hr |] `shouldBe` (-1800 :: DiffTime)
