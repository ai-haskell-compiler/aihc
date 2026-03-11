--
-- Time to manipulate time
--
-- Copyright © 2013-2016 Operational Dynamics Consulting, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

-- | Test serialisation/deserialiastion for TimeStamp type

{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE NegativeLiterals #-}
{-# OPTIONS -fno-warn-orphans #-}

module CheckTimeStamp where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Test.Hspec
import Test.QuickCheck
import Data.Hourglass

import Chrono.TimeStamp
import Chrono.Compat

checkTimeStamp :: Spec
checkTimeStamp = do
    describe "Smallest valid TimeStamp" $
      let
        t = TimeStamp (-9223372036854775808)
      in do
        it "should be representable" $ do
            t `shouldBe` (minBound :: TimeStamp)

        it "should be match when Shown" $ do
            show t `shouldBe` show (minBound :: TimeStamp)

        it "should equal expected value" $ do
            show t `shouldBe` "1677-09-21T00:12:43.145224192Z"

    describe "Largest valid TimeStamp" $
      let
        t = TimeStamp 9223372036854775807
      in do
        it "should be representable" $ do
            t `shouldBe` (maxBound :: TimeStamp)

        it "should be match when Shown" $ do
            show t `shouldBe` show (maxBound :: TimeStamp)

        it "should equal expected value" $ do
            show t `shouldBe` "2262-04-11T23:47:16.854775807Z"

    describe "Printing and parsing with precise format" $ do
        it "formats a known date correctly" $ do
            timePrint ISO8601_Precise (TimeStamp 1406849015948797001) `shouldBe` "2014-07-31T23:23:35.948797001Z"

        it "uses timeParse effectively" $ do
            timeParse ISO8601_Precise "2014-07-31T23:42:35.948797001Z" `shouldBe`
                Just (DateTime (Date 2014 July 31) (TimeOfDay 23 42 35 948797001))

    describe "Round trip through Read and Show instances" $ do
        it "outputs a correctly formated ISO 8601 timestamp when Shown" $ do
            show (TimeStamp 1406849015948797001) `shouldBe` "2014-07-31T23:23:35.948797001Z"
            show (TimeStamp 1406849015948797001) `shouldBe` "2014-07-31T23:23:35.948797001Z"
            show (TimeStamp 0) `shouldBe` "1970-01-01T00:00:00.000000000Z"

        it "Reads ISO 8601 timestamps" $ do
            read "2014-07-31T23:23:35.948797001Z" `shouldBe` TimeStamp 1406849015948797001
            read "2014-07-31T23:23:35Z" `shouldBe` TimeStamp 1406849015000000000
            read "2014-07-31" `shouldBe` TimeStamp 1406764800000000000

        it "reads the Unix epoch date" $
            read "1970-01-01" `shouldBe` TimeStamp 0

        it "permissively reads various formats" $ do
            show (read "1970-01-01T00:00:00.000000000Z" :: TimeStamp) `shouldBe` "1970-01-01T00:00:00.000000000Z"
            show (read "1970-01-01" :: TimeStamp) `shouldBe` "1970-01-01T00:00:00.000000000Z"
            show (read "0" :: TimeStamp) `shouldBe` "1970-01-01T00:00:00.000000000Z"

        it "permissively reads Posix seconds since epoch" $ do
            show (read "1406849015.948797001" :: TimeStamp) `shouldBe` "2014-07-31T23:23:35.948797001Z"
            show (read "1406849015.948797" :: TimeStamp) `shouldBe` "2014-07-31T23:23:35.948797000Z"
            show (read "1406849015.948" :: TimeStamp) `shouldBe` "2014-07-31T23:23:35.948000000Z"
            show (read "1406849015" :: TimeStamp) `shouldBe` "2014-07-31T23:23:35.000000000Z"
{-
    This is a bit fragile, depending as it does on the serialization to String
    in the Show instance of UTCTime. Not that they're going to change it
    anytime soon.
-}

    describe "Round trip through base time types" $ do
        it "converts to POSIXTime and back again" $ do
            let t = TimeStamp 1406849015948797001
            convertFromPosix (convertToPosix t) `shouldBe` t
            show (convertToPosix t) `shouldBe` "1406849015.948797001s"

        it "converts to UTCTime and back again" $ do
            let t = TimeStamp 1406849015948797001
            convertFromUTC (convertToUTC t) `shouldBe` t
            show (convertToUTC t) `shouldBe` "2014-07-31 23:23:35.948797001 UTC"

        it "behaves when QuickChecked" $ do
            property prop_RoundTrip_ReadShow

    describe "Round trip via JSON" $ do
        it "explicit JSON encoding is correct" $ do
            let t = read "2018-05-01T01:42:12Z" :: TimeStamp
            encode t `shouldBe` L.pack "\"2018-05-01T01:42:12.000000000Z\""

        it "converts to a JSON String and back again" $ do
            let t = read "2018-05-01T01:42:12Z" :: TimeStamp
            (decode . encode) t `shouldBe` Just t

        it "behaves when QuickChecked" $ do
            property prop_RoundTrip_Aeson

instance Arbitrary TimeStamp where
    arbitrary = do
        tick <- arbitrary
        return (TimeStamp tick)

prop_RoundTrip_ReadShow :: TimeStamp -> Bool
prop_RoundTrip_ReadShow t = (read . show) t == t

prop_RoundTrip_Aeson :: TimeStamp -> Bool
prop_RoundTrip_Aeson t = (decode . encode) t == Just t
