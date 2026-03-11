{-|
Module:      Spec.Data.TimeSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @time@ library.
-}
module Spec.Data.TimeSpec (main, spec) where

import Data.Proxy (Proxy(..))
import Data.Time (UniversalTime)
import Data.Time.Calendar (Day)
import Data.Time.Clock (DiffTime, UTCTime, NominalDiffTime)
import Data.Time.Clock.System (SystemTime)
import Data.Time.Clock.TAI (AbsoluteTime)
import Data.Time.LocalTime (TimeZone, TimeOfDay, LocalTime, ZonedTime)

import Instances.Data.Time ()

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.QuickCheck.Instances ()

import TextShow.Data.Time ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Day" $
        matchesTextShowSpec (Proxy :: Proxy Day)
    describe "DiffTime" $
        matchesTextShowSpec (Proxy :: Proxy DiffTime)
    describe "UTCTime" $
        matchesTextShowSpec (Proxy :: Proxy UTCTime)
    describe "NominalDiffTime" $
        matchesTextShowSpec (Proxy :: Proxy NominalDiffTime)
    describe "AbsoluteTime" $
        matchesTextShowSpec (Proxy :: Proxy AbsoluteTime)
    describe "TimeZone" $
        matchesTextShowSpec (Proxy :: Proxy TimeZone)
    describe "TimeOfDay" $
        matchesTextShowSpec (Proxy :: Proxy TimeOfDay)
    describe "LocalTime" $
        matchesTextShowSpec (Proxy :: Proxy LocalTime)
    describe "ZonedTime" $
        matchesTextShowSpec (Proxy :: Proxy ZonedTime)
    describe "UniversalTime" $
        matchesTextShowSpec (Proxy :: Proxy UniversalTime)
    describe "SystemTime" $
        matchesTextShowSpec (Proxy :: Proxy SystemTime)
