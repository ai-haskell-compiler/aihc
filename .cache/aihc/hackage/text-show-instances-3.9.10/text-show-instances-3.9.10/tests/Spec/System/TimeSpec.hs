{-|
Module:      Spec.System.TimeSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@QuickCheck@ properties for data types in @old-time@.
-}
module Spec.System.TimeSpec (main, spec) where

import Data.Proxy (Proxy(..))

import Spec.Utils (matchesTextShowSpec)

import System.Time (ClockTime, TimeDiff, CalendarTime, Month, Day)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.QuickCheck.Instances ()

import TextShow.System.Time ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "ClockTime" $
        matchesTextShowSpec (Proxy :: Proxy ClockTime)
    describe "TimeDiff" $
        matchesTextShowSpec (Proxy :: Proxy TimeDiff)
    describe "CalendarTime" $
        matchesTextShowSpec (Proxy :: Proxy CalendarTime)
    describe "Month" $
        matchesTextShowSpec (Proxy :: Proxy Month)
    describe "Day" $
        matchesTextShowSpec (Proxy :: Proxy Day)
