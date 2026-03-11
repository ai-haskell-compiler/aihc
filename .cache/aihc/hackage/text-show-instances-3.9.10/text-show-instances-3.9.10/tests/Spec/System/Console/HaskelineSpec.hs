{-|
Module:      Spec.System.Console.HaskelineSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @haskeline@ library.
-}
module Spec.System.Console.HaskelineSpec (main, spec) where

import Data.Proxy (Proxy(..))

import Instances.System.Console.Haskeline ()

import Spec.Utils (matchesTextShowSpec)

import System.Console.Haskeline (Interrupt, defaultPrefs)
import System.Console.Haskeline.Completion (Completion)
import System.Console.Haskeline.History (History)

import Test.Hspec (Spec, describe, hspec, it, parallel, shouldBe)

import TextShow (FromStringShow(..), showb)
import TextShow.System.Console.Haskeline ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Interrupt" $
        matchesTextShowSpec (Proxy :: Proxy Interrupt)
--     describe "Prefs" $
--         matchesTextShowSpec (Proxy :: Proxy Prefs)
    describe "defaultPrefs" $
        it "should have coinciding string and text Show output" $
            showb (FromStringShow defaultPrefs) `shouldBe` showb defaultPrefs
    describe "Completion" $
        matchesTextShowSpec (Proxy :: Proxy Completion)
    describe "History" $
        matchesTextShowSpec (Proxy :: Proxy History)
