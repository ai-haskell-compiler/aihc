{-|
Module:      Spec.System.LocaleSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for old 'TimeLocale's.
-}
module Spec.System.LocaleSpec (main, spec) where

import Data.Proxy (Proxy(..))
import Instances.System.Locale ()
import Spec.Utils (matchesTextShowSpec)
import System.Locale (TimeLocale)
import Test.Hspec (Spec, describe, hspec, parallel)
import TextShow.System.Locale ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "TimeLocale" $
    matchesTextShowSpec (Proxy :: Proxy TimeLocale)
