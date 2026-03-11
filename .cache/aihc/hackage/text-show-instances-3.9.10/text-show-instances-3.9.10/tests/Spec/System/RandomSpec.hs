{-|
Module:      Spec.System.RandomSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'StdGen' values.
-}
module Spec.System.RandomSpec (main, spec) where

import Data.Proxy (Proxy(..))
import Instances.System.Random ()
import Spec.Utils (matchesTextShowSpec)
import System.Random (StdGen)
import Test.Hspec (Spec, describe, hspec, parallel)
import TextShow.System.Random ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "StdGen" $
    matchesTextShowSpec (Proxy :: Proxy StdGen)
