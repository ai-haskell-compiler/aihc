{-|
Module:      Spec.Data.ScientificSpec
Copyright:   (C) 2014-2018 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Scientific' (from the @scientific@ package).
-}
module Spec.Data.ScientificSpec (main, spec) where

import           Data.Proxy                (Proxy (..))
import           Data.Scientific           (Scientific)

import           Spec.Utils                (matchesTextShowSpec)

import           Test.Hspec                (Spec, describe, hspec, parallel)
import           Test.QuickCheck.Instances ()

import           TextShow.Data.Scientific   ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Scientific" $
        matchesTextShowSpec (Proxy :: Proxy Scientific)
