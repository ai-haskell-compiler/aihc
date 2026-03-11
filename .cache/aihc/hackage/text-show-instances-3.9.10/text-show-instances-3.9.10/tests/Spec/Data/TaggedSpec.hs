{-|
Module:      Spec.Data.TaggedSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Tagged' values.
-}
module Spec.Data.TaggedSpec (main, spec) where

import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged)

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.QuickCheck.Instances ()

import TextShow.Data.Tagged ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel . describe "Tagged Char Int" $
    matchesTextShowSpec (Proxy :: Proxy (Tagged Char Int))
