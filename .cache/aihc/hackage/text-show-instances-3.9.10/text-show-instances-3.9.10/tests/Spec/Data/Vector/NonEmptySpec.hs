{-|
Module:      Spec.Data.Vector.NonEmptySpec
Copyright:   (C) 2023 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for the 'NonEmptyVector' type.
-}
module Spec.Data.Vector.NonEmptySpec (main, spec) where

import           Data.Proxy (Proxy(..))
import qualified Data.Vector.NonEmpty as B (NonEmptyVector)

import           Instances.Data.Vector.NonEmpty ()

import           Spec.Utils (matchesTextShowSpec, matchesTextShow1Spec)

import           Test.Hspec (Spec, describe, hspec, parallel)

import           TextShow.Data.Vector.NonEmpty ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "(boxed) NonEmptyVector Char" $ do
        let p :: Proxy (B.NonEmptyVector Char)
            p = Proxy
        matchesTextShowSpec  p
        matchesTextShow1Spec p
