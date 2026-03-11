{-|
Module:      Spec.Control.Applicative.TransSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for applicative functor transformers.
-}
module Spec.Control.Applicative.TransSpec (main, spec) where

import Control.Applicative.Backwards (Backwards)
import Control.Applicative.Lift      (Lift)

import Data.Proxy (Proxy(..))

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.QuickCheck.Instances ()

import TextShow.Control.Applicative.Trans ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Backwards Maybe Int" $
        matchesTextShowSpec (Proxy :: Proxy (Backwards Maybe Int))
    describe "Lift Maybe Int" $
        matchesTextShowSpec (Proxy :: Proxy (Lift Maybe Int))
