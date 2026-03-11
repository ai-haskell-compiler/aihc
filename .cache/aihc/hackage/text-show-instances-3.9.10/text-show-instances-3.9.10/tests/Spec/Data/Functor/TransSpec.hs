{-|
Module:      Spec.Data.Functor.TransSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for functor transformers.
-}
module Spec.Data.Functor.TransSpec (main, spec) where

import Data.Functor.Compose  (Compose)
import Data.Functor.Constant (Constant)
import Data.Functor.Product  (Product)
import Data.Functor.Reverse  (Reverse)
import Data.Functor.Sum      (Sum)
import Data.Proxy            (Proxy(..))

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.QuickCheck.Instances ()

import TextShow.Data.Functor.Trans ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Compose Maybe (Either Int) Char" $
        matchesTextShowSpec (Proxy :: Proxy (Compose Maybe (Either Int) Char))
    describe "Constant Int Char" $
        matchesTextShowSpec (Proxy :: Proxy (Constant Int Char))
    describe "Product Maybe (Either Int) Char" $
        matchesTextShowSpec (Proxy :: Proxy (Product Maybe (Either Int) Char))
    describe "Reverse Maybe Int" $
        matchesTextShowSpec (Proxy :: Proxy (Reverse Maybe Int))
    describe "Sum Maybe (Either Int) Char" $
        matchesTextShowSpec (Proxy :: Proxy (Sum Maybe (Either Int) Char))
