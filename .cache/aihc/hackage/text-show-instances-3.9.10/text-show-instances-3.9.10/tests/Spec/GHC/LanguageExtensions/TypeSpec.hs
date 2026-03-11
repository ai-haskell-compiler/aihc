{-|
Module:      Spec.GHC.LanguageExtensions.TypeSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for the 'Extension' data type.
-}
module Spec.GHC.LanguageExtensions.TypeSpec (main, spec) where

import Data.Proxy (Proxy(..))

import GHC.LanguageExtensions.Type (Extension)

import Instances.GHC.LanguageExtensions.Type ()

import Prelude ()
import Prelude.Compat

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

import TextShow.GHC.LanguageExtensions.Type ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $
    describe "Extension" $
        matchesTextShowSpec (Proxy :: Proxy Extension)
