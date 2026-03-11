{-|
Module:      Spec.GHC.ForeignSrcLang.TypeSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for the 'ForeignSrcLang' data type.
-}
module Spec.GHC.ForeignSrcLang.TypeSpec (main, spec) where

import Data.Proxy (Proxy(..))

import GHC.ForeignSrcLang.Type (ForeignSrcLang)

import Instances.GHC.ForeignSrcLang.Type ()

import Prelude ()
import Prelude.Compat

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

import TextShow.GHC.ForeignSrcLang.Type ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "ForeignSrcLang" $
        matchesTextShowSpec (Proxy :: Proxy ForeignSrcLang)
