{-|
Module:      Spec.Trace.HpcSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @hpc@ library.
-}
module Spec.Trace.HpcSpec (main, spec) where

import Data.Proxy (Proxy(..))

import Instances.Trace.Hpc ()

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)

import TextShow.Trace.Hpc ()

import Trace.Hpc.Mix (Mix, BoxLabel, CondBox)
import Trace.Hpc.Tix (Tix, TixModule)
import Trace.Hpc.Util (HpcPos, Hash)

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Mix" $
        matchesTextShowSpec (Proxy :: Proxy Mix)
    describe "BoxLabel" $
        matchesTextShowSpec (Proxy :: Proxy BoxLabel)
    describe "CondBox" $
        matchesTextShowSpec (Proxy :: Proxy CondBox)
    describe "Tix" $
        matchesTextShowSpec (Proxy :: Proxy Tix)
    describe "TixModule" $
        matchesTextShowSpec (Proxy :: Proxy TixModule)
    describe "HpcPos" $
        matchesTextShowSpec (Proxy :: Proxy HpcPos)
    describe "Hash" $
        matchesTextShowSpec (Proxy :: Proxy Hash)
