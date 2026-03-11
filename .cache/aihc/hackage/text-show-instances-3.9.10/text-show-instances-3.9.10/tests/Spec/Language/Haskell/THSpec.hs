{-|
Module:      Spec.Language.Haskell.THSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@QuickCheck@ properties for data types in the @template-haskell@ library.
-}
module Spec.Language.Haskell.THSpec (main, spec) where

import Data.Proxy (Proxy(..))

import Instances.Language.Haskell.TH ()

import Language.Haskell.TH.PprLib
import Language.Haskell.TH.Syntax

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Expectation, Spec, describe, hspec, parallel, shouldBe)
import Test.Hspec.QuickCheck (prop)

import TextShow (fromString)
import TextShow.Language.Haskell.TH (showbName')

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Doc" $
        matchesTextShowSpec (Proxy :: Proxy Doc)
    describe "ModName" $
        matchesTextShowSpec (Proxy :: Proxy ModName)
    describe "Name" $
        matchesTextShowSpec (Proxy :: Proxy Name)
    describe "showbName'" $
        prop "has the same output as showName" prop_showName'
    describe "NameFlavour" $
        matchesTextShowSpec (Proxy :: Proxy NameFlavour)
    describe "NameSpace" $
        matchesTextShowSpec (Proxy :: Proxy NameSpace)
    describe "OccName" $
        matchesTextShowSpec (Proxy :: Proxy OccName)
    describe "PkgName" $
        matchesTextShowSpec (Proxy :: Proxy PkgName)

-- | Verifies that `showName'` and `showbName'` have the same output.
prop_showName' :: NameIs -> Name -> Expectation
prop_showName' nameIs name = fromString (showName' nameIs name) `shouldBe` showbName' nameIs name
