{-|
Module:      Spec.Data.UnorderedContainersSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'HashMap's and 'HashSet's.
-}
module Spec.Data.UnorderedContainersSpec (main, spec) where

import Data.HashMap.Lazy (HashMap)
import Data.HashSet (HashSet)
import Data.Proxy (Proxy(..))

import Spec.Utils (matchesTextShowSpec)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.QuickCheck.Instances ()

import TextShow.Data.UnorderedContainers ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "HashMap Char Char" $
        matchesTextShowSpec (Proxy :: Proxy (HashMap Char Char))
    describe "HashSet Char" $
        matchesTextShowSpec (Proxy :: Proxy (HashSet Char))
