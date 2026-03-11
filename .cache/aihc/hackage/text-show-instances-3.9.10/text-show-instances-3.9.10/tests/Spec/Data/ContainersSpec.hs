{-|
Module:      Spec.Data.ContainersSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @containers@ library.
-}
module Spec.Data.ContainersSpec (main, spec) where

import Data.Graph (SCC)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Proxy (Proxy(..))
import Data.Sequence (Seq, ViewL, ViewR)
import Data.Set (Set)
import Data.Tree (Tree)

import Instances.Data.Containers ()

import Spec.Utils (matchesTextShowSpec, matchesTextShow1Spec, matchesTextShow2Spec)

import Test.Hspec (Spec, describe, hspec, parallel)
import Test.QuickCheck.Instances ()

import TextShow.Data.Containers ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "IntMap Char" $ do
        let p :: Proxy (IntMap Char)
            p = Proxy
        matchesTextShowSpec  p
        matchesTextShow1Spec p
    describe "IntSet" $
        matchesTextShowSpec (Proxy :: Proxy IntSet)
    describe "Map Char Char" $ do
        let p :: Proxy (Map Char Char)
            p = Proxy
        matchesTextShowSpec  p
        matchesTextShow1Spec p
        matchesTextShow2Spec p
    describe "Sequence Char" $ do
        let p :: Proxy (Seq Char)
            p = Proxy
        matchesTextShowSpec  p
        matchesTextShow1Spec p
    describe "ViewL Char" $
        matchesTextShowSpec (Proxy :: Proxy (ViewL Char))
    describe "ViewR Char" $
        matchesTextShowSpec (Proxy :: Proxy (ViewR Char))
    describe "SCC Char" $ do
        let p :: Proxy (SCC Char)
            p = Proxy
        matchesTextShowSpec  p
        matchesTextShow1Spec p
    describe "Set Char" $ do
        let p :: Proxy (Set Char)
            p = Proxy
        matchesTextShowSpec  p
        matchesTextShow1Spec p
    describe "Tree Char" $ do
        let p :: Proxy (Tree Char)
            p = Proxy
        matchesTextShowSpec  p
        matchesTextShow1Spec p
