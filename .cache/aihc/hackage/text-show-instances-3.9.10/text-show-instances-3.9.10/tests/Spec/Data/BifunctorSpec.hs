{-|
Module:      Spec.Data.BifunctorSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @bifunctors@ library.
-}
module Spec.Data.BifunctorSpec (main, spec) where

import Data.Bifunctor.Biff (Biff)
import Data.Bifunctor.Clown (Clown)
import Data.Bifunctor.Fix (Fix)
import Data.Bifunctor.Flip (Flip)
import Data.Bifunctor.Join (Join)
import Data.Bifunctor.Joker (Joker)
import Data.Bifunctor.Product (Product)
import Data.Bifunctor.Sum (Sum)
import Data.Bifunctor.Tannen (Tannen)
import Data.Bifunctor.Wrapped (WrappedBifunctor)
import Data.Proxy (Proxy(..))

import Instances.Data.Bifunctor ()

import Spec.Utils

import Test.Hspec (Spec, describe, hspec, parallel)

import TextShow.Data.Bifunctor ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Biff Either [] Maybe Char Int" $ do
        matchesTextShowSpec  (Proxy :: Proxy (Biff Either [] Maybe Char Int))
        matchesTextShow1Spec (Proxy :: Proxy (Biff Either [] Maybe Char Int))
        matchesTextShow2Spec (Proxy :: Proxy (Biff Either [] Maybe Char Int))
    describe "Clown [] Char Int" $ do
        matchesTextShowSpec  (Proxy :: Proxy (Clown [] Char Int))
        matchesTextShow1Spec (Proxy :: Proxy (Clown [] Char Int))
        matchesTextShow2Spec (Proxy :: Proxy (Clown [] Char Int))
    describe "Fix Either Int" $ do
        matchesTextShowSpec  (Proxy :: Proxy (Fix Either Int))
        matchesTextShow1Spec (Proxy :: Proxy (Fix Either Int))
    describe "Flip Either Int Char" $ do
        matchesTextShowSpec  (Proxy :: Proxy (Flip Either Int Char))
        matchesTextShow1Spec (Proxy :: Proxy (Flip Either Int Char))
        matchesTextShow2Spec (Proxy :: Proxy (Flip Either Int Char))
    describe "Join Either Int" $ do
        matchesTextShowSpec  (Proxy :: Proxy (Join Either Int))
        matchesTextShow1Spec (Proxy :: Proxy (Join Either Int))
    describe "Joker [] Char Int" $ do
        matchesTextShowSpec  (Proxy :: Proxy (Joker [] Char Int))
        matchesTextShow1Spec (Proxy :: Proxy (Joker [] Char Int))
        matchesTextShow2Spec (Proxy :: Proxy (Joker [] Char Int))
    describe "Product Either ((,,) Bool) Int Char" $ do
        matchesTextShowSpec  (Proxy :: Proxy (Product Either (,) Int Char))
        matchesTextShow1Spec (Proxy :: Proxy (Product Either (,) Int Char))
        matchesTextShow2Spec (Proxy :: Proxy (Product Either (,) Int Char))
    describe "Sum Either ((,,) Bool) Int Char" $ do
        matchesTextShowSpec  (Proxy :: Proxy (Sum Either (,) Int Char))
        matchesTextShow1Spec (Proxy :: Proxy (Sum Either (,) Int Char))
        matchesTextShow2Spec (Proxy :: Proxy (Sum Either (,) Int Char))
    describe "Tannen Maybe Either Int Char" $ do
        matchesTextShowSpec  (Proxy :: Proxy (Tannen Maybe Either Int Char))
        matchesTextShow1Spec (Proxy :: Proxy (Tannen Maybe Either Int Char))
        matchesTextShow2Spec (Proxy :: Proxy (Tannen Maybe Either Int Char))
    describe "WrappedBifunctor Either Int Char" $ do
        matchesTextShowSpec  (Proxy :: Proxy (WrappedBifunctor Either Int Char))
        matchesTextShow1Spec (Proxy :: Proxy (WrappedBifunctor Either Int Char))
        matchesTextShow2Spec (Proxy :: Proxy (WrappedBifunctor Either Int Char))
