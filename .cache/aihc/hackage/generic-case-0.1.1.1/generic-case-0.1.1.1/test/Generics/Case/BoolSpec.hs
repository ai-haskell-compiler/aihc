module Generics.Case.BoolSpec (spec) where

import Data.Bool
import Generics.Case
import qualified Test.Hspec as H
import qualified Test.QuickCheck as Q
import Util

type BoolFn r = Bool -> r -> r -> r

type BoolFnR r = r -> r -> Bool -> r

type FunArgs r = '[Bool, r, r]

type FunArgsR r = '[r, r, Bool]

manual :: BoolFn r
manual b f t = bool f t b

manualR :: BoolFnR r
manualR = bool

specBool ::
  forall r.
  (Show r, Eq r, Q.Arbitrary r) =>
  String ->
  BoolFn r ->
  H.Spec
specBool name f = specG @(FunArgs r) ("bool", manual) (name, f)

specBoolR ::
  forall r.
  (Show r, Eq r, Q.Arbitrary r) =>
  String ->
  BoolFnR r ->
  H.Spec
specBoolR name f = specG @(FunArgsR r) ("bool", manualR) (name, f)

spec :: H.Spec
spec = do
  H.describe "Left" $ do
    H.describe "()" $ do
      specBool @() "boolL" boolL
    H.describe "Char" $ do
      specBool @Char "boolL" boolL
    H.describe "String" $ do
      specBool @String "boolL" boolL
    H.describe "[Maybe (Int, String)]" $ do
      specBool @[Maybe (Int, String)] "boolL" boolL
  H.describe "Right" $ do
    H.describe "()" $ do
      specBoolR @() "boolR" boolR
    H.describe "Char" $ do
      specBoolR @Char "boolR" boolR
    H.describe "String" $ do
      specBoolR @String "boolR" boolR
    H.describe "[Maybe (Int, String)]" $ do
      specBoolR @[Maybe (Int, String)] "boolR" boolR
