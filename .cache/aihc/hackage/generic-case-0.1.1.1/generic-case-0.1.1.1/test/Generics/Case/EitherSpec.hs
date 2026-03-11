module Generics.Case.EitherSpec (spec) where

import Generics.Case
import Generics.Chain
import qualified Test.Hspec as H
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Function
import Util

type EitherFn a b r = Either a b -> (a -> r) -> (b -> r) -> r

type EitherFnR a b r = (a -> r) -> (b -> r) -> Either a b -> r

type FunArgs a b r = '[Either a b, Fun a r, Fun b r]

type FunArgsR a b r = '[Fun a r, Fun b r, Either a b]

type EitherFun a b r = Chain (FunArgs a b r) r

type EitherFunR a b r = Chain (FunArgsR a b r) r

manual :: EitherFn a b r
manual e l r = either l r e

manualR :: EitherFnR a b r
manualR = either

specEither ::
  forall a b r.
  ( Show a
  , Function a
  , Q.CoArbitrary a
  , Q.Arbitrary a
  , Show b
  , Function b
  , Q.CoArbitrary b
  , Q.Arbitrary b
  , Q.Arbitrary r
  , Show r
  , Eq r
  ) =>
  String ->
  EitherFn a b r ->
  H.Spec
specEither name f =
  specG @(FunArgs a b r)
    ("either", mkFn manual)
    (name, mkFn f)

specEitherR ::
  forall a b r.
  ( Show a
  , Function a
  , Q.CoArbitrary a
  , Q.Arbitrary a
  , Show b
  , Function b
  , Q.CoArbitrary b
  , Q.Arbitrary b
  , Q.Arbitrary r
  , Show r
  , Eq r
  ) =>
  String ->
  EitherFnR a b r ->
  H.Spec
specEitherR name f =
  specG @(FunArgsR a b r)
    ("either", mkFnR manualR)
    (name, mkFnR f)

mkFn ::
  EitherFn a b r ->
  EitherFun a b r
mkFn e x f g = e x (applyFun f) (applyFun g)

mkFnR ::
  EitherFnR a b r ->
  EitherFunR a b r
mkFnR e f g = e (applyFun f) (applyFun g)

spec :: H.Spec
spec = do
  H.describe "left" $ do
    H.describe "Either () Char -> Char" $ do
      specEither @() @Char @Char "eitherL" eitherL
    H.describe "Either Char String -> Either String ()" $ do
      specEither @Char @String @(Either String ()) "eitherL" eitherL
    H.describe "Either String (Maybe Integer) -> (Int, Either Integer Int)" $ do
      specEither @String @(Maybe Integer) @(Int, Either Integer Int) "eitherL" eitherL
    H.describe "Either [Maybe (Int, String)] Int -> (Int, [Either (Maybe ()) String])" $ do
      specEither @(Maybe (Int, String)) @Int @(Int, [Either (Maybe ()) String]) "eitherL" eitherL
  H.describe "right" $ do
    H.describe "Either () Char -> Char" $ do
      specEitherR @() @Char @Char "eitherR" eitherR
    H.describe "Either Char String -> Either String ()" $ do
      specEitherR @Char @String @(Either String ()) "eitherR" eitherR
    H.describe "Either String (Maybe Integer) -> (Int, Either Integer Int)" $ do
      specEitherR @String @(Maybe Integer) @(Int, Either Integer Int) "eitherR" eitherR
    H.describe "Either [Maybe (Int, String)] Int -> (Int, [Either (Maybe ()) String])" $ do
      specEitherR @(Maybe (Int, String)) @Int @(Int, [Either (Maybe ()) String]) "eitherR" eitherR
