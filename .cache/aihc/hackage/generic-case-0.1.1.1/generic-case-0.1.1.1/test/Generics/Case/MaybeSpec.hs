module Generics.Case.MaybeSpec (spec) where

import Generics.Case
import Generics.Chain
import qualified Test.Hspec as H
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Function
import Util

type MaybeFn a r = Maybe a -> r -> (a -> r) -> r

type MaybeFnR a r = r -> (a -> r) -> Maybe a -> r

type FunArgs a r = '[Maybe a, r, Fun a r]

type FunArgsR a r = '[r, Fun a r, Maybe a]

type MaybeFun a r = Chain (FunArgs a r) r

type MaybeFunR a r = Chain (FunArgsR a r) r

manual :: MaybeFn a r
manual m r f = maybe r f m

manualR :: MaybeFnR a r
manualR = maybe

specMaybe ::
  forall a r.
  ( Show a
  , Function a
  , Q.Arbitrary r
  , Q.CoArbitrary a
  , Q.Arbitrary a
  , Show r
  , Eq r
  ) =>
  String ->
  MaybeFn a r ->
  H.Spec
specMaybe name f =
  specG @(FunArgs a r)
    ("maybe", mkFn manual)
    (name, mkFn f)

specMaybeR ::
  forall a r.
  ( Show a
  , Function a
  , Q.Arbitrary r
  , Q.CoArbitrary a
  , Q.Arbitrary a
  , Show r
  , Eq r
  ) =>
  String ->
  MaybeFnR a r ->
  H.Spec
specMaybeR name f =
  specG @(FunArgsR a r)
    ("maybe", mkFnR manualR)
    (name, mkFnR f)

mkFn ::
  MaybeFn a r ->
  MaybeFun a r
mkFn f m r fn = f m r (applyFun fn)

mkFnR ::
  MaybeFnR a r ->
  MaybeFunR a r
mkFnR f r fn = f r (applyFun fn)

spec :: H.Spec
spec = do
  H.describe "left" $ do
    H.describe "Maybe () -> Char" $ do
      specMaybe @() @Char "maybeL" maybeL
    H.describe "Maybe Char -> Either String ()" $ do
      specMaybe @Char @(Either String ()) "maybeL" maybeL
    H.describe "Maybe String -> (Int, Either Integer Int)" $ do
      specMaybe @String @(Int, Either Integer Int) "maybeL" maybeL
    H.describe "Maybe [Maybe (Int, String)] -> (Int, [Either (Maybe ()) String])" $ do
      specMaybe @(Maybe (Int, String)) @(Int, [Either (Maybe ()) String]) "maybeL" maybeL
  H.describe "right" $ do
    H.describe "Maybe () -> Char" $ do
      specMaybeR @() @Char "maybeR" maybeR
    H.describe "Maybe Char -> Either String ()" $ do
      specMaybeR @Char @(Either String ()) "maybeR" maybeR
    H.describe "Maybe String -> (Int, Either Integer Int)" $ do
      specMaybeR @String @(Int, Either Integer Int) "maybeR" maybeR
    H.describe "Maybe [Maybe (Int, String)] -> (Int, [Either (Maybe ()) String])" $ do
      specMaybeR @(Maybe (Int, String)) @(Int, [Either (Maybe ()) String]) "maybeR" maybeR
