{-# LANGUAGE DeriveGeneric #-}

module Generics.Case.Custom.NoParamTypeSpec (spec) where

import qualified GHC.Generics as G
import Generics.Case
import Generics.Chain
import qualified Generics.SOP as SOP
import qualified Test.Hspec as H
import qualified Test.QuickCheck as Q
import Test.QuickCheck.Function
import Util

data NoParamType
  = NPT1
  | NPT2 Int
  | NPT3 String Char
  deriving (Show, Eq, G.Generic)

instance SOP.Generic NoParamType

instance Q.Arbitrary NoParamType where
  arbitrary =
    Q.oneof
      [ pure NPT1
      , NPT2 <$> Q.arbitrary
      , NPT3 <$> Q.arbitrary <*> Q.arbitrary
      ]
  shrink = Q.genericShrink

type NPTFn r = NoParamType -> r -> (Int -> r) -> (String -> Char -> r) -> r

type NPTFnR r = r -> (Int -> r) -> (String -> Char -> r) -> NoParamType -> r

type FunArgs r = '[NoParamType, r, Fun Int r, Fun String (Fun Char r)]

type FunArgsR r = '[r, Fun Int r, Fun String (Fun Char r), NoParamType]

type NPTFun r = Chain (FunArgs r) r

type NPTFunR r = Chain (FunArgsR r) r

manual :: NPTFn r
manual npt r fromInt fromStringChar = case npt of
  NPT1 -> r
  NPT2 int -> fromInt int
  NPT3 string char -> fromStringChar string char

manualR :: NPTFnR r
manualR r fromInt fromStringChar npt = case npt of
  NPT1 -> r
  NPT2 int -> fromInt int
  NPT3 string char -> fromStringChar string char

nptL :: NPTFn r
nptL = gcase @NoParamType

nptR :: NPTFnR r
nptR = gcaseR @NoParamType

specNPT ::
  forall r.
  ( Q.Arbitrary r
  , Show r
  , Eq r
  ) =>
  String ->
  NPTFn r ->
  H.Spec
specNPT name f =
  specG @(FunArgs r)
    ("manual", mkFn manual)
    (name, mkFn f)

specNPTR ::
  forall r.
  ( Q.Arbitrary r
  , Show r
  , Eq r
  ) =>
  String ->
  NPTFnR r ->
  H.Spec
specNPTR name f =
  specG @(FunArgsR r)
    ("manual", mkFnR manualR)
    (name, mkFnR f)

mkFn ::
  NPTFn r ->
  NPTFun r
mkFn f npt' r f1 f2 = f npt' r (applyFun f1) (applyFun <$> applyFun f2)

mkFnR ::
  NPTFnR r ->
  NPTFunR r
mkFnR f r f1 f2 = f r (applyFun f1) (applyFun <$> applyFun f2)

spec :: H.Spec
spec = do
  H.describe "left" $ do
    H.describe "()" $ do
      specNPT @() "nptL" nptL
    H.describe "Char" $ do
      specNPT @Char "nptL" nptL
    H.describe "String" $ do
      specNPT @String "nptL" nptL
    H.describe "[Maybe (Int, String)]" $ do
      specNPT @[Maybe (Int, String)] "nptL" nptL
  H.describe "right" $ do
    H.describe "()" $ do
      specNPTR @() "nptR" nptR
    H.describe "Char" $ do
      specNPTR @Char "nptR" nptR
    H.describe "String" $ do
      specNPTR @String "nptR" nptR
    H.describe "[Maybe (Int, String)]" $ do
      specNPTR @[Maybe (Int, String)] "nptR" nptR
