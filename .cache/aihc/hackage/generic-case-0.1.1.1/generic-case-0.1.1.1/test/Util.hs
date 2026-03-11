{-# LANGUAGE DerivingVia #-}

module Util where

import Data.SOP
import Data.SOP.NP
import Generics.Chain
import qualified Test.Hspec as H
import qualified Test.Hspec.QuickCheck as H
import qualified Test.QuickCheck as Q

newtype ChainF r xs = ChainF (NP I xs -> r)

propG ::
  forall args r.
  (SListI args, All Show args, Eq r, Show r) =>
  (String, Chain args r) ->
  (String, Chain args r) ->
  NP I args ->
  Q.Property
propG (refName, refF) (name, f) args =
  let expected = fromChain @args @r refF args
      actual = fromChain @args @r f args
      argsS = unwords $ fmap ($ "") $ collapse_NP $ cmap_NP (Proxy @Show) (K . showsPrec 11 . unI) args
      expS = unwords [refName, argsS, "=", show expected]
      actS = unwords [name, argsS, "=", show actual]
      s = unlines [expS, actS]
  in  Q.counterexample s $ expected == actual

testG ::
  forall args r.
  (SListI args, All Show args, Eq r, Show r, Q.Arbitrary r, All Q.Arbitrary args) =>
  (String, Chain args r) ->
  (String, Chain args r) ->
  Q.Property
testG ref f = Q.property @(ChainF Q.Property args) $ ChainF $ propG @args @r ref f

specG ::
  forall args r.
  (SListI args, All Show args, Eq r, Show r, Q.Arbitrary r, All Q.Arbitrary args) =>
  (String, Chain args r) ->
  (String, Chain args r) ->
  H.Spec
specG (refName, refF) (name, f) =
  H.prop (name <> " = " <> refName) $ testG @args @r (refName, refF) (name, f)

instance
  (SListI xs, All Show xs, Q.Testable r, All Q.Arbitrary xs) =>
  Q.Testable (ChainF r xs)
  where
  property (ChainF chain) = case sList @xs of
    SNil -> Q.property $ chain Nil
    SCons -> Q.property $ \x -> ChainF $ \xs -> chain (I x :* xs)
