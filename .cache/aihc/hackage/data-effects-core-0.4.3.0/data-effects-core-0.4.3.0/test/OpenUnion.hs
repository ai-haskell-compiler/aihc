{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=16 #-}

-- SPDX-License-Identifier: MPL-2.0

module OpenUnion where

import Control.Effect (Eff, Free, perform)
import Data.Effect (Catch, Effect, LabelOf, Throw (Throw), UnliftIO)
import Data.Effect.OpenUnion (Membership (UnsafeMembership), labelMembership, membershipAt, weakenExpsFor, weakenHOEsFor, (:>))
import Data.Proxy (Proxy (Proxy))
import GHC.TypeNats (KnownNat, Natural, natVal)
import Test.Hspec (Spec, describe, it, shouldBe)

data A x :: Effect
data ALabel
type instance LabelOf (A x) = ALabel

data B x y :: Effect
data BLabel x
type instance LabelOf (B x y) = BLabel x

spec_membership :: Spec
spec_membership = describe "Open-Union Membership" do
    it "Membership (A 0) '[B 1 2, A 0] -> 1" $
        label1 `shouldBe` UnsafeMembership 1

    it "Membership (B 5 6) '[B 1 2, B 3 4, A 1, B 5 6, A 0] -> 3" $
        label2 `shouldBe` UnsafeMembership 3

    it "Membership (B 5 ?) '[B 1 2, B 3 4, A 1, B 5 6, A 0] --infer--> ?=6" $
        infer1 labelMembership `shouldBe` 6
  where
    label1 :: Membership (A 0) '[B 1 2, A 0]
    label1 = labelMembership

    label2 :: Membership (B 5 6) '[B 1 2, B 3 4, A 1, B 5 6, A 0]
    label2 = labelMembership

    infer1 :: forall x. (KnownNat x) => Membership (B 5 x) '[B 1 2, B 3 4, A 1, B 5 6, A 0] -> Natural
    infer1 _ = natVal @x Proxy

type F = Throw ()
type H = Catch ()

spec_onlyFOEs :: Spec
spec_onlyFOEs = describe "onlyFOEs index shift" do
    it "'[<F>, F , F , F ] -> '[ H ,<F>, F , H , H , F , H , F ]" $
        shiftIx ix0 `shouldBe` UnsafeMembership 1
    it "'[ F ,<F>, F , F ] -> '[ H , F ,<F>, H , H , F , H , F ]" $
        shiftIx ix1 `shouldBe` UnsafeMembership 2
    it "'[ F , F ,<F>, F ] -> '[ H , F , F , H , H ,<F>, H , F ]" $
        shiftIx ix2 `shouldBe` UnsafeMembership 5
    it "'[ F , F , F ,<F>] -> '[ H , F , F , H , H , F , H ,<F>]" $
        shiftIx ix3 `shouldBe` UnsafeMembership 7
  where
    shiftIx :: Membership e '[F, F, F, F] -> Membership e '[H, F, F, H, H, F, H, F]
    shiftIx = weakenHOEsFor

    ix0, ix1, ix2, ix3 :: Membership F '[F, F, F, F]
    ix0 = membershipAt @0
    ix1 = membershipAt @1
    ix2 = membershipAt @2
    ix3 = membershipAt @3

type P = Catch ()
type E = UnliftIO

spec_onlyPolys :: Spec
spec_onlyPolys = describe "onlyPolys index shift" do
    it "'[<P>, P , P , P ] -> '[ E ,<P>, P , E , E , P , E , P ]" $
        shiftIx ix0 `shouldBe` UnsafeMembership 1
    it "'[ P ,<P>, P , P ] -> '[ E , P ,<P>, E , E , P , E , P ]" $
        shiftIx ix1 `shouldBe` UnsafeMembership 2
    it "'[ P , P ,<P>, P ] -> '[ E , P , P , E , E ,<P>, E , P ]" $
        shiftIx ix2 `shouldBe` UnsafeMembership 5
    it "'[ P , P , P ,<P>] -> '[ E , P , P , E , E , P , E ,<P>]" $
        shiftIx ix3 `shouldBe` UnsafeMembership 7
  where
    shiftIx :: Membership e '[P, P, P, P] -> Membership e '[E, P, P, E, E, P, E, P]
    shiftIx = weakenExpsFor

    ix0, ix1, ix2, ix3 :: Membership P '[P, P, P, P]
    ix0 = membershipAt @0
    ix1 = membershipAt @1
    ix2 = membershipAt @2
    ix3 = membershipAt @3

inferCompileTest :: (Throw () :> es, Catch () :> es, Free Monad ff) => Eff ff es a
inferCompileTest = perform $ Throw mempty
