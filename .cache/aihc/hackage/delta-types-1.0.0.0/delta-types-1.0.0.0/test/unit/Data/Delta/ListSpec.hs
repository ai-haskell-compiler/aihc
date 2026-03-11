{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Delta.ListSpec
    ( spec
    ) where

import Prelude

import Data.Delta.Core
    ( apply
    )
import Data.Delta.List
    ( DeltaList (Append)
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , listOf
    , property
    , (===)
    )

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}

spec :: Spec
spec = do
    describe "DeltaList" $ do
        it "apply definition" $ property $
            \(xs :: [Int]) zs ->
                apply (Append xs) zs
                    === xs ++ zs

        it "apply mempty" $ property $
            \z ->
                apply (mempty :: DeltaList Int) z
                    === z

        it "apply is a morphism" $ property $
            \(x :: DeltaList Int) y zs ->
                apply (x <> y) zs
                    === (apply x . apply y) zs

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
instance Arbitrary a => Arbitrary (DeltaList a) where
    arbitrary = Append <$> listOf arbitrary
