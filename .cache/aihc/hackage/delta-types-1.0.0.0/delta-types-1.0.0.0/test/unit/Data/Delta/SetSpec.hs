{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Delta.SetSpec
    ( spec
    ) where

import Prelude

import Data.Delta.Core
    ( apply
    )
import Data.Delta.Set
    ( DeltaSet1 (Delete, Insert)
    , deltaSetFromList
    , diffSet
    , listFromDeltaSet
    )
import Data.List
    ( foldl'
    )
import Data.Set
    ( Set
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , elements
    , forAll
    , infiniteList
    , listOf
    , oneof
    , property
    , (===)
    , (==>)
    , (.&&.)
    )

import qualified Data.Set as Set

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}

spec :: Spec
spec = do
    describe "DeltaSet1" $ do
        it "cancellation laws" $ property $
            \(x :: Int) -> forAll (genSetWithOrWithout x) $ \zs ->
                apply [Insert x, Delete x] zs === apply [Insert x] zs
                .&&. apply [Insert x, Insert x] zs === apply [Insert x] zs
                .&&. apply [Delete x, Insert x] zs === apply [Delete x] zs
                .&&. apply [Delete x, Delete x] zs === apply [Delete x] zs

    describe "DeltaSet" $ do
        describe "diffSet" $ do
            it "example" $ property $
                \(a :: Int) b c ->
                (a /= b && a /= c && b /= c) ==>
                Set.fromList [a, c] `diffSet` Set.fromList [a, b]
                    == deltaSetFromList [Insert c, Delete b]

            it "new = apply (diffSet new old) old" $ property $
                \(whole :: Set Int) -> forAll (genSubset whole) $
                \old -> forAll (genSubset whole) $
                \new ->
                    new === apply (new `diffSet` old) old

        describe "to/from DeltaSet1" $ do
            it "deltaSetFromList . listFromDeltaSet = id" $ property $
                \(whole :: Set Int) -> forAll (genSubset whole) $
                \a -> forAll (genSubset whole) $
                \b ->
                    let d = (a `diffSet` b)
                    in  d == (deltaSetFromList . listFromDeltaSet) d

            it "apply (deltaSetFromList ds) = apply ds" $ property $
                \(whole :: Set Int) ->
                    forAll (genDeltaSet1FromElements whole) $
                \xs -> forAll (genSubset whole) $
                \zs ->
                    apply (deltaSetFromList xs) zs
                        === apply xs zs

{-----------------------------------------------------------------------------
    Random generators
------------------------------------------------------------------------------}
-- | Generate a 'Set' with equal chances of containing a given item.
genSetWithOrWithout :: (Ord a, Arbitrary a) => a -> Gen (Set a)
genSetWithOrWithout x =
    oneof
        [ Set.insert x <$> arbitrary
        , Set.delete x <$> arbitrary
        ]

-- | Generate a random subset of a given 'Set'.
-- Useful for generating multiple sets that have common elements.
genSubset :: Ord a => Set a -> Gen (Set a)
genSubset xs = do
    isMembers <- infiniteList
    pure
        $ foldl' (flip addMember) Set.empty
        $ zip (Set.toList xs) isMembers
  where
    addMember (x, True) = Set.insert x
    addMember (_, False) = id

-- | Generate a random list of 'DeltaSet1' by picking items from a 'Set'.
genDeltaSet1FromElements :: Set a -> Gen [DeltaSet1 a]
genDeltaSet1FromElements xs
    | Set.null xs = pure []
    | otherwise =
        listOf $ oneof [Insert <$> genElement, Delete <$> genElement]
  where
    genElement = elements $ Set.toList xs
