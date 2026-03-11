{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Delta.CoreSpec
    ( spec
    ) where

import Prelude

import Data.Delta.Core
    ( Replace (Replace)
    , apply
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
    ( property
    , (===)
    )

{-----------------------------------------------------------------------------
    Tests
------------------------------------------------------------------------------}

spec :: Spec
spec = do
    describe "Replace" $ do
        it "apply is a morphism" $ property $
            \(x :: Int) y z ->
                apply (Replace x <> Replace y) z
                    === (apply (Replace x) . apply (Replace y)) z
        
        it "apply (Replace x <> _) = apply (Replace x)" $ property $
            \(x :: Int) y z ->
                apply (Replace x <> Replace y) z
                    === apply (Replace x) z

    describe "Lists of deltas" $ do
        it "apply []" $ property $
            \(z :: Int) ->
                apply ([] :: [Replace Int]) z === z

        it "apply is a morphism" $ property $
            \xs ys (z :: [Int]) ->
                let d1 = map Append xs
                    d2 = map Append ys
                in
                    apply (d1 ++ d2) z
                        === (apply d1 . apply d2) z
