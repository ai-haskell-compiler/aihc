{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      Instances.Data.Vector
Copyright:   (C) 2023 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for the 'NonEmptyVector' type.
-}
module Instances.Data.Vector.NonEmpty () where

import           Data.Vector.NonEmpty as B (NonEmptyVector, fromNonEmpty)

import           Prelude ()
import           Prelude.Compat

import           Test.QuickCheck (Arbitrary(..))
import           Test.QuickCheck.Instances ()

instance Arbitrary a => Arbitrary (B.NonEmptyVector a) where
    arbitrary = fromNonEmpty <$> arbitrary
