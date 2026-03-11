{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module:      Instances.Data.Bifunctor
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for data types in the @bifunctors@ library.
-}
module Instances.Data.Bifunctor () where

import Data.Bifunctor.Biff (Biff(..))
import Data.Bifunctor.Clown (Clown(..))
import Data.Bifunctor.Fix (Fix(..))
import Data.Bifunctor.Flip (Flip(..))
import Data.Bifunctor.Join (Join(..))
import Data.Bifunctor.Joker (Joker(..))
import Data.Bifunctor.Product (Product(..))
import Data.Bifunctor.Sum (Sum(..))
import Data.Bifunctor.Tannen (Tannen(..))
import Data.Bifunctor.Wrapped (WrappedBifunctor(..))

import Instances.Utils.GenericArbitrary (genericArbitrary)

import Test.QuickCheck (Arbitrary(..))

deriving instance Arbitrary (p (f a) (g b)) => Arbitrary (Biff p f g a b)
deriving instance Arbitrary (f a)           => Arbitrary (Clown f a b)
deriving instance Arbitrary (p (Fix p a) a) => Arbitrary (Fix p a)
deriving instance Arbitrary (p b a)         => Arbitrary (Flip p a b)
deriving instance Arbitrary (p a a)         => Arbitrary (Join p a)
deriving instance Arbitrary (g b)           => Arbitrary (Joker g a b)
deriving instance Arbitrary (f (p a b))     => Arbitrary (Tannen f p a b)
deriving instance Arbitrary (p a b)         => Arbitrary (WrappedBifunctor p a b)

instance (Arbitrary (f a b), Arbitrary (g a b)) => Arbitrary (Product f g a b) where
    arbitrary = genericArbitrary

instance (Arbitrary (p a b), Arbitrary (q a b)) => Arbitrary (Sum p q a b) where
    arbitrary = genericArbitrary
