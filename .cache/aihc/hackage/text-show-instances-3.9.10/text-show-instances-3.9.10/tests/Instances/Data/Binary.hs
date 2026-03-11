{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      Instances.Data.Binary
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for 'Decoder's.
-}
module Instances.Data.Binary () where

import Data.Binary.Get.Internal (Decoder(..))

import GHC.Generics (Generic)

import Instances.Utils.GenericArbitrary (genericArbitrary)

import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Instances ()

instance Arbitrary a => Arbitrary (Decoder a) where
    arbitrary = genericArbitrary

deriving instance Generic (Decoder a)
