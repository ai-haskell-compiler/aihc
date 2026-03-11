{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      Instances.System.Locale
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for old 'TimeLocale' values.
-}
module Instances.System.Locale () where

import           GHC.Generics (Generic)
import           Instances.Utils.GenericArbitrary (genericArbitrary)
import           System.Locale (TimeLocale(..))
import           Test.QuickCheck (Arbitrary(..))

instance Arbitrary TimeLocale where
    arbitrary = genericArbitrary

deriving instance Generic TimeLocale
