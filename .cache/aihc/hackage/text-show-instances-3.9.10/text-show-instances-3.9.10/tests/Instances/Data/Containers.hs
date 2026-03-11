{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      Instances.Data.Containers
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types located in @containers@.
-}
module Instances.Data.Containers () where

import           Data.Graph (SCC(..))
import           Data.Sequence (ViewL(..), ViewR(..))

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Test.QuickCheck (Arbitrary(..))
import           Test.QuickCheck.Instances ()

instance Arbitrary vertex => Arbitrary (SCC vertex) where
    arbitrary = genericArbitrary

instance Arbitrary a => Arbitrary (ViewL a) where
    arbitrary = genericArbitrary

instance Arbitrary a => Arbitrary (ViewR a) where
    arbitrary = genericArbitrary
