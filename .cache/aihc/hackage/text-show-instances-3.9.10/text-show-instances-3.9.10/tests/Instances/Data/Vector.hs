{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      Instances.Data.Vector
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for 'Vector' types.
-}
module Instances.Data.Vector () where

import           Data.Vector.Fusion.Bundle.Size (Size(..))

import           GHC.Generics (Generic)

import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Test.QuickCheck (Arbitrary(..))
import           Test.QuickCheck.Instances ()

#if !MIN_VERSION_quickcheck_instances(0,3,32)
import           Data.Vector.Generic (fromList)
import qualified Data.Vector.Primitive as P (Vector)
import           Data.Vector.Primitive (Prim)

import           Prelude ()
import           Prelude.Compat
#endif

#if !MIN_VERSION_quickcheck_instances(0,3,32)
instance (Arbitrary a, Prim a) => Arbitrary (P.Vector a) where
    arbitrary = fromList <$> arbitrary
#endif

instance Arbitrary Size where
    arbitrary = genericArbitrary

deriving instance Generic Size
