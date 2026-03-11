{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      Instances.System.Directory
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for 'Permissions'.
-}
module Instances.System.Directory () where

import qualified Generics.Deriving.TH as Generics (deriveAll0)
import           Instances.Utils.GenericArbitrary (genericArbitrary)
import           System.Directory (Permissions, XdgDirectory(..))
import           Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

$(Generics.deriveAll0 ''Permissions)
instance Arbitrary Permissions where
    arbitrary = genericArbitrary

instance Arbitrary XdgDirectory where
    arbitrary = arbitraryBoundedEnum
