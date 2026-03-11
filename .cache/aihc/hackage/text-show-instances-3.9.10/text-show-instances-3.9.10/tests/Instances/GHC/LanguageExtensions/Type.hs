{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Module:      Instances.GHC.LanguageExtensions.Type
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides an 'Arbitrary' instance for the 'Extension' data type.
-}
module Instances.GHC.LanguageExtensions.Type () where

import GHC.LanguageExtensions.Type (Extension(..))
import Language.Haskell.TH.Instances ()
import Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

instance Arbitrary Extension where
    arbitrary = arbitraryBoundedEnum
