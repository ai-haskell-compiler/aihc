{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      Instances.System.Console.Haskeline
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types in the @haskeline@ library.
-}
module Instances.System.Console.Haskeline () where

import           GHC.Generics (Generic)

import           Instances.Utils ((<@>))
import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Prelude ()
import           Prelude.Compat

import           System.Console.Haskeline (Interrupt(..))
import           System.Console.Haskeline.Completion (Completion(..))
import           System.Console.Haskeline.History (History, addHistory, emptyHistory)

import           Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)

deriving instance Bounded Interrupt
deriving instance Enum Interrupt
instance Arbitrary Interrupt where
    arbitrary = arbitraryBoundedEnum

-- instance Arbitrary Prefs

instance Arbitrary Completion where
    arbitrary = genericArbitrary

instance Arbitrary History where
    arbitrary = addHistory <$> arbitrary <@> emptyHistory

deriving instance Generic Completion
