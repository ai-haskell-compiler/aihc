{-# LANGUAGE CPP                #-}

#if !defined(mingw32_HOST_OS)
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
#endif

{-|
Module:      Instances.System.Posix
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides 'Arbitrary' instances for data types in the @unix@ library.
-}
module Instances.System.Posix () where

#if !defined(mingw32_HOST_OS)
import           GHC.Generics (Generic)

import           Instances.Miscellaneous ()
import           Instances.Utils.GenericArbitrary (genericArbitrary)

import           Prelude ()
import           Prelude.Compat

import           System.Posix.DynamicLinker (RTLDFlags(..), DL(..))
import           System.Posix.Process (ProcessStatus(..))
# if MIN_VERSION_unix(2,8,0)
import           System.Posix.User.ByteString
# else
import           System.Posix.User
# endif
                   (GroupEntry(..), UserEntry(..))

import           Test.QuickCheck (Arbitrary(..), arbitraryBoundedEnum)
import           Test.QuickCheck.Instances ()

deriving instance Bounded RTLDFlags
deriving instance Enum RTLDFlags
instance Arbitrary RTLDFlags where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary DL where
    arbitrary = genericArbitrary

instance Arbitrary ProcessStatus where
    arbitrary = genericArbitrary

instance Arbitrary GroupEntry where
    arbitrary = genericArbitrary

instance Arbitrary UserEntry where
    arbitrary = genericArbitrary

deriving instance Generic DL
deriving instance Generic ProcessStatus
deriving instance Generic GroupEntry
deriving instance Generic UserEntry
#endif
