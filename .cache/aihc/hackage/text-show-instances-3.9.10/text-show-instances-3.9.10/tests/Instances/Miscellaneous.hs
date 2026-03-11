{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module:      Instances.Miscellaneous
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Provides miscellaneous 'Arbitrary' instances (taken from @text-show@).
-}
module Instances.Miscellaneous () where

#include "HsBaseConfig.h"

import Foreign.Ptr (Ptr, nullPtr, plusPtr)

import Generics.Deriving.Instances ()

import Prelude ()
import Prelude.Compat

import System.IO (Handle, stdin, stdout, stderr)
#if defined(HTYPE_GID_T)
import System.Posix.Types (CGid(..))
#endif
#if defined(HTYPE_UID_T)
import System.Posix.Types (CUid(..))
#endif

import Test.QuickCheck (Arbitrary(..), oneof)

instance Arbitrary Handle where
    arbitrary = oneof $ map pure [stdin, stdout, stderr]

instance Arbitrary (Ptr a) where
    arbitrary = plusPtr nullPtr <$> arbitrary

#if defined(HTYPE_GID_T)
deriving instance Arbitrary CGid
#endif

#if defined(HTYPE_UID_T)
deriving instance Arbitrary CUid
#endif
