{-# LANGUAGE CPP #-}
{-|
Module:      Spec.System.PosixSpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for data types in the @unix@ library.
-}
module Spec.System.PosixSpec (main, spec) where

import Prelude ()
import Prelude.Compat

import Test.Hspec (Spec, hspec, parallel)

#if !defined(mingw32_HOST_OS)
import Data.Proxy (Proxy(..))

import Instances.System.Posix ()

import Spec.Utils (matchesTextShowSpec)

import System.Posix.DynamicLinker (RTLDFlags, DL)
import System.Posix.Process (ProcessStatus)
# if MIN_VERSION_unix(2,8,0)
import System.Posix.User.ByteString
# else
import System.Posix.User
# endif
  (GroupEntry, UserEntry)

import Test.Hspec (describe)

import TextShow.System.Posix ()
#endif

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
#if !defined(mingw32_HOST_OS)
    describe "RTLDFlags" $
        matchesTextShowSpec (Proxy :: Proxy RTLDFlags)
    describe "DL" $
        matchesTextShowSpec (Proxy :: Proxy DL)
    describe "ProcessStatus" $
        matchesTextShowSpec (Proxy :: Proxy ProcessStatus)
    describe "GroupEntry" $
        matchesTextShowSpec (Proxy :: Proxy GroupEntry)
    describe "UserEntry" $
        matchesTextShowSpec (Proxy :: Proxy UserEntry)
#else
    pure ()
#endif
