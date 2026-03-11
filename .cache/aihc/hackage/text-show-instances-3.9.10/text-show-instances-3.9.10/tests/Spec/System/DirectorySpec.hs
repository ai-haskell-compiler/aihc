{-|
Module:      Spec.System.DirectorySpec
Copyright:   (C) 2014-2017 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

@hspec@ tests for 'Permissions'.
-}
module Spec.System.DirectorySpec (main, spec) where

import Data.Proxy (Proxy(..))

import Instances.System.Directory ()

import Prelude ()
import Prelude.Compat

import Spec.Utils (matchesTextShowSpec)

import System.Directory (Permissions, XdgDirectory)

import Test.Hspec (Spec, describe, hspec, parallel)

import TextShow.System.Directory ()

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "Permissions" $
        matchesTextShowSpec (Proxy :: Proxy Permissions)
    describe "XdgDirectory" $
        matchesTextShowSpec (Proxy :: Proxy XdgDirectory)
