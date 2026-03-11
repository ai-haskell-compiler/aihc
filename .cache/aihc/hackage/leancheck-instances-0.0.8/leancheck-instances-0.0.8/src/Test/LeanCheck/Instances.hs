-- |
-- Module      : Test.LeanCheck.Instances
-- Copyright   : (c) 2018-2022 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- This module provides "Test.LeanCheck" 'Listable' instances for types
-- provided by the Haskell Platform.  More specifically, for types of the
-- following packages:
--
-- * <https://hackage.haskell.org/package/bytestring bytestring>
-- * <https://hackage.haskell.org/package/containers containers>
-- * <https://hackage.haskell.org/package/nats       nats>
-- * <https://hackage.haskell.org/package/text       text>
-- * <https://hackage.haskell.org/package/time       time>
-- * <https://hackage.haskell.org/package/array      array>
--
-- These instances are not defined in the
-- <https://hackage.haskell.org/package/leancheck main leancheck package>
-- to not introduce needless dependencies to LeanCheck.
--
-- These instances are defined in the following submodules:
--
-- * "Test.LeanCheck.Instances.Text"
-- * "Test.LeanCheck.Instances.Natural"
-- * "Test.LeanCheck.Instances.ByteString"
-- * "Test.LeanCheck.Instances.Containers"
-- * "Test.LeanCheck.Instances.Time"
-- * "Test.LeanCheck.Instances.Array"
--
-- Check the Haddock documentation of each module listed above for the full
-- list of supported types.
module Test.LeanCheck.Instances () where

import Test.LeanCheck (Listable) -- for Haddock
import Test.LeanCheck.Instances.Text ()
import Test.LeanCheck.Instances.Natural ()
import Test.LeanCheck.Instances.Array ()
import Test.LeanCheck.Instances.ByteString ()
import Test.LeanCheck.Instances.Containers ()
import Test.LeanCheck.Instances.Time ()
import Test.LeanCheck.Instances.Void ()
