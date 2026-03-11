{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | Show for @(':->')@.
--
-- === Warning
--
-- This is an internal module: it is not subject to any versioning policy,
-- breaking changes can happen at any time.
-- It is made available only for debugging.
-- Otherwise, use "Test.Fun".
--
-- If something here seems useful, please open an issue to export it from an
-- external module.

module Test.Fun.Internal.Orphan where

import Test.Fun.Internal.Types ((:->))
import Test.Fun.Internal.Pretty (showsPrecFun)

-- | Pretty-printed 'Show' instance.
instance Show r => Show (a :-> r) where
  showsPrec = showsPrecFun showsPrec
