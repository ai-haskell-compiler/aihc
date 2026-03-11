{-# LANGUAGE TypeOperators #-}

-- | Testable representation of (higher-order) functions.
--
-- See the README for an introduction.

module Test.Fun
  ( -- * Testable functions
    (:->)()
  , applyFun
  , applyFun2
  , applyFun3

    -- * Shrink and show
  , shrinkFun
  , showsPrecFun
  , indent
  , ShowsPrec

    -- * Cogenerators
  , Co

    -- ** Main combinators
  , cogenEmbed
  , cogenIntegral
  , cogenIntegral'
  , cogenFun
  , Concrete(..)
  , FunName
  , TypeName

    -- ** Generic cogenerators
  , cogenGeneric
  , (:+)(..)

    -- ** Secondary combinators
  , cogenList
  , cogenConst
  , cogenApply

    -- ** @CoArbitrary@
  , CoArbitrary(..)
  , coarbitraryGeneric

    -- ** Generic classes
  , GCoGen()
  , GCoArbitrary()
  , GSumCo
  ) where

import Test.Fun.Internal.Types
  ((:->), applyFun, applyFun2, applyFun3, Concrete(..), ShowsPrec, FunName, TypeName)
import Test.Fun.Internal.CoGen
import Test.Fun.Internal.Generic
import Test.Fun.Internal.Pretty (showsPrecFun, indent)
import Test.Fun.Internal.Shrink (shrinkFun)
import Test.Fun.Internal.Orphan ()
