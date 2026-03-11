module GHC.CmmToLlvm.Version.Bounds
  ( supportedLlvmVersionLowerBound
  , supportedLlvmVersionUpperBound
  )
where

import GHC.Prelude ()

import GHC.CmmToLlvm.Version.Type

import qualified Data.List.NonEmpty as NE

-- | The (inclusive) lower bound on the LLVM Version that is currently supported.
supportedLlvmVersionLowerBound :: LlvmVersion
supportedLlvmVersionLowerBound = LlvmVersion (13 NE.:| [])

-- | The (not-inclusive) upper bound  bound on the LLVM Version that is currently supported.
supportedLlvmVersionUpperBound :: LlvmVersion
supportedLlvmVersionUpperBound = LlvmVersion (20 NE.:| [])
