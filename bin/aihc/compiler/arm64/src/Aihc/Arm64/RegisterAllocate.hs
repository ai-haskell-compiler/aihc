{-# LANGUAGE OverloadedStrings #-}

-- | AArch64 policy for shared register allocation over CPS-GRIN.
module Aihc.Arm64.RegisterAllocate
  ( Location (..),
    Allocation (..),
    allocateFunction,
    allocatableRegisters,
  )
where

import Aihc.Grin.Syntax (GrinFunction, GrinVar)
import Aihc.Native.RegisterAllocate (Allocation (..), AllocatorConfig (..), Location (..))
import Aihc.Native.RegisterAllocate qualified as Native
import Data.Map.Strict (Map)
import Data.Text (Text)

-- x9-x11 remain instruction-selection scratch registers. x18 is reserved by
-- Darwin, and x19-x22 hold AIHC runtime state.
allocatableRegisters :: [Text]
allocatableRegisters = ["x12", "x13", "x14", "x15", "x16", "x17"]

allocateFunction :: Map GrinVar (Location Text) -> GrinFunction -> Allocation Text
allocateFunction fixedLocations =
  Native.allocateFunction
    AllocatorConfig
      { allocatorRegisters = allocatableRegisters,
        allocatorFixedLocations = fixedLocations
      }
