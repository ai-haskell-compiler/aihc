{-# LANGUAGE OverloadedStrings #-}

-- | AMD64 policy for shared register allocation over CPS-GRIN.
module Aihc.Amd64.RegisterAllocate
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

-- rax is the expression result register, r10-r11 are instruction-selection
-- scratch registers, and r12-r15 hold AIHC runtime state. Values assigned one
-- of the SysV argument registers are spilled whenever they cross a C call.
allocatableRegisters :: [Text]
allocatableRegisters = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

allocateFunction :: Map GrinVar (Location Text) -> GrinFunction -> Allocation Text
allocateFunction fixedLocations =
  Native.allocateFunction
    AllocatorConfig
      { allocatorRegisters = allocatableRegisters,
        allocatorFixedLocations = fixedLocations
      }
