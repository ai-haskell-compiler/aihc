-- | AArch64 policy for shared register allocation over CPS-GRIN.
module Aihc.Arm64.RegisterAllocate
  ( Location (..),
    Allocation (..),
    allocateFunction,
    allocatableRegisters,
  )
where

import Aihc.Arm64.Assemble (Arm64Register (..))
import Aihc.Grin.Syntax (GrinFunction, GrinVar)
import Aihc.Native.RegisterAllocate (Allocation (..), AllocatorConfig (..), Location (..))
import Aihc.Native.RegisterAllocate qualified as Native
import Data.Map.Strict (Map)

-- x9-x11 remain instruction-selection scratch registers. x18 is reserved by
-- Darwin, and x19-x22 hold AIHC runtime state.
allocatableRegisters :: [Arm64Register]
allocatableRegisters = [X12, X13, X14, X15, X16, X17]

allocateFunction :: Map GrinVar (Location Arm64Register) -> GrinFunction -> Allocation Arm64Register
allocateFunction fixedLocations =
  Native.allocateFunction
    AllocatorConfig
      { allocatorRegisters = allocatableRegisters,
        allocatorFixedLocations = fixedLocations
      }
