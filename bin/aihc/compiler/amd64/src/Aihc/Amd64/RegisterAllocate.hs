-- | AMD64 policy for shared register allocation over CPS-GRIN.
module Aihc.Amd64.RegisterAllocate
  ( Location (..),
    Allocation (..),
    allocateFunction,
    allocatableRegisters,
  )
where

import Aihc.Amd64.Assemble (Amd64Register (..))
import Aihc.Grin.Syntax (GrinFunction, GrinVar)
import Aihc.Native.RegisterAllocate (Allocation (..), AllocatorConfig (..), Location (..))
import Aihc.Native.RegisterAllocate qualified as Native
import Data.Map.Strict (Map)

-- rax is the expression result register, r10-r11 are instruction-selection
-- scratch registers, and r12-r15 hold AIHC runtime state. Values assigned one
-- of the SysV argument registers are spilled whenever they cross a C call.
allocatableRegisters :: [Amd64Register]
allocatableRegisters = [RDI, RSI, RDX, RCX, R8, R9]

allocateFunction :: Map GrinVar (Location Amd64Register) -> GrinFunction -> Allocation Amd64Register
allocateFunction fixedLocations =
  Native.allocateFunction
    AllocatorConfig
      { allocatorRegisters = allocatableRegisters,
        allocatorFixedLocations = fixedLocations
      }
