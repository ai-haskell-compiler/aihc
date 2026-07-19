-- | AArch64 policy for the shared native linear-scan allocator.
module Aihc.Arm64.RegisterAllocate
  ( Location (..),
    Allocation (..),
    allocateBlock,
    allocatableRegisters,
  )
where

import Aihc.Arm64.Lir
import Aihc.Native.RegisterAllocate (Allocation (..), AllocatorConfig (..), Location (..))
import Aihc.Native.RegisterAllocate qualified as Native

-- | Registers which are caller-clobbered by C and not reserved for the linker,
-- platform, frame pointer, link register, spill materialization, or runtime
-- state. Values live across a 'Call' are assigned heap spill slots instead.
allocatableRegisters :: [PhysicalReg]
allocatableRegisters = [X9, X10, X11, X12, X13, X14, X15]

allocateBlock :: [Instruction PhysicalReg] -> Allocation PhysicalReg
allocateBlock = Native.allocateBlock AllocatorConfig {allocatorRegisters = allocatableRegisters}
