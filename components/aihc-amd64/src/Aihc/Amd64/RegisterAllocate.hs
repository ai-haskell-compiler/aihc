-- | AMD64 policy for the shared native linear-scan allocator.
module Aihc.Amd64.RegisterAllocate
  ( Location (..),
    Allocation (..),
    allocateBlock,
    allocatableRegisters,
  )
where

import Aihc.Amd64.Lir
import Aihc.Native.RegisterAllocate (Allocation (..), AllocatorConfig (..), Location (..))
import Aihc.Native.RegisterAllocate qualified as Native

-- | Caller-clobbered registers not reserved for spill materialization or
-- runtime state. Values live across a 'Call' are assigned heap spill slots.
allocatableRegisters :: [PhysicalReg]
allocatableRegisters = [Rax, Rdi, Rsi, Rdx, Rcx, R8, R9]

allocateBlock :: [Instruction PhysicalReg] -> Allocation PhysicalReg
allocateBlock = Native.allocateBlock AllocatorConfig {allocatorRegisters = allocatableRegisters}
