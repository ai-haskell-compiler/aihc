-- | AMD64 physical registers for the shared native LIR.
module Aihc.Amd64.Lir
  ( module Aihc.Native.Lir,
    PhysicalReg (..),
  )
where

import Aihc.Native.Lir

data PhysicalReg
  = Rax
  | Rdi
  | Rsi
  | Rdx
  | Rcx
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  deriving (Eq, Ord, Show, Enum, Bounded)
