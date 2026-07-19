-- | AArch64 physical registers for the shared native LIR.
module Aihc.Arm64.Lir
  ( module Aihc.Native.Lir,
    PhysicalReg (..),
  )
where

import Aihc.Native.Lir

data PhysicalReg
  = X0
  | X1
  | X2
  | X3
  | X4
  | X5
  | X6
  | X7
  | X8
  | X9
  | X10
  | X11
  | X12
  | X13
  | X14
  | X15
  | X19
  | X20
  | X21
  | X22
  deriving (Eq, Ord, Show, Enum, Bounded)
