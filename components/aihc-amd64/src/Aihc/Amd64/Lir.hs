-- | Small AMD64-shaped low-level IR used immediately before register
-- allocation. Blocks may contain virtual registers, but calls and control-flow
-- edges are explicit.
module Aihc.Amd64.Lir
  ( VirtualReg (..),
    Register (..),
    PhysicalReg (..),
    Instruction (..),
    instructionDefs,
    instructionUses,
  )
where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

newtype VirtualReg = VirtualReg Int
  deriving (Eq, Ord, Show)

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

data Register
  = Virtual !VirtualReg
  | Physical !PhysicalReg
  deriving (Eq, Ord, Show)

data Instruction
  = Move !Register !Register
  | MoveImmediate !Register !Integer
  | Load !Register !Register !Int
  | Store !Register !Register !Int
  | Add !Register !Register !Register
  | Compare !Register !Register
  | CompareImmediate !Register !Integer
  | LoadAddress !Register !Text
  | Call !Text
  deriving (Eq, Show)

instructionDefs :: Instruction -> Set VirtualReg
instructionDefs instruction =
  case instruction of
    Move destination _ -> virtual destination
    MoveImmediate destination _ -> virtual destination
    Load destination _ _ -> virtual destination
    Store {} -> Set.empty
    Add destination _ _ -> virtual destination
    Compare {} -> Set.empty
    CompareImmediate {} -> Set.empty
    LoadAddress destination _ -> virtual destination
    Call {} -> Set.empty

instructionUses :: Instruction -> Set VirtualReg
instructionUses instruction =
  case instruction of
    Move _ source -> virtual source
    MoveImmediate {} -> Set.empty
    Load _ base _ -> virtual base
    Store source base _ -> virtual source <> virtual base
    Add _ left right -> virtual left <> virtual right
    Compare left right -> virtual left <> virtual right
    CompareImmediate value _ -> virtual value
    LoadAddress {} -> Set.empty
    Call {} -> Set.empty

virtual :: Register -> Set VirtualReg
virtual register =
  case register of
    Virtual value -> Set.singleton value
    Physical _ -> Set.empty
