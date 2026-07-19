-- | Architecture-neutral low-level IR used immediately before register
-- allocation. Physical registers remain backend-specific through the
-- @physical@ type parameter.
module Aihc.Native.Lir
  ( VirtualReg (..),
    Register (..),
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

data Register physical
  = Virtual !VirtualReg
  | Physical !physical
  deriving (Eq, Ord, Show)

data Instruction physical
  = Move !(Register physical) !(Register physical)
  | MoveImmediate !(Register physical) !Integer
  | Load !(Register physical) !(Register physical) !Int
  | Store !(Register physical) !(Register physical) !Int
  | Add !(Register physical) !(Register physical) !(Register physical)
  | Compare !(Register physical) !(Register physical)
  | CompareImmediate !(Register physical) !Integer
  | LoadAddress !(Register physical) !Text
  | Call !Text
  deriving (Eq, Show)

instructionDefs :: Instruction physical -> Set VirtualReg
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

instructionUses :: Instruction physical -> Set VirtualReg
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

virtual :: Register physical -> Set VirtualReg
virtual register =
  case register of
    Virtual value -> Set.singleton value
    Physical _ -> Set.empty
