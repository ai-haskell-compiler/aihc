-- | Simplify Boolean switches before register allocation.
module Aihc.Lir.Simplify (simplifyBooleanBranches) where

import Aihc.Lir.RegAlloc (readCounts)
import Aihc.Lir.Syntax
import Control.Applicative ((<|>))
import Data.Map.Strict qualified as Map

-- | A zero extension from i1 can only produce zero or one.
-- Keep the extension if another instruction or an edge argument reads it.
-- Keep switches without both destinations, because their other path traps.
simplifyBooleanBranches :: Function -> Function
simplifyBooleanBranches function =
  function {functionBlocks = map simplifyBlock (functionBlocks function)}
  where
    uses = readCounts function
    simplifyBlock block =
      case (reverse (blockInstructions block), blockTerminator block) of
        (Instruction [wide] (Convert ZExt I1 condition wideType) : before, Switch ty (OperandVar scrutinee) cases fallback)
          | wide == scrutinee,
            wideType == ty,
            Just targets <- traverse literalCase cases,
            Just whenTrue <- lookup 1 targets <|> fallback,
            Just whenFalse <- lookup 0 targets <|> fallback ->
              block
                { blockInstructions =
                    if Map.lookup wide uses == Just 1
                      then reverse before
                      else blockInstructions block,
                  blockTerminator = Branch condition whenTrue whenFalse
                }
        _ -> block
    literalCase (SwitchCase value target) = Just (value, target)
    literalCase (SwitchCaseConstant _ _) = Nothing
