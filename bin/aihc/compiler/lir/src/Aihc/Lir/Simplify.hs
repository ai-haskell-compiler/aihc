-- | Simplify Boolean switches and rotate expressions before register allocation.
module Aihc.Lir.Simplify (simplifyBooleanBranches, simplifyRotations) where

import Aihc.Lir.RegAlloc (operationReads, readCounts)
import Aihc.Lir.Syntax
import Control.Applicative ((<|>))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

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

-- | Replace complementary constant shifts with a rotate before register allocation.
-- A wide expression can use a narrow rotate only when its source has zero high bits.
simplifyRotations :: Function -> Function
simplifyRotations function = rewritten {functionBlocks = map clean (functionBlocks rewritten)}
  where
    selected = map simplify (functionBlocks function)
    rewritten = function {functionBlocks = map fst selected}
    candidates = Set.unions (map snd selected)
    uses = readCounts rewritten
    occupied = Set.fromList (map fst (functionParameters function) <> concat [map fst (blockParameters block) <> concatMap instructionResults (blockInstructions block) | block <- functionBlocks function])
    fresh result = unused (Var (unVar result <> "_rotate"))
    unused v
      | Set.member v occupied = unused (Var (unVar v <> "_"))
      | otherwise = v
    clean block = block {blockInstructions = fst (foldr remove ([], uses) (blockInstructions block))}
    remove instruction@(Instruction [v] operation) (rest, counts)
      | Set.member v candidates,
        Map.findWithDefault 0 v counts == 0 =
          (rest, foldl' (flip (Map.adjust (subtract 1))) counts (operationReads operation))
      | otherwise = (instruction : rest, counts)
    remove instruction (rest, counts) = (instruction : rest, counts)
    simplify block = (block {blockInstructions = concatMap rewrite (blockInstructions block)}, removed)
      where
        definitions = Map.fromList [(v, op) | Instruction [v] op <- blockInstructions block]
        matches = Map.fromList [(v, result) | Instruction [v] (Binary Or ty a b) <- blockInstructions block, Just result <- [match ty a b <|> match ty b a <|> wideMatch ty a b <|> wideMatch ty b a]]
        removed = Set.fromList [v | (_, _, _, _, obsolete) <- Map.elems matches, v <- obsolete]
        rewrite instruction@(Instruction [v] _) = case Map.lookup v matches of
          Just (ty, source, count, Nothing, _) -> [Instruction [v] (Binary RotR ty source (OperandLiteral (LitInt count)))]
          Just (ty, source, count, Just wide, _) ->
            let result = fresh v
             in [Instruction [result] (Binary RotR ty source (OperandLiteral (LitInt count))), Instruction [v] (Convert ZExt ty (OperandVar result) wide)]
          Nothing -> [instruction]
        rewrite instruction = [instruction]
        match ty (OperandVar l) (OperandVar r) = do
          Binary ShrU lt x (OperandLiteral (LitInt right)) <- Map.lookup l definitions
          Binary Shl rt y (OperandLiteral (LitInt left)) <- Map.lookup r definitions
          let width = toInteger (typeBits ty)
              count = right `mod` width
          if lt == ty && rt == ty && x == y && (left + right) `mod` width == 0
            then Just (ty, x, count, Nothing, [l, r])
            else Nothing
        match _ _ _ = Nothing
        wideMatch wide (OperandVar l) (OperandVar r) = do
          Convert ZExt ty (OperandVar narrow) wide' <- Map.lookup l definitions
          Convert Trunc wide'' (OperandVar shifted) ty' <- Map.lookup narrow definitions
          Binary Shl shiftTy x@(OperandVar source) (OperandLiteral (LitInt left)) <- Map.lookup shifted definitions
          Binary ShrU rightTy y (OperandLiteral (LitInt right)) <- Map.lookup r definitions
          Convert ZExt sourceTy value sourceWide <- Map.lookup source definitions
          let width = toInteger (typeBits ty)
              wideWidth = toInteger (typeBits wide)
              leftCount = left `mod` wideWidth
              rightCount = right `mod` wideWidth
          if wide == wide' && wide == wide'' && wide == shiftTy && wide == rightTy && wide == sourceWide && ty == ty' && ty == sourceTy && x == y && leftCount > 0 && rightCount > 0 && leftCount + rightCount == width
            then Just (ty, value, rightCount, Just wide, [l, narrow, shifted, r, source])
            else Nothing
        wideMatch _ _ _ = Nothing
