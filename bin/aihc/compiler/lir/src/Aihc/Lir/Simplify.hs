-- | Simplify Boolean switches, rotate expressions, arithmetic widths, and
-- pointer copies before register allocation.
module Aihc.Lir.Simplify (removePointerCopies, simplifyBooleanBranches, simplifyRotations, simplifyWidths) where

import Aihc.Lir.RegAlloc (Interval (..), functionIntervals, operationReads, readCounts)
import Aihc.Lir.Syntax
import Control.Applicative ((<|>))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
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

-- | Use 32-bit arithmetic when a truncation discards the high bits.
-- Only modular operations permit this transformation. Division and shifts do not.
-- Keep wide operations with other users to avoid duplicate arithmetic.
-- Do not replace one result truncation with two operand truncations.
simplifyWidths :: Function -> Function
simplifyWidths function = rewritten {functionBlocks = map clean (functionBlocks rewritten)}
  where
    uses = readCounts function
    occupied = Set.fromList (map fst (functionParameters function) <> concat [map fst (blockParameters block) <> concatMap instructionResults (blockInstructions block) | block <- functionBlocks function])
    (blocks, _) = mapAccumLBlocks occupied (functionBlocks function)
    rewritten = function {functionBlocks = blocks}
    candidates = Set.fromList [v | block <- functionBlocks function, Instruction [v] op <- blockInstructions block, removable op]
    remaining = readCounts rewritten
    clean block = block {blockInstructions = fst (foldr remove ([], remaining) (blockInstructions block))}
    remove (Instruction [v] operation) (rest, counts)
      | Set.member v candidates,
        Map.findWithDefault 0 v counts == 0 =
          (rest, foldl' (flip (Map.adjust (subtract 1))) counts (operationReads operation))
    remove instruction (rest, counts) = (instruction : rest, counts)
    modular op = op `elem` [Add, Sub, Mul, And, Or, Xor]
    removable (Binary op I64 _ _) = modular op
    removable (Convert op _ _ _) = op `elem` [Trunc, ZExt]
    removable _ = False
    mapAccumLBlocks names [] = ([], names)
    mapAccumLBlocks names (block : rest) =
      let definitions = Map.fromList [(v, op) | Instruction [v] op <- blockInstructions block]
          (instructions, _, names') = foldl' rewrite ([], definitions, names) (blockInstructions block)
          (blocks', names'') = mapAccumLBlocks names' rest
       in (block {blockInstructions = reverse instructions} : blocks', names'')
    rewrite (before, definitions, names) instruction@(Instruction [v] operation) =
      case operation of
        Convert Trunc I64 (OperandVar wide) I32
          | Just (Binary op I64 a b) <- Map.lookup wide definitions,
            modular op,
            Map.lookup wide uses == Just 1,
            cheapOperand definitions a || cheapOperand definitions b ->
              let (leftCode, left, names') = narrowOperand definitions names v "_left" a
                  (rightCode, right, names'') = narrowOperand definitions names' v "_right" b
               in append (leftCode <> rightCode <> [Instruction [v] (Binary op I32 left right)]) names''
        Binary op I64 a b
          | op `elem` [And, Or, Xor],
            Just left <- narrowValue definitions a,
            Just right <- narrowValue definitions b ->
              let (result, names') = fresh names v "_word"
               in append [Instruction [result] (Binary op I32 left right), Instruction [v] (Convert ZExt I32 (OperandVar result) I64)] names'
        _ -> append [instruction] names
      where
        append code names' = (reverse code <> before, foldl' define definitions code, names')
        define defs (Instruction [result] op) = Map.insert result op defs
        define defs _ = defs
    rewrite (before, definitions, names) instruction = (instruction : before, definitions, names)
    narrowValue definitions (OperandVar v)
      | Just (Convert ZExt I32 source I64) <- Map.lookup v definitions = Just source
    narrowValue _ operand@(OperandLiteral (LitInt value))
      | value >= 0 && value < 2 ^ (32 :: Int) = Just operand
    narrowValue _ _ = Nothing
    cheapOperand _ (OperandLiteral (LitInt _)) = True
    cheapOperand definitions operand = isJust (narrowValue definitions operand)
    narrowOperand definitions names _ _ operand
      | Just value <- narrowValue definitions operand = ([], value, names)
    narrowOperand _ names _ _ (OperandLiteral (LitInt value)) = ([], OperandLiteral (LitInt (value `mod` 2 ^ (32 :: Int))), names)
    narrowOperand _ names v suffix operand =
      let (result, names') = fresh names v suffix
       in ([Instruction [result] (Convert Trunc I64 operand I32)], OperandVar result, names')
    fresh names v suffix =
      let result = unused (Var (unVar v <> "_width" <> suffix))
          unused candidate
            | Set.member candidate names = unused (Var (unVar candidate <> "_"))
            | otherwise = candidate
       in (result, Set.insert result names)

-- | Use the source of a pointer copy (@ptr.add %source, 0@) in place of the
-- copy when the live interval of the source covers the interval of the
-- copy. The register allocator gives each value one interval, so the
-- substitution then does not make the source live longer, and the copy is
-- one move less.
--
-- A copy that lives longer than its source stays. It lets the source die
-- first, so that a new value can take the register of the source. See
-- @bumpAllocate@ in "Aihc.Lir.Lower", which copies the heap pointer for
-- this reason.
removePointerCopies :: Function -> Function
removePointerCopies function
  | Map.null candidates = function
  | Map.null copies = function
  | otherwise =
      function
        { functionBlocks =
            [ block
                { blockInstructions =
                    [ instruction {instructionOperation = mapOperation rename (instructionOperation instruction)}
                    | instruction@(Instruction results _) <- blockInstructions block,
                      not (any (`Map.member` copies) results)
                    ],
                  blockTerminator = mapTerminator rename id Return (blockTerminator block)
                }
            | block <- functionBlocks function
            ]
        }
  where
    candidates =
      Map.fromList
        [ (copy, source)
        | block <- functionBlocks function,
          Instruction [copy] (PtrAdd (OperandVar source) (OperandLiteral (LitInt 0))) <- blockInstructions block
        ]
    intervals = Map.fromList [(intervalVar interval, intervalEnd interval) | interval <- functionIntervals function]
    copies = Map.filterWithKey covered candidates
    covered copy source = Map.lookup copy intervals <= Map.lookup source intervals
    rename operand =
      case operand of
        OperandVar var -> OperandVar (original var)
        OperandLiteral _ -> operand
    -- The source of a copy can be a copy that goes too.
    original var = maybe var original (Map.lookup var copies)
