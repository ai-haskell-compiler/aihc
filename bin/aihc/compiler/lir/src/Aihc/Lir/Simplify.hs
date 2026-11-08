-- | Simplify Boolean switches, rotate expressions, arithmetic widths, and
-- repeated computations before register allocation.
module Aihc.Lir.Simplify
  ( simplifyBooleanBranches,
    eliminateDeadInstructions,
    narrowExtendedCompares,
    duplicateInvariantLoads,
    splitConditionBranches,
    threadConstantBranches,
    simplifyRotations,
    simplifyWidths,
    simplifyMultiplications,
    eliminateCommonSubexpressions,
    eliminateRedundantBlockParameters,
  )
where

import Aihc.Lir.RegAlloc (operationReads, readCounts)
import Aihc.Lir.Syntax
import Control.Applicative ((<|>))
import Data.Bits (popCount)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, listToMaybe)
import Data.Monoid (Sum)
import Data.MonoidMap qualified as MonoidMap
import Data.Set qualified as Set
import Data.Text qualified as T

-- | Drop each instruction whose results nothing reads and whose operation
-- has no effect. A drop can make another instruction dead, so the pass
-- repeats until nothing changes. A load is pure here: the lowering reads a
-- field only from an object it knows. A division and a conversion stay,
-- because they can trap.
eliminateDeadInstructions :: Function -> Function
eliminateDeadInstructions function
  | null dead = function
  | otherwise = eliminateDeadInstructions function {functionBlocks = map dropDead (functionBlocks function)}
  where
    uses = readCounts function
    unread instruction = all (`MonoidMap.nullKey` uses) (instructionResults instruction)
    removable instruction = unread instruction && pure' (instructionOperation instruction)
    dead = [() | block <- functionBlocks function, instruction <- blockInstructions block, removable instruction]
    dropDead block = block {blockInstructions = filter (not . removable) (blockInstructions block)}
    pure' operation = case operation of
      Binary op _ _ _ -> op `notElem` [DivS, DivU, RemS, RemU]
      Unary {} -> True
      Wide {} -> True
      Compare {} -> True
      FloatBinary {} -> True
      FloatUnary {} -> True
      Convert op _ _ _ -> op `elem` [SExt, ZExt, Trunc, Bitcast, FpExt, FpTrunc]
      PtrToInt {} -> True
      PtrFromInt {} -> True
      Select {} -> True
      Load {} -> True
      LoadInvariant {} -> True
      PtrAdd {} -> True
      GlobalGet {} -> True
      _ -> False

-- | Compare a zero-extended value with a constant at the narrow width when
-- the constant fits it. A signed compare of such a value with a constant
-- that is not negative is the unsigned compare. The extension can then die,
-- and the backend can compare the narrow value where it was loaded.
narrowExtendedCompares :: Function -> Function
narrowExtendedCompares function
  | Map.null extensions = function
  | otherwise = function {functionBlocks = map narrowBlock (functionBlocks function)}
  where
    extensions =
      Map.fromList
        [ (wide, (narrowTy, narrow))
        | block <- functionBlocks function,
          Instruction [wide] (Convert ZExt narrowTy narrow _) <- blockInstructions block,
          narrowTy `elem` [I8, I16, I32]
        ]
    narrowBlock block = block {blockInstructions = map narrowInstruction (blockInstructions block)}
    narrowInstruction instruction =
      case instructionOperation instruction of
        Compare op _ (OperandVar wide) (OperandLiteral (LitInt constant))
          | Just (narrowTy, narrow) <- Map.lookup wide extensions,
            Just op' <- narrowed op narrowTy constant ->
              instruction {instructionOperation = Compare op' narrowTy narrow (OperandLiteral (LitInt constant))}
        Compare op _ (OperandLiteral (LitInt constant)) (OperandVar wide)
          | Just (narrowTy, narrow) <- Map.lookup wide extensions,
            Just op' <- narrowed op narrowTy constant ->
              instruction {instructionOperation = Compare op' narrowTy (OperandLiteral (LitInt constant)) narrow}
        _ -> instruction
    narrowed op narrowTy constant
      | constant < 0 || constant >= 2 ^ typeBits narrowTy = Nothing
      | otherwise = case op of
          LtS -> Just LtU
          LeS -> Just LeU
          GtS -> Just GtU
          GeS -> Just GeU
          _ | op `elem` [Eq, Ne, LtU, LeU, GtU, GeU] -> Just op
          _ -> Nothing

-- | Give each compare its own copy of an invariant load when every reader of
-- the load is a compare with a constant. Invariant memory does not change,
-- so the copy reads the same value, and the base of the address dominates
-- each compare because the load does. A backend can then compare each copy
-- in memory. The original load dies.
duplicateInvariantLoads :: Function -> Function
duplicateInvariantLoads function
  | Map.null candidates = function
  | otherwise = function {functionBlocks = snd (List.mapAccumL copyBlock (0 :: Int) (functionBlocks function))}
  where
    uses = readCounts function
    definitions =
      Map.fromList
        [ (var, operation)
        | block <- functionBlocks function,
          Instruction [var] operation@(LoadInvariant ty _ _) <- blockInstructions block,
          ty `elem` [I8, I16, I32, I64, Ptr, Code]
        ]
    compareReads =
      MonoidMap.fromList
        [ (var, 1 :: Sum Int)
        | block <- functionBlocks function,
          Instruction _ (Compare _ _ left right) <- blockInstructions block,
          (OperandVar var, OperandLiteral _) <- [(left, right), (right, left)]
        ]
    candidates =
      Map.filterWithKey (\var _ -> MonoidMap.get var uses > 1 && MonoidMap.get var compareReads == MonoidMap.get var uses) definitions
    names = Set.fromList (map (unVar . fst) (functionParameters function) <> [unVar name | block <- functionBlocks function, name <- map fst (blockParameters block) <> concatMap instructionResults (blockInstructions block)])
    fresh count (Var base) =
      let candidate n = base <> "$load" <> tshow n
          search n
            | Set.member (candidate n) names = search (n + 1)
            | otherwise = (n + 1, Var (candidate n))
       in search count
    copyBlock count block =
      let (count', instructions) = List.mapAccumL copyInstruction count (blockInstructions block)
       in (count', block {blockInstructions = concat instructions})
    copyInstruction count instruction =
      case instructionOperation instruction of
        Compare op ty left right
          | Just (var, load, side) <- loaded left right ->
              let (count', copy) = fresh count var
                  operation = if side then Compare op ty (OperandVar copy) right else Compare op ty left (OperandVar copy)
               in (count', [Instruction [copy] load, instruction {instructionOperation = operation}])
        _ -> (count, [instruction])
    loaded left right =
      case (left, right) of
        (OperandVar var, OperandLiteral _) | Just load <- Map.lookup var candidates -> Just (var, load, True)
        (OperandLiteral _, OperandVar var) | Just load <- Map.lookup var candidates -> Just (var, load, False)
        _ -> Nothing
    tshow = T.pack . show

-- | Split a branch on the conjunction or the disjunction of two Booleans
-- into two branches. A target computes each Boolean with a compare and a
-- set instruction, and the branch then tests their combination. Two
-- branches test the two compares directly. The second Boolean goes to a new
-- block: its compare moves there when only the conjunction reads it, and
-- so does a load that only that compare reads, when nothing after the load
-- in the block writes memory. The compare of the first Boolean moves to the
-- end of the block. Thus each compare is next to its branch.
splitConditionBranches :: Function -> Function
splitConditionBranches function
  | not (any splittable (functionBlocks function)) = function
  | otherwise = function {functionBlocks = concatMap (split (0 :: Int)) (functionBlocks function)}
  where
    uses = readCounts function
    useOnce var = MonoidMap.get var uses == 1
    labels = Set.fromList (map blockLabel (functionBlocks function))
    splittable block = case combination block of
      Just _ -> True
      Nothing -> False
    combination block =
      case (reverse (blockInstructions block), blockTerminator block) of
        (Instruction [joined] (Binary op I1 (OperandVar first) (OperandVar second)) : before, Branch (OperandVar condition) whenTrue whenFalse)
          | joined == condition,
            op `elem` [And, Or],
            useOnce joined,
            first /= second ->
              Just (op, first, second, before, whenTrue, whenFalse)
        _ -> Nothing
    split count block =
      case combination block of
        Nothing -> [block]
        Just (op, first, second, before, whenTrue, whenFalse) ->
          let label = freshLabel count (blockLabel block)
              -- The instructions are in reverse order.
              (secondCode, rest) = takeDefinition second (reverse before)
              (firstCode, kept) = takeDefinition first rest
              next = Target label []
              (onTrue, onFalse) = if op == And then (next, whenFalse) else (whenTrue, next)
              head' = block {blockInstructions = kept <> firstCode, blockTerminator = Branch (OperandVar first) onTrue onFalse}
              tail' =
                Block
                  { blockLabel = label,
                    blockParameters = [],
                    blockInstructions = secondCode,
                    blockTerminator = Branch (OperandVar second) whenTrue whenFalse,
                    blockCold = blockCold block
                  }
           in head' : split (count + 1) tail'
    freshLabel count (Label base) =
      let candidate n = Label (base <> "$next" <> tshow n)
          search n
            | Set.member (candidate n) labels = search (n + 1)
            | otherwise = candidate n
       in search count
    -- The compare that defines a Boolean, with the load that only it reads,
    -- out of the instructions in program order. A definition that is not a
    -- compare of one use stays where it is.
    takeDefinition var instructions =
      case break (defines var) instructions of
        (earlier, definition@(Instruction _ (Compare _ _ left right)) : later)
          | useOnce var ->
              let (loads, earlier') = takeLoads [left, right] earlier later
               in (loads <> [definition], earlier' <> later)
        _ -> ([], instructions)
    -- A load moves past each instruction after it, so none of them may
    -- write memory.
    takeLoads operands instructions after =
      case [var | OperandVar var <- operands, useOnce var] of
        [] -> ([], instructions)
        candidates ->
          case break (\instruction -> any (`defines` instruction) candidates) instructions of
            (earlier, load@(Instruction _ operation) : later)
              | isLoad operation,
                not (any writes (later <> after)) ->
                  ([load], earlier <> later)
            _ -> ([], instructions)
    defines var instruction = instructionResults instruction == [var]
    isLoad operation = case operation of
      Load {} -> True
      LoadInvariant {} -> True
      _ -> False
    writes instruction = case instructionOperation instruction of
      Store {} -> True
      StoreAtomic {} -> True
      CompareSwap {} -> True
      AtomicRmw {} -> True
      Fence {} -> True
      GlobalSet {} -> True
      Call {} -> True
      CallIndirect {} -> True
      _ -> False
    tshow = T.pack . show

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
                    if MonoidMap.get wide uses == 1
                      then reverse before
                      else blockInstructions block,
                  blockTerminator = Branch condition whenTrue whenFalse
                }
        _ -> block
    literalCase (SwitchCase value target) = Just (value, target)
    literalCase (SwitchCaseConstant _ _) = Nothing

-- | Send a jump that gives a constant to a block that only branches on that
-- constant straight to the destination of the branch. An inline function
-- that returns a constant on one path gives this shape: its returns jump to
-- the block after the call, which branches on the result. The block keeps
-- its other predecessors.
threadConstantBranches :: Function -> Function
threadConstantBranches function
  | Map.null deciders = function
  | otherwise = function {functionBlocks = map threadBlock (functionBlocks function)}
  where
    deciders =
      Map.fromList
        [ (blockLabel block, (index, map fst (blockParameters block), whenTrue, whenFalse))
        | block <- drop 1 (functionBlocks function),
          null (blockInstructions block),
          Branch (OperandVar condition) whenTrue whenFalse <- [blockTerminator block],
          Just index <- [lookup condition (zip (map fst (blockParameters block)) [0 :: Int ..])]
        ]
    threadBlock block = block {blockTerminator = threadTerminator (blockTerminator block)}
      where
        threadTerminator terminator = case terminator of
          Jump target -> Jump (thread target)
          Branch condition whenTrue whenFalse -> Branch condition (thread whenTrue) (thread whenFalse)
          Switch ty scrutinee cases fallback -> Switch ty scrutinee [switchCase {switchCaseTarget = thread (switchCaseTarget switchCase)} | switchCase <- cases] (fmap thread fallback)
          other -> other
    thread target = case Map.lookup (targetLabel target) deciders of
      Just (index, parameters, whenTrue, whenFalse)
        | length (targetArguments target) == length parameters,
          OperandLiteral (LitInt value) <- targetArguments target !! index ->
            let chosen = if value /= 0 then whenTrue else whenFalse
                substitution = Map.fromList (zip parameters (targetArguments target))
                substitute operand = case operand of
                  OperandVar var -> Map.findWithDefault operand var substitution
                  _ -> operand
             in chosen {targetArguments = map substitute (targetArguments chosen)}
      _ -> target

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
        MonoidMap.nullKey v counts =
          (rest, foldl' (flip (MonoidMap.adjust (subtract 1))) counts (operationReads operation))
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
--
-- The definition table holds only the instructions that a rewrite reads: a
-- modular wide operation and a zero extension from 32 bits. When no block
-- changes, the reads of the rewritten function are the reads of the input.
simplifyWidths :: Function -> Function
simplifyWidths function = rewritten {functionBlocks = map clean (functionBlocks rewritten)}
  where
    uses = readCounts function
    occupied = Set.fromList (map fst (functionParameters function) <> concat [map fst (blockParameters block) <> concatMap instructionResults (blockInstructions block) | block <- functionBlocks function])
    (blocks, changed) = mapAccumLBlocks occupied (functionBlocks function)
    rewritten = function {functionBlocks = blocks}
    candidates = Set.fromList [v | block <- functionBlocks function, Instruction [v] op <- blockInstructions block, removable op]
    remaining = if changed then readCounts rewritten else uses
    clean block = block {blockInstructions = fst (foldr remove ([], remaining) (blockInstructions block))}
    remove (Instruction [v] operation) (rest, counts)
      | Set.member v candidates,
        MonoidMap.nullKey v counts =
          (rest, foldl' (flip (MonoidMap.adjust (subtract 1))) counts (operationReads operation))
    remove instruction (rest, counts) = (instruction : rest, counts)
    modular op = op `elem` [Add, Sub, Mul, And, Or, Xor]
    removable (Binary op I64 _ _) = modular op
    removable (Convert op _ _ _) = op `elem` [Trunc, ZExt]
    removable _ = False
    mapAccumLBlocks _ [] = ([], False)
    mapAccumLBlocks names (block : rest) =
      let definitions = foldl' define Map.empty (blockInstructions block)
          (instructions, _, names', blockChanged) = foldl' rewrite ([], definitions, names, False) (blockInstructions block)
          (blocks', restChanged) = mapAccumLBlocks names' rest
       in (block {blockInstructions = reverse instructions} : blocks', blockChanged || restChanged)
    define defs (Instruction [result] op)
      | tabled op = Map.insert result op defs
    define defs _ = defs
    tabled op =
      case op of
        Binary binary I64 _ _ -> modular binary
        Convert ZExt I32 _ I64 -> True
        _ -> False
    rewrite (before, definitions, names, seen) instruction@(Instruction [v] operation) =
      case operation of
        Convert Trunc I64 (OperandVar wide) I32
          | Just (Binary op I64 a b) <- Map.lookup wide definitions,
            modular op,
            MonoidMap.get wide uses == 1,
            cheapOperand definitions a || cheapOperand definitions b ->
              let (leftCode, left, names') = narrowOperand definitions names v "_left" a
                  (rightCode, right, names'') = narrowOperand definitions names' v "_right" b
               in append (leftCode <> rightCode <> [Instruction [v] (Binary op I32 left right)]) names'' True
        Binary op I64 a b
          | op `elem` [And, Or, Xor],
            Just left <- narrowValue definitions a,
            Just right <- narrowValue definitions b ->
              let (result, names') = fresh names v "_word"
               in append [Instruction [result] (Binary op I32 left right), Instruction [v] (Convert ZExt I32 (OperandVar result) I64)] names' True
        _ -> append [instruction] names seen
      where
        append code names' seen' = (reverse code <> before, foldl' define definitions code, names', seen')
    rewrite (before, definitions, names, seen) instruction = (instruction : before, definitions, names, seen)
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

-- | Replace a multiplication by a power of two with a left shift. Both give
-- the same result modulo the width of the type.
simplifyMultiplications :: Function -> Function
simplifyMultiplications function =
  function {functionBlocks = map simplifyBlock (functionBlocks function)}
  where
    simplifyBlock block = block {blockInstructions = map simplify (blockInstructions block)}
    simplify instruction@(Instruction results operation) =
      case operation of
        Binary Mul ty value (OperandLiteral (LitInt factor))
          | Just count <- power ty factor -> Instruction results (Binary Shl ty value (OperandLiteral (LitInt count)))
        Binary Mul ty (OperandLiteral (LitInt factor)) value
          | Just count <- power ty factor -> Instruction results (Binary Shl ty value (OperandLiteral (LitInt count)))
        _ -> instruction
    power ty factor
      | factor > 1,
        factor < 2 ^ typeBits ty,
        popCount factor == 1 =
          Just (toInteger (length (takeWhile (< factor) (iterate (* 2) 1))))
      | otherwise = Nothing

-- | Use the value of an earlier instruction of the same block in place of a
-- later instruction that computes the same value.
--
-- A pure operation gives the same value for the same operands. A load gives
-- the value of an earlier load of the same location when no instruction
-- between the two can write that location. A store can write only its own
-- region, and a store with no region can write any region. A call, a fence,
-- and an atomic access can write any location, or order the later loads
-- after an access of another thread.
--
-- The earlier instruction is in the same block as the later one and comes
-- before it, thus it dominates each use of the later value. Thus the
-- substitution can cover the whole function.
eliminateCommonSubexpressions :: Function -> Function
eliminateCommonSubexpressions function
  | Map.null replacements = function
  | otherwise = function {functionBlocks = map substitute blocks}
  where
    (blocks, replacements) = foldr visit ([], Map.empty) (functionBlocks function)
    visit block (done, replaced) =
      let (instructions, replaced') = eliminate replaced block
       in (block {blockInstructions = instructions} : done, replaced')
    operand replaced value =
      case value of
        OperandVar v -> Map.findWithDefault value v replaced
        OperandLiteral _ -> value
    substitute block =
      block
        { blockInstructions = [instruction {instructionOperation = mapOperation (operand replacements) (instructionOperation instruction)} | instruction <- blockInstructions block],
          blockTerminator = mapTerminator (operand replacements) id Return (blockTerminator block)
        }
    eliminate replaced0 block = go replaced0 Map.empty Map.empty [] (blockInstructions block)
      where
        go replaced _ _ kept [] = (reverse kept, replaced)
        go replaced pure' loads kept (Instruction results operation0 : rest) =
          let operation = mapOperation (operand replaced) operation0
              instruction = Instruction results operation
              reuse earlier =
                go (foldl' (\table (v, u) -> Map.insert v (OperandVar u) table) replaced (zip results earlier)) pure' loads kept rest
           in case operation of
                _
                  | isPure operation,
                    not (null results) ->
                      case Map.lookup operation pure' of
                        Just earlier -> reuse earlier
                        Nothing -> go replaced (Map.insert operation results pure') loads (instruction : kept) rest
                Load _ address _
                  | not (null results) ->
                      case lookup operation loads' of
                        Just earlier -> reuse earlier
                        Nothing -> go replaced pure' (Map.insert (addressRegion address) ((operation, results) : Map.findWithDefault [] (addressRegion address) loads) loads) (instruction : kept) rest
                  where
                    loads' = concat (Map.elems loads)
                Store _ _ address _
                  | addressRegion address /= AnyRegion ->
                      go replaced pure' (Map.delete AnyRegion (Map.delete (addressRegion address) loads)) (instruction : kept) rest
                _
                  | writesMemory operation -> go replaced pure' Map.empty (instruction : kept) rest
                  | otherwise -> go replaced pure' loads (instruction : kept) rest
    isPure operation =
      case operation of
        Binary {} -> True
        Unary {} -> True
        Wide {} -> True
        QuotRemWide {} -> True
        Compare {} -> True
        FloatBinary {} -> True
        FloatUnary {} -> True
        Convert {} -> True
        PtrToInt {} -> True
        PtrFromInt {} -> True
        Select {} -> True
        PtrAdd {} -> True
        LoadInvariant {} -> True
        _ -> False
    writesMemory operation =
      case operation of
        Store {} -> True
        LoadAtomic {} -> True
        StoreAtomic {} -> True
        LoadDependent {} -> True
        CompareSwap {} -> True
        AtomicRmw {} -> True
        Fence {} -> True
        GlobalSet {} -> True
        Call {} -> True
        CallIndirect {} -> True
        _ -> False

-- | Remove each block parameter that receives the same value on every edge
-- into its block, and use that value in place of the parameter.
--
-- A parameter can also receive itself, or another parameter that the pass
-- removes for the same reason. This is the shape of a loop that
-- 'Aihc.Lir.Inline.mergeTailCallees' makes: the merged blocks jump to each
-- other and pass the values that do not change. Each such value then needs
-- only one home in the register allocator, and a jump of the loop moves only
-- the values that change.
--
-- The value is a variable. Each edge into the block gives it, thus the
-- definition of the value is on each path to the block, and it dominates
-- each use of the parameter. A literal stays a parameter, because some
-- operands take only a variable.
--
-- Only a block in a cycle of the control flow loses parameters. A value that
-- a loop passes on lives across the loop anyway. A block after the loop
-- keeps its parameters: the interval of a register allocator that keeps no
-- holes would otherwise reach from the loop to the block, across the code
-- between them.
eliminateRedundantBlockParameters :: Function -> Function
eliminateRedundantBlockParameters function
  | Map.null replacements = function
  | otherwise = function {functionBlocks = map rewrite (functionBlocks function)}
  where
    blocks = functionBlocks function
    parameterLists = Map.fromList [(blockLabel block, map fst (blockParameters block)) | block <- blocks]
    cyclic =
      Set.fromList
        [ label
        | CyclicSCC labels <- stronglyConnComp [(block, blockLabel block, map targetLabel (terminatorTargets (blockTerminator block))) | block <- blocks],
          label <- map blockLabel labels,
          -- The entry block also starts the function, without a jump.
          Just label /= fmap blockLabel (listToMaybe blocks)
        ]
    candidates = Set.fromList [parameter | (label, names) <- Map.toList parameterLists, Set.member label cyclic, parameter <- names]
    -- The values that each parameter receives. A literal is 'Nothing'.
    incoming =
      Map.fromListWith
        (<>)
        [ (parameter, [variable argument])
        | block <- blocks,
          target <- terminatorTargets (blockTerminator block),
          Just names <- [Map.lookup (targetLabel target) parameterLists],
          length names == length (targetArguments target),
          (parameter, argument) <- zip names (targetArguments target)
        ]
    variable argument = case argument of
      OperandVar var -> Just var
      OperandLiteral _ -> Nothing
    parameters = concat (Map.elems parameterLists)
    replacements = settle Map.empty
    settle current =
      let next = List.foldl' remove current parameters
       in if Map.size next == Map.size current then current else settle next
    remove current parameter
      | Map.member parameter current || not (Set.member parameter candidates) = current
      | otherwise =
          case Set.toList (Set.delete (Just parameter) (Set.fromList (map (fmap (resolve current)) (Map.findWithDefault [] parameter incoming)))) of
            [Just value] -> Map.insert parameter value current
            _ -> current
    resolve current var = maybe var (resolve current) (Map.lookup var current)
    operand value = case value of
      OperandVar var -> OperandVar (resolve replacements var)
      OperandLiteral _ -> value
    kept var = not (Map.member var replacements)
    keepArguments target = case Map.lookup (targetLabel target) parameterLists of
      Just names
        | length names == length (targetArguments target) ->
            target {targetArguments = [argument | (name, argument) <- zip names (targetArguments target), kept name]}
      _ -> target
    rewrite block =
      block
        { blockParameters = filter (kept . fst) (blockParameters block),
          blockInstructions = [instruction {instructionOperation = mapOperation operand (instructionOperation instruction)} | instruction <- blockInstructions block],
          blockTerminator = mapTargets keepArguments (mapTerminator operand id Return (blockTerminator block))
        }
    mapTargets change terminator = case terminator of
      Jump target -> Jump (change target)
      Branch condition whenTrue whenFalse -> Branch condition (change whenTrue) (change whenFalse)
      Switch ty scrutinee cases fallback -> Switch ty scrutinee [switchCase {switchCaseTarget = change (switchCaseTarget switchCase)} | switchCase <- cases] (fmap change fallback)
      other -> other
