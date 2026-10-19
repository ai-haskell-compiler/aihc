{-# LANGUAGE OverloadedStrings #-}

-- | Compile Lir modules to WebAssembly in the assembly syntax of LLVM MC.
--
-- The narrow integer types, @ptr@, and @code@ are @i32@; a narrow integer is
-- canonical, that is zero-extended.
--
-- A Lir value stays on the operand stack when an instruction of its block
-- reads it from the top of the stack. Otherwise the value goes into a
-- WebAssembly local. The values that are not live at the same time share a
-- local, and the most used locals get the lowest indices, so that most
-- local instructions encode the index in one byte.
-- A @code@ value is an index into the function table, so a data field or a
-- literal that names a function is a table-index relocation.
--
-- WebAssembly has structured control flow. A reducible function becomes
-- nested @block@, @loop@, and @if@ constructs, as "Aihc.Wasm.ControlFlow"
-- describes, and a jump assigns the parameters of the target and then
-- continues at the target or branches to the construct that ends at it. An
-- irreducible function is one loop with one nested block per Lir block and
-- a @br_table@ on the current block index.
--
-- Both calling conventions are the WebAssembly convention: the @c@
-- convention of wasm32 maps every Lir type to the same value type. Tail
-- calls are @return_call@.
--
-- @stack.alloc@ reserves memory on the shadow stack below
-- @__stack_pointer@. When each block that uses the memory writes it before
-- it reads it, only those blocks make a frame, and the other paths do not
-- touch @__stack_pointer@. Otherwise the frame lasts for the whole function. A trap calls @aihc_lir_trap@ with the message and its
-- length and then executes @unreachable@; the host runtime provides that
-- function.
module Aihc.Wasm.Lir
  ( WasmLirError (..),
    compileLirModule,
  )
where

import Aihc.Lir.Convert (integerConversionBounds)
import Aihc.Lir.Division (lowerWideDivision)
import Aihc.Lir.Inline (prepareCheckedModule)
import Aihc.Lir.Lint (LintError)
import Aihc.Lir.Resolve (resolvedSwitchCaseValue, unresolvedConstant)
import Aihc.Lir.Syntax
import Aihc.Wasm.ControlFlow (ControlFlow (..), analyzeControlFlow)
import Control.Monad (forM_, unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify', put, runStateT)
import Data.ByteString qualified as BS
import Data.Either (fromRight)
import Data.Functor.Const (Const (..))
import Data.List (elemIndex, find, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Word (Word8)
import GHC.Float (castDoubleToWord64, castFloatToWord32, double2Float)

data WasmLirError
  = WasmLirLintErrors ![LintError]
  | WasmLirUnsupported !Text
  deriving (Eq, Show)

-- | Lint the module as written and as this backend receives it, then render
-- it. The helpers for wide 64-bit
-- multiplication are Lir functions that the backend adds to the module when
-- it uses them.
compileLirModule :: Module -> Either WasmLirError Text
compileLirModule lirModule =
  case prepared of
    Right _ -> do
      let items = userItems <> [ItemFunction helper | usesWideMultiply, helper <- wideHelpers] <> [ItemFunction helper | usesBitScatter, helper <- bitScatterHelpers]
          ctx = moduleContext items
      (functions, final) <- runStateT (mapM (compileFunction ctx) [function | ItemFunction function <- items]) initialState
      let traps = Map.toAscList (moduleTraps final)
      pure
        ( T.unlines
            ( header ctx items (moduleUsesStack final)
                <> concat functions
                <> concatMap (renderData ctx) [dataItem | ItemData dataItem <- items]
                <> concatMap (renderGlobal ctx) [global | ItemGlobal global <- items]
                <> concat
                  [ renderBytes (trapMessageSymbol index) (Text.encodeUtf8 message)
                  | (message, index) <- traps
                  ]
                <> ["\t.no_dead_strip\t__indirect_function_table", ""]
            )
        )
    Left errors -> Left (WasmLirLintErrors errors)
  where
    prepared = prepareCheckedModule wordBytes lirModule
    Module userItems = fromRight (Module []) prepared
    usesWideMultiply =
      or
        [ True
        | ItemFunction function <- userItems,
          block <- functionBlocks function,
          Instruction _ (Wide op I64 _ _) <- blockInstructions block,
          op `elem` [MulWideU, MulWideS]
        ]
    usesBitScatter =
      or
        [ True
        | ItemFunction function <- userItems,
          block <- functionBlocks function,
          Instruction _ (Binary op _ _ _) <- blockInstructions block,
          op `elem` [Pdep, Pext]
        ]
    initialState = ModuleState {moduleTraps = Map.empty, moduleUsesStack = False}

-- | The module-level facts the functions need.
data Ctx = Ctx
  { ctxSymbols :: !(Map Symbol Text),
    ctxSignatures :: !(Map Symbol Signature),
    ctxGlobals :: !(Map Symbol Type)
  }

moduleContext :: [Item] -> Ctx
moduleContext items =
  Ctx
    { ctxSymbols = Map.fromList (mapMaybe symbolName items),
      ctxSignatures =
        Map.fromList
          ( [(functionName function, functionSignature function) | ItemFunction function <- items]
              <> [(externFunctionName external, externFunctionSignature external) | ItemExternFunction external <- items]
          ),
      ctxGlobals = Map.fromList [(globalName global, globalType global) | ItemGlobal global <- items]
    }
  where
    symbolName item =
      case item of
        ItemFunction function -> Just (functionName function, linkedName (functionLinkage function) (functionName function))
        ItemExternFunction external -> Just (externFunctionName external, unSymbol (externFunctionName external))
        ItemGlobal global -> Just (globalName global, linkedName Internal (globalName global))
        ItemData dataItem -> Just (dataName dataItem, linkedName (dataLinkage dataItem) (dataName dataItem))
        ItemExternData symbol -> Just (symbol, unSymbol symbol)
        ItemConstant _ -> Nothing
        ItemInclude _ -> Nothing

-- | An internal symbol is local to its object. An inline function is
-- spliced and dropped before a backend runs, so none reaches here.
linkedName :: Linkage -> Symbol -> Text
linkedName linkage symbol =
  case linkage of
    Export -> unSymbol symbol
    Internal -> ".L" <> unSymbol symbol
    Inline -> error ("Lir inline function " <> T.unpack (unSymbol symbol) <> " reached the wasm backend")

symbolText :: Ctx -> Symbol -> Text
symbolText ctx symbol = fromMaybe (unSymbol symbol) (Map.lookup symbol (ctxSymbols ctx))

data ModuleState = ModuleState
  { moduleTraps :: !(Map Text Int),
    moduleUsesStack :: !Bool
  }

trapMessageSymbol :: Int -> Text
trapMessageSymbol index = ".Llir_trap_" <> tshow index

trapSymbol :: Text
trapSymbol = "aihc_lir_trap"

stackPointer :: Text
stackPointer = "__stack_pointer"

-- Types

wasmType :: Type -> Text
wasmType ty =
  case ty of
    I64 -> "i64"
    F32 -> "f32"
    F64 -> "f64"
    _ -> "i32"

-- | Whether a value type is @i32@ or @i64@.
is64 :: Type -> Bool
is64 ty = ty == I64

-- | The instruction prefix of the integer type.
prefix :: Type -> Text
prefix ty = if is64 ty then "i64" else "i32"

renderSignature :: Signature -> Text
renderSignature signature =
  "(" <> T.intercalate ", " (map wasmType (signatureParameters signature)) <> ") -> (" <> T.intercalate ", " (map wasmType (signatureResults signature)) <> ")"

-- Module header

header :: Ctx -> [Item] -> Bool -> [Text]
header ctx items usesStack =
  [ "# Lir module compiled by Aihc.Wasm.Lir.",
    "\t.text",
    "\t.functype\t" <> trapSymbol <> " (i32, i64) -> ()"
  ]
    <> ["\t.functype\t" <> symbolText ctx (externFunctionName external) <> " " <> renderSignature (externFunctionSignature external) | ItemExternFunction external <- items]
    <> ["\t.functype\t" <> symbolText ctx (functionName function) <> " " <> renderSignature (functionSignature function) | ItemFunction function <- items]
    <> ["\t.globaltype\t" <> stackPointer <> ", i32" | usesStack]
    <> ["\t.globaltype\t" <> symbolText ctx (globalName global) <> ", " <> wasmType (globalType global) | ItemGlobal global <- items]
    <> ["\t.tabletype\t__indirect_function_table, funcref", ""]

-- Data

renderData :: Ctx -> DataItem -> [Text]
renderData ctx dataItem =
  [ "\t.type\t" <> name <> ",@object",
    "\t.section\t" <> (if dataMutable dataItem then ".data." else ".rodata.") <> name <> ",\"\",@"
  ]
    <> ["\t.hidden\t" <> name | dataLinkage dataItem == Export]
    <> ["\t.globl\t" <> name | dataLinkage dataItem == Export]
    <> ["\t.p2align\t" <> tshow (log2 (dataAlignment dataItem)) <> ", 0x0" | dataAlignment dataItem > 1]
    <> [name <> ":"]
    <> concatMap (fst . field) (dataFields dataItem)
    <> ["\t.size\t" <> name <> ", " <> tshow (sum (map (snd . field) (dataFields dataItem))), ""]
  where
    name = symbolText ctx (dataName dataItem)
    field :: DataField -> ([Text], Int)
    field dataField =
      case dataField of
        DataIntConstant _ constant -> unresolvedConstant constant
        DataInt ty value -> (["\t.int" <> tshow (8 * typeBytes ty) <> "\t" <> renderInteger (typeBytes ty) value], typeBytes ty)
        DataFloat F32 value -> (["\t.int32\t" <> tshow (castFloatToWord32 (double2Float value))], 4)
        DataFloat _ value -> (["\t.int64\t" <> tshow (castDoubleToWord64 value)], 8)
        DataSymbol target 0 -> (["\t.int32\t" <> symbolText ctx target], 4)
        DataSymbol target addend -> (["\t.int32\t" <> symbolText ctx target <> (if addend < 0 then "-" <> tshow (negate addend) else "+" <> tshow addend)], 4)
        DataNull -> (["\t.int32\t0"], 4)
        DataWordConstant constant -> unresolvedConstant constant
        DataWord value -> (["\t.int32\t" <> renderInteger 4 value], 4)
        DataCode Nothing -> (["\t.int32\t0"], 4)
        DataCode (Just target) -> (["\t.int32\t" <> symbolText ctx target], 4)
        DataBytes bytes -> (["\t.ascii\t\"" <> escapeBytes bytes <> "\"" | not (BS.null bytes)], BS.length bytes)
        DataZero count -> (["\t.skip\t" <> tshow count | count > 0], fromInteger count)

-- | A read-only byte object with a local symbol.
renderBytes :: Text -> BS.ByteString -> [Text]
renderBytes name bytes =
  [ "\t.type\t" <> name <> ",@object",
    "\t.section\t.rodata." <> name <> ",\"\",@",
    name <> ":",
    "\t.ascii\t\"" <> escapeBytes bytes <> "\"",
    "\t.size\t" <> name <> ", " <> tshow (BS.length bytes),
    ""
  ]

renderGlobal :: Ctx -> Global -> [Text]
renderGlobal ctx global =
  [ "\t.globaltype\t" <> name <> ", " <> wasmType (globalType global),
    name <> ":",
    ""
  ]
  where
    name = symbolText ctx (globalName global)

escapeBytes :: BS.ByteString -> Text
escapeBytes = T.concat . map escapeByte . BS.unpack
  where
    escapeByte :: Word8 -> Text
    escapeByte byte
      | byte >= 0x20 && byte < 0x7f && byte /= 0x22 && byte /= 0x5c = T.singleton (toEnum (fromIntegral byte))
      | otherwise = "\\" <> T.pack (octal byte)
    octal byte = [digit (byte `div` 64), digit ((byte `div` 8) `mod` 8), digit (byte `mod` 8)]
    digit value = toEnum (fromIntegral value + 48)

log2 :: Integer -> Int
log2 value = length (takeWhile (< value) (iterate (* 2) 1))

-- | wasm32 has four-byte words, so a word-scaled address offset counts
-- four bytes.
wordBytes :: Integer
wordBytes = 4

typeBytes :: Type -> Int
typeBytes ty = max 1 (typeBits ty `div` 8)

-- | An integer in the signed range of its width, as the assembler expects.
renderInteger :: Int -> Integer -> Text
renderInteger bytes value =
  let bits = 8 * bytes
      wrapped = value `mod` (2 ^ bits)
   in tshow (if wrapped >= 2 ^ (bits - 1) then wrapped - 2 ^ bits else wrapped)

-- Functions

data FunctionState = FunctionState
  { functionModule :: !ModuleState,
    functionLinesRev :: ![Line],
    -- | The values on the operand stack that no instruction read yet, top
    -- first.
    functionPending :: ![Var],
    -- | The operands of the current instruction that are already on the
    -- operand stack, in the order that the instruction pushes them.
    functionSkip :: ![Var],
    -- | The values that an instruction read from the operand stack.
    functionConsumed :: !(Set Var),
    -- | The next tag of a copy to a block parameter.
    functionCopies :: !Int,
    -- | Whether the operand stack holds exactly the pending values. Only
    -- then can an instruction read its operands from the stack.
    functionClean :: !Bool
  }

-- | A line of the function body. The indices of the locals are known only
-- when the whole body is complete.
data Line
  = LineText !Text
  | LineLocal !LocalOp !Slot
  | -- | The end of the code that computes a value. The value stays on the
    -- operand stack, or a @local.set@, @local.tee@, or @drop@ goes here.
    LineDef !Var
  | -- | A line of a copy from a local to a block parameter: the read of the
    -- argument or the write of the parameter.
    LineCopy !LocalOp !Var !Int

data LocalOp = LocalGet | LocalSet | LocalTee
  deriving (Eq)

-- | A local before it has an index.
data Slot
  = SlotVar !Var
  | -- | The current block index of the dispatch loop.
    SlotState
  | -- | The address of the shadow-stack frame.
    SlotFrame
  | -- | A scratch local of one value type.
    SlotScratch !Text
  deriving (Eq, Ord)

type M = StateT FunctionState (Either WasmLirError)

data Fn = Fn
  { fnCtx :: !Ctx,
    fnFunction :: !Function,
    -- | The type of each parameter and each value of the function.
    fnTypes :: !(Map Var Type),
    -- | The number of reads of each value.
    fnUses :: !(Map Var Int),
    -- | The values that can stay on the operand stack: each one is the only
    -- result of its instruction, and only its own block reads it.
    fnCandidates :: !(Set Var),
    -- | The instructions that the code of their only reader computes, by
    -- result.
    fnDeferred :: !(Map Var Instruction),
    fnBlocks :: !(Map Label Block),
    fnBlockIndex :: !(Map Label Int),
    fnBlockParameters :: !(Map Label [(Var, Type)]),
    -- | The shadow-stack frame when the function allocates stack memory.
    fnFrame :: !(Maybe Frame),
    fnAllocs :: !(Map Var Int)
  }

data Frame = Frame
  { frameSize :: !Int,
    -- | The blocks that make and remove a frame of their own, or 'Nothing'
    -- when one frame lasts for the whole function.
    frameBlocks :: !(Maybe (Set Label))
  }

unsupported :: Text -> M value
unsupported = lift . Left . WasmLirUnsupported

-- | Add a line. When an operand of the current instruction is already on
-- the operand stack, the instruction must push its operands before any
-- other code, or the code is incorrect.
addLine :: Line -> M ()
addLine line = do
  state <- get
  case functionSkip state of
    var : _ -> unsupported ("code before the operand " <> unVar var <> " that is already on the stack")
    [] -> put state {functionLinesRev = line : functionLinesRev state, functionClean = False}

emit :: Text -> M ()
emit line = addLine (LineText ("\t" <> line))

emitLabel :: Text -> M ()
emitLabel line = addLine (LineText line)

emitLocal :: LocalOp -> Slot -> M ()
emitLocal op slot = addLine (LineLocal op slot)

trapIndex :: Text -> M Int
trapIndex message = do
  state <- get
  let traps = moduleTraps (functionModule state)
  case Map.lookup message traps of
    Just index -> pure index
    Nothing -> do
      let index = Map.size traps
      put state {functionModule = (functionModule state) {moduleTraps = Map.insert message index traps}}
      pure index

-- | Report a trap message and stop.
trap :: Text -> M ()
trap message = do
  index <- trapIndex message
  emit ("i32.const\t" <> trapMessageSymbol index)
  emit ("i64.const\t" <> tshow (BS.length (Text.encodeUtf8 message)))
  emit ("call\t" <> trapSymbol)
  emit "unreachable"

-- | Trap when the value on the stack is nonzero.
trapIf :: Text -> M ()
trapIf message = do
  emit "if"
  trap message
  emit "end_if"

compileFunction :: Ctx -> Function -> StateT ModuleState (Either WasmLirError) [Text]
compileFunction ctx input = do
  let function = lowerWideDivision False input
  moduleState <- get
  let parameters = functionParameters function
      blocks = functionBlocks function
      definitionTypes = Map.fromList (concat [blockParameters block | block <- blocks] <> concat [resultTypes ctx instruction | block <- blocks, instruction <- blockInstructions block])
      allocations = [(var, size, alignmentInBytes wordBytes alignment) | block <- take 1 blocks, Instruction [var] (StackAlloc size alignment) <- blockInstructions block]
      (allocs, frameEnd) = placeAllocations allocations
      frameSize = ((frameEnd + 15) `div` 16) * 16
      hasFrame = not (null allocations)
      readers = valueReaders blocks
      uses = Map.map fst readers
      candidates =
        Set.fromList
          [ var
          | block <- blocks,
            Instruction [var] _ <- blockInstructions block,
            all (== blockLabel block) (maybe Set.empty snd (Map.lookup var readers))
          ]
      fn =
        Fn
          { fnCtx = ctx,
            fnFunction = function,
            fnTypes = Map.fromList parameters <> definitionTypes,
            fnUses = uses,
            fnCandidates = candidates,
            fnDeferred = deferredInstructions uses candidates blocks,
            fnBlocks = Map.fromList [(blockLabel block, block) | block <- blocks],
            fnBlockIndex = Map.fromList (zip (map blockLabel blocks) [0 ..]),
            fnBlockParameters = Map.fromList [(blockLabel block, blockParameters block) | block <- blocks],
            fnFrame =
              if hasFrame
                then
                  Just
                    Frame
                      { frameSize = frameSize,
                        frameBlocks = blockFrameLabels (Map.fromList [(var, size) | (var, size, _) <- allocations]) blocks
                      }
                else Nothing,
            fnAllocs = allocs
          }
      name = symbolText ctx (functionName function)
      initial =
        FunctionState
          { functionModule = moduleState,
            functionLinesRev = [],
            functionPending = [],
            functionSkip = [],
            functionConsumed = Set.empty,
            functionCopies = 0,
            functionClean = True
          }
  ((), final) <- lift (runStateT (functionBody fn) initial)
  put (functionModule final) {moduleUsesStack = moduleUsesStack (functionModule final) || hasFrame}
  let (localTypes, body) = assignLocals fn (functionConsumed final) (reverse (functionLinesRev final))
  pure
    ( [ "\t.section\t.text." <> name <> ",\"\",@",
        "\t.type\t" <> name <> ",@function"
      ]
        <> ["\t.hidden\t" <> name | functionLinkage function == Export]
        <> ["\t.globl\t" <> name | functionLinkage function == Export]
        <> [ name <> ":",
             "\t.functype\t" <> name <> " " <> renderSignature (functionSignature function)
           ]
        <> ["\t.local\t" <> T.intercalate ", " localTypes | not (null localTypes)]
        <> body
        <> ["\tend_function", ""]
    )

-- | The number of reads of each value and the blocks that read it. The
-- result of a compare-and-swap or of an atomic operation is also read by
-- the code of its own instruction.
valueReaders :: [Block] -> Map Var (Int, Set Label)
valueReaders blocks =
  Map.fromListWith
    (\(count, labels) (count', labels') -> (count + count', Set.union labels labels'))
    [ (var, (1, Set.singleton (blockLabel block)))
    | block <- blocks,
      var <-
        concat
          [ operationReads results operation
          | Instruction results operation <- blockInstructions block
          ]
          <> terminatorReads (blockTerminator block)
    ]
  where
    operationReads results operation =
      getConst (forOperationOperands (Const . operandVars) operation)
        <> case operation of
          CompareSwap {} -> results <> results
          AtomicRmw {} -> results <> results
          _ -> []

-- | The instructions that the code of their only reader computes, by
-- result. Such an instruction has no effect and cannot trap. Its result is
-- a candidate that only one instruction of the block reads. A load can wait
-- only when no instruction writes memory before the reader runs. The reader
-- runs when its own reader runs, if it also waits.
deferredInstructions :: Map Var Int -> Set Var -> [Block] -> Map Var Instruction
deferredInstructions uses candidates = Map.unions . map blockDeferred
  where
    blockDeferred block =
      let instructions = blockInstructions block
          indexed = zip [0 :: Int ..] instructions
          readers =
            Map.fromList
              ( [(var, index) | (index, Instruction _ operation) <- indexed, var <- getConst (forOperationOperands (Const . operandVars) operation)]
                  <> [(var, length instructions) | var <- terminatorReads (blockTerminator block)]
              )
          writes = Set.fromList [index | (index, Instruction _ operation) <- indexed, writesMemory operation]
          step (index, instruction@(Instruction results operation)) (deferred, roots) =
            case results of
              [var]
                | deferrable operation,
                  Map.findWithDefault 0 var uses == 1,
                  Set.member var candidates,
                  Just reader <- Map.lookup var readers,
                  reader > index,
                  root <- Map.findWithDefault reader reader roots,
                  not (readsMemory operation) || maybe True (>= root) (Set.lookupGT index writes) ->
                    (Map.insert var instruction deferred, Map.insert index root roots)
              _ -> (deferred, roots)
       in fst (foldr step (Map.empty, Map.empty) indexed)
    deferrable operation =
      case operation of
        Binary op _ _ _ -> op `notElem` [DivS, DivU, RemS, RemU, Pdep, Pext]
        Unary {} -> True
        Compare {} -> True
        FloatBinary {} -> True
        FloatUnary {} -> True
        Convert op _ _ _ -> op `notElem` [FToIS, FToIU]
        PtrToInt _ -> True
        PtrFromInt _ -> True
        Select {} -> True
        PtrAdd _ _ -> True
        _ -> readsMemory operation
    readsMemory operation =
      case operation of
        Load {} -> True
        LoadDependent {} -> True
        LoadInvariant {} -> True
        _ -> False
    writesMemory operation =
      case operation of
        Store {} -> True
        StoreAtomic {} -> True
        CompareSwap {} -> True
        AtomicRmw {} -> True
        Fence _ -> True
        Call _ _ -> True
        CallIndirect {} -> True
        _ -> False

terminatorReads :: Terminator -> [Var]
terminatorReads = getConst . forTerminatorOperands (Const . operandVars)

operandVars :: Operand -> [Var]
operandVars operand = case operand of
  OperandVar var -> [var]
  OperandLiteral _ -> []

-- | Give each local an index, and render the body with the indices. The
-- result is also the types of the local declarations after the parameters.
--
-- A value that an instruction read from the operand stack needs a
-- @local.tee@ only when other code reads it from its local. A value that
-- stays in a local needs a @local.set@, or a @drop@ when no code reads it.
assignLocals :: Fn -> Set Var -> [Line] -> ([Text], [Text])
assignLocals fn consumed body = (map snd declared, map render resolved)
  where
    readVars = Set.fromList ([var | LineLocal LocalGet (SlotVar var) <- body] <> [var | LineCopy LocalGet var _ <- body])
    unresolved = concatMap resolve body
    resolve line =
      case line of
        LineDef var
          | Set.member var consumed -> [Right (LocalTee, SlotVar var, Nothing) | Set.member var readVars]
          | Set.member var readVars -> [Right (LocalSet, SlotVar var, Nothing)]
          | otherwise -> [Left "\tdrop"]
        LineLocal op slot -> [Right (op, slot, Nothing)]
        LineCopy op var copy -> [Right (op, SlotVar var, Just copy)]
        LineText text -> [Left text]
    -- A copy is useless when the argument and the parameter share a local.
    copySlots = Map.fromListWith (<>) [(copy, [slotId slot]) | Right (_, slot, Just copy) <- unresolved]
    useless = Map.keysSet (Map.filter (\slots -> case slots of [one, two] -> one == two; _ -> False) copySlots)
    resolved =
      combineTee
        [ either Left (\(op, slot, _) -> Right (op, slot)) item
        | item <- unresolved,
          case item of
            Right (_, _, Just copy) -> not (Set.member copy useless)
            _ -> True
        ]
    combineTee items =
      case items of
        Right (LocalSet, slot) : Right (LocalGet, slot') : rest
          | slot == slot' -> Right (LocalTee, slot) : combineTee rest
        item : rest -> item : combineTee rest
        [] -> []
    parameters = functionParameters (fnFunction fn)
    parameterCount = length parameters
    used = Set.fromList [slot | Right (_, slot, _) <- unresolved]
    values = Set.fromList [var | SlotVar var <- Set.toList used] <> Set.fromList (map fst parameters)
    blocks = functionBlocks (fnFunction fn)
    definitions =
      [ var
      | block <- blocks,
        var <- map fst (blockParameters block) <> concatMap instructionResults (blockInstructions block),
        Set.member var values
      ]
    valueType var = wasmType (Map.findWithDefault I64 var (fnTypes fn))
    (coloring, slotTypes) =
      colorLocals
        (interference fn values)
        (copyPartners blocks)
        [(var, wasmType ty) | (var, ty) <- parameters]
        [(var, valueType var) | var <- definitions]
        (Map.keysSet (fnAllocs fn))
    special = [slot | slot <- Set.toList used, not (isVar slot)]
    isVar slot = case slot of
      SlotVar _ -> True
      _ -> False
    specialIds = Map.fromList (zip special [Map.size slotTypes ..])
    specialType slot = case slot of
      SlotScratch ty -> ty
      _ -> "i32"
    slotId slot = case slot of
      SlotVar var -> Map.findWithDefault 0 var coloring
      _ -> Map.findWithDefault 0 slot specialIds
    types = slotTypes <> Map.fromList [(index, specialType slot) | (slot, index) <- Map.toList specialIds]
    counts = Map.fromListWith (+) [(slotId slot, 1 :: Int) | Right (_, slot) <- resolved]
    -- The most used locals take the indices that encode in one byte. Each
    -- of the two groups is sorted by type, so that the declarations are a
    -- few runs of one type.
    others = sortOn (negate . flip (Map.findWithDefault 0) counts) [index | index <- Map.keys types, index >= parameterCount, Map.member index counts]
    (hot, cold) = splitAt (max 0 (128 - parameterCount)) others
    byType = sortOn (typeOrder . (types Map.!))
    declared = [(index, types Map.! index) | index <- byType hot <> byType cold]
    indices = Map.fromList (zip [0 .. parameterCount - 1] [0 ..] <> zip (map fst declared) [parameterCount ..])
    typeOrder ty = fromMaybe 4 (elemIndex ty ["i32", "i64", "f32", "f64"])
    render item =
      case item of
        Left text -> text
        Right (op, slot) ->
          "\t" <> localOpName op <> "\t" <> tshow (Map.findWithDefault 0 (slotId slot) indices)
    localOpName op = case op of
      LocalGet -> "local.get"
      LocalSet -> "local.set"
      LocalTee -> "local.tee"

-- | The arguments of the jumps to each block parameter, and the reverse.
copyPartners :: [Block] -> Map Var [Var]
copyPartners blocks =
  Map.fromListWith
    (<>)
    ( concat
        [ [(parameter, [argument]), (argument, [parameter])]
        | block <- blocks,
          Target label arguments <- terminatorTargets (blockTerminator block),
          Just target <- [Map.lookup label labels],
          (parameter, OperandVar argument) <- zip (map fst (blockParameters target)) arguments
        ]
    )
  where
    labels = Map.fromList [(blockLabel block, block) | block <- blocks]

-- | Give each value a local of its type that no value which interferes with
-- it has. The parameters keep their own locals, and the other values can
-- share them. A value in the unique set gets a local of its own. A value
-- takes the local of a copy partner when it can, so that the copy does
-- nothing. The result maps each value to a local and each local to its
-- type.
colorLocals :: Map Var (Set Var) -> Map Var [Var] -> [(Var, Text)] -> [(Var, Text)] -> Set Var -> (Map Var Int, Map Int Text)
colorLocals adjacency partners parameters values unique =
  let (coloring, slots) = foldl' place (Map.fromList initial, Map.fromList [(index, (ty, Set.singleton var, False)) | (var, index, ty) <- numbered]) values
   in (coloring, Map.map (\(ty, _, _) -> ty) slots)
  where
    numbered = [(var, index, ty) | ((var, ty), index) <- zip parameters [0 ..]]
    initial = [(var, index) | (var, index, _) <- numbered]
    place (coloring, slots) (var, ty)
      | Map.member var coloring = (coloring, slots)
      | otherwise =
          let neighbours = Map.findWithDefault Set.empty var adjacency
              fits (ty', members, closed) = ty' == ty && not closed && Set.disjoint members neighbours
              preferred = [index | partner <- Map.findWithDefault [] var partners, Just index <- [Map.lookup partner coloring], Just slot <- [Map.lookup index slots], fits slot]
              free = [index | not (Set.member var unique), index <- preferred <> [index | (index, slot) <- Map.toList slots, fits slot]]
           in case free of
                index : _ -> (Map.insert var index coloring, Map.adjust (\(ty', members, closed) -> (ty', Set.insert var members, closed)) index slots)
                [] ->
                  let index = Map.size slots
                   in (Map.insert var index coloring, Map.insert index (ty, Set.singleton var, Set.member var unique) slots)

-- | The pairs of values in the set that must not share a local, from the
-- liveness of the values in the Lir function.
--
-- A value interferes with each value that is live after its instruction,
-- and the results of one instruction interfere with each other. A
-- compare-and-swap, an atomic operation, and a wide operation write a
-- result before they read all of their operands, so the result also
-- interferes with the operands. The jump to a block writes its parameters
-- at the end of the jump, so they interfere with each value that is live
-- into the block.
interference :: Fn -> Set Var -> Map Var (Set Var)
interference fn locals = foldl' blockEdges Map.empty blocks
  where
    function = fnFunction fn
    blocks = functionBlocks function
    restrict = Set.filter (`Set.member` locals)
    readsOf = restrict . Set.fromList
    parametersOf label = maybe [] (map fst) (Map.lookup label (fnBlockParameters fn))
    liveIn = liveness Map.empty
    liveInOf label = Map.findWithDefault Set.empty label liveIn
    liveOut block = Set.unions [liveInOf (targetLabel target) | target <- terminatorTargets (blockTerminator block)]
    instructionReads (Instruction _ operation) = readsOf (getConst (forOperationOperands (Const . operandVars) operation))
    -- The values that are live into each block, without its parameters.
    liveness live =
      let live' = foldr (\block acc -> Map.insert (blockLabel block) (blockLiveIn acc block) acc) live blocks
       in if live' == live then live else liveness live'
    blockLiveIn live block =
      let out = Set.unions [Map.findWithDefault Set.empty (targetLabel target) live | target <- terminatorTargets (blockTerminator block)]
          start = foldr transfer (Set.union out (readsOf (terminatorReads (blockTerminator block)))) (blockInstructions block)
       in Set.difference start (Set.fromList (map fst (blockParameters block)))
    transfer instruction@(Instruction results _) after =
      Set.union (Set.difference after (Set.fromList results)) (instructionReads instruction)
    blockEdges edges block =
      let targetEdges =
            foldl'
              (\acc target -> let written = parametersOf (targetLabel target) in addEdges written (Set.union (readsOf written) (liveInOf (targetLabel target))) acc)
              edges
              (terminatorTargets (blockTerminator block))
          end = Set.union (liveOut block) (readsOf (terminatorReads (blockTerminator block)))
          steps = [instructionStep instruction | instruction@(Instruction results _) <- blockInstructions block, not (any (`Map.member` fnDeferred fn) results)] <> [groupStep [] Set.empty False (terminatorReads (blockTerminator block))]
          (start, instructionEdges) = foldr edgeStep (end, targetEdges) steps
          parameters = map fst (blockParameters block)
       in addEdges parameters (Set.union start (readsOf parameters)) instructionEdges
    -- An instruction and the deferred instructions that its code computes,
    -- as one step: the results, the reads, and whether a result is written
    -- before all reads.
    instructionStep instruction@(Instruction results operation) =
      let early = case operation of
            Wide {} -> True
            CompareSwap {} -> True
            AtomicRmw {} -> True
            _ -> False
       in groupStep results (instructionReads instruction) early (getConst (forOperationOperands (Const . operandVars) operation))
    groupStep results direct early readVars =
      let group = deferredGroup readVars
          groupResults = concat [rs | Instruction rs _ <- group]
          groupReads = Set.unions (map instructionReads group)
       in (results <> groupResults, Set.difference (Set.union direct groupReads) (Set.fromList groupResults), early || not (null group))
    deferredGroup readVars =
      [ instruction'
      | var <- readVars,
        Just instruction@(Instruction _ operation) <- [Map.lookup var (fnDeferred fn)],
        instruction' <- instruction : deferredGroup (getConst (forOperationOperands (Const . operandVars) operation))
      ]
    edgeStep (results, inputs, early) (after, acc) =
      let written = addEdges results (Set.union after (readsOf results)) acc
          final = if early then addEdges results inputs written else written
       in (Set.union (Set.difference after (Set.fromList results)) inputs, final)
    addEdges vars others acc = foldl' (addEdge others) acc vars
    addEdge others acc var
      | Set.member var locals =
          let others' = Set.delete var others
           in Set.foldl' (\acc' other -> Map.insertWith Set.union other (Set.singleton var) acc') (Map.insertWith Set.union var others' acc) others'
      | otherwise = acc

-- | The blocks that use the stack allocations, when each of them can make
-- a frame of its own.
--
-- A block frame is at the same address as a frame for the whole function.
-- Between two block frames the memory is below the stack pointer, and only
-- a call writes there. Thus one of these two conditions must be true:
--
-- * Each block that makes a call uses the memory, so it has a frame. Then
--   no code writes the memory between two block frames.
-- * The memory keeps no value from one block to another. Each use of the
--   memory in a block reads only bytes that the block wrote before. A load
--   reads its bytes. Any other use, such as a call that gets the address,
--   can read all of the memory, and after it every byte counts as written.
--
-- A terminator must not use the memory, because the block removes its
-- frame before it.
blockFrameLabels :: Map Var Integer -> [Block] -> Maybe (Set Label)
blockFrameLabels sizes blocks
  | any (any isAllocation . terminatorVars . blockTerminator) blocks = Nothing
  | all (`Set.member` labels) callers || all (selfContained . blockInstructions) users = Just labels
  | otherwise = Nothing
  where
    isAllocation var = Map.member var sizes
    users = [block | block <- blocks, any (any isAllocation . operationVars) (uses block)]
    labels = Set.fromList (map blockLabel users)
    callers = [blockLabel block | block <- blocks, any isCall (uses block)]
    isCall operation = case operation of
      Call _ _ -> True
      CallIndirect {} -> True
      _ -> False
    uses block = [operation | Instruction _ operation <- blockInstructions block, not (isStackAlloc operation)]
    isStackAlloc operation = case operation of
      StackAlloc _ _ -> True
      _ -> False
    selfContained = go Map.empty
    go _ [] = True
    go written (Instruction _ operation : rest) =
      case operation of
        StackAlloc _ _ -> go written rest
        Load ty address _
          | Just (var, bytes) <- allocationBytes ty address ->
              all (`Set.member` Map.findWithDefault Set.empty var written) bytes && go written rest
        Store ty value address _
          | Just (var, bytes) <- allocationBytes ty address,
            not (any isAllocation (operandVars value)) ->
              go (Map.insertWith Set.union var (Set.fromList bytes) written) rest
        _ ->
          let escaped = filter isAllocation (operationVars operation)
           in all (complete written) escaped && go (foldr fill written escaped) rest
    allocationBytes ty address =
      case addressBase address of
        OperandVar var
          | isAllocation var ->
              let start = addressByteOffset wordBytes address
               in Just (var, [start .. start + accessBytes ty - 1])
        _ -> Nothing
    complete written var = Set.fromList [0 .. sizes Map.! var - 1] `Set.isSubsetOf` Map.findWithDefault Set.empty var written
    fill var = Map.insert var (Set.fromList [0 .. sizes Map.! var - 1])
    operationVars = getConst . forOperationOperands (Const . operandVars)
    terminatorVars = getConst . forTerminatorOperands (Const . operandVars)

-- | The bytes of one memory access. A @ptr@ and a @code@ value are one
-- word.
accessBytes :: Type -> Integer
accessBytes ty
  | ty `elem` [Ptr, Code] = wordBytes
  | otherwise = toInteger (typeBytes ty)

placeAllocations :: [(Var, Integer, Integer)] -> (Map Var Int, Int)
placeAllocations = go Map.empty 0
  where
    go placed next [] = (placed, next)
    go placed next ((var, size, alignment) : rest) =
      let start = roundUp (fromInteger alignment) next
       in go (Map.insert var start placed) (start + fromInteger size) rest
    roundUp alignment value = ((value + alignment - 1) `div` alignment) * alignment

-- | The types of the results of an instruction, for the local declarations.
resultTypes :: Ctx -> Instruction -> [(Var, Type)]
resultTypes ctx (Instruction results operation) =
  zip results $ case operation of
    Binary _ ty _ _ -> [ty]
    Unary _ ty _ -> [ty]
    Wide op ty _ _ -> if op `elem` [AddCarry, SubBorrow] then [ty, I1] else [ty, ty]
    QuotRemWide ty _ _ _ -> [ty, ty]
    Compare {} -> [I1]
    FloatBinary _ ty _ _ -> [ty]
    FloatUnary _ ty _ -> [ty]
    Convert _ _ _ to -> [to]
    PtrToInt _ -> [I64]
    PtrFromInt _ -> [Ptr]
    Select ty _ _ _ -> [ty]
    Load ty _ _ -> [ty]
    Store {} -> []
    LoadAtomic _ ty _ -> [ty]
    StoreAtomic {} -> []
    LoadDependent ty _ -> [ty]
    LoadInvariant ty _ _ -> [ty]
    CompareSwap _ ty _ _ _ -> [ty]
    AtomicRmw _ _ ty _ _ -> [ty]
    Fence _ -> []
    PtrAdd _ _ -> [Ptr]
    StackAlloc _ _ -> [Ptr]
    GlobalGet symbol -> [Map.findWithDefault I64 symbol (ctxGlobals ctx)]
    GlobalSet _ _ -> []
    Call symbol _ -> maybe [] signatureResults (Map.lookup symbol (ctxSignatures ctx))
    CallIndirect _ _ signature -> signatureResults signature

-- | The prologue and the bodies of the blocks.
functionBody :: Fn -> M ()
functionBody fn = do
  forM_ (fnFrame fn) $ \frame -> when (isNothing (frameBlocks frame)) (enterFrame frame)
  let blocks = functionBlocks (fnFunction fn)
  case (blocks, analyzeControlFlow blocks) of
    (entry : _, Just flow) -> structuredTree fn flow [] (blockLabel entry)
    _ -> dispatchBody fn

-- | A construct around the current point of the code. A branch names its
-- target by the depth of the construct in the list, innermost first.
data Enclosing
  = EnclosingIf
  | -- | A @loop@ whose start is the code of the block.
    EnclosingLoop !Label
  | -- | A @block@ whose end is followed by the code of the block.
    EnclosingBlock !Label
  deriving (Eq)

-- | The code of a block and of the blocks that it immediately dominates.
structuredTree :: Fn -> ControlFlow -> [Enclosing] -> Label -> M ()
structuredTree fn flow context label
  | Set.member label (flowLoopHeaders flow) = do
      emit "loop"
      within (EnclosingLoop label : context)
      emit "end_loop"
      -- No path leaves the loop at its end. The validator cannot know
      -- this, and a function with results must not reach its end.
      emit "unreachable"
  | otherwise = within context
  where
    within enclosing = nodeWithin fn flow enclosing label (Map.findWithDefault [] label (flowMergeChildren flow))

-- | The code of a block in one @block@ for each of its merge children. The
-- first child has the largest reverse postorder number, so its @block@ is
-- the outermost one, and its code comes last.
nodeWithin :: Fn -> ControlFlow -> [Enclosing] -> Label -> [Label] -> M ()
nodeWithin fn flow context label children =
  case children of
    child : rest -> do
      emit "block"
      nodeWithin fn flow (EnclosingBlock child : context) label rest
      emit "end_block"
      structuredTree fn flow context child
    [] -> do
      block <- maybe (unsupported ("unknown block " <> unLabel label)) pure (Map.lookup label (fnBlocks fn))
      blockCode fn block (\opened -> structuredBranch fn flow (replicate opened EnclosingIf <> context) label)

-- | A backward edge continues its loop. A forward edge to a merge node
-- leaves the @block@ that the merge node follows. Any other forward edge is
-- the only one to its target, so the code of the target goes here.
structuredBranch :: Fn -> ControlFlow -> [Enclosing] -> Label -> Target -> M ()
structuredBranch fn flow context source target = do
  assignParameters fn target
  continueAt
  where
    label = targetLabel target
    order block = Map.findWithDefault 0 block (flowOrder flow)
    continueAt
      | order label <= order source = branchOut (EnclosingLoop label)
      | Set.member label (flowMergeNodes flow) = branchOut (EnclosingBlock label)
      | otherwise = structuredTree fn flow context label
    branchOut enclosing =
      case elemIndex enclosing context of
        Just depth -> emit ("br\t" <> tshow depth)
        Nothing -> unsupported ("no construct encloses the jump to " <> unLabel label)

-- | One loop around one nested block per Lir block, with a @br_table@ on
-- the current block index. Only an irreducible function uses it.
dispatchBody :: Fn -> M ()
dispatchBody fn = do
  let blocks = functionBlocks (fnFunction fn)
      count = length blocks
  emit "loop"
  forM_ [1 .. count] $ \_ -> emit "block"
  emitLocal LocalGet SlotState
  emit ("br_table\t{" <> T.intercalate ", " (map tshow ([0 .. count - 1] <> [count - 1])) <> "}")
  forM_ (zip [0 ..] blocks) $ \(index, block) -> do
    emit "end_block"
    blockCode fn block (\opened -> jumpTo fn (count - 1 - index + opened))
  emit "end_loop"
  emit "unreachable"

known :: Fn -> Var -> M ()
known fn var = unless (Map.member var (fnTypes fn)) (unsupported ("unknown value " <> unVar var))

-- | Read a value from its local. A value that is still on the operand
-- stack goes into its local when its instruction completes.
getVar :: Fn -> Var -> M ()
getVar fn var = do
  known fn var
  modify' $ \state -> state {functionPending = filter (/= var) (functionPending state)}
  emitLocal LocalGet (SlotVar var)

setVar :: Fn -> Var -> M ()
setVar fn var = do
  known fn var
  emitLocal LocalSet (SlotVar var)

-- | Complete the code of a value that is on the operand stack. A value
-- that no code reads is dropped. A candidate stays on the stack until an
-- instruction reads it from there or from its local.
define :: Fn -> Var -> M ()
define fn var
  | Map.findWithDefault 0 var (fnUses fn) == 0 = emit "drop"
  | Set.member var (fnCandidates fn) = do
      addLine (LineDef var)
      modify' $ \state -> state {functionPending = var : functionPending state}
  | otherwise = setVar fn var

-- | Run the code of an instruction that pushes these operands first. The
-- longest prefix of the operands that is on the top of the operand stack
-- stays there, and the code does not push it again. This is possible only
-- when no code of an enclosing instruction pushed a value yet.
withOperands :: [Operand] -> M a -> M a
withOperands operands action = do
  clean <- functionClean <$> get
  modify' $ \state ->
    let pending = functionPending state
        vars = [var | OperandVar var <- takeWhile isVarOperand operands]
        fits n = reverse (take n pending) == take n vars
        most = if clean then min (length vars) (length pending) else 0
        count = fromMaybe 0 (find fits [most, most - 1 .. 1])
        taken = take count vars
     in state
          { functionPending = drop count pending,
            functionSkip = taken,
            functionConsumed = foldr Set.insert (functionConsumed state) taken
          }
  result <- action
  skip <- functionSkip <$> get
  case skip of
    var : _ -> unsupported ("the operand " <> unVar var <> " stays on the stack")
    [] -> do
      -- The code of an instruction keeps the values below its operands.
      modify' $ \state -> state {functionClean = clean}
      pure result
  where
    isVarOperand operand = case operand of
      OperandVar _ -> True
      OperandLiteral _ -> False

-- | Run the code of a terminator that pushes these operands first. The
-- other values on the operand stack go into their locals, because the
-- stack must be empty at a branch.
withTerminatorOperands :: [Operand] -> M () -> M ()
withTerminatorOperands operands action =
  withOperands operands $ do
    modify' $ \state -> state {functionPending = []}
    action

-- | The operands that the code of an operation pushes first, in order and
-- with no other code between them.
leadingOperands :: Operation -> [Operand]
leadingOperands operation =
  case operation of
    Binary op ty left right
      | op `elem` [DivS, DivU, RemS, RemU] -> [right]
      | op == ShrS -> left : [right | not (extends ty)]
      | op `elem` [Pdep, Pext] -> left : [right | is64 ty]
      | op `elem` [RotL, RotR] && ty == I1 -> [left]
      | otherwise -> [left, right]
    QuotRemWide {} -> []
    Wide op ty left right
      | op `elem` [MulWideU, MulWideS] && not (is64 ty) -> [left]
      | otherwise -> [left, right]
    Unary _ _ value -> [value]
    Compare op ty left right
      | op `elem` [LtS, LeS, GtS, GeS] && not (isFloatType ty) && extends ty -> [left]
      | otherwise -> [left, right]
    FloatBinary _ _ left right -> [left, right]
    FloatUnary _ _ value -> [value]
    Convert op _ value _
      | op `elem` [FToIS, FToIU] -> [value, value]
      | otherwise -> [value]
    PtrToInt value -> [value]
    PtrFromInt value -> [value]
    Select _ condition left right -> [left, right, condition]
    Load _ address _ -> [addressBase address]
    Store _ value address _ -> storeOperands value address
    LoadAtomic _ _ address -> [addressBase address]
    StoreAtomic _ _ value address -> storeOperands value address
    LoadDependent _ address -> [addressBase address]
    LoadInvariant _ address _ -> [addressBase address]
    CompareSwap _ _ _ _ address -> [addressBase address]
    AtomicRmw _ _ _ _ address -> [addressBase address]
    Fence _ -> []
    PtrAdd base offset -> [base, offset]
    StackAlloc _ _ -> []
    GlobalGet _ -> []
    GlobalSet _ value -> [value]
    Call _ arguments -> arguments
    CallIndirect target _ _ -> [target]
  where
    storeOperands value address =
      addressBase address : [value | addressByteOffset wordBytes address >= 0]

-- | Whether 'signExtend' emits code for a type.
extends :: Type -> Bool
extends ty = ty `elem` [I1, I8, I16]

-- | Push an operand with the encoding of its type.
push :: Fn -> Type -> Operand -> M ()
push fn ty operand =
  case operand of
    OperandVar var -> do
      state <- get
      case functionSkip state of
        next : rest | next == var -> put state {functionSkip = rest, functionClean = False}
        _
          | Just instruction <- Map.lookup var (fnDeferred fn),
            not (Set.member var (functionConsumed state)) ->
              pushDeferred fn var instruction
          | otherwise -> getVar fn var
    OperandLiteral literal ->
      case literal of
        LitInt value
          | ty == F64 -> pushDouble (fromInteger value)
          | ty == F32 -> pushFloat (fromInteger value)
          | is64 ty -> emit ("i64.const\t" <> renderInteger 8 value)
          | otherwise -> emit ("i32.const\t" <> renderInteger 4 (canonicalInteger ty value))
        LitFloat value
          | ty == F32 -> pushFloat value
          | otherwise -> pushDouble value
        LitNull -> emit "i32.const\t0"
        LitSymbol symbol -> emit ("i32.const\t" <> symbolText (fnCtx fn) symbol)
  where
    pushDouble value = do
      emit ("i64.const\t" <> renderInteger 8 (toInteger (castDoubleToWord64 value)))
      emit "f64.reinterpret_i64"
    pushFloat value = do
      emit ("i32.const\t" <> renderInteger 4 (toInteger (castFloatToWord32 (double2Float value))))
      emit "f32.reinterpret_i32"

-- | Push the value of a deferred instruction: compute it here.
pushDeferred :: Fn -> Var -> Instruction -> M ()
pushDeferred fn var instruction@(Instruction _ operation) = do
  withOperands (leadingOperands operation) (compileInstruction fn instruction)
  state <- get
  case functionPending state of
    top : rest
      | top == var ->
          put state {functionPending = rest, functionConsumed = Set.insert var (functionConsumed state), functionClean = False}
    _ -> unsupported ("the deferred value " <> unVar var <> " is not on the stack")

-- | Push the low 32 bits of an @i64@ operand as an @i32@. The low bits of a
-- literal, of a load from little-endian memory, and of an addition, a
-- subtraction, a multiplication, or a bitwise operation do not need the
-- high bits. Thus such a deferred operand is computed in @i32@ when that
-- needs at most one @i32.wrap_i64@.
pushLow :: Fn -> Operand -> M ()
pushLow fn operand = do
  state <- get
  case operand of
    OperandLiteral (LitInt value) -> emit ("i32.const\t" <> renderInteger 4 value)
    OperandVar var
      | take 1 (functionSkip state) /= [var],
        not (Set.member var (functionConsumed state)),
        Just instruction <- Map.lookup var (fnDeferred fn),
        Just low <- lowInstruction instruction,
        either (const True) (\(_, left, right) -> lowCost fn left + lowCost fn right <= 1) low ->
          case low of
            Left narrowed -> pushDeferred fn var narrowed
            Right (op, left, right) -> do
              modify' $ \state' -> state' {functionConsumed = Set.insert var (functionConsumed state')}
              withOperands [left, right] $ do
                pushLow fn left
                pushLow fn right
              emit ("i32." <> lowOperation op)
              modify' $ \state' -> state' {functionClean = False}
    _ -> push fn I64 operand >> emit "i32.wrap_i64"

-- | The @i32@ form of an @i64@ instruction: a narrower load, or a binary
-- operation on the low bits of its operands.
lowInstruction :: Instruction -> Maybe (Either Instruction (BinaryOp, Operand, Operand))
lowInstruction (Instruction results operation) =
  case operation of
    Load I64 address alignment -> Just (Left (Instruction results (Load I32 address alignment)))
    LoadInvariant I64 address alignment -> Just (Left (Instruction results (LoadInvariant I32 address alignment)))
    LoadDependent I64 address -> Just (Left (Instruction results (LoadDependent I32 address)))
    Binary op I64 left right | op `elem` [Add, Sub, Mul, And, Or, Xor] -> Just (Right (op, left, right))
    _ -> Nothing

-- | The number of @i32.wrap_i64@ instructions that 'pushLow' emits.
lowCost :: Fn -> Operand -> Int
lowCost fn operand =
  case operand of
    OperandLiteral (LitInt _) -> 0
    OperandVar var
      | Just instruction <- Map.lookup var (fnDeferred fn),
        Just low <- lowInstruction instruction ->
          case low of
            Left _ -> 0
            Right (_, left, right) -> min 1 (lowCost fn left + lowCost fn right)
    -- An operand that 'pushLow' cannot narrow costs one wrap.
    _ -> 1

lowOperation :: BinaryOp -> Text
lowOperation op =
  case op of
    Add -> "add"
    Sub -> "sub"
    Mul -> "mul"
    And -> "and"
    Or -> "or"
    _ -> "xor"

canonicalInteger :: Type -> Integer -> Integer
canonicalInteger ty value = value `mod` (2 ^ min 64 (typeBits ty))

-- | Mask the value on the stack to the width of a narrow type.
narrow :: Type -> M ()
narrow ty =
  case ty of
    I1 -> emit "i32.const\t1" >> emit "i32.and"
    I8 -> emit "i32.const\t255" >> emit "i32.and"
    I16 -> emit "i32.const\t65535" >> emit "i32.and"
    _ -> pure ()

-- | Sign-extend the canonical value on the stack within its value type.
signExtend :: Type -> M ()
signExtend ty =
  case ty of
    I1 -> emit "i32.const\t31" >> emit "i32.shl" >> emit "i32.const\t31" >> emit "i32.shr_s"
    I8 -> emit "i32.extend8_s"
    I16 -> emit "i32.extend16_s"
    _ -> pure ()

-- | Restore the shadow stack pointer before the function leaves, when one
-- frame lasts for the whole function. A block frame is already removed.
leaveFunctionFrame :: Fn -> M ()
leaveFunctionFrame fn =
  forM_ (fnFrame fn) $ \frame -> when (isNothing (frameBlocks frame)) (leaveFrame frame)

-- | The instructions and the terminator of one block. A block with a frame
-- of its own makes the frame first and computes the address of each stack
-- allocation. It removes the frame before its terminator.
blockCode :: Fn -> Block -> (Int -> Target -> M ()) -> M ()
blockCode fn block jump = do
  emitLabel ("# " <> unLabel (blockLabel block))
  let blockFrame = [frame | Just frame <- [fnFrame fn], Just labels <- [frameBlocks frame], Set.member (blockLabel block) labels]
  forM_ blockFrame $ \frame -> do
    enterFrame frame
    forM_ (Map.toAscList (fnAllocs fn)) $ \(var, offset) -> do
      allocationAddress offset
      setVar fn var
  setClean
  forM_ (blockInstructions block) $ \instruction@(Instruction results operation) ->
    unless (any (`Map.member` fnDeferred fn) results) $
      withOperands (leadingOperands operation) (compileInstruction fn instruction)
  mapM_ leaveFrame blockFrame
  setClean
  compileTerminator fn jump (blockTerminator block)

-- | Record that the operand stack holds exactly the pending values, after
-- code that keeps the stack as it was.
setClean :: M ()
setClean = modify' $ \state -> state {functionClean = True}

-- | Move the shadow stack pointer below the frame and keep the new value.
enterFrame :: Frame -> M ()
enterFrame frame = do
  emit ("global.get\t" <> stackPointer)
  emit ("i32.const\t" <> tshow (frameSize frame))
  emit "i32.sub"
  emitLocal LocalTee SlotFrame
  emit ("global.set\t" <> stackPointer)

-- | Restore the shadow stack pointer.
leaveFrame :: Frame -> M ()
leaveFrame frame = do
  emitLocal LocalGet SlotFrame
  emit ("i32.const\t" <> tshow (frameSize frame))
  emit "i32.add"
  emit ("global.set\t" <> stackPointer)

-- | Push the address of a stack allocation in the frame.
allocationAddress :: Int -> M ()
allocationAddress offset = do
  emitLocal LocalGet SlotFrame
  when (offset /= 0) $ do
    emit ("i32.const\t" <> tshow offset)
    emit "i32.add"

-- | Assign the parameters of the target. All arguments are on the stack
-- before the first assignment, so an argument can name a parameter.
--
-- A copy from a local to the same local does nothing, but the locals are
-- not known yet. Thus each copy from a local has a tag on its two lines, so
-- that 'assignLocals' can remove them.
assignParameters :: Fn -> Target -> M ()
assignParameters fn (Target label arguments) = do
  let parameters = Map.findWithDefault [] label (fnBlockParameters fn)
  tags <- mapM (uncurry copyArgument) (zip (map snd parameters) arguments)
  forM_ (reverse (zip parameters tags)) $ \((var, _), tag) ->
    case tag of
      Just copy -> known fn var >> addLine (LineCopy LocalSet var copy)
      Nothing -> setVar fn var
  where
    copyArgument ty argument = do
      state <- get
      case argument of
        OperandVar var
          | take 1 (functionSkip state) /= [var],
            not (Map.member var (fnDeferred fn)) -> do
              known fn var
              let copy = functionCopies state
              put state {functionPending = filter (/= var) (functionPending state), functionCopies = copy + 1}
              addLine (LineCopy LocalGet var copy)
              pure (Just copy)
        _ -> push fn ty argument >> pure Nothing

-- | Assign the parameters of the target and continue the dispatch loop.
-- The depth is the number of labels between the current point and the loop.
jumpTo :: Fn -> Int -> Target -> M ()
jumpTo fn depth target = do
  assignParameters fn target
  emit ("i32.const\t" <> tshow (Map.findWithDefault 0 (targetLabel target) (fnBlockIndex fn)))
  emitLocal LocalSet SlotState
  emit ("br\t" <> tshow depth)

-- | The terminator of a block. The code of a jump never continues after
-- it, so a branch needs no @else@. The jump function takes the number of
-- @if@ constructs that the terminator opened around the jump.
compileTerminator :: Fn -> (Int -> Target -> M ()) -> Terminator -> M ()
compileTerminator fn jump terminator =
  case terminator of
    Jump target -> withTerminatorOperands (targetArguments target) (jump 0 target)
    Branch condition whenTrue whenFalse -> withTerminatorOperands [condition] $ do
      push fn I1 condition
      emit "if"
      jump 1 whenTrue
      emit "end_if"
      jump 0 whenFalse
    Switch ty scrutinee cases fallback -> withTerminatorOperands (if null cases then maybe [] targetArguments fallback else [scrutinee]) $ do
      forM_ cases $ \switchCase -> do
        push fn ty scrutinee
        push fn ty (OperandLiteral (LitInt (resolvedSwitchCaseValue switchCase)))
        emit (prefix ty <> ".eq")
        emit "if"
        jump 1 (switchCaseTarget switchCase)
        emit "end_if"
      case fallback of
        Just target -> jump 0 target
        Nothing -> trap "switch without a matching case"
    Return values -> do
      leaveFunctionFrame fn
      setClean
      let results = zip (functionResults (fnFunction fn)) values
      withTerminatorOperands (map snd results) $ do
        forM_ results (uncurry (push fn))
        emit "return"
    TailCall symbol arguments -> do
      let signature = Map.findWithDefault (Signature [] [] AihcConvention) symbol (ctxSignatures (fnCtx fn))
      leaveFunctionFrame fn
      setClean
      let pushed = zip (signatureParameters signature) arguments
      withTerminatorOperands (map snd pushed) $ do
        forM_ pushed (uncurry (push fn))
        emit ("return_call\t" <> symbolText (fnCtx fn) symbol)
    TailCallIndirect target arguments signature -> withTerminatorOperands [target] $ do
      guardCallee fn target
      leaveFunctionFrame fn
      forM_ (zip (signatureParameters signature) arguments) (uncurry (push fn))
      push fn Code target
      emit ("return_call_indirect\t__indirect_function_table, " <> renderSignature signature)
    Trap message -> withTerminatorOperands [] (trap message)

-- | Trap when a callee is null.
guardCallee :: Fn -> Operand -> M ()
guardCallee fn target = do
  push fn Code target
  emit "i32.eqz"
  trapIf "indirect call to a non-function"

compileInstruction :: Fn -> Instruction -> M ()
compileInstruction fn (Instruction results operation) =
  case operation of
    Binary op ty left right -> do
      let p = prefix ty
      case op of
        Add -> operands ty left right >> emit (p <> ".add") >> narrow ty >> single
        Sub -> operands ty left right >> emit (p <> ".sub") >> narrow ty >> single
        Mul -> operands ty left right >> emit (p <> ".mul") >> narrow ty >> single
        And -> operands ty left right >> emit (p <> ".and") >> single
        Or -> operands ty left right >> emit (p <> ".or") >> single
        Xor -> operands ty left right >> emit (p <> ".xor") >> single
        Shl -> push fn ty left >> shiftCount ty right >> emit (p <> ".shl") >> narrow ty >> single
        ShrU -> push fn ty left >> shiftCount ty right >> emit (p <> ".shr_u") >> single
        RotL -> rotateBits True ty left right >> single
        RotR -> rotateBits False ty left right >> single
        ShrS -> push fn ty left >> signExtend ty >> shiftCount ty right >> emit (p <> ".shr_s") >> narrow ty >> single
        DivS -> do
          zeroCheck ty right
          overflowCheck ty left right
          signedOperands ty left right
          emit (p <> ".div_s") >> narrow ty >> single
        DivU -> zeroCheck ty right >> operands ty left right >> emit (p <> ".div_u") >> single
        RemS -> do
          zeroCheck ty right
          signedOperands ty left right
          emit (p <> ".rem_s") >> narrow ty >> single
        RemU -> zeroCheck ty right >> operands ty left right >> emit (p <> ".rem_u") >> single
        -- WebAssembly has no bit deposit or extract. Both call a helper on
        -- 64-bit operands; a narrow mask keeps the result in the width.
        Pdep -> bitScatterCall "aihc_lir_wasm_pdep64"
        Pext -> bitScatterCall "aihc_lir_wasm_pext64"
      where
        bitScatterCall helper = do
          push fn ty left
          unless (is64 ty) (emit "i64.extend_i32_u")
          push fn ty right
          unless (is64 ty) (emit "i64.extend_i32_u")
          emit ("call\t" <> linkedName Internal (Symbol helper))
          unless (is64 ty) (emit "i32.wrap_i64")
          single
    QuotRemWide {} -> unsupported "wide division reached instruction selection"
    Wide op ty left right ->
      case results of
        [first, second] ->
          case op of
            MulWideU
              | is64 ty -> wideCall "aihc_lir_wasm_mul_wide_u64" first second
              | otherwise -> narrowWide "extend_i32_u" "shr_u" first second
            MulWideS
              | is64 ty -> wideCall "aihc_lir_wasm_mul_wide_s64" first second
              | otherwise -> narrowWide "extend_i32_s" "shr_s" first second
            AddCarry -> do
              operands ty left right
              emit (prefix ty <> ".add")
              narrow ty
              setVar fn first
              -- The sum is below the left operand exactly when it wrapped.
              getVar fn first
              push fn ty left
              emit (prefix ty <> ".lt_u")
              setVar fn second
            SubBorrow -> do
              push fn ty left
              push fn ty right
              emit (prefix ty <> ".lt_u")
              setVar fn second
              operands ty left right
              emit (prefix ty <> ".sub")
              narrow ty
              setVar fn first
          where
            wideCall helper first' second' = do
              operands ty left right
              emit ("call\t" <> linkedName Internal (Symbol helper))
              setVar fn second'
              setVar fn first'
            -- A narrow product fits an i64.
            narrowWide extend shift first' second' = do
              let bits = typeBits ty
                  scratch = SlotScratch "i64"
              push fn ty left >> signOrZero extend
              emit ("i64." <> extend)
              push fn ty right >> signOrZero extend
              emit ("i64." <> extend)
              emit "i64.mul"
              emitLocal LocalTee scratch
              emit "i32.wrap_i64"
              narrow ty
              setVar fn first'
              emitLocal LocalGet scratch
              emit ("i64.const\t" <> tshow bits)
              emit ("i64." <> shift)
              emit "i32.wrap_i64"
              narrow ty
              setVar fn second'
            signOrZero extend = when (extend == "extend_i32_s") (signExtend ty)
        _ -> unsupported "wide operation result count"
    -- WebAssembly counts the bits of the whole container. A narrow type
    -- lives zero-extended in an i32, so a leading-zero count subtracts the
    -- bits above the type, and a trailing-zero count sets the first bit above
    -- it so that a zero operand counts the width of the type instead of 32.
    Unary op ty value -> do
      let bits = typeBits ty
          narrowContainer = bits < 32
      push fn ty value
      case op of
        Clz -> do
          emit (prefix ty <> ".clz")
          when narrowContainer $ do
            emit ("i32.const\t" <> tshow (32 - bits))
            emit "i32.sub"
        Ctz -> do
          when narrowContainer $ do
            emit ("i32.const\t" <> tshow ((2 :: Integer) ^ bits))
            emit "i32.or"
          emit (prefix ty <> ".ctz")
        Popcount -> emit (prefix ty <> ".popcnt")
      single
    Compare op ty left right -> do
      let p = if isFloatType ty then wasmType ty else prefix ty
      if op `elem` [LtS, LeS, GtS, GeS] && not (isFloatType ty)
        then signedOperands ty left right
        else operands ty left right
      emit (p <> "." <> comparison op ty)
      single
    FloatBinary op ty left right -> do
      operands ty left right
      emit (wasmType ty <> "." <> floatBinary op)
      single
    FloatUnary op ty value -> do
      push fn ty value
      emit (wasmType ty <> "." <> floatUnary op)
      single
    Convert op from value to ->
      case op of
        SExt -> do
          push fn from value
          signExtend from
          when (is64 to) (emit "i64.extend_i32_s")
          narrow to
          single
        ZExt
          | is64 to,
            OperandLiteral (LitInt literal) <- value -> do
              push fn to (OperandLiteral (LitInt (canonicalInteger from literal)))
              single
          | otherwise -> do
              push fn from value
              when (is64 to) (emit "i64.extend_i32_u")
              single
        Trunc -> do
          if is64 from then pushLow fn value else push fn from value
          narrow to
          single
        IToFS -> do
          push fn from value
          signExtend from
          emit (wasmType to <> ".convert_" <> prefix from <> "_s")
          single
        IToFU -> do
          push fn from value
          emit (wasmType to <> ".convert_" <> prefix from <> "_u")
          single
        FToIS -> floatToInteger True from value to
        FToIU -> floatToInteger False from value to
        FpExt -> push fn from value >> emit "f64.promote_f32" >> single
        FpTrunc -> push fn from value >> emit "f32.demote_f64" >> single
        Bitcast -> do
          push fn from value
          emit (wasmType to <> ".reinterpret_" <> wasmType from)
          single
    PtrToInt value -> push fn Ptr value >> emit "i64.extend_i32_u" >> single
    PtrFromInt value -> pushLow fn value >> single
    Select ty condition left right -> do
      push fn ty left
      push fn ty right
      push fn I1 condition
      emit (wasmType ty <> ".select")
      single
    Load ty address _ -> do
      offset <- effectiveAddress address
      emit (loadInstruction ty <> "\t" <> tshow offset)
      when (ty == I1) (narrow I1)
      single
    Store ty value address _ -> do
      offset <- effectiveAddress address
      push fn ty value
      emit (storeInstruction ty <> "\t" <> tshow offset)
    -- A module without shared memory runs one thread, so each atomic
    -- operation is a plain access.
    LoadAtomic _ ty address -> plainLoad ty address
    LoadDependent ty address -> plainLoad ty address
    LoadInvariant ty address _ -> plainLoad ty address
    StoreAtomic _ ty value address -> do
      offset <- effectiveAddress address
      push fn ty value
      emit (storeInstruction ty <> "\t" <> tshow offset)
    CompareSwap _ ty expected replacement address ->
      case results of
        [old] -> do
          plainLoad ty address
          offset <- effectiveAddress address
          push fn ty replacement
          push fn ty (OperandVar old)
          push fn ty (OperandVar old)
          push fn ty expected
          emit (prefix ty <> ".eq")
          emit (wasmType ty <> ".select")
          emit (storeInstruction ty <> "\t" <> tshow offset)
        _ -> unsupported "cas result count"
    AtomicRmw op _ ty value address ->
      case results of
        [old] -> do
          plainLoad ty address
          offset <- effectiveAddress address
          case op of
            AtomicXchg -> push fn ty value
            AtomicAdd -> do
              push fn ty (OperandVar old)
              push fn ty value
              emit (prefix ty <> ".add")
              narrow ty
            AtomicOr -> do
              push fn ty (OperandVar old)
              push fn ty value
              emit (prefix ty <> ".or")
          emit (storeInstruction ty <> "\t" <> tshow offset)
        _ -> unsupported "atomic operation result count"
    Fence _ -> pure ()
    PtrAdd base offset -> do
      push fn Ptr base
      pushLow fn offset
      emit "i32.add"
      single
    StackAlloc _ _ ->
      case (results, fnFrame fn) of
        -- Each block frame computes the address when it starts.
        (_, Just Frame {frameBlocks = Just _}) -> pure ()
        ([var], Just _) | Just offset <- Map.lookup var (fnAllocs fn) -> do
          allocationAddress offset
          single
        _ -> unsupported "stack.alloc without a placed result"
    GlobalGet symbol -> emit ("global.get\t" <> symbolText (fnCtx fn) symbol) >> single
    GlobalSet symbol value -> do
      push fn (Map.findWithDefault I64 symbol (ctxGlobals (fnCtx fn))) value
      emit ("global.set\t" <> symbolText (fnCtx fn) symbol)
    Call symbol arguments -> do
      let original = Map.findWithDefault (Signature [] [] AihcConvention) symbol (ctxSignatures (fnCtx fn))
      forM_ (zip (signatureParameters original) arguments) (uncurry (push fn))
      emit ("call\t" <> symbolText (fnCtx fn) symbol)
      storeResults
    CallIndirect target arguments signature -> do
      guardCallee fn target
      forM_ (zip (signatureParameters signature) arguments) (uncurry (push fn))
      push fn Code target
      emit ("call_indirect\t__indirect_function_table, " <> renderSignature signature)
      storeResults
  where
    single =
      case results of
        [var] -> define fn var
        _ -> unsupported "instruction result count"
    storeResults =
      case results of
        [var] -> define fn var
        _ -> forM_ (reverse results) (setVar fn)
    operands ty left right = push fn ty left >> push fn ty right
    signedOperands ty left right = do
      push fn ty left
      signExtend ty
      push fn ty right
      signExtend ty

    rotateBits leftward ty value count
      | typeBits ty >= 32 = operands ty value count >> emit (prefix ty <> if leftward then ".rotl" else ".rotr")
      | ty == I1 = push fn ty value
      | otherwise = do
          push fn ty value
          shiftCount ty count
          emit (if leftward then "i32.shl" else "i32.shr_u")
          push fn ty value
          emit ("i32.const\t" <> tshow (typeBits ty))
          shiftCount ty count
          emit "i32.sub"
          emit (if leftward then "i32.shr_u" else "i32.shl")
          emit "i32.or"
          narrow ty

    shiftCount ty count = do
      push fn ty count
      unless (typeBits ty `elem` [32, 64]) $ do
        emit ("i32.const\t" <> tshow (typeBits ty - 1))
        emit "i32.and"

    zeroCheck ty divisor = do
      push fn ty divisor
      emit (prefix ty <> ".eqz")
      trapIf "integer division by zero"

    overflowCheck ty left right = do
      push fn ty left
      push fn ty (OperandLiteral (LitInt (negate (2 ^ (typeBits ty - 1)))))
      emit (prefix ty <> ".eq")
      push fn ty right
      push fn ty (OperandLiteral (LitInt (-1)))
      emit (prefix ty <> ".eq")
      emit "i32.and"
      trapIf "integer overflow"

    -- Reject NaN and values whose truncation is outside the integer range.
    floatToInteger signed from value to = do
      let bits = typeBits to
          (lower, excludeLower, upper) = integerConversionBounds signed from to
          f = wasmType from
          target = if bits > 32 then I64 else I32
          suffix = if signed then "_s" else "_u"
      push fn from value
      push fn from value
      emit (f <> ".ne")
      trapIf "invalid float to integer conversion"
      push fn from value
      push fn from (OperandLiteral (LitFloat lower))
      emit (f <> (if excludeLower then ".le" else ".lt"))
      trapIf "invalid float to integer conversion"
      push fn from value
      push fn from (OperandLiteral (LitFloat upper))
      emit (f <> ".ge")
      trapIf "invalid float to integer conversion"
      push fn from value
      emit (prefix target <> ".trunc_" <> f <> suffix)
      narrow to
      single

    plainLoad ty address = do
      offset <- effectiveAddress address
      emit (loadInstruction ty <> "\t" <> tshow offset)
      single

    effectiveAddress address = do
      let offset = addressByteOffset wordBytes address
      push fn Ptr (addressBase address)
      if offset >= 0
        then pure offset
        else do
          emit ("i32.const\t" <> tshow offset)
          emit "i32.add"
          pure 0

loadInstruction :: Type -> Text
loadInstruction ty =
  case ty of
    I1 -> "i32.load8_u"
    I8 -> "i32.load8_u"
    I16 -> "i32.load16_u"
    I64 -> "i64.load"
    F32 -> "f32.load"
    F64 -> "f64.load"
    _ -> "i32.load"

storeInstruction :: Type -> Text
storeInstruction ty =
  case ty of
    I1 -> "i32.store8"
    I8 -> "i32.store8"
    I16 -> "i32.store16"
    I64 -> "i64.store"
    F32 -> "f32.store"
    F64 -> "f64.store"
    _ -> "i32.store"

comparison :: CompareOp -> Type -> Text
comparison op ty
  | isFloatType ty =
      case op of
        Eq -> "eq"
        Ne -> "ne"
        FLt -> "lt"
        FLe -> "le"
        FGt -> "gt"
        FGe -> "ge"
        _ -> "eq"
  | otherwise =
      case op of
        Eq -> "eq"
        Ne -> "ne"
        LtS -> "lt_s"
        LtU -> "lt_u"
        LeS -> "le_s"
        LeU -> "le_u"
        GtS -> "gt_s"
        GtU -> "gt_u"
        GeS -> "ge_s"
        GeU -> "ge_u"
        FLt -> "lt_u"
        FLe -> "le_u"
        FGt -> "gt_u"
        FGe -> "ge_u"

floatBinary :: FloatBinaryOp -> Text
floatBinary op =
  case op of
    FAdd -> "add"
    FSub -> "sub"
    FMul -> "mul"
    FDiv -> "div"

floatUnary :: FloatUnaryOp -> Text
floatUnary op =
  case op of
    FNeg -> "neg"
    FAbs -> "abs"
    FSqrt -> "sqrt"

-- Wide multiplication helpers

-- | The full 64-bit multiplication in 32-bit limbs, as Lir. The signed form
-- corrects the unsigned high half by the operands that are negative.
wideHelpers :: [Function]
wideHelpers = [unsignedHelper, signedHelper]
  where
    a = Var "a"
    b = Var "b"
    var = OperandVar . Var
    int = OperandLiteral . LitInt
    op name = Instruction [Var name]
    limbs =
      [ op "al" (Binary And I64 (var "a") (int 0xffffffff)),
        op "ah" (Binary ShrU I64 (var "a") (int 32)),
        op "bl" (Binary And I64 (var "b") (int 0xffffffff)),
        op "bh" (Binary ShrU I64 (var "b") (int 32)),
        op "p00" (Binary Mul I64 (var "al") (var "bl")),
        op "p01" (Binary Mul I64 (var "al") (var "bh")),
        op "p10" (Binary Mul I64 (var "ah") (var "bl")),
        op "p11" (Binary Mul I64 (var "ah") (var "bh")),
        op "p00h" (Binary ShrU I64 (var "p00") (int 32)),
        op "p01l" (Binary And I64 (var "p01") (int 0xffffffff)),
        op "p10l" (Binary And I64 (var "p10") (int 0xffffffff)),
        op "t0" (Binary Add I64 (var "p00h") (var "p01l")),
        op "t" (Binary Add I64 (var "t0") (var "p10l")),
        op "p00l" (Binary And I64 (var "p00") (int 0xffffffff)),
        op "tl" (Binary Shl I64 (var "t") (int 32)),
        op "low" (Binary Or I64 (var "p00l") (var "tl")),
        op "p01h" (Binary ShrU I64 (var "p01") (int 32)),
        op "p10h" (Binary ShrU I64 (var "p10") (int 32)),
        op "th" (Binary ShrU I64 (var "t") (int 32)),
        op "h0" (Binary Add I64 (var "p11") (var "p01h")),
        op "h1" (Binary Add I64 (var "h0") (var "p10h")),
        op "high" (Binary Add I64 (var "h1") (var "th"))
      ]
    helper name instructions =
      Function
        { functionName = Symbol name,
          functionLinkage = Internal,
          functionParameters = [(a, I64), (b, I64)],
          functionResults = [I64, I64],
          functionConvention = AihcConvention,
          functionBlocks = [Block (Label "entry") [] instructions (Return [var "low", var "result_high"]) False]
        }
    unsignedHelper = helper "aihc_lir_wasm_mul_wide_u64" (limbs <> [op "result_high" (Binary Add I64 (var "high") (int 0))])
    signedHelper =
      helper
        "aihc_lir_wasm_mul_wide_s64"
        ( limbs
            <> [ op "a_sign" (Binary ShrS I64 (var "a") (int 63)),
                 op "b_sign" (Binary ShrS I64 (var "b") (int 63)),
                 op "a_correction" (Binary And I64 (var "a_sign") (var "b")),
                 op "b_correction" (Binary And I64 (var "b_sign") (var "a")),
                 op "high_a" (Binary Sub I64 (var "high") (var "a_correction")),
                 op "result_high" (Binary Sub I64 (var "high_a") (var "b_correction"))
               ]
        )

-- Bit deposit and extract helpers

-- | The parallel bit deposit and extract of 64-bit values, as Lir loops
-- over the set bits of the mask. The deposit walks the mask from its
-- lowest set bit and takes the source bits from the low end. The extract
-- walks the mask from its highest set bit, which a leading-zero count
-- finds, and shifts each source bit into the result from the low end.
bitScatterHelpers :: [Function]
bitScatterHelpers = [depositHelper, extractHelper]
  where
    var = OperandVar . Var
    int = OperandLiteral . LitInt
    op name = Instruction [Var name]
    loop = Label "loop"
    entry = Block (Label "entry") [] [] (Jump (Target loop [var "a", var "b", int 0])) False
    test =
      Block
        loop
        [(Var "source", I64), (Var "mask", I64), (Var "result", I64)]
        [op "done" (Compare Eq I64 (var "mask") (int 0))]
        (Branch (var "done") (Target (Label "exit") [var "result"]) (Target (Label "body") []))
        False
    exit = Block (Label "exit") [(Var "value", I64)] [] (Return [var "value"]) False
    helper name body =
      Function
        { functionName = Symbol name,
          functionLinkage = Internal,
          functionParameters = [(Var "a", I64), (Var "b", I64)],
          functionResults = [I64],
          functionConvention = AihcConvention,
          functionBlocks = [entry, test, body, exit]
        }
    depositHelper =
      helper
        "aihc_lir_wasm_pdep64"
        ( Block
            (Label "body")
            []
            [ op "negated" (Binary Sub I64 (int 0) (var "mask")),
              op "lowest" (Binary And I64 (var "mask") (var "negated")),
              op "rest" (Binary Xor I64 (var "mask") (var "lowest")),
              op "bit" (Binary And I64 (var "source") (int 1)),
              op "keep" (Binary Sub I64 (int 0) (var "bit")),
              op "placed" (Binary And I64 (var "lowest") (var "keep")),
              op "next" (Binary Or I64 (var "result") (var "placed")),
              op "shifted" (Binary ShrU I64 (var "source") (int 1))
            ]
            (Jump (Target loop [var "shifted", var "rest", var "next"]))
            False
        )
    extractHelper =
      helper
        "aihc_lir_wasm_pext64"
        ( Block
            (Label "body")
            []
            [ op "leading" (Unary Clz I64 (var "mask")),
              op "aligned_mask" (Binary Shl I64 (var "mask") (var "leading")),
              op "aligned_source" (Binary Shl I64 (var "source") (var "leading")),
              op "grown" (Binary Shl I64 (var "result") (int 1)),
              op "top" (Binary ShrU I64 (var "aligned_source") (int 63)),
              op "next" (Binary Or I64 (var "grown") (var "top")),
              op "rest" (Binary Shl I64 (var "aligned_mask") (int 1)),
              op "shifted" (Binary Shl I64 (var "aligned_source") (int 1))
            ]
            (Jump (Target loop [var "shifted", var "rest", var "next"]))
            False
        )

tshow :: (Show value) => value -> Text
tshow = T.pack . show
