{-# LANGUAGE OverloadedStrings #-}

-- | Lower individual CPS GRIN functions to AMD64 basic blocks.
module Aihc.Amd64.Codegen.Function
  ( compileFunction,
    reserveLocalsLines,
  )
where

import Aihc.Amd64.Assemble
  ( Amd64Address (..),
    Amd64BinarySource (..),
    Amd64Condition (..),
    Amd64Instruction (..),
    Amd64JumpTarget (..),
    Amd64Memory (..),
    Amd64MoveSource (..),
    Amd64Register (..),
    Amd64Rm (..),
    Amd64Statement,
    amd64Align,
    amd64Global,
    amd64Instruction,
    amd64Label,
  )
import Aihc.Amd64.Codegen.Runtime
import Aihc.Amd64.RegisterAllocate qualified as RegisterAllocate
import Aihc.Grin.Syntax
import Aihc.Native
  ( NativeCpsCall (..),
    NativeCpsTransfer (..),
    NativeRuntimeCall (..),
    nativeCpsPrimitiveCall,
    nativeLocalsSlots,
    nativeRuntimePrimitiveCall,
    nativeSplitRuntimePrimitiveCall,
  )
import Aihc.Native.BlockLayout qualified as BlockLayout
import Aihc.Native.RegisterAllocate (Location (..), grinExprFreeVariables)
import Control.Monad (forM, forM_, replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (execStateT, get, modify')
import Data.Either (fromRight)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)

data MoveSource
  = MoveRegister !Amd64Register
  | MoveSpill !Int
  | MoveValue !GrinValue
  deriving (Eq)

compileFunction :: CompileEnv -> GrinFunction -> Either Amd64Error CompiledFunction
compileFunction env function = do
  label <- functionCodeLabel env (grinFunctionName function)
  let parameters = grinFunctionParameters function
      parameterCount = length parameters
      isContinuation = grinFunctionName function `Set.member` compileContinuationFunctions env
      valueParameterCount = if isContinuation then parameterCount else max 0 (parameterCount - 1)
      fixedOverflowLocations =
        Map.fromList
          [ (parameter, InHeapSpill index)
          | (index, parameter) <- zip [0 :: Int ..] (take valueParameterCount parameters),
            index >= length applyArgumentRegisters
          ]
      allocation = RegisterAllocate.allocateFunction fixedOverflowLocations function
      locations = RegisterAllocate.allocationLocations allocation
      firstScratch = RegisterAllocate.allocationSpillCount allocation
      bodyLabel = label <> "_body"
      initialState = FunctionState 0 firstScratch []
      valueEnv = ValueEnv env locations label (grinFunctionName function) parameters bodyLabel
  finalState <- execStateT (compileExpr valueEnv [] bodyLabel (grinFunctionBody function)) initialState
  let slotCount = max 1 (functionNextSlot finalState)
  validateSlotCount label slotCount
  let registerParameterCopies =
        moveEntryParameters
          [ (source, location)
          | (parameter, source) <- parameterRegisterPairs,
            Just location <- [Map.lookup parameter locations]
          ]
      parameterRegisterPairs =
        zip (take valueParameterCount parameters) applyArgumentRegisters
          <> [ (parameters !! valueParameterCount, applyContinuationRegister)
             | parameterCount > 0 && not isContinuation
             ]
      entry =
        exportLines env function label
          <> [ amd64Align 3,
               amd64Label label
             ]
          <> storeCurrentSrt (Map.lookup (grinFunctionName function) (compileSrtLabels env))
          <> registerParameterCopies
      blocks =
        BlockLayout.renderBlocks
          amd64Label
          (amd64Instruction . AmdJmp . Amd64JumpLabel)
          (BlockLayout.layoutBlocks bodyLabel (reverse (functionBlocksRev finalState)))
  pure
    CompiledFunction
      { compiledFunctionSlots = slotCount,
        compiledFunctionLines = entry <> blocks
      }

-- | The entry unit reserves the slot area once for every linked function.
-- Modules cannot report their slot counts to the entry unit, so the area has
-- the fixed size 'nativeLocalsSlots' and 'validateSlotCount' rejects a
-- function that would overflow it.
reserveLocalsLines :: [Amd64Statement]
reserveLocalsLines =
  [ immediate RSI nativeLocalsSlots,
    amd64Instruction (AmdMov RDI (Amd64MoveRegister R15)),
    amd64Instruction (AmdCall "aihc_alloc_locals"),
    amd64Instruction (AmdMov R14 (Amd64MoveRegister RAX))
  ]

validateSlotCount :: Text -> Int -> Either Amd64Error ()
validateSlotCount label slotCount
  | slotCount > nativeLocalsSlots = Left (Amd64UnsupportedExpression ("function " <> label <> " needs " <> tshow slotCount <> " local slots"))
  | otherwise = Right ()

exportLines :: CompileEnv -> GrinFunction -> Text -> [Amd64Statement]
exportLines env _function label
  | compileExposeAllFunctions env = [amd64Global label]
  | otherwise = []

compileExpr :: ValueEnv -> [Amd64Statement] -> Text -> GrinExpr -> FunctionM ()
compileExpr env prefix label expression =
  case expression of
    GrinConstant {} -> unsupportedExpression "direct-style constant return after CPS"
    GrinBind vars valueExpression body -> do
      directLines <- compileDirectBinding env vars valueExpression
      compileExpr env (prefix <> directLines) label body
    GrinStore {} -> unsupportedExpression "direct-style store return after CPS"
    GrinEnsureHeap {} -> unsupportedExpression "unbound heap reservation"
    GrinStoreUnchecked {} -> unsupportedExpression "unbound unchecked store"
    GrinStoreRec bindings body -> compileStoreRec allocateNode bindings body
    GrinStoreRecUnchecked bindings body -> compileStoreRec allocateNodeUnchecked bindings body
    GrinUpdate {} -> unsupportedExpression "direct-style update return after CPS"
    GrinUpdateBlackhole {} -> unsupportedExpression "unbound blackhole update"
    GrinEval {} -> unsupportedExpression "direct-style eval after CPS"
    GrinCpsEval runtimeRep value continuation updateContinuation -> do
      valueSlot <- freshSlot
      continuationSlot <- freshSlot
      updateSlot <- freshSlot
      storedLines <-
        materializeIntoSlots env [(value, valueSlot), (continuation, continuationSlot), (updateContinuation, updateSlot)]
      addBlock
        label
        ( prefix
            <> storedLines
            <> [ loadAt applyFunctionRegister R14 valueSlot,
                 loadAt applyContinuationRegister R14 continuationSlot,
                 loadAt RAX R14 updateSlot,
                 immediate R11 (fromEnum (isLiftedRuntimeRep runtimeRep))
               ]
        )
        (BlockLayout.Jump ".Laihc_eval")
    GrinCall _ functionName arguments -> do
      target <- liftEither (functionCodeLabel (valueCompileEnv env) functionName)
      if functionName == valueFunctionName env
        then
          if length arguments == length (valueFunctionParameters env)
            then do
              transferLines <- moveValuesToLocations env arguments (map (valueLocations env Map.!) (valueFunctionParameters env))
              addBlock label (prefix <> transferLines) (BlockLayout.Jump (valueBodyLabel env))
            else unsupportedExpression "self tail-call arity mismatch"
        else
          if functionName `Set.member` compileContinuationFunctions (valueCompileEnv env)
            then compileTransfer target arguments []
            else case reverse arguments of
              continuation : reversedValues -> do
                let values = reverse reversedValues
                continuationLines <- liftEither (materializeValueTo env applyContinuationRegister continuation)
                compileTransfer target values continuationLines
              [] -> unsupportedExpression "direct CPS call has no continuation"
    GrinPrimitiveCall {} -> unsupportedExpression "unbound primitive call after CPS"
    GrinCpsPrimitiveCall _ name arguments continuation ->
      compileCpsPrimitive env prefix label name arguments continuation
    GrinApply {} -> unsupportedExpression "direct-style apply after CPS"
    GrinCpsApply _ function arguments continuation -> do
      scratch <- freshSlot
      continuationSlot <- freshSlot
      slowLabel <- freshLabel (valueLabelPrefix env) "apply_slow"
      argumentSlots <- freshSlots (length arguments)
      storedLines <-
        materializeIntoSlots env $
          zip (function : continuation : arguments) (scratch : continuationSlot : argumentSlots)
      let stackBytes = applyStackBytes (length arguments)
          stackRestoreLines = restoreApplyStackLines stackBytes
          slowApplyLines =
            stackRestoreLines
              <> [ loadAt RSI R14 scratch,
                   immediate RDX (length arguments)
                 ]
              <> slotPointer RCX argumentSlots
              <> [ amd64Instruction (AmdLea R8 (Amd64MemoryAddress (Amd64Memory R14 (fromIntegral (continuationSlot * 8))))),
                   amd64Instruction (AmdMov RDI (Amd64MoveRegister R15)),
                   amd64Instruction (AmdCall "aihc_apply_slow"),
                   loadAt applyFunctionRegister R14 continuationSlot
                 ]
      addBlock
        label
        ( prefix
            <> storedLines
            <> [ loadAt applyFunctionRegister R14 scratch,
                 loadAt applyContinuationRegister R14 continuationSlot
               ]
            <> [loadAt register R14 slot | (register, slot) <- zip applyArgumentRegisters argumentSlots]
            <> saveApplyOverflowLines R14 argumentSlots
            <> [ amd64Instruction (AmdMov R11 (Amd64MoveMemory (Amd64Memory applyFunctionRegister 0))),
                 amd64Instruction (AmdMov R11 (Amd64MoveMemory (Amd64Memory R11 48))),
                 amd64Instruction (AmdTest (Amd64RmRegister R11) R11),
                 amd64Instruction (AmdJe slowLabel),
                 amd64Instruction (AmdJmp (Amd64JumpRegister R11)),
                 amd64Label slowLabel
               ]
            <> slowApplyLines
        )
        (BlockLayout.Jump ".Laihc_enter")
    GrinContinue continuation values -> do
      overflowLines <- liftEither (saveValueOverflowLines env values)
      continuationLines <- liftEither (materializeValueTo env applyFunctionRegister continuation)
      valueLines <- liftEither (moveValuesToRegisters env values applyArgumentRegisters)
      addBlock
        label
        ( prefix
            <> overflowLines
            <> continuationLines
            <> valueLines
        )
        (BlockLayout.Jump ".Laihc_enter")
    GrinCpsRaise exception continuation -> do
      exceptionSlot <- freshSlot
      continuationSlot <- freshSlot
      storedLines <- materializeIntoSlots env [(exception, exceptionSlot), (continuation, continuationSlot)]
      addBlock
        label
        ( prefix
            <> storedLines
            <> [ loadAt RSI R14 exceptionSlot,
                 loadAt RDX R14 continuationSlot,
                 amd64Instruction (AmdMov RDI (Amd64MoveRegister R15)),
                 amd64Instruction (AmdCall "aihc_raise")
               ]
        )
        (BlockLayout.Jump ".Laihc_resume")
    GrinHalt _ ->
      addBlock
        label
        (prefix <> [amd64Instruction (AmdMov RDI (Amd64MoveRegister R15)), amd64Instruction (AmdCall "aihc_halt"), amd64Instruction (AmdJmp (Amd64JumpRegister RAX))])
        BlockLayout.Exit
    GrinExit status -> do
      statusLines <- liftEither (materializeValueTo env RDI status)
      addBlock
        label
        ( prefix
            <> statusLines
            <> [ amd64Instruction (AmdCall "aihc_exit_process"),
                 amd64Instruction AmdUd2
               ]
        )
        BlockLayout.Exit
    GrinCase scrutinee binder alternatives ->
      compileCase env prefix label scrutinee binder alternatives
    GrinThrow {} -> unsupportedExpression "direct-style throw after CPS"
    GrinCatch {} -> unsupportedExpression "direct-style catch after CPS"
    GrinForeignCallExpr {} -> unsupportedExpression "unbound foreign call after CPS"
  where
    unsupportedExpression name = lift (Left (Amd64UnsupportedExpression name))
    compileTransfer target values extraLines = do
      overflowLines <- liftEither (saveValueOverflowLines env values)
      registerLines <- liftEither (moveValuesToRegisters env values applyArgumentRegisters)
      addBlock
        label
        (prefix <> overflowLines <> extraLines <> registerLines <> moveDirectOverflowLines R14 (length values))
        (BlockLayout.Jump target)
    compileStoreRec allocate bindings body = do
      allocationLines <- fmap concat . forM bindings $ \(var, node) -> do
        location <- liftEither (variableLocation env var)
        nodeLines <- liftEither (allocate env node)
        pure (nodeLines <> storeLocation RAX location)
      initializationLines <- fmap concat . forM bindings $ \(var, node) -> do
        location <- liftEither (variableLocation env var)
        fieldLines <- liftEither (initializeNodeFields env node)
        pure (loadLocation R13 location <> fieldLines)
      compileExpr env (prefix <> allocationLines <> initializationLines) label body

compileCpsPrimitive :: ValueEnv -> [Amd64Statement] -> Text -> Text -> [GrinValue] -> GrinValue -> FunctionM ()
compileCpsPrimitive env prefix label name arguments continuation =
  case nativeCpsPrimitiveCall name of
    Just runtimeCall
      | nativeCpsCallOperandCount runtimeCall == length arguments,
        1 + length arguments + fromEnum (nativeCpsCallPassContinuation runtimeCall) <= length foreignArgumentRegisters ->
          compileRuntimeCall runtimeCall
    _ -> unsupportedCpsPrimitive
  where
    compileRuntimeCall runtimeCall = do
      continuationSlot <- freshSlot
      argumentSlots <- freshSlots (length arguments)
      storedLines <-
        materializeIntoSlots env $
          zip (arguments <> [continuation]) (argumentSlots <> [continuationSlot])
      let (returnLines, successor) =
            case nativeCpsCallTransfer runtimeCall of
              NativeCpsEnterContinuation ->
                ([loadAt applyFunctionRegister R14 continuationSlot], BlockLayout.Jump ".Laihc_enter")
              NativeCpsResumeScheduler ->
                ([], BlockLayout.Jump ".Laihc_resume")
      addBlock
        label
        ( prefix
            <> storedLines
            <> renderCpsCallArguments runtimeCall argumentSlots continuationSlot
            <> [amd64Instruction (AmdCall (nativeCpsCallSymbol runtimeCall))]
            <> returnLines
        )
        successor

    unsupportedCpsPrimitive =
      lift (Left (Amd64UnsupportedExpression ("CPS primitive call " <> name)))

renderCpsCallArguments :: NativeCpsCall -> [Int] -> Int -> [Amd64Statement]
renderCpsCallArguments runtimeCall operandSlots continuationSlot =
  [amd64Instruction (AmdMov RDI (Amd64MoveRegister R15))]
    <> [loadAt register R14 slot | (register, slot) <- zip (drop 1 foreignArgumentRegisters) operandSlots]
    <> [ loadAt (foreignArgumentRegisters !! (length operandSlots + 1)) R14 continuationSlot
       | nativeCpsCallPassContinuation runtimeCall
       ]

compileDirectBinding :: ValueEnv -> [GrinVar] -> GrinExpr -> FunctionM [Amd64Statement]
compileDirectBinding env vars expression =
  case expression of
    GrinConstant values
      | length vars == length values ->
          fmap concat . forM (zip vars values) $ \(var, value) -> do
            location <- liftEither (variableLocation env var)
            valueLines <- liftEither (materializeValue env value)
            pure (valueLines <> storeLocation RAX location)
    GrinStore node -> liftEither (materializeNode env node) >>= storeSingleResult
    GrinEnsureHeap requiredWords roots
      | length vars == length roots -> do
          (argumentLines, argumentSlots) <- materializeIntoFreshSlots env (requiredWords : roots)
          case argumentSlots of
            requiredSlot : rootSlots -> do
              resultLines <-
                fmap concat . forM (zip vars rootSlots) $ \(var, slot) -> do
                  location <- liftEither (variableLocation env var)
                  pure ([loadAt R11 R14 slot] <> storeLocation R11 location)
              pure
                ( argumentLines
                    <> [ amd64Instruction (AmdMov RDI (Amd64MoveRegister R15)),
                         loadAt RSI R14 requiredSlot,
                         immediate RDX (length roots)
                       ]
                    <> slotPointer RCX rootSlots
                    <> [ amd64Instruction (AmdCall "aihc_ensure_heap")
                       ]
                    <> resultLines
                )
            [] -> lift (Left (Amd64UnsupportedExpression "heap reservation size"))
      | otherwise -> lift (Left (Amd64UnsupportedExpression "heap reservation result arity"))
    GrinStoreUnchecked node -> liftEither (materializeNodeUnchecked env node) >>= storeSingleResult
    GrinUpdate pointer value -> compileUpdateBinding False "aihc_update" pointer value
    GrinUpdateBlackhole pointer value -> compileUpdateBinding True "aihc_update_blackhole" pointer value
    GrinPrimitiveCall _ name [left, right]
      | Just instructions <- lookup name singleResultBinaryPrimitives ->
          compileBinary R10 RAX storeSingleResult instructions left right
      | Just (firstRegister, secondRegister, instructions) <- lookup name pairResultBinaryPrimitives ->
          compileBinary R10 RAX (storeTwoResults firstRegister secondRegister) instructions left right
      | Just instructions <- lookup name singleResultDividendPrimitives ->
          compileBinary RAX R10 storeSingleResult instructions left right
      | Just (firstRegister, secondRegister, instructions) <- lookup name pairResultDividendPrimitives ->
          compileBinary RAX R10 (storeTwoResults firstRegister secondRegister) instructions left right
    GrinPrimitiveCall _ "quotRemWord2#" arguments@[_, _, _] -> do
      (argumentLines, argumentSlots) <- materializeIntoFreshSlots env arguments
      case argumentSlots of
        [highSlot, lowSlot, divisorSlot] ->
          storeTwoResults
            R10
            R11
            ( argumentLines
                <> [ loadAt RDX R14 highSlot,
                     loadAt RAX R14 lowSlot,
                     loadAt R10 R14 divisorSlot,
                     amd64Instruction (AmdDiv (Amd64RmRegister R10)),
                     amd64Instruction (AmdMov R10 (Amd64MoveRegister RAX)),
                     amd64Instruction (AmdMov R11 (Amd64MoveRegister RDX))
                   ]
            )
        _ -> lift (Left (Amd64UnsupportedExpression "internal quotRemWord2# arity"))
    GrinPrimitiveCall _ name [value, amount]
      | Just opcode <- lookup name [("uncheckedShiftL#", AmdShl), ("uncheckedShiftRL#", AmdShr)] -> do
          savedCountRegister <- freshSlot
          valueLines <- liftEither (materializeValueTo env R10 value)
          amountLines <- liftEither (materializeValue env amount)
          storeSingleResult
            ( [storeAt RCX R14 savedCountRegister]
                <> valueLines
                <> amountLines
                <> [amd64Instruction (AmdMov RCX (Amd64MoveRegister RAX)), amd64Instruction (opcode (Amd64RmRegister R10)), amd64Instruction (AmdMov RAX (Amd64MoveRegister R10)), loadAt RCX R14 savedCountRegister]
            )
    GrinPrimitiveCall _ "nullAddr#" [] ->
      storeSingleResult [amd64Instruction (AmdXor (Amd64RmRegister RAX) (Amd64BinaryRegister RAX))]
    GrinPrimitiveCall runtimeRep name arguments
      | name == "realWorld#",
        null arguments,
        null vars,
        null (runtimeRepComponents runtimeRep) ->
          pure []
    GrinPrimitiveCall _ name [value]
      | Just instructions <- lookup name unaryPrimitives -> do
          valueLines <- liftEither (materializeValue env value)
          storeSingleResult (valueLines <> instructions)
    GrinPrimitiveCall _ "casMutVar#" [reference, expected, replacement]
      | Just swapCall <- nativeRuntimePrimitiveCall "casMutVar#",
        Just readCall <- nativeRuntimePrimitiveCall "readMutVar#",
        [flag, current] <- vars -> do
          swapLines <- compileRuntimeCallLines env swapCall [reference, expected, replacement]
          flagLocation <- liftEither (variableLocation env flag)
          readLines <- compileRuntimeCallLines env readCall [reference]
          currentLocation <- liftEither (variableLocation env current)
          pure (swapLines <> storeLocation RAX flagLocation <> readLines <> storeLocation RAX currentLocation)
    GrinPrimitiveCall _ name arguments
      | Just splitCalls <- nativeSplitRuntimePrimitiveCall name,
        length splitCalls == length vars ->
          concat
            <$> mapM
              ( \(var, splitCall) -> do
                  callLines <- compileRuntimeCallLines env splitCall arguments
                  location <- liftEither (variableLocation env var)
                  pure (callLines <> storeLocation RAX location)
              )
              (zip vars splitCalls)
    GrinPrimitiveCall _ name arguments
      | Just runtimeCall <- nativeRuntimePrimitiveCall name -> do
          callLines <- compileRuntimeCallLines env runtimeCall arguments
          case nativeRuntimeCallResultCount runtimeCall of
            0 | null vars -> pure callLines
            1 -> storeSingleResult callLines
            _ -> lift (Left (Amd64UnsupportedExpression ("runtime primitive result arity " <> name)))
      | compileAllowUnsupportedPrimitives (valueCompileEnv env) ->
          pure [amd64Instruction (AmdCall "aihc_unsupported_primitive")]
      | otherwise -> lift (Left (Amd64UnsupportedExpression ("primitive call " <> name)))
    GrinForeignCallExpr foreignCall arguments ->
      compileForeignCallLines env foreignCall arguments >>= storeSingleResult
    _ -> lift (Left (Amd64UnsupportedExpression "non-direct expression remained in a CPS bind"))
  where
    storeSingleResult lines' =
      case vars of
        [var] -> do
          location <- liftEither (variableLocation env var)
          pure (lines' <> storeLocation RAX location)
        _ -> lift (Left (Amd64UnsupportedExpression "direct expression result arity"))
    storeTwoResults firstRegister secondRegister lines' =
      case vars of
        [first, second] -> do
          firstLocation <- liftEither (variableLocation env first)
          secondLocation <- liftEither (variableLocation env second)
          pure (lines' <> storeLocation firstRegister firstLocation <> storeLocation secondRegister secondLocation)
        _ -> lift (Left (Amd64UnsupportedExpression "direct expression pair result arity"))
    compileBinary leftRegister rightRegister store instructions left right = do
      leftLines <- liftEither (materializeValueTo env leftRegister left)
      rightLines <- liftEither (materializeValueTo env rightRegister right)
      store (leftLines <> rightLines <> instructions)
    compileUpdateBinding passMachine symbol pointer value = do
      pointerSlot <- freshSlot
      valueSlot <- freshSlot
      storedLines <- materializeIntoSlots env [(pointer, pointerSlot), (value, valueSlot)]
      resultLines <- storeSingleResult [loadAt RAX R14 valueSlot]
      pure
        ( storedLines
            <> [ loadAt (if passMachine then RSI else RDI) R14 pointerSlot,
                 loadAt (if passMachine then RDX else RSI) R14 valueSlot
               ]
            <> [amd64Instruction (AmdMov RDI (Amd64MoveRegister R15)) | passMachine]
            <> [amd64Instruction (AmdCall symbol)]
            <> resultLines
        )

    singleResultBinaryPrimitives =
      concat
        [ binary AmdAdd ["+#", "plusWord#"],
          binary AmdSub ["-#", "minusWord#"],
          binary (\_ _ -> AmdImul R10 (Amd64RmRegister RAX)) ["*#", "timesWord#"],
          binary AmdAnd ["and#"],
          binary AmdOr ["or#"],
          binary AmdXor ["xor#"],
          comparison AmdEqual ["==#", "eqWord#", "eqWord64#"],
          comparison AmdLess ["<#"],
          comparison AmdGreater [">#"],
          comparison AmdGreaterOrEqual [">=#"],
          comparison AmdLessOrEqual ["<=#"],
          comparison AmdNotEqual ["/=#", "neWord#", "neWord64#"],
          comparison AmdBelow ["ltWord#", "ltWord64#"],
          comparison AmdBelowOrEqual ["leWord#", "leWord64#"],
          comparison AmdAbove ["gtWord#", "gtWord64#"],
          comparison AmdAboveOrEqual ["geWord#", "geWord64#"]
        ]
        <> [ ("compareInt#", [amd64Instruction (AmdCmp (Amd64RmRegister R10) (Amd64BinaryRegister RAX)), amd64Instruction (AmdSet AmdGreater (Amd64RmRegister AL)), amd64Instruction (AmdSet AmdLess (Amd64RmRegister R10B)), amd64Instruction (AmdMovzx RAX (Amd64RmRegister AL)), amd64Instruction (AmdMovzx R10 (Amd64RmRegister R10B)), amd64Instruction (AmdSub (Amd64RmRegister RAX) (Amd64BinaryRegister R10))])
           ]
    pairResultBinaryPrimitives =
      [ carry "addIntC#" AmdAdd AmdOverflow,
        carry "subIntC#" AmdSub AmdOverflow,
        carry "addWordC#" AmdAdd AmdCarry,
        carry "subWordC#" AmdSub AmdCarry
      ]
    singleResultDividendPrimitives =
      [ ("quotWord#", [amd64Instruction (AmdXor (Amd64RmRegister RDX) (Amd64BinaryRegister RDX)), amd64Instruction (AmdDiv (Amd64RmRegister R10))]),
        ("remWord#", [amd64Instruction (AmdXor (Amd64RmRegister RDX) (Amd64BinaryRegister RDX)), amd64Instruction (AmdDiv (Amd64RmRegister R10)), amd64Instruction (AmdMov RAX (Amd64MoveRegister RDX))])
      ]
    pairResultDividendPrimitives =
      [ ("timesWord2#", (RDX, RAX, [amd64Instruction (AmdMul (Amd64RmRegister R10))])),
        ("quotRemWord#", (R10, R11, [amd64Instruction (AmdXor (Amd64RmRegister RDX) (Amd64BinaryRegister RDX)), amd64Instruction (AmdDiv (Amd64RmRegister R10)), amd64Instruction (AmdMov R10 (Amd64MoveRegister RAX)), amd64Instruction (AmdMov R11 (Amd64MoveRegister RDX))]))
      ]
    unaryPrimitives =
      ("not#", [amd64Instruction (AmdNot (Amd64RmRegister RAX))])
        : [ (name, [])
          | name <- ["int2Word#", "word2Int#", "word8ToWord#", "word32ToWord#", "word64ToWord#", "wordToWord64#", "word16ToWord#", "ord#", "chr#", "unsafeFreezeArray#", "unsafeThawArray#", "unsafeFreezeByteArray#", "unsafeThawByteArray#", "castFloatToWord32#", "castWord32ToFloat#", "castDoubleToWord64#", "castWord64ToDouble#"]
          ]
    binary opcode names =
      [(name, [amd64Instruction (opcode (Amd64RmRegister R10) (Amd64BinaryRegister RAX)), amd64Instruction (AmdMov RAX (Amd64MoveRegister R10))]) | name <- names]
    comparison condition names =
      [(name, [amd64Instruction (AmdCmp (Amd64RmRegister R10) (Amd64BinaryRegister RAX)), amd64Instruction (AmdSet condition (Amd64RmRegister AL)), amd64Instruction (AmdMovzx RAX (Amd64RmRegister AL))]) | name <- names]
    carry name opcode condition =
      ( name,
        ( R10,
          R11,
          [amd64Instruction (opcode (Amd64RmRegister R10) (Amd64BinaryRegister RAX)), amd64Instruction (AmdSet condition (Amd64RmRegister R11B)), amd64Instruction (AmdMovzx R11 (Amd64RmRegister R11B))]
        )
      )

compileForeignCallLines :: ValueEnv -> GrinForeignCall -> [GrinValue] -> FunctionM [Amd64Statement]
compileForeignCallLines env = compileCallLines env False

compileRuntimeCallLines :: ValueEnv -> NativeRuntimeCall -> [GrinValue] -> FunctionM [Amd64Statement]
compileRuntimeCallLines env runtimeCall =
  compileCallLines env (nativeRuntimeCallPassMachine runtimeCall) (nativeRuntimeCallForeignCall runtimeCall)

compileCallLines :: ValueEnv -> Bool -> GrinForeignCall -> [GrinValue] -> FunctionM [Amd64Statement]
compileCallLines _ _ foreignCall arguments
  -- An address import materializes the symbol address instead of calling it.
  | GrinForeignAddress <- grinForeignCallTarget foreignCall =
      if null arguments
        then pure [address RAX (grinForeignCallSymbol foreignCall)]
        else lift (Left (Amd64UnsupportedExpression "address foreign import with arguments"))
compileCallLines env passMachine foreignCall arguments = do
  let signature = grinForeignCallSignature foreignCall
      operandArity = length (grinForeignArgumentTypes signature)
      abiArity = operandArity + fromEnum passMachine
      expectedArity = length (grinForeignOperandReps signature)
  if length arguments /= expectedArity
    then lift (Left (Amd64UnsupportedExpression "foreign call arity mismatch"))
    else
      if abiArity > length foreignArgumentRegisters
        then lift (Left (Amd64UnsupportedExpression "foreign calls with more than six arguments"))
        else do
          (argumentLines, argumentSlots) <- materializeIntoFreshSlots env arguments
          let argumentRegisters = drop (fromEnum passMachine) foreignArgumentRegisters
              loadAbiArguments =
                [ loadAt register R14 slot
                | (register, slot) <- zip argumentRegisters argumentSlots
                ]
              callLines =
                argumentLines
                  <> [amd64Instruction (AmdMov RDI (Amd64MoveRegister R15)) | passMachine]
                  <> loadAbiArguments
                  <> [amd64Instruction (AmdCall (grinForeignCallSymbol foreignCall))]
                  <> normalizeForeignResult (grinForeignResultType signature)
          pure callLines

materializeIntoFreshSlots :: ValueEnv -> [GrinValue] -> FunctionM ([Amd64Statement], [Int])
materializeIntoFreshSlots env values = do
  slots <- freshSlots (length values)
  lines' <- materializeIntoSlots env (zip values slots)
  pure (lines', slots)

materializeIntoSlots :: ValueEnv -> [(GrinValue, Int)] -> FunctionM [Amd64Statement]
materializeIntoSlots env = fmap concat . mapM store
  where
    store (value, slot) = do
      lines' <- liftEither (materializeValue env value)
      pure (lines' <> [storeAt RAX R14 slot])

normalizeForeignResult :: GrinForeignType -> [Amd64Statement]
normalizeForeignResult foreignType =
  case foreignType of
    GrinForeignInt -> []
    GrinForeignInt32 -> [amd64Instruction (AmdMovsxd RAX (Amd64RmRegister EAX))]
    GrinForeignWord64 -> []
    GrinForeignAddr -> []

foreignArgumentRegisters :: [Amd64Register]
foreignArgumentRegisters = [RDI, RSI, RDX, RCX, R8, R9]

compileCase :: ValueEnv -> [Amd64Statement] -> Text -> GrinValue -> GrinVar -> [GrinAlt] -> FunctionM ()
compileCase env prefix label scrutinee binder alternatives = do
  (resultLocation, scrutineeLines) <- case scrutinee of
    GrinVarValue var | Just location <- Map.lookup var (valueLocations env) -> pure (location, [])
    _ -> materializedScrutinee
  binderLocation <- liftEither (variableLocation env binder)
  let scrutineeIsPointer = isPointerRuntimeRep (grinValueRuntimeRep scrutinee)
  alternativeTargets <- forM alternatives $ \alternative -> do
    alternativeLabel <- freshLabel label "case_alt"
    pure (alternative, alternativeLabel)
  (checks, successor) <- caseChecks resultLocation scrutineeIsPointer alternativeTargets
  addBlock
    label
    ( prefix
        <> scrutineeLines
        <> checks
    )
    successor
  forM_ alternativeTargets $ \(alternative, alternativeLabel) -> do
    let rhs = grinAltRhs alternative
        binderLines =
          if binder `Set.member` grinExprFreeVariables rhs
            then loadLocation R11 resultLocation <> storeLocation R11 binderLocation
            else []
    prefixLines <- alternativePrefix env resultLocation alternative
    compileExpr env (binderLines <> prefixLines) alternativeLabel rhs
  where
    materializedScrutinee = do
      slot <- freshSlot
      lines' <- liftEither (materializeValue env scrutinee)
      pure (InHeapSpill slot, lines' <> [storeAt RAX R14 slot])

alternativePrefix :: ValueEnv -> Location Amd64Register -> GrinAlt -> FunctionM [Amd64Statement]
alternativePrefix env resultLocation alternative =
  case grinAltCon alternative of
    GrinDataAlt _ -> do
      fields <- fmap concat . forM liveIndexedBinders $ \(index, binder) -> do
        location <- liftEither (variableLocation env binder)
        pure ([loadByteOffset R10 R11 (8 + index * 8)] <> storeLocation R10 location)
      pure (if null fields then [] else loadLocation R11 resultLocation <> fields)
    GrinLitAlt _ -> pure []
    GrinDefaultAlt ->
      fmap concat . forM (filter isLive (grinAltBinders alternative)) $ \binder -> do
        location <- liftEither (variableLocation env binder)
        pure (loadLocation R11 resultLocation <> storeLocation R11 location)
  where
    isLive binder = binder `Set.member` grinExprFreeVariables (grinAltRhs alternative)
    liveIndexedBinders = filter (isLive . snd) (zip [0 ..] (grinAltBinders alternative))

caseChecks :: Location Amd64Register -> Bool -> [(GrinAlt, Text)] -> FunctionM ([Amd64Statement], BlockLayout.Terminator Text)
caseChecks resultLocation scrutineeIsPointer targets = do
  let nonDefault = [(alternative, label) | (alternative, label) <- targets, grinAltCon alternative /= GrinDefaultAlt]
      defaultTarget = [label | (alternative, label) <- targets, grinAltCon alternative == GrinDefaultAlt]
  checks <- fmap concat . forM nonDefault $ \(alternative, target) ->
    case grinAltCon alternative of
      GrinDataAlt _
        | not scrutineeIsPointer ->
            lift (Left (Amd64UnsupportedExpression "constructor case on an unboxed value"))
      GrinDataAlt name -> do
        let identity = constructorStageLabel name 0
        pure $
          loadLocation R11 resultLocation
            <> [ loadByteOffset R10 R11 0,
                 loadByteOffset R10 R10 0,
                 address R11 identity,
                 amd64Instruction (AmdCmp (Amd64RmRegister R10) (Amd64BinaryRegister R11)),
                 amd64Instruction (AmdJe target)
               ]
      GrinLitAlt _
        | scrutineeIsPointer ->
            lift (Left (Amd64UnsupportedExpression "literal case on a lifted value"))
      GrinLitAlt literal ->
        case normalizedLiteralInteger literal of
          Just integer ->
            pure $
              loadLocation R10 resultLocation
                <> [immediate R11 integer, amd64Instruction (AmdCmp (Amd64RmRegister R10) (Amd64BinaryRegister R11)), amd64Instruction (AmdJe target)]
          Nothing -> lift (Left (Amd64UnsupportedValue "string case alternative"))
      GrinDefaultAlt -> pure []
  pure $ case defaultTarget of
    target : _ -> (checks, BlockLayout.Jump target)
    [] -> (checks <> [amd64Instruction (AmdCall "aihc_no_match"), amd64Instruction AmdUd2], BlockLayout.Exit)

moveEntryParameters :: [(Amd64Register, Location Amd64Register)] -> [Amd64Statement]
moveEntryParameters pairs =
  spillMoves <> renderRegisterMovesWithoutValues registerMoves
  where
    spillMoves =
      concat
        [ storeLocation source destination
        | (source, destination@InHeapSpill {}) <- pairs
        ]
    registerMoves =
      [ (destination, MoveRegister source)
      | (source, InRegister destination) <- pairs,
        source /= destination
      ]

moveValuesToRegisters :: ValueEnv -> [GrinValue] -> [Amd64Register] -> Either Amd64Error [Amd64Statement]
moveValuesToRegisters env values registers =
  renderRegisterMoves
    env
    [ (destination, moveSource env value)
    | (value, destination) <- zip values registers,
      moveSource env value /= MoveRegister destination
    ]

moveValuesToLocations :: ValueEnv -> [GrinValue] -> [Location Amd64Register] -> FunctionM [Amd64Statement]
moveValuesToLocations env values destinations
  | and (zipWith alreadyThere values destinations) = pure []
  | otherwise = do
      (stores, slots) <- materializeIntoFreshSlots env values
      let loads =
            concat
              [ [loadAt R11 R14 slot] <> storeLocation R11 destination
              | (slot, destination) <- zip slots destinations
              ]
      pure (stores <> loads)
  where
    alreadyThere value destination =
      case value of
        GrinVarValue var -> Map.lookup var (valueLocations env) == Just destination
        GrinGlobalValue {} -> False
        GrinLitValue {} -> False

moveSource :: ValueEnv -> GrinValue -> MoveSource
moveSource env value =
  case value of
    GrinVarValue var ->
      case Map.lookup var (valueLocations env) of
        Just (InRegister register) -> MoveRegister register
        Just (InHeapSpill slot) -> MoveSpill slot
        Nothing -> MoveValue value
    GrinGlobalValue {} -> MoveValue value
    GrinLitValue {} -> MoveValue value

renderRegisterMoves :: ValueEnv -> [(Amd64Register, MoveSource)] -> Either Amd64Error [Amd64Statement]
renderRegisterMoves env = renderRegisterMovesWith emitMove
  where
    emitMove destination source =
      case source of
        MoveRegister register -> pure [amd64Instruction (AmdMov destination (Amd64MoveRegister register))]
        MoveSpill slot -> pure [loadAt destination R14 slot]
        MoveValue value -> materializeValueTo env destination value

renderRegisterMovesWithoutValues :: [(Amd64Register, MoveSource)] -> [Amd64Statement]
renderRegisterMovesWithoutValues = fromRight [] . renderRegisterMovesWith emitMove
  where
    emitMove destination source =
      case source of
        MoveRegister register -> pure [amd64Instruction (AmdMov destination (Amd64MoveRegister register))]
        MoveSpill slot -> pure [loadAt destination R14 slot]
        MoveValue {} -> Left (Amd64UnsupportedExpression "value in entry register transfer")

renderRegisterMovesWith :: (Amd64Register -> MoveSource -> Either Amd64Error [Amd64Statement]) -> [(Amd64Register, MoveSource)] -> Either Amd64Error [Amd64Statement]
renderRegisterMovesWith emitMove = go
  where
    go [] = pure []
    go moves =
      case takeSafeMove moves of
        Just ((destination, source), remaining) -> do
          line <- emitMove destination source
          rest <- go remaining
          pure (line <> rest)
        Nothing ->
          case [source | (_, MoveRegister source) <- moves] of
            source : _ -> do
              rest <- go (map (replaceSource source R10) moves)
              pure ([amd64Instruction (AmdMov R10 (Amd64MoveRegister source))] <> rest)
            [] -> Left (Amd64UnsupportedExpression "unresolvable register transfer")

takeSafeMove :: [(Amd64Register, MoveSource)] -> Maybe ((Amd64Register, MoveSource), [(Amd64Register, MoveSource)])
takeSafeMove moves = select [] moves
  where
    sourceRegisters = Set.fromList [register | (_, MoveRegister register) <- moves]
    select _ [] = Nothing
    select previous (move@(destination, _) : rest)
      | destination `Set.notMember` sourceRegisters = Just (move, reverse previous <> rest)
      | otherwise = select (move : previous) rest

replaceSource :: Amd64Register -> Amd64Register -> (Amd64Register, MoveSource) -> (Amd64Register, MoveSource)
replaceSource old new (destination, source) =
  ( destination,
    case source of
      MoveRegister register | register == old -> MoveRegister new
      _ -> source
  )

saveValueOverflowLines :: ValueEnv -> [GrinValue] -> Either Amd64Error [Amd64Statement]
saveValueOverflowLines env values
  | stackBytes == 0 = pure []
  | otherwise = do
      stores <-
        fmap concat . forM (zip [0 :: Int ..] (drop (length applyArgumentRegisters) values)) $ \(index, value) -> do
          lines' <- materializeValueTo env R11 value
          pure (lines' <> [storeByteOffset R11 RSP (index * 8)])
      pure ([amd64Instruction (AmdSub (Amd64RmRegister RSP) (Amd64BinaryImmediate (fromIntegral stackBytes)))] <> stores)
  where
    stackBytes = applyStackBytes (length values)

saveApplyOverflowLines :: Amd64Register -> [Int] -> [Amd64Statement]
saveApplyOverflowLines base slots
  | stackBytes == 0 = []
  | otherwise =
      [amd64Instruction (AmdSub (Amd64RmRegister RSP) (Amd64BinaryImmediate (fromIntegral stackBytes)))]
        <> concat
          [ [loadAt R11 base slot, storeByteOffset R11 RSP (index * 8)]
          | (index, slot) <- zip [0 :: Int ..] (drop (length applyArgumentRegisters) slots)
          ]
  where
    stackBytes = applyStackBytes (length slots)

moveDirectOverflowLines :: Amd64Register -> Int -> [Amd64Statement]
moveDirectOverflowLines base valueCount
  | stackBytes == 0 = []
  | otherwise =
      concat
        [ [ loadByteOffset R11 RSP ((targetIndex - length applyArgumentRegisters) * 8),
            storeAt R11 base targetIndex
          ]
        | targetIndex <- [length applyArgumentRegisters .. valueCount - 1]
        ]
        <> restoreApplyStackLines stackBytes
  where
    stackBytes = applyStackBytes valueCount

variableLocation :: ValueEnv -> GrinVar -> Either Amd64Error (Location Amd64Register)
variableLocation env var =
  maybe
    (Left (Amd64UnsupportedExpression ("missing location for " <> grinVarName var)))
    Right
    (Map.lookup var (valueLocations env))

freshSlot :: FunctionM Int
freshSlot = do
  state <- get
  let slot = functionNextSlot state
  modify' $ \current -> current {functionNextSlot = slot + 1}
  pure slot

freshSlots :: Int -> FunctionM [Int]
freshSlots count = replicateM count freshSlot

freshLabel :: Text -> Text -> FunctionM Text
freshLabel parent kind = do
  state <- get
  let identifier = functionNextLabel state
  modify' $ \current -> current {functionNextLabel = identifier + 1}
  pure (parent <> "_" <> kind <> "_" <> tshow identifier)

addBlock :: Text -> [Amd64Statement] -> BlockLayout.Terminator Text -> FunctionM ()
addBlock label instructions terminator =
  modify' $ \state ->
    state
      { functionBlocksRev =
          BlockLayout.Block label instructions terminator : functionBlocksRev state
      }

slotPointer :: Amd64Register -> [Int] -> [Amd64Statement]
slotPointer register slots =
  case slots of
    first : _ -> [amd64Instruction (AmdLea register (Amd64MemoryAddress (Amd64Memory R14 (fromIntegral (first * 8)))))]
    [] -> [amd64Instruction (AmdXor (Amd64RmRegister register) (Amd64BinaryRegister register))]

liftEither :: Either Amd64Error value -> FunctionM value
liftEither = lift
