{-# LANGUAGE OverloadedStrings #-}

-- | Lower individual CPS GRIN functions to AArch64 basic blocks.
module Aihc.Arm64.Codegen.Function
  ( compileFunction,
    reserveLocalsLines,
  )
where

import Aihc.Arm64.Assemble
  ( Arm64Address (..),
    Arm64Condition (..),
    Arm64Instruction (..),
    Arm64Register (..),
    Arm64Shift (..),
    Arm64Statement,
    Arm64Value (..),
    arm64Align,
    arm64Global,
    arm64Instruction,
    arm64Label,
  )
import Aihc.Arm64.Codegen.Runtime
import Aihc.Arm64.RegisterAllocate qualified as RegisterAllocate
import Aihc.Grin.Syntax
import Aihc.Native
  ( NativeCpsCall (..),
    NativeCpsTransfer (..),
    NativeRuntimeCall (..),
    nativeCpsPrimitiveCall,
    nativeRuntimePrimitiveCall,
    nativeSplitRuntimePrimitiveCall,
  )
import Aihc.Native.BlockLayout qualified as BlockLayout
import Aihc.Native.RegisterAllocate (Location (..), grinExprFreeVariables)
import Control.Monad (forM, forM_, replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (execStateT, get, modify')
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)

compileFunction :: CompileEnv -> GrinFunction -> Either Arm64Error CompiledFunction
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
      valueEnv = ValueEnv env locations label (grinFunctionName function) (grinFunctionParameters function) bodyLabel
  finalState <- execStateT (compileExpr valueEnv [] bodyLabel (grinFunctionBody function)) initialState
  let slotCount = max 1 (functionNextSlot finalState)
      parameterRegisterPairs =
        zip (take valueParameterCount parameters) applyArgumentRegisters
          <> [ (parameters !! valueParameterCount, applyContinuationRegister)
             | parameterCount > 0 && not isContinuation
             ]
      registerParameterCopies =
        concat
          [ storeLocation source location
          | (parameter, source) <- parameterRegisterPairs,
            Just location <- [Map.lookup parameter locations]
          ]
      entry =
        exportLines env function label
          <> [ arm64Align 3,
               arm64Label label
             ]
          <> storeCurrentSrt (Map.lookup (grinFunctionName function) (compileSrtLabels env))
          <> registerParameterCopies
      blocks =
        BlockLayout.renderBlocks
          arm64Label
          (arm64Instruction . ArmB)
          (BlockLayout.layoutBlocks bodyLabel (reverse (functionBlocksRev finalState)))
  pure
    CompiledFunction
      { compiledFunctionSlots = slotCount,
        compiledFunctionLines = entry <> blocks
      }

reserveLocalsLines :: [CompiledFunction] -> [Arm64Statement]
reserveLocalsLines functions =
  [ immediate X1 maximumSlots,
    arm64Instruction (ArmMov X0 (Arm64RegisterValue X22)),
    arm64Instruction (ArmBl "_aihc_alloc_locals"),
    arm64Instruction (ArmMov X19 (Arm64RegisterValue X0))
  ]
  where
    maximumSlots = maximum (2 : map compiledFunctionSlots functions)

exportLines :: CompileEnv -> GrinFunction -> Text -> [Arm64Statement]
exportLines env _function label
  | compileExposeAllFunctions env = [arm64Global label]
  | otherwise = []

compileExpr :: ValueEnv -> [Arm64Statement] -> Text -> GrinExpr -> FunctionM ()
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
            <> [ loadAt applyFunctionRegister X19 valueSlot,
                 loadAt applyContinuationRegister X19 continuationSlot,
                 loadAt X0 X19 updateSlot,
                 immediate X8 (fromEnum (isLiftedRuntimeRep runtimeRep))
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
              <> [ loadAt X1 X19 scratch,
                   immediate X2 (length arguments)
                 ]
              <> slotPointer X3 argumentSlots
              <> [ immediate X4 (continuationSlot * 8),
                   arm64Instruction (ArmAdd X4 X19 (Arm64RegisterValue X4)),
                   arm64Instruction (ArmMov X0 (Arm64RegisterValue X22)),
                   arm64Instruction (ArmBl "_aihc_apply_slow"),
                   loadAt applyFunctionRegister X19 continuationSlot
                 ]
      addBlock
        label
        ( prefix
            <> storedLines
            <> [ loadAt applyFunctionRegister X19 scratch,
                 loadAt applyContinuationRegister X19 continuationSlot
               ]
            <> [loadAt register X19 slot | (register, slot) <- zip applyArgumentRegisters argumentSlots]
            <> saveApplyOverflowLines X19 argumentSlots
            <> [ arm64Instruction (ArmLdr X8 (Arm64Offset applyFunctionRegister 0)),
                 arm64Instruction (ArmLdr X8 (Arm64Offset X8 48)),
                 arm64Instruction (ArmCbz X8 slowLabel),
                 arm64Instruction (ArmBr X8),
                 arm64Label slowLabel
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
            <> [ loadAt X1 X19 exceptionSlot,
                 loadAt X2 X19 continuationSlot,
                 arm64Instruction (ArmMov X0 (Arm64RegisterValue X22)),
                 arm64Instruction (ArmBl "_aihc_raise")
               ]
        )
        (BlockLayout.Jump ".Laihc_resume")
    GrinHalt _ ->
      addBlock
        label
        (prefix <> [arm64Instruction (ArmMov X0 (Arm64RegisterValue X22)), arm64Instruction (ArmBl "_aihc_halt"), arm64Instruction (ArmBr X0)])
        BlockLayout.Exit
    GrinExit status -> do
      statusLines <- liftEither (materializeValueTo env X0 status)
      addBlock
        label
        ( prefix
            <> statusLines
            <> [ arm64Instruction (ArmBl "_aihc_exit_process"),
                 arm64Instruction (ArmBrk 0)
               ]
        )
        BlockLayout.Exit
    GrinCase scrutinee binder alternatives ->
      compileCase env prefix label scrutinee binder alternatives
    GrinThrow {} -> unsupportedExpression "direct-style throw after CPS"
    GrinCatch {} -> unsupportedExpression "direct-style catch after CPS"
    GrinForeignCallExpr {} -> unsupportedExpression "unbound foreign call after CPS"
  where
    unsupportedExpression name = lift (Left (Arm64UnsupportedExpression name))
    compileTransfer target values extraLines = do
      overflowLines <- liftEither (saveValueOverflowLines env values)
      registerLines <- liftEither (moveValuesToRegisters env values applyArgumentRegisters)
      addBlock
        label
        (prefix <> overflowLines <> extraLines <> registerLines <> moveDirectOverflowLines X19 (length values))
        (BlockLayout.Jump target)
    compileStoreRec allocate bindings body = do
      allocationLines <- fmap concat . forM bindings $ \(var, node) -> do
        location <- liftEither (variableLocation env var)
        nodeLines <- liftEither (allocate env node)
        pure (nodeLines <> storeLocation X0 location)
      initializationLines <- fmap concat . forM bindings $ \(var, node) -> do
        location <- liftEither (variableLocation env var)
        fieldLines <- liftEither (initializeNodeFields env node)
        pure (loadLocation X20 location <> fieldLines)
      compileExpr env (prefix <> allocationLines <> initializationLines) label body

compileCpsPrimitive :: ValueEnv -> [Arm64Statement] -> Text -> Text -> [GrinValue] -> GrinValue -> FunctionM ()
compileCpsPrimitive env prefix label name arguments continuation =
  case nativeCpsPrimitiveCall name of
    Just runtimeCall
      | nativeCpsCallOperandCount runtimeCall == length arguments,
        1 + length arguments + fromEnum (nativeCpsCallPassContinuation runtimeCall) <= length applyArgumentRegisters ->
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
                ([loadAt applyFunctionRegister X19 continuationSlot], BlockLayout.Jump ".Laihc_enter")
              NativeCpsResumeScheduler ->
                ([], BlockLayout.Jump ".Laihc_resume")
      addBlock
        label
        ( prefix
            <> storedLines
            <> renderCpsCallArguments runtimeCall argumentSlots continuationSlot
            <> [arm64Instruction (ArmBl ("_" <> nativeCpsCallSymbol runtimeCall))]
            <> returnLines
        )
        successor

    unsupportedCpsPrimitive =
      lift (Left (Arm64UnsupportedExpression ("CPS primitive call " <> name)))

renderCpsCallArguments :: NativeCpsCall -> [Int] -> Int -> [Arm64Statement]
renderCpsCallArguments runtimeCall operandSlots continuationSlot =
  [arm64Instruction (ArmMov X0 (Arm64RegisterValue X22))]
    <> [loadAt register X19 slot | (register, slot) <- zip (drop 1 applyArgumentRegisters) operandSlots]
    <> [ loadAt (applyArgumentRegisters !! (length operandSlots + 1)) X19 continuationSlot
       | nativeCpsCallPassContinuation runtimeCall
       ]

compileDirectBinding :: ValueEnv -> [GrinVar] -> GrinExpr -> FunctionM [Arm64Statement]
compileDirectBinding env vars expression =
  case expression of
    GrinConstant values
      | length vars == length values ->
          fmap concat . forM (zip vars values) $ \(var, value) -> do
            location <- liftEither (variableLocation env var)
            valueLines <- liftEither (materializeValue env value)
            pure (valueLines <> storeLocation X0 location)
    GrinStore node -> liftEither (materializeNode env node) >>= storeSingleResult
    GrinEnsureHeap requiredWords roots
      | length vars == length roots -> do
          (argumentLines, argumentSlots) <- materializeIntoFreshSlots env (requiredWords : roots)
          case argumentSlots of
            requiredSlot : rootSlots -> do
              resultLines <-
                fmap concat . forM (zip vars rootSlots) $ \(var, slot) -> do
                  location <- liftEither (variableLocation env var)
                  pure ([loadAt X9 X19 slot] <> storeLocation X9 location)
              pure
                ( argumentLines
                    <> [ arm64Instruction (ArmMov X0 (Arm64RegisterValue X22)),
                         loadAt X1 X19 requiredSlot,
                         immediate X2 (length roots)
                       ]
                    <> slotPointer X3 rootSlots
                    <> [ arm64Instruction (ArmBl "_aihc_ensure_heap")
                       ]
                    <> resultLines
                )
            [] -> lift (Left (Arm64UnsupportedExpression "heap reservation size"))
      | otherwise -> lift (Left (Arm64UnsupportedExpression "heap reservation result arity"))
    GrinStoreUnchecked node -> liftEither (materializeNodeUnchecked env node) >>= storeSingleResult
    GrinUpdate pointer value -> compileUpdateBinding False "_aihc_update" pointer value
    GrinUpdateBlackhole pointer value -> compileUpdateBinding True "_aihc_update_blackhole" pointer value
    GrinPrimitiveCall _ name [left, right]
      | Just instructions <- lookup name singleResultBinaryPrimitives ->
          compileBinary storeSingleResult instructions left right
      | Just (firstRegister, secondRegister, instructions) <- lookup name pairResultBinaryPrimitives ->
          compileBinary (storeTwoResults firstRegister secondRegister) instructions left right
    GrinPrimitiveCall _ "quotRemWord2#" arguments@[_, _, _] -> do
      (argumentLines, argumentSlots) <- materializeIntoFreshSlots env arguments
      savedRegisters <- freshSlots 4
      loopLabel <- freshLabel (valueLabelPrefix env) "quot_word2_loop"
      subtractLabel <- freshLabel (valueLabelPrefix env) "quot_word2_subtract"
      nextLabel <- freshLabel (valueLabelPrefix env) "quot_word2_next"
      case (argumentSlots, savedRegisters) of
        ([highSlot, lowSlot, divisorSlot], [saved12, saved13, saved14, saved15]) ->
          storeTwoResults
            X9
            X10
            ( argumentLines
                <> [ storeAt X12 X19 saved12,
                     storeAt X13 X19 saved13,
                     storeAt X14 X19 saved14,
                     storeAt X15 X19 saved15,
                     loadAt X9 X19 highSlot,
                     loadAt X10 X19 lowSlot,
                     loadAt X11 X19 divisorSlot,
                     arm64Instruction (ArmMov X12 (Arm64ImmediateValue 0)),
                     arm64Instruction (ArmMov X13 (Arm64ImmediateValue 64)),
                     arm64Label loopLabel,
                     arm64Instruction (ArmLsr X14 X9 (Arm64ImmediateShift 63)),
                     arm64Instruction (ArmLsr X15 X10 (Arm64ImmediateShift 63)),
                     arm64Instruction (ArmLsl X9 X9 (Arm64ImmediateShift 1)),
                     arm64Instruction (ArmOrr X9 X9 (Arm64RegisterValue X15)),
                     arm64Instruction (ArmLsl X10 X10 (Arm64ImmediateShift 1)),
                     arm64Instruction (ArmLsl X12 X12 (Arm64ImmediateShift 1)),
                     arm64Instruction (ArmCbnz X14 subtractLabel),
                     arm64Instruction (ArmCmp X9 (Arm64RegisterValue X11)),
                     arm64Instruction (ArmBCond ArmCc nextLabel),
                     arm64Label subtractLabel,
                     arm64Instruction (ArmSub X9 X9 (Arm64RegisterValue X11)),
                     arm64Instruction (ArmOrr X12 X12 (Arm64ImmediateValue 1)),
                     arm64Label nextLabel,
                     arm64Instruction (ArmSubs X13 X13 (Arm64ImmediateValue 1)),
                     arm64Instruction (ArmBCond ArmNe loopLabel),
                     arm64Instruction (ArmMov X10 (Arm64RegisterValue X9)),
                     arm64Instruction (ArmMov X9 (Arm64RegisterValue X12)),
                     loadAt X12 X19 saved12,
                     loadAt X13 X19 saved13,
                     loadAt X14 X19 saved14,
                     loadAt X15 X19 saved15
                   ]
            )
        _ -> lift (Left (Arm64UnsupportedExpression "internal quotRemWord2# arity"))
    GrinPrimitiveCall _ "nullAddr#" [] ->
      storeSingleResult [arm64Instruction (ArmMov X0 (Arm64ImmediateValue 0))]
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
          pure (swapLines <> storeLocation X0 flagLocation <> readLines <> storeLocation X0 currentLocation)
    GrinPrimitiveCall _ name arguments
      | Just splitCalls <- nativeSplitRuntimePrimitiveCall name,
        length splitCalls == length vars ->
          concat
            <$> mapM
              ( \(var, splitCall) -> do
                  callLines <- compileRuntimeCallLines env splitCall arguments
                  location <- liftEither (variableLocation env var)
                  pure (callLines <> storeLocation X0 location)
              )
              (zip vars splitCalls)
    GrinPrimitiveCall _ name arguments
      | Just runtimeCall <- nativeRuntimePrimitiveCall name -> do
          callLines <- compileRuntimeCallLines env runtimeCall arguments
          case nativeRuntimeCallResultCount runtimeCall of
            0 | null vars -> pure callLines
            1 -> storeSingleResult callLines
            _ -> lift (Left (Arm64UnsupportedExpression ("runtime primitive result arity " <> name)))
      | compileAllowUnsupportedPrimitives (valueCompileEnv env) ->
          pure [arm64Instruction (ArmBl "_aihc_unsupported_primitive")]
      | otherwise -> lift (Left (Arm64UnsupportedExpression ("primitive call " <> name)))
    GrinForeignCallExpr foreignCall arguments ->
      compileForeignCallLines env foreignCall arguments >>= storeSingleResult
    _ -> lift (Left (Arm64UnsupportedExpression "non-direct expression remained in a CPS bind"))
  where
    storeSingleResult lines' =
      case vars of
        [var] -> do
          location <- liftEither (variableLocation env var)
          pure (lines' <> storeLocation X0 location)
        _ -> lift (Left (Arm64UnsupportedExpression "direct expression result arity"))
    storeTwoResults firstRegister secondRegister lines' =
      case vars of
        [first, second] -> do
          firstLocation <- liftEither (variableLocation env first)
          secondLocation <- liftEither (variableLocation env second)
          pure (lines' <> storeLocation firstRegister firstLocation <> storeLocation secondRegister secondLocation)
        _ -> lift (Left (Arm64UnsupportedExpression "direct expression pair result arity"))
    compileBinary store instructions left right = do
      leftLines <- liftEither (materializeValueTo env X9 left)
      rightLines <- liftEither (materializeValue env right)
      store (leftLines <> rightLines <> instructions)
    compileUpdateBinding passMachine symbol pointer value = do
      pointerSlot <- freshSlot
      valueSlot <- freshSlot
      storedLines <- materializeIntoSlots env [(pointer, pointerSlot), (value, valueSlot)]
      resultLines <- storeSingleResult [loadAt X0 X19 valueSlot]
      pure
        ( storedLines
            <> [ loadAt (if passMachine then X1 else X0) X19 pointerSlot,
                 loadAt (if passMachine then X2 else X1) X19 valueSlot
               ]
            <> [arm64Instruction (ArmMov X0 (Arm64RegisterValue X22)) | passMachine]
            <> [arm64Instruction (ArmBl symbol)]
            <> resultLines
        )

    singleResultBinaryPrimitives =
      concat
        [ binary (\destination left right -> ArmAdd destination left (Arm64RegisterValue right)) ["+#", "plusWord#"],
          binary (\destination left right -> ArmSub destination left (Arm64RegisterValue right)) ["-#", "minusWord#"],
          binary ArmMul ["*#", "timesWord#"],
          binary ArmAnd ["and#"],
          binary (\destination left right -> ArmOrr destination left (Arm64RegisterValue right)) ["or#"],
          binary ArmEor ["xor#"],
          comparison ArmEq ["==#", "eqWord#", "eqWord64#"],
          comparison ArmLt ["<#"],
          comparison ArmGt [">#"],
          comparison ArmGe [">=#"],
          comparison ArmLe ["<=#"],
          comparison ArmNe ["/=#", "neWord#", "neWord64#"],
          comparison ArmCc ["ltWord#", "ltWord64#"],
          comparison ArmLs ["leWord#", "leWord64#"],
          comparison ArmHi ["gtWord#", "gtWord64#"],
          comparison ArmCs ["geWord#", "geWord64#"]
        ]
        <> [ ("compareInt#", [arm64Instruction (ArmCmp X9 (Arm64RegisterValue X0)), arm64Instruction (ArmCset X0 ArmGt), arm64Instruction (ArmCsinv X0 X0 XZR ArmGe)]),
             ("quotWord#", [arm64Instruction (ArmUdiv X0 X9 X0)]),
             ("remWord#", [arm64Instruction (ArmUdiv X10 X9 X0), arm64Instruction (ArmMsub X0 X10 X0 X9)]),
             ("uncheckedShiftL#", [arm64Instruction (ArmLsl X0 X9 (Arm64RegisterShift X0))]),
             ("uncheckedShiftRL#", [arm64Instruction (ArmLsr X0 X9 (Arm64RegisterShift X0))])
           ]
    pairResultBinaryPrimitives =
      [ carry "addIntC#" ArmAdds ArmVs,
        carry "subIntC#" ArmSubs ArmVs,
        carry "addWordC#" ArmAdds ArmCs,
        carry "subWordC#" ArmSubs ArmCc,
        ("timesWord2#", (X10, X11, [arm64Instruction (ArmUmulh X10 X9 X0), arm64Instruction (ArmMul X11 X9 X0)])),
        ("quotRemWord#", (X10, X11, [arm64Instruction (ArmUdiv X10 X9 X0), arm64Instruction (ArmMsub X11 X10 X0 X9)]))
      ]
    unaryPrimitives =
      ("not#", [arm64Instruction (ArmMvn X0 X0)])
        : [ (name, [])
          | name <- ["int2Word#", "word2Int#", "word8ToWord#", "word32ToWord#", "word64ToWord#", "wordToWord64#", "word16ToWord#", "ord#", "chr#", "unsafeFreezeArray#", "unsafeThawArray#", "unsafeFreezeByteArray#", "unsafeThawByteArray#", "castFloatToWord32#", "castWord32ToFloat#", "castDoubleToWord64#", "castWord64ToDouble#"]
          ]
    binary opcode names =
      [(name, [arm64Instruction (opcode X0 X9 X0)]) | name <- names]
    comparison condition names =
      [(name, [arm64Instruction (ArmCmp X9 (Arm64RegisterValue X0)), arm64Instruction (ArmCset X0 condition)]) | name <- names]
    carry name opcode condition =
      ( name,
        ( X9,
          X10,
          [arm64Instruction (opcode X9 X9 (Arm64RegisterValue X0)), arm64Instruction (ArmCset X10 condition)]
        )
      )

compileForeignCallLines :: ValueEnv -> GrinForeignCall -> [GrinValue] -> FunctionM [Arm64Statement]
compileForeignCallLines env = compileCallLines env False

compileRuntimeCallLines :: ValueEnv -> NativeRuntimeCall -> [GrinValue] -> FunctionM [Arm64Statement]
compileRuntimeCallLines env runtimeCall =
  compileCallLines env (nativeRuntimeCallPassMachine runtimeCall) (nativeRuntimeCallForeignCall runtimeCall)

compileCallLines :: ValueEnv -> Bool -> GrinForeignCall -> [GrinValue] -> FunctionM [Arm64Statement]
compileCallLines env passMachine foreignCall arguments = do
  let signature = grinForeignCallSignature foreignCall
      operandArity = length (grinForeignArgumentTypes signature)
      abiArity = operandArity + fromEnum passMachine
      expectedArity = length (grinForeignOperandReps signature)
  if length arguments /= expectedArity
    then lift (Left (Arm64UnsupportedExpression "foreign call arity mismatch"))
    else
      if abiArity > 8
        then lift (Left (Arm64UnsupportedExpression "foreign calls with more than eight arguments"))
        else do
          (argumentLines, argumentSlots) <- materializeIntoFreshSlots env arguments
          let argumentRegisters = drop (fromEnum passMachine) applyArgumentRegisters
              loadAbiArguments =
                [ loadAt register X19 slot
                | (register, slot) <- zip argumentRegisters argumentSlots
                ]
              callLines =
                argumentLines
                  <> [arm64Instruction (ArmMov X0 (Arm64RegisterValue X22)) | passMachine]
                  <> loadAbiArguments
                  <> [arm64Instruction (ArmBl ("_" <> grinForeignCallSymbol foreignCall))]
                  <> normalizeForeignResult (grinForeignResultType signature)
          pure callLines

materializeIntoFreshSlots :: ValueEnv -> [GrinValue] -> FunctionM ([Arm64Statement], [Int])
materializeIntoFreshSlots env values = do
  slots <- freshSlots (length values)
  lines' <- materializeIntoSlots env (zip values slots)
  pure (lines', slots)

materializeIntoSlots :: ValueEnv -> [(GrinValue, Int)] -> FunctionM [Arm64Statement]
materializeIntoSlots env = fmap concat . mapM store
  where
    store (value, slot) = do
      lines' <- liftEither (materializeValue env value)
      pure (lines' <> [storeAt X0 X19 slot])

normalizeForeignResult :: GrinForeignType -> [Arm64Statement]
normalizeForeignResult foreignType =
  case foreignType of
    GrinForeignInt -> []
    GrinForeignInt32 -> [arm64Instruction (ArmSxtw X0 W0)]
    GrinForeignWord64 -> []
    GrinForeignAddr -> []

compileCase :: ValueEnv -> [Arm64Statement] -> Text -> GrinValue -> GrinVar -> [GrinAlt] -> FunctionM ()
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
            then loadLocation X9 resultLocation <> storeLocation X9 binderLocation
            else []
    prefixLines <- alternativePrefix env resultLocation alternative
    compileExpr env (binderLines <> prefixLines) alternativeLabel rhs
  where
    materializedScrutinee = do
      slot <- freshSlot
      lines' <- liftEither (materializeValue env scrutinee)
      pure (InHeapSpill slot, lines' <> [storeAt X0 X19 slot])

alternativePrefix :: ValueEnv -> Location Arm64Register -> GrinAlt -> FunctionM [Arm64Statement]
alternativePrefix env resultLocation alternative =
  case grinAltCon alternative of
    GrinDataAlt _ -> do
      fields <- fmap concat . forM liveIndexedBinders $ \(index, binder) -> do
        location <- liftEither (variableLocation env binder)
        pure ([loadByteOffset X10 X9 (8 + index * 8)] <> storeLocation X10 location)
      pure (if null fields then [] else loadLocation X9 resultLocation <> fields)
    GrinLitAlt _ -> pure []
    GrinDefaultAlt ->
      fmap concat . forM (filter isLive (grinAltBinders alternative)) $ \binder -> do
        location <- liftEither (variableLocation env binder)
        pure (loadLocation X9 resultLocation <> storeLocation X9 location)
  where
    isLive binder = binder `Set.member` grinExprFreeVariables (grinAltRhs alternative)
    liveIndexedBinders = filter (isLive . snd) (zip [0 ..] (grinAltBinders alternative))

caseChecks :: Location Arm64Register -> Bool -> [(GrinAlt, Text)] -> FunctionM ([Arm64Statement], BlockLayout.Terminator Text)
caseChecks resultLocation scrutineeIsPointer targets = do
  let nonDefault = [(alternative, label) | (alternative, label) <- targets, grinAltCon alternative /= GrinDefaultAlt]
      defaultTarget = [label | (alternative, label) <- targets, grinAltCon alternative == GrinDefaultAlt]
  checks <- fmap concat . forM nonDefault $ \(alternative, target) ->
    case grinAltCon alternative of
      GrinDataAlt _
        | not scrutineeIsPointer ->
            lift (Left (Arm64UnsupportedExpression "constructor case on an unboxed value"))
      GrinDataAlt name -> do
        let identity = constructorStageLabel name 0
        pure $
          loadLocation X9 resultLocation
            <> [ arm64Instruction (ArmLdr X10 (Arm64Offset X9 0)),
                 arm64Instruction (ArmLdr X10 (Arm64Offset X10 0))
               ]
            <> address X11 identity
            <> [ arm64Instruction (ArmCmp X10 (Arm64RegisterValue X11)),
                 arm64Instruction (ArmBCond ArmEq target)
               ]
      GrinLitAlt _
        | scrutineeIsPointer ->
            lift (Left (Arm64UnsupportedExpression "literal case on a lifted value"))
      GrinLitAlt literal ->
        case normalizedLiteralInteger literal of
          Just integer ->
            pure $
              loadLocation X10 resultLocation
                <> [immediate X11 integer, arm64Instruction (ArmCmp X10 (Arm64RegisterValue X11)), arm64Instruction (ArmBCond ArmEq target)]
          Nothing -> lift (Left (Arm64UnsupportedValue "string case alternative"))
      GrinDefaultAlt -> pure []
  pure $ case defaultTarget of
    target : _ -> (checks, BlockLayout.Jump target)
    [] -> (checks <> [arm64Instruction (ArmBl "_aihc_no_match"), arm64Instruction (ArmBrk 0)], BlockLayout.Exit)

moveValuesToRegisters :: ValueEnv -> [GrinValue] -> [Arm64Register] -> Either Arm64Error [Arm64Statement]
moveValuesToRegisters env values registers =
  fmap concat . forM (zip values registers) $ \(value, register) ->
    materializeValueTo env register value

moveValuesToLocations :: ValueEnv -> [GrinValue] -> [Location Arm64Register] -> FunctionM [Arm64Statement]
moveValuesToLocations env values destinations
  | and (zipWith alreadyThere values destinations) = pure []
  | otherwise = do
      (stores, slots) <- materializeIntoFreshSlots env values
      let loads =
            concat
              [ [loadAt X9 X19 slot] <> storeLocation X9 destination
              | (slot, destination) <- zip slots destinations
              ]
      pure (stores <> loads)
  where
    alreadyThere value destination =
      case value of
        GrinVarValue var -> Map.lookup var (valueLocations env) == Just destination
        GrinGlobalValue {} -> False
        GrinLitValue {} -> False

saveValueOverflowLines :: ValueEnv -> [GrinValue] -> Either Arm64Error [Arm64Statement]
saveValueOverflowLines env values
  | stackBytes == 0 = pure []
  | otherwise = do
      stores <-
        fmap concat . forM (zip [0 :: Int ..] (drop (length applyArgumentRegisters) values)) $ \(index, value) -> do
          lines' <- materializeValueTo env X8 value
          pure (lines' <> [storeByteOffset X8 X10 (index * 8)])
      pure ([immediate X8 stackBytes, arm64Instruction (ArmSub SP SP (Arm64RegisterValue X8)), arm64Instruction (ArmMov X10 (Arm64RegisterValue SP))] <> stores)
  where
    stackBytes = applyStackBytes (length values)

saveApplyOverflowLines :: Arm64Register -> [Int] -> [Arm64Statement]
saveApplyOverflowLines base slots
  | stackBytes == 0 = []
  | otherwise =
      [immediate X8 stackBytes, arm64Instruction (ArmSub SP SP (Arm64RegisterValue X8)), arm64Instruction (ArmMov X9 (Arm64RegisterValue SP))]
        <> concat
          [ [loadAt X8 base slot, arm64Instruction (ArmStr X8 (Arm64PostIndex X9 8))]
          | slot <- drop (length applyArgumentRegisters) slots
          ]
  where
    stackBytes = applyStackBytes (length slots)

moveDirectOverflowLines :: Arm64Register -> Int -> [Arm64Statement]
moveDirectOverflowLines base valueCount
  | stackBytes == 0 = []
  | otherwise =
      [arm64Instruction (ArmMov X9 (Arm64RegisterValue SP))]
        <> concat
          [ [arm64Instruction (ArmLdr X8 (Arm64PostIndex X9 8)), storeAt X8 base targetIndex]
          | targetIndex <- [length applyArgumentRegisters .. valueCount - 1]
          ]
        <> restoreApplyStackLines stackBytes
  where
    stackBytes = applyStackBytes valueCount

variableLocation :: ValueEnv -> GrinVar -> Either Arm64Error (Location Arm64Register)
variableLocation env var =
  maybe
    (Left (Arm64UnsupportedExpression ("missing location for " <> grinVarName var)))
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

addBlock :: Text -> [Arm64Statement] -> BlockLayout.Terminator Text -> FunctionM ()
addBlock label instructions terminator =
  modify' $ \state ->
    state
      { functionBlocksRev =
          BlockLayout.Block label instructions terminator : functionBlocksRev state
      }

slotPointer :: Arm64Register -> [Int] -> [Arm64Statement]
slotPointer register slots =
  case slots of
    first : _ ->
      let offset = first * 8
       in case offset <= 4095 of
            True -> [arm64Instruction (ArmAdd register X19 (Arm64ImmediateValue (fromIntegral offset)))]
            False -> [immediate register offset, arm64Instruction (ArmAdd register X19 (Arm64RegisterValue register))]
    [] -> [arm64Instruction (ArmMov register (Arm64RegisterValue XZR))]

liftEither :: Either Arm64Error value -> FunctionM value
liftEither = lift
