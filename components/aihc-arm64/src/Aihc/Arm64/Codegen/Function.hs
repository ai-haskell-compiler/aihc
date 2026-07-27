{-# LANGUAGE OverloadedStrings #-}

-- | Lower individual CPS GRIN functions to AArch64 basic blocks.
module Aihc.Arm64.Codegen.Function
  ( compileFunction,
    reserveLocalsLines,
  )
where

import Aihc.Arm64.Codegen.Runtime
import Aihc.Arm64.RegisterAllocate qualified as RegisterAllocate
import Aihc.Grin.Syntax
import Aihc.Native
  ( NativeCpsCall (..),
    NativeCpsTransfer (..),
    nativeCpsPrimitiveCall,
    nativeRuntimePrimitiveCall,
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
          <> [ ".p2align 3",
               label <> ":"
             ]
          <> registerParameterCopies
      blocks =
        BlockLayout.renderBlocks
          (<> ":")
          ("  b " <>)
          (BlockLayout.layoutBlocks bodyLabel (reverse (functionBlocksRev finalState)))
  pure
    CompiledFunction
      { compiledFunctionSlots = slotCount,
        compiledFunctionLines = entry <> blocks
      }

reserveLocalsLines :: [CompiledFunction] -> [Text]
reserveLocalsLines functions =
  [ immediate "x1" maximumSlots,
    "  mov x0, x22",
    "  bl _aihc_alloc_locals",
    "  mov x19, x0"
  ]
  where
    maximumSlots = maximum (2 : map compiledFunctionSlots functions)

exportLines :: CompileEnv -> GrinFunction -> Text -> [Text]
exportLines env function label
  | compileExposeAllFunctions env = [".globl " <> label]
  | otherwise =
      case grinFunctionLinkName function of
        Just _ -> [".globl " <> label]
        Nothing -> []

compileExpr :: ValueEnv -> [Text] -> Text -> GrinExpr -> FunctionM ()
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
    GrinFetch {} -> unsupportedExpression "direct-style fetch return after CPS"
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
            <> [ loadAt applyFunctionRegister "x19" valueSlot,
                 loadAt applyContinuationRegister "x19" continuationSlot,
                 loadAt "x0" "x19" updateSlot,
                 immediate "x8" (fromEnum (isLiftedRuntimeRep runtimeRep))
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
              <> [ loadAt "x1" "x19" scratch,
                   immediate "x2" (length arguments),
                   slotPointer "x3" argumentSlots,
                   immediate "x4" (continuationSlot * 8),
                   "  add x4, x19, x4",
                   "  mov x0, x22",
                   "  bl _aihc_apply_slow",
                   loadAt applyFunctionRegister "x19" continuationSlot
                 ]
      addBlock
        label
        ( prefix
            <> storedLines
            <> [ loadAt applyFunctionRegister "x19" scratch,
                 loadAt applyContinuationRegister "x19" continuationSlot
               ]
            <> [loadAt register "x19" slot | (register, slot) <- zip applyArgumentRegisters argumentSlots]
            <> saveApplyOverflowLines "x19" argumentSlots
            <> [ "  ldr x8, [" <> applyFunctionRegister <> "]",
                 "  and x8, x8, #0xfffffffffffffff8",
                 "  ldr x8, [x8, #48]",
                 "  cbz x8, " <> slowLabel,
                 "  br x8",
                 slowLabel <> ":"
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
            <> [ loadAt "x1" "x19" exceptionSlot,
                 loadAt "x2" "x19" continuationSlot,
                 "  mov x0, x22",
                 "  bl _aihc_raise"
               ]
        )
        (BlockLayout.Jump ".Laihc_resume")
    GrinHalt _ ->
      addBlock
        label
        (prefix <> ["  mov x0, x22", "  bl _aihc_halt", "  br x0"])
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
        (prefix <> overflowLines <> extraLines <> registerLines <> moveDirectOverflowLines "x19" (length values))
        (BlockLayout.Jump target)
    compileStoreRec allocate bindings body = do
      allocationLines <- fmap concat . forM bindings $ \(var, node) -> do
        location <- liftEither (variableLocation env var)
        nodeLines <- liftEither (allocate env node)
        pure (nodeLines <> storeLocation "x0" location)
      initializationLines <- fmap concat . forM bindings $ \(var, node) -> do
        location <- liftEither (variableLocation env var)
        fieldLines <- liftEither (initializeNodeFields env node)
        pure (loadLocation "x20" location <> fieldLines)
      compileExpr env (prefix <> allocationLines <> initializationLines) label body

compileCpsPrimitive :: ValueEnv -> [Text] -> Text -> Text -> [GrinValue] -> GrinValue -> FunctionM ()
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
                ([loadAt applyFunctionRegister "x19" continuationSlot], BlockLayout.Jump ".Laihc_enter")
              NativeCpsResumeScheduler ->
                ([], BlockLayout.Jump ".Laihc_resume")
      addBlock
        label
        ( prefix
            <> storedLines
            <> renderCpsCallArguments runtimeCall argumentSlots continuationSlot
            <> ["  bl _" <> nativeCpsCallSymbol runtimeCall]
            <> returnLines
        )
        successor

    unsupportedCpsPrimitive =
      lift (Left (Arm64UnsupportedExpression ("CPS primitive call " <> name)))

renderCpsCallArguments :: NativeCpsCall -> [Int] -> Int -> [Text]
renderCpsCallArguments runtimeCall operandSlots continuationSlot =
  ["  mov x0, x22"]
    <> [loadAt register "x19" slot | (register, slot) <- zip (drop 1 applyArgumentRegisters) operandSlots]
    <> [ loadAt (applyArgumentRegisters !! (length operandSlots + 1)) "x19" continuationSlot
       | nativeCpsCallPassContinuation runtimeCall
       ]

compileDirectBinding :: ValueEnv -> [GrinVar] -> GrinExpr -> FunctionM [Text]
compileDirectBinding env vars expression =
  case expression of
    GrinConstant values
      | length vars == length values ->
          fmap concat . forM (zip vars values) $ \(var, value) -> do
            location <- liftEither (variableLocation env var)
            valueLines <- liftEither (materializeValue env value)
            pure (valueLines <> storeLocation "x0" location)
    GrinStore node -> liftEither (materializeNode env node) >>= storeSingleResult
    GrinEnsureHeap requiredWords roots
      | length vars == length roots -> do
          (rootLines, rootSlots) <- materializeIntoFreshSlots env roots
          resultLines <-
            fmap concat . forM (zip vars rootSlots) $ \(var, slot) -> do
              location <- liftEither (variableLocation env var)
              pure ([loadAt "x9" "x19" slot] <> storeLocation "x9" location)
          readyLabel <- freshLabel (valueLabelPrefix env) "heap_ready"
          pure
            ( rootLines
                <> [ "  ldr x9, [x22, #24]",
                     "  ldr x10, [x22, #32]",
                     immediate "x11" (requiredWords * 8),
                     "  add x11, x9, x11",
                     "  cmp x11, x10",
                     "  b.ls " <> readyLabel,
                     "  mov x0, x22",
                     immediate "x1" requiredWords,
                     immediate "x2" (length roots),
                     slotPointer "x3" rootSlots,
                     "  bl _aihc_ensure_heap",
                     readyLabel <> ":"
                   ]
                <> resultLines
            )
      | otherwise -> lift (Left (Arm64UnsupportedExpression "heap reservation result arity"))
    GrinStoreUnchecked node -> liftEither (materializeNodeUnchecked env node) >>= storeSingleResult
    GrinFetch _ pointer -> liftEither (materializeValue env pointer) >>= storeSingleResult
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
            "x9"
            "x10"
            ( argumentLines
                <> [ storeAt "x12" "x19" saved12,
                     storeAt "x13" "x19" saved13,
                     storeAt "x14" "x19" saved14,
                     storeAt "x15" "x19" saved15,
                     loadAt "x9" "x19" highSlot,
                     loadAt "x10" "x19" lowSlot,
                     loadAt "x11" "x19" divisorSlot,
                     "  mov x12, #0",
                     "  mov x13, #64",
                     loopLabel <> ":",
                     "  lsr x14, x9, #63",
                     "  lsr x15, x10, #63",
                     "  lsl x9, x9, #1",
                     "  orr x9, x9, x15",
                     "  lsl x10, x10, #1",
                     "  lsl x12, x12, #1",
                     "  cbnz x14, " <> subtractLabel,
                     "  cmp x9, x11",
                     "  b.lo " <> nextLabel,
                     subtractLabel <> ":",
                     "  sub x9, x9, x11",
                     "  orr x12, x12, #1",
                     nextLabel <> ":",
                     "  subs x13, x13, #1",
                     "  b.ne " <> loopLabel,
                     "  mov x10, x9",
                     "  mov x9, x12",
                     loadAt "x12" "x19" saved12,
                     loadAt "x13" "x19" saved13,
                     loadAt "x14" "x19" saved14,
                     loadAt "x15" "x19" saved15
                   ]
            )
        _ -> lift (Left (Arm64UnsupportedExpression "internal quotRemWord2# arity"))
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
    GrinPrimitiveCall _ name arguments
      | Just foreignCall <- nativeRuntimePrimitiveCall name -> do
          callLines <- compileForeignCallLines env foreignCall arguments
          case vars of
            [] -> pure callLines
            [_] -> storeSingleResult callLines
            _ -> lift (Left (Arm64UnsupportedExpression ("runtime primitive result arity " <> name)))
      | compileAllowUnsupportedPrimitives (valueCompileEnv env) ->
          pure ["  bl _aihc_unsupported_primitive"]
      | otherwise -> lift (Left (Arm64UnsupportedExpression ("primitive call " <> name)))
    GrinForeignCallExpr foreignCall arguments ->
      compileForeignCallLines env foreignCall arguments >>= storeSingleResult
    _ -> lift (Left (Arm64UnsupportedExpression "non-direct expression remained in a CPS bind"))
  where
    storeSingleResult lines' =
      case vars of
        [var] -> do
          location <- liftEither (variableLocation env var)
          pure (lines' <> storeLocation "x0" location)
        _ -> lift (Left (Arm64UnsupportedExpression "direct expression result arity"))
    storeTwoResults firstRegister secondRegister lines' =
      case vars of
        [first, second] -> do
          firstLocation <- liftEither (variableLocation env first)
          secondLocation <- liftEither (variableLocation env second)
          pure (lines' <> storeLocation firstRegister firstLocation <> storeLocation secondRegister secondLocation)
        _ -> lift (Left (Arm64UnsupportedExpression "direct expression pair result arity"))
    compileBinary store instructions left right = do
      leftLines <- liftEither (materializeValueTo env "x9" left)
      rightLines <- liftEither (materializeValue env right)
      store (leftLines <> rightLines <> instructions)
    compileUpdateBinding passMachine symbol pointer value = do
      pointerSlot <- freshSlot
      valueSlot <- freshSlot
      storedLines <- materializeIntoSlots env [(pointer, pointerSlot), (value, valueSlot)]
      resultLines <- storeSingleResult [loadAt "x0" "x19" valueSlot]
      pure
        ( storedLines
            <> [ loadAt (if passMachine then "x1" else "x0") "x19" pointerSlot,
                 loadAt (if passMachine then "x2" else "x1") "x19" valueSlot
               ]
            <> ["  mov x0, x22" | passMachine]
            <> [ "  bl " <> symbol
               ]
            <> resultLines
        )

    singleResultBinaryPrimitives =
      concat
        [ binary "add" ["+#", "plusWord#"],
          binary "sub" ["-#", "minusWord#"],
          binary "mul" ["*#", "timesWord#"],
          binary "and" ["and#"],
          binary "orr" ["or#"],
          binary "eor" ["xor#"],
          comparison "eq" ["==#", "eqWord#"],
          comparison "lt" ["<#"],
          comparison "ne" ["neWord#"],
          comparison "lo" ["ltWord#"],
          comparison "ls" ["leWord#"],
          comparison "hi" ["gtWord#"],
          comparison "hs" ["geWord#"]
        ]
        <> [ ("compareInt#", ["  cmp x9, x0", "  cset x0, gt", "  csinv x0, x0, xzr, ge"]),
             ("quotWord#", ["  udiv x0, x9, x0"]),
             ("remWord#", ["  udiv x10, x9, x0", "  msub x0, x10, x0, x9"]),
             ("uncheckedShiftL#", ["  lsl x0, x9, x0"]),
             ("uncheckedShiftRL#", ["  lsr x0, x9, x0"])
           ]
    pairResultBinaryPrimitives =
      [ carry "addIntC#" "adds" "vs",
        carry "subIntC#" "subs" "vs",
        carry "addWordC#" "adds" "cs",
        carry "subWordC#" "subs" "cc",
        ("timesWord2#", ("x10", "x11", ["  umulh x10, x9, x0", "  mul x11, x9, x0"])),
        ("quotRemWord#", ("x10", "x11", ["  udiv x10, x9, x0", "  msub x11, x10, x0, x9"]))
      ]
    unaryPrimitives =
      ("not#", ["  mvn x0, x0"])
        : [ (name, [])
          | name <- ["int2Word#", "word2Int#", "ord#", "intToChar#", "unsafeFreezeByteArray#", "unsafeThawByteArray#"]
          ]
    binary instruction names =
      [(name, ["  " <> instruction <> " x0, x9, x0"]) | name <- names]
    comparison condition names =
      [(name, ["  cmp x9, x0", "  cset x0, " <> condition]) | name <- names]
    carry name instruction condition =
      ( name,
        ( "x9",
          "x10",
          ["  " <> instruction <> " x9, x9, x0", "  cset x10, " <> condition]
        )
      )

compileForeignCallLines :: ValueEnv -> GrinForeignCall -> [GrinValue] -> FunctionM [Text]
compileForeignCallLines env foreignCall arguments = do
  let signature = grinForeignCallSignature foreignCall
      abiArity = length (grinForeignArgumentTypes signature)
      expectedArity = length (grinForeignOperandReps signature)
  if length arguments /= expectedArity
    then lift (Left (Arm64UnsupportedExpression "foreign call arity mismatch"))
    else
      if abiArity > 8
        then lift (Left (Arm64UnsupportedExpression "foreign calls with more than eight arguments"))
        else do
          (argumentLines, argumentSlots) <- materializeIntoFreshSlots env arguments
          let abiSlots = take abiArity argumentSlots
              loadAbiArguments =
                [ loadAt ("x" <> tshow index) "x19" slot
                | (index, slot) <- zip [0 :: Int ..] abiSlots
                ]
              callLines =
                argumentLines
                  <> loadAbiArguments
                  <> ["  bl _" <> grinForeignCallSymbol foreignCall]
                  <> normalizeForeignResult (grinForeignResultType signature)
          pure callLines

materializeIntoFreshSlots :: ValueEnv -> [GrinValue] -> FunctionM ([Text], [Int])
materializeIntoFreshSlots env values = do
  slots <- freshSlots (length values)
  lines' <- materializeIntoSlots env (zip values slots)
  pure (lines', slots)

materializeIntoSlots :: ValueEnv -> [(GrinValue, Int)] -> FunctionM [Text]
materializeIntoSlots env = fmap concat . mapM store
  where
    store (value, slot) = do
      lines' <- liftEither (materializeValue env value)
      pure (lines' <> [storeAt "x0" "x19" slot])

normalizeForeignResult :: GrinForeignType -> [Text]
normalizeForeignResult foreignType =
  case foreignType of
    GrinForeignInt -> []
    GrinForeignInt32 -> ["  sxtw x0, w0"]
    GrinForeignWord64 -> []
    GrinForeignAddr -> []

compileCase :: ValueEnv -> [Text] -> Text -> GrinValue -> GrinVar -> [GrinAlt] -> FunctionM ()
compileCase env prefix label scrutinee binder alternatives = do
  (resultLocation, scrutineeLines) <- case scrutinee of
    GrinVarValue var | Just location <- Map.lookup var (valueLocations env) -> pure (location, [])
    _ -> materializedScrutinee
  binderLocation <- liftEither (variableLocation env binder)
  let scrutineeIsPointer = isPointerRuntimeRep (grinValueRuntimeRep scrutinee)
  alternativeTargets <- forM alternatives $ \alternative -> do
    alternativeLabel <- freshLabel label "case_alt"
    pure (alternative, alternativeLabel)
  (checks, successor) <- caseChecks env resultLocation scrutineeIsPointer alternativeTargets
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
            then loadLocation "x9" resultLocation <> storeLocation "x9" binderLocation
            else []
    prefixLines <- alternativePrefix env resultLocation alternative
    compileExpr env (binderLines <> prefixLines) alternativeLabel rhs
  where
    materializedScrutinee = do
      slot <- freshSlot
      lines' <- liftEither (materializeValue env scrutinee)
      pure (InHeapSpill slot, lines' <> [storeAt "x0" "x19" slot])

alternativePrefix :: ValueEnv -> Location Text -> GrinAlt -> FunctionM [Text]
alternativePrefix env resultLocation alternative =
  case grinAltCon alternative of
    GrinDataAlt _ -> do
      fields <- fmap concat . forM liveIndexedBinders $ \(index, binder) -> do
        location <- liftEither (variableLocation env binder)
        pure ([loadByteOffset "x10" "x9" (8 + index * 8)] <> storeLocation "x10" location)
      pure (if null fields then [] else loadLocation "x9" resultLocation <> fields)
    GrinLitAlt _ -> pure []
    GrinDefaultAlt ->
      fmap concat . forM (filter isLive (grinAltBinders alternative)) $ \binder -> do
        location <- liftEither (variableLocation env binder)
        pure (loadLocation "x9" resultLocation <> storeLocation "x9" location)
  where
    isLive binder = binder `Set.member` grinExprFreeVariables (grinAltRhs alternative)
    liveIndexedBinders = filter (isLive . snd) (zip [0 ..] (grinAltBinders alternative))

caseChecks :: ValueEnv -> Location Text -> Bool -> [(GrinAlt, Text)] -> FunctionM ([Text], BlockLayout.Terminator Text)
caseChecks env resultLocation scrutineeIsPointer targets = do
  let nonDefault = [(alternative, label) | (alternative, label) <- targets, grinAltCon alternative /= GrinDefaultAlt]
      defaultTarget = [label | (alternative, label) <- targets, grinAltCon alternative == GrinDefaultAlt]
  checks <- fmap concat . forM nonDefault $ \(alternative, target) ->
    case grinAltCon alternative of
      GrinDataAlt _
        | not scrutineeIsPointer ->
            lift (Left (Arm64UnsupportedExpression "constructor case on an unboxed value"))
      GrinDataAlt name -> do
        identifier <- liftEither (constructorId (valueCompileEnv env) name)
        pure $
          loadLocation "x9" resultLocation
            <> [ "  ldr x10, [x9, #0]",
                 "  ldr x10, [x10, #0]",
                 immediate "x11" identifier,
                 "  cmp x10, x11",
                 "  b.eq " <> target
               ]
      GrinLitAlt _
        | scrutineeIsPointer ->
            lift (Left (Arm64UnsupportedExpression "literal case on a lifted value"))
      GrinLitAlt literal ->
        case normalizedLiteralInteger literal of
          Just integer ->
            pure $
              loadLocation "x10" resultLocation
                <> [immediate "x11" integer, "  cmp x10, x11", "  b.eq " <> target]
          Nothing -> lift (Left (Arm64UnsupportedValue "string case alternative"))
      GrinDefaultAlt -> pure []
  pure $ case defaultTarget of
    target : _ -> (checks, BlockLayout.Jump target)
    [] -> (checks <> ["  bl _aihc_no_match", "  brk #0"], BlockLayout.Exit)

moveValuesToRegisters :: ValueEnv -> [GrinValue] -> [Text] -> Either Arm64Error [Text]
moveValuesToRegisters env values registers =
  fmap concat . forM (zip values registers) $ \(value, register) ->
    materializeValueTo env register value

moveValuesToLocations :: ValueEnv -> [GrinValue] -> [Location Text] -> FunctionM [Text]
moveValuesToLocations env values destinations
  | and (zipWith alreadyThere values destinations) = pure []
  | otherwise = do
      (stores, slots) <- materializeIntoFreshSlots env values
      let loads =
            concat
              [ [loadAt "x9" "x19" slot] <> storeLocation "x9" destination
              | (slot, destination) <- zip slots destinations
              ]
      pure (stores <> loads)
  where
    alreadyThere value destination =
      case value of
        GrinVarValue var -> Map.lookup var (valueLocations env) == Just destination
        GrinLitValue {} -> False

saveValueOverflowLines :: ValueEnv -> [GrinValue] -> Either Arm64Error [Text]
saveValueOverflowLines env values
  | stackBytes == 0 = pure []
  | otherwise = do
      stores <-
        fmap concat . forM (zip [0 :: Int ..] (drop (length applyArgumentRegisters) values)) $ \(index, value) -> do
          lines' <- materializeValueTo env "x8" value
          pure (lines' <> ["  str x8, [x10, #" <> tshow (index * 8) <> "]"])
      pure ([immediate "x8" stackBytes, "  sub sp, sp, x8", "  mov x10, sp"] <> stores)
  where
    stackBytes = applyStackBytes (length values)

saveApplyOverflowLines :: Text -> [Int] -> [Text]
saveApplyOverflowLines base slots
  | stackBytes == 0 = []
  | otherwise =
      [immediate "x8" stackBytes, "  sub sp, sp, x8", "  mov x9, sp"]
        <> concat
          [ [loadAt "x8" base slot, "  str x8, [x9], #8"]
          | slot <- drop (length applyArgumentRegisters) slots
          ]
  where
    stackBytes = applyStackBytes (length slots)

moveDirectOverflowLines :: Text -> Int -> [Text]
moveDirectOverflowLines base valueCount
  | stackBytes == 0 = []
  | otherwise =
      ["  mov x9, sp"]
        <> concat
          [ ["  ldr x8, [x9], #8", storeAt "x8" base targetIndex]
          | targetIndex <- [length applyArgumentRegisters .. valueCount - 1]
          ]
        <> restoreApplyStackLines stackBytes
  where
    stackBytes = applyStackBytes valueCount

variableLocation :: ValueEnv -> GrinVar -> Either Arm64Error (Location Text)
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

addBlock :: Text -> [Text] -> BlockLayout.Terminator Text -> FunctionM ()
addBlock label instructions terminator =
  modify' $ \state ->
    state
      { functionBlocksRev =
          BlockLayout.Block label instructions terminator : functionBlocksRev state
      }

slotPointer :: Text -> [Int] -> Text
slotPointer register slots =
  case slots of
    first : _ -> "  add " <> register <> ", x19, #" <> tshow (first * 8)
    [] -> "  mov " <> register <> ", xzr"

liftEither :: Either Arm64Error value -> FunctionM value
liftEither = lift
