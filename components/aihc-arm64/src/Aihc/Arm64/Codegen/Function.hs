{-# LANGUAGE OverloadedStrings #-}

-- | Lower individual CPS GRIN functions to AArch64 basic blocks.
module Aihc.Arm64.Codegen.Function
  ( compileFunction,
    reserveLocalsLines,
  )
where

import Aihc.Arm64.Codegen.Runtime
import Aihc.Arm64.Codegen.Types
import Aihc.Arm64.Codegen.Value
import Aihc.Arm64.RegisterAllocate qualified as RegisterAllocate
import Aihc.Grin.Syntax
import Aihc.Native
  ( NativeCpsCall (..),
    NativeCpsParameter (..),
    NativeCpsTransfer (..),
    nativeCpsPrimitiveCall,
    nativeRuntimePrimitiveCall,
  )
import Aihc.Native.BlockLayout qualified as BlockLayout
import Aihc.Native.RegisterAllocate (Location (..), grinExprFreeVariables)
import Aihc.Tc.Types (RuntimeRep (..))
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
    GrinStoreRec bindings body -> do
      allocationLines <-
        fmap concat . forM bindings $ \(var, node) -> do
          location <- liftEither (variableLocation env var)
          nodeLines <- liftEither (allocateNode env node)
          pure (nodeLines <> storeLocation "x0" location)
      initializationLines <-
        fmap concat . forM bindings $ \(var, node) -> do
          location <- liftEither (variableLocation env var)
          fieldLines <- liftEither (initializeNodeFields env node)
          pure (loadLocation "x20" location <> fieldLines)
      compileExpr env (prefix <> allocationLines <> initializationLines) label body
    GrinStoreRecUnchecked bindings body -> do
      allocationLines <-
        fmap concat . forM bindings $ \(var, node) -> do
          location <- liftEither (variableLocation env var)
          nodeLines <- liftEither (allocateNodeUnchecked env node)
          pure (nodeLines <> storeLocation "x0" location)
      initializationLines <-
        fmap concat . forM bindings $ \(var, node) -> do
          location <- liftEither (variableLocation env var)
          fieldLines <- liftEither (initializeNodeFields env node)
          pure (loadLocation "x20" location <> fieldLines)
      compileExpr env (prefix <> allocationLines <> initializationLines) label body
    GrinFetch {} -> unsupportedExpression "direct-style fetch return after CPS"
    GrinUpdate {} -> unsupportedExpression "direct-style update return after CPS"
    GrinUpdateBlackhole {} -> unsupportedExpression "unbound blackhole update"
    GrinEval {} -> unsupportedExpression "direct-style eval after CPS"
    GrinCpsEval runtimeRep value continuation updateContinuation -> do
      valueSlot <- freshSlot
      continuationSlot <- freshSlot
      updateSlot <- freshSlot
      valueLines <- liftEither (materializeValue env value)
      continuationLines <- liftEither (materializeValue env continuation)
      updateLines <- liftEither (materializeValue env updateContinuation)
      addBlock
        label
        ( prefix
            <> valueLines
            <> [storeAt "x0" "x19" valueSlot]
            <> continuationLines
            <> [storeAt "x0" "x19" continuationSlot]
            <> updateLines
            <> [ storeAt "x0" "x19" updateSlot,
                 loadAt applyFunctionRegister "x19" valueSlot,
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
            then do
              overflowLines <- liftEither (saveValueOverflowLines env arguments)
              registerLines <- liftEither (moveValuesToRegisters env arguments applyArgumentRegisters)
              addBlock
                label
                ( prefix
                    <> overflowLines
                    <> registerLines
                    <> moveDirectOverflowLines "x19" (length arguments)
                )
                (BlockLayout.Jump target)
            else case reverse arguments of
              continuation : reversedValues -> do
                let values = reverse reversedValues
                overflowLines <- liftEither (saveValueOverflowLines env values)
                continuationLines <- liftEither (materializeValueTo env applyContinuationRegister continuation)
                registerLines <- liftEither (moveValuesToRegisters env values applyArgumentRegisters)
                addBlock
                  label
                  ( prefix
                      <> overflowLines
                      <> continuationLines
                      <> registerLines
                      <> moveDirectOverflowLines "x19" (length values)
                  )
                  (BlockLayout.Jump target)
              [] -> unsupportedExpression "direct CPS call has no continuation"
    GrinPrimitiveCall {} -> unsupportedExpression "unbound primitive call after CPS"
    GrinCpsPrimitiveCall _ name arguments continuation ->
      compileCpsPrimitive env prefix label name arguments continuation
    GrinApply {} -> unsupportedExpression "direct-style apply after CPS"
    GrinCpsApply _ function arguments continuation -> do
      scratch <- freshSlot
      continuationSlot <- freshSlot
      slowLabel <- freshLabel (valueLabelPrefix env) "apply_slow"
      functionLines <- liftEither (materializeValue env function)
      continuationLines <- liftEither (materializeValue env continuation)
      argumentSlots <- freshSlots (length arguments)
      argumentLines <-
        fmap concat . forM (zip arguments argumentSlots) $ \(argument, slot) -> do
          lines' <- liftEither (materializeValue env argument)
          pure (lines' <> [storeAt "x0" "x19" slot])
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
            <> functionLines
            <> [storeAt "x0" "x19" scratch]
            <> continuationLines
            <> [storeAt "x0" "x19" continuationSlot]
            <> argumentLines
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
    GrinHalt _ ->
      addBlock
        label
        (prefix <> ["  mov x0, x22", "  bl _aihc_halt", "  br x0"])
        BlockLayout.Exit
    GrinCase scrutinee binder alternatives ->
      compileCase env prefix label scrutinee binder alternatives
    GrinThrow {} -> unsupportedExpression "throw"
    GrinCatch {} -> unsupportedExpression "catch"
    GrinForeignCallExpr {} -> unsupportedExpression "unbound foreign call after CPS"
  where
    unsupportedExpression name = lift (Left (Arm64UnsupportedExpression name))

compileCpsPrimitive :: ValueEnv -> [Text] -> Text -> Text -> [GrinValue] -> GrinValue -> FunctionM ()
compileCpsPrimitive env prefix label name arguments continuation =
  case nativeCpsPrimitiveCall name of
    Just runtimeCall
      | operandCount runtimeCall == length arguments -> compileRuntimeCall runtimeCall
    _ -> unsupportedCpsPrimitive
  where
    compileRuntimeCall runtimeCall = do
      continuationSlot <- freshSlot
      argumentSlots <- freshSlots (length arguments)
      argumentLines <-
        fmap concat . forM (zip arguments argumentSlots) $ \(argument, slot) -> do
          lines' <- liftEither (materializeValue env argument)
          pure (lines' <> [storeAt "x0" "x19" slot])
      continuationLines <- liftEither (materializeValue env continuation)
      callArgumentLines <-
        liftEither $
          renderCpsCallArguments name (nativeCpsCallParameters runtimeCall) argumentSlots continuationSlot
      let (returnLines, successor) =
            case nativeCpsCallTransfer runtimeCall of
              NativeCpsEnterContinuation ->
                ([loadAt applyFunctionRegister "x19" continuationSlot], BlockLayout.Jump ".Laihc_enter")
              NativeCpsResumeScheduler ->
                ([], BlockLayout.Jump ".Laihc_resume")
      addBlock
        label
        ( prefix
            <> argumentLines
            <> continuationLines
            <> [storeAt "x0" "x19" continuationSlot]
            <> callArgumentLines
            <> ["  bl _" <> nativeCpsCallSymbol runtimeCall]
            <> returnLines
        )
        successor

    operandCount = length . filter (== NativeCpsOperand) . nativeCpsCallParameters
    unsupportedCpsPrimitive =
      lift (Left (Arm64UnsupportedExpression ("CPS primitive call " <> name)))

renderCpsCallArguments :: Text -> [NativeCpsParameter] -> [Int] -> Int -> Either Arm64Error [Text]
renderCpsCallArguments primitive parameters operandSlots continuationSlot =
  go applyArgumentRegisters parameters operandSlots
  where
    go _ [] [] = Right []
    go (register : registers) (parameter : rest) operands =
      case parameter of
        NativeCpsMachine ->
          (("  mov " <> register <> ", x22") :) <$> go registers rest operands
        NativeCpsOperand ->
          case operands of
            slot : remaining ->
              (loadAt register "x19" slot :) <$> go registers rest remaining
            [] -> invalidSignature
        NativeCpsContinuation ->
          (loadAt register "x19" continuationSlot :) <$> go registers rest operands
    go _ _ _ = invalidSignature

    invalidSignature =
      Left (Arm64UnsupportedExpression ("invalid native CPS signature for " <> primitive))

compileDirectBinding :: ValueEnv -> [GrinVar] -> GrinExpr -> FunctionM [Text]
compileDirectBinding env vars expression =
  case expression of
    GrinConstant values
      | length vars == length values ->
          fmap concat . forM (zip vars values) $ \(var, value) -> do
            location <- liftEither (variableLocation env var)
            valueLines <- liftEither (materializeValue env value)
            pure (valueLines <> storeLocation "x0" location)
    GrinStore node -> do
      nodeLines <- liftEither (materializeNode env node)
      storeSingleResult vars nodeLines
    GrinEnsureHeap requiredWords roots
      | length vars == length roots -> do
          rootSlots <- freshSlots (length roots)
          rootLines <-
            fmap concat . forM (zip rootSlots roots) $ \(slot, root) -> do
              valueLines <- liftEither (materializeValue env root)
              pure (valueLines <> [storeAt "x0" "x19" slot])
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
    GrinStoreUnchecked node -> do
      nodeLines <- liftEither (materializeNodeUnchecked env node)
      storeSingleResult vars nodeLines
    GrinFetch _ pointer -> do
      pointerLines <- liftEither (materializeValue env pointer)
      storeSingleResult vars pointerLines
    GrinUpdate pointer value -> compileUpdateBinding False "_aihc_update" pointer value
    GrinUpdateBlackhole pointer value -> compileUpdateBinding True "_aihc_update_blackhole" pointer value
    GrinPrimitiveCall IntRep "+#" [left, right] -> do
      leftLines <- liftEither (materializeValueTo env "x9" left)
      rightLines <- liftEither (materializeValue env right)
      storeSingleResult
        vars
        ( leftLines
            <> rightLines
            <> ["  add x0, x9, x0"]
        )
    GrinPrimitiveCall IntRep "-#" [left, right] -> do
      leftLines <- liftEither (materializeValueTo env "x9" left)
      rightLines <- liftEither (materializeValue env right)
      storeSingleResult vars (leftLines <> rightLines <> ["  sub x0, x9, x0"])
    GrinPrimitiveCall IntRep "*#" [left, right] -> do
      leftLines <- liftEither (materializeValueTo env "x9" left)
      rightLines <- liftEither (materializeValue env right)
      storeSingleResult vars (leftLines <> rightLines <> ["  mul x0, x9, x0"])
    GrinPrimitiveCall IntRep "<#" [left, right] -> do
      leftLines <- liftEither (materializeValueTo env "x9" left)
      rightLines <- liftEither (materializeValue env right)
      storeSingleResult vars (leftLines <> rightLines <> ["  cmp x9, x0", "  cset x0, lt"])
    GrinPrimitiveCall IntRep "==#" [left, right] -> do
      leftLines <- liftEither (materializeValueTo env "x9" left)
      rightLines <- liftEither (materializeValue env right)
      storeSingleResult vars (leftLines <> rightLines <> ["  cmp x9, x0", "  cset x0, eq"])
    GrinPrimitiveCall IntRep "compareInt#" [left, right] -> do
      leftLines <- liftEither (materializeValueTo env "x9" left)
      rightLines <- liftEither (materializeValue env right)
      storeSingleResult vars (leftLines <> rightLines <> ["  cmp x9, x0", "  cset x0, gt", "  csinv x0, x0, xzr, ge"])
    GrinPrimitiveCall runtimeRep name arguments
      | name == "realWorld#",
        null arguments,
        null vars,
        null (runtimeRepComponents runtimeRep) ->
          pure []
    GrinPrimitiveCall _ name [value]
      | name `elem` ["charToInt#", "intToChar#", "unsafeFreezeByteArray#", "unsafeThawByteArray#"] -> do
          valueLines <- liftEither (materializeValue env value)
          storeSingleResult vars valueLines
    GrinPrimitiveCall _ name arguments
      | Just foreignCall <- nativeRuntimePrimitiveCall name -> do
          callLines <- compileForeignCallLines env foreignCall arguments
          case vars of
            [] -> pure callLines
            [_] -> storeSingleResult vars callLines
            _ -> lift (Left (Arm64UnsupportedExpression ("byte array primitive result arity " <> name)))
      | compileAllowUnsupportedPrimitives (valueCompileEnv env) ->
          pure ["  bl _aihc_unsupported_primitive"]
      | otherwise -> lift (Left (Arm64UnsupportedExpression ("primitive call " <> name)))
    GrinForeignCallExpr foreignCall arguments -> do
      callLines <- compileForeignCallLines env foreignCall arguments
      storeSingleResult vars callLines
    _ -> lift (Left (Arm64UnsupportedExpression "non-direct expression remained in a CPS bind"))
  where
    storeSingleResult resultVars lines' =
      case resultVars of
        [var] -> do
          location <- liftEither (variableLocation env var)
          pure (lines' <> storeLocation "x0" location)
        _ -> lift (Left (Arm64UnsupportedExpression "direct expression result arity"))
    compileUpdateBinding passMachine symbol pointer value = do
      pointerSlot <- freshSlot
      valueSlot <- freshSlot
      pointerLines <- liftEither (materializeValue env pointer)
      valueLines <- liftEither (materializeValue env value)
      resultLines <- storeSingleResult vars [loadAt "x0" "x19" valueSlot]
      pure
        ( pointerLines
            <> [storeAt "x0" "x19" pointerSlot]
            <> valueLines
            <> [ storeAt "x0" "x19" valueSlot,
                 loadAt (if passMachine then "x1" else "x0") "x19" pointerSlot,
                 loadAt (if passMachine then "x2" else "x1") "x19" valueSlot
               ]
            <> ["  mov x0, x22" | passMachine]
            <> [ "  bl " <> symbol
               ]
            <> resultLines
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
          argumentSlots <- mapM (const freshSlot) arguments
          argumentLines <-
            fmap concat . forM (zip arguments argumentSlots) $ \(argument, slot) -> do
              valueLines <- liftEither (materializeValue env argument)
              pure (valueLines <> [storeAt "x0" "x19" slot])
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

normalizeForeignResult :: GrinForeignType -> [Text]
normalizeForeignResult foreignType =
  case foreignType of
    GrinForeignInt -> []
    GrinForeignInt32 -> ["  sxtw x0, w0"]
    GrinForeignWord64 -> []
    GrinForeignAddr -> []

compileCase :: ValueEnv -> [Text] -> Text -> GrinValue -> GrinVar -> [GrinAlt] -> FunctionM ()
compileCase env prefix label scrutinee binder alternatives = do
  (resultLocation, scrutineeLines) <-
    case scrutinee of
      GrinVarValue var ->
        case Map.lookup var (valueLocations env) of
          Just location -> pure (location, [])
          Nothing -> materializedScrutinee
      GrinLitValue {} -> materializedScrutinee
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
    GrinDataAlt _ ->
      do
        fields <-
          fmap concat . forM (zip [0 ..] (grinAltBinders alternative)) $ \(index, binder) -> do
            if binder `Set.member` grinExprFreeVariables (grinAltRhs alternative)
              then do
                location <- liftEither (variableLocation env binder)
                pure ([loadByteOffset "x10" "x9" (8 + index * 8)] <> storeLocation "x10" location)
              else pure []
        pure (if null fields then [] else loadLocation "x9" resultLocation <> fields)
    GrinLitAlt _ -> pure []
    GrinDefaultAlt ->
      fmap concat . forM (grinAltBinders alternative) $ \binder -> do
        if binder `Set.member` grinExprFreeVariables (grinAltRhs alternative)
          then do
            location <- liftEither (variableLocation env binder)
            pure (loadLocation "x9" resultLocation <> storeLocation "x9" location)
          else pure []

caseChecks :: ValueEnv -> Location Text -> Bool -> [(GrinAlt, Text)] -> FunctionM ([Text], BlockLayout.Terminator Text)
caseChecks env resultLocation scrutineeIsPointer targets = do
  let nonDefault = [(alternative, label) | (alternative, label) <- targets, grinAltCon alternative /= GrinDefaultAlt]
      defaultTarget = [label | (alternative, label) <- targets, grinAltCon alternative == GrinDefaultAlt]
  checks <- fmap concat . forM nonDefault $ \(alternative, target) ->
    case grinAltCon alternative of
      GrinDataAlt name -> do
        if scrutineeIsPointer
          then do
            identifier <- liftEither (constructorId (valueCompileEnv env) name)
            pure
              ( loadLocation "x9" resultLocation
                  <> [ "  ldr x10, [x9, #0]",
                       "  ldr x10, [x10, #0]",
                       immediate "x11" identifier,
                       "  cmp x10, x11",
                       "  b.eq " <> target
                     ]
              )
          else lift (Left (Arm64UnsupportedExpression "constructor case on an unboxed value"))
      GrinLitAlt literal ->
        case normalizedLiteralInteger literal of
          Just integer ->
            if scrutineeIsPointer
              then lift (Left (Arm64UnsupportedExpression "literal case on a lifted value"))
              else
                pure
                  ( loadLocation "x10" resultLocation
                      <> [ immediate "x11" integer,
                           "  cmp x10, x11",
                           "  b.eq " <> target
                         ]
                  )
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
      slots <- freshSlots (length values)
      stores <-
        fmap concat . forM (zip values slots) $ \(value, slot) -> do
          lines' <- liftEither (materializeValue env value)
          pure (lines' <> [storeAt "x0" "x19" slot])
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

restoreApplyStackLines :: Int -> [Text]
restoreApplyStackLines stackBytes
  | stackBytes == 0 = []
  | otherwise = [immediate "x8" stackBytes, "  add sp, sp, x8"]

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
