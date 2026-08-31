{-# LANGUAGE OverloadedStrings #-}

-- | Lower runtime-explicit GRIN to AArch64 Mach-O objects for Darwin.
-- Generated Haskell entries transfer only with branches; calls are reserved
-- for the C runtime and foreign functions.
module Aihc.Arm64.Codegen
  ( Arm64Error (..),
    compileEntryObject,
    compileModuleObject,
    ObservedProgram (..),
    compileObservedFunction,
    supportedNativePrimitiveNames,
    validateProgramPrimitives,
    validatePrimitiveNames,
  )
where

import Aihc.Arm64.Assemble
  ( Arm64Opcode (..),
    Arm64Statement,
    arm64Align,
    arm64Global,
    arm64Instruction,
    arm64Label,
    arm64Quad,
    arm64Section,
    assembleMachO,
  )
import Aihc.Arm64.Codegen.Function
import Aihc.Arm64.Codegen.Runtime
import Aihc.Grin.Cps (ContinuationFrameKind (..))
import Aihc.Grin.Gc
  ( GcGrinProgram,
    entryGcProgram,
    gcContinuationFrames,
    gcContinuationFunctions,
    gcFunctionContinuations,
    gcGrinProgram,
    gcUpdateFunction,
  )
import Aihc.Grin.Syntax
import Aihc.Native
  ( buildAddrLiteralPool,
    executableEntryName,
    renderLinkedGlobalSymbol,
    supportedNativePrimitiveNames,
  )
import Aihc.Native.Object (SectionRole (..))
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | Compile the fixed executable entry unit.
compileEntryObject :: Either Arm64Error BL.ByteString
compileEntryObject = do
  gcProgram <- either (Left . Arm64UnsupportedExpression . T.pack . show) Right entryGcProgram
  statements <- compileEntryUnit executableEntryName gcProgram
  assembleObject statements

-- | Compile a nullary function with a driver that snapshots its raw return
-- values. The driver supports cooperative scheduling but exits when the
-- observed function returns; it does not evaluate returned objects or drain
-- other runnable threads.
compileObservedFunction :: FunctionName -> GcGrinProgram -> Either Arm64Error ObservedProgram
compileObservedFunction entryName gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  validateProgramPrimitives program
  entryFunction <-
    maybe (Left (Arm64MissingFunction entryName)) Right $
      findFunction entryName (grinFunctions program)
  case Map.lookup entryName (gcFunctionContinuations gcProgram) of
    Just continuation
      | grinFunctionParameters entryFunction == [continuation] -> pure ()
    _ -> Left (Arm64UnsupportedExpression "observed entry function must have only its CPS continuation")
  entryLabel <- functionCodeLabel compileEnv entryName
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  staticGlobals <- renderStaticGlobals compileEnv program
  metadata <- renderObservedMetadata compileEnv program resultReps
  let resultCount = length resultReps
      statements =
        mainPrologue 0
          <> [arm64Instruction ArmMov ["x0", "x22"], arm64Instruction ArmBl ["_aihc_alloc_linked_locals"], arm64Instruction ArmMov ["x19", "x0"]]
          <> makeNodeLines (InfoAddress ".Laihc_thread_done_info")
          <> [ arm64Instruction ArmMov ["x1", "x0"],
               arm64Instruction ArmMov ["x0", "x22"],
               arm64Instruction ArmBl ["_aihc_set_thread_done_continuation"]
             ]
          <> makeNodeLines (InfoAddress ".Laihc_snapshot_info")
          <> [ arm64Instruction ArmMov ["x21", "x0"],
               arm64Instruction ArmMov ["x0", "x22"],
               arm64Instruction ArmBl ["_aihc_reset_allocation_count"],
               arm64Instruction ArmB [entryLabel],
               arm64Align 3,
               arm64Label ".Laihc_snapshot_result"
             ]
          <> [storeAt register "x19" index | (index, register) <- zip [0 :: Int ..] applyArgumentRegisters, index < resultCount]
          <> [ arm64Instruction ArmMov ["x1", "x19"],
               arm64Instruction ArmMov ["x2", "x22"],
               immediate "x0" resultCount,
               arm64Instruction ArmBl ["_aihc_snapshot_dump_result"],
               arm64Instruction ArmMov ["w0", "#0"]
             ]
          <> entryEpilogue
          <> threadDoneContinuation
          <> staticGlobals
          <> renderLinkedLocals functions
          <> renderCompiledSupport compileEnv functions observedRuntimeInfos
  object <- assembleObject statements
  pure ObservedProgram {observedObject = object, observedMetadataSource = metadata}
  where
    program = gcGrinProgram gcProgram
    compileEnv = (compileEnvironmentWith True (gcContinuationFrames gcProgram) program) {compileContinuationFunctions = gcContinuationFunctions gcProgram}
    resultReps =
      maybe [] (runtimeRepComponents . grinFunctionResultRep) $
        findFunction entryName (grinFunctions program)
    observedRuntimeInfos =
      threadDoneRuntimeInfos
        <> continuationRuntimeInfos
          ContinuationFrameStop
          ".Laihc_snapshot_info"
          ".Laihc_snapshot_applied_info"
          ".Laihc_snapshot_result"
          []
          resultReps

-- | Compile one library module to a relocatable object.
compileModuleObject :: GcGrinProgram -> Either Arm64Error BL.ByteString
compileModuleObject gcProgram = compileModuleStatements gcProgram >>= assembleObject

assembleObject :: [Arm64Statement] -> Either Arm64Error BL.ByteString
assembleObject = either (Left . Arm64ObjectError . T.pack . show) pure . assembleMachO

compileModuleStatements :: GcGrinProgram -> Either Arm64Error [Arm64Statement]
compileModuleStatements gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  staticGlobals <- renderStaticGlobals compileEnv program
  pure (staticGlobals <> renderLinkedLocals functions <> renderCompiledSupport compileEnv functions [])
  where
    program = gcGrinProgram gcProgram
    compileEnv =
      (compileEnvironment (gcContinuationFrames gcProgram) program)
        { compileAllowUnsupportedPrimitives = True,
          compileContinuationFunctions = gcContinuationFunctions gcProgram
        }

compileEntryUnit :: Text -> GcGrinProgram -> Either Arm64Error [Arm64Statement]
compileEntryUnit entryName gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  staticGlobals <- renderStaticGlobals compileEnv program
  updateLabel <- functionCodeLabel compileEnv (gcUpdateFunction gcProgram)
  pure $
    mainPrologue 0
      <> [arm64Instruction ArmMov ["x0", "x22"], arm64Instruction ArmBl ["_aihc_alloc_linked_locals"], arm64Instruction ArmMov ["x19", "x0"]]
      <> [ arm64Instruction ArmMov ["x0", "x22"],
           immediate "x1" (7 :: Int),
           arm64Instruction ArmMov ["x2", "xzr"],
           arm64Instruction ArmMov ["x3", "xzr"],
           arm64Instruction ArmBl ["_aihc_ensure_heap"]
         ]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_final_info")
      <> [arm64Instruction ArmMov ["x21", "x0"]]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_top_info")
      <> [ arm64Instruction ArmMov ["x20", "x0"],
           arm64Instruction ArmMov ["x2", "x21"],
           arm64Instruction ArmMov ["x1", "#0"],
           arm64Instruction ArmBl ["_aihc_set_field"]
         ]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_update_info")
      <> [ storeAt "x0" "x19" 0,
           arm64Instruction ArmMov ["x2", "x20"],
           loadAt "x0" "x19" 0,
           arm64Instruction ArmMov ["x1", "#0"],
           arm64Instruction ArmBl ["_aihc_set_field"]
         ]
      <> address "x2" ("_" <> renderLinkedGlobalSymbol entryName)
      <> [ loadAt "x0" "x19" 0,
           arm64Instruction ArmMov ["x1", "#1"],
           arm64Instruction ArmBl ["_aihc_set_field"]
         ]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_thread_done_info")
      <> [ arm64Instruction ArmMov ["x10", "x0"],
           arm64Instruction ArmMov ["x1", "x10"],
           arm64Instruction ArmMov ["x0", "x22"],
           arm64Instruction ArmBl ["_aihc_set_thread_done_continuation"],
           arm64Instruction ArmAdr ["x9", ".Laihc_exit"],
           arm64Instruction ArmStr ["x9", "[x22, #16]"]
         ]
      <> address applyFunctionRegister ("_" <> renderLinkedGlobalSymbol entryName)
      <> [ loadAt "x0" "x19" 0,
           arm64Instruction ArmLdr ["x21", "[x0, #8]"],
           arm64Instruction ArmMov ["x8", "#1"],
           arm64Instruction ArmB [".Laihc_eval"]
         ]
      <> [ arm64Align 3,
           arm64Label ".Laihc_top_continuation",
           arm64Instruction ArmMov ["x21", "x0"],
           arm64Instruction ArmMov ["x20", "x1"],
           arm64Instruction ArmB [".Laihc_enter"]
         ]
      <> threadDoneContinuation
      <> [ arm64Align 3,
           arm64Label ".Laihc_final_continuation",
           arm64Instruction ArmB [".Laihc_exit"]
         ]
      <> [ arm64Label ".Laihc_exit",
           arm64Instruction ArmMov ["w0", "#0"]
         ]
      <> entryEpilogue
      <> staticGlobals
      <> renderLinkedLocals functions
      <> renderCompiledSupport compileEnv functions (programRuntimeInfos updateLabel)
  where
    program = gcGrinProgram gcProgram
    compileEnv = (compileEnvironmentWith False (gcContinuationFrames gcProgram) program) {compileContinuationFunctions = gcContinuationFunctions gcProgram}
    pointerRep = BoxedRep Lifted
    programRuntimeInfos updateLabel =
      continuationRuntimeInfos
        ContinuationFrameStop
        ".Laihc_final_info"
        ".Laihc_final_applied_info"
        ".Laihc_final_continuation"
        []
        [pointerRep]
        <> continuationRuntimeInfos
          ContinuationFrameNormal
          ".Laihc_top_info"
          ".Laihc_top_applied_info"
          ".Laihc_top_continuation"
          [pointerRep]
          [pointerRep]
        <> continuationRuntimeInfos
          ContinuationFrameUpdate
          ".Laihc_update_info"
          ".Laihc_update_applied_info"
          updateLabel
          [pointerRep, pointerRep]
          [pointerRep]
        <> threadDoneRuntimeInfos

compileEnvironment :: Map.Map FunctionName ContinuationFrameKind -> GrinProgram -> CompileEnv
compileEnvironment = compileEnvironmentWith False

compileEnvironmentWith :: Bool -> Map.Map FunctionName ContinuationFrameKind -> GrinProgram -> CompileEnv
compileEnvironmentWith exposeAllFunctions continuationFrames program =
  CompileEnv
    { compileFunctionLabels =
        functionLabelMap,
      compileAddrLiteralLabels =
        Map.fromList (buildAddrLiteralPool program),
      compileNodeInfoLabels = constructorInfoLabels <> functionInfoLabels,
      compileRuntimeInfos = map third constructorInfoEntries <> functionInfos,
      compileContinuationFunctions = Set.empty,
      compileExposeAllFunctions = exposeAllFunctions,
      compileAllowUnsupportedPrimitives = False
    }
  where
    constructorLayouts = grinConstructors program
    constructorInfoEntries =
      [ ( key,
          label,
          RuntimeInfo label (InfoConstructor (constructorStageLabel name 0)) fields remaining next Nothing Nothing (runtimeInfoKeyObjectKind key)
        )
      | (name, layouts) <- constructorLayouts,
        let arity = length layouts,
        remaining <- [arity, arity - 1 .. 0],
        let key = ConstructorRuntimeInfo name remaining
            label = constructorStageLabel name remaining
            fields = concat (take (arity - remaining) layouts)
            next = if remaining == 0 then Nothing else Just (constructorStageLabel name (remaining - 1))
      ]
    constructorInfoLabels = Map.fromList [(key, label) | (key, label, _) <- constructorInfoEntries]
    functionLabels =
      [ (grinFunctionName function, localFunctionLabelWith exposeAllFunctions index function)
      | (index, function) <- zip [0 ..] (grinFunctions program)
      ]
    functionLabelMap = Map.fromList functionLabels
    functionInfoKeys =
      [ (key, functionName)
      | key <- Set.toAscList (Set.fromList (concatMap runtimeInfoKeyStages (programNodes program))),
        Just functionName <- [runtimeInfoFunctionName key],
        functionName `Map.member` functionLabelMap
      ]
    functionInfoLabels =
      Map.fromList
        [ (key, ".Laihc_function_info_" <> tshow index)
        | (index, (key, _)) <- zip [0 :: Int ..] functionInfoKeys
        ]
    functionInfos =
      [ RuntimeInfo
          label
          (InfoAddress target)
          (runtimeInfoKeyFields key)
          (runtimeInfoKeyRemainingArity key)
          (runtimeInfoKeyNext key >>= (`Map.lookup` functionInfoLabels))
          ( case key of
              ClosureRuntimeInfo _ fields [supplied] ->
                Just (RuntimeEnter target (length fields) (length supplied))
              ThunkRuntimeInfo _ fields ->
                Just (RuntimeEnter target (length fields) 0)
              _ -> Nothing
          )
          (Map.lookup functionName continuationFrames)
          (runtimeInfoKeyObjectKind key)
      | (key, functionName) <- functionInfoKeys,
        let label = functionInfoLabels Map.! key
            target = functionLabelMap Map.! functionName
      ]
    third (_, _, value) = value

renderStaticGlobals :: CompileEnv -> GrinProgram -> Either Arm64Error [Arm64Statement]
renderStaticGlobals env program = fmap concat (mapM renderGlobal globals)
  where
    declaredGlobals = grinGlobals program
    declaredNames = map fst declaredGlobals
    constructorLayouts = grinConstructors program
    implicitConstructors =
      [ (name, GrinNode (GrinConstructor name 0) [])
      | (name, layouts) <- constructorLayouts,
        null layouts,
        name `notElem` declaredNames
      ]
    globals = declaredGlobals <> implicitConstructors
    renderGlobal (name, node) = do
      info <- staticNodeInfo node
      fields <- mapM renderStaticValue (grinNodeFields node)
      let symbol = "_" <> renderLinkedGlobalSymbol name
          payload = if null fields && isThunk node then [arm64Quad "0"] else fields
      pure $
        [ arm64Section DataSection,
          arm64Align 3,
          arm64Global symbol,
          arm64Label symbol,
          arm64Quad info
        ]
          <> payload
          <> [ arm64Section RootsSection,
               arm64Align 3,
               arm64Quad symbol
             ]
    staticNodeInfo node =
      case grinNodeTag node of
        GrinConstructor name remaining -> pure (constructorStageLabel name remaining)
        GrinClosure functionName layouts -> lookupRuntimeInfoLabel env (ClosureRuntimeInfo functionName fields layouts)
        GrinThunk functionName -> lookupRuntimeInfoLabel env (ThunkRuntimeInfo functionName fields)
      where
        fields = map grinValueRuntimeRep (grinNodeFields node)
    renderStaticValue value =
      case value of
        GrinVarValue var -> pure (arm64Quad ("_" <> renderLinkedGlobalSymbol (grinVarName var)))
        GrinGlobalValue name -> pure (arm64Quad ("_" <> renderLinkedGlobalSymbol name))
        GrinLitValue literal ->
          case literal of
            GrinLitAddr bytes ->
              maybe (Left (Arm64UnsupportedValue "unregistered Addr# literal")) (pure . arm64Quad) (Map.lookup bytes (compileAddrLiteralLabels env))
            _ -> maybe (Left (Arm64UnsupportedValue "string literal")) (pure . arm64Quad . T.pack . show) (normalizedLiteralInteger literal)
    isThunk node =
      case grinNodeTag node of
        GrinThunk {} -> True
        _ -> False

renderLinkedLocals :: [CompiledFunction] -> [Arm64Statement]
renderLinkedLocals functions =
  [ arm64Section LocalsSection,
    arm64Align 3,
    arm64Quad (tshow (maximum (2 : map compiledFunctionSlots functions)))
  ]

mainPrologue :: Int -> [Arm64Statement]
mainPrologue globalCount =
  entryPrologue "_main"
    <> [ arm64Instruction ArmBl ["_aihc_program_arguments_initialize"],
         immediate "x0" globalCount,
         arm64Instruction ArmBl ["_aihc_machine_new"],
         arm64Instruction ArmMov ["x22", "x0"]
       ]

entryPrologue :: Text -> [Arm64Statement]
entryPrologue symbol =
  [ arm64Section TextSection,
    arm64Align 2,
    arm64Global symbol,
    arm64Label symbol,
    arm64Instruction ArmStp ["x29", "x30", "[sp, #-48]!"],
    arm64Instruction ArmMov ["x29", "sp"],
    arm64Instruction ArmStp ["x19", "x20", "[sp, #16]"],
    arm64Instruction ArmStp ["x21", "x22", "[sp, #32]"]
  ]

entryEpilogue :: [Arm64Statement]
entryEpilogue =
  [ arm64Instruction ArmLdp ["x21", "x22", "[sp, #32]"],
    arm64Instruction ArmLdp ["x19", "x20", "[sp, #16]"],
    arm64Instruction ArmLdp ["x29", "x30", "[sp]", "#48"],
    arm64Instruction ArmRet []
  ]

threadDoneContinuation :: [Arm64Statement]
threadDoneContinuation =
  [ arm64Align 3,
    arm64Label ".Laihc_thread_done_continuation",
    arm64Instruction ArmMov ["x0", "x22"],
    arm64Instruction ArmBl ["_aihc_thread_done"],
    arm64Instruction ArmB [".Laihc_resume"]
  ]

threadDoneRuntimeInfos :: [RuntimeInfo]
threadDoneRuntimeInfos =
  continuationRuntimeInfos
    ContinuationFrameStop
    ".Laihc_thread_done_info"
    ".Laihc_thread_done_applied_info"
    ".Laihc_thread_done_continuation"
    []
    [BoxedRep Lifted]

renderCompiledSupport :: CompileEnv -> [CompiledFunction] -> [RuntimeInfo] -> [Arm64Statement]
renderCompiledSupport env functions runtimeInfos =
  renderNativeControl
    <> concatMap renderFunction functions
    <> renderRuntimeSupport env runtimeInfos
  where
    renderFunction = compiledFunctionLines

-- | Reject primitives that reachable native code would not execute correctly.
-- Relocatable library objects may carry dormant primitive declarations, but
-- the linked program is checked after whole-program dead-code elimination.
validateProgramPrimitives :: GrinProgram -> Either Arm64Error ()
validateProgramPrimitives program =
  validatePrimitiveNames (map (grinVarName . fst) (grinPrimitives program))

validatePrimitiveNames :: [Text] -> Either Arm64Error ()
validatePrimitiveNames = mapM_ (validatePrimitiveName False)

validatePrimitiveName :: Bool -> Text -> Either Arm64Error ()
validatePrimitiveName allowUnsupported name
  | name `elem` supportedNativePrimitiveNames = Right ()
  | allowUnsupported = Right ()
  | otherwise = Left (Arm64UnsupportedPrimitive name)

validateRuntimeRep :: GrinRep -> Either Arm64Error ()
validateRuntimeRep runtimeRep =
  case runtimeRep of
    VecRep {} -> Left (Arm64UnsupportedRuntimeRep runtimeRep)
    TupleRep fieldReps -> mapM_ validateRuntimeRep fieldReps
    SumRep alternativeReps -> mapM_ validateRuntimeRep alternativeReps
    _ -> Right ()

programRuntimeReps :: GrinProgram -> [GrinRep]
programRuntimeReps program =
  concatMap (concat . snd) (grinConstructors program)
    <> map (grinVarRuntimeRep . fst) (grinPrimitives program)
    <> concatMap bindingRuntimeReps (grinGlobals program)
    <> concatMap functionRuntimeReps (grinFunctions program)
  where
    bindingRuntimeReps (_, node) = nodeRuntimeReps node
    functionRuntimeReps function =
      grinFunctionResultRep function
        : map grinVarRuntimeRep (grinFunctionParameters function)
          <> exprRuntimeReps (grinFunctionBody function)

programNodes :: GrinProgram -> [GrinNode]
programNodes program =
  map snd (grinGlobals program)
    <> concatMap (exprNodes . grinFunctionBody) (grinFunctions program)

exprNodes :: GrinExpr -> [GrinNode]
exprNodes expression =
  case expression of
    GrinBind _ valueExpression body -> exprNodes valueExpression <> exprNodes body
    GrinStore node -> [node]
    GrinStoreUnchecked node -> [node]
    GrinStoreRec bindings body -> storedNodes bindings body
    GrinStoreRecUnchecked bindings body -> storedNodes bindings body
    GrinCase _ _ alternatives -> concatMap (exprNodes . grinAltRhs) alternatives
    GrinConstant {} -> []
    GrinEnsureHeap {} -> []
    GrinFetch {} -> []
    GrinUpdate {} -> []
    GrinUpdateBlackhole {} -> []
    GrinEval {} -> []
    GrinCpsEval {} -> []
    GrinCall {} -> []
    GrinPrimitiveCall {} -> []
    GrinCpsPrimitiveCall {} -> []
    GrinApply {} -> []
    GrinCpsApply {} -> []
    GrinContinue {} -> []
    GrinCpsRaise {} -> []
    GrinHalt {} -> []
    GrinExit {} -> []
    GrinThrow {} -> []
    GrinCatch {} -> []
    GrinForeignCallExpr {} -> []
  where
    storedNodes bindings body = map snd bindings <> exprNodes body

exprRuntimeReps :: GrinExpr -> [GrinRep]
exprRuntimeReps expression =
  case expression of
    GrinConstant values -> concatMap valueRuntimeReps values
    GrinBind vars valueExpression body ->
      map grinVarRuntimeRep vars <> exprRuntimeReps valueExpression <> exprRuntimeReps body
    GrinStore node -> nodeRuntimeReps node
    GrinEnsureHeap requiredWords roots -> valueRuntimeReps requiredWords <> concatMap valueRuntimeReps roots
    GrinStoreUnchecked node -> nodeRuntimeReps node
    GrinStoreRec bindings body -> storedRuntimeReps bindings body
    GrinStoreRecUnchecked bindings body -> storedRuntimeReps bindings body
    GrinFetch runtimeRep pointer -> runtimeRep : valueRuntimeReps pointer
    GrinUpdate pointer value -> updatedRuntimeReps pointer value
    GrinUpdateBlackhole pointer value -> updatedRuntimeReps pointer value
    GrinEval runtimeRep value -> runtimeRep : valueRuntimeReps value
    GrinCpsEval runtimeRep value continuation updateContinuation ->
      runtimeRep : concatMap valueRuntimeReps [value, continuation, updateContinuation]
    GrinCall runtimeRep _ arguments -> runtimeRep : concatMap valueRuntimeReps arguments
    GrinPrimitiveCall runtimeRep _ arguments -> runtimeRep : concatMap valueRuntimeReps arguments
    GrinCpsPrimitiveCall runtimeRep _ arguments continuation ->
      runtimeRep : concatMap valueRuntimeReps arguments <> valueRuntimeReps continuation
    GrinApply runtimeRep function arguments ->
      runtimeRep : valueRuntimeReps function <> concatMap valueRuntimeReps arguments
    GrinCpsApply runtimeRep function arguments continuation ->
      runtimeRep : valueRuntimeReps function <> concatMap valueRuntimeReps arguments <> valueRuntimeReps continuation
    GrinContinue continuation values -> valueRuntimeReps continuation <> concatMap valueRuntimeReps values
    GrinCpsRaise exception continuation -> valueRuntimeReps exception <> valueRuntimeReps continuation
    GrinHalt values -> concatMap valueRuntimeReps values
    GrinExit status -> valueRuntimeReps status
    GrinCase scrutinee binder alternatives ->
      valueRuntimeReps scrutinee <> (grinVarRuntimeRep binder : concatMap altRuntimeReps alternatives)
    GrinThrow exception -> valueRuntimeReps exception
    GrinCatch runtimeRep action handler state -> runtimeRep : concatMap valueRuntimeReps (action : handler : state)
    GrinForeignCallExpr foreignCall arguments ->
      grinForeignCallResultReps (grinForeignCallSignature foreignCall) <> concatMap valueRuntimeReps arguments
  where
    altRuntimeReps alternative =
      map grinVarRuntimeRep (grinAltBinders alternative) <> exprRuntimeReps (grinAltRhs alternative)
    storedRuntimeReps bindings body =
      concatMap (\(var, node) -> grinVarRuntimeRep var : nodeRuntimeReps node) bindings
        <> exprRuntimeReps body
    updatedRuntimeReps pointer value = valueRuntimeReps pointer <> valueRuntimeReps value

valueRuntimeReps :: GrinValue -> [GrinRep]
valueRuntimeReps value = [grinValueRuntimeRep value]

nodeRuntimeReps :: GrinNode -> [GrinRep]
nodeRuntimeReps node = concatMap valueRuntimeReps (grinNodeFields node)

findFunction :: FunctionName -> [GrinFunction] -> Maybe GrinFunction
findFunction name =
  foldr
    (\function rest -> if grinFunctionName function == name then Just function else rest)
    Nothing

renderObservedMetadata :: CompileEnv -> GrinProgram -> [GrinRep] -> Either Arm64Error Text
renderObservedMetadata env program resultReps = do
  renderedResultReps <- mapM snapshotRepName resultReps
  constructors <- mapM renderConstructorDescriptor constructorEntries
  functions <- mapM renderFunctionDescriptor functionEntries
  pure . T.unlines $
    [ "#include \"aihc_snapshot.h\"",
      "#include <stddef.h>",
      ""
    ]
      <> map renderFunctionDeclaration functions
      <> [""]
      <> concatMap renderConstructorRepDeclaration constructors
      <> concatMap renderFunctionRepDeclaration functions
      <> renderRepDeclaration "result_reps" renderedResultReps
      <> renderConstructorTable constructors
      <> renderFunctionTable functions
      <> [ "void aihc_snapshot_dump_result(uint64_t count, const AihcSlot *values, const AihcMachine *machine) {",
           "  aihc_snapshot_dump(count, values, " <> pointerOrNull renderedResultReps "result_reps" <> ",",
           "                     aihc_allocation_count(machine),",
           "                     " <> tshow (length constructors) <> ", " <> pointerOrNull constructors "constructors" <> ",",
           "                     " <> tshow (length functions) <> ", " <> pointerOrNull functions "functions" <> ");",
           "}"
         ]
  where
    layouts =
      Map.fromList [(name, concat argumentLayouts) | (name, argumentLayouts) <- grinConstructors program]
    constructorEntries =
      [ (index, name, fields)
      | (index, (name, fields)) <- zip [0 :: Int ..] (Map.toAscList layouts)
      ]
    localFunctionEntries =
      [ (grinFunctionName function, map grinVarRuntimeRep (grinFunctionParameters function))
      | function <- grinFunctions program
      ]
    functionEntries = localFunctionEntries

    renderConstructorDescriptor (identifier, name, fields) = do
      reps <- mapM snapshotRepName fields
      pure (identifier, name, reps)

    renderFunctionDescriptor (name, parameters) = do
      label <- functionCodeLabel env name
      reps <- mapM snapshotRepName parameters
      pure (name, label, reps)

renderFunctionDeclaration :: (FunctionName, Text, [Text]) -> Text
renderFunctionDeclaration (_, label, _) =
  "extern void " <> cSymbol label <> "(void);"

renderConstructorRepDeclaration :: (Int, Text, [Text]) -> [Text]
renderConstructorRepDeclaration (identifier, _, reps) =
  renderRepDeclaration ("constructor_reps_" <> tshow identifier) reps

renderFunctionRepDeclaration :: (FunctionName, Text, [Text]) -> [Text]
renderFunctionRepDeclaration (_, label, reps) =
  renderRepDeclaration ("function_reps_" <> cSymbol label) reps

renderRepDeclaration :: Text -> [Text] -> [Text]
renderRepDeclaration _ [] = []
renderRepDeclaration name reps =
  [ "static const AihcSnapshotRep "
      <> name
      <> "[] = {"
      <> T.intercalate ", " reps
      <> "};"
  ]

renderConstructorTable :: [(Int, Text, [Text])] -> [Text]
renderConstructorTable [] = []
renderConstructorTable constructors =
  ["extern const char " <> cSymbol (constructorStageLabel name 0) <> "[];" | (_, name, _) <- constructors]
    <> ["static const AihcSnapshotConstructor constructors[] = {"]
    <> [ "  {"
           <> "(uintptr_t)&"
           <> cSymbol (constructorStageLabel name 0)
           <> ", "
           <> cString name
           <> ", "
           <> tshow (length reps)
           <> ", "
           <> pointerOrNull reps ("constructor_reps_" <> tshow identifier)
           <> "},"
       | (identifier, name, reps) <- constructors
       ]
    <> ["};"]

renderFunctionTable :: [(FunctionName, Text, [Text])] -> [Text]
renderFunctionTable [] = []
renderFunctionTable functions =
  ["static const AihcSnapshotFunction functions[] = {"]
    <> [ "  {(uintptr_t)&"
           <> cSymbol label
           <> ", "
           <> cString (unFunctionName name)
           <> ", "
           <> tshow (length reps)
           <> ", "
           <> pointerOrNull reps ("function_reps_" <> cSymbol label)
           <> "},"
       | (name, label, reps) <- functions
       ]
    <> ["};"]

snapshotRepName :: GrinRep -> Either Arm64Error Text
snapshotRepName runtimeRep =
  case runtimeRep of
    BoxedRep {} -> pure "AIHC_SNAPSHOT_POINTER"
    SumRep {} -> pure "AIHC_SNAPSHOT_POINTER"
    IntRep -> pure "AIHC_SNAPSHOT_INT"
    Int8Rep -> pure "AIHC_SNAPSHOT_INT8"
    Int16Rep -> pure "AIHC_SNAPSHOT_INT16"
    Int32Rep -> pure "AIHC_SNAPSHOT_INT32"
    Int64Rep -> pure "AIHC_SNAPSHOT_INT64"
    WordRep -> pure "AIHC_SNAPSHOT_WORD"
    Word8Rep -> pure "AIHC_SNAPSHOT_WORD8"
    Word16Rep -> pure "AIHC_SNAPSHOT_WORD16"
    Word32Rep -> pure "AIHC_SNAPSHOT_WORD32"
    Word64Rep -> pure "AIHC_SNAPSHOT_WORD64"
    AddrRep -> pure "AIHC_SNAPSHOT_ADDR"
    FloatRep -> pure "AIHC_SNAPSHOT_FLOAT"
    DoubleRep -> pure "AIHC_SNAPSHOT_DOUBLE"
    _ -> Left (Arm64UnsupportedRuntimeRep runtimeRep)

pointerOrNull :: [value] -> Text -> Text
pointerOrNull values name
  | null values = "NULL"
  | otherwise = name

cSymbol :: Text -> Text
cSymbol = T.drop 1

cString :: Text -> Text
cString value = "\"" <> T.concatMap escape value <> "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape character = T.singleton character
