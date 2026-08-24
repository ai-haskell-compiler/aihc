{-# LANGUAGE OverloadedStrings #-}

-- | Lower runtime-explicit GRIN to textual AArch64 assembly for Darwin.
-- Generated Haskell entries transfer only with branches; calls are reserved
-- for the C runtime and foreign functions.
module Aihc.Arm64.Codegen
  ( Arm64Error (..),
    LinkLayout,
    LinkInterface,
    buildLinkLayout,
    buildLinkLayoutFromInterfaces,
    compileModule,
    ObservedProgram (..),
    compileObservedFunction,
    compileProgram,
    compileProgramWithDependencies,
    extendLinkLayout,
    extendLinkLayoutWithInterface,
    extractLinkInterface,
    supportedNativePrimitiveNames,
    validateProgramPrimitives,
    validatePrimitiveNames,
  )
where

import Aihc.Arm64.Codegen.Function
import Aihc.Arm64.Codegen.Runtime
import Aihc.Grin.Cps (ContinuationFrameKind (..))
import Aihc.Grin.Gc
  ( GcGrinProgram,
    gcContinuationFrames,
    gcContinuationFunctions,
    gcFunctionContinuations,
    gcGrinProgram,
    gcUpdateFunction,
  )
import Aihc.Grin.Syntax
import Aihc.Native
  ( LinkInterface,
    LinkLayout (..),
    buildAddrLiteralPool,
    buildLinkLayout,
    buildLinkLayoutFromInterfaces,
    extendLinkLayout,
    extendLinkLayoutWithInterface,
    extractLinkInterface,
    supportedNativePrimitiveNames,
  )
import Control.Monad (forM)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

compileProgram :: Text -> GcGrinProgram -> Either Arm64Error Text
compileProgram entryName gcProgram =
  compileProgramWithDependencies (buildLinkLayout [program]) [] entryName gcProgram
  where
    program = gcGrinProgram gcProgram

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
  constructorLines <- compileConstructorInitializers compileEnv
  initLines <- compileInitializers compileEnv program
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  metadata <- renderObservedMetadata compileEnv program resultReps
  let resultCount = length resultReps
      assembly =
        T.unlines $
          mainPrologue (length globalNames)
            <> reserveLocalsLines functions
            <> constructorLines
            <> initLines
            <> makeNodeLines (InfoAddress ".Laihc_thread_done_info")
            <> [ "  mov x1, x0",
                 "  mov x0, x22",
                 "  bl _aihc_set_thread_done_continuation"
               ]
            <> makeNodeLines (InfoAddress ".Laihc_snapshot_info")
            <> [ "  mov x21, x0",
                 "  mov x0, x22",
                 "  bl _aihc_reset_allocation_count",
                 "  b " <> entryLabel,
                 ".p2align 3",
                 ".Laihc_snapshot_result:"
               ]
            <> [storeAt register "x19" index | (index, register) <- zip [0 :: Int ..] applyArgumentRegisters, index < resultCount]
            <> [ "  mov x1, x19",
                 "  mov x2, x22",
                 immediate "x0" resultCount,
                 "  bl _aihc_snapshot_dump_result",
                 "  mov w0, #0"
               ]
            <> entryEpilogue
            <> threadDoneContinuation
            <> renderCompiledSupport compileEnv functions observedRuntimeInfos
  pure ObservedProgram {observedAssembly = assembly, observedMetadataSource = metadata}
  where
    program = gcGrinProgram gcProgram
    layout = buildLinkLayout [program]
    compileEnv = (compileEnvironmentWith True (gcContinuationFrames gcProgram) layout program) {compileContinuationFunctions = gcContinuationFunctions gcProgram}
    globalNames = linkGlobalNames layout
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

-- | Compile a library SCC to relocatable assembly. The exported initializer
-- installs the unit's primitive, static, and CAF globals into the shared
-- machine table. Constructors are installed once by the executable entry unit.
compileModule :: LinkLayout -> Text -> GcGrinProgram -> Either Arm64Error Text
compileModule layout initializerSymbol gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  initLines <- compileInitializers compileEnv program
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  pure . T.unlines $
    entryPrologue initializerSymbol
      <> ["  mov x22, x0"]
      <> reserveLocalsLines functions
      <> initLines
      <> entryEpilogue
      <> renderCompiledSupport compileEnv functions []
  where
    program = gcGrinProgram gcProgram
    compileEnv =
      (compileEnvironment (gcContinuationFrames gcProgram) layout program)
        { compileAllowUnsupportedPrimitives = True,
          compileContinuationFunctions = gcContinuationFunctions gcProgram
        }

-- | Compile the user program entry unit against cached dependency modules.
-- Dependency initializers are called after constructors are installed and
-- before the user module's own globals are initialized.
compileProgramWithDependencies :: LinkLayout -> [Text] -> Text -> GcGrinProgram -> Either Arm64Error Text
compileProgramWithDependencies layout dependencyInitializers entryName gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  rootSlot <- maybe (Left (Arm64MissingEntry entryName)) Right (Map.lookup entryName globalSlots)
  constructorLines <- compileConstructorInitializers compileEnv
  initLines <- compileInitializers compileEnv program
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  updateLabel <- functionCodeLabel compileEnv (gcUpdateFunction gcProgram)
  pure . T.unlines $
    mainPrologue (length globalNames)
      <> constructorLines
      <> concatMap callInitializer dependencyInitializers
      <> initLines
      <> reserveLocalsLines functions
      <> [ "  mov x0, x22",
           immediate "x1" (7 :: Int),
           "  mov x2, xzr",
           "  mov x3, xzr",
           "  bl _aihc_ensure_heap"
         ]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_final_info")
      <> ["  mov x21, x0"]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_top_info")
      <> [ "  mov x20, x0",
           "  mov x2, x21",
           "  mov x1, #0",
           "  bl _aihc_set_field"
         ]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_update_info")
      <> [ storeAt "x0" "x19" 0,
           "  mov x2, x20",
           loadAt "x0" "x19" 0,
           "  mov x1, #0",
           "  bl _aihc_set_field",
           loadAt "x0" "x19" 0,
           "  mov x1, #1",
           "  ldr x9, [x22, #0]",
           loadAt "x2" "x9" rootSlot,
           "  bl _aihc_set_field"
         ]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_thread_done_info")
      <> [ "  mov x10, x0",
           "  mov x1, x10",
           "  mov x0, x22",
           "  bl _aihc_set_thread_done_continuation",
           "  adr x9, .Laihc_exit",
           "  str x9, [x22, #16]",
           "  ldr x9, [x22, #0]",
           loadAt applyFunctionRegister "x9" rootSlot,
           loadAt "x0" "x19" 0,
           "  ldr x21, [x0, #8]",
           "  mov x8, #1",
           "  b .Laihc_eval"
         ]
      <> [ ".p2align 3",
           ".Laihc_top_continuation:",
           "  mov x21, x0",
           "  mov x20, x1",
           "  b .Laihc_enter"
         ]
      <> threadDoneContinuation
      <> [ ".p2align 3",
           ".Laihc_final_continuation:",
           "  b .Laihc_exit"
         ]
      <> [ ".Laihc_exit:",
           "  mov w0, #0"
         ]
      <> entryEpilogue
      <> renderCompiledSupport compileEnv functions (programRuntimeInfos updateLabel)
  where
    program = gcGrinProgram gcProgram
    compileEnv = (compileEnvironment (gcContinuationFrames gcProgram) layout program) {compileContinuationFunctions = gcContinuationFunctions gcProgram}
    globalSlots = compileGlobalSlots compileEnv
    globalNames = linkGlobalNames layout
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
    callInitializer symbol =
      [ "  mov x0, x22",
        "  bl " <> symbol
      ]

compileEnvironment :: Map.Map FunctionName ContinuationFrameKind -> LinkLayout -> GrinProgram -> CompileEnv
compileEnvironment = compileEnvironmentWith False

compileEnvironmentWith :: Bool -> Map.Map FunctionName ContinuationFrameKind -> LinkLayout -> GrinProgram -> CompileEnv
compileEnvironmentWith exposeAllFunctions continuationFrames layout program =
  CompileEnv
    { compileConstructorIds = Map.fromList (zip (map fst constructors) [1 ..]),
      compileConstructorArities = Map.fromList constructors,
      compileGlobalSlots = Map.fromList (zip (linkGlobalNames layout) [0 ..]),
      compileFunctionLabels =
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
    constructorLayouts = linkConstructors layout
    constructors = [(name, length layouts) | (name, layouts) <- constructorLayouts]
    constructorIdentifiers = zip (map fst constructors) [1 ..]
    constructorInfoEntries =
      [ ( key,
          label,
          RuntimeInfo label (InfoImmediate identifier) fields remaining next Nothing Nothing (runtimeInfoKeyObjectKind key)
        )
      | ((name, layouts), (_, identifier)) <- zip constructorLayouts constructorIdentifiers,
        let arity = length layouts,
        remaining <- [arity, arity - 1 .. 0],
        let key = ConstructorRuntimeInfo name remaining
            label = constructorStageLabel identifier remaining
            fields = concat (take (arity - remaining) layouts)
            next = if remaining == 0 then Nothing else Just (constructorStageLabel identifier (remaining - 1))
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

compileConstructorInitializers :: CompileEnv -> Either Arm64Error [Text]
compileConstructorInitializers env =
  fmap concat . forM nullaryConstructors $ \name -> do
    slot <- globalSlot env name
    info <- lookupRuntimeInfoLabel env (ConstructorRuntimeInfo name 0)
    pure $ makeNodeLines (InfoAddress info) <> storeGlobal slot
  where
    nullaryConstructors = Map.keys (Map.filter (== 0) (compileConstructorArities env))

compileInitializers :: CompileEnv -> GrinProgram -> Either Arm64Error [Text]
compileInitializers env program = do
  valueGlobalLines <- compileGlobals materializeNode valueGlobals
  thunkAllocationLines <- compileGlobals allocateNode thunkGlobals
  thunkInitializationLines <- fmap concat . forM thunkGlobals $ \(name, node) -> do
    slot <- globalSlot env name
    fieldLines <- initializeNodeFields valueEnv node
    pure $
      ["  ldr x9, [x22, #0]", loadAt "x20" "x9" slot]
        <> fieldLines
  pure (thunkAllocationLines <> valueGlobalLines <> thunkInitializationLines)
  where
    (thunkGlobals, valueGlobals) = partitionGlobals (grinGlobals program)
    valueEnv = ValueEnv env Map.empty ".Laihc_initializer" (FunctionName "") [] ".Laihc_initializer"
    compileGlobals emit globals = fmap concat . forM globals $ \(name, node) -> do
      slot <- globalSlot env name
      lines' <- emit valueEnv node
      pure (lines' <> storeGlobal slot)
    partitionGlobals = foldr partitionOne ([], [])
    partitionOne binding@(_, GrinNode GrinThunk {} _) (thunks, values) = (binding : thunks, values)
    partitionOne binding (thunks, values) = (thunks, binding : values)

mainPrologue :: Int -> [Text]
mainPrologue globalCount =
  entryPrologue "_main"
    <> [ "  bl _aihc_program_arguments_initialize",
         immediate "x0" globalCount,
         "  bl _aihc_machine_new",
         "  mov x22, x0"
       ]

entryPrologue :: Text -> [Text]
entryPrologue symbol =
  [ ".section __TEXT,__text,regular,pure_instructions",
    ".p2align 2",
    ".globl " <> symbol,
    symbol <> ":",
    "  stp x29, x30, [sp, #-48]!",
    "  mov x29, sp",
    "  stp x19, x20, [sp, #16]",
    "  stp x21, x22, [sp, #32]"
  ]

entryEpilogue :: [Text]
entryEpilogue =
  [ "  ldp x21, x22, [sp, #32]",
    "  ldp x19, x20, [sp, #16]",
    "  ldp x29, x30, [sp], #48",
    "  ret"
  ]

threadDoneContinuation :: [Text]
threadDoneContinuation =
  [ ".p2align 3",
    ".Laihc_thread_done_continuation:",
    "  mov x0, x22",
    "  bl _aihc_thread_done",
    "  b .Laihc_resume"
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

renderCompiledSupport :: CompileEnv -> [CompiledFunction] -> [RuntimeInfo] -> [Text]
renderCompiledSupport env functions runtimeInfos =
  renderNativeControl
    <> concatMap compiledFunctionLines functions
    <> renderRuntimeSupport env runtimeInfos

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
      Map.fromList
        ( builtinConstructorLayouts
            <> [(name, concat argumentLayouts) | (name, argumentLayouts) <- grinConstructors program]
        )
    constructorEntries =
      [ (identifier, name, fields)
      | (name, identifier) <- Map.toAscList (compileConstructorIds env),
        Just fields <- [Map.lookup name layouts]
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
  ["static const AihcSnapshotConstructor constructors[] = {"]
    <> [ "  {"
           <> tshow identifier
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
