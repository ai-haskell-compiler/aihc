{-# LANGUAGE OverloadedStrings #-}

-- | Lower runtime-explicit GRIN to AMD64 ELF objects for Linux.
-- Generated Haskell entries transfer only with branches; calls are reserved
-- for the C runtime and foreign functions.
module Aihc.Amd64.Codegen
  ( Amd64Error (..),
    compileEntryObject,
    compileModuleObject,
    ObservedProgram (..),
    compileObservedFunction,
    supportedNativePrimitiveNames,
    validateProgramPrimitives,
    validatePrimitiveNames,
  )
where

import Aihc.Amd64.Assemble (assembleElf)
import Aihc.Amd64.Codegen.Function
import Aihc.Amd64.Codegen.Runtime
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
import Data.ByteString.Lazy qualified as BL
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | Compile the fixed executable entry unit.
compileEntryObject :: Either Amd64Error BL.ByteString
compileEntryObject = do
  gcProgram <- either (Left . Amd64UnsupportedExpression . T.pack . show) Right entryGcProgram
  statements <- compileEntryUnit executableEntryName gcProgram
  assembleObject statements

-- | Compile a nullary function with a driver that snapshots its raw return
-- values. The driver supports cooperative scheduling but exits when the
-- observed function returns; it does not evaluate returned objects or drain
-- other runnable threads.
compileObservedFunction :: FunctionName -> GcGrinProgram -> Either Amd64Error ObservedProgram
compileObservedFunction entryName gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  validateProgramPrimitives program
  entryFunction <-
    maybe (Left (Amd64MissingFunction entryName)) Right $
      findFunction entryName (grinFunctions program)
  case Map.lookup entryName (gcFunctionContinuations gcProgram) of
    Just continuation
      | grinFunctionParameters entryFunction == [continuation] -> pure ()
    _ -> Left (Amd64UnsupportedExpression "observed entry function must have only its CPS continuation")
  entryLabel <- functionCodeLabel compileEnv entryName
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  staticGlobals <- renderStaticGlobals compileEnv program
  metadata <- renderObservedMetadata compileEnv program resultReps
  let resultCount = length resultReps
      statements =
        mainPrologue 0
          <> ["  mov rdi, r15", "  call aihc_alloc_linked_locals", "  mov r14, rax"]
          <> makeNodeLines (InfoAddress ".Laihc_thread_done_info")
          <> [ "  mov rdi, r15",
               "  mov rsi, rax",
               "  call aihc_set_thread_done_continuation"
             ]
          <> makeNodeLines (InfoAddress ".Laihc_snapshot_info")
          <> [ "  mov r13, rax",
               "  mov rdi, r15",
               "  call aihc_reset_allocation_count",
               "  jmp " <> entryLabel,
               ".p2align 3",
               ".Laihc_snapshot_result:"
             ]
          <> [storeAt register "r14" index | (index, register) <- zip [0 :: Int ..] applyArgumentRegisters, index < resultCount]
          <> [ "  mov rsi, r14",
               "  mov rdx, r15",
               immediate "rdi" resultCount,
               "  call aihc_snapshot_dump_result",
               "  xor eax, eax"
             ]
          <> mainEpilogue
          <> threadDoneContinuation
          <> staticGlobals
          <> renderLinkedLocals functions
          <> renderCompiledSupport compileEnv functions observedRuntimeInfos
          <> nonExecutableStack
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

-- | Reject primitives that reachable native code would not execute correctly.
-- Relocatable library objects may carry dormant primitive declarations, but
-- the linked program is checked after whole-program dead-code elimination.
validateProgramPrimitives :: GrinProgram -> Either Amd64Error ()
validateProgramPrimitives program =
  validatePrimitiveNames (map (grinVarName . fst) (grinPrimitives program))

validatePrimitiveNames :: [Text] -> Either Amd64Error ()
validatePrimitiveNames = mapM_ (validatePrimitiveName False)

-- | Compile one library module to a relocatable object.
compileModuleObject :: GcGrinProgram -> Either Amd64Error BL.ByteString
compileModuleObject gcProgram = compileModuleStatements gcProgram >>= assembleObject

assembleObject :: [Text] -> Either Amd64Error BL.ByteString
assembleObject = either (Left . Amd64ObjectError . T.pack . show) pure . assembleElf

compileModuleStatements :: GcGrinProgram -> Either Amd64Error [Text]
compileModuleStatements gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  staticGlobals <- renderStaticGlobals compileEnv program
  pure $
    [".intel_syntax noprefix"]
      <> staticGlobals
      <> renderLinkedLocals functions
      <> renderCompiledSupport compileEnv functions []
      <> nonExecutableStack
  where
    program = gcGrinProgram gcProgram
    compileEnv =
      (compileEnvironment (gcContinuationFrames gcProgram) program)
        { compileAllowUnsupportedPrimitives = True,
          compileContinuationFunctions = gcContinuationFunctions gcProgram
        }

compileEntryUnit :: Text -> GcGrinProgram -> Either Amd64Error [Text]
compileEntryUnit entryName gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  staticGlobals <- renderStaticGlobals compileEnv program
  updateLabel <- functionCodeLabel compileEnv (gcUpdateFunction gcProgram)
  pure $
    mainPrologue 0
      <> ["  mov rdi, r15", "  call aihc_alloc_linked_locals", "  mov r14, rax"]
      <> [ "  mov rdi, r15",
           immediate "rsi" (7 :: Int),
           "  xor edx, edx",
           "  xor ecx, ecx",
           "  call aihc_ensure_heap"
         ]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_final_info")
      <> ["  mov r13, rax"]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_top_info")
      <> [ "  mov r12, rax",
           "  mov rdi, r12",
           "  xor esi, esi",
           "  mov rdx, r13",
           "  call aihc_set_field"
         ]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_update_info")
      <> [ storeAt "rax" "r14" 0,
           "  mov rdx, r12",
           loadAt "rdi" "r14" 0,
           "  xor esi, esi",
           "  call aihc_set_field",
           loadAt "rdi" "r14" 0,
           "  mov esi, 1",
           address "rdx" (renderLinkedGlobalSymbol entryName),
           "  call aihc_set_field"
         ]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_thread_done_info")
      <> [ "  mov r10, rax",
           "  mov rsi, r10",
           "  mov rdi, r15",
           "  call aihc_set_thread_done_continuation",
           address "r11" ".Laihc_exit",
           "  mov QWORD PTR [r15 + 16], r11",
           address applyFunctionRegister (renderLinkedGlobalSymbol entryName),
           loadAt "rax" "r14" 0,
           "  mov r13, QWORD PTR [rax + 8]",
           "  mov r11, 1",
           "  jmp .Laihc_eval"
         ]
      <> [ ".p2align 3",
           ".Laihc_top_continuation:",
           "  mov r13, rax",
           "  mov r12, rdi",
           "  jmp .Laihc_enter"
         ]
      <> threadDoneContinuation
      <> [ ".p2align 3",
           ".Laihc_final_continuation:",
           "  jmp .Laihc_exit"
         ]
      <> [ ".Laihc_exit:",
           "  xor eax, eax"
         ]
      <> mainEpilogue
      <> staticGlobals
      <> renderLinkedLocals functions
      <> renderCompiledSupport compileEnv functions (programRuntimeInfos updateLabel)
      <> nonExecutableStack
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

mainPrologue :: Int -> [Text]
mainPrologue globalCount =
  entryPrologue "main"
    <> [ "  call aihc_program_arguments_initialize",
         immediate "rdi" globalCount,
         "  call aihc_machine_new",
         "  mov r15, rax"
       ]

entryPrologue :: Text -> [Text]
entryPrologue symbol =
  [ ".intel_syntax noprefix",
    ".text",
    ".p2align 4",
    ".globl " <> symbol,
    symbol <> ":",
    "  push rbp",
    "  mov rbp, rsp",
    "  push r12",
    "  push r13",
    "  push r14",
    "  push r15"
  ]

mainEpilogue :: [Text]
mainEpilogue =
  [ "  pop r15",
    "  pop r14",
    "  pop r13",
    "  pop r12",
    "  pop rbp",
    "  ret"
  ]

threadDoneContinuation :: [Text]
threadDoneContinuation =
  [ ".p2align 3",
    ".Laihc_thread_done_continuation:",
    "  mov rdi, r15",
    "  call aihc_thread_done",
    "  jmp .Laihc_resume"
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

nonExecutableStack :: [Text]
nonExecutableStack = [".section .note.GNU-stack,\"\",@progbits"]

compileEnvironment :: Map.Map FunctionName ContinuationFrameKind -> GrinProgram -> CompileEnv
compileEnvironment = compileEnvironmentWith False

compileEnvironmentWith :: Bool -> Map.Map FunctionName ContinuationFrameKind -> GrinProgram -> CompileEnv
compileEnvironmentWith exposeAllFunctions continuationFrames program =
  CompileEnv
    { compileFunctionLabels = functionLabelMap,
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

renderStaticGlobals :: CompileEnv -> GrinProgram -> Either Amd64Error [Text]
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
      let symbol = renderLinkedGlobalSymbol name
          payload = if null fields && isThunk node then ["  .quad 0"] else fields
      pure $
        [ ".section .data",
          ".p2align 3",
          ".globl " <> symbol,
          symbol <> ":",
          "  .quad " <> info
        ]
          <> payload
          <> [ ".section aihc_roots,\"aw\"",
               ".p2align 3",
               "  .quad " <> symbol
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
        GrinVarValue var -> pure ("  .quad " <> renderLinkedGlobalSymbol (grinVarName var))
        GrinGlobalValue name -> pure ("  .quad " <> renderLinkedGlobalSymbol name)
        GrinLitValue literal ->
          case literal of
            GrinLitAddr bytes ->
              maybe (Left (Amd64UnsupportedValue "unregistered Addr# literal")) (pure . ("  .quad " <>)) (Map.lookup bytes (compileAddrLiteralLabels env))
            _ -> maybe (Left (Amd64UnsupportedValue "string literal")) (pure . ("  .quad " <>) . T.pack . show) (normalizedLiteralInteger literal)
    isThunk node =
      case grinNodeTag node of
        GrinThunk {} -> True
        _ -> False

renderLinkedLocals :: [CompiledFunction] -> [Text]
renderLinkedLocals functions =
  [ ".section aihc_locals,\"aw\"",
    ".p2align 3",
    "  .quad " <> tshow (maximum (2 : map compiledFunctionSlots functions))
  ]

validatePrimitiveName :: Bool -> Text -> Either Amd64Error ()
validatePrimitiveName allowUnsupported name
  | name `elem` supportedNativePrimitiveNames = Right ()
  | allowUnsupported = Right ()
  | otherwise = Left (Amd64UnsupportedPrimitive name)

validateRuntimeRep :: GrinRep -> Either Amd64Error ()
validateRuntimeRep runtimeRep =
  case runtimeRep of
    VecRep {} -> Left (Amd64UnsupportedRuntimeRep runtimeRep)
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
      runtimeRep
        : concatMap valueRuntimeReps [value, continuation, updateContinuation]
    GrinCall runtimeRep _ arguments ->
      runtimeRep : concatMap valueRuntimeReps arguments
    GrinPrimitiveCall runtimeRep _ arguments ->
      runtimeRep : concatMap valueRuntimeReps arguments
    GrinCpsPrimitiveCall runtimeRep _ arguments continuation ->
      runtimeRep : concatMap valueRuntimeReps arguments <> valueRuntimeReps continuation
    GrinApply runtimeRep function arguments ->
      runtimeRep : valueRuntimeReps function <> concatMap valueRuntimeReps arguments
    GrinCpsApply runtimeRep function arguments continuation ->
      runtimeRep
        : valueRuntimeReps function
          <> concatMap valueRuntimeReps arguments
          <> valueRuntimeReps continuation
    GrinContinue continuation values ->
      valueRuntimeReps continuation <> concatMap valueRuntimeReps values
    GrinCpsRaise exception continuation ->
      valueRuntimeReps exception <> valueRuntimeReps continuation
    GrinHalt values -> concatMap valueRuntimeReps values
    GrinExit status -> valueRuntimeReps status
    GrinCase scrutinee binder alternatives ->
      valueRuntimeReps scrutinee
        <> (grinVarRuntimeRep binder : concatMap altRuntimeReps alternatives)
    GrinThrow exception -> valueRuntimeReps exception
    GrinCatch runtimeRep action handler state ->
      runtimeRep : concatMap valueRuntimeReps (action : handler : state)
    GrinForeignCallExpr foreignCall arguments ->
      grinForeignCallResultReps (grinForeignCallSignature foreignCall)
        <> concatMap valueRuntimeReps arguments
  where
    altRuntimeReps alternative =
      map grinVarRuntimeRep (grinAltBinders alternative)
        <> exprRuntimeReps (grinAltRhs alternative)
    storedRuntimeReps bindings body =
      concatMap (\(var, node) -> grinVarRuntimeRep var : nodeRuntimeReps node) bindings
        <> exprRuntimeReps body
    updatedRuntimeReps pointer value = valueRuntimeReps pointer <> valueRuntimeReps value

valueRuntimeReps :: GrinValue -> [GrinRep]
valueRuntimeReps value = [grinValueRuntimeRep value]

nodeRuntimeReps :: GrinNode -> [GrinRep]
nodeRuntimeReps node = concatMap valueRuntimeReps (grinNodeFields node)

findFunction :: FunctionName -> [GrinFunction] -> Maybe GrinFunction
findFunction name = find ((== name) . grinFunctionName)

renderObservedMetadata :: CompileEnv -> GrinProgram -> [GrinRep] -> Either Amd64Error Text
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
  [ "static const AihcSnapshotFunction functions[] = {"
  ]
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

snapshotRepName :: GrinRep -> Either Amd64Error Text
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
    _ -> Left (Amd64UnsupportedRuntimeRep runtimeRep)

pointerOrNull :: [value] -> Text -> Text
pointerOrNull values name
  | null values = "NULL"
  | otherwise = name

cSymbol :: Text -> Text
cSymbol = id

cString :: Text -> Text
cString value = "\"" <> T.concatMap escape value <> "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape character = T.singleton character
