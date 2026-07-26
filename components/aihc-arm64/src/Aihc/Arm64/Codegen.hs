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
    validateProgramPrimitives,
    validatePrimitiveNames,
  )
where

import Aihc.Arm64.Codegen.Analysis
import Aihc.Arm64.Codegen.Function
import Aihc.Arm64.Codegen.Runtime
import Aihc.Arm64.Codegen.Snapshot
import Aihc.Arm64.Codegen.Types
import Aihc.Arm64.Codegen.Value
import Aihc.Grin.Gc
  ( GcGrinProgram,
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
  )
import Aihc.Tc.Types (Levity (..), RuntimeRep (..))
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
          [ ".section __TEXT,__text,regular,pure_instructions",
            ".p2align 2",
            ".globl _main",
            "_main:",
            "  stp x29, x30, [sp, #-48]!",
            "  mov x29, sp",
            "  stp x19, x20, [sp, #16]",
            "  stp x21, x22, [sp, #32]",
            immediate "x0" (length globalNames),
            "  bl _aihc_machine_new",
            "  mov x22, x0"
          ]
            <> reserveLocalsLines functions
            <> constructorLines
            <> initLines
            <> makeNodeLines runtimeTagClosure (InfoAddress ".Laihc_thread_done_info")
            <> [ "  mov x1, x0",
                 "  mov x0, x22",
                 "  bl _aihc_set_thread_done_continuation"
               ]
            <> makeNodeLines runtimeTagClosure (InfoAddress ".Laihc_snapshot_info")
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
                 "  mov w0, #0",
                 "  ldp x21, x22, [sp, #32]",
                 "  ldp x19, x20, [sp, #16]",
                 "  ldp x29, x30, [sp], #48",
                 "  ret"
               ]
            <> [ ".p2align 3",
                 ".Laihc_thread_done_continuation:",
                 "  mov x0, x22",
                 "  bl _aihc_thread_done",
                 "  b .Laihc_resume"
               ]
            <> renderNativeControl
            <> concatMap compiledFunctionLines functions
            <> renderRuntimeSupport compileEnv observedRuntimeInfos
  pure ObservedProgram {observedAssembly = assembly, observedMetadataSource = metadata}
  where
    program = gcGrinProgram gcProgram
    layout = buildLinkLayout [program]
    compileEnv = (compileEnvironmentWith True layout program) {compileContinuationFunctions = gcContinuationFunctions gcProgram}
    globalNames = linkGlobalNames layout
    resultReps =
      maybe [] (runtimeRepComponents . grinFunctionResultRep) $
        findFunction entryName (grinFunctions program)
    observedRuntimeInfos =
      continuationRuntimeInfos
        ".Laihc_thread_done_info"
        ".Laihc_thread_done_applied_info"
        ".Laihc_thread_done_continuation"
        []
        [BoxedRep Lifted]
        <> continuationRuntimeInfos
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
    [ ".section __TEXT,__text,regular,pure_instructions",
      ".p2align 2",
      ".globl " <> initializerSymbol,
      initializerSymbol <> ":",
      "  stp x29, x30, [sp, #-48]!",
      "  mov x29, sp",
      "  stp x19, x20, [sp, #16]",
      "  stp x21, x22, [sp, #32]",
      "  mov x22, x0"
    ]
      <> reserveLocalsLines functions
      <> initLines
      <> [ "  ldp x21, x22, [sp, #32]",
           "  ldp x19, x20, [sp, #16]",
           "  ldp x29, x30, [sp], #48",
           "  ret"
         ]
      <> renderNativeControl
      <> concatMap compiledFunctionLines functions
      <> renderRuntimeSupport compileEnv []
  where
    program = gcGrinProgram gcProgram
    compileEnv =
      (compileEnvironment layout program)
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
    [ ".section __TEXT,__text,regular,pure_instructions",
      ".p2align 2",
      ".globl _main",
      "_main:",
      "  stp x29, x30, [sp, #-48]!",
      "  mov x29, sp",
      "  stp x19, x20, [sp, #16]",
      "  stp x21, x22, [sp, #32]",
      immediate "x0" (length globalNames),
      "  bl _aihc_machine_new",
      "  mov x22, x0"
    ]
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
      <> makeNodeUncheckedLines runtimeTagClosure (InfoAddress ".Laihc_final_info")
      <> ["  mov x21, x0"]
      <> makeNodeUncheckedLines runtimeTagClosure (InfoAddress ".Laihc_top_info")
      <> [ "  mov x20, x0",
           "  mov x2, x21",
           "  mov x1, #0",
           "  bl _aihc_set_field"
         ]
      <> makeNodeUncheckedLines runtimeTagClosure (InfoAddress ".Laihc_update_info")
      <> [ storeAt "x0" "x19" 0,
           "  ldr x9, [x22, #0]",
           loadAt "x2" "x9" rootSlot,
           loadAt "x0" "x19" 0,
           "  mov x1, #0",
           "  bl _aihc_set_field",
           loadAt "x0" "x19" 0,
           "  mov x1, #1",
           "  mov x2, x20",
           "  bl _aihc_set_field"
         ]
      <> makeNodeUncheckedLines runtimeTagClosure (InfoAddress ".Laihc_thread_done_info")
      <> [ "  mov x10, x0",
           "  mov x1, x10",
           "  mov x0, x22",
           "  bl _aihc_set_thread_done_continuation",
           "  adr x9, .Laihc_exit",
           "  str x9, [x22, #16]",
           "  ldr x9, [x22, #0]",
           loadAt applyFunctionRegister "x9" rootSlot,
           loadAt "x0" "x19" 0,
           "  ldr x21, [x0, #16]",
           "  mov x8, #1",
           "  b .Laihc_eval"
         ]
      <> [ ".p2align 3",
           ".Laihc_top_continuation:",
           "  mov x21, x0",
           "  mov x20, x1",
           "  b .Laihc_enter"
         ]
      <> [ ".p2align 3",
           ".Laihc_thread_done_continuation:",
           "  mov x0, x22",
           "  bl _aihc_thread_done",
           "  b .Laihc_resume"
         ]
      <> [ ".p2align 3",
           ".Laihc_final_continuation:",
           "  b .Laihc_exit"
         ]
      <> [ ".Laihc_exit:",
           "  mov w0, #0",
           "  ldp x21, x22, [sp, #32]",
           "  ldp x19, x20, [sp, #16]",
           "  ldp x29, x30, [sp], #48",
           "  ret"
         ]
      <> renderNativeControl
      <> concatMap compiledFunctionLines functions
      <> renderRuntimeSupport compileEnv (programRuntimeInfos updateLabel)
  where
    program = gcGrinProgram gcProgram
    compileEnv = (compileEnvironment layout program) {compileContinuationFunctions = gcContinuationFunctions gcProgram}
    globalSlots = compileGlobalSlots compileEnv
    globalNames = linkGlobalNames layout
    pointerRep = BoxedRep Lifted
    programRuntimeInfos updateLabel =
      continuationRuntimeInfos
        ".Laihc_final_info"
        ".Laihc_final_applied_info"
        ".Laihc_final_continuation"
        []
        [pointerRep]
        <> continuationRuntimeInfos
          ".Laihc_top_info"
          ".Laihc_top_applied_info"
          ".Laihc_top_continuation"
          [pointerRep]
          [pointerRep]
        <> continuationRuntimeInfos
          ".Laihc_update_info"
          ".Laihc_update_applied_info"
          updateLabel
          [pointerRep, pointerRep]
          [pointerRep]
        <> continuationRuntimeInfos
          ".Laihc_thread_done_info"
          ".Laihc_thread_done_applied_info"
          ".Laihc_thread_done_continuation"
          []
          [pointerRep]
    callInitializer symbol =
      [ "  mov x0, x22",
        "  bl " <> symbol
      ]

compileEnvironment :: LinkLayout -> GrinProgram -> CompileEnv
compileEnvironment = compileEnvironmentWith False

compileEnvironmentWith :: Bool -> LinkLayout -> GrinProgram -> CompileEnv
compileEnvironmentWith exposeAllFunctions layout program =
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
          RuntimeInfo label (InfoImmediate identifier) fields remaining next Nothing
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
      [ (grinCodeFunctionName info, linkedFunctionLabel (grinCodeSourceName info))
      | info <- grinExternalFunctions program
      ]
        <> [ (grinFunctionName function, localFunctionLabelWith exposeAllFunctions index function)
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
      | (key, functionName) <- functionInfoKeys,
        let label = functionInfoLabels Map.! key
            target = functionLabelMap Map.! functionName
      ]
    third (_, _, value) = value

compileConstructorInitializers :: CompileEnv -> Either Arm64Error [Text]
compileConstructorInitializers env =
  fmap concat . forM nullaryConstructors $ \(name, _) -> do
    slot <- globalSlot env name
    info <- lookupRuntimeInfoLabel env (ConstructorRuntimeInfo name 0)
    pure $ makeNodeLines runtimeTagNode (InfoAddress info) <> storeGlobal slot
  where
    nullaryConstructors =
      [ (name, constructor)
      | (name, constructor) <- Map.toAscList (compileConstructorIds env),
        Map.lookup name (compileConstructorArities env) == Just 0
      ]

compileInitializers :: CompileEnv -> GrinProgram -> Either Arm64Error [Text]
compileInitializers env program = do
  whnfGlobalLines <- fmap concat . forM (grinWhnfGlobals program) $ \(var, node) -> do
    slot <- globalSlot env (grinVarName var)
    nodeLines <- materializeNode valueEnv node
    pure (nodeLines <> storeGlobal slot)
  cafAllocationLines <- fmap concat . forM (grinCafs program) $ \(var, node) -> do
    slot <- globalSlot env (grinVarName var)
    allocationLines <- allocateNode valueEnv node
    pure (allocationLines <> storeGlobal slot)
  cafInitializationLines <- fmap concat . forM (grinCafs program) $ \(var, node) -> do
    slot <- globalSlot env (grinVarName var)
    fieldLines <- initializeNodeFields valueEnv node
    pure $
      ["  ldr x9, [x22, #0]", loadAt "x20" "x9" slot]
        <> fieldLines
  pure (cafAllocationLines <> whnfGlobalLines <> cafInitializationLines)
  where
    valueEnv = ValueEnv env Map.empty ".Laihc_initializer" (FunctionName "") [] ".Laihc_initializer"
