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

import Aihc.Arm64.RegisterAllocate qualified as RegisterAllocate
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
import Aihc.Native.BlockLayout qualified as BlockLayout
import Aihc.Native.RegisterAllocate (Location (..), grinExprFreeVariables)
import Aihc.Tc.Types (Levity (..), RuntimeRep (..))
import Control.Monad (forM, forM_, replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, execStateT, get, modify')
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)

data Arm64Error
  = Arm64MissingEntry !Text
  | Arm64MissingGlobal !Text
  | Arm64MissingFunction !FunctionName
  | Arm64MissingConstructor !Text
  | Arm64UnsupportedPrimitive !Text
  | Arm64UnsupportedExpression !Text
  | Arm64UnsupportedValue !Text
  | Arm64UnsupportedRuntimeRep !RuntimeRep
  deriving (Eq, Show)

data CompileEnv = CompileEnv
  { compileConstructorIds :: !(Map Text Int),
    compileConstructorArities :: !(Map Text Int),
    compileGlobalSlots :: !(Map Text Int),
    compileFunctionLabels :: !(Map FunctionName Text),
    compileAddrLiteralLabels :: !(Map BS.ByteString Text),
    compileNodeInfoLabels :: !(Map RuntimeInfoKey Text),
    compileRuntimeInfos :: ![RuntimeInfo],
    compileContinuationFunctions :: !(Set.Set FunctionName),
    compileExposeAllFunctions :: !Bool,
    compileAllowUnsupportedPrimitives :: !Bool
  }

data ObservedProgram = ObservedProgram
  { observedAssembly :: !Text,
    observedMetadataSource :: !Text
  }
  deriving (Eq, Show)

data FunctionState = FunctionState
  { functionNextLabel :: !Int,
    functionNextSlot :: !Int,
    functionBlocksRev :: ![BlockLayout.Block Text Text]
  }

data CompiledFunction = CompiledFunction
  { compiledFunctionSlots :: !Int,
    compiledFunctionLines :: ![Text]
  }

type FunctionM = StateT FunctionState (Either Arm64Error)

data ValueEnv = ValueEnv
  { valueCompileEnv :: !CompileEnv,
    valueLocations :: !(Map GrinVar (Location Text)),
    valueLabelPrefix :: !Text,
    valueFunctionName :: !FunctionName,
    valueFunctionParameters :: ![GrinVar],
    valueBodyLabel :: !Text
  }

data RuntimeInfo = RuntimeInfo
  { runtimeInfoLabel :: !Text,
    runtimeInfoIdentity :: !NodeInfo,
    runtimeInfoFields :: ![RuntimeRep],
    runtimeInfoRemainingArity :: !Int,
    runtimeInfoNext :: !(Maybe Text),
    runtimeInfoEnter :: !(Maybe RuntimeEnter)
  }

data RuntimeEnter = RuntimeEnter
  { runtimeEnterTarget :: !Text,
    runtimeEnterStoredCount :: !Int,
    runtimeEnterSuppliedCount :: !Int
  }

data RuntimeInfoKey
  = ConstructorRuntimeInfo !Text !Int
  | ClosureRuntimeInfo !FunctionName ![RuntimeRep] ![[RuntimeRep]]
  | ThunkRuntimeInfo !FunctionName ![RuntimeRep]
  deriving (Eq, Ord, Show)

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
            <> renderEnterStubs
              ( compileRuntimeInfos compileEnv
                  <> [ RuntimeInfo ".Laihc_thread_done_info" (InfoAddress ".Laihc_thread_done_continuation") [] 1 (Just ".Laihc_thread_done_applied_info") (Just (RuntimeEnter ".Laihc_thread_done_continuation" 0 1)),
                       RuntimeInfo ".Laihc_snapshot_info" (InfoAddress ".Laihc_snapshot_result") [] 1 (Just ".Laihc_snapshot_applied_info") (Just (RuntimeEnter ".Laihc_snapshot_result" 0 resultCount))
                     ]
              )
            <> renderAddrLiteralPool compileEnv
            <> renderRuntimeInfos
              ( compileRuntimeInfos compileEnv
                  <> [ RuntimeInfo ".Laihc_thread_done_info" (InfoAddress ".Laihc_thread_done_continuation") [] 1 (Just ".Laihc_thread_done_applied_info") (Just (RuntimeEnter ".Laihc_thread_done_continuation" 0 1)),
                       RuntimeInfo ".Laihc_thread_done_applied_info" (InfoAddress ".Laihc_thread_done_continuation") [BoxedRep Lifted] 0 Nothing Nothing,
                       RuntimeInfo ".Laihc_snapshot_info" (InfoAddress ".Laihc_snapshot_result") [] 1 (Just ".Laihc_snapshot_applied_info") (Just (RuntimeEnter ".Laihc_snapshot_result" 0 resultCount)),
                       RuntimeInfo ".Laihc_snapshot_applied_info" (InfoAddress ".Laihc_snapshot_result") resultReps 0 Nothing Nothing
                     ]
              )
  pure ObservedProgram {observedAssembly = assembly, observedMetadataSource = metadata}
  where
    program = gcGrinProgram gcProgram
    layout = buildLinkLayout [program]
    compileEnv = (compileEnvironmentWith True layout program) {compileContinuationFunctions = gcContinuationFunctions gcProgram}
    globalNames = linkGlobalNames layout
    resultReps =
      maybe [] (runtimeRepComponents . grinFunctionResultRep) $
        findFunction entryName (grinFunctions program)

-- | Reject primitives that reachable native code would not execute correctly.
-- Relocatable library objects may carry dormant primitive declarations, but
-- the linked program is checked after whole-program dead-code elimination.
validateProgramPrimitives :: GrinProgram -> Either Arm64Error ()
validateProgramPrimitives program =
  validatePrimitiveNames (map (grinVarName . fst) (grinPrimitives program))

validatePrimitiveNames :: [Text] -> Either Arm64Error ()
validatePrimitiveNames = mapM_ (validatePrimitiveName False)

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
      <> renderEnterStubs (compileRuntimeInfos compileEnv)
      <> renderAddrLiteralPool compileEnv
      <> renderRuntimeInfos (compileRuntimeInfos compileEnv)
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
      <> renderEnterStubs
        ( compileRuntimeInfos compileEnv
            <> [ RuntimeInfo ".Laihc_final_info" (InfoAddress ".Laihc_final_continuation") [] 1 (Just ".Laihc_final_applied_info") (Just (RuntimeEnter ".Laihc_final_continuation" 0 1)),
                 RuntimeInfo ".Laihc_top_info" (InfoAddress ".Laihc_top_continuation") [BoxedRep Lifted] 1 (Just ".Laihc_top_applied_info") (Just (RuntimeEnter ".Laihc_top_continuation" 1 1)),
                 RuntimeInfo ".Laihc_update_info" (InfoAddress updateLabel) [BoxedRep Lifted, BoxedRep Lifted] 1 (Just ".Laihc_update_applied_info") (Just (RuntimeEnter updateLabel 2 1)),
                 RuntimeInfo ".Laihc_thread_done_info" (InfoAddress ".Laihc_thread_done_continuation") [] 1 (Just ".Laihc_thread_done_applied_info") (Just (RuntimeEnter ".Laihc_thread_done_continuation" 0 1))
               ]
        )
      <> renderAddrLiteralPool compileEnv
      <> renderRuntimeInfos
        ( compileRuntimeInfos compileEnv
            <> [ RuntimeInfo ".Laihc_final_info" (InfoAddress ".Laihc_final_continuation") [] 1 (Just ".Laihc_final_applied_info") (Just (RuntimeEnter ".Laihc_final_continuation" 0 1)),
                 RuntimeInfo ".Laihc_final_applied_info" (InfoAddress ".Laihc_final_continuation") [BoxedRep Lifted] 0 Nothing Nothing,
                 RuntimeInfo ".Laihc_top_info" (InfoAddress ".Laihc_top_continuation") [BoxedRep Lifted] 1 (Just ".Laihc_top_applied_info") (Just (RuntimeEnter ".Laihc_top_continuation" 1 1)),
                 RuntimeInfo ".Laihc_top_applied_info" (InfoAddress ".Laihc_top_continuation") [BoxedRep Lifted, BoxedRep Lifted] 0 Nothing Nothing,
                 RuntimeInfo ".Laihc_update_info" (InfoAddress updateLabel) [BoxedRep Lifted, BoxedRep Lifted] 1 (Just ".Laihc_update_applied_info") (Just (RuntimeEnter updateLabel 2 1)),
                 RuntimeInfo ".Laihc_update_applied_info" (InfoAddress updateLabel) [BoxedRep Lifted, BoxedRep Lifted, BoxedRep Lifted] 0 Nothing Nothing,
                 RuntimeInfo ".Laihc_thread_done_info" (InfoAddress ".Laihc_thread_done_continuation") [] 1 (Just ".Laihc_thread_done_applied_info") (Just (RuntimeEnter ".Laihc_thread_done_continuation" 0 1)),
                 RuntimeInfo ".Laihc_thread_done_applied_info" (InfoAddress ".Laihc_thread_done_continuation") [BoxedRep Lifted] 0 Nothing Nothing
               ]
        )
  where
    program = gcGrinProgram gcProgram
    compileEnv = (compileEnvironment layout program) {compileContinuationFunctions = gcContinuationFunctions gcProgram}
    globalSlots = compileGlobalSlots compileEnv
    globalNames = linkGlobalNames layout
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
compileCpsPrimitive env prefix label name arguments continuation = do
  continuationSlot <- freshSlot
  continuationLines <- liftEither (materializeValue env continuation)
  case (name, arguments) of
    ("fork#", [action]) -> do
      actionSlot <- freshSlot
      actionLines <- liftEither (materializeValue env action)
      addBlock
        label
        ( prefix
            <> actionLines
            <> [storeAt "x0" "x19" actionSlot]
            <> continuationLines
            <> [ storeAt "x0" "x19" continuationSlot,
                 loadAt "x1" "x19" actionSlot,
                 "  mov x0, x22",
                 "  bl _aihc_fork",
                 loadAt applyFunctionRegister "x19" continuationSlot
               ]
        )
        (BlockLayout.Jump ".Laihc_enter")
    ("yield#", []) ->
      addBlock
        label
        ( prefix
            <> continuationLines
            <> [ storeAt "x0" "x19" continuationSlot,
                 loadAt "x1" "x19" continuationSlot,
                 "  mov x0, x22",
                 "  bl _aihc_yield"
               ]
        )
        (BlockLayout.Jump ".Laihc_resume")
    _ -> lift (Left (Arm64UnsupportedExpression ("CPS primitive call " <> name)))

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
    GrinPrimitiveCall runtimeRep name arguments
      | name == "realWorld#",
        null arguments,
        null vars,
        null (runtimeRepComponents runtimeRep) ->
          pure []
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

materializeValue :: ValueEnv -> GrinValue -> Either Arm64Error [Text]
materializeValue env = materializeValueTo env "x0"

materializeValueTo :: ValueEnv -> Text -> GrinValue -> Either Arm64Error [Text]
materializeValueTo env destination value =
  case value of
    GrinVarValue var ->
      case Map.lookup var (valueLocations env) of
        Just location -> Right (loadLocation destination location)
        Nothing -> do
          slot <- globalSlot (valueCompileEnv env) (grinVarName var)
          pure ["  ldr x9, [x22, #0]", loadAt destination "x9" slot]
    GrinLitValue literal -> materializeLiteralTo destination (valueCompileEnv env) literal

materializeLiteralTo :: Text -> CompileEnv -> GrinLiteral -> Either Arm64Error [Text]
materializeLiteralTo destination env literal =
  case literal of
    GrinLitAddr value -> do
      label <-
        maybe
          (Left (Arm64UnsupportedValue "unregistered Addr# literal"))
          Right
          (Map.lookup value (compileAddrLiteralLabels env))
      pure [address destination label]
    _ ->
      case normalizedLiteralInteger literal of
        Just integer -> Right [immediate destination integer]
        Nothing -> Left (Arm64UnsupportedValue "string literal")

normalizedLiteralInteger :: GrinLiteral -> Maybe Integer
normalizedLiteralInteger literal = do
  integer <- literalInteger literal
  pure $
    case literal of
      GrinLitInt runtimeRep _ -> normalizeScalar runtimeRep integer
      GrinLitChar {} -> normalizeUnsigned 64 integer
      GrinLitString {} -> integer
      GrinLitAddr {} -> integer

normalizeScalar :: RuntimeRep -> Integer -> Integer
normalizeScalar runtimeRep integer =
  case runtimeRep of
    IntRep -> normalizeSigned 64 integer
    Int8Rep -> normalizeSigned 8 integer
    Int16Rep -> normalizeSigned 16 integer
    Int32Rep -> normalizeSigned 32 integer
    Int64Rep -> normalizeSigned 64 integer
    WordRep -> normalizeUnsigned 64 integer
    Word8Rep -> normalizeUnsigned 8 integer
    Word16Rep -> normalizeUnsigned 16 integer
    Word32Rep -> normalizeUnsigned 32 integer
    Word64Rep -> normalizeUnsigned 64 integer
    _ -> integer

normalizeSigned :: Int -> Integer -> Integer
normalizeSigned bits integer =
  let modulus = 2 ^ bits
      signBit = 2 ^ (bits - 1)
      unsigned = integer `mod` modulus
   in if unsigned >= signBit then unsigned - modulus else unsigned

normalizeUnsigned :: Int -> Integer -> Integer
normalizeUnsigned bits integer = integer `mod` (2 ^ bits)

literalInteger :: GrinLiteral -> Maybe Integer
literalInteger literal =
  case literal of
    GrinLitInt _ integer -> Just integer
    GrinLitChar _ character -> Just (fromIntegral (ord character))
    GrinLitString _ -> Nothing
    GrinLitAddr _ -> Nothing

materializeNode :: ValueEnv -> GrinNode -> Either Arm64Error [Text]
materializeNode env node = do
  allocationLines <- allocateNode env node
  fieldLines <- initializeNodeFields env node
  pure $
    allocationLines
      <> ["  mov x20, x0"]
      <> fieldLines
      <> ["  mov x0, x20"]

materializeNodeUnchecked :: ValueEnv -> GrinNode -> Either Arm64Error [Text]
materializeNodeUnchecked env node = do
  allocationLines <- allocateNodeUnchecked env node
  fieldLines <- initializeNodeFields env node
  pure $
    allocationLines
      <> ["  mov x20, x0"]
      <> fieldLines
      <> ["  mov x0, x20"]

allocateNode :: ValueEnv -> GrinNode -> Either Arm64Error [Text]
allocateNode env node = do
  (tag, info) <- nodeHeader env node
  pure (makeNodeLines tag info)

allocateNodeUnchecked :: ValueEnv -> GrinNode -> Either Arm64Error [Text]
allocateNodeUnchecked env node = do
  (tag, info) <- nodeHeader env node
  pure (makeNodeUncheckedLines tag info)

initializeNodeFields :: ValueEnv -> GrinNode -> Either Arm64Error [Text]
initializeNodeFields env node =
  fmap concat . forM (zip [0 :: Int ..] (grinNodeFields node)) $ \(index, field) -> do
    valueLines <- materializeValue env field
    pure $
      valueLines
        <> [ "  mov x2, x0",
             "  mov x0, x20",
             immediate "x1" index,
             "  bl _aihc_set_field"
           ]

nodeHeader :: ValueEnv -> GrinNode -> Either Arm64Error (Int, NodeInfo)
nodeHeader env node =
  case grinNodeTag node of
    GrinConstructor name remaining -> do
      label <- lookupRuntimeInfoLabel compileEnv (ConstructorRuntimeInfo name remaining)
      pure
        ( if remaining == 0 then runtimeTagNode else runtimeTagPartialConstructor,
          InfoAddress label
        )
    GrinClosure functionName argumentLayouts -> do
      label <- lookupRuntimeInfoLabel compileEnv (ClosureRuntimeInfo functionName fields argumentLayouts)
      pure (runtimeTagClosure, InfoAddress label)
    GrinThunk functionName -> do
      label <- lookupRuntimeInfoLabel compileEnv (ThunkRuntimeInfo functionName fields)
      pure (runtimeTagThunk, InfoAddress label)
  where
    compileEnv = valueCompileEnv env
    fields = map grinValueRuntimeRep (grinNodeFields node)

data NodeInfo
  = InfoImmediate !Int
  | InfoAddress !Text

makeNodeLines :: Int -> NodeInfo -> [Text]
makeNodeLines kind info =
  [ "  mov x0, x22",
    immediate "x1" kind,
    infoLine info,
    "  bl _aihc_make_node"
  ]
  where
    infoLine nodeInfo =
      case nodeInfo of
        InfoImmediate integer -> immediate "x2" integer
        InfoAddress label -> address "x2" label

makeNodeUncheckedLines :: Int -> NodeInfo -> [Text]
makeNodeUncheckedLines kind info =
  init (makeNodeLines kind info) <> ["  bl _aihc_make_node_unchecked"]

renderEnterStubs :: [RuntimeInfo] -> [Text]
renderEnterStubs = concatMap renderStub
  where
    renderStub info =
      case runtimeInfoEnter info of
        Nothing -> []
        Just apply ->
          [ ".section __TEXT,__text,regular,pure_instructions",
            ".p2align 3",
            enterEntryLabel info <> ":"
          ]
            <> moveSupplied apply
            <> moveSuppliedOverflow apply
            <> loadStored apply
            <> restoreApplyStackLines (applyStackBytes (runtimeEnterSuppliedCount apply))
            <> ["  b " <> runtimeEnterTarget apply]
    moveSupplied apply =
      concat
        [ placeArgument targetIndex source
        | (sourceIndex, source) <- reverse (zip [0 :: Int ..] applyArgumentRegisters),
          sourceIndex < runtimeEnterSuppliedCount apply,
          let targetIndex = runtimeEnterStoredCount apply + sourceIndex
        ]
    moveSuppliedOverflow apply
      | runtimeEnterSuppliedCount apply <= length applyArgumentRegisters = []
      | otherwise =
          ["  mov x9, sp"]
            <> concat
              [ [ "  ldr x8, [x9], #8",
                  storeAt "x8" "x19" targetIndex
                ]
              | sourceIndex <- [length applyArgumentRegisters .. runtimeEnterSuppliedCount apply - 1],
                let targetIndex = runtimeEnterStoredCount apply + sourceIndex
              ]
    loadStored apply =
      concat
        [ if targetIndex < length applyArgumentRegisters
            then ["  ldr " <> applyArgumentRegisters !! targetIndex <> ", [" <> applyFunctionRegister <> ", #" <> tshow ((targetIndex + 1) * 8) <> "]"]
            else
              [ "  ldr x8, [" <> applyFunctionRegister <> ", #" <> tshow ((targetIndex + 1) * 8) <> "]",
                storeAt "x8" "x19" targetIndex
              ]
        | targetIndex <- [0 .. runtimeEnterStoredCount apply - 1]
        ]
    placeArgument targetIndex source
      | targetIndex < length applyArgumentRegisters =
          ["  mov " <> applyArgumentRegisters !! targetIndex <> ", " <> source]
      | otherwise = [storeAt source "x19" targetIndex]

enterEntryLabel :: RuntimeInfo -> Text
enterEntryLabel info = runtimeInfoLabel info <> "_enter"

applyFunctionRegister, applyContinuationRegister :: Text
applyFunctionRegister = "x20"
applyContinuationRegister = "x21"

applyArgumentRegisters :: [Text]
applyArgumentRegisters = ["x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"]

applyStackBytes :: Int -> Int
applyStackBytes suppliedCount =
  ((overflowCount * 8 + 15) `div` 16) * 16
  where
    overflowCount = max 0 (suppliedCount - length applyArgumentRegisters)

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

renderRuntimeInfos :: [RuntimeInfo] -> [Text]
renderRuntimeInfos infos =
  [".section __DATA,__const"] <> concatMap renderInfo infos
  where
    renderInfo info =
      bitmapLines
        <> [ ".p2align 3",
             runtimeInfoLabel info <> ":",
             identityLine (runtimeInfoIdentity info),
             entryLine (runtimeInfoIdentity info),
             "  .quad " <> tshow (length fields),
             "  .quad " <> tshow (runtimeInfoRemainingArity info),
             "  .quad " <> if null fields then "0" else bitmapLabel,
             "  .quad " <> fromMaybe "0" (runtimeInfoNext info),
             "  .quad " <> maybe "0" (const (enterEntryLabel info)) (runtimeInfoEnter info)
           ]
      where
        fields = runtimeInfoFields info
        bitmapLabel = runtimeInfoLabel info <> "_bitmap"
        bitmapLines =
          if null fields
            then []
            else
              [ bitmapLabel <> ":",
                "  .byte " <> T.intercalate ", " [if isPointerRuntimeRep runtimeRep then "1" else "0" | runtimeRep <- fields]
              ]
    identityLine nodeInfo =
      case nodeInfo of
        InfoImmediate integer -> "  .quad " <> tshow integer
        InfoAddress label -> "  .quad " <> label
    entryLine nodeInfo =
      case nodeInfo of
        InfoImmediate {} -> "  .quad 0"
        InfoAddress label -> "  .quad " <> label

runtimeTagNode, runtimeTagClosure, runtimeTagThunk, runtimeTagPartialConstructor :: Int
runtimeTagNode = 0
runtimeTagClosure = 1
runtimeTagThunk = 2
runtimeTagPartialConstructor = 3

variableLocation :: ValueEnv -> GrinVar -> Either Arm64Error (Location Text)
variableLocation env var =
  maybe
    (Left (Arm64UnsupportedExpression ("missing location for " <> grinVarName var)))
    Right
    (Map.lookup var (valueLocations env))

loadLocation :: Text -> Location Text -> [Text]
loadLocation destination location =
  case location of
    InRegister source
      | destination == source -> []
      | otherwise -> ["  mov " <> destination <> ", " <> source]
    InHeapSpill slot -> [loadAt destination "x19" slot]

storeLocation :: Text -> Location Text -> [Text]
storeLocation source location =
  case location of
    InRegister destination
      | destination == source -> []
      | otherwise -> ["  mov " <> destination <> ", " <> source]
    InHeapSpill slot -> [storeAt source "x19" slot]

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

renderNativeControl :: [Text]
renderNativeControl =
  [ ".section __TEXT,__text,regular,pure_instructions",
    ".p2align 3",
    ".Laihc_enter:",
    "  ldr x9, [x20]",
    "  and x9, x9, #0xfffffffffffffff8",
    "  ldr x9, [x9, #48]",
    "  cbz x9, .Laihc_invalid_enter",
    "  br x9",
    ".Laihc_resume:",
    "  ldr w9, [x0]",
    "  cmp w9, #1",
    "  b.eq .Laihc_resume_apply",
    "  cmp w9, #2",
    "  b.ne .Laihc_invalid_enter",
    "  ldr x8, [x0, #24]",
    "  ldr x9, [x0, #32]",
    "  ldr x20, [x0, #8]",
    "  stp xzr, xzr, [x0]",
    "  stp xzr, xzr, [x0, #16]",
    "  str xzr, [x0, #32]",
    "  cbz x9, .Laihc_enter",
    "  cmp x9, #1",
    "  b.ne .Laihc_invalid_enter",
    "  mov x0, x8",
    "  b .Laihc_enter",
    ".Laihc_resume_apply:",
    "  ldr x20, [x0, #8]",
    "  ldr x21, [x0, #16]",
    "  stp xzr, xzr, [x0]",
    "  stp xzr, xzr, [x0, #16]",
    "  str xzr, [x0, #32]",
    "  b .Laihc_enter",
    ".Laihc_eval:",
    "  str x0, [x19]",
    "  str x8, [x19, #8]",
    ".Laihc_eval_loop:",
    "  ldr x9, [x20]",
    "  and x10, x9, #7",
    "  cmp x10, #2",
    "  b.eq .Laihc_eval_thunk",
    "  cmp x10, #4",
    "  b.eq .Laihc_eval_indirection",
    "  cmp x10, #5",
    "  b.eq .Laihc_eval_blackhole",
    "  mov x0, x20",
    "  mov x20, x21",
    "  b .Laihc_enter",
    ".Laihc_eval_thunk:",
    "  mov x1, x20",
    "  mov x0, x22",
    "  bl _aihc_begin_blackhole",
    "  ldr x21, [x19]",
    "  b .Laihc_enter",
    ".Laihc_eval_indirection:",
    "  ldr x9, [x19, #8]",
    "  cbz x9, .Laihc_eval_unlifted_indirection",
    "  ldr x20, [x20, #8]",
    "  b .Laihc_eval_loop",
    ".Laihc_eval_unlifted_indirection:",
    "  ldr x0, [x20, #8]",
    "  mov x20, x21",
    "  b .Laihc_enter",
    ".Laihc_eval_blackhole:",
    "  mov x2, x21",
    "  mov x1, x20",
    "  mov x0, x22",
    "  bl _aihc_block_on_blackhole",
    "  b .Laihc_resume",
    ".Laihc_invalid_enter:",
    "  bl _aihc_no_match",
    "  brk #0"
  ]

globalSlot :: CompileEnv -> Text -> Either Arm64Error Int
globalSlot env name =
  maybe (Left (Arm64MissingGlobal name)) Right (Map.lookup name (compileGlobalSlots env))

constructorId :: CompileEnv -> Text -> Either Arm64Error Int
constructorId env name =
  maybe (Left (Arm64MissingConstructor name)) Right (Map.lookup name (compileConstructorIds env))

lookupRuntimeInfoLabel :: CompileEnv -> RuntimeInfoKey -> Either Arm64Error Text
lookupRuntimeInfoLabel env key =
  case Map.lookup key (compileNodeInfoLabels env) of
    Just label -> Right label
    Nothing ->
      case key of
        ConstructorRuntimeInfo name _ -> Left (Arm64MissingConstructor name)
        ClosureRuntimeInfo functionName _ _ -> Left (Arm64MissingFunction functionName)
        ThunkRuntimeInfo functionName _ -> Left (Arm64MissingFunction functionName)

functionCodeLabel :: CompileEnv -> FunctionName -> Either Arm64Error Text
functionCodeLabel env name =
  maybe (Left (Arm64MissingFunction name)) Right (Map.lookup name (compileFunctionLabels env))

constructorStageLabel :: Int -> Int -> Text
constructorStageLabel identifier remaining =
  ".Laihc_constructor_info_" <> tshow identifier <> "_remaining_" <> tshow remaining

runtimeInfoKeyStages :: GrinNode -> [RuntimeInfoKey]
runtimeInfoKeyStages node =
  case grinNodeTag node of
    GrinConstructor name remaining -> [ConstructorRuntimeInfo name remaining]
    GrinClosure functionName argumentLayouts -> closureStages fields argumentLayouts
      where
        closureStages current remainingLayouts =
          ClosureRuntimeInfo functionName current remainingLayouts
            : case remainingLayouts of
              [] -> []
              layout : rest -> closureStages (current <> layout) rest
    GrinThunk functionName -> [ThunkRuntimeInfo functionName fields]
  where
    fields = map grinValueRuntimeRep (grinNodeFields node)

runtimeInfoFunctionName :: RuntimeInfoKey -> Maybe FunctionName
runtimeInfoFunctionName key =
  case key of
    ConstructorRuntimeInfo {} -> Nothing
    ClosureRuntimeInfo functionName _ _ -> Just functionName
    ThunkRuntimeInfo functionName _ -> Just functionName

runtimeInfoKeyFields :: RuntimeInfoKey -> [RuntimeRep]
runtimeInfoKeyFields key =
  case key of
    ConstructorRuntimeInfo {} -> []
    ClosureRuntimeInfo _ fields _ -> fields
    ThunkRuntimeInfo _ fields -> fields

runtimeInfoKeyRemainingArity :: RuntimeInfoKey -> Int
runtimeInfoKeyRemainingArity key =
  case key of
    ConstructorRuntimeInfo _ remaining -> remaining
    ClosureRuntimeInfo _ _ argumentLayouts -> length argumentLayouts
    ThunkRuntimeInfo {} -> 0

runtimeInfoKeyNext :: RuntimeInfoKey -> Maybe RuntimeInfoKey
runtimeInfoKeyNext key =
  case key of
    ConstructorRuntimeInfo name remaining
      | remaining > 0 -> Just (ConstructorRuntimeInfo name (remaining - 1))
    ConstructorRuntimeInfo {} -> Nothing
    ClosureRuntimeInfo functionName fields (layout : rest) ->
      Just (ClosureRuntimeInfo functionName (fields <> layout) rest)
    ClosureRuntimeInfo {} -> Nothing
    ThunkRuntimeInfo {} -> Nothing

validatePrimitiveName :: Bool -> Text -> Either Arm64Error ()
validatePrimitiveName allowUnsupported name
  | name `elem` ["+#", "fork#", "realWorld#", "yield#"] = Right ()
  | allowUnsupported = Right ()
  | otherwise = Left (Arm64UnsupportedPrimitive name)

validateRuntimeRep :: RuntimeRep -> Either Arm64Error ()
validateRuntimeRep runtimeRep =
  case runtimeRep of
    VecRep {} -> Left (Arm64UnsupportedRuntimeRep runtimeRep)
    TupleRep fieldReps -> mapM_ validateRuntimeRep fieldReps
    SumRep alternativeReps -> mapM_ validateRuntimeRep alternativeReps
    RuntimeRepVar {} -> Left (Arm64UnsupportedRuntimeRep runtimeRep)
    RuntimeRepMeta {} -> Left (Arm64UnsupportedRuntimeRep runtimeRep)
    _ -> Right ()

programRuntimeReps :: GrinProgram -> [RuntimeRep]
programRuntimeReps program =
  concatMap (concat . snd) (grinConstructors program)
    <> map (grinVarRuntimeRep . fst) (grinPrimitives program)
    <> concatMap globalRuntimeReps (grinWhnfGlobals program)
    <> concatMap cafRuntimeReps (grinCafs program)
    <> concatMap functionRuntimeReps (grinFunctions program)
  where
    globalRuntimeReps (var, node) = grinVarRuntimeRep var : nodeRuntimeReps node
    cafRuntimeReps (var, node) = grinVarRuntimeRep var : nodeRuntimeReps node
    functionRuntimeReps function =
      grinFunctionResultRep function
        : map grinVarRuntimeRep (grinFunctionParameters function)
          <> exprRuntimeReps (grinFunctionBody function)

programNodes :: GrinProgram -> [GrinNode]
programNodes program =
  map snd (grinWhnfGlobals program)
    <> map snd (grinCafs program)
    <> concatMap (exprNodes . grinFunctionBody) (grinFunctions program)

exprNodes :: GrinExpr -> [GrinNode]
exprNodes expression =
  case expression of
    GrinBind _ valueExpression body -> exprNodes valueExpression <> exprNodes body
    GrinStore node -> [node]
    GrinStoreUnchecked node -> [node]
    GrinStoreRec bindings body -> map snd bindings <> exprNodes body
    GrinStoreRecUnchecked bindings body -> map snd bindings <> exprNodes body
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
    GrinHalt {} -> []
    GrinThrow {} -> []
    GrinCatch {} -> []
    GrinForeignCallExpr {} -> []

exprRuntimeReps :: GrinExpr -> [RuntimeRep]
exprRuntimeReps expression =
  case expression of
    GrinConstant values -> concatMap valueRuntimeReps values
    GrinBind vars valueExpression body ->
      map grinVarRuntimeRep vars <> exprRuntimeReps valueExpression <> exprRuntimeReps body
    GrinStore node -> nodeRuntimeReps node
    GrinEnsureHeap _ roots -> concatMap valueRuntimeReps roots
    GrinStoreUnchecked node -> nodeRuntimeReps node
    GrinStoreRec bindings body ->
      concatMap (\(var, node) -> grinVarRuntimeRep var : nodeRuntimeReps node) bindings
        <> exprRuntimeReps body
    GrinStoreRecUnchecked bindings body ->
      concatMap (\(var, node) -> grinVarRuntimeRep var : nodeRuntimeReps node) bindings
        <> exprRuntimeReps body
    GrinFetch runtimeRep pointer -> runtimeRep : valueRuntimeReps pointer
    GrinUpdate pointer value -> valueRuntimeReps pointer <> valueRuntimeReps value
    GrinUpdateBlackhole pointer value -> valueRuntimeReps pointer <> valueRuntimeReps value
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
    GrinHalt values -> concatMap valueRuntimeReps values
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

valueRuntimeReps :: GrinValue -> [RuntimeRep]
valueRuntimeReps value = [grinValueRuntimeRep value]

nodeRuntimeReps :: GrinNode -> [RuntimeRep]
nodeRuntimeReps node = concatMap valueRuntimeReps (grinNodeFields node)

findFunction :: FunctionName -> [GrinFunction] -> Maybe GrinFunction
findFunction name =
  foldr
    ( \function rest ->
        if grinFunctionName function == name
          then Just function
          else rest
    )
    Nothing

renderObservedMetadata :: CompileEnv -> GrinProgram -> [RuntimeRep] -> Either Arm64Error Text
renderObservedMetadata env program resultReps = do
  renderedResultReps <- mapM snapshotRepName resultReps
  constructors <- mapM renderConstructorDescriptor constructorEntries
  functions <- mapM renderFunctionDescriptor functionEntries
  pure . T.unlines $
    [ "#include \"aihc_runtime.h\"",
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
           "                     " <> tshow (length constructors) <> ", " <> tableOrNull constructors "constructors" <> ",",
           "                     " <> tshow (length functions) <> ", " <> tableOrNull functions "functions" <> ");",
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
    externalFunctionEntries =
      [ (grinCodeFunctionName info, concat (grinCodeParameterLayouts info))
      | info <- grinExternalFunctions program
      ]
    functionEntries = externalFunctionEntries <> localFunctionEntries

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
  [ "static const AihcSnapshotConstructor constructors[] = {"
  ]
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

snapshotRepName :: RuntimeRep -> Either Arm64Error Text
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

tableOrNull :: [value] -> Text -> Text
tableOrNull = pointerOrNull

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

renderAddrLiteralPool :: CompileEnv -> [Text]
renderAddrLiteralPool env =
  case Map.toAscList (compileAddrLiteralLabels env) of
    [] -> []
    literals ->
      [".section __TEXT,__cstring,cstring_literals"]
        <> concatMap renderLiteral literals
  where
    renderLiteral (value, label) =
      [ label <> ":",
        "  .byte " <> T.intercalate ", " (map tshow (BS.unpack value <> [0]))
      ]

functionLabel :: Int -> Text
functionLabel index = ".Laihc_function_" <> tshow index

localFunctionLabelWith :: Bool -> Int -> GrinFunction -> Text
localFunctionLabelWith exposeAllFunctions index function
  | exposeAllFunctions = "_aihc_snapshot_function_" <> tshow index
  | otherwise =
      case grinFunctionLinkName function of
        Just name -> linkedFunctionLabel name
        Nothing -> functionLabel index

linkedFunctionLabel :: Text -> Text
linkedFunctionLabel name =
  "_aihc_entry_" <> T.concatMap encode name
  where
    encode character = T.pack (showHex (ord character) "_")

storeGlobal :: Int -> [Text]
storeGlobal slot =
  [ "  ldr x9, [x22, #0]",
    storeAt "x0" "x9" slot
  ]

loadAt :: Text -> Text -> Int -> Text
loadAt destination base slot = loadByteOffset destination base (slot * 8)

storeAt :: Text -> Text -> Int -> Text
storeAt source base slot = storeByteOffset source base (slot * 8)

loadByteOffset :: Text -> Text -> Int -> Text
loadByteOffset destination base offset =
  "  ldr " <> destination <> ", [" <> base <> ", #" <> tshow offset <> "]"

storeByteOffset :: Text -> Text -> Int -> Text
storeByteOffset source base offset =
  "  str " <> source <> ", [" <> base <> ", #" <> tshow offset <> "]"

immediate :: (Show value) => Text -> value -> Text
immediate register value = "  ldr " <> register <> ", =" <> T.pack (show value)

address :: Text -> Text -> Text
address register label =
  "  adrp " <> register <> ", " <> label <> "@PAGE\n  add " <> register <> ", " <> register <> ", " <> label <> "@PAGEOFF"

tshow :: (Show value) => value -> Text
tshow = T.pack . show

liftEither :: Either Arm64Error value -> FunctionM value
liftEither = lift
