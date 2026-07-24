{-# LANGUAGE OverloadedStrings #-}

-- | Lower runtime-explicit GRIN to Intel-syntax AMD64 assembly for Linux.
-- Generated Haskell entries transfer only with branches; calls are reserved
-- for the C runtime and foreign functions.
module Aihc.Amd64.Codegen
  ( Amd64Error (..),
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

import Aihc.Amd64.RegisterAllocate qualified as RegisterAllocate
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
    nativeRuntimePrimitiveCall,
    supportedNativePrimitiveNames,
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

data Amd64Error
  = Amd64MissingEntry !Text
  | Amd64MissingGlobal !Text
  | Amd64MissingFunction !FunctionName
  | Amd64MissingConstructor !Text
  | Amd64UnsupportedPrimitive !Text
  | Amd64UnsupportedExpression !Text
  | Amd64UnsupportedValue !Text
  | Amd64UnsupportedRuntimeRep !RuntimeRep
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

type FunctionM = StateT FunctionState (Either Amd64Error)

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

data MoveSource
  = MoveRegister !Text
  | MoveSpill !Int
  | MoveValue !GrinValue
  deriving (Eq)

compileProgram :: Text -> GcGrinProgram -> Either Amd64Error Text
compileProgram entryName gcProgram =
  compileProgramWithDependencies (buildLinkLayout [program]) [] entryName gcProgram
  where
    program = gcGrinProgram gcProgram

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
  constructorLines <- compileConstructorInitializers compileEnv
  initLines <- compileInitializers compileEnv program
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  metadata <- renderObservedMetadata compileEnv program resultReps
  let resultCount = length resultReps
      assembly =
        T.unlines $
          [ ".intel_syntax noprefix",
            ".text",
            ".p2align 4",
            ".globl main",
            "main:",
            "  push rbp",
            "  mov rbp, rsp",
            "  push r12",
            "  push r13",
            "  push r14",
            "  push r15",
            immediate "rdi" (length globalNames),
            "  call aihc_machine_new",
            "  mov r15, rax"
          ]
            <> reserveLocalsLines functions
            <> constructorLines
            <> initLines
            <> makeNodeLines runtimeTagClosure (InfoAddress ".Laihc_thread_done_info")
            <> [ "  mov rdi, r15",
                 "  mov rsi, rax",
                 "  call aihc_set_thread_done_continuation"
               ]
            <> makeNodeLines runtimeTagClosure (InfoAddress ".Laihc_snapshot_info")
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
            <> [ ".p2align 3",
                 ".Laihc_thread_done_continuation:",
                 "  mov rdi, r15",
                 "  call aihc_thread_done",
                 "  jmp .Laihc_resume"
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
            <> nonExecutableStack
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
validateProgramPrimitives :: GrinProgram -> Either Amd64Error ()
validateProgramPrimitives program =
  validatePrimitiveNames (map (grinVarName . fst) (grinPrimitives program))

validatePrimitiveNames :: [Text] -> Either Amd64Error ()
validatePrimitiveNames = mapM_ (validatePrimitiveName False)

-- | Compile a library SCC to relocatable assembly. The exported initializer
-- installs the unit's primitive, static, and CAF globals into the shared
-- machine table. Constructors are installed once by the executable entry unit.
compileModule :: LinkLayout -> Text -> GcGrinProgram -> Either Amd64Error Text
compileModule layout initializerSymbol gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  initLines <- compileInitializers compileEnv program
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  pure . T.unlines $
    [ ".intel_syntax noprefix",
      ".text",
      ".p2align 4",
      ".globl " <> initializerSymbol,
      initializerSymbol <> ":",
      "  push rbp",
      "  mov rbp, rsp",
      "  push r12",
      "  push r13",
      "  push r14",
      "  push r15",
      "  mov r15, rdi"
    ]
      <> reserveLocalsLines functions
      <> initLines
      <> mainEpilogue
      <> renderNativeControl
      <> concatMap compiledFunctionLines functions
      <> renderEnterStubs (compileRuntimeInfos compileEnv)
      <> renderAddrLiteralPool compileEnv
      <> renderRuntimeInfos (compileRuntimeInfos compileEnv)
      <> nonExecutableStack
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
compileProgramWithDependencies :: LinkLayout -> [Text] -> Text -> GcGrinProgram -> Either Amd64Error Text
compileProgramWithDependencies layout dependencyInitializers entryName gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  rootSlot <- maybe (Left (Amd64MissingEntry entryName)) Right (Map.lookup entryName globalSlots)
  constructorLines <- compileConstructorInitializers compileEnv
  initLines <- compileInitializers compileEnv program
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  updateLabel <- functionCodeLabel compileEnv (gcUpdateFunction gcProgram)
  pure . T.unlines $
    [ ".intel_syntax noprefix",
      ".text",
      ".p2align 4",
      ".globl main",
      "main:",
      "  push rbp",
      "  mov rbp, rsp",
      "  push r12",
      "  push r13",
      "  push r14",
      "  push r15",
      immediate "rdi" (length globalNames),
      "  call aihc_machine_new",
      "  mov r15, rax"
    ]
      <> constructorLines
      <> concatMap callInitializer dependencyInitializers
      <> initLines
      <> reserveLocalsLines functions
      <> [ "  mov rdi, r15",
           immediate "rsi" (7 :: Int),
           "  xor edx, edx",
           "  xor ecx, ecx",
           "  call aihc_ensure_heap"
         ]
      <> makeNodeUncheckedLines runtimeTagClosure (InfoAddress ".Laihc_final_info")
      <> ["  mov r13, rax"]
      <> makeNodeUncheckedLines runtimeTagClosure (InfoAddress ".Laihc_top_info")
      <> [ "  mov r12, rax",
           "  mov rdi, r12",
           "  xor esi, esi",
           "  mov rdx, r13",
           "  call aihc_set_field"
         ]
      <> makeNodeUncheckedLines runtimeTagClosure (InfoAddress ".Laihc_update_info")
      <> [ storeAt "rax" "r14" 0,
           loadByteOffset "r11" "r15" 0,
           loadAt "rdx" "r11" rootSlot,
           loadAt "rdi" "r14" 0,
           "  xor esi, esi",
           "  call aihc_set_field",
           loadAt "rdi" "r14" 0,
           "  mov esi, 1",
           "  mov rdx, r12",
           "  call aihc_set_field"
         ]
      <> makeNodeUncheckedLines runtimeTagClosure (InfoAddress ".Laihc_thread_done_info")
      <> [ "  mov r10, rax",
           "  mov rsi, r10",
           "  mov rdi, r15",
           "  call aihc_set_thread_done_continuation",
           address "r11" ".Laihc_exit",
           "  mov QWORD PTR [r15 + 16], r11",
           loadByteOffset "r11" "r15" 0,
           loadAt applyFunctionRegister "r11" rootSlot,
           loadAt "rax" "r14" 0,
           "  mov r13, QWORD PTR [rax + 16]",
           "  mov r11, 1",
           "  jmp .Laihc_eval"
         ]
      <> [ ".p2align 3",
           ".Laihc_top_continuation:",
           "  mov r13, rax",
           "  mov r12, rdi",
           "  jmp .Laihc_enter"
         ]
      <> [ ".p2align 3",
           ".Laihc_thread_done_continuation:",
           "  mov rdi, r15",
           "  call aihc_thread_done",
           "  jmp .Laihc_resume"
         ]
      <> [ ".p2align 3",
           ".Laihc_final_continuation:",
           "  jmp .Laihc_exit"
         ]
      <> [ ".Laihc_exit:",
           "  xor eax, eax"
         ]
      <> mainEpilogue
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
      <> nonExecutableStack
  where
    program = gcGrinProgram gcProgram
    compileEnv = (compileEnvironment layout program) {compileContinuationFunctions = gcContinuationFunctions gcProgram}
    globalSlots = compileGlobalSlots compileEnv
    globalNames = linkGlobalNames layout
    callInitializer symbol =
      [ "  mov rdi, r15",
        "  call " <> symbol
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

nonExecutableStack :: [Text]
nonExecutableStack = [".section .note.GNU-stack,\"\",@progbits"]

compileEnvironment :: LinkLayout -> GrinProgram -> CompileEnv
compileEnvironment = compileEnvironmentWith False

compileEnvironmentWith :: Bool -> LinkLayout -> GrinProgram -> CompileEnv
compileEnvironmentWith exposeAllFunctions layout program =
  CompileEnv
    { compileConstructorIds = Map.fromList (zip (map fst constructors) [1 ..]),
      compileConstructorArities = Map.fromList constructors,
      compileGlobalSlots = Map.fromList (zip (linkGlobalNames layout) [0 ..]),
      compileFunctionLabels = functionLabelMap,
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

compileConstructorInitializers :: CompileEnv -> Either Amd64Error [Text]
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

compileInitializers :: CompileEnv -> GrinProgram -> Either Amd64Error [Text]
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
      [loadByteOffset "r11" "r15" 0, loadAt "r13" "r11" slot]
        <> fieldLines
  pure (cafAllocationLines <> whnfGlobalLines <> cafInitializationLines)
  where
    valueEnv = ValueEnv env Map.empty ".Laihc_initializer" (FunctionName "") [] ".Laihc_initializer"

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
      registerParameterCopies =
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
          <> [ ".p2align 3",
               label <> ":"
             ]
          <> registerParameterCopies
      blocks =
        BlockLayout.renderBlocks
          (<> ":")
          ("  jmp " <>)
          (BlockLayout.layoutBlocks bodyLabel (reverse (functionBlocksRev finalState)))
  pure
    CompiledFunction
      { compiledFunctionSlots = slotCount,
        compiledFunctionLines = entry <> blocks
      }

reserveLocalsLines :: [CompiledFunction] -> [Text]
reserveLocalsLines functions =
  [ immediate "rsi" maximumSlots,
    "  mov rdi, r15",
    "  call aihc_alloc_locals",
    "  mov r14, rax"
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
          pure (nodeLines <> storeLocation "rax" location)
      initializationLines <-
        fmap concat . forM bindings $ \(var, node) -> do
          location <- liftEither (variableLocation env var)
          fieldLines <- liftEither (initializeNodeFields env node)
          pure (loadLocation "r13" location <> fieldLines)
      compileExpr env (prefix <> allocationLines <> initializationLines) label body
    GrinStoreRecUnchecked bindings body -> do
      allocationLines <-
        fmap concat . forM bindings $ \(var, node) -> do
          location <- liftEither (variableLocation env var)
          nodeLines <- liftEither (allocateNodeUnchecked env node)
          pure (nodeLines <> storeLocation "rax" location)
      initializationLines <-
        fmap concat . forM bindings $ \(var, node) -> do
          location <- liftEither (variableLocation env var)
          fieldLines <- liftEither (initializeNodeFields env node)
          pure (loadLocation "r13" location <> fieldLines)
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
            <> [storeAt "rax" "r14" valueSlot]
            <> continuationLines
            <> [storeAt "rax" "r14" continuationSlot]
            <> updateLines
            <> [ storeAt "rax" "r14" updateSlot,
                 loadAt applyFunctionRegister "r14" valueSlot,
                 loadAt applyContinuationRegister "r14" continuationSlot,
                 loadAt "rax" "r14" updateSlot,
                 immediate "r11" (fromEnum (isLiftedRuntimeRep runtimeRep))
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
                    <> moveDirectOverflowLines "r14" (length arguments)
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
                      <> moveDirectOverflowLines "r14" (length values)
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
          pure (lines' <> [storeAt "rax" "r14" slot])
      let stackBytes = applyStackBytes (length arguments)
          stackRestoreLines = restoreApplyStackLines stackBytes
          slowApplyLines =
            stackRestoreLines
              <> [ loadAt "rsi" "r14" scratch,
                   immediate "rdx" (length arguments),
                   slotPointer "rcx" argumentSlots,
                   "  lea r8, [r14 + " <> tshow (continuationSlot * 8) <> "]",
                   "  mov rdi, r15",
                   "  call aihc_apply_slow",
                   loadAt applyFunctionRegister "r14" continuationSlot
                 ]
      addBlock
        label
        ( prefix
            <> functionLines
            <> [storeAt "rax" "r14" scratch]
            <> continuationLines
            <> [storeAt "rax" "r14" continuationSlot]
            <> argumentLines
            <> [ loadAt applyFunctionRegister "r14" scratch,
                 loadAt applyContinuationRegister "r14" continuationSlot
               ]
            <> [loadAt register "r14" slot | (register, slot) <- zip applyArgumentRegisters argumentSlots]
            <> saveApplyOverflowLines "r14" argumentSlots
            <> [ "  mov r11, QWORD PTR [" <> applyFunctionRegister <> "]",
                 "  and r11, -8",
                 "  mov r11, QWORD PTR [r11 + 48]",
                 "  test r11, r11",
                 "  jz " <> slowLabel,
                 "  jmp r11",
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
        (prefix <> ["  mov rdi, r15", "  call aihc_halt", "  jmp rax"])
        BlockLayout.Exit
    GrinCase scrutinee binder alternatives ->
      compileCase env prefix label scrutinee binder alternatives
    GrinThrow {} -> unsupportedExpression "throw"
    GrinCatch {} -> unsupportedExpression "catch"
    GrinForeignCallExpr {} -> unsupportedExpression "unbound foreign call after CPS"
  where
    unsupportedExpression name = lift (Left (Amd64UnsupportedExpression name))

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
            <> [storeAt "rax" "r14" actionSlot]
            <> continuationLines
            <> [ storeAt "rax" "r14" continuationSlot,
                 loadAt "rsi" "r14" actionSlot,
                 "  mov rdi, r15",
                 "  call aihc_fork",
                 loadAt applyFunctionRegister "r14" continuationSlot
               ]
        )
        (BlockLayout.Jump ".Laihc_enter")
    ("yield#", []) ->
      addBlock
        label
        ( prefix
            <> continuationLines
            <> [ storeAt "rax" "r14" continuationSlot,
                 loadAt "rsi" "r14" continuationSlot,
                 "  mov rdi, r15",
                 "  call aihc_yield"
               ]
        )
        (BlockLayout.Jump ".Laihc_resume")
    ("awaitIO#", [request]) -> do
      requestSlot <- freshSlot
      requestLines <- liftEither (materializeValue env request)
      addBlock
        label
        ( prefix
            <> requestLines
            <> [storeAt "rax" "r14" requestSlot]
            <> continuationLines
            <> [ storeAt "rax" "r14" continuationSlot,
                 loadAt "rsi" "r14" requestSlot,
                 loadAt "rdx" "r14" continuationSlot,
                 "  mov rdi, r15",
                 "  call aihc_await_io"
               ]
        )
        (BlockLayout.Jump ".Laihc_resume")
    _ -> lift (Left (Amd64UnsupportedExpression ("CPS primitive call " <> name)))

compileDirectBinding :: ValueEnv -> [GrinVar] -> GrinExpr -> FunctionM [Text]
compileDirectBinding env vars expression =
  case expression of
    GrinConstant values
      | length vars == length values ->
          fmap concat . forM (zip vars values) $ \(var, value) -> do
            location <- liftEither (variableLocation env var)
            valueLines <- liftEither (materializeValue env value)
            pure (valueLines <> storeLocation "rax" location)
    GrinStore node -> do
      nodeLines <- liftEither (materializeNode env node)
      storeSingleResult vars nodeLines
    GrinEnsureHeap requiredWords roots
      | length vars == length roots -> do
          rootSlots <- freshSlots (length roots)
          rootLines <-
            fmap concat . forM (zip rootSlots roots) $ \(slot, root) -> do
              valueLines <- liftEither (materializeValue env root)
              pure (valueLines <> [storeAt "rax" "r14" slot])
          resultLines <-
            fmap concat . forM (zip vars rootSlots) $ \(var, slot) -> do
              location <- liftEither (variableLocation env var)
              pure ([loadAt "r11" "r14" slot] <> storeLocation "r11" location)
          readyLabel <- freshLabel (valueLabelPrefix env) "heap_ready"
          pure
            ( rootLines
                <> [ loadByteOffset "r10" "r15" 24,
                     loadByteOffset "r11" "r15" 32,
                     immediate "rax" (requiredWords * 8),
                     "  add rax, r10",
                     "  cmp rax, r11",
                     "  jbe " <> readyLabel,
                     "  mov rdi, r15",
                     immediate "rsi" requiredWords,
                     immediate "rdx" (length roots),
                     slotPointer "rcx" rootSlots,
                     "  call aihc_ensure_heap",
                     readyLabel <> ":"
                   ]
                <> resultLines
            )
      | otherwise -> lift (Left (Amd64UnsupportedExpression "heap reservation result arity"))
    GrinStoreUnchecked node -> do
      nodeLines <- liftEither (materializeNodeUnchecked env node)
      storeSingleResult vars nodeLines
    GrinFetch _ pointer -> do
      pointerLines <- liftEither (materializeValue env pointer)
      storeSingleResult vars pointerLines
    GrinUpdate pointer value -> compileUpdateBinding False "aihc_update" pointer value
    GrinUpdateBlackhole pointer value -> compileUpdateBinding True "aihc_update_blackhole" pointer value
    GrinPrimitiveCall IntRep "+#" [left, right] -> do
      leftLines <- liftEither (materializeValueTo env "r10" left)
      rightLines <- liftEither (materializeValue env right)
      storeSingleResult
        vars
        ( leftLines
            <> rightLines
            <> ["  add rax, r10"]
        )
    GrinPrimitiveCall runtimeRep name arguments
      | name == "realWorld#",
        null arguments,
        null vars,
        null (runtimeRepComponents runtimeRep) ->
          pure []
    GrinPrimitiveCall _ name [value]
      | name `elem` ["unsafeFreezeByteArray#", "unsafeThawByteArray#"] -> do
          valueLines <- liftEither (materializeValue env value)
          storeSingleResult vars valueLines
    GrinPrimitiveCall _ name arguments
      | Just foreignCall <- nativeRuntimePrimitiveCall name -> do
          callLines <- compileForeignCallLines env foreignCall arguments
          case vars of
            [] -> pure callLines
            [_] -> storeSingleResult vars callLines
            _ -> lift (Left (Amd64UnsupportedExpression ("byte array primitive result arity " <> name)))
      | compileAllowUnsupportedPrimitives (valueCompileEnv env) ->
          pure ["  call aihc_unsupported_primitive"]
      | otherwise -> lift (Left (Amd64UnsupportedExpression ("primitive call " <> name)))
    GrinForeignCallExpr foreignCall arguments -> do
      callLines <- compileForeignCallLines env foreignCall arguments
      storeSingleResult vars callLines
    _ -> lift (Left (Amd64UnsupportedExpression "non-direct expression remained in a CPS bind"))
  where
    storeSingleResult resultVars lines' =
      case resultVars of
        [var] -> do
          location <- liftEither (variableLocation env var)
          pure (lines' <> storeLocation "rax" location)
        _ -> lift (Left (Amd64UnsupportedExpression "direct expression result arity"))
    compileUpdateBinding passMachine symbol pointer value = do
      pointerSlot <- freshSlot
      valueSlot <- freshSlot
      pointerLines <- liftEither (materializeValue env pointer)
      valueLines <- liftEither (materializeValue env value)
      resultLines <- storeSingleResult vars [loadAt "rax" "r14" valueSlot]
      pure
        ( pointerLines
            <> [storeAt "rax" "r14" pointerSlot]
            <> valueLines
            <> [ storeAt "rax" "r14" valueSlot,
                 loadAt (if passMachine then "rsi" else "rdi") "r14" pointerSlot,
                 loadAt (if passMachine then "rdx" else "rsi") "r14" valueSlot
               ]
            <> ["  mov rdi, r15" | passMachine]
            <> [ "  call " <> symbol
               ]
            <> resultLines
        )

compileForeignCallLines :: ValueEnv -> GrinForeignCall -> [GrinValue] -> FunctionM [Text]
compileForeignCallLines env foreignCall arguments = do
  let signature = grinForeignCallSignature foreignCall
      abiArity = length (grinForeignArgumentTypes signature)
      expectedArity = length (grinForeignOperandReps signature)
  if length arguments /= expectedArity
    then lift (Left (Amd64UnsupportedExpression "foreign call arity mismatch"))
    else
      if abiArity > length foreignArgumentRegisters
        then lift (Left (Amd64UnsupportedExpression "foreign calls with more than six arguments"))
        else do
          argumentSlots <- mapM (const freshSlot) arguments
          argumentLines <-
            fmap concat . forM (zip arguments argumentSlots) $ \(argument, slot) -> do
              valueLines <- liftEither (materializeValue env argument)
              pure (valueLines <> [storeAt "rax" "r14" slot])
          let abiSlots = take abiArity argumentSlots
              loadAbiArguments =
                [ loadAt register "r14" slot
                | (register, slot) <- zip foreignArgumentRegisters abiSlots
                ]
              callLines =
                argumentLines
                  <> loadAbiArguments
                  <> ["  call " <> grinForeignCallSymbol foreignCall]
                  <> normalizeForeignResult (grinForeignResultType signature)
          pure callLines

normalizeForeignResult :: GrinForeignType -> [Text]
normalizeForeignResult foreignType =
  case foreignType of
    GrinForeignInt32 -> ["  movsxd rax, eax"]
    GrinForeignWord64 -> []
    GrinForeignAddr -> []

foreignArgumentRegisters :: [Text]
foreignArgumentRegisters = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

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
            then loadLocation "r11" resultLocation <> storeLocation "r11" binderLocation
            else []
    prefixLines <- alternativePrefix env resultLocation alternative
    compileExpr env (binderLines <> prefixLines) alternativeLabel rhs
  where
    materializedScrutinee = do
      slot <- freshSlot
      lines' <- liftEither (materializeValue env scrutinee)
      pure (InHeapSpill slot, lines' <> [storeAt "rax" "r14" slot])

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
                pure ([loadByteOffset "r10" "r11" (8 + index * 8)] <> storeLocation "r10" location)
              else pure []
        pure (if null fields then [] else loadLocation "r11" resultLocation <> fields)
    GrinLitAlt _ -> pure []
    GrinDefaultAlt ->
      fmap concat . forM (grinAltBinders alternative) $ \binder -> do
        if binder `Set.member` grinExprFreeVariables (grinAltRhs alternative)
          then do
            location <- liftEither (variableLocation env binder)
            pure (loadLocation "r11" resultLocation <> storeLocation "r11" location)
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
              ( loadLocation "r11" resultLocation
                  <> [ loadByteOffset "r10" "r11" 0,
                       loadByteOffset "r10" "r10" 0,
                       immediate "r9" identifier,
                       "  cmp r10, r9",
                       "  je " <> target
                     ]
              )
          else lift (Left (Amd64UnsupportedExpression "constructor case on an unboxed value"))
      GrinLitAlt literal ->
        case normalizedLiteralInteger literal of
          Just integer ->
            if scrutineeIsPointer
              then lift (Left (Amd64UnsupportedExpression "literal case on a lifted value"))
              else
                pure
                  ( loadLocation "r10" resultLocation
                      <> [ immediate "r9" integer,
                           "  cmp r10, r9",
                           "  je " <> target
                         ]
                  )
          Nothing -> lift (Left (Amd64UnsupportedValue "string case alternative"))
      GrinDefaultAlt -> pure []
  pure $ case defaultTarget of
    target : _ -> (checks, BlockLayout.Jump target)
    [] -> (checks <> ["  call aihc_no_match", "  ud2"], BlockLayout.Exit)

materializeValue :: ValueEnv -> GrinValue -> Either Amd64Error [Text]
materializeValue env = materializeValueTo env "rax"

materializeValueTo :: ValueEnv -> Text -> GrinValue -> Either Amd64Error [Text]
materializeValueTo env destination value =
  case value of
    GrinVarValue var ->
      case Map.lookup var (valueLocations env) of
        Just location -> Right (loadLocation destination location)
        Nothing -> do
          slot <- globalSlot (valueCompileEnv env) (grinVarName var)
          pure [loadByteOffset "r11" "r15" 0, loadAt destination "r11" slot]
    GrinLitValue literal -> materializeLiteralTo destination (valueCompileEnv env) literal

materializeLiteralTo :: Text -> CompileEnv -> GrinLiteral -> Either Amd64Error [Text]
materializeLiteralTo destination env literal =
  case literal of
    GrinLitAddr value -> do
      label <-
        maybe
          (Left (Amd64UnsupportedValue "unregistered Addr# literal"))
          Right
          (Map.lookup value (compileAddrLiteralLabels env))
      pure [address destination label]
    _ ->
      case normalizedLiteralInteger literal of
        Just integer -> Right [immediate destination integer]
        Nothing -> Left (Amd64UnsupportedValue "string literal")

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

materializeNode :: ValueEnv -> GrinNode -> Either Amd64Error [Text]
materializeNode env node = do
  allocationLines <- allocateNode env node
  fieldLines <- initializeNodeFields env node
  pure $
    allocationLines
      <> ["  mov r13, rax"]
      <> fieldLines
      <> ["  mov rax, r13"]

materializeNodeUnchecked :: ValueEnv -> GrinNode -> Either Amd64Error [Text]
materializeNodeUnchecked env node = do
  allocationLines <- allocateNodeUnchecked env node
  fieldLines <- initializeNodeFields env node
  pure $
    allocationLines
      <> ["  mov r13, rax"]
      <> fieldLines
      <> ["  mov rax, r13"]

allocateNode :: ValueEnv -> GrinNode -> Either Amd64Error [Text]
allocateNode env node = do
  (tag, info) <- nodeHeader env node
  pure (makeNodeLines tag info)

allocateNodeUnchecked :: ValueEnv -> GrinNode -> Either Amd64Error [Text]
allocateNodeUnchecked env node = do
  (tag, info) <- nodeHeader env node
  pure (makeNodeUncheckedLines tag info)

initializeNodeFields :: ValueEnv -> GrinNode -> Either Amd64Error [Text]
initializeNodeFields env node =
  fmap concat . forM (zip [0 :: Int ..] (grinNodeFields node)) $ \(index, field) -> do
    valueLines <- materializeValue env field
    pure $
      valueLines
        <> [ "  mov rdx, rax",
             "  mov rdi, r13",
             immediate "rsi" index,
             "  call aihc_set_field"
           ]

nodeHeader :: ValueEnv -> GrinNode -> Either Amd64Error (Int, NodeInfo)
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
  [ "  mov rdi, r15",
    immediate "rsi" kind,
    infoLine info,
    "  call aihc_make_node"
  ]
  where
    infoLine nodeInfo =
      case nodeInfo of
        InfoImmediate integer -> immediate "rdx" integer
        InfoAddress label -> address "rdx" label

makeNodeUncheckedLines :: Int -> NodeInfo -> [Text]
makeNodeUncheckedLines kind info =
  init (makeNodeLines kind info) <> ["  call aihc_make_node_unchecked"]

renderEnterStubs :: [RuntimeInfo] -> [Text]
renderEnterStubs = concatMap renderStub
  where
    renderStub info =
      case runtimeInfoEnter info of
        Nothing -> []
        Just apply ->
          [ ".text",
            ".p2align 4",
            enterEntryLabel info <> ":"
          ]
            <> moveSupplied apply
            <> moveSuppliedOverflow apply
            <> loadStored apply
            <> restoreApplyStackLines (applyStackBytes (runtimeEnterSuppliedCount apply))
            <> ["  jmp " <> runtimeEnterTarget apply]
    moveSupplied apply =
      concat
        [ placeArgument targetIndex source
        | (sourceIndex, source) <- reverse (zip [0 :: Int ..] applyArgumentRegisters),
          sourceIndex < runtimeEnterSuppliedCount apply,
          let targetIndex = runtimeEnterStoredCount apply + sourceIndex
        ]
    moveSuppliedOverflow apply =
      concat
        [ [ "  mov r11, QWORD PTR [rsp + " <> tshow ((sourceIndex - length applyArgumentRegisters) * 8) <> "]",
            storeAt "r11" "r14" targetIndex
          ]
        | sourceIndex <- [length applyArgumentRegisters .. runtimeEnterSuppliedCount apply - 1],
          let targetIndex = runtimeEnterStoredCount apply + sourceIndex
        ]
    loadStored apply =
      concat
        [ if targetIndex < length applyArgumentRegisters
            then [loadByteOffset (applyArgumentRegisters !! targetIndex) applyFunctionRegister ((targetIndex + 1) * 8)]
            else
              [ loadByteOffset "r11" applyFunctionRegister ((targetIndex + 1) * 8),
                storeAt "r11" "r14" targetIndex
              ]
        | targetIndex <- [0 .. runtimeEnterStoredCount apply - 1]
        ]
    placeArgument targetIndex source
      | targetIndex < length applyArgumentRegisters =
          ["  mov " <> applyArgumentRegisters !! targetIndex <> ", " <> source]
      | otherwise = [storeAt source "r14" targetIndex]

enterEntryLabel :: RuntimeInfo -> Text
enterEntryLabel info = runtimeInfoLabel info <> "_enter"

applyFunctionRegister, applyContinuationRegister :: Text
applyFunctionRegister = "r12"
applyContinuationRegister = "r13"

applyArgumentRegisters :: [Text]
applyArgumentRegisters = ["rax", "rdi", "rsi", "rdx", "rcx", "r8", "r9"]

applyStackBytes :: Int -> Int
applyStackBytes suppliedCount =
  ((overflowCount * 8 + 15) `div` 16) * 16
  where
    overflowCount = max 0 (suppliedCount - length applyArgumentRegisters)

moveEntryParameters :: [(Text, Location Text)] -> [Text]
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

moveValuesToRegisters :: ValueEnv -> [GrinValue] -> [Text] -> Either Amd64Error [Text]
moveValuesToRegisters env values registers =
  renderRegisterMoves
    env
    [ (destination, moveSource env value)
    | (value, destination) <- zip values registers,
      moveSource env value /= MoveRegister destination
    ]

moveValuesToLocations :: ValueEnv -> [GrinValue] -> [Location Text] -> FunctionM [Text]
moveValuesToLocations env values destinations
  | and (zipWith alreadyThere values destinations) = pure []
  | otherwise = do
      slots <- freshSlots (length values)
      stores <-
        fmap concat . forM (zip values slots) $ \(value, slot) -> do
          lines' <- liftEither (materializeValue env value)
          pure (lines' <> [storeAt "rax" "r14" slot])
      let loads =
            concat
              [ [loadAt "r11" "r14" slot] <> storeLocation "r11" destination
              | (slot, destination) <- zip slots destinations
              ]
      pure (stores <> loads)
  where
    alreadyThere value destination =
      case value of
        GrinVarValue var -> Map.lookup var (valueLocations env) == Just destination
        GrinLitValue {} -> False

moveSource :: ValueEnv -> GrinValue -> MoveSource
moveSource env value =
  case value of
    GrinVarValue var ->
      case Map.lookup var (valueLocations env) of
        Just (InRegister register) -> MoveRegister register
        Just (InHeapSpill slot) -> MoveSpill slot
        Nothing -> MoveValue value
    GrinLitValue {} -> MoveValue value

renderRegisterMoves :: ValueEnv -> [(Text, MoveSource)] -> Either Amd64Error [Text]
renderRegisterMoves env = go
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
              rest <- go (map (replaceSource source "r10") moves)
              pure (["  mov r10, " <> source] <> rest)
            [] -> Left (Amd64UnsupportedExpression "unresolvable register transfer")
    emitMove destination source =
      case source of
        MoveRegister register -> pure ["  mov " <> destination <> ", " <> register]
        MoveSpill slot -> pure [loadAt destination "r14" slot]
        MoveValue value -> materializeValueTo env destination value

renderRegisterMovesWithoutValues :: [(Text, MoveSource)] -> [Text]
renderRegisterMovesWithoutValues = go
  where
    go [] = []
    go moves =
      case takeSafeMove moves of
        Just ((destination, source), remaining) -> emitMove destination source <> go remaining
        Nothing ->
          case [source | (_, MoveRegister source) <- moves] of
            source : _ ->
              ["  mov r10, " <> source] <> go (map (replaceSource source "r10") moves)
            [] -> []
    emitMove destination source =
      case source of
        MoveRegister register -> ["  mov " <> destination <> ", " <> register]
        MoveSpill slot -> [loadAt destination "r14" slot]
        MoveValue {} -> []

takeSafeMove :: [(Text, MoveSource)] -> Maybe ((Text, MoveSource), [(Text, MoveSource)])
takeSafeMove moves = select [] moves
  where
    sourceRegisters = Set.fromList [register | (_, MoveRegister register) <- moves]
    select _ [] = Nothing
    select previous (move@(destination, _) : rest)
      | destination `Set.notMember` sourceRegisters = Just (move, reverse previous <> rest)
      | otherwise = select (move : previous) rest

replaceSource :: Text -> Text -> (Text, MoveSource) -> (Text, MoveSource)
replaceSource old new (destination, source) =
  ( destination,
    case source of
      MoveRegister register | register == old -> MoveRegister new
      _ -> source
  )

saveValueOverflowLines :: ValueEnv -> [GrinValue] -> Either Amd64Error [Text]
saveValueOverflowLines env values
  | stackBytes == 0 = pure []
  | otherwise = do
      stores <-
        fmap concat . forM (zip [0 :: Int ..] (drop (length applyArgumentRegisters) values)) $ \(index, value) -> do
          lines' <- materializeValueTo env "r11" value
          pure (lines' <> ["  mov QWORD PTR [rsp + " <> tshow (index * 8) <> "], r11"])
      pure (["  sub rsp, " <> tshow stackBytes] <> stores)
  where
    stackBytes = applyStackBytes (length values)

saveApplyOverflowLines :: Text -> [Int] -> [Text]
saveApplyOverflowLines base slots
  | stackBytes == 0 = []
  | otherwise =
      ["  sub rsp, " <> tshow stackBytes]
        <> concat
          [ [loadAt "r11" base slot, "  mov QWORD PTR [rsp + " <> tshow (index * 8) <> "], r11"]
          | (index, slot) <- zip [0 :: Int ..] (drop (length applyArgumentRegisters) slots)
          ]
  where
    stackBytes = applyStackBytes (length slots)

restoreApplyStackLines :: Int -> [Text]
restoreApplyStackLines stackBytes
  | stackBytes == 0 = []
  | otherwise = ["  add rsp, " <> tshow stackBytes]

moveDirectOverflowLines :: Text -> Int -> [Text]
moveDirectOverflowLines base valueCount
  | stackBytes == 0 = []
  | otherwise =
      concat
        [ [ "  mov r11, QWORD PTR [rsp + " <> tshow ((targetIndex - length applyArgumentRegisters) * 8) <> "]",
            storeAt "r11" base targetIndex
          ]
        | targetIndex <- [length applyArgumentRegisters .. valueCount - 1]
        ]
        <> restoreApplyStackLines stackBytes
  where
    stackBytes = applyStackBytes valueCount

renderRuntimeInfos :: [RuntimeInfo] -> [Text]
renderRuntimeInfos infos =
  [".section .rodata"] <> concatMap renderInfo infos
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

variableLocation :: ValueEnv -> GrinVar -> Either Amd64Error (Location Text)
variableLocation env var =
  maybe
    (Left (Amd64UnsupportedExpression ("missing location for " <> grinVarName var)))
    Right
    (Map.lookup var (valueLocations env))

loadLocation :: Text -> Location Text -> [Text]
loadLocation destination location =
  case location of
    InRegister source
      | destination == source -> []
      | otherwise -> ["  mov " <> destination <> ", " <> source]
    InHeapSpill slot -> [loadAt destination "r14" slot]

storeLocation :: Text -> Location Text -> [Text]
storeLocation source location =
  case location of
    InRegister destination
      | destination == source -> []
      | otherwise -> ["  mov " <> destination <> ", " <> source]
    InHeapSpill slot -> [storeAt source "r14" slot]

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
    first : _ -> "  lea " <> register <> ", [r14" <> offsetText (first * 8) <> "]"
    [] -> "  xor " <> register <> ", " <> register

renderNativeControl :: [Text]
renderNativeControl =
  [ ".text",
    ".p2align 4",
    ".Laihc_enter:",
    "  mov r11, QWORD PTR [r12]",
    "  and r11, -8",
    "  mov r11, QWORD PTR [r11 + 48]",
    "  test r11, r11",
    "  jz .Laihc_invalid_enter",
    "  jmp r11",
    ".Laihc_resume:",
    "  mov r11d, DWORD PTR [rax]",
    "  cmp r11d, 1",
    "  je .Laihc_resume_apply",
    "  cmp r11d, 2",
    "  jne .Laihc_invalid_enter",
    "  mov r10, QWORD PTR [rax + 24]",
    "  mov r11, QWORD PTR [rax + 32]",
    "  mov r12, QWORD PTR [rax + 8]",
    "  mov QWORD PTR [rax], 0",
    "  mov QWORD PTR [rax + 8], 0",
    "  mov QWORD PTR [rax + 16], 0",
    "  mov QWORD PTR [rax + 24], 0",
    "  mov QWORD PTR [rax + 32], 0",
    "  test r11, r11",
    "  jz .Laihc_enter",
    "  cmp r11, 1",
    "  jne .Laihc_invalid_enter",
    "  mov rax, r10",
    "  jmp .Laihc_enter",
    ".Laihc_resume_apply:",
    "  mov r12, QWORD PTR [rax + 8]",
    "  mov r13, QWORD PTR [rax + 16]",
    "  mov QWORD PTR [rax], 0",
    "  mov QWORD PTR [rax + 8], 0",
    "  mov QWORD PTR [rax + 16], 0",
    "  mov QWORD PTR [rax + 24], 0",
    "  mov QWORD PTR [rax + 32], 0",
    "  jmp .Laihc_enter",
    ".Laihc_eval:",
    "  mov QWORD PTR [r14], rax",
    "  mov QWORD PTR [r14 + 8], r11",
    ".Laihc_eval_loop:",
    "  mov r11, QWORD PTR [r12]",
    "  mov r10, r11",
    "  and r10, 7",
    "  cmp r10, 2",
    "  je .Laihc_eval_thunk",
    "  cmp r10, 4",
    "  je .Laihc_eval_indirection",
    "  cmp r10, 5",
    "  je .Laihc_eval_blackhole",
    "  mov rax, r12",
    "  mov r12, r13",
    "  jmp .Laihc_enter",
    ".Laihc_eval_thunk:",
    "  mov rsi, r12",
    "  mov rdi, r15",
    "  call aihc_begin_blackhole",
    "  mov r13, QWORD PTR [r14]",
    "  jmp .Laihc_enter",
    ".Laihc_eval_indirection:",
    "  cmp QWORD PTR [r14 + 8], 0",
    "  je .Laihc_eval_unlifted_indirection",
    "  mov r12, QWORD PTR [r12 + 8]",
    "  jmp .Laihc_eval_loop",
    ".Laihc_eval_unlifted_indirection:",
    "  mov rax, QWORD PTR [r12 + 8]",
    "  mov r12, r13",
    "  jmp .Laihc_enter",
    ".Laihc_eval_blackhole:",
    "  mov rdx, r13",
    "  mov rsi, r12",
    "  mov rdi, r15",
    "  call aihc_block_on_blackhole",
    "  jmp .Laihc_resume",
    ".Laihc_invalid_enter:",
    "  call aihc_no_match",
    "  ud2"
  ]

globalSlot :: CompileEnv -> Text -> Either Amd64Error Int
globalSlot env name =
  maybe (Left (Amd64MissingGlobal name)) Right (Map.lookup name (compileGlobalSlots env))

constructorId :: CompileEnv -> Text -> Either Amd64Error Int
constructorId env name =
  maybe (Left (Amd64MissingConstructor name)) Right (Map.lookup name (compileConstructorIds env))

lookupRuntimeInfoLabel :: CompileEnv -> RuntimeInfoKey -> Either Amd64Error Text
lookupRuntimeInfoLabel env key =
  case Map.lookup key (compileNodeInfoLabels env) of
    Just label -> Right label
    Nothing ->
      case key of
        ConstructorRuntimeInfo name _ -> Left (Amd64MissingConstructor name)
        ClosureRuntimeInfo functionName _ _ -> Left (Amd64MissingFunction functionName)
        ThunkRuntimeInfo functionName _ -> Left (Amd64MissingFunction functionName)

functionCodeLabel :: CompileEnv -> FunctionName -> Either Amd64Error Text
functionCodeLabel env name =
  maybe (Left (Amd64MissingFunction name)) Right (Map.lookup name (compileFunctionLabels env))

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

validatePrimitiveName :: Bool -> Text -> Either Amd64Error ()
validatePrimitiveName allowUnsupported name
  | name `elem` supportedNativePrimitiveNames = Right ()
  | allowUnsupported = Right ()
  | otherwise = Left (Amd64UnsupportedPrimitive name)

validateRuntimeRep :: RuntimeRep -> Either Amd64Error ()
validateRuntimeRep runtimeRep =
  case runtimeRep of
    VecRep {} -> Left (Amd64UnsupportedRuntimeRep runtimeRep)
    TupleRep fieldReps -> mapM_ validateRuntimeRep fieldReps
    SumRep alternativeReps -> mapM_ validateRuntimeRep alternativeReps
    RuntimeRepVar {} -> Left (Amd64UnsupportedRuntimeRep runtimeRep)
    RuntimeRepMeta {} -> Left (Amd64UnsupportedRuntimeRep runtimeRep)
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

renderObservedMetadata :: CompileEnv -> GrinProgram -> [RuntimeRep] -> Either Amd64Error Text
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

snapshotRepName :: RuntimeRep -> Either Amd64Error Text
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

tableOrNull :: [value] -> Text -> Text
tableOrNull = pointerOrNull

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

renderAddrLiteralPool :: CompileEnv -> [Text]
renderAddrLiteralPool env =
  case Map.toAscList (compileAddrLiteralLabels env) of
    [] -> []
    literals ->
      [".section .rodata"]
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
  | exposeAllFunctions = "aihc_snapshot_function_" <> tshow index
  | otherwise =
      case grinFunctionLinkName function of
        Just name -> linkedFunctionLabel name
        Nothing -> functionLabel index

linkedFunctionLabel :: Text -> Text
linkedFunctionLabel name =
  "aihc_entry_" <> T.concatMap encode name
  where
    encode character = T.pack (showHex (ord character) "_")

storeGlobal :: Int -> [Text]
storeGlobal slot =
  [ loadByteOffset "r11" "r15" 0,
    storeAt "rax" "r11" slot
  ]

loadAt :: Text -> Text -> Int -> Text
loadAt destination base slot = loadByteOffset destination base (slot * 8)

storeAt :: Text -> Text -> Int -> Text
storeAt source base slot = storeByteOffset source base (slot * 8)

loadByteOffset :: Text -> Text -> Int -> Text
loadByteOffset destination base offset =
  "  mov " <> destination <> ", QWORD PTR [" <> base <> offsetText offset <> "]"

storeByteOffset :: Text -> Text -> Int -> Text
storeByteOffset source base offset =
  "  mov QWORD PTR [" <> base <> offsetText offset <> "], " <> source

offsetText :: Int -> Text
offsetText offset
  | offset == 0 = ""
  | offset > 0 = " + " <> tshow offset
  | otherwise = " - " <> tshow (abs offset)

immediate :: (Show value) => Text -> value -> Text
immediate register value = "  mov " <> register <> ", " <> T.pack (show value)

address :: Text -> Text -> Text
address register label =
  "  lea " <> register <> ", [rip + " <> label <> "]"

tshow :: (Show value) => value -> Text
tshow = T.pack . show

liftEither :: Either Amd64Error value -> FunctionM value
liftEither = lift
