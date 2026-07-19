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

import Aihc.Amd64.Emit (EmitError, renderAllocatedBlock)
import Aihc.Amd64.Lir qualified as Lir
import Aihc.Grin.Cps
  ( CpsGrinProgram,
    cpsFunctionContinuations,
    cpsGrinProgram,
    cpsUpdateFunction,
  )
import Aihc.Grin.Syntax
import Aihc.Native
  ( LinkInterface,
    LinkLayout (..),
    buildLinkLayout,
    buildLinkLayoutFromInterfaces,
    extendLinkLayout,
    extendLinkLayoutWithInterface,
    extractLinkInterface,
  )
import Aihc.Tc.Types (RuntimeRep (..))
import Control.Monad (forM, replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, execStateT, get, modify')
import Data.Char (ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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
  | Amd64EmitError !EmitError
  deriving (Eq, Show)

data CompileEnv = CompileEnv
  { compileConstructorIds :: !(Map Text Int),
    compileConstructorArities :: !(Map Text Int),
    compileGlobalSlots :: !(Map Text Int),
    compileFunctionLabels :: !(Map FunctionName Text),
    compileExposeAllFunctions :: !Bool,
    compileAllowUnsupportedPrimitives :: !Bool
  }

data ObservedProgram = ObservedProgram
  { observedAssembly :: !Text,
    observedMetadataSource :: !Text
  }
  deriving (Eq, Show)

data Block = Block
  { blockLabel :: !Text,
    blockLines :: ![Text]
  }

data FunctionState = FunctionState
  { functionNextLabel :: !Int,
    functionNextSlot :: !Int,
    functionBlocksRev :: ![Block]
  }

type FunctionM = StateT FunctionState (Either Amd64Error)

data ValueEnv = ValueEnv
  { valueCompileEnv :: !CompileEnv,
    valueLocalSlots :: !(Map GrinVar Int)
  }

compileProgram :: Text -> CpsGrinProgram -> Either Amd64Error Text
compileProgram entryName cpsProgram =
  compileProgramWithDependencies (buildLinkLayout [program]) [] entryName cpsProgram
  where
    program = cpsGrinProgram cpsProgram

-- | Compile a nullary function with a driver that snapshots its raw return
-- values. The driver does not evaluate or apply any returned heap object.
compileObservedFunction :: FunctionName -> CpsGrinProgram -> Either Amd64Error ObservedProgram
compileObservedFunction entryName cpsProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  validateProgramPrimitives program
  entryFunction <-
    maybe (Left (Amd64MissingFunction entryName)) Right $
      findFunction entryName (grinFunctions program)
  case Map.lookup entryName (cpsFunctionContinuations cpsProgram) of
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
            <> constructorLines
            <> initLines
            <> makeNodeLines runtimeTagClosure (InfoAddress ".Laihc_snapshot_result") 1 0
            <> [ "  mov r13, rax",
                 immediate "rdi" (1 :: Int),
                 "  call aihc_alloc_locals",
                 storeByteOffset "r13" "rax" 0,
                 storeByteOffset "rax" "r15" 0,
                 "  jmp " <> entryLabel,
                 ".p2align 3",
                 ".Laihc_snapshot_result:",
                 loadByteOffset "rsi" "r15" 0,
                 immediate "rdi" resultCount,
                 "  call aihc_snapshot_dump_result",
                 "  xor eax, eax"
               ]
            <> mainEpilogue
            <> concat functions
            <> nonExecutableStack
  pure ObservedProgram {observedAssembly = assembly, observedMetadataSource = metadata}
  where
    program = cpsGrinProgram cpsProgram
    layout = buildLinkLayout [program]
    compileEnv = compileEnvironmentWith True layout program
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
compileModule :: LinkLayout -> Text -> CpsGrinProgram -> Either Amd64Error Text
compileModule layout initializerSymbol cpsProgram = do
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
      <> initLines
      <> mainEpilogue
      <> concat functions
      <> nonExecutableStack
  where
    program = cpsGrinProgram cpsProgram
    compileEnv = (compileEnvironment layout program) {compileAllowUnsupportedPrimitives = True}

-- | Compile the user program entry unit against cached dependency modules.
-- Dependency initializers are called after constructors are installed and
-- before the user module's own globals are initialized.
compileProgramWithDependencies :: LinkLayout -> [Text] -> Text -> CpsGrinProgram -> Either Amd64Error Text
compileProgramWithDependencies layout dependencyInitializers entryName cpsProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  rootSlot <- maybe (Left (Amd64MissingEntry entryName)) Right (Map.lookup entryName globalSlots)
  constructorLines <- compileConstructorInitializers compileEnv
  initLines <- compileInitializers compileEnv program
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  updateLabel <- functionCodeLabel compileEnv (cpsUpdateFunction cpsProgram)
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
      <> makeNodeLines runtimeTagClosure (InfoAddress ".Laihc_final_continuation") 1 0
      <> ["  mov r13, rax"]
      <> makeNodeLines runtimeTagClosure (InfoAddress ".Laihc_top_continuation") 1 1
      <> [ "  mov r12, rax",
           "  mov rdi, r12",
           "  xor esi, esi",
           "  mov rdx, r13",
           "  call aihc_set_field"
         ]
      <> makeNodeLines runtimeTagClosure (InfoAddress updateLabel) 1 2
      <> [ "  mov r14, rax",
           loadByteOffset "r11" "r15" 8,
           loadAt "rdx" "r11" rootSlot,
           "  mov rdi, r14",
           "  xor esi, esi",
           "  call aihc_set_field",
           "  mov rdi, r14",
           "  mov esi, 1",
           "  mov rdx, r12",
           "  call aihc_set_field",
           loadByteOffset "r11" "r15" 8,
           loadAt "rsi" "r11" rootSlot,
           "  mov rdi, r15",
           "  mov rdx, r12",
           "  mov rcx, r14",
           address "r8" ".Laihc_exit",
           "  call aihc_start"
         ]
      <> tailDispatchLines
      <> [ ".p2align 3",
           ".Laihc_top_continuation:",
           loadByteOffset "r11" "r15" 0,
           loadByteOffset "r8" "r11" 0,
           loadByteOffset "rsi" "r11" 8,
           "  mov rdi, r15",
           "  xor edx, edx",
           "  xor ecx, ecx",
           "  call aihc_apply_cps"
         ]
      <> tailDispatchLines
      <> [ ".p2align 3",
           ".Laihc_final_continuation:",
           "  mov rdi, r15",
           "  call aihc_halt"
         ]
      <> tailDispatchLines
      <> [ ".Laihc_exit:",
           "  xor eax, eax"
         ]
      <> mainEpilogue
      <> concat functions
      <> nonExecutableStack
  where
    program = cpsGrinProgram cpsProgram
    compileEnv = compileEnvironment layout program
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
      compileFunctionLabels =
        Map.fromList
          ( [ (grinCodeFunctionName info, linkedFunctionLabel (grinCodeSourceName info))
            | info <- grinExternalFunctions program
            ]
              <> [ (grinFunctionName function, localFunctionLabelWith exposeAllFunctions index function)
                 | (index, function) <- zip [0 ..] (grinFunctions program)
                 ]
          ),
      compileExposeAllFunctions = exposeAllFunctions,
      compileAllowUnsupportedPrimitives = False
    }
  where
    constructors = linkConstructors layout

compileConstructorInitializers :: CompileEnv -> Either Amd64Error [Text]
compileConstructorInitializers env =
  fmap concat . forM nullaryConstructors $ \(name, constructor) -> do
    slot <- globalSlot env name
    pure $ makeNodeLines runtimeTagNode (InfoImmediate constructor) 0 0 <> storeGlobal slot
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
    nodeLines <- materializeNode (ValueEnv env Map.empty) node
    pure (nodeLines <> storeGlobal slot)
  cafAllocationLines <- fmap concat . forM (grinCafs program) $ \(var, node) -> do
    slot <- globalSlot env (grinVarName var)
    allocationLines <- allocateNode (ValueEnv env Map.empty) node
    pure (allocationLines <> storeGlobal slot)
  cafInitializationLines <- fmap concat . forM (grinCafs program) $ \(var, node) -> do
    slot <- globalSlot env (grinVarName var)
    fieldLines <- initializeNodeFields (ValueEnv env Map.empty) node
    pure $
      [loadByteOffset "r11" "r15" 8, loadAt "r13" "r11" slot]
        <> fieldLines
  pure (cafAllocationLines <> whnfGlobalLines <> cafInitializationLines)

compileFunction :: CompileEnv -> GrinFunction -> Either Amd64Error [Text]
compileFunction env function = do
  label <- functionCodeLabel env (grinFunctionName function)
  let localSlots = functionLocalSlots function
      firstScratch = Map.size localSlots
      bodyLabel = label <> "_body"
      initialState = FunctionState 0 firstScratch []
      valueEnv = ValueEnv env localSlots
  finalState <- execStateT (compileExpr valueEnv [] bodyLabel (grinFunctionBody function)) initialState
  let spillBase = functionNextSlot finalState
  (parameterCopies, spillCount) <-
    either (Left . Amd64EmitError) Right (renderAllocatedBlock spillBase (parameterCopyLir localSlots (grinFunctionParameters function)))
  let slotCount = max 1 (spillBase + spillCount)
      entry =
        exportLines env function label
          <> [ ".p2align 3",
               label <> ":",
               immediate "rdi" slotCount,
               "  call aihc_alloc_locals",
               "  mov r14, rax",
               loadByteOffset "r12" "r15" 0
             ]
          <> parameterCopies
          <> ["  jmp " <> bodyLabel]
      blocks = concatMap renderBlock (reverse (functionBlocksRev finalState))
  pure (entry <> blocks)

exportLines :: CompileEnv -> GrinFunction -> Text -> [Text]
exportLines env function label
  | compileExposeAllFunctions env = [".globl " <> label]
  | otherwise =
      case grinFunctionLinkName function of
        Just _ -> [".globl " <> label]
        Nothing -> []

parameterCopyLir :: Map GrinVar Int -> [GrinVar] -> [Lir.Instruction]
parameterCopyLir slots parameters =
  concat
    [ [ Lir.Load (Lir.Virtual register) (Lir.Physical Lir.R12) (argumentIndex * 8),
        Lir.Store (Lir.Virtual register) (Lir.Physical Lir.R14) (slot * 8)
      ]
    | (argumentIndex, var) <- zip [0 :: Int ..] parameters,
      let register = Lir.VirtualReg argumentIndex,
      Just slot <- [Map.lookup var slots]
    ]

compileExpr :: ValueEnv -> [Text] -> Text -> GrinExpr -> FunctionM ()
compileExpr env prefix label expression =
  case expression of
    GrinConstant {} -> unsupportedExpression "direct-style constant return after CPS"
    GrinBind vars valueExpression body -> do
      directLines <- compileDirectBinding env vars valueExpression
      compileExpr env (prefix <> directLines) label body
    GrinStore {} -> unsupportedExpression "direct-style store return after CPS"
    GrinStoreRec bindings body -> do
      allocationLines <-
        fmap concat . forM bindings $ \(var, node) -> do
          slot <- localSlot env var
          nodeLines <- liftEither (allocateNode env node)
          pure (nodeLines <> [storeAt "rax" "r14" slot])
      initializationLines <-
        fmap concat . forM bindings $ \(var, node) -> do
          slot <- localSlot env var
          fieldLines <- liftEither (initializeNodeFields env node)
          pure ([loadAt "r13" "r14" slot] <> fieldLines)
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
                 loadAt "rsi" "r14" valueSlot,
                 immediate "rdx" (fromEnum (isLiftedRuntimeRep runtimeRep)),
                 loadAt "rcx" "r14" continuationSlot,
                 loadAt "r8" "r14" updateSlot,
                 "  mov rdi, r15",
                 "  call aihc_eval_cps"
               ]
            <> tailDispatchLines
        )
    GrinCall _ functionName arguments -> do
      target <- liftEither (functionCodeLabel (valueCompileEnv env) functionName)
      argumentSlots <- freshSlots (length arguments)
      argumentLines <-
        fmap concat . forM (zip arguments argumentSlots) $ \(argument, slot) -> do
          lines' <- liftEither (materializeValue env argument)
          pure (lines' <> [storeAt "rax" "r14" slot])
      addBlock
        label
        ( prefix
            <> argumentLines
            <> [slotPointer "r12" argumentSlots, storeByteOffset "r12" "r15" 0, "  jmp " <> target]
        )
    GrinPrimitiveCall {} -> unsupportedExpression "unbound primitive call after CPS"
    GrinApply {} -> unsupportedExpression "direct-style apply after CPS"
    GrinCpsApply _ function arguments continuation -> do
      scratch <- freshSlot
      continuationSlot <- freshSlot
      functionLines <- liftEither (materializeValue env function)
      continuationLines <- liftEither (materializeValue env continuation)
      argumentSlots <- freshSlots (length arguments)
      argumentLines <-
        fmap concat . forM (zip arguments argumentSlots) $ \(argument, slot) -> do
          lines' <- liftEither (materializeValue env argument)
          pure (lines' <> [storeAt "rax" "r14" slot])
      addBlock
        label
        ( prefix
            <> functionLines
            <> [storeAt "rax" "r14" scratch]
            <> continuationLines
            <> [storeAt "rax" "r14" continuationSlot]
            <> argumentLines
            <> [ loadAt "rsi" "r14" scratch,
                 immediate "rdx" (length arguments),
                 slotPointer "rcx" argumentSlots,
                 loadAt "r8" "r14" continuationSlot,
                 "  mov rdi, r15",
                 "  call aihc_apply_cps"
               ]
            <> tailDispatchLines
        )
    GrinContinue continuation values -> do
      continuationSlot <- freshSlot
      valueSlots <- freshSlots (length values)
      continuationLines <- liftEither (materializeValue env continuation)
      valueLines <-
        fmap concat . forM (zip values valueSlots) $ \(value, slot) -> do
          lines' <- liftEither (materializeValue env value)
          pure (lines' <> [storeAt "rax" "r14" slot])
      addBlock
        label
        ( prefix
            <> continuationLines
            <> [storeAt "rax" "r14" continuationSlot]
            <> valueLines
            <> [ loadAt "rsi" "r14" continuationSlot,
                 immediate "rdx" (length values),
                 slotPointer "rcx" valueSlots,
                 "  mov rdi, r15",
                 "  call aihc_continue_values"
               ]
            <> tailDispatchLines
        )
    GrinHalt _ ->
      addBlock
        label
        (prefix <> ["  mov rdi, r15", "  call aihc_halt"] <> tailDispatchLines)
    GrinCase scrutinee binder alternatives ->
      compileCase env prefix label scrutinee binder alternatives
    GrinThrow {} -> unsupportedExpression "throw"
    GrinCatch {} -> unsupportedExpression "catch"
    GrinForeignCallExpr {} -> unsupportedExpression "unbound foreign call after CPS"
  where
    unsupportedExpression name = lift (Left (Amd64UnsupportedExpression name))

compileDirectBinding :: ValueEnv -> [GrinVar] -> GrinExpr -> FunctionM [Text]
compileDirectBinding env vars expression =
  case expression of
    GrinConstant values
      | length vars == length values ->
          fmap concat . forM (zip vars values) $ \(var, value) -> do
            slot <- localSlot env var
            valueLines <- liftEither (materializeValue env value)
            pure (valueLines <> [storeAt "rax" "r14" slot])
    GrinStore node -> do
      nodeLines <- liftEither (materializeNode env node)
      storeSingleResult vars nodeLines
    GrinFetch _ pointer -> do
      pointerLines <- liftEither (materializeValue env pointer)
      storeSingleResult vars pointerLines
    GrinUpdate pointer value -> compileUpdateBinding "aihc_update" pointer value
    GrinUpdateBlackhole pointer value -> compileUpdateBinding "aihc_update_blackhole" pointer value
    GrinPrimitiveCall runtimeRep name arguments
      | name == "realWorld#",
        null arguments,
        null vars,
        null (runtimeRepComponents runtimeRep) ->
          pure []
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
          slot <- localSlot env var
          pure (lines' <> [storeAt "rax" "r14" slot])
        _ -> lift (Left (Amd64UnsupportedExpression "direct expression result arity"))
    compileUpdateBinding symbol pointer value = do
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
                 loadAt "rdi" "r14" pointerSlot,
                 loadAt "rsi" "r14" valueSlot,
                 "  call " <> symbol
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

foreignArgumentRegisters :: [Text]
foreignArgumentRegisters = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

compileCase :: ValueEnv -> [Text] -> Text -> GrinValue -> GrinVar -> [GrinAlt] -> FunctionM ()
compileCase env prefix label scrutinee binder alternatives = do
  resultSlot <- freshSlot
  dispatchLabel <- freshLabel label "case_dispatch"
  scrutineeLines <- liftEither (materializeValue env scrutinee)
  let scrutineeIsPointer = isPointerRuntimeRep (grinValueRuntimeRep scrutinee)
  addBlock
    label
    ( prefix
        <> scrutineeLines
        <> [storeAt "rax" "r14" resultSlot, "  jmp " <> dispatchLabel]
    )
  binderSlot <- localSlot env binder
  alternativeTargets <- forM alternatives $ \alternative -> do
    alternativeLabel <- freshLabel label "case_alt"
    prefixLines <- alternativePrefix env resultSlot alternative
    compileExpr env prefixLines alternativeLabel (grinAltRhs alternative)
    pure (alternative, alternativeLabel)
  checks <- caseChecks env resultSlot scrutineeIsPointer alternativeTargets
  addBlock
    dispatchLabel
    ( [ loadAt "r11" "r14" resultSlot,
        storeAt "r11" "r14" binderSlot
      ]
        <> checks
    )

alternativePrefix :: ValueEnv -> Int -> GrinAlt -> FunctionM [Text]
alternativePrefix env resultSlot alternative =
  case grinAltCon alternative of
    GrinDataAlt _ ->
      fmap concat . forM (zip [0 ..] (grinAltBinders alternative)) $ \(index, binder) -> do
        slot <- localSlot env binder
        pure
          [ loadAt "r11" "r14" resultSlot,
            loadByteOffset "r10" "r11" (8 + index * 8),
            storeAt "r10" "r14" slot
          ]
    GrinLitAlt _ -> pure []
    GrinDefaultAlt ->
      fmap concat . forM (grinAltBinders alternative) $ \binder -> do
        slot <- localSlot env binder
        pure
          [ loadAt "r11" "r14" resultSlot,
            storeAt "r11" "r14" slot
          ]

caseChecks :: ValueEnv -> Int -> Bool -> [(GrinAlt, Text)] -> FunctionM [Text]
caseChecks env resultSlot scrutineeIsPointer targets = do
  let nonDefault = [(alternative, label) | (alternative, label) <- targets, grinAltCon alternative /= GrinDefaultAlt]
      defaultTarget = [label | (alternative, label) <- targets, grinAltCon alternative == GrinDefaultAlt]
  checks <- fmap concat . forM nonDefault $ \(alternative, target) ->
    case grinAltCon alternative of
      GrinDataAlt name -> do
        if scrutineeIsPointer
          then do
            identifier <- liftEither (constructorId (valueCompileEnv env) name)
            pure
              [ loadAt "r11" "r14" resultSlot,
                loadByteOffset "r10" "r11" 0,
                immediate "r9" (identifier * 8),
                "  cmp r10, r9",
                "  je " <> target
              ]
          else lift (Left (Amd64UnsupportedExpression "constructor case on an unboxed value"))
      GrinLitAlt literal ->
        case normalizedLiteralInteger literal of
          Just integer ->
            if scrutineeIsPointer
              then lift (Left (Amd64UnsupportedExpression "literal case on a lifted value"))
              else
                pure
                  [ loadAt "r10" "r14" resultSlot,
                    immediate "r9" integer,
                    "  cmp r10, r9",
                    "  je " <> target
                  ]
          Nothing -> lift (Left (Amd64UnsupportedValue "string case alternative"))
      GrinDefaultAlt -> pure []
  pure $
    checks
      <> case defaultTarget of
        target : _ -> ["  jmp " <> target]
        [] -> ["  call aihc_no_match", "  ud2"]

materializeValue :: ValueEnv -> GrinValue -> Either Amd64Error [Text]
materializeValue env value =
  case value of
    GrinVarValue var -> loadVariable env var
    GrinLitValue literal -> materializeLiteral literal

materializeLiteral :: GrinLiteral -> Either Amd64Error [Text]
materializeLiteral literal =
  case normalizedLiteralInteger literal of
    Just integer -> Right [immediate "rax" integer]
    Nothing -> Left (Amd64UnsupportedValue "string literal")

normalizedLiteralInteger :: GrinLiteral -> Maybe Integer
normalizedLiteralInteger literal = do
  integer <- literalInteger literal
  pure $
    case literal of
      GrinLitInt runtimeRep _ -> normalizeScalar runtimeRep integer
      GrinLitChar {} -> normalizeUnsigned 64 integer
      GrinLitString {} -> integer

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

materializeNode :: ValueEnv -> GrinNode -> Either Amd64Error [Text]
materializeNode env node = do
  allocationLines <- allocateNode env node
  fieldLines <- initializeNodeFields env node
  pure $
    allocationLines
      <> ["  mov r13, rax"]
      <> fieldLines
      <> ["  mov rax, r13"]

allocateNode :: ValueEnv -> GrinNode -> Either Amd64Error [Text]
allocateNode env node = do
  (tag, info, arity) <- nodeHeader env node
  pure (makeNodeLines tag info arity (length (grinNodeFields node)))

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

nodeHeader :: ValueEnv -> GrinNode -> Either Amd64Error (Int, NodeInfo, Int)
nodeHeader env node =
  case grinNodeTag node of
    GrinConstructor name remaining -> do
      identifier <- constructorId compileEnv name
      pure
        ( if remaining == 0 then runtimeTagNode else runtimeTagPartialConstructor,
          InfoImmediate identifier,
          remaining
        )
    GrinClosure functionName argumentLayouts -> do
      label <- functionCodeLabel compileEnv functionName
      pure (runtimeTagClosure, InfoAddress label, length argumentLayouts)
    GrinThunk functionName -> do
      label <- functionCodeLabel compileEnv functionName
      pure (runtimeTagThunk, InfoAddress label, 0)
  where
    compileEnv = valueCompileEnv env

data NodeInfo
  = InfoImmediate !Int
  | InfoAddress !Text

makeNodeLines :: Int -> NodeInfo -> Int -> Int -> [Text]
makeNodeLines kind info arity count =
  [ immediate "rdi" kind,
    infoLine info,
    immediate "rdx" arity,
    immediate "rcx" count,
    "  call aihc_make_node"
  ]
  where
    infoLine nodeInfo =
      case nodeInfo of
        InfoImmediate integer -> immediate "rsi" integer
        InfoAddress label -> address "rsi" label

runtimeTagNode, runtimeTagClosure, runtimeTagThunk, runtimeTagPartialConstructor :: Int
runtimeTagNode = 0
runtimeTagClosure = 1
runtimeTagThunk = 2
runtimeTagPartialConstructor = 3

loadVariable :: ValueEnv -> GrinVar -> Either Amd64Error [Text]
loadVariable env var =
  case Map.lookup var (valueLocalSlots env) of
    Just slot -> Right [loadAt "rax" "r14" slot]
    Nothing -> do
      slot <- globalSlot (valueCompileEnv env) (grinVarName var)
      pure [loadByteOffset "r11" "r15" 8, loadAt "rax" "r11" slot]

localSlot :: ValueEnv -> GrinVar -> FunctionM Int
localSlot env var =
  case Map.lookup var (valueLocalSlots env) of
    Just slot -> pure slot
    Nothing -> lift (Left (Amd64UnsupportedExpression ("missing local slot for " <> grinVarName var)))

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

addBlock :: Text -> [Text] -> FunctionM ()
addBlock label lines' =
  modify' $ \state -> state {functionBlocksRev = Block label lines' : functionBlocksRev state}

renderBlock :: Block -> [Text]
renderBlock block = blockLabel block <> ":" : blockLines block

slotPointer :: Text -> [Int] -> Text
slotPointer register slots =
  case slots of
    first : _ -> "  lea " <> register <> ", [r14" <> offsetText (first * 8) <> "]"
    [] -> "  xor " <> register <> ", " <> register

tailDispatchLines :: [Text]
tailDispatchLines = ["  jmp rax"]

globalSlot :: CompileEnv -> Text -> Either Amd64Error Int
globalSlot env name =
  maybe (Left (Amd64MissingGlobal name)) Right (Map.lookup name (compileGlobalSlots env))

constructorId :: CompileEnv -> Text -> Either Amd64Error Int
constructorId env name =
  maybe (Left (Amd64MissingConstructor name)) Right (Map.lookup name (compileConstructorIds env))

functionCodeLabel :: CompileEnv -> FunctionName -> Either Amd64Error Text
functionCodeLabel env name =
  maybe (Left (Amd64MissingFunction name)) Right (Map.lookup name (compileFunctionLabels env))

validatePrimitiveName :: Bool -> Text -> Either Amd64Error ()
validatePrimitiveName allowUnsupported name
  | name == "realWorld#" = Right ()
  | allowUnsupported = Right ()
  | otherwise = Left (Amd64UnsupportedPrimitive name)

functionLocalSlots :: GrinFunction -> Map GrinVar Int
functionLocalSlots function = snd (foldl' assignGroup (0, Map.empty) groups)
  where
    groups = grinFunctionParameters function : boundVarGroups (grinFunctionBody function)
    assignGroup = foldl' assignVar
    assignVar (next, slots) var =
      case Map.lookup var slots of
        Just _ -> (next, slots)
        Nothing -> (next + 1, Map.insert var next slots)

boundVarGroups :: GrinExpr -> [[GrinVar]]
boundVarGroups expression =
  case expression of
    GrinConstant _ -> []
    GrinBind vars valueExpression body -> vars : boundVarGroups valueExpression <> boundVarGroups body
    GrinStore _ -> []
    GrinStoreRec bindings body -> map (pure . fst) bindings <> boundVarGroups body
    GrinFetch _ _ -> []
    GrinUpdate _ _ -> []
    GrinUpdateBlackhole _ _ -> []
    GrinEval _ _ -> []
    GrinCpsEval {} -> []
    GrinCall {} -> []
    GrinPrimitiveCall {} -> []
    GrinApply {} -> []
    GrinCpsApply {} -> []
    GrinContinue {} -> []
    GrinHalt {} -> []
    GrinCase _ binder alternatives -> [binder] : concatMap altBoundVarGroups alternatives
    GrinThrow _ -> []
    GrinCatch {} -> []
    GrinForeignCallExpr {} -> []
  where
    altBoundVarGroups alternative = grinAltBinders alternative : boundVarGroups (grinAltRhs alternative)

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

exprRuntimeReps :: GrinExpr -> [RuntimeRep]
exprRuntimeReps expression =
  case expression of
    GrinConstant values -> concatMap valueRuntimeReps values
    GrinBind vars valueExpression body ->
      map grinVarRuntimeRep vars <> exprRuntimeReps valueExpression <> exprRuntimeReps body
    GrinStore node -> nodeRuntimeReps node
    GrinStoreRec bindings body ->
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
      <> [ "void aihc_snapshot_dump_result(uint64_t count, const AihcSlot *values) {",
           "  aihc_snapshot_dump(count, values, " <> pointerOrNull renderedResultReps "result_reps" <> ",",
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
  [ loadByteOffset "r11" "r15" 8,
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
