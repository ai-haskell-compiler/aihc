{-# LANGUAGE OverloadedStrings #-}

-- | Lower runtime-explicit GRIN to portable C11.
-- Generated entries form a trampoline: every entry stores its successor in
-- @aihc_next_transfer@ and returns, avoiding dependence on C tail-call support.
module Aihc.C.Codegen
  ( CError (..),
    compileModule,
    compileProgram,
    compileProgramWithDependencies,
    validatePrimitiveNames,
    validateProgramPrimitives,
  )
where

import Aihc.Grin.Gc (GcGrinProgram, gcGrinProgram, gcUpdateFunction)
import Aihc.Grin.Syntax
import Aihc.Native (LinkLayout (..), buildAddrLiteralPool, buildLinkLayout)
import Aihc.Tc.Types (Levity (..), RuntimeRep (..))
import Control.Monad (forM, replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, execStateT, get, modify')
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)

data CError
  = CMissingEntry !Text
  | CMissingGlobal !Text
  | CMissingFunction !FunctionName
  | CMissingConstructor !Text
  | CUnsupportedPrimitive !Text
  | CUnsupportedExpression !Text
  | CUnsupportedValue !Text
  | CUnsupportedRuntimeRep !RuntimeRep
  deriving (Eq, Show)

data CompileEnv = CompileEnv
  { compileConstructorIds :: !(Map Text Int),
    compileConstructorArities :: !(Map Text Int),
    compileGlobalSlots :: !(Map Text Int),
    compileFunctionLabels :: !(Map FunctionName Text),
    compileAddrLiteralLabels :: !(Map BS.ByteString Text),
    compileNodeInfoLabels :: !(Map RuntimeInfoKey Text),
    compileRuntimeInfos :: ![RuntimeInfo],
    compileAllowUnsupportedPrimitives :: !Bool
  }

data CompilationUnit
  = ExecutableUnit
  | LibraryUnit

data RuntimeInfo = RuntimeInfo
  { runtimeInfoLabel :: !Text,
    runtimeInfoIdentity :: !(Maybe Int),
    runtimeInfoEntry :: !(Maybe Text),
    runtimeInfoFields :: ![RuntimeRep],
    runtimeInfoRemainingArity :: !Int,
    runtimeInfoNext :: !(Maybe Text)
  }

data RuntimeInfoKey
  = ConstructorRuntimeInfo !Text !Int
  | ClosureRuntimeInfo !FunctionName ![RuntimeRep] ![[RuntimeRep]]
  | ThunkRuntimeInfo !FunctionName ![RuntimeRep]
  deriving (Eq, Ord, Show)

data FunctionState = FunctionState
  { functionNextLabel :: !Int,
    functionNextSlot :: !Int,
    functionBlocksRev :: ![(Text, [Text])]
  }

data CompiledFunction = CompiledFunction
  { compiledFunctionSlots :: !Int,
    compiledFunctionLines :: ![Text]
  }

type FunctionM = StateT FunctionState (Either CError)

data ValueEnv = ValueEnv
  { valueCompileEnv :: !CompileEnv,
    valueLocalSlots :: !(Map GrinVar Int)
  }

compileProgram :: Text -> GcGrinProgram -> Either CError Text
compileProgram entryName gcProgram =
  compileProgramWithDependencies (buildLinkLayout [program]) [] entryName gcProgram
  where
    program = gcGrinProgram gcProgram

compileProgramWithDependencies :: LinkLayout -> [Text] -> Text -> GcGrinProgram -> Either CError Text
compileProgramWithDependencies layout dependencyInitializers entryName gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  validateProgramPrimitives program
  rootSlot <- maybe (Left (CMissingEntry entryName)) Right (Map.lookup entryName globalSlots)
  updateLabel <- functionCodeLabel env (gcUpdateFunction gcProgram)
  functionDefinitions <- mapM (compileFunction env) (grinFunctions program)
  initializer <- compileInitializers env program
  constructorInitializer <- compileConstructorInitializers env
  let specialInfos =
        [ specialInfo "aihc_final_info" "aihc_final_continuation" [] 1 (Just "aihc_final_applied_info"),
          specialInfo "aihc_final_applied_info" "aihc_final_continuation" [BoxedRep Lifted] 0 Nothing,
          specialInfo "aihc_top_info" "aihc_top_continuation" [BoxedRep Lifted] 1 (Just "aihc_top_applied_info"),
          specialInfo "aihc_top_applied_info" "aihc_top_continuation" [BoxedRep Lifted, BoxedRep Lifted] 0 Nothing,
          specialInfo "aihc_update_info" updateLabel [BoxedRep Lifted, BoxedRep Lifted] 1 (Just "aihc_update_applied_info"),
          specialInfo "aihc_update_applied_info" updateLabel [BoxedRep Lifted, BoxedRep Lifted, BoxedRep Lifted] 0 Nothing,
          specialInfo "aihc_thread_done_info" "aihc_thread_done_continuation" [] 1 (Just "aihc_thread_done_applied_info"),
          specialInfo "aihc_thread_done_applied_info" "aihc_thread_done_continuation" [BoxedRep Lifted] 0 Nothing
        ]
      source =
        [ "#include \"aihc_runtime.h\"",
          "#include <stdint.h>",
          "#include <stddef.h>",
          "",
          "AihcMachine *aihc_machine;",
          "AihcPortableTransfer aihc_next_transfer;",
          "static AihcSlot aihc_arguments[" <> tshow (portableArgumentCapacity layout) <> "];",
          ""
        ]
          <> renderForeignDeclarations program
          <> renderFunctionDeclarations env program
          <> ["extern void " <> dependencyInitializer <> "(void);" | dependencyInitializer <- dependencyInitializers]
          <> ["" | not (null dependencyInitializers)]
          <> renderSpecialDeclarations
          <> renderAddrLiterals env
          <> renderRuntimeInfos (compileRuntimeInfos env <> specialInfos)
          <> concatMap compiledFunctionLines functionDefinitions
          <> renderSpecialFunctions
          <> [ "int main(void) {",
               "  AihcValue *final_continuation;",
               "  AihcValue *top_continuation;",
               "  AihcValue *update_continuation;",
               "  AihcValue *thread_done_continuation;",
               "  AihcPortableTransfer transfer;",
               "  aihc_machine = aihc_machine_new(" <> tshow (length (linkGlobalNames layout)) <> ");"
             ]
          <> indent (reserveLocalsLines functionDefinitions)
          <> indent constructorInitializer
          <> indent [dependencyInitializer <> "();" | dependencyInitializer <- dependencyInitializers]
          <> indent initializer
          <> [ "  aihc_ensure_heap(aihc_machine, 7, 0, NULL);",
               "  final_continuation = aihc_make_node_unchecked(aihc_machine, AIHC_TAG_CLOSURE, &aihc_final_info);",
               "  top_continuation = aihc_make_node_unchecked(aihc_machine, AIHC_TAG_CLOSURE, &aihc_top_info);",
               "  aihc_set_field(top_continuation, 0, (AihcSlot)(uintptr_t)final_continuation);",
               "  update_continuation = aihc_make_node_unchecked(aihc_machine, AIHC_TAG_CLOSURE, &aihc_update_info);",
               "  aihc_set_field(update_continuation, 0, aihc_machine->globals[" <> tshow rootSlot <> "]);",
               "  aihc_set_field(update_continuation, 1, (AihcSlot)(uintptr_t)top_continuation);",
               "  thread_done_continuation = aihc_make_node_unchecked(aihc_machine, AIHC_TAG_CLOSURE, &aihc_thread_done_info);",
               "  aihc_next_transfer = aihc_portable_start(aihc_machine, aihc_arguments, (AihcValue *)(uintptr_t)aihc_machine->globals[" <> tshow rootSlot <> "], top_continuation, update_continuation, thread_done_continuation, aihc_exit);",
               "  while (aihc_next_transfer.entry != NULL) {",
               "    transfer = aihc_next_transfer;",
               "    aihc_next_transfer = (AihcPortableTransfer){0};",
               "    transfer.entry(transfer.arguments);",
               "  }",
               "  return 0;",
               "}"
             ]
  pure (T.unlines source)
  where
    program = gcGrinProgram gcProgram
    env = compileEnvironment ExecutableUnit layout program
    globalSlots = compileGlobalSlots env
    specialInfo label entry = RuntimeInfo label Nothing (Just entry)

compileModule :: LinkLayout -> Text -> GcGrinProgram -> Either CError Text
compileModule layout initializerSymbol gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  functionDefinitions <- mapM (compileFunction env) (grinFunctions program)
  initializer <- compileInitializers env program
  let source =
        [ "#include \"aihc_runtime.h\"",
          "#include <stdint.h>",
          "#include <stddef.h>",
          "",
          "extern AihcMachine *aihc_machine;",
          "extern AihcPortableTransfer aihc_next_transfer;",
          "static AihcSlot aihc_arguments[" <> tshow (portableArgumentCapacity layout) <> "];",
          ""
        ]
          <> renderForeignDeclarations program
          <> renderFunctionDeclarations env program
          <> renderAddrLiterals env
          <> renderRuntimeInfos (compileRuntimeInfos env)
          <> concatMap compiledFunctionLines functionDefinitions
          <> [ "void " <> initializerSymbol <> "(void) {"
             ]
          <> indent (reserveLocalsLines functionDefinitions)
          <> indent initializer
          <> ["}"]
  pure (T.unlines source)
  where
    program = gcGrinProgram gcProgram
    env = compileEnvironment LibraryUnit layout program

validateProgramPrimitives :: GrinProgram -> Either CError ()
validateProgramPrimitives = validatePrimitiveNames . map (grinVarName . fst) . grinPrimitives

validatePrimitiveNames :: [Text] -> Either CError ()
validatePrimitiveNames = mapM_ $ \name ->
  if name `elem` ["+#", "awaitIO#", "fork#", "realWorld#", "yield#"]
    then Right ()
    else Left (CUnsupportedPrimitive name)

compileEnvironment :: CompilationUnit -> LinkLayout -> GrinProgram -> CompileEnv
compileEnvironment unitKind layout program =
  CompileEnv
    { compileConstructorIds = Map.fromList (zip (map fst constructors) [1 ..]),
      compileConstructorArities = Map.fromList constructors,
      compileGlobalSlots = Map.fromList (zip (linkGlobalNames layout) [0 ..]),
      compileFunctionLabels = functionLabels,
      compileAddrLiteralLabels = Map.fromList [(bytes, cLabel label) | (bytes, label) <- buildAddrLiteralPool program],
      compileNodeInfoLabels = Map.fromList [(key, label) | (key, label, _) <- constructorEntries <> functionEntries],
      compileRuntimeInfos = map third (constructorEntries <> functionEntries),
      compileAllowUnsupportedPrimitives =
        case unitKind of
          ExecutableUnit -> False
          LibraryUnit -> True
    }
  where
    constructors = [(name, length layouts) | (name, layouts) <- linkConstructors layout]
    constructorIds = zip (map fst constructors) [1 ..]
    functionLabels =
      Map.fromList
        ( [ (grinCodeFunctionName info, linkedFunctionLabel (grinCodeSourceName info))
          | info <- grinExternalFunctions program
          ]
            <> [ (grinFunctionName function, localFunctionLabel index function)
               | (index, function) <- zip [0 :: Int ..] (grinFunctions program)
               ]
        )
    constructorEntries =
      [ (key, label, RuntimeInfo label (Just identifier) Nothing fields remaining next)
      | ((name, layouts), (_, identifier)) <- zip (linkConstructors layout) constructorIds,
        let arity = length layouts,
        remaining <- [arity, arity - 1 .. 0],
        let key = ConstructorRuntimeInfo name remaining,
        key `Set.member` requiredConstructorInfos,
        let label = "aihc_constructor_info_" <> tshow identifier <> "_remaining_" <> tshow remaining
            fields = concat (take (arity - remaining) layouts)
            next = if remaining == 0 then Nothing else Just ("aihc_constructor_info_" <> tshow identifier <> "_remaining_" <> tshow (remaining - 1))
      ]
    requiredConstructorInfos =
      Set.fromList
        ( executableConstructorInfos
            <> concatMap requiredNodeConstructorInfos (programNodes program)
        )
    executableConstructorInfos =
      case unitKind of
        ExecutableUnit -> [ConstructorRuntimeInfo name 0 | (name, arity) <- constructors, arity == 0]
        LibraryUnit -> []
    infoKeys =
      [ key
      | key <- Set.toAscList (Set.fromList (concatMap runtimeInfoKeyStages (programNodes program))),
        Just functionName <- [runtimeInfoFunctionName key],
        functionName `Map.member` functionLabels
      ]
    functionEntries =
      [ ( key,
          label,
          RuntimeInfo label Nothing (runtimeInfoFunctionName key >>= (`Map.lookup` functionLabels)) (runtimeInfoKeyFields key) (runtimeInfoKeyRemainingArity key) (runtimeInfoKeyNext key >>= (`Map.lookup` infoLabels))
        )
      | (index, key) <- zip [0 :: Int ..] infoKeys,
        let label = "aihc_function_info_" <> tshow index
      ]
    infoLabels = Map.fromList [(key, "aihc_function_info_" <> tshow index) | (index, key) <- zip [0 :: Int ..] infoKeys]
    third (_, _, value) = value

portableArgumentCapacity :: LinkLayout -> Int
portableArgumentCapacity layout = max 3 (linkMaximumArgumentSlots layout)

requiredNodeConstructorInfos :: GrinNode -> [RuntimeInfoKey]
requiredNodeConstructorInfos node =
  case grinNodeTag node of
    GrinConstructor name remaining -> [ConstructorRuntimeInfo name stage | stage <- [remaining, remaining - 1 .. 0]]
    GrinClosure {} -> []
    GrinThunk {} -> []

compileConstructorInitializers :: CompileEnv -> Either CError [Text]
compileConstructorInitializers env = fmap concat . forM nullary $ \(name, identifier) -> do
  slot <- globalSlot env name
  info <- lookupRuntimeInfoLabel env (ConstructorRuntimeInfo name 0)
  pure ["aihc_machine->globals[" <> tshow slot <> "] = (AihcSlot)(uintptr_t)aihc_make_node(aihc_machine, AIHC_TAG_NODE, &" <> info <> "); /* constructor " <> tshow identifier <> " */"]
  where
    nullary =
      [ (name, identifier)
      | (name, identifier) <- Map.toAscList (compileConstructorIds env),
        Map.lookup name (compileConstructorArities env) == Just 0
      ]

compileInitializers :: CompileEnv -> GrinProgram -> Either CError [Text]
compileInitializers env program = do
  cafAllocations <- fmap concat . forM (grinCafs program) $ \(var, node) -> do
    slot <- globalSlot env (grinVarName var)
    (tag, info) <- nodeHeader env node
    pure ["aihc_machine->globals[" <> tshow slot <> "] = (AihcSlot)(uintptr_t)aihc_make_node(aihc_machine, " <> tag <> ", &" <> info <> ");"]
  whnfs <- fmap concat . forM (grinWhnfGlobals program) $ \(var, node) -> do
    slot <- globalSlot env (grinVarName var)
    renderGlobalNode env slot node
  cafFields <- fmap concat . forM (grinCafs program) $ \(var, node) -> do
    slot <- globalSlot env (grinVarName var)
    initializeGlobalFields env ("(AihcValue *)(uintptr_t)aihc_machine->globals[" <> tshow slot <> "]") node
  pure (cafAllocations <> whnfs <> cafFields)

renderGlobalNode :: CompileEnv -> Int -> GrinNode -> Either CError [Text]
renderGlobalNode env slot node = do
  (tag, info) <- nodeHeader env node
  fields <- initializeGlobalFields env ("(AihcValue *)(uintptr_t)aihc_machine->globals[" <> tshow slot <> "]") node
  pure (["aihc_machine->globals[" <> tshow slot <> "] = (AihcSlot)(uintptr_t)aihc_make_node(aihc_machine, " <> tag <> ", &" <> info <> ");"] <> fields)

initializeGlobalFields :: CompileEnv -> Text -> GrinNode -> Either CError [Text]
initializeGlobalFields env object node =
  forM (zip [0 :: Int ..] (grinNodeFields node)) $ \(index, value) -> do
    expression <- materializeGlobalValue env value
    pure ("aihc_set_field(" <> object <> ", " <> tshow index <> ", " <> expression <> ");")

compileFunction :: CompileEnv -> GrinFunction -> Either CError CompiledFunction
compileFunction env function = do
  label <- functionCodeLabel env (grinFunctionName function)
  let slots = functionLocalSlots function
      initial = FunctionState 0 (Map.size slots) []
      valueEnv = ValueEnv env slots
  final <- execStateT (compileExpr valueEnv [] (label <> "_body") (grinFunctionBody function)) initial
  let slotCount = max 1 (functionNextSlot final)
      parameters =
        [ "  locals[" <> tshow slot <> "] = arguments[" <> tshow index <> "];"
        | (index, var) <- zip [0 :: Int ..] (grinFunctionParameters function),
          Just slot <- [Map.lookup var slots]
        ]
      blocks = concatMap renderBlock (reverse (functionBlocksRev final))
  pure
    CompiledFunction
      { compiledFunctionSlots = slotCount,
        compiledFunctionLines =
          [ functionStorage function <> "void " <> label <> "(AihcSlot *arguments) {",
            "  AihcSlot *locals = aihc_alloc_locals(aihc_machine, " <> tshow slotCount <> ");",
            "  AihcSlot aihc_scratch = 0;",
            "  (void)arguments;",
            "  (void)aihc_scratch;"
          ]
            <> parameters
            <> ["  goto " <> label <> "_body;"]
            <> indent blocks
            <> ["}", ""]
      }

reserveLocalsLines :: [CompiledFunction] -> [Text]
reserveLocalsLines functions =
  ["aihc_alloc_locals(aihc_machine, " <> tshow maximumSlots <> ");"]
  where
    maximumSlots = maximum (1 : map compiledFunctionSlots functions)

compileExpr :: ValueEnv -> [Text] -> Text -> GrinExpr -> FunctionM ()
compileExpr env prefix label expression =
  case expression of
    GrinBind vars valueExpression body -> do
      direct <- compileDirectBinding env vars valueExpression
      compileExpr env (prefix <> direct) label body
    GrinStoreRec bindings body -> compileStoreRec False bindings body
    GrinStoreRecUnchecked bindings body -> compileStoreRec True bindings body
    GrinCpsEval runtimeRep value continuation updateContinuation -> do
      values <- materializeIntoFresh env [value, continuation, updateContinuation]
      case snd values of
        [valueSlot, continuationSlot, updateSlot] ->
          terminal label (prefix <> fst values <> [setNext ("aihc_portable_eval_cps(aihc_machine, aihc_arguments, " <> valuePointer (localRef valueSlot) <> ", " <> boolText (isLiftedRuntimeRep runtimeRep) <> ", " <> valuePointer (localRef continuationSlot) <> ", " <> valuePointer (localRef updateSlot) <> ")")])
        _ -> unsupported "internal CPS evaluation slot arity"
    GrinCall _ functionName arguments -> do
      target <- liftEither (functionCodeLabel (valueCompileEnv env) functionName)
      values <- materializeIntoFresh env arguments
      terminal label (prefix <> fst values <> setArguments (snd values) <> ["aihc_next_transfer = (AihcPortableTransfer){" <> target <> ", aihc_arguments};", "return;"])
    GrinCpsPrimitiveCall _ name arguments continuation -> compileCpsPrimitive env prefix label name arguments continuation
    GrinCpsApply _ function arguments continuation -> do
      values <- materializeIntoFresh env (function : continuation : arguments)
      let slots = snd values
          argumentSlots = drop 2 slots
      case slots of
        functionSlot : continuationSlot : _ ->
          terminal label (prefix <> fst values <> [setNext ("aihc_portable_apply_cps(aihc_machine, aihc_arguments, " <> valuePointer (localRef functionSlot) <> ", " <> tshow (length arguments) <> ", " <> slotPointer argumentSlots <> ", " <> valuePointer (localRef continuationSlot) <> ")")])
        _ -> unsupported "internal CPS application slot arity"
    GrinContinue continuation values -> do
      stored <- materializeIntoFresh env (continuation : values)
      case snd stored of
        continuationSlot : valueSlots ->
          terminal label (prefix <> fst stored <> [setNext ("aihc_portable_continue_values(aihc_machine, aihc_arguments, " <> valuePointer (localRef continuationSlot) <> ", " <> tshow (length values) <> ", " <> slotPointer valueSlots <> ")")])
        [] -> unsupported "internal continuation slot arity"
    GrinHalt _ -> terminal label (prefix <> [setNext "aihc_halt(aihc_machine)"])
    GrinCase scrutinee binder alternatives -> compileCase env prefix label scrutinee binder alternatives
    GrinConstant {} -> unsupported "direct-style constant return after CPS"
    GrinStore {} -> unsupported "direct-style store return after CPS"
    GrinEnsureHeap {} -> unsupported "unbound heap reservation"
    GrinStoreUnchecked {} -> unsupported "unbound unchecked store"
    GrinFetch {} -> unsupported "direct-style fetch return after CPS"
    GrinUpdate {} -> unsupported "direct-style update return after CPS"
    GrinUpdateBlackhole {} -> unsupported "unbound blackhole update"
    GrinEval {} -> unsupported "direct-style eval after CPS"
    GrinPrimitiveCall {} -> unsupported "unbound primitive call after CPS"
    GrinApply {} -> unsupported "direct-style apply after CPS"
    GrinThrow {} -> unsupported "throw"
    GrinCatch {} -> unsupported "catch"
    GrinForeignCallExpr {} -> unsupported "unbound foreign call after CPS"
  where
    unsupported = lift . Left . CUnsupportedExpression
    terminal blockLabel lines' = addBlock blockLabel (lines' <> ["return;"])
    compileStoreRec unchecked bindings body = do
      allocations <- fmap concat . forM bindings $ \(var, node) -> do
        destination <- localSlot env var
        (tag, info) <- liftEither (nodeHeader (valueCompileEnv env) node)
        pure [localRef destination <> " = (AihcSlot)(uintptr_t)" <> allocation unchecked tag info <> ";"]
      fields <- fmap concat . forM bindings $ \(var, node) -> do
        destination <- localSlot env var
        initializeLocalFields env (valuePointer (localRef destination)) node
      compileExpr env (prefix <> allocations <> fields) label body

compileCpsPrimitive :: ValueEnv -> [Text] -> Text -> Text -> [GrinValue] -> GrinValue -> FunctionM ()
compileCpsPrimitive env prefix label name arguments continuation =
  case (name, arguments) of
    ("awaitIO#", [request]) -> transfer "aihc_portable_await_io_cps" [request, continuation]
    ("fork#", [action]) -> transfer "aihc_portable_fork_cps" [action, continuation]
    ("yield#", []) -> transfer "aihc_portable_yield_cps" [continuation]
    _
      | compileAllowUnsupportedPrimitives (valueCompileEnv env) ->
          addBlock label (prefix <> ["aihc_unsupported_primitive();", "return;"])
    _ -> lift (Left (CUnsupportedExpression ("CPS primitive call " <> name)))
  where
    transfer function values = do
      stored <- materializeIntoFresh env values
      let arguments' = T.intercalate ", " (map (valuePointer . localRef) (snd stored))
      addBlock label (prefix <> fst stored <> [setNext (function <> "(aihc_machine, aihc_arguments, " <> arguments' <> ")"), "return;"])

compileDirectBinding :: ValueEnv -> [GrinVar] -> GrinExpr -> FunctionM [Text]
compileDirectBinding env vars expression =
  case expression of
    GrinConstant values
      | length vars == length values -> fmap concat . forM (zip vars values) $ \(var, value) -> do
          destination <- localSlot env var
          source <- liftEither (materializeValue env value)
          pure [localRef destination <> " = " <> source <> ";"]
    GrinStore node -> materializeNode False node >>= storeOne
    GrinEnsureHeap requiredWords roots
      | length vars == length roots -> do
          rootLines <- fmap concat . forM (zip vars roots) $ \(var, value) -> do
            destination <- localSlot env var
            source <- liftEither (materializeValue env value)
            pure [localRef destination <> " = " <> source <> ";"]
          rootSlots <- mapM (localSlot env) vars
          pure (rootLines <> ["aihc_ensure_heap(aihc_machine, " <> tshow requiredWords <> ", " <> tshow (length roots) <> ", " <> slotPointer rootSlots <> ");"])
    GrinStoreUnchecked node -> materializeNode True node >>= storeOne
    GrinFetch _ pointer -> liftEither (materializeValue env pointer) >>= storeOne . pure
    GrinUpdate pointer value -> update "aihc_update" False pointer value
    GrinUpdateBlackhole pointer value -> update "aihc_update_blackhole" True pointer value
    GrinPrimitiveCall IntRep "+#" [left, right] -> binaryIntPrimitive "+" left right
    GrinPrimitiveCall runtimeRep "realWorld#" []
      | null vars && null (runtimeRepComponents runtimeRep) -> pure []
    GrinPrimitiveCall {}
      | compileAllowUnsupportedPrimitives (valueCompileEnv env) -> do
          zeroResults <- forM vars $ \var -> do
            destination <- localSlot env var
            pure (localRef destination <> " = 0;")
          pure (["aihc_unsupported_primitive();"] <> zeroResults)
    GrinPrimitiveCall _ name _ -> lift (Left (CUnsupportedExpression ("primitive call " <> name)))
    GrinForeignCallExpr foreignCall arguments -> compileForeignCall env foreignCall arguments >>= storeOne
    _ -> lift (Left (CUnsupportedExpression "non-direct expression remained in a CPS bind"))
  where
    storeOne lines' =
      case vars of
        [var] -> do
          destination <- localSlot env var
          pure (lines' <> [localRef destination <> " = aihc_scratch;"])
        _ -> lift (Left (CUnsupportedExpression "direct expression result arity"))
    materializeNode unchecked node = do
      scratch <- freshSlot
      (tag, info) <- liftEither (nodeHeader (valueCompileEnv env) node)
      fields <- initializeLocalFields env (valuePointer (localRef scratch)) node
      pure ([localRef scratch <> " = (AihcSlot)(uintptr_t)" <> allocation unchecked tag info <> ";"] <> fields <> ["aihc_scratch = " <> localRef scratch <> ";"])
    update function passMachine pointer value = do
      stored <- materializeIntoFresh env [pointer, value]
      case snd stored of
        [pointerSlot, valueSlot] -> do
          let callArguments = (if passMachine then "aihc_machine, " else "") <> valuePointer (localRef pointerSlot) <> ", " <> valuePointer (localRef valueSlot)
          result <- storeOne [function <> "(" <> callArguments <> ");", "aihc_scratch = " <> localRef valueSlot <> ";"]
          pure (fst stored <> result)
        _ -> lift (Left (CUnsupportedExpression "internal update slot arity"))
    binaryIntPrimitive operator left right = do
      stored <- materializeIntoFresh env [left, right]
      case snd stored of
        [leftSlot, rightSlot] ->
          storeOne
            ( fst stored
                <> [ "aihc_scratch = (AihcSlot)((uint64_t)"
                       <> localRef leftSlot
                       <> " "
                       <> operator
                       <> " (uint64_t)"
                       <> localRef rightSlot
                       <> ");"
                   ]
            )
        _ -> lift (Left (CUnsupportedExpression "internal binary Int# primitive arity"))

compileForeignCall :: ValueEnv -> GrinForeignCall -> [GrinValue] -> FunctionM [Text]
compileForeignCall env foreignCall arguments = do
  let signature = grinForeignCallSignature foreignCall
      argumentTypes = grinForeignArgumentTypes signature
  if length arguments /= length (grinForeignOperandReps signature)
    then lift (Left (CUnsupportedExpression "foreign call arity mismatch"))
    else do
      stored <- materializeIntoFresh env arguments
      let abiSlots = take (length argumentTypes) (snd stored)
          callArguments = T.intercalate ", " (zipWith foreignArgument argumentTypes abiSlots)
          call = grinForeignCallSymbol foreignCall <> "(" <> callArguments <> ")"
      pure (fst stored <> ["aihc_scratch = " <> foreignResult (grinForeignResultType signature) call <> ";"])

compileCase :: ValueEnv -> [Text] -> Text -> GrinValue -> GrinVar -> [GrinAlt] -> FunctionM ()
compileCase env prefix label scrutinee binder alternatives = do
  result <- freshSlot
  dispatch <- freshLabel label "case_dispatch"
  value <- liftEither (materializeValue env scrutinee)
  addBlock label (prefix <> [localRef result <> " = " <> value <> ";", "goto " <> dispatch <> ";"])
  binderSlot <- localSlot env binder
  targets <- forM alternatives $ \alternative -> do
    target <- freshLabel label "case_alt"
    alternativeLines <- alternativePrefix env result alternative
    compileExpr env alternativeLines target (grinAltRhs alternative)
    pure (alternative, target)
  checks <- caseChecks env result (isPointerRuntimeRep (grinValueRuntimeRep scrutinee)) targets
  addBlock dispatch ([localRef binderSlot <> " = " <> localRef result <> ";"] <> checks)

alternativePrefix :: ValueEnv -> Int -> GrinAlt -> FunctionM [Text]
alternativePrefix env result alternative =
  case grinAltCon alternative of
    GrinDataAlt _ -> fmap concat . forM (zip [0 :: Int ..] (grinAltBinders alternative)) $ \(index, binder) -> do
      destination <- localSlot env binder
      pure [localRef destination <> " = aihc_value_fields(" <> valuePointer (localRef result) <> ")[" <> tshow index <> "];"]
    GrinLitAlt _ -> pure []
    GrinDefaultAlt -> fmap concat . forM (grinAltBinders alternative) $ \binder -> do
      destination <- localSlot env binder
      pure [localRef destination <> " = " <> localRef result <> ";"]

caseChecks :: ValueEnv -> Int -> Bool -> [(GrinAlt, Text)] -> FunctionM [Text]
caseChecks env result pointer targets = do
  checks <- fmap concat . forM nonDefault $ \(alternative, target) ->
    case grinAltCon alternative of
      GrinDataAlt name
        | pointer -> do
            identifier <- liftEither (constructorId (valueCompileEnv env) name)
            pure ["if (aihc_value_info(" <> valuePointer (localRef result) <> ") == " <> tshow identifier <> ") goto " <> target <> ";"]
        | otherwise -> lift (Left (CUnsupportedExpression "constructor case on an unboxed value"))
      GrinLitAlt literal
        | pointer -> lift (Left (CUnsupportedExpression "literal case on a lifted value"))
        | otherwise ->
            case normalizedLiteralInteger literal of
              Just integer -> pure ["if (" <> localRef result <> " == " <> renderSlotInteger integer <> ") goto " <> target <> ";"]
              Nothing -> lift (Left (CUnsupportedValue "string case alternative"))
      GrinDefaultAlt -> pure []
  pure (checks <> maybe ["aihc_no_match();", "return;"] (\target -> ["goto " <> target <> ";"]) defaultTarget)
  where
    nonDefault = [(alternative, target) | (alternative, target) <- targets, grinAltCon alternative /= GrinDefaultAlt]
    defaultTarget = case [target | (alternative, target) <- targets, grinAltCon alternative == GrinDefaultAlt] of
      target : _ -> Just target
      [] -> Nothing

materializeIntoFresh :: ValueEnv -> [GrinValue] -> FunctionM ([Text], [Int])
materializeIntoFresh env values = do
  slots <- replicateM (length values) freshSlot
  lines' <- fmap concat . forM (zip values slots) $ \(value, destination) -> do
    source <- liftEither (materializeValue env value)
    pure [localRef destination <> " = " <> source <> ";"]
  pure (lines', slots)

initializeLocalFields :: ValueEnv -> Text -> GrinNode -> FunctionM [Text]
initializeLocalFields env object node =
  forM (zip [0 :: Int ..] (grinNodeFields node)) $ \(index, field) -> do
    value <- liftEither (materializeValue env field)
    pure ("aihc_set_field(" <> object <> ", " <> tshow index <> ", " <> value <> ");")

materializeValue :: ValueEnv -> GrinValue -> Either CError Text
materializeValue env value =
  case value of
    GrinVarValue var ->
      case Map.lookup var (valueLocalSlots env) of
        Just index -> Right (localRef index)
        Nothing -> do
          index <- globalSlot (valueCompileEnv env) (grinVarName var)
          pure ("aihc_machine->globals[" <> tshow index <> "]")
    GrinLitValue literal -> materializeLiteral (valueCompileEnv env) literal

materializeGlobalValue :: CompileEnv -> GrinValue -> Either CError Text
materializeGlobalValue env value =
  case value of
    GrinVarValue var -> do
      index <- globalSlot env (grinVarName var)
      pure ("aihc_machine->globals[" <> tshow index <> "]")
    GrinLitValue literal -> materializeLiteral env literal

materializeLiteral :: CompileEnv -> GrinLiteral -> Either CError Text
materializeLiteral env literal =
  case literal of
    GrinLitAddr bytes ->
      maybe (Left (CUnsupportedValue "unregistered Addr# literal")) (Right . ("(AihcSlot)(uintptr_t)" <>)) (Map.lookup bytes (compileAddrLiteralLabels env))
    _ -> maybe (Left (CUnsupportedValue "string literal")) (Right . renderSlotInteger) (normalizedLiteralInteger literal)

nodeHeader :: CompileEnv -> GrinNode -> Either CError (Text, Text)
nodeHeader env node = do
  info <- lookupRuntimeInfoLabel env key
  pure (tag, info)
  where
    fields = map grinValueRuntimeRep (grinNodeFields node)
    (tag, key) =
      case grinNodeTag node of
        GrinConstructor name remaining -> (if remaining == 0 then "AIHC_TAG_NODE" else "AIHC_TAG_PARTIAL_CONSTRUCTOR", ConstructorRuntimeInfo name remaining)
        GrinClosure functionName layouts -> ("AIHC_TAG_CLOSURE", ClosureRuntimeInfo functionName fields layouts)
        GrinThunk functionName -> ("AIHC_TAG_THUNK", ThunkRuntimeInfo functionName fields)

renderRuntimeInfos :: [RuntimeInfo] -> [Text]
renderRuntimeInfos infos = concatMap bitmap infos <> [""] <> map declaration infos <> [""] <> map definition infos <> [""]
  where
    bitmap info
      | null (runtimeInfoFields info) = []
      | otherwise = ["static const uint8_t " <> runtimeInfoLabel info <> "_bitmap[] = {" <> T.intercalate ", " [if isPointerRuntimeRep field then "1" else "0" | field <- runtimeInfoFields info] <> "};"]
    declaration info = "static const AihcInfo " <> runtimeInfoLabel info <> ";"
    definition info =
      "static const AihcInfo "
        <> runtimeInfoLabel info
        <> " = {"
        <> maybe "0" tshow (runtimeInfoIdentity info)
        <> ", "
        <> maybe "NULL" ("&" <>) (runtimeInfoEntry info)
        <> ", "
        <> tshow (length (runtimeInfoFields info))
        <> ", "
        <> tshow (runtimeInfoRemainingArity info)
        <> ", "
        <> (if null (runtimeInfoFields info) then "NULL" else runtimeInfoLabel info <> "_bitmap")
        <> ", "
        <> maybe "NULL" ("&" <>) (runtimeInfoNext info)
        <> ", NULL"
        <> "};"

renderForeignDeclarations :: GrinProgram -> [Text]
renderForeignDeclarations program =
  [ "extern " <> foreignType (grinForeignResultType signature) <> " " <> grinForeignCallSymbol foreignCall <> "(" <> arguments signature <> ");"
  | foreignCall <- grinForeignCalls program,
    let signature = grinForeignCallSignature foreignCall
  ]
    <> [""]
  where
    arguments signature =
      case grinForeignArgumentTypes signature of
        [] -> "void"
        types -> T.intercalate ", " (map foreignType types)

renderFunctionDeclarations :: CompileEnv -> GrinProgram -> [Text]
renderFunctionDeclarations env program =
  [ functionStorage function <> "void " <> label <> "(AihcSlot *arguments);"
  | function <- grinFunctions program,
    Just label <- [Map.lookup (grinFunctionName function) (compileFunctionLabels env)]
  ]
    <> [ "extern void " <> label <> "(AihcSlot *arguments);"
       | info <- grinExternalFunctions program,
         Just label <- [Map.lookup (grinCodeFunctionName info) (compileFunctionLabels env)]
       ]
    <> [""]

functionStorage :: GrinFunction -> Text
functionStorage function =
  case grinFunctionLinkName function of
    Nothing -> "static "
    Just _ -> ""

renderSpecialDeclarations :: [Text]
renderSpecialDeclarations =
  [ "static void aihc_top_continuation(AihcSlot *arguments);",
    "static void aihc_thread_done_continuation(AihcSlot *arguments);",
    "static void aihc_final_continuation(AihcSlot *arguments);",
    "static void aihc_exit(AihcSlot *arguments);",
    ""
  ]

renderSpecialFunctions :: [Text]
renderSpecialFunctions =
  [ "static void aihc_top_continuation(AihcSlot *arguments) {",
    "  aihc_next_transfer = aihc_portable_apply_cps(aihc_machine, aihc_arguments, (AihcValue *)(uintptr_t)arguments[1], 0, NULL, (AihcValue *)(uintptr_t)arguments[0]);",
    "}",
    "",
    "static void aihc_thread_done_continuation(AihcSlot *arguments) {",
    "  (void)arguments;",
    "  aihc_next_transfer = aihc_portable_thread_done(aihc_machine, aihc_arguments);",
    "}",
    "",
    "static void aihc_final_continuation(AihcSlot *arguments) {",
    "  (void)arguments;",
    "  aihc_next_transfer = (AihcPortableTransfer){aihc_halt(aihc_machine), NULL};",
    "}",
    "",
    "static void aihc_exit(AihcSlot *arguments) {",
    "  (void)arguments;",
    "  aihc_next_transfer = (AihcPortableTransfer){0};",
    "}",
    ""
  ]

renderAddrLiterals :: CompileEnv -> [Text]
renderAddrLiterals env =
  [ "static const unsigned char " <> label <> "[] = {" <> T.intercalate ", " (map tshow (BS.unpack bytes <> [0])) <> "};"
  | (bytes, label) <- Map.toAscList (compileAddrLiteralLabels env)
  ]
    <> [""]

foreignType :: GrinForeignType -> Text
foreignType foreignType' =
  case foreignType' of
    GrinForeignInt32 -> "int32_t"
    GrinForeignWord64 -> "uint64_t"
    GrinForeignAddr -> "void *"

foreignArgument :: GrinForeignType -> Int -> Text
foreignArgument foreignType' index =
  case foreignType' of
    GrinForeignInt32 -> "(int32_t)" <> localRef index
    GrinForeignWord64 -> "(uint64_t)" <> localRef index
    GrinForeignAddr -> "(void *)(uintptr_t)" <> localRef index

foreignResult :: GrinForeignType -> Text -> Text
foreignResult foreignType' call =
  case foreignType' of
    GrinForeignInt32 -> "(AihcSlot)(int64_t)(int32_t)" <> call
    GrinForeignWord64 -> "(AihcSlot)" <> call
    GrinForeignAddr -> "(AihcSlot)(uintptr_t)" <> call

allocation :: Bool -> Text -> Text -> Text
allocation unchecked tag info =
  (if unchecked then "aihc_make_node_unchecked" else "aihc_make_node") <> "(aihc_machine, " <> tag <> ", &" <> info <> ")"

setArguments :: [Int] -> [Text]
setArguments slots =
  ["aihc_arguments[" <> tshow index <> "] = " <> localRef slot <> ";" | (index, slot) <- zip [0 :: Int ..] slots]

setNext :: Text -> Text
setNext expression = "aihc_next_transfer = " <> expression <> ";"

slotPointer :: [Int] -> Text
slotPointer slots = maybe "NULL" (\index -> "&locals[" <> tshow index <> "]") (safeHead slots)

localRef :: Int -> Text
localRef index = "locals[" <> tshow index <> "]"

valuePointer :: Text -> Text
valuePointer expression = "(AihcValue *)(uintptr_t)" <> expression

boolText :: Bool -> Text
boolText True = "1"
boolText False = "0"

localSlot :: ValueEnv -> GrinVar -> FunctionM Int
localSlot env var = maybe (lift (Left (CUnsupportedExpression ("missing local slot for " <> grinVarName var)))) pure (Map.lookup var (valueLocalSlots env))

freshSlot :: FunctionM Int
freshSlot = do
  state <- get
  let result = functionNextSlot state
  modify' $ \current -> current {functionNextSlot = result + 1}
  pure result

freshLabel :: Text -> Text -> FunctionM Text
freshLabel parent kind = do
  state <- get
  let identifier = functionNextLabel state
  modify' $ \current -> current {functionNextLabel = identifier + 1}
  pure (parent <> "_" <> kind <> "_" <> tshow identifier)

addBlock :: Text -> [Text] -> FunctionM ()
addBlock label lines' = modify' $ \state -> state {functionBlocksRev = (label, lines') : functionBlocksRev state}

renderBlock :: (Text, [Text]) -> [Text]
renderBlock (label, lines') = (label <> ":;") : lines'

globalSlot :: CompileEnv -> Text -> Either CError Int
globalSlot env name = maybe (Left (CMissingGlobal name)) Right (Map.lookup name (compileGlobalSlots env))

constructorId :: CompileEnv -> Text -> Either CError Int
constructorId env name = maybe (Left (CMissingConstructor name)) Right (Map.lookup name (compileConstructorIds env))

lookupRuntimeInfoLabel :: CompileEnv -> RuntimeInfoKey -> Either CError Text
lookupRuntimeInfoLabel env key =
  case Map.lookup key (compileNodeInfoLabels env) of
    Just label -> Right label
    Nothing -> case key of
      ConstructorRuntimeInfo name _ -> Left (CMissingConstructor name)
      ClosureRuntimeInfo functionName _ _ -> Left (CMissingFunction functionName)
      ThunkRuntimeInfo functionName _ -> Left (CMissingFunction functionName)

functionCodeLabel :: CompileEnv -> FunctionName -> Either CError Text
functionCodeLabel env name = maybe (Left (CMissingFunction name)) Right (Map.lookup name (compileFunctionLabels env))

runtimeInfoKeyStages :: GrinNode -> [RuntimeInfoKey]
runtimeInfoKeyStages node =
  case grinNodeTag node of
    GrinConstructor name remaining -> [ConstructorRuntimeInfo name remaining]
    GrinClosure functionName layouts -> stages fields layouts
      where
        stages current remaining =
          ClosureRuntimeInfo functionName current remaining : case remaining of
            [] -> []
            layout : rest -> stages (current <> layout) rest
    GrinThunk functionName -> [ThunkRuntimeInfo functionName fields]
  where
    fields = map grinValueRuntimeRep (grinNodeFields node)

runtimeInfoFunctionName :: RuntimeInfoKey -> Maybe FunctionName
runtimeInfoFunctionName key = case key of
  ConstructorRuntimeInfo {} -> Nothing
  ClosureRuntimeInfo name _ _ -> Just name
  ThunkRuntimeInfo name _ -> Just name

runtimeInfoKeyFields :: RuntimeInfoKey -> [RuntimeRep]
runtimeInfoKeyFields key = case key of
  ConstructorRuntimeInfo {} -> []
  ClosureRuntimeInfo _ fields _ -> fields
  ThunkRuntimeInfo _ fields -> fields

runtimeInfoKeyRemainingArity :: RuntimeInfoKey -> Int
runtimeInfoKeyRemainingArity key = case key of
  ConstructorRuntimeInfo _ remaining -> remaining
  ClosureRuntimeInfo _ _ layouts -> length layouts
  ThunkRuntimeInfo {} -> 0

runtimeInfoKeyNext :: RuntimeInfoKey -> Maybe RuntimeInfoKey
runtimeInfoKeyNext key = case key of
  ConstructorRuntimeInfo name remaining | remaining > 0 -> Just (ConstructorRuntimeInfo name (remaining - 1))
  ConstructorRuntimeInfo {} -> Nothing
  ClosureRuntimeInfo name fields (layout : rest) -> Just (ClosureRuntimeInfo name (fields <> layout) rest)
  ClosureRuntimeInfo {} -> Nothing
  ThunkRuntimeInfo {} -> Nothing

functionLocalSlots :: GrinFunction -> Map GrinVar Int
functionLocalSlots function = snd (foldl' assignGroup (0, Map.empty) groups)
  where
    groups = grinFunctionParameters function : boundVarGroups (grinFunctionBody function)
    assignGroup = foldl' $ \(next, slots) var -> case Map.lookup var slots of
      Just _ -> (next, slots)
      Nothing -> (next + 1, Map.insert var next slots)

boundVarGroups :: GrinExpr -> [[GrinVar]]
boundVarGroups expression = case expression of
  GrinBind vars value body -> vars : boundVarGroups value <> boundVarGroups body
  GrinStoreRec bindings body -> map (pure . fst) bindings <> boundVarGroups body
  GrinStoreRecUnchecked bindings body -> map (pure . fst) bindings <> boundVarGroups body
  GrinCase _ binder alternatives -> [binder] : concatMap (\alternative -> grinAltBinders alternative : boundVarGroups (grinAltRhs alternative)) alternatives
  _ -> []

programNodes :: GrinProgram -> [GrinNode]
programNodes program = map snd (grinWhnfGlobals program <> grinCafs program) <> concatMap (exprNodes . grinFunctionBody) (grinFunctions program)

exprNodes :: GrinExpr -> [GrinNode]
exprNodes expression = case expression of
  GrinBind _ value body -> exprNodes value <> exprNodes body
  GrinStore node -> [node]
  GrinStoreUnchecked node -> [node]
  GrinStoreRec bindings body -> map snd bindings <> exprNodes body
  GrinStoreRecUnchecked bindings body -> map snd bindings <> exprNodes body
  GrinCase _ _ alternatives -> concatMap (exprNodes . grinAltRhs) alternatives
  _ -> []

programRuntimeReps :: GrinProgram -> [RuntimeRep]
programRuntimeReps program = concatMap (concat . snd) (grinConstructors program) <> concatMap nodeReps (programNodes program) <> concatMap functionReps (grinFunctions program)
  where
    nodeReps = map grinValueRuntimeRep . grinNodeFields
    functionReps function = grinFunctionResultRep function : map grinVarRuntimeRep (grinFunctionParameters function) <> exprReps (grinFunctionBody function)

exprReps :: GrinExpr -> [RuntimeRep]
exprReps expression = case expression of
  GrinBind vars value body -> map grinVarRuntimeRep vars <> exprReps value <> exprReps body
  GrinStore node -> nodeReps node
  GrinEnsureHeap _ roots -> map grinValueRuntimeRep roots
  GrinStoreUnchecked node -> nodeReps node
  GrinStoreRec bindings body -> concatMap (nodeReps . snd) bindings <> exprReps body
  GrinStoreRecUnchecked bindings body -> concatMap (nodeReps . snd) bindings <> exprReps body
  GrinCase value binder alternatives -> grinValueRuntimeRep value : grinVarRuntimeRep binder : concatMap (exprReps . grinAltRhs) alternatives
  _ -> []
  where
    nodeReps = map grinValueRuntimeRep . grinNodeFields

validateRuntimeRep :: RuntimeRep -> Either CError ()
validateRuntimeRep runtimeRep = case runtimeRep of
  VecRep {} -> Left (CUnsupportedRuntimeRep runtimeRep)
  TupleRep reps -> mapM_ validateRuntimeRep reps
  SumRep reps -> mapM_ validateRuntimeRep reps
  RuntimeRepVar {} -> Left (CUnsupportedRuntimeRep runtimeRep)
  RuntimeRepMeta {} -> Left (CUnsupportedRuntimeRep runtimeRep)
  _ -> Right ()

normalizedLiteralInteger :: GrinLiteral -> Maybe Integer
normalizedLiteralInteger literal = do
  integer <- case literal of
    GrinLitInt _ value -> Just value
    GrinLitChar _ value -> Just (fromIntegral (ord value))
    _ -> Nothing
  pure $ case literal of
    GrinLitInt runtimeRep _ -> normalizeScalar runtimeRep integer
    _ -> integer

normalizeScalar :: RuntimeRep -> Integer -> Integer
normalizeScalar runtimeRep integer = case runtimeRep of
  IntRep -> signed 64
  Int8Rep -> signed 8
  Int16Rep -> signed 16
  Int32Rep -> signed 32
  Int64Rep -> signed 64
  WordRep -> unsigned 64
  Word8Rep -> unsigned 8
  Word16Rep -> unsigned 16
  Word32Rep -> unsigned 32
  Word64Rep -> unsigned 64
  _ -> integer
  where
    unsigned :: Int -> Integer
    unsigned bits = integer `mod` (2 ^ bits)
    signed :: Int -> Integer
    signed bits = let value = unsigned bits; sign = 2 ^ (bits - 1) in if value >= sign then value - 2 ^ bits else value

renderSlotInteger :: Integer -> Text
renderSlotInteger integer = "(AihcSlot)UINT64_C(" <> tshow (integer `mod` (2 ^ (64 :: Int))) <> ")"

localFunctionLabel :: Int -> GrinFunction -> Text
localFunctionLabel index function = maybe ("aihc_function_" <> tshow index) linkedFunctionLabel (grinFunctionLinkName function)

linkedFunctionLabel :: Text -> Text
linkedFunctionLabel name = "aihc_entry_" <> T.concatMap (\character -> T.pack (showHex (ord character) "_")) name

cLabel :: Text -> Text
cLabel = T.map (\character -> if character `elem` ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ['_'] then character else '_')

safeHead :: [value] -> Maybe value
safeHead [] = Nothing
safeHead (value : _) = Just value

indent :: [Text] -> [Text]
indent = map ("  " <>)

tshow :: (Show value) => value -> Text
tshow = T.pack . show

liftEither :: Either CError value -> FunctionM value
liftEither = either (lift . Left) pure
