{-# LANGUAGE OverloadedStrings #-}

-- | Lower runtime-explicit GC-GRIN directly to WebAssembly machine
-- instructions in LLVM's WebAssembly assembly syntax. The integrated
-- assembler only serializes these instructions and emits linker relocations;
-- generated Haskell code never passes through C or LLVM IR.
module Aihc.Wasm.Codegen
  ( WasmError (..),
    compileProgram,
    validatePrimitiveNames,
    validateProgramPrimitives,
  )
where

import Aihc.Grin.Gc (GcGrinProgram, gcGrinProgram, gcUpdateFunction)
import Aihc.Grin.Syntax
import Aihc.Native (LinkLayout (..), buildAddrLiteralPool, buildLinkLayout)
import Aihc.Tc.Types (Levity (..), RuntimeRep (..))
import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Numeric (showHex)

data WasmError
  = WasmMissingEntry !Text
  | WasmMissingGlobal !Text
  | WasmMissingFunction !FunctionName
  | WasmMissingConstructor !Text
  | WasmUnsupportedPrimitive !Text
  | WasmUnsupportedExpression !Text
  | WasmUnsupportedValue !Text
  | WasmUnsupportedRuntimeRep !RuntimeRep
  deriving (Eq, Show)

data CompileEnv = CompileEnv
  { compileConstructorIds :: !(Map Text Int),
    compileConstructorArities :: !(Map Text Int),
    compileGlobalSlots :: !(Map Text Int),
    compileFunctionLabels :: !(Map FunctionName Text),
    compileAddrLiteralLabels :: !(Map BS.ByteString Text),
    compileNodeInfoLabels :: !(Map RuntimeInfoKey Text),
    compileRuntimeInfos :: ![RuntimeInfo]
  }

data ValueEnv = ValueEnv
  { valueCompileEnv :: !CompileEnv,
    valueLocalSlots :: !(Map GrinVar Int),
    valueScratchBase :: !Int
  }

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

data InitialValue
  = InitialGlobal !Int
  | InitialInteger !Integer
  | InitialAddress !Text

data InitialNode = InitialNode
  { initialNodeSlot :: !Int,
    initialNodeTag :: !Int,
    initialNodeInfo :: !Text,
    initialNodeFields :: ![InitialValue]
  }

data CompiledFunction = CompiledFunction
  { compiledFunctionSlots :: !Int,
    compiledFunctionLines :: ![Text]
  }

type Instructions = [Text]

compileProgram :: Text -> GcGrinProgram -> Either WasmError Text
compileProgram entryName gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  validateProgramPrimitives program
  rootSlot <- maybe (Left (WasmMissingEntry entryName)) Right (Map.lookup entryName (compileGlobalSlots env))
  updateLabel <- functionCodeLabel env (gcUpdateFunction gcProgram)
  functions <- mapM (compileFunction env) (grinFunctions program)
  constructorInit <- compileConstructorInitializers env
  programInit <- compileInitializers env program
  let specialInfos =
        [ specialInfo "aihc_wasm_final_info" "aihc_wasm_final_continuation" [] 1 (Just "aihc_wasm_final_applied_info"),
          specialInfo "aihc_wasm_final_applied_info" "aihc_wasm_final_continuation" [BoxedRep Lifted] 0 Nothing,
          specialInfo "aihc_wasm_top_info" "aihc_wasm_top_continuation" [BoxedRep Lifted] 1 (Just "aihc_wasm_top_applied_info"),
          specialInfo "aihc_wasm_top_applied_info" "aihc_wasm_top_continuation" [BoxedRep Lifted, BoxedRep Lifted] 0 Nothing,
          specialInfo "aihc_wasm_update_info" updateLabel [BoxedRep Lifted, BoxedRep Lifted] 1 (Just "aihc_wasm_update_applied_info"),
          specialInfo "aihc_wasm_update_applied_info" updateLabel [BoxedRep Lifted, BoxedRep Lifted, BoxedRep Lifted] 0 Nothing,
          specialInfo "aihc_wasm_thread_done_info" "aihc_wasm_thread_done_continuation" [] 1 (Just "aihc_wasm_thread_done_applied_info"),
          specialInfo "aihc_wasm_thread_done_applied_info" "aihc_wasm_thread_done_continuation" [BoxedRep Lifted] 0 Nothing
        ]
      source =
        moduleHeader layout env program
          <> concatMap compiledFunctionLines functions
          <> renderSpecialFunctions
          <> renderInitializer (length (linkGlobalNames layout)) rootSlot functions constructorInit programInit
          <> renderArguments layout
          <> renderAddrLiterals env
          <> renderRuntimeInfos (compileRuntimeInfos env <> specialInfos)
          <> ["\t.no_dead_strip\t__indirect_function_table", ""]
  pure (T.unlines source)
  where
    program = gcGrinProgram gcProgram
    layout = buildLinkLayout [program]
    env = compileEnvironment layout program
    specialInfo label entry = RuntimeInfo label Nothing (Just entry)

validateProgramPrimitives :: GrinProgram -> Either WasmError ()
validateProgramPrimitives = validatePrimitiveNames . map (grinVarName . fst) . grinPrimitives

validatePrimitiveNames :: [Text] -> Either WasmError ()
validatePrimitiveNames = mapM_ $ \name ->
  if name `elem` ["+#", "awaitIO#", "fork#", "realWorld#", "yield#"]
    then Right ()
    else Left (WasmUnsupportedPrimitive name)

compileEnvironment :: LinkLayout -> GrinProgram -> CompileEnv
compileEnvironment layout program =
  CompileEnv
    { compileConstructorIds = Map.fromList constructorIds,
      compileConstructorArities = Map.fromList constructors,
      compileGlobalSlots = Map.fromList (zip (linkGlobalNames layout) [0 ..]),
      compileFunctionLabels = functionLabels,
      compileAddrLiteralLabels = Map.fromList [(bytes, "aihc_wasm_addr_" <> tshow index) | (index, (bytes, _)) <- zip [0 :: Int ..] (buildAddrLiteralPool program)],
      compileNodeInfoLabels = Map.fromList [(key, label) | (key, label, _) <- constructorEntries <> functionEntries],
      compileRuntimeInfos = map third (constructorEntries <> functionEntries)
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
        let label = "aihc_wasm_constructor_info_" <> tshow identifier <> "_remaining_" <> tshow remaining
            fields = concat (take (arity - remaining) layouts)
            next = if remaining == 0 then Nothing else Just ("aihc_wasm_constructor_info_" <> tshow identifier <> "_remaining_" <> tshow (remaining - 1))
      ]
    requiredConstructorInfos =
      Set.fromList
        ([ConstructorRuntimeInfo name 0 | (name, arity) <- constructors, arity == 0] <> concatMap requiredNodeConstructorInfos (programNodes program))
    infoKeys =
      [ key
      | key <- Set.toAscList (Set.fromList (concatMap runtimeInfoKeyStages (programNodes program))),
        Just functionName <- [runtimeInfoFunctionName key],
        functionName `Map.member` functionLabels
      ]
    infoLabels = Map.fromList [(key, "aihc_wasm_function_info_" <> tshow index) | (index, key) <- zip [0 :: Int ..] infoKeys]
    functionEntries =
      [ ( key,
          label,
          RuntimeInfo label Nothing (runtimeInfoFunctionName key >>= (`Map.lookup` functionLabels)) (runtimeInfoKeyFields key) (runtimeInfoKeyRemainingArity key) (runtimeInfoKeyNext key >>= (`Map.lookup` infoLabels))
        )
      | (index, key) <- zip [0 :: Int ..] infoKeys,
        let label = "aihc_wasm_function_info_" <> tshow index
      ]
    third (_, _, value) = value

moduleHeader :: LinkLayout -> CompileEnv -> GrinProgram -> [Text]
moduleHeader layout env program =
  [ "# Direct GRIN-to-WebAssembly backend output. No C or LLVM IR is generated.",
    "\t.text"
  ]
    <> map renderFunctionType runtimeFunctionTypes
    <> map (renderFunctionType . (,([I32], [])) . snd) (Map.toAscList (compileFunctionLabels env))
    <> map renderForeignType (grinForeignCalls program)
    <> [""]
  where
    _argumentCapacity = max 3 (linkMaximumArgumentSlots layout + 1)
    renderFunctionType (name, (arguments, results)) =
      "\t.functype\t" <> name <> " (" <> T.intercalate ", " (map renderValueType arguments) <> ") -> (" <> T.intercalate ", " (map renderValueType results) <> ")"
    renderForeignType foreignCall =
      renderFunctionType
        ( grinForeignCallSymbol foreignCall,
          (map foreignValueType (grinForeignArgumentTypes signature), [foreignValueType (grinForeignResultType signature)])
        )
      where
        signature = grinForeignCallSignature foreignCall

data WasmValueType = I32 | I64

renderValueType :: WasmValueType -> Text
renderValueType I32 = "i32"
renderValueType I64 = "i64"

foreignValueType :: GrinForeignType -> WasmValueType
foreignValueType GrinForeignInt32 = I32
foreignValueType GrinForeignWord64 = I64
foreignValueType GrinForeignAddr = I32

runtimeFunctionTypes :: [(Text, ([WasmValueType], [WasmValueType]))]
runtimeFunctionTypes =
  [ ("aihc_machine_new", ([I64], [I32])),
    ("aihc_alloc_locals", ([I32, I64], [I32])),
    ("aihc_wasm_make_node", ([I32, I64, I32], [I64])),
    ("aihc_wasm_make_node_unchecked", ([I32, I64, I32], [I64])),
    ("aihc_ensure_heap", ([I32, I64, I64, I32], [])),
    ("aihc_wasm_set_field", ([I64, I64, I64], [])),
    ("aihc_wasm_update", ([I64, I64], [])),
    ("aihc_wasm_update_blackhole", ([I32, I64, I64], [])),
    ("aihc_wasm_slot_get", ([I32, I64], [I64])),
    ("aihc_wasm_slot_set", ([I32, I64, I64], [])),
    ("aihc_wasm_slot_address", ([I32, I64], [I32])),
    ("aihc_wasm_global_get", ([I32, I64], [I64])),
    ("aihc_wasm_global_set", ([I32, I64, I64], [])),
    ("aihc_wasm_value_field", ([I64, I64], [I64])),
    ("aihc_wasm_value_info", ([I64], [I64])),
    ("aihc_wasm_transfer_direct", ([I32, I32], [])),
    ("aihc_wasm_transfer_eval", ([I32, I32, I64, I64, I64, I64], [])),
    ("aihc_wasm_transfer_apply", ([I32, I32, I64, I64, I32, I64], [])),
    ("aihc_wasm_transfer_continue", ([I32, I32, I64, I64, I32], [])),
    ("aihc_wasm_transfer_fork", ([I32, I32, I64, I64], [])),
    ("aihc_wasm_transfer_yield", ([I32, I32, I64], [])),
    ("aihc_wasm_transfer_await_io", ([I32, I32, I64, I64], [])),
    ("aihc_wasm_transfer_thread_done", ([I32, I32], [])),
    ("aihc_wasm_transfer_halt", ([I32], [])),
    ("aihc_wasm_transfer_start", ([I32, I32, I64, I64, I64, I64, I32], [])),
    ("aihc_set_thread_done_continuation", ([I32, I32], [])),
    ("aihc_no_match", ([], [])),
    ("aihc_unsupported_primitive", ([], []))
  ]

compileFunction :: CompileEnv -> GrinFunction -> Either WasmError CompiledFunction
compileFunction env function = do
  label <- functionCodeLabel env (grinFunctionName function)
  let slots = functionLocalSlots function
      scratchCount = maximumScratchSlots (grinFunctionBody function)
      slotCount = max 1 (Map.size slots + scratchCount)
      valueEnv = ValueEnv env slots (Map.size slots)
  body <- compileExpr valueEnv (grinFunctionBody function)
  let parameterLines = concatMap copyParameter (zip [0 :: Int ..] (grinFunctionParameters function))
      copyParameter (index, var) =
        case Map.lookup var slots of
          Nothing -> []
          Just slot -> localSet slot (argumentGet index)
  pure
    CompiledFunction
      { compiledFunctionSlots = slotCount,
        compiledFunctionLines =
          functionStart label [I32, I32, I64]
            <> indent
              ( [ "i32.const\t0",
                  "i32.load\taihc_machine",
                  "local.set\t1",
                  "local.get\t1",
                  "i64.const\t" <> tshow slotCount,
                  "call\taihc_alloc_locals",
                  "local.set\t2"
                ]
                  <> parameterLines
                  <> body
              )
            <> functionEnd
      }

compileExpr :: ValueEnv -> GrinExpr -> Either WasmError Instructions
compileExpr env expression =
  case expression of
    GrinBind vars value body -> (<>) <$> compileDirectBinding env vars value <*> compileExpr env body
    GrinStoreRec bindings body -> compileStoreRec False bindings body
    GrinStoreRecUnchecked bindings body -> compileStoreRec True bindings body
    GrinCpsEval runtimeRep value continuation updateContinuation ->
      pure . terminal $
        ( machine
            <> argumentsAddress
            <> materializeValue env value
            <> i64Const (boolInteger (isLiftedRuntimeRep runtimeRep))
            <> materializeValue env continuation
            <> materializeValue env updateContinuation
            <> call "aihc_wasm_transfer_eval"
        )
    GrinCall _ functionName values -> do
      target <- functionCodeLabel (valueCompileEnv env) functionName
      pure (terminal (concatMap (uncurry (argumentSet env)) (zip [0 :: Int ..] values) <> i32Symbol target <> argumentsAddress <> call "aihc_wasm_transfer_direct"))
    GrinCpsPrimitiveCall _ name values continuation -> compileCpsPrimitive env name values continuation
    GrinCpsApply _ function values continuation -> do
      scratch <- storeScratchValues env values
      pure
        ( terminal
            ( machine
                <> argumentsAddress
                <> materializeValue env function
                <> i64Const (tshow (length values))
                <> scratch
                <> materializeValue env continuation
                <> call "aihc_wasm_transfer_apply"
            )
        )
    GrinContinue continuation values -> do
      scratch <- storeScratchValues env values
      pure
        ( terminal
            ( machine
                <> argumentsAddress
                <> materializeValue env continuation
                <> i64Const (tshow (length values))
                <> scratch
                <> call "aihc_wasm_transfer_continue"
            )
        )
    GrinHalt _ -> pure (terminal (machine <> call "aihc_wasm_transfer_halt"))
    GrinCase scrutinee binder alternatives -> compileCase env scrutinee binder alternatives
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
    unsupported = Left . WasmUnsupportedExpression
    terminal instructions = instructions <> ["return"]
    compileStoreRec unchecked bindings body = do
      allocations <- fmap concat . forM bindings $ uncurry (allocateNodeInto env unchecked)
      fields <- fmap concat . forM bindings $ \(var, node) -> initializeNodeFields env (localGet env var) node
      rest <- compileExpr env body
      pure (allocations <> fields <> rest)

compileCpsPrimitive :: ValueEnv -> Text -> [GrinValue] -> GrinValue -> Either WasmError Instructions
compileCpsPrimitive env name values continuation =
  case (name, values) of
    ("awaitIO#", [request]) -> transfer "aihc_wasm_transfer_await_io" [request, continuation]
    ("fork#", [action]) -> transfer "aihc_wasm_transfer_fork" [action, continuation]
    ("yield#", []) -> transfer "aihc_wasm_transfer_yield" [continuation]
    _ -> Left (WasmUnsupportedExpression ("CPS primitive call " <> name))
  where
    transfer function arguments =
      Right (machine <> argumentsAddress <> concatMap (materializeValue env) arguments <> call function <> ["return"])

compileDirectBinding :: ValueEnv -> [GrinVar] -> GrinExpr -> Either WasmError Instructions
compileDirectBinding env vars expression =
  case expression of
    GrinConstant values
      | length vars == length values -> pure (concat [localSetFor env var (materializeValue env value) | (var, value) <- zip vars values])
    GrinStore node -> storeNode False node
    GrinEnsureHeap requiredWords roots
      | length vars == length roots -> do
          slots <- mapM (localSlot env) vars
          let stores = concat [localSetFor env var (materializeValue env value) | (var, value) <- zip vars roots]
              rootsAddress = case slots of
                [] -> i32Const "0"
                slot : _ -> locals <> i64Const (tshow slot) <> call "aihc_wasm_slot_address"
          pure (stores <> machine <> i64Const (tshow requiredWords) <> i64Const (tshow (length roots)) <> rootsAddress <> call "aihc_ensure_heap")
    GrinStoreUnchecked node -> storeNode True node
    GrinFetch _ pointer -> storeSingle (materializeValue env pointer)
    GrinUpdate pointer value -> update "aihc_wasm_update" False pointer value
    GrinUpdateBlackhole pointer value -> update "aihc_wasm_update_blackhole" True pointer value
    GrinPrimitiveCall IntRep "+#" [left, right] -> storeSingle (materializeValue env left <> materializeValue env right <> ["i64.add"])
    GrinPrimitiveCall runtimeRep "realWorld#" []
      | null vars && null (runtimeRepComponents runtimeRep) -> pure []
    GrinPrimitiveCall _ name _ -> Left (WasmUnsupportedExpression ("primitive call " <> name))
    GrinForeignCallExpr foreignCall arguments -> compileForeignCall env foreignCall arguments >>= storeSingle
    _ -> Left (WasmUnsupportedExpression "non-direct expression remained in a CPS bind")
  where
    storeSingle instructions = case vars of
      [var] -> pure (localSetFor env var instructions)
      _ -> Left (WasmUnsupportedExpression "direct expression result arity")
    storeNode unchecked node = case vars of
      [var] -> allocateNodeInto env unchecked var node
      _ -> Left (WasmUnsupportedExpression "node result arity")
    update function passMachine pointer value =
      storeSingle
        ((if passMachine then machine else []) <> materializeValue env pointer <> materializeValue env value <> call function <> materializeValue env value)

compileForeignCall :: ValueEnv -> GrinForeignCall -> [GrinValue] -> Either WasmError Instructions
compileForeignCall env foreignCall arguments = do
  let signature = grinForeignCallSignature foreignCall
      argumentTypes = grinForeignArgumentTypes signature
  if length arguments /= length (grinForeignOperandReps signature)
    then Left (WasmUnsupportedExpression "foreign call arity mismatch")
    else
      pure
        ( concat (zipWith foreignArgumentInstructions argumentTypes arguments)
            <> call (grinForeignCallSymbol foreignCall)
            <> foreignResultInstructions (grinForeignResultType signature)
        )
  where
    foreignArgumentInstructions kind value =
      materializeValue env value <> case kind of
        GrinForeignInt32 -> ["i32.wrap_i64"]
        GrinForeignWord64 -> []
        GrinForeignAddr -> ["i32.wrap_i64"]
    foreignResultInstructions kind = case kind of
      GrinForeignInt32 -> ["i64.extend_i32_s"]
      GrinForeignWord64 -> []
      GrinForeignAddr -> ["i64.extend_i32_u"]

compileCase :: ValueEnv -> GrinValue -> GrinVar -> [GrinAlt] -> Either WasmError Instructions
compileCase env scrutinee binder alternatives = do
  choices <- compileChoices alternatives
  pure (materializeValue env scrutinee <> ["local.set\t3"] <> localSetFor env binder ["local.get\t3"] <> choices)
  where
    pointer = isPointerRuntimeRep (grinValueRuntimeRep scrutinee)
    compileChoices [] = pure (call "aihc_no_match" <> ["unreachable"])
    compileChoices (alternative : rest) = case grinAltCon alternative of
      GrinDefaultAlt -> compileAlternative alternative
      constructor -> do
        condition <- case constructor of
          GrinDataAlt name
            | pointer -> do
                identifier <- constructorId (valueCompileEnv env) name
                pure (["local.get\t3"] <> call "aihc_wasm_value_info" <> i64Const (tshow identifier) <> ["i64.eq"])
          GrinDataAlt {} -> Left (WasmUnsupportedExpression "constructor case on unboxed value")
          GrinLitAlt literal
            | pointer -> Left (WasmUnsupportedExpression "literal case on lifted value")
            | otherwise -> case normalizedLiteralInteger literal of
                Nothing -> Left (WasmUnsupportedValue "string case alternative")
                Just integer -> pure (["local.get\t3"] <> i64Const (renderInteger integer) <> ["i64.eq"])
        accepted <- compileAlternative alternative
        rejected <- compileChoices rest
        pure (condition <> ["if"] <> indent accepted <> ["else"] <> indent rejected <> ["end_if"])
    compileAlternative alternative = do
      bindings <- case grinAltCon alternative of
        GrinDataAlt _ -> fmap concat . forM (zip [0 :: Int ..] (grinAltBinders alternative)) $ \(index, var) ->
          pure (localSetFor env var (["local.get\t3"] <> i64Const (tshow index) <> call "aihc_wasm_value_field"))
        GrinLitAlt _ -> pure []
        GrinDefaultAlt -> pure (concatMap (\var -> localSetFor env var ["local.get\t3"]) (grinAltBinders alternative))
      (bindings <>) <$> compileExpr env (grinAltRhs alternative)

allocateNodeInto :: ValueEnv -> Bool -> GrinVar -> GrinNode -> Either WasmError Instructions
allocateNodeInto env unchecked var node = do
  (tag, info) <- nodeHeader (valueCompileEnv env) node
  let allocation = machine <> i64Const (tshow tag) <> i32Data info <> call (if unchecked then "aihc_wasm_make_node_unchecked" else "aihc_wasm_make_node")
  fields <- initializeNodeFields env (localGet env var) node
  pure (localSetFor env var allocation <> fields)

initializeNodeFields :: ValueEnv -> Instructions -> GrinNode -> Either WasmError Instructions
initializeNodeFields env object node =
  pure . concat $
    [ object <> i64Const (tshow index) <> materializeValue env field <> call "aihc_wasm_set_field"
    | (index, field) <- zip [0 :: Int ..] (grinNodeFields node)
    ]

materializeValue :: ValueEnv -> GrinValue -> Instructions
materializeValue env value = case value of
  GrinVarValue var ->
    case Map.lookup var (valueLocalSlots env) of
      Just slot -> localGetSlot slot
      Nothing -> case Map.lookup (grinVarName var) (compileGlobalSlots (valueCompileEnv env)) of
        Just slot -> machine <> i64Const (tshow slot) <> call "aihc_wasm_global_get"
        Nothing -> call "aihc_no_match" <> ["unreachable", "i64.const\t0"]
  GrinLitValue literal -> case literal of
    GrinLitAddr bytes -> case Map.lookup bytes (compileAddrLiteralLabels (valueCompileEnv env)) of
      Just label -> i32Data label <> ["i64.extend_i32_u"]
      Nothing -> i64Const "0"
    _ -> maybe (i64Const "0") (i64Const . renderInteger) (normalizedLiteralInteger literal)

storeScratchValues :: ValueEnv -> [GrinValue] -> Either WasmError Instructions
storeScratchValues env values =
  pure
    ( concat [localSet (valueScratchBase env + index) (materializeValue env value) | (index, value) <- zip [0 :: Int ..] values]
        <> case values of
          [] -> i32Const "0"
          _ -> locals <> i64Const (tshow (valueScratchBase env)) <> call "aihc_wasm_slot_address"
    )

argumentGet :: Int -> Instructions
argumentGet index = ["local.get\t0"] <> i64Const (tshow index) <> call "aihc_wasm_slot_get"

argumentSet :: ValueEnv -> Int -> GrinValue -> Instructions
argumentSet env index value = argumentsAddress <> i64Const (tshow index) <> materializeValue env value <> call "aihc_wasm_slot_set"

localGet :: ValueEnv -> GrinVar -> Instructions
localGet env var = maybe (i64Const "0") localGetSlot (Map.lookup var (valueLocalSlots env))

localGetSlot :: Int -> Instructions
localGetSlot slot = locals <> i64Const (tshow slot) <> call "aihc_wasm_slot_get"

localSetFor :: ValueEnv -> GrinVar -> Instructions -> Instructions
localSetFor env var value = maybe [] (`localSet` value) (Map.lookup var (valueLocalSlots env))

localSet :: Int -> Instructions -> Instructions
localSet slot value = locals <> i64Const (tshow slot) <> value <> call "aihc_wasm_slot_set"

localSlot :: ValueEnv -> GrinVar -> Either WasmError Int
localSlot env var = maybe (Left (WasmUnsupportedExpression ("missing local slot for " <> grinVarName var))) Right (Map.lookup var (valueLocalSlots env))

machine, locals, argumentsAddress :: Instructions
machine = ["local.get\t1"]
locals = ["local.get\t2"]
argumentsAddress = i32Symbol "aihc_arguments"

i32Const, i64Const, i32Symbol, i32Data, call :: Text -> Instructions
i32Const value = ["i32.const\t" <> value]
i64Const value = ["i64.const\t" <> value]
i32Symbol = i32Const
i32Data = i32Const . dataLabel
call function = ["call\t" <> function]

functionStart :: Text -> [WasmValueType] -> [Text]
functionStart label localTypes =
  [ "\t.section\t.text." <> label <> ",\"\",@",
    "\t.hidden\t" <> label,
    "\t.globl\t" <> label,
    "\t.type\t" <> label <> ",@function",
    label <> ":",
    "\t.functype\t" <> label <> " (i32) -> ()"
  ]
    <> ["\t.local\t" <> T.intercalate ", " (map renderValueType localTypes) | not (null localTypes)]

functionStartNoArguments :: Text -> [WasmValueType] -> [Text]
functionStartNoArguments label localTypes =
  [ "\t.section\t.text." <> label <> ",\"\",@",
    "\t.hidden\t" <> label,
    "\t.globl\t" <> label,
    "\t.type\t" <> label <> ",@function",
    label <> ":",
    "\t.functype\t" <> label <> " () -> ()"
  ]
    <> ["\t.local\t" <> T.intercalate ", " (map renderValueType localTypes) | not (null localTypes)]

functionEnd :: [Text]
functionEnd = ["\tend_function", ""]

renderSpecialFunctions :: [Text]
renderSpecialFunctions =
  functionStart "aihc_wasm_top_continuation" []
    <> indent
      ( ["i32.const\t0", "i32.load\taihc_machine"]
          <> argumentsAddress
          <> ["local.get\t0"]
          <> i64Const "1"
          <> call "aihc_wasm_slot_get"
          <> i64Const "0"
          <> i32Const "0"
          <> ["local.get\t0"]
          <> i64Const "0"
          <> call "aihc_wasm_slot_get"
          <> call "aihc_wasm_transfer_apply"
          <> ["return"]
      )
    <> functionEnd
    <> functionStart "aihc_wasm_thread_done_continuation" []
    <> indent (["i32.const\t0", "i32.load\taihc_machine"] <> argumentsAddress <> call "aihc_wasm_transfer_thread_done" <> ["return"])
    <> functionEnd
    <> functionStart "aihc_wasm_final_continuation" []
    <> indent (["i32.const\t0", "i32.load\taihc_machine"] <> call "aihc_wasm_transfer_halt" <> ["return"])
    <> functionEnd
    <> functionStart "aihc_wasm_exit" []
    <> indent (i32Const "0" <> i32Const "0" <> call "aihc_wasm_transfer_direct" <> ["return"])
    <> functionEnd

compileConstructorInitializers :: CompileEnv -> Either WasmError [(Int, Text)]
compileConstructorInitializers env = forM nullary $ \(name, _identifier) -> do
  slot <- globalSlot env name
  info <- lookupRuntimeInfoLabel env (ConstructorRuntimeInfo name 0)
  pure (slot, info)
  where
    nullary =
      [ (name, identifier)
      | (name, identifier) <- Map.toAscList (compileConstructorIds env),
        Map.lookup name (compileConstructorArities env) == Just 0
      ]

compileInitializers :: CompileEnv -> GrinProgram -> Either WasmError [InitialNode]
compileInitializers env program = mapM withSlot (grinCafs program <> grinWhnfGlobals program)
  where
    withSlot (var, node) = do
      slot <- globalSlot env (grinVarName var)
      (tag, info) <- nodeHeader env node
      fields <- mapM initialValue (grinNodeFields node)
      pure (InitialNode slot tag info fields)
    initialValue value = case value of
      GrinVarValue var -> InitialGlobal <$> globalSlot env (grinVarName var)
      GrinLitValue literal -> case literal of
        GrinLitAddr bytes -> maybe (Left (WasmUnsupportedValue "unregistered initializer address")) (Right . InitialAddress) (Map.lookup bytes (compileAddrLiteralLabels env))
        _ -> maybe (Left (WasmUnsupportedValue "unsupported initializer literal")) (Right . InitialInteger) (normalizedLiteralInteger literal)

renderInitializer :: Int -> Int -> [CompiledFunction] -> [(Int, Text)] -> [InitialNode] -> [Text]
renderInitializer globalCount rootSlot functions constructors globals =
  functionStartNoArguments "aihc_wasm_program_initialize" [I32, I32, I64]
    <> indent
      ( i64Const (tshow globalCount)
          <> call "aihc_machine_new"
          <> ["local.set\t0", "i32.const\t0", "local.get\t0", "i32.store\taihc_machine", "local.get\t0"]
          <> i64Const (tshow maximumSlots)
          <> call "aihc_alloc_locals"
          <> ["local.set\t1"]
          <> concatMap (uncurry (initializeGlobalNode 0)) constructors
          <> concatMap (\node -> initializeGlobalNode (initialNodeTag node) (initialNodeSlot node) (initialNodeInfo node)) globals
          <> concatMap initializeGlobalFields globals
          <> initializeSpecials
          <> ["local.get\t0"]
          <> argumentsAddress
          <> globalGet rootSlot
          <> specialGet 1
          <> specialGet 2
          <> specialGet 3
          <> i32Symbol "aihc_wasm_exit"
          <> call "aihc_wasm_transfer_start"
          <> ["return"]
      )
    <> functionEnd
  where
    maximumSlots = max 4 (maximum (1 : map compiledFunctionSlots functions))
    initializeGlobalNode :: Int -> Int -> Text -> Instructions
    initializeGlobalNode tag slot info =
      ["local.get\t0"]
        <> i64Const (tshow tag)
        <> i32Data info
        <> call "aihc_wasm_make_node"
        <> ["local.set\t2", "local.get\t0"]
        <> i64Const (tshow slot)
        <> ["local.get\t2"]
        <> call "aihc_wasm_global_set"
    initializeGlobalFields node = concatMap (initializeField node) (zip [0 :: Int ..] (initialNodeFields node))
    initializeField node (index, value) =
      globalGet (initialNodeSlot node)
        <> i64Const (tshow index)
        <> initialValue value
        <> call "aihc_wasm_set_field"
    initialValue value = case value of
      InitialGlobal slot -> globalGet slot
      InitialInteger integer -> i64Const (renderInteger integer)
      InitialAddress label -> i32Data label <> ["i64.extend_i32_u"]
    globalGet :: Int -> Instructions
    globalGet slot = ["local.get\t0"] <> i64Const (tshow slot) <> call "aihc_wasm_global_get"
    specialSet :: Int -> Instructions -> Instructions
    specialSet slot value = ["local.get\t1"] <> i64Const (tshow slot) <> value <> call "aihc_wasm_slot_set"
    specialGet :: Int -> Instructions
    specialGet slot = ["local.get\t1"] <> i64Const (tshow slot) <> call "aihc_wasm_slot_get"
    makeSpecial info = ["local.get\t0"] <> i64Const "1" <> i32Data info <> call "aihc_wasm_make_node"
    initializeSpecials =
      specialSet 0 (makeSpecial "aihc_wasm_final_info")
        <> specialSet 1 (makeSpecial "aihc_wasm_top_info")
        <> specialGet 1
        <> i64Const "0"
        <> specialGet 0
        <> call "aihc_wasm_set_field"
        <> specialSet 2 (makeSpecial "aihc_wasm_update_info")
        <> specialGet 2
        <> i64Const "0"
        <> globalGet rootSlot
        <> call "aihc_wasm_set_field"
        <> specialGet 2
        <> i64Const "1"
        <> specialGet 1
        <> call "aihc_wasm_set_field"
        <> specialSet 3 (makeSpecial "aihc_wasm_thread_done_info")
        <> ["local.get\t0"]
        <> specialGet 3
        <> ["i32.wrap_i64"]
        <> call "aihc_set_thread_done_continuation"

renderArguments :: LinkLayout -> [Text]
renderArguments layout =
  [ "\t.hidden\taihc_machine",
    "\t.hidden\taihc_next_transfer",
    "\t.hidden\taihc_arguments",
    "\t.type\taihc_arguments,@object",
    "\t.section\t.bss.aihc_arguments,\"\",@",
    "\t.globl\taihc_arguments",
    "\t.p2align\t3, 0x0",
    "aihc_arguments:",
    "\t.skip\t" <> tshow (8 * argumentCapacity),
    "\t.size\taihc_arguments, " <> tshow (8 * argumentCapacity),
    ""
  ]
  where
    argumentCapacity = max 3 (linkMaximumArgumentSlots layout + 1)

renderAddrLiterals :: CompileEnv -> [Text]
renderAddrLiterals env = concatMap render (Map.toAscList (compileAddrLiteralLabels env))
  where
    render (bytes, label) =
      objectStart label 0
        <> map (("\t.int8\t" <>) . tshow) (BS.unpack bytes <> [0])
        <> ["\t.size\t" <> dataLabel label <> ", " <> tshow (BS.length bytes + 1), ""]

renderRuntimeInfos :: [RuntimeInfo] -> [Text]
renderRuntimeInfos infos = concatMap renderBitmap infos <> concatMap renderInfo infos
  where
    renderBitmap info
      | null (runtimeInfoFields info) = []
      | otherwise =
          objectStart (runtimeInfoLabel info <> "_bitmap") 0
            <> ["\t.int8\t" <> if isPointerRuntimeRep field then "1" else "0" | field <- runtimeInfoFields info]
            <> [ "\t.size\t" <> dataLabel (runtimeInfoLabel info <> "_bitmap") <> ", " <> tshow (length (runtimeInfoFields info)),
                 ""
               ]
    renderInfo info =
      objectStart (runtimeInfoLabel info) 3
        <> [ "\t.int32\t" <> maybe "0" tshow (runtimeInfoIdentity info),
             "\t.int32\t" <> fromMaybe "0" (runtimeInfoEntry info),
             "\t.int64\t" <> tshow (length (runtimeInfoFields info)),
             "\t.int64\t" <> tshow (runtimeInfoRemainingArity info),
             "\t.int32\t" <> if null (runtimeInfoFields info) then "0" else dataLabel (runtimeInfoLabel info <> "_bitmap"),
             "\t.int32\t" <> maybe "0" dataLabel (runtimeInfoNext info),
             "\t.int32\t0",
             "\t.skip\t4",
             "\t.size\t" <> dataLabel (runtimeInfoLabel info) <> ", 40",
             ""
           ]

objectStart :: Text -> Int -> [Text]
objectStart label alignment =
  [ "\t.type\t" <> dataLabel label <> ",@object",
    "\t.section\t.rodata." <> dataLabel label <> ",\"\",@"
  ]
    <> ["\t.p2align\t" <> tshow alignment <> ", 0x0" | alignment /= 0]
    <> [dataLabel label <> ":"]

dataLabel :: Text -> Text
dataLabel = (".L" <>)

nodeHeader :: CompileEnv -> GrinNode -> Either WasmError (Int, Text)
nodeHeader env node = do
  info <- lookupRuntimeInfoLabel env key
  pure (tag, info)
  where
    fields = map grinValueRuntimeRep (grinNodeFields node)
    (tag, key) = case grinNodeTag node of
      GrinConstructor name remaining -> (if remaining == 0 then 0 else 3, ConstructorRuntimeInfo name remaining)
      GrinClosure functionName layouts -> (1, ClosureRuntimeInfo functionName fields layouts)
      GrinThunk functionName -> (2, ThunkRuntimeInfo functionName fields)

globalSlot :: CompileEnv -> Text -> Either WasmError Int
globalSlot env name = maybe (Left (WasmMissingGlobal name)) Right (Map.lookup name (compileGlobalSlots env))

constructorId :: CompileEnv -> Text -> Either WasmError Int
constructorId env name = maybe (Left (WasmMissingConstructor name)) Right (Map.lookup name (compileConstructorIds env))

lookupRuntimeInfoLabel :: CompileEnv -> RuntimeInfoKey -> Either WasmError Text
lookupRuntimeInfoLabel env key = case Map.lookup key (compileNodeInfoLabels env) of
  Just label -> Right label
  Nothing -> case key of
    ConstructorRuntimeInfo name _ -> Left (WasmMissingConstructor name)
    ClosureRuntimeInfo functionName _ _ -> Left (WasmMissingFunction functionName)
    ThunkRuntimeInfo functionName _ -> Left (WasmMissingFunction functionName)

functionCodeLabel :: CompileEnv -> FunctionName -> Either WasmError Text
functionCodeLabel env name = maybe (Left (WasmMissingFunction name)) Right (Map.lookup name (compileFunctionLabels env))

requiredNodeConstructorInfos :: GrinNode -> [RuntimeInfoKey]
requiredNodeConstructorInfos node = case grinNodeTag node of
  GrinConstructor name remaining -> [ConstructorRuntimeInfo name stage | stage <- [remaining, remaining - 1 .. 0]]
  _ -> []

runtimeInfoKeyStages :: GrinNode -> [RuntimeInfoKey]
runtimeInfoKeyStages node = case grinNodeTag node of
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

maximumScratchSlots :: GrinExpr -> Int
maximumScratchSlots expression = case expression of
  GrinBind _ value body -> max (maximumScratchSlots value) (maximumScratchSlots body)
  GrinStoreRec _ body -> maximumScratchSlots body
  GrinStoreRecUnchecked _ body -> maximumScratchSlots body
  GrinCpsApply _ _ values _ -> length values
  GrinContinue _ values -> length values
  GrinCase _ _ alternatives -> maximum (0 : map (maximumScratchSlots . grinAltRhs) alternatives)
  _ -> 0

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

validateRuntimeRep :: RuntimeRep -> Either WasmError ()
validateRuntimeRep runtimeRep = case runtimeRep of
  VecRep {} -> Left (WasmUnsupportedRuntimeRep runtimeRep)
  TupleRep reps -> mapM_ validateRuntimeRep reps
  SumRep reps -> mapM_ validateRuntimeRep reps
  RuntimeRepVar {} -> Left (WasmUnsupportedRuntimeRep runtimeRep)
  RuntimeRepMeta {} -> Left (WasmUnsupportedRuntimeRep runtimeRep)
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
  IntRep -> signed (64 :: Int)
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

renderInteger :: Integer -> Text
renderInteger = tshow

localFunctionLabel :: Int -> GrinFunction -> Text
localFunctionLabel index function = maybe ("aihc_function_" <> tshow index) linkedFunctionLabel (grinFunctionLinkName function)

linkedFunctionLabel :: Text -> Text
linkedFunctionLabel name = "aihc_link_" <> T.concat [T.pack (pad2 (showHex byte "")) | byte <- BS.unpack (Text.encodeUtf8 name)]
  where
    pad2 value = replicate (2 - length value) '0' <> value

boolInteger :: Bool -> Text
boolInteger True = "1"
boolInteger False = "0"

indent :: [Text] -> [Text]
indent = map ("\t" <>)

tshow :: (Show value) => value -> Text
tshow = T.pack . show
