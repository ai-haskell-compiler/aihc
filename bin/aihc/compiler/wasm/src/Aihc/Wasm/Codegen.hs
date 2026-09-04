{-# LANGUAGE OverloadedStrings #-}

-- | Lower runtime-explicit GC-GRIN directly to WebAssembly machine
-- instructions in LLVM's WebAssembly assembly syntax. The integrated
-- assembler only serializes these instructions and emits linker relocations;
-- generated Haskell code never passes through C or LLVM IR.
module Aihc.Wasm.Codegen
  ( WasmError (..),
    compileEntry,
    compileModule,
    validatePrimitiveNames,
    validateProgramPrimitives,
  )
where

import Aihc.Grin.Cps (ContinuationFrameKind (..), continuationFrameKindCode)
import Aihc.Grin.Gc (GcGrinProgram, entryGcProgram, gcContinuationFrames, gcContinuationFunctions, gcGrinProgram, gcUpdateFunction)
import Aihc.Grin.Srt
import Aihc.Grin.Syntax
import Aihc.Native
  ( NativeRuntimeCall (..),
    buildAddrLiteralPool,
    executableEntryName,
    nativeRuntimePrimitiveCall,
    nativeSplitRuntimePrimitiveCall,
    renderLinkedConstructorInfoSymbol,
    renderLinkedGlobalSymbol,
    supportedNativePrimitiveNames,
  )
import Control.Monad (forM)
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data WasmError
  = WasmMissingGlobal !Text
  | WasmMissingFunction !FunctionName
  | WasmMissingConstructor !Text
  | WasmUnsupportedPrimitive !Text
  | WasmUnsupportedExpression !Text
  | WasmUnsupportedValue !Text
  | WasmUnsupportedRuntimeRep !GrinRep
  deriving (Eq, Show)

data CompileEnv = CompileEnv
  { compileFunctionLabels :: !(Map FunctionName Text),
    compileFunctionArities :: !(Map FunctionName Int),
    compileAddrLiteralLabels :: !(Map BS.ByteString Text),
    compileNodeInfoLabels :: !(Map RuntimeInfoKey Text),
    compileRuntimeInfos :: ![RuntimeInfo],
    compileStaticReferences :: !StaticReferences,
    compileSrtLabels :: !(Map FunctionName Text),
    compileAllowUnsupportedPrimitives :: !Bool
  }

data ValueEnv = ValueEnv
  { valueCompileEnv :: !CompileEnv,
    valueLocals :: !(Map GrinVar Int),
    valueCaseLocal :: !Int
  }

data RuntimeAdapter = RuntimeAdapter
  { runtimeAdapterTarget :: !Text,
    runtimeAdapterArity :: !Int,
    runtimeAdapterEnter :: !(Maybe RuntimeEnter)
  }

data RuntimeEnter = RuntimeEnter
  { runtimeEnterStoredCount :: !Int,
    runtimeEnterSuppliedCount :: !Int,
    runtimeEnterPassContinuation :: !Bool
  }

data RuntimeInfo = RuntimeInfo
  { runtimeInfoLabel :: !Text,
    runtimeInfoIdentity :: !(Maybe Text),
    runtimeInfoEntry :: !(Maybe Text),
    runtimeInfoFields :: ![GrinRep],
    runtimeInfoRemainingArity :: !Int,
    runtimeInfoNext :: !(Maybe Text),
    runtimeInfoAdapter :: !(Maybe RuntimeAdapter),
    runtimeInfoFrameKind :: !(Maybe ContinuationFrameKind),
    runtimeInfoObjectKind :: !Int,
    runtimeInfoSrt :: !(Maybe Text)
  }

data RuntimeInfoKey
  = ConstructorRuntimeInfo !Text !Int
  | ClosureRuntimeInfo !FunctionName ![GrinRep] ![[GrinRep]]
  | ThunkRuntimeInfo !FunctionName ![GrinRep]
  deriving (Eq, Ord, Show)

data CompiledFunction = CompiledFunction
  { compiledFunctionScratchSlots :: !Int,
    compiledFunctionLines :: ![Text]
  }

type Instructions = [Text]

data CompilationUnit = EntryUnit | LibraryUnit

-- | Compile the fixed executable entry unit.
compileEntry :: Either WasmError Text
compileEntry = do
  gcProgram <- either (Left . WasmUnsupportedExpression . T.pack . show) Right entryGcProgram
  compileEntryUnit executableEntryName gcProgram

compileEntryUnit :: Text -> GcGrinProgram -> Either WasmError Text
compileEntryUnit entryName gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  updateLabel <- functionCodeLabel env (gcUpdateFunction gcProgram)
  updateArity <- functionArity env (gcUpdateFunction gcProgram)
  functions <- mapM (compileFunction env) (grinFunctions program)
  staticGlobals <- renderStaticGlobals env program
  let specialInfo label entry fields remaining next frameKind = RuntimeInfo label Nothing (Just entry) fields remaining next Nothing (Just frameKind) runtimeObjectClosure Nothing
      updateInfo label fields remaining next enter =
        RuntimeInfo
          label
          Nothing
          (Just (dataLabel label <> "_portable"))
          fields
          remaining
          next
          (Just (RuntimeAdapter updateLabel updateArity enter))
          (Just ContinuationFrameUpdate)
          runtimeObjectClosure
          Nothing
      specialInfos =
        [ specialInfo "aihc_wasm_final_info" "aihc_wasm_final_continuation" [] 1 (Just "aihc_wasm_final_applied_info") ContinuationFrameStop,
          specialInfo "aihc_wasm_final_applied_info" "aihc_wasm_final_continuation" [BoxedRep Lifted] 0 Nothing ContinuationFrameStop,
          specialInfo "aihc_wasm_top_info" "aihc_wasm_top_continuation" [BoxedRep Lifted] 1 (Just "aihc_wasm_top_applied_info") ContinuationFrameNormal,
          specialInfo "aihc_wasm_top_applied_info" "aihc_wasm_top_continuation" [BoxedRep Lifted, BoxedRep Lifted] 0 Nothing ContinuationFrameNormal,
          updateInfo "aihc_wasm_update_info" [BoxedRep Lifted, BoxedRep Lifted] 1 (Just "aihc_wasm_update_applied_info") (Just (RuntimeEnter 2 1 False)),
          updateInfo "aihc_wasm_update_applied_info" [BoxedRep Lifted, BoxedRep Lifted, BoxedRep Lifted] 0 Nothing Nothing,
          specialInfo "aihc_wasm_thread_done_info" "aihc_wasm_thread_done_continuation" [] 1 (Just "aihc_wasm_thread_done_applied_info") ContinuationFrameStop,
          specialInfo "aihc_wasm_thread_done_applied_info" "aihc_wasm_thread_done_continuation" [BoxedRep Lifted] 0 Nothing ContinuationFrameStop
        ]
      runtimeInfos = compileRuntimeInfos env <> specialInfos
      source =
        moduleHeader env program
          <> renderEntryAdapters runtimeInfos
          <> concatMap compiledFunctionLines functions
          <> renderSpecialFunctions
          <> renderProgramInitializer entryName
          <> renderRuntimeSymbols
          <> renderAddrLiterals env
          <> renderRuntimeInfos runtimeInfos
          <> staticGlobals
          <> renderStaticReferenceTables env
          <> renderScratch functions
          <> ["\t.no_dead_strip\t__indirect_function_table", ""]
  pure (T.unlines source)
  where
    program = gcGrinProgram gcProgram
    env = compileEnvironment EntryUnit gcProgram

compileModule :: GcGrinProgram -> Either WasmError Text
compileModule gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  functions <- mapM (compileFunction env) (grinFunctions program)
  staticGlobals <- renderStaticGlobals env program
  let source =
        moduleHeader env program
          <> renderEntryAdapters (compileRuntimeInfos env)
          <> concatMap compiledFunctionLines functions
          <> renderAddrLiterals env
          <> renderRuntimeInfos (compileRuntimeInfos env)
          <> staticGlobals
          <> renderStaticReferenceTables env
          <> renderScratch functions
          <> ["\t.no_dead_strip\t__indirect_function_table", ""]
  pure (T.unlines source)
  where
    program = gcGrinProgram gcProgram
    env = compileEnvironment LibraryUnit gcProgram

validateProgramPrimitives :: GrinProgram -> Either WasmError ()
validateProgramPrimitives = validatePrimitiveNames . map (grinVarName . fst) . grinPrimitives

validatePrimitiveNames :: [Text] -> Either WasmError ()
validatePrimitiveNames = mapM_ $ \name ->
  if name `elem` supportedNativePrimitiveNames
    then Right ()
    else Left (WasmUnsupportedPrimitive name)

compileEnvironment :: CompilationUnit -> GcGrinProgram -> CompileEnv
compileEnvironment unitKind gcProgram =
  CompileEnv
    { compileFunctionLabels = functionLabels,
      compileFunctionArities = functionArities,
      compileAddrLiteralLabels = Map.fromList [(bytes, "aihc_wasm_addr_" <> tshow index) | (index, (bytes, _)) <- zip [0 :: Int ..] (buildAddrLiteralPool program)],
      compileNodeInfoLabels = Map.fromList [(key, label) | (key, label, _) <- constructorEntries <> functionEntries],
      compileRuntimeInfos = map third (constructorEntries <> functionEntries),
      compileStaticReferences = staticReferences,
      compileSrtLabels = srtLabels,
      compileAllowUnsupportedPrimitives =
        case unitKind of
          EntryUnit -> False
          LibraryUnit -> True
    }
  where
    program = gcGrinProgram gcProgram
    constructorLayouts = grinConstructors program
    staticReferences = programStaticReferences program
    srtLabels =
      Map.fromList
        [ (name, "aihc_wasm_srt_" <> tshow index)
        | (index, name) <- zip [0 :: Int ..] (Map.keys (staticReferenceTables staticReferences))
        ]
    functionLabels =
      Map.fromList
        [ (grinFunctionName function, localFunctionLabel index function)
        | (index, function) <- zip [0 :: Int ..] (grinFunctions program)
        ]
    functionArities =
      Map.fromList
        [ (grinFunctionName function, length (grinFunctionParameters function))
        | function <- grinFunctions program
        ]
    constructorEntries =
      [ (key, label, RuntimeInfo label (Just (renderLinkedConstructorInfoSymbol name 0)) Nothing fields remaining next Nothing Nothing (runtimeInfoKeyObjectKind key) Nothing)
      | (name, layouts) <- constructorLayouts,
        let arity = length layouts,
        remaining <- [arity, arity - 1 .. 0],
        let key = ConstructorRuntimeInfo name remaining,
        let label = renderLinkedConstructorInfoSymbol name remaining
            fields = concat (take (arity - remaining) layouts)
            next = if remaining == 0 then Nothing else Just (renderLinkedConstructorInfoSymbol name (remaining - 1))
      ]
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
          RuntimeInfo
            label
            Nothing
            (Just (dataLabel label <> "_portable"))
            (runtimeInfoKeyFields key)
            (runtimeInfoKeyRemainingArity key)
            (runtimeInfoKeyNext key >>= (`Map.lookup` infoLabels))
            (Just (RuntimeAdapter target arity enter))
            (Map.lookup functionName (gcContinuationFrames gcProgram))
            (runtimeInfoKeyObjectKind key)
            (Map.lookup functionName srtLabels)
        )
      | (index, key) <- zip [0 :: Int ..] infoKeys,
        Just functionName <- [runtimeInfoFunctionName key],
        let label = "aihc_wasm_function_info_" <> tshow index
            target = functionLabels Map.! functionName
            arity = functionArities Map.! functionName
            passContinuation = functionName `Set.notMember` gcContinuationFunctions gcProgram
            enter = case key of
              ClosureRuntimeInfo _ fields [supplied] -> Just (RuntimeEnter (length fields) (length supplied) passContinuation)
              ThunkRuntimeInfo _ fields -> Just (RuntimeEnter (length fields) 0 passContinuation)
              _ -> Nothing
      ]
    third (_, _, value) = value

moduleHeader :: CompileEnv -> GrinProgram -> [Text]
moduleHeader env program =
  [ "# Direct GRIN-to-WebAssembly backend output. No C or LLVM IR is generated.",
    "\t.text"
  ]
    <> map renderFunctionType runtimeFunctionTypes
    <> [ renderFunctionType (label, (I32 : replicate arity I64, []))
       | (functionName, label) <- Map.toAscList (compileFunctionLabels env),
         let arity = Map.findWithDefault 0 functionName (compileFunctionArities env)
       ]
    <> [renderForeignType foreignCall | foreignCall <- grinForeignCalls program, grinForeignCallTarget foreignCall == GrinForeignFunction]
    <> [""]
  where
    renderFunctionType (name, (arguments, results)) =
      "\t.functype\t" <> name <> " (" <> T.intercalate ", " (map renderValueType arguments) <> ") -> (" <> T.intercalate ", " (map renderValueType results) <> ")"
    renderForeignType foreignCall =
      renderFunctionType
        ( grinForeignCallSymbol foreignCall,
          (map foreignValueType (grinForeignArgumentTypes signature), [foreignValueType (grinForeignResultType signature) | grinForeignResultType signature /= GrinForeignVoid])
        )
      where
        signature = grinForeignCallSignature foreignCall

data WasmValueType = I32 | I64

renderValueType :: WasmValueType -> Text
renderValueType I32 = "i32"
renderValueType I64 = "i64"

foreignValueType :: GrinForeignType -> WasmValueType
foreignValueType foreignType =
  case foreignType of
    GrinForeignInt -> I64
    GrinForeignInt8 -> I32
    GrinForeignInt16 -> I32
    GrinForeignInt32 -> I32
    GrinForeignInt64 -> I64
    GrinForeignWord -> I64
    GrinForeignWord8 -> I32
    GrinForeignWord16 -> I32
    GrinForeignWord32 -> I32
    GrinForeignWord64 -> I64
    GrinForeignAddr -> I32
    GrinForeignVoid -> I32

runtimeFunctionTypes :: [(Text, ([WasmValueType], [WasmValueType]))]
runtimeFunctionTypes =
  [ ("aihc_machine_new", ([I64], [I32])),
    ("aihc_wasm_make_node", ([I32, I32], [I64])),
    ("aihc_wasm_make_node_unchecked", ([I32, I32], [I64])),
    ("aihc_ensure_heap", ([I32, I64, I64, I32], [])),
    ("aihc_array_new", ([I32, I64, I64], [I32])),
    ("aihc_array_index", ([I32, I64], [I64])),
    ("aihc_array_write", ([I32, I64, I64], [I64])),
    ("aihc_array_same", ([I32, I32], [I64])),
    ("aihc_mutvar_new", ([I32, I64], [I32])),
    ("aihc_mutvar_read", ([I32], [I64])),
    ("aihc_mutvar_write", ([I32, I64], [I64])),
    ("aihc_int_to_int8", ([I64], [I64])),
    ("aihc_int8_to_int", ([I64], [I64])),
    ("aihc_int_to_int16", ([I64], [I64])),
    ("aihc_int16_to_int", ([I64], [I64])),
    ("aihc_int_to_int32", ([I64], [I64])),
    ("aihc_int32_to_int", ([I64], [I64])),
    ("aihc_int_to_int64", ([I64], [I64])),
    ("aihc_int64_to_int", ([I64], [I64])),
    ("aihc_float_negate", ([I64], [I64])),
    ("aihc_float_abs", ([I64], [I64])),
    ("aihc_int_to_float", ([I64], [I64])),
    ("aihc_float_to_int", ([I64], [I64])),
    ("aihc_double_negate", ([I64], [I64])),
    ("aihc_double_abs", ([I64], [I64])),
    ("aihc_int_to_double", ([I64], [I64])),
    ("aihc_double_to_int", ([I64], [I64])),
    ("aihc_float_plus", ([I64, I64], [I64])),
    ("aihc_float_minus", ([I64, I64], [I64])),
    ("aihc_float_times", ([I64, I64], [I64])),
    ("aihc_float_gt", ([I64, I64], [I64])),
    ("aihc_float_lt", ([I64, I64], [I64])),
    ("aihc_float_eq", ([I64, I64], [I64])),
    ("aihc_double_plus", ([I64, I64], [I64])),
    ("aihc_double_minus", ([I64, I64], [I64])),
    ("aihc_double_times", ([I64, I64], [I64])),
    ("aihc_double_gt", ([I64, I64], [I64])),
    ("aihc_double_lt", ([I64, I64], [I64])),
    ("aihc_double_eq", ([I64, I64], [I64])),
    ("aihc_float_to_double", ([I64], [I64])),
    ("aihc_double_to_float", ([I64], [I64])),
    ("aihc_word_byte_swap16", ([I64], [I64])),
    ("aihc_word_byte_swap32", ([I64], [I64])),
    ("aihc_word_byte_swap64", ([I64], [I64])),
    ("aihc_int_times2_high_needed", ([I64, I64], [I64])),
    ("aihc_int_times2_high", ([I64, I64], [I64])),
    ("aihc_int_times2_low", ([I64, I64], [I64])),
    ("aihc_byte_array_index_byte_word8", ([I32, I64], [I64])),
    ("aihc_byte_array_index_byte_word16", ([I32, I64], [I64])),
    ("aihc_byte_array_index_byte_word32", ([I32, I64], [I64])),
    ("aihc_byte_array_index_byte_word64", ([I32, I64], [I64])),
    ("aihc_mutvar_compare_and_swap", ([I32, I64, I64], [I64])),
    ("aihc_mutvar_same", ([I32, I32], [I64])),
    ("aihc_stable_name_make", ([I32, I32], [I32])),
    ("aihc_stable_name_equal", ([I32, I32], [I64])),
    ("aihc_stable_name_hash", ([I32], [I64])),
    ("aihc_wasm_set_field", ([I64, I64, I64], [])),
    ("aihc_wasm_update", ([I64, I64], [])),
    ("aihc_wasm_update_blackhole", ([I32, I64, I64], [])),
    ("aihc_wasm_global_get", ([I32, I64], [I64])),
    ("aihc_wasm_global_set", ([I32, I64, I64], [])),
    ("aihc_wasm_value_field", ([I64, I64], [I64])),
    ("aihc_wasm_value_info", ([I64], [I64])),
    ("aihc_addr_index_word8", ([I32, I64], [I64])),
    ("aihc_addr_index_word32", ([I32, I64], [I64])),
    ("aihc_addr_index_word64", ([I32, I64], [I64])),
    ("aihc_addr_index_word16", ([I32, I64], [I64])),
    ("aihc_addr_write_word8", ([I32, I64, I64], [I64])),
    ("aihc_addr_write_word16", ([I32, I64, I64], [I64])),
    ("aihc_addr_write_word32", ([I32, I64, I64], [I64])),
    ("aihc_addr_write_word64", ([I32, I64, I64], [I64])),
    ("aihc_addr_index_byte_word16", ([I32, I64], [I64])),
    ("aihc_addr_index_byte_word32", ([I32, I64], [I64])),
    ("aihc_addr_index_byte_word64", ([I32, I64], [I64])),
    ("aihc_addr_write_byte_word16", ([I32, I64, I64], [I64])),
    ("aihc_addr_write_byte_word32", ([I32, I64, I64], [I64])),
    ("aihc_addr_write_byte_word64", ([I32, I64, I64], [I64])),
    ("aihc_addr_plus", ([I32, I64], [I32])),
    ("aihc_addr_minus", ([I32, I32], [I64])),
    ("aihc_addr_eq", ([I32, I32], [I64])),
    ("aihc_addr_ne", ([I32, I32], [I64])),
    ("aihc_addr_lt", ([I32, I32], [I64])),
    ("aihc_addr_le", ([I32, I32], [I64])),
    ("aihc_addr_gt", ([I32, I32], [I64])),
    ("aihc_addr_ge", ([I32, I32], [I64])),
    ("aihc_addr_to_int", ([I32], [I64])),
    ("aihc_int_to_addr", ([I64], [I32])),
    ("aihc_addr_cstring_length", ([I32], [I64])),
    ("aihc_touch", ([I64], [I64])),
    ("aihc_word_to_word8", ([I64], [I64])),
    ("aihc_word_to_word16", ([I64], [I64])),
    ("aihc_word_to_word32", ([I64], [I64])),
    ("aihc_byte_array_new", ([I64], [I32])),
    ("aihc_byte_array_new_pinned", ([I64], [I32])),
    ("aihc_byte_array_new_aligned_pinned", ([I64, I64], [I32])),
    ("aihc_byte_array_is_pinned", ([I32], [I64])),
    ("aihc_byte_array_contents", ([I32], [I32])),
    ("aihc_byte_array_shrink", ([I32, I64], [I64])),
    ("aihc_byte_array_resize", ([I32, I64], [I32])),
    ("aihc_byte_array_get_size", ([I32], [I64])),
    ("aihc_byte_array_copy_from_addr", ([I32, I32, I64, I64], [I64])),
    ("aihc_byte_array_index_word", ([I32, I64], [I64])),
    ("aihc_byte_array_read_word", ([I32, I64], [I64])),
    ("aihc_byte_array_write_word", ([I32, I64, I64], [I64])),
    ("aihc_byte_array_copy", ([I32, I64, I32, I64, I64], [I64])),
    ("aihc_byte_array_copy_to_addr", ([I32, I64, I32, I64], [I64])),
    ("aihc_byte_array_compare", ([I32, I64, I32, I64, I64], [I64])),
    ("aihc_wasm_times_word2_high", ([I64, I64], [I64])),
    ("aihc_wasm_quot_rem_word2_quotient", ([I64, I64, I64], [I64])),
    ("aihc_wasm_transfer_direct", ([I32, I32, I64, I32], [])),
    ("aihc_wasm_transfer_eval", ([I32, I64, I64, I64, I64], [])),
    ("aihc_wasm_transfer_apply", ([I32, I64, I64, I32, I64], [])),
    ("aihc_wasm_transfer_continue", ([I32, I64, I64, I32], [])),
    ("aihc_wasm_transfer_raise", ([I32, I64, I64], [])),
    ("aihc_wasm_transfer_fork", ([I32, I64, I64], [])),
    ("aihc_wasm_transfer_yield", ([I32, I64], [])),
    ("aihc_wasm_transfer_await_io", ([I32, I64, I64], [])),
    ("aihc_wasm_transfer_new_mvar", ([I32, I64], [])),
    ("aihc_wasm_transfer_read_mvar", ([I32, I64, I64], [])),
    ("aihc_wasm_transfer_take_mvar", ([I32, I64, I64], [])),
    ("aihc_wasm_transfer_put_mvar", ([I32, I64, I64, I64], [])),
    ("aihc_wasm_transfer_thread_done", ([I32], [])),
    ("aihc_wasm_transfer_halt", ([I32], [])),
    ("aihc_set_exit_status", ([I32, I64], [])),
    ("aihc_wasm_transfer_start", ([I32, I64, I64, I64, I64, I32], [])),
    ("aihc_set_thread_done_continuation", ([I32, I32], [])),
    ("aihc_no_match", ([], [])),
    ("aihc_unsupported_primitive", ([], []))
  ]

compileFunction :: CompileEnv -> GrinFunction -> Either WasmError CompiledFunction
compileFunction env function = do
  label <- functionCodeLabel env (grinFunctionName function)
  let parameters = grinFunctionParameters function
      parameterCount = length parameters
      caseLocal = parameterCount + 1
      valueLocals = functionValueLocals function
      localCount = maximum (caseLocal : Map.elems valueLocals) - parameterCount
      scratchCount = maximumScratchSlots (grinFunctionBody function)
      valueEnv = ValueEnv env valueLocals caseLocal
  body <- compileExpr valueEnv (grinFunctionBody function)
  pure
    CompiledFunction
      { compiledFunctionScratchSlots = scratchCount,
        compiledFunctionLines =
          functionStartWithParameters label (I32 : replicate parameterCount I64) (replicate localCount I64)
            <> indent (storeCurrentSrt (Map.lookup (grinFunctionName function) (compileSrtLabels env)) <> body)
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
            <> materializeValue env value
            <> i64Const (boolInteger (isLiftedRuntimeRep runtimeRep))
            <> materializeValue env continuation
            <> materializeValue env updateContinuation
            <> call "aihc_wasm_transfer_eval"
        )
    GrinCall _ functionName values -> do
      target <- functionCodeLabel (valueCompileEnv env) functionName
      pure (machine <> concatMap (materializeValue env) values <> ["return_call\t" <> target])
    GrinCpsPrimitiveCall _ name values continuation -> compileCpsPrimitive env name values continuation
    GrinCpsApply _ function values continuation -> do
      scratch <- storeScratchValues env values
      pure
        ( terminal
            ( machine
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
                <> materializeValue env continuation
                <> i64Const (tshow (length values))
                <> scratch
                <> call "aihc_wasm_transfer_continue"
            )
        )
    GrinCpsRaise exception continuation ->
      pure
        ( terminal
            ( machine
                <> materializeValue env exception
                <> materializeValue env continuation
                <> call "aihc_wasm_transfer_raise"
            )
        )
    GrinHalt _ -> pure (terminal (machine <> call "aihc_wasm_transfer_halt"))
    GrinExit status ->
      pure . terminal $
        machine
          <> materializeValue env status
          <> call "aihc_set_exit_status"
          <> machine
          <> call "aihc_wasm_transfer_halt"
    GrinCase scrutinee binder alternatives -> compileCase env scrutinee binder alternatives
    GrinConstant {} -> unsupported "direct-style constant return after CPS"
    GrinStore {} -> unsupported "direct-style store return after CPS"
    GrinEnsureHeap {} -> unsupported "unbound heap reservation"
    GrinStoreUnchecked {} -> unsupported "unbound unchecked store"
    GrinUpdate {} -> unsupported "direct-style update return after CPS"
    GrinUpdateBlackhole {} -> unsupported "unbound blackhole update"
    GrinEval {} -> unsupported "direct-style eval after CPS"
    GrinPrimitiveCall {} -> unsupported "unbound primitive call after CPS"
    GrinApply {} -> unsupported "direct-style apply after CPS"
    GrinThrow {} -> unsupported "direct-style throw after CPS"
    GrinCatch {} -> unsupported "direct-style catch after CPS"
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
    ("newMVar#", []) -> transfer "aihc_wasm_transfer_new_mvar" [continuation]
    ("readMVar#", [mvar]) -> transfer "aihc_wasm_transfer_read_mvar" [mvar, continuation]
    ("takeMVar#", [mvar]) -> transfer "aihc_wasm_transfer_take_mvar" [mvar, continuation]
    ("putMVar#", [mvar, value]) -> transfer "aihc_wasm_transfer_put_mvar" [mvar, value, continuation]
    ("yield#", []) -> transfer "aihc_wasm_transfer_yield" [continuation]
    _
      | compileAllowUnsupportedPrimitives (valueCompileEnv env) -> Right (call "aihc_unsupported_primitive" <> ["return"])
    _ -> Left (WasmUnsupportedExpression ("CPS primitive call " <> name))
  where
    transfer function arguments =
      Right (machine <> concatMap (materializeValue env) arguments <> call function <> ["return"])

compileDirectBinding :: ValueEnv -> [GrinVar] -> GrinExpr -> Either WasmError Instructions
compileDirectBinding env vars expression =
  case expression of
    GrinConstant values
      | length vars == length values -> pure (concat [localSetFor env var (materializeValue env value) | (var, value) <- zip vars values])
    GrinStore node -> storeNode False node
    GrinEnsureHeap requiredWords roots
      | length vars == length roots -> do
          let stores = concat [storeScratch index (materializeValue env root) | (index, root) <- zip [0 :: Int ..] roots]
              reloads = concat [localSetFor env var (loadScratch index) | (index, var) <- zip [0 :: Int ..] vars]
              rootsAddress = if null roots then i32Const "0" else scratchAddress
          pure (stores <> machine <> materializeValue env requiredWords <> i64Const (tshow (length roots)) <> rootsAddress <> call "aihc_ensure_heap" <> reloads)
    GrinStoreUnchecked node -> storeNode True node
    GrinUpdate pointer value -> update "aihc_wasm_update" False pointer value
    GrinUpdateBlackhole pointer value -> update "aihc_wasm_update_blackhole" True pointer value
    GrinPrimitiveCall IntRep name [left, right]
      | Just operator <- lookup name [("+#", "i64.add"), ("-#", "i64.sub"), ("*#", "i64.mul")] ->
          binaryPrimitive operator left right
    GrinPrimitiveCall WordRep name [left, right]
      | Just operator <-
          lookup
            name
            [ ("plusWord#", "i64.add"),
              ("minusWord#", "i64.sub"),
              ("timesWord#", "i64.mul"),
              ("quotWord#", "i64.div_u"),
              ("remWord#", "i64.rem_u"),
              ("and#", "i64.and"),
              ("or#", "i64.or"),
              ("xor#", "i64.xor")
            ] ->
          binaryPrimitive operator left right
    GrinPrimitiveCall _ name [left, right]
      | Just operator <-
          lookup
            name
            [ ("<#", "i64.lt_s"),
              ("==#", "i64.eq"),
              (">#", "i64.gt_s"),
              (">=#", "i64.ge_s"),
              ("<=#", "i64.le_s"),
              ("/=#", "i64.ne"),
              ("eqWord#", "i64.eq"),
              ("neWord#", "i64.ne"),
              ("ltWord#", "i64.lt_u"),
              ("leWord#", "i64.le_u"),
              ("gtWord#", "i64.gt_u"),
              ("geWord#", "i64.ge_u"),
              ("eqWord64#", "i64.eq"),
              ("neWord64#", "i64.ne"),
              ("ltWord64#", "i64.lt_u"),
              ("leWord64#", "i64.le_u"),
              ("gtWord64#", "i64.gt_u"),
              ("geWord64#", "i64.ge_u")
            ] ->
          comparisonPrimitive operator left right
    GrinPrimitiveCall _ name [left, right]
      | name `elem` ["addIntC#", "subIntC#", "addWordC#", "subWordC#"] ->
          carryPrimitive name left right
    GrinPrimitiveCall _ "timesWord2#" [left, right] ->
      storePair
        (materializeValue env left <> materializeValue env right <> call "aihc_wasm_times_word2_high")
        (materializeValue env left <> materializeValue env right <> ["i64.mul"])
    GrinPrimitiveCall _ "quotRemWord#" [left, right] ->
      storePair
        (materializeValue env left <> materializeValue env right <> ["i64.div_u"])
        (materializeValue env left <> materializeValue env right <> ["i64.rem_u"])
    GrinPrimitiveCall _ "quotRemWord2#" [high, low, divisor] ->
      case vars of
        [quotient, remainder] ->
          pure
            ( localSetFor
                env
                quotient
                (materializeValue env high <> materializeValue env low <> materializeValue env divisor <> call "aihc_wasm_quot_rem_word2_quotient")
                <> localSetFor
                  env
                  remainder
                  (materializeValue env low <> localGet env quotient <> materializeValue env divisor <> ["i64.mul", "i64.sub"])
            )
        _ -> Left (WasmUnsupportedExpression "direct expression pair result arity")
    GrinPrimitiveCall _ "not#" [value] -> storeSingle (materializeValue env value <> i64Const "-1" <> ["i64.xor"])
    GrinPrimitiveCall _ name [value, amount]
      | Just operator <- lookup name [("uncheckedShiftL#", "i64.shl"), ("uncheckedShiftRL#", "i64.shr_u")] ->
          binaryPrimitive operator value amount
    GrinPrimitiveCall _ name [value]
      | name `elem` ["int2Word#", "word2Int#", "word8ToWord#", "word32ToWord#", "word64ToWord#", "wordToWord64#", "word16ToWord#"] -> storeSingle (materializeValue env value)
    GrinPrimitiveCall _ name [value]
      | Just operator <- lookup name [("clz#", "i64.clz"), ("ctz#", "i64.ctz"), ("popCnt#", "i64.popcnt")] ->
          storeSingle (materializeValue env value <> [operator])
    GrinPrimitiveCall IntRep "compareInt#" [left, right] ->
      storeSingle
        ( materializeValue env left
            <> materializeValue env right
            <> ["i64.gt_s"]
            <> materializeValue env left
            <> materializeValue env right
            <> ["i64.lt_s", "i32.sub", "i64.extend_i32_s"]
        )
    GrinPrimitiveCall _ "nullAddr#" [] -> storeSingle (i64Const "0")
    GrinPrimitiveCall runtimeRep "realWorld#" []
      | null vars && null (runtimeRepComponents runtimeRep) -> pure []
    GrinPrimitiveCall _ name [value]
      | name `elem` ["ord#", "chr#", "unsafeFreezeArray#", "unsafeThawArray#", "unsafeFreezeByteArray#", "unsafeThawByteArray#", "castFloatToWord32#", "castWord32ToFloat#", "castDoubleToWord64#", "castWord64ToDouble#"] -> storeSingle (materializeValue env value)
    GrinPrimitiveCall _ "newArray#" [size, initial] ->
      storeSingle
        ( machine
            <> materializeValue env size
            <> materializeValue env initial
            <> call "aihc_array_new"
            <> ["i64.extend_i32_u"]
        )
    GrinPrimitiveCall _ "newMutVar#" [initial] ->
      storeSingle
        ( machine
            <> materializeValue env initial
            <> call "aihc_mutvar_new"
            <> ["i64.extend_i32_u"]
        )
    GrinPrimitiveCall _ "makeStableName#" [value] ->
      storeSingle
        ( machine
            <> materializeValue env value
            <> ["i32.wrap_i64"]
            <> call "aihc_stable_name_make"
            <> ["i64.extend_i32_u"]
        )
    GrinPrimitiveCall _ "casMutVar#" [reference, expected, replacement]
      | Just swapCall <- nativeRuntimePrimitiveCall "casMutVar#",
        Just readCall <- nativeRuntimePrimitiveCall "readMutVar#" -> do
          swapInstructions <- compileForeignCall env (nativeRuntimeCallForeignCall swapCall) [reference, expected, replacement]
          readInstructions <- compileForeignCall env (nativeRuntimeCallForeignCall readCall) [reference]
          storePair swapInstructions readInstructions
    GrinPrimitiveCall _ name arguments
      | Just splitCalls <- nativeSplitRuntimePrimitiveCall name,
        length splitCalls == length vars ->
          concat
            <$> mapM
              ( \(var, splitCall) -> do
                  instructions <- compileForeignCall env (nativeRuntimeCallForeignCall splitCall) arguments
                  pure (localSetFor env var instructions)
              )
              (zip vars splitCalls)
    GrinPrimitiveCall _ name arguments
      | Just runtimeCall <- nativeRuntimePrimitiveCall name -> do
          instructions <- compileForeignCall env (nativeRuntimeCallForeignCall runtimeCall) arguments
          case vars of
            [] -> pure (instructions <> ["drop"])
            [_] -> storeSingle instructions
            _ -> Left (WasmUnsupportedExpression ("byte array primitive result arity " <> name))
    GrinPrimitiveCall {}
      | compileAllowUnsupportedPrimitives (valueCompileEnv env) ->
          pure (call "aihc_unsupported_primitive" <> concatMap (\var -> localSetFor env var (i64Const "0")) vars)
    GrinPrimitiveCall _ name _ -> Left (WasmUnsupportedExpression ("primitive call " <> name))
    GrinForeignCallExpr foreignCall arguments
      | null vars -> compileForeignCall env foreignCall arguments
      | otherwise -> compileForeignCall env foreignCall arguments >>= storeSingle
    _ -> Left (WasmUnsupportedExpression "non-direct expression remained in a CPS bind")
  where
    storeSingle instructions = case vars of
      [var] -> pure (localSetFor env var instructions)
      _ -> Left (WasmUnsupportedExpression "direct expression result arity")
    storePair firstInstructions secondInstructions = case vars of
      [first, second] -> pure (localSetFor env first firstInstructions <> localSetFor env second secondInstructions)
      _ -> Left (WasmUnsupportedExpression "direct expression pair result arity")
    binaryPrimitive operator left right = storeSingle (materializeValue env left <> materializeValue env right <> [operator])
    comparisonPrimitive operator left right =
      storeSingle (materializeValue env left <> materializeValue env right <> [operator, "i64.extend_i32_u"])
    carryPrimitive name left right = case vars of
      [result, carry] ->
        pure
          ( localSetFor env result (materializeValue env left <> materializeValue env right <> [if name `elem` ["addIntC#", "addWordC#"] then "i64.add" else "i64.sub"])
              <> localSetFor env carry (carryInstructions result)
          )
      _ -> Left (WasmUnsupportedExpression "direct expression pair result arity")
      where
        carryInstructions result = case name of
          "addIntC#" ->
            localGet env result
              <> materializeValue env left
              <> ["i64.xor"]
              <> localGet env result
              <> materializeValue env right
              <> ["i64.xor", "i64.and"]
              <> i64Const "63"
              <> ["i64.shr_u"]
          "subIntC#" ->
            materializeValue env left
              <> materializeValue env right
              <> ["i64.xor"]
              <> materializeValue env left
              <> localGet env result
              <> ["i64.xor", "i64.and"]
              <> i64Const "63"
              <> ["i64.shr_u"]
          "addWordC#" -> localGet env result <> materializeValue env left <> ["i64.lt_u", "i64.extend_i32_u"]
          _ -> materializeValue env left <> materializeValue env right <> ["i64.lt_u", "i64.extend_i32_u"]
    storeNode unchecked node = case vars of
      [var] -> allocateNodeInto env unchecked var node
      _ -> Left (WasmUnsupportedExpression "node result arity")
    update function passMachine pointer value =
      storeSingle
        ((if passMachine then machine else []) <> materializeValue env pointer <> materializeValue env value <> call function <> materializeValue env value)

compileForeignCall :: ValueEnv -> GrinForeignCall -> [GrinValue] -> Either WasmError Instructions
compileForeignCall _ foreignCall _
  -- Taking the address of a static C symbol has no wasm lowering yet.
  | GrinForeignAddress <- grinForeignCallTarget foreignCall =
      Left (WasmUnsupportedExpression "address foreign import")
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
    -- GRIN keeps narrow integers extended to 64 bits, so their low 32 bits
    -- are the extended C argument.
    foreignArgumentInstructions kind value =
      materializeValue env value <> case foreignValueType kind of
        I64 -> []
        I32 -> ["i32.wrap_i64"]
    -- A narrow C result may carry unspecified high bits, so it is extended
    -- from its own width.
    foreignResultInstructions kind = case kind of
      GrinForeignInt -> []
      GrinForeignInt8 -> ["i32.extend8_s", "i64.extend_i32_s"]
      GrinForeignInt16 -> ["i32.extend16_s", "i64.extend_i32_s"]
      GrinForeignInt32 -> ["i64.extend_i32_s"]
      GrinForeignInt64 -> []
      GrinForeignWord -> []
      GrinForeignWord8 -> ["i32.const 255", "i32.and", "i64.extend_i32_u"]
      GrinForeignWord16 -> ["i32.const 65535", "i32.and", "i64.extend_i32_u"]
      GrinForeignWord32 -> ["i64.extend_i32_u"]
      GrinForeignWord64 -> []
      GrinForeignAddr -> ["i64.extend_i32_u"]
      GrinForeignVoid -> []

compileCase :: ValueEnv -> GrinValue -> GrinVar -> [GrinAlt] -> Either WasmError Instructions
compileCase env scrutinee binder alternatives = do
  choices <- compileChoices alternatives
  pure (materializeValue env scrutinee <> ["local.set\t" <> caseLocal] <> localSetFor env binder ["local.get\t" <> caseLocal] <> choices)
  where
    caseLocal = tshow (valueCaseLocal env)
    pointer = isPointerRuntimeRep (grinValueRuntimeRep scrutinee)
    compileChoices [] = pure (call "aihc_no_match" <> ["unreachable"])
    compileChoices (alternative : rest) = case grinAltCon alternative of
      GrinDefaultAlt -> compileAlternative alternative
      constructor -> do
        condition <- case constructor of
          GrinDataAlt name
            | pointer -> do
                pure
                  ( ["local.get\t" <> caseLocal]
                      <> call "aihc_wasm_value_info"
                      <> i32Const (renderLinkedConstructorInfoSymbol name 0)
                      <> ["i64.extend_i32_u", "i64.eq"]
                  )
          GrinDataAlt {} -> Left (WasmUnsupportedExpression "constructor case on unboxed value")
          GrinLitAlt literal
            | pointer -> Left (WasmUnsupportedExpression "literal case on lifted value")
            | otherwise -> case normalizedLiteralInteger literal of
                Nothing -> Left (WasmUnsupportedValue "string case alternative")
                Just integer -> pure (["local.get\t" <> caseLocal] <> i64Const (renderInteger integer) <> ["i64.eq"])
        accepted <- compileAlternative alternative
        rejected <- compileChoices rest
        pure (condition <> ["if"] <> indent accepted <> ["else"] <> indent rejected <> ["end_if"])
    compileAlternative alternative = do
      bindings <- case grinAltCon alternative of
        GrinDataAlt _ -> fmap concat . forM (zip [0 :: Int ..] (grinAltBinders alternative)) $ \(index, var) ->
          pure (localSetFor env var (["local.get\t" <> caseLocal] <> i64Const (tshow index) <> call "aihc_wasm_value_field"))
        GrinLitAlt _ -> pure []
        GrinDefaultAlt -> pure (concatMap (\var -> localSetFor env var ["local.get\t" <> caseLocal]) (grinAltBinders alternative))
      (bindings <>) <$> compileExpr env (grinAltRhs alternative)

allocateNodeInto :: ValueEnv -> Bool -> GrinVar -> GrinNode -> Either WasmError Instructions
allocateNodeInto env unchecked var node = do
  info <- nodeHeader (valueCompileEnv env) node
  let allocation = machine <> i32RuntimeData info <> call (if unchecked then "aihc_wasm_make_node_unchecked" else "aihc_wasm_make_node")
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
    case Map.lookup var (valueLocals env) of
      Just wasmLocal -> getWasmLocal wasmLocal
      Nothing -> linkedGlobalValue (grinVarName var)
  GrinGlobalValue name -> linkedGlobalValue name
  GrinLitValue literal -> case literal of
    GrinLitAddr bytes -> case Map.lookup bytes (compileAddrLiteralLabels (valueCompileEnv env)) of
      Just label -> i32Data label <> ["i64.extend_i32_u"]
      Nothing -> i64Const "0"
    _ -> maybe (i64Const "0") (i64Const . renderInteger) (normalizedLiteralInteger literal)
  where
    linkedGlobalValue name = i32Const (renderLinkedGlobalSymbol name) <> ["i64.extend_i32_u"]

storeScratchValues :: ValueEnv -> [GrinValue] -> Either WasmError Instructions
storeScratchValues env values =
  pure
    ( concat [storeScratch index (materializeValue env value) | (index, value) <- zip [0 :: Int ..] values]
        <> case values of
          [] -> i32Const "0"
          _ -> scratchAddress
    )

localGet :: ValueEnv -> GrinVar -> Instructions
localGet env var = maybe (i64Const "0") getWasmLocal (Map.lookup var (valueLocals env))

getWasmLocal :: Int -> Instructions
getWasmLocal wasmLocal = ["local.get\t" <> tshow wasmLocal]

localSetFor :: ValueEnv -> GrinVar -> Instructions -> Instructions
localSetFor env var value = maybe [] (`setWasmLocal` value) (Map.lookup var (valueLocals env))

setWasmLocal :: Int -> Instructions -> Instructions
setWasmLocal wasmLocal value = value <> ["local.set\t" <> tshow wasmLocal]

loadScratch :: Int -> Instructions
loadScratch = loadSlot scratchAddress

storeScratch :: Int -> Instructions -> Instructions
storeScratch = storeSlot scratchAddress

scratchAddress :: Instructions
scratchAddress = i32Data "aihc_wasm_scratch"

loadSlot :: Instructions -> Int -> Instructions
loadSlot address index = address <> ["i64.load\t" <> tshow (index * 8)]

storeSlot :: Instructions -> Int -> Instructions -> Instructions
storeSlot address index value = address <> value <> ["i64.store\t" <> tshow (index * 8)]

machine :: Instructions
machine = ["local.get\t0"]

i32Const, i64Const, i32Data, i32RuntimeData, call :: Text -> Instructions
i32Const value = ["i32.const\t" <> value]
i64Const value = ["i64.const\t" <> value]
i32Data = i32Const . dataLabel
i32RuntimeData = i32Const . runtimeDataLabel
call function = ["call\t" <> function]

functionStart :: Text -> [WasmValueType] -> [Text]
functionStart label = functionStartWithParameters label [I32]

functionStartWithParameters :: Text -> [WasmValueType] -> [WasmValueType] -> [Text]
functionStartWithParameters label parameterTypes localTypes =
  [ "\t.section\t.text." <> label <> ",\"\",@",
    "\t.type\t" <> label <> ",@function"
  ]
    <> functionVisibility label
    <> [ label <> ":",
         "\t.functype\t" <> label <> " (" <> T.intercalate ", " (map renderValueType parameterTypes) <> ") -> ()"
       ]
    <> ["\t.local\t" <> T.intercalate ", " (map renderValueType localTypes) | not (null localTypes)]

functionStartNoArguments :: Text -> [WasmValueType] -> [Text]
functionStartNoArguments label localTypes =
  [ "\t.section\t.text." <> label <> ",\"\",@",
    "\t.type\t" <> label <> ",@function"
  ]
    <> functionVisibility label
    <> [ label <> ":",
         "\t.functype\t" <> label <> " () -> ()"
       ]
    <> ["\t.local\t" <> T.intercalate ", " (map renderValueType localTypes) | not (null localTypes)]

functionEnd :: [Text]
functionEnd = ["\tend_function", ""]

functionVisibility :: Text -> [Text]
functionVisibility label
  | ".L" `T.isPrefixOf` label = []
  | otherwise = ["\t.hidden\t" <> label, "\t.globl\t" <> label]

renderSpecialFunctions :: [Text]
renderSpecialFunctions =
  functionStart "aihc_wasm_top_continuation" []
    <> indent
      ( ["i32.const\t0", "i32.load\taihc_machine"]
          <> loadSlot ["local.get\t0"] 1
          <> i64Const "0"
          <> i32Const "0"
          <> loadSlot ["local.get\t0"] 0
          <> call "aihc_wasm_transfer_apply"
          <> ["return"]
      )
    <> functionEnd
    <> functionStart "aihc_wasm_thread_done_continuation" []
    <> indent (["i32.const\t0", "i32.load\taihc_machine"] <> call "aihc_wasm_transfer_thread_done" <> ["return"])
    <> functionEnd
    <> functionStart "aihc_wasm_final_continuation" []
    <> indent (["i32.const\t0", "i32.load\taihc_machine"] <> call "aihc_wasm_transfer_halt" <> ["return"])
    <> functionEnd
    <> functionStart "aihc_wasm_exit" []
    <> indent (["i32.const\t0", "i32.load\taihc_machine"] <> i32Const "0" <> i64Const "0" <> i32Const "0" <> call "aihc_wasm_transfer_direct" <> ["return"])
    <> functionEnd

renderProgramInitializer :: Text -> [Text]
renderProgramInitializer entryName =
  functionStartNoArguments "aihc_wasm_program_initialize" [I32, I64, I64, I64, I64, I64]
    <> indent
      ( i64Const "0"
          <> call "aihc_machine_new"
          <> ["local.set\t0", "i32.const\t0", "local.get\t0", "i32.store\taihc_machine"]
          <> initializeSpecials
          <> ["local.get\t0"]
          <> i32Const (renderLinkedGlobalSymbol entryName)
          <> ["i64.extend_i32_u"]
          <> specialGet 1
          <> specialGet 2
          <> specialGet 3
          <> i32Const "aihc_wasm_exit"
          <> call "aihc_wasm_transfer_start"
          <> ["return"]
      )
    <> functionEnd
  where
    specialSet :: Int -> Instructions -> Instructions
    specialSet slot value = value <> ["local.set\t" <> tshow (slot + 2)]
    specialGet :: Int -> Instructions
    specialGet slot = ["local.get\t" <> tshow (slot + 2)]
    makeSpecial info = ["local.get\t0"] <> i32RuntimeData info <> call "aihc_wasm_make_node"
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
        <> specialGet 1
        <> call "aihc_wasm_set_field"
        <> specialGet 2
        <> i64Const "1"
        <> i32Const (renderLinkedGlobalSymbol entryName)
        <> ["i64.extend_i32_u"]
        <> call "aihc_wasm_set_field"
        <> specialSet 3 (makeSpecial "aihc_wasm_thread_done_info")
        <> ["local.get\t0"]
        <> specialGet 3
        <> ["i32.wrap_i64"]
        <> call "aihc_set_thread_done_continuation"

renderStaticGlobals :: CompileEnv -> GrinProgram -> Either WasmError [Text]
renderStaticGlobals env program = fmap concat (mapM renderGlobal (programStaticObjects program))
  where
    renderGlobal object = do
      let node = staticObjectNode object
      info <- nodeHeader env node
      fields <- mapM renderStaticValue (grinNodeFields node)
      let symbol = renderLinkedGlobalSymbol (staticObjectName object)
          payload = if null fields && isThunk node then ["\t.int64\t0"] else fields
          size = (1 + length payload) * 8
      pure
        ( [ "\t.type\t" <> symbol <> ",@object",
            "\t.section\t.data." <> symbol <> ",\"\",@",
            "\t.p2align\t3, 0x0",
            "\t.hidden\t" <> symbol,
            "\t.globl\t" <> symbol,
            symbol <> ":",
            "\t.int32\t" <> runtimeDataLabel info,
            "\t.int32\t0"
          ]
            <> payload
            <> ["\t.size\t" <> symbol <> ", " <> tshow size]
            <> [""]
        )
    renderStaticValue value =
      case value of
        GrinVarValue var -> pure (linkedAddress (grinVarName var))
        GrinGlobalValue name -> pure (linkedAddress name)
        GrinLitValue literal ->
          case literal of
            GrinLitAddr bytes ->
              maybe
                (Left (WasmUnsupportedValue "unregistered initializer address"))
                (pure . (<> "\n\t.int32\t0") . ("\t.int32\t" <>) . dataLabel)
                (Map.lookup bytes (compileAddrLiteralLabels env))
            _ -> maybe (Left (WasmUnsupportedValue "unsupported initializer literal")) (pure . ("\t.int64\t" <>) . renderInteger) (normalizedLiteralInteger literal)
    linkedAddress name = "\t.int32\t" <> renderLinkedGlobalSymbol name <> "\n\t.int32\t0"
    isThunk node = case grinNodeTag node of GrinThunk {} -> True; _ -> False

-- | Render one record per non-empty static reference table: the collector's
-- walk link, the two counts, then the static objects followed by the tables of
-- the directly called functions. The link is mutable, so records live in a
-- writable data section rather than beside the read-only info tables.
renderStaticReferenceTables :: CompileEnv -> [Text]
renderStaticReferenceTables env =
  concatMap renderTable (Map.toList (staticReferenceTables (compileStaticReferences env)))
  where
    renderTable (name, table) =
      case Map.lookup name (compileSrtLabels env) of
        Nothing -> []
        Just label ->
          [ "\t.type\t" <> label <> ",@object",
            "\t.section\t.data." <> label <> ",\"\",@",
            "\t.p2align\t2, 0x0",
            label <> ":",
            "\t.int32\t0",
            "\t.int32\t" <> tshow (length (srtObjects table)),
            "\t.int32\t" <> tshow (length (srtChildren table))
          ]
            <> entries
            <> [ "\t.size\t" <> label <> ", " <> tshow ((3 + length entries) * 4),
                 ""
               ]
      where
        entries =
          ["\t.int32\t" <> renderLinkedGlobalSymbol object | object <- srtObjects table]
            <> [ "\t.int32\t" <> childLabel
               | child <- srtChildren table,
                 Just childLabel <- [Map.lookup child (compileSrtLabels env)]
               ]

-- | Publish one function's static reference table as the machine's current
-- table. A collection can happen anywhere inside a function - at one of its
-- own safepoints or inside a runtime helper it called - and the running
-- function has no heap object of its own to carry the table, so it stores the
-- table on entry. Functions without a table store null rather than leaving a
-- table behind from a function that has already transferred control away.
storeCurrentSrt :: Maybe Text -> Instructions
storeCurrentSrt label =
  i32Const "0" <> i32Const (fromMaybe "0" label) <> ["i32.store\taihc_current_srt"]

renderScratch :: [CompiledFunction] -> [Text]
renderScratch functions =
  [ "\t.type\t" <> dataLabel scratchLabel <> ",@object",
    "\t.section\t.bss." <> dataLabel scratchLabel <> ",\"\",@",
    "\t.p2align\t3, 0x0",
    dataLabel scratchLabel <> ":",
    "\t.skip\t" <> tshow size,
    "\t.size\t" <> dataLabel scratchLabel <> ", " <> tshow size,
    ""
  ]
  where
    size = max 1 (maximum (0 : map compiledFunctionScratchSlots functions)) * 8
    scratchLabel = "aihc_wasm_scratch"

renderRuntimeSymbols :: [Text]
renderRuntimeSymbols =
  [ "\t.hidden\taihc_machine",
    "\t.hidden\taihc_next_transfer",
    ""
  ]

renderAddrLiterals :: CompileEnv -> [Text]
renderAddrLiterals env = concatMap render (Map.toAscList (compileAddrLiteralLabels env))
  where
    render (bytes, label) =
      objectStart label 0
        <> map (("\t.int8\t" <>) . tshow) (BS.unpack bytes <> [0])
        <> ["\t.size\t" <> dataLabel label <> ", " <> tshow (BS.length bytes + 1), ""]

-- Runtime dispatch still needs one uniform function-pointer type. The portable
-- adapter preserves the existing scheduler ABI, while the object adapter reads
-- captured fields directly from the closure or thunk and enters typed code.
renderEntryAdapters :: [RuntimeInfo] -> [Text]
renderEntryAdapters = concatMap renderAdapters
  where
    renderAdapters info = case runtimeInfoAdapter info of
      Nothing -> []
      Just adapter -> renderPortable info adapter <> maybe [] (renderObject info adapter) (runtimeAdapterEnter adapter)
    renderPortable info adapter =
      functionStart (portableEntryLabel info) []
        <> indent
          ( ["i32.const\t0", "i32.load\taihc_machine"]
              <> concatMap (loadSlot ["local.get\t0"]) [0 .. runtimeAdapterArity adapter - 1]
              <> ["return_call\t" <> runtimeAdapterTarget adapter]
          )
        <> functionEnd
    renderObject info adapter enter =
      functionStartWithParameters (objectEntryLabel info) [I32, I64, I32, I64] []
        <> indent
          ( ["local.get\t0"]
              <> concatMap loadStored [0 .. runtimeEnterStoredCount enter - 1]
              <> concatMap (loadSlot ["local.get\t2"]) [0 .. runtimeEnterSuppliedCount enter - 1]
              <> ["local.get\t3" | runtimeEnterPassContinuation enter]
              <> ["return_call\t" <> runtimeAdapterTarget adapter]
          )
        <> functionEnd
    loadStored index =
      [ "local.get\t1",
        "i32.wrap_i64",
        "i64.load\t" <> tshow ((index + 1) * 8)
      ]

portableEntryLabel :: RuntimeInfo -> Text
portableEntryLabel info = dataLabel (runtimeInfoLabel info) <> "_portable"

objectEntryLabel :: RuntimeInfo -> Text
objectEntryLabel info = dataLabel (runtimeInfoLabel info) <> "_enter"

renderRuntimeInfos :: [RuntimeInfo] -> [Text]
renderRuntimeInfos infos = concatMap renderBitmap infos <> concatMap renderInfo infos
  where
    renderBitmap info
      | null (runtimeInfoFields info) = []
      | otherwise =
          objectStart (runtimeInfoLabel info <> "_bitmap") 0
            <> ["\t.int8\t" <> if isPointerRuntimeRep field then "1" else "0" | field <- runtimeInfoFields info]
            <> [ "\t.size\t" <> runtimeDataLabel (runtimeInfoLabel info <> "_bitmap") <> ", " <> tshow (length (runtimeInfoFields info)),
                 ""
               ]
    renderInfo info =
      objectStart (runtimeInfoLabel info) 3
        <> [ "\t.int32\t" <> fromMaybe "0" (runtimeInfoIdentity info),
             "\t.int32\t" <> fromMaybe "0" (runtimeInfoEntry info),
             "\t.int64\t" <> tshow (length (runtimeInfoFields info)),
             "\t.int64\t" <> tshow (runtimeInfoRemainingArity info),
             "\t.int32\t" <> if null (runtimeInfoFields info) then "0" else runtimeDataLabel (runtimeInfoLabel info <> "_bitmap"),
             "\t.int32\t" <> maybe "0" runtimeDataLabel (runtimeInfoNext info),
             "\t.int32\t" <> maybe "0" (const (objectEntryLabel info)) (runtimeInfoAdapter info >>= runtimeAdapterEnter),
             "\t.skip\t4",
             "\t.int64\t" <> tshow (continuationFrameKindCode (runtimeInfoFrameKind info)),
             "\t.int64\t" <> tshow (runtimeInfoObjectKind info),
             "\t.int32\t" <> fromMaybe "0" (runtimeInfoSrt info),
             "\t.skip\t4",
             "\t.size\t" <> runtimeDataLabel (runtimeInfoLabel info) <> ", 64",
             ""
           ]

objectStart :: Text -> Int -> [Text]
objectStart label alignment =
  [ "\t.type\t" <> runtimeDataLabel label <> ",@object",
    "\t.section\t.rodata." <> runtimeDataLabel label <> ",\"\",@"
  ]
    <> ["\t.p2align\t" <> tshow alignment <> ", 0x0" | alignment /= 0]
    <> (if isLinkedData label then ["\t.hidden\t" <> runtimeDataLabel label, "\t.globl\t" <> runtimeDataLabel label] else [])
    <> [runtimeDataLabel label <> ":"]

runtimeDataLabel :: Text -> Text
runtimeDataLabel label
  | isLinkedData label = label
  | otherwise = dataLabel label

isLinkedData :: Text -> Bool
isLinkedData = T.isPrefixOf "aihc_constructor_"

dataLabel :: Text -> Text
dataLabel = (".L" <>)

nodeHeader :: CompileEnv -> GrinNode -> Either WasmError Text
nodeHeader env node = lookupRuntimeInfoLabel env key
  where
    fields = map grinValueRuntimeRep (grinNodeFields node)
    key = case grinNodeTag node of
      GrinConstructor name remaining -> ConstructorRuntimeInfo name remaining
      GrinClosure functionName layouts -> ClosureRuntimeInfo functionName fields layouts
      GrinThunk functionName -> ThunkRuntimeInfo functionName fields

lookupRuntimeInfoLabel :: CompileEnv -> RuntimeInfoKey -> Either WasmError Text
lookupRuntimeInfoLabel env key = case Map.lookup key (compileNodeInfoLabels env) of
  Just label -> Right label
  Nothing -> case key of
    ConstructorRuntimeInfo name remaining -> Right (renderLinkedConstructorInfoSymbol name remaining)
    ClosureRuntimeInfo functionName _ _ -> Left (WasmMissingFunction functionName)
    ThunkRuntimeInfo functionName _ -> Left (WasmMissingFunction functionName)

functionCodeLabel :: CompileEnv -> FunctionName -> Either WasmError Text
functionCodeLabel env name = maybe (Left (WasmMissingFunction name)) Right (Map.lookup name (compileFunctionLabels env))

functionArity :: CompileEnv -> FunctionName -> Either WasmError Int
functionArity env name = maybe (Left (WasmMissingFunction name)) Right (Map.lookup name (compileFunctionArities env))

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

runtimeInfoKeyFields :: RuntimeInfoKey -> [GrinRep]
runtimeInfoKeyFields key = case key of
  ConstructorRuntimeInfo {} -> []
  ClosureRuntimeInfo _ fields _ -> fields
  ThunkRuntimeInfo _ fields -> fields

runtimeInfoKeyRemainingArity :: RuntimeInfoKey -> Int
runtimeInfoKeyRemainingArity key = case key of
  ConstructorRuntimeInfo _ remaining -> remaining
  ClosureRuntimeInfo _ _ layouts -> length layouts
  ThunkRuntimeInfo {} -> 0

runtimeInfoKeyObjectKind :: RuntimeInfoKey -> Int
runtimeInfoKeyObjectKind key = case key of
  ConstructorRuntimeInfo _ 0 -> runtimeObjectNode
  ConstructorRuntimeInfo {} -> runtimeObjectPartialConstructor
  ClosureRuntimeInfo {} -> runtimeObjectClosure
  ThunkRuntimeInfo {} -> runtimeObjectThunk

runtimeObjectNode, runtimeObjectClosure, runtimeObjectThunk, runtimeObjectPartialConstructor :: Int
runtimeObjectNode = 0
runtimeObjectClosure = 1
runtimeObjectThunk = 2
runtimeObjectPartialConstructor = 3

runtimeInfoKeyNext :: RuntimeInfoKey -> Maybe RuntimeInfoKey
runtimeInfoKeyNext key = case key of
  ConstructorRuntimeInfo name remaining | remaining > 0 -> Just (ConstructorRuntimeInfo name (remaining - 1))
  ConstructorRuntimeInfo {} -> Nothing
  ClosureRuntimeInfo name fields (layout : rest) -> Just (ClosureRuntimeInfo name (fields <> layout) rest)
  ClosureRuntimeInfo {} -> Nothing
  ThunkRuntimeInfo {} -> Nothing

functionValueLocals :: GrinFunction -> Map GrinVar Int
functionValueLocals function = snd (foldl' assignGroup (length parameters + 2, parameterLocals) groups)
  where
    parameters = grinFunctionParameters function
    parameterLocals = Map.fromList (zip parameters [1 ..])
    groups = boundVarGroups (grinFunctionBody function)
    assignGroup = foldl' $ \(next, slots) var -> case Map.lookup var slots of
      Just _ -> (next, slots)
      Nothing -> (next + 1, Map.insert var next slots)

maximumScratchSlots :: GrinExpr -> Int
maximumScratchSlots expression = case expression of
  GrinBind _ value body -> max (maximumScratchSlots value) (maximumScratchSlots body)
  GrinStoreRec _ body -> maximumScratchSlots body
  GrinStoreRecUnchecked _ body -> maximumScratchSlots body
  GrinCall {} -> 0
  GrinCpsApply _ _ values _ -> length values
  GrinContinue _ values -> length values
  GrinEnsureHeap _ roots -> length roots
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
programNodes program = map snd (grinGlobals program) <> concatMap (exprNodes . grinFunctionBody) (grinFunctions program)

exprNodes :: GrinExpr -> [GrinNode]
exprNodes expression = case expression of
  GrinBind _ value body -> exprNodes value <> exprNodes body
  GrinStore node -> [node]
  GrinStoreUnchecked node -> [node]
  GrinStoreRec bindings body -> map snd bindings <> exprNodes body
  GrinStoreRecUnchecked bindings body -> map snd bindings <> exprNodes body
  GrinCase _ _ alternatives -> concatMap (exprNodes . grinAltRhs) alternatives
  _ -> []

programRuntimeReps :: GrinProgram -> [GrinRep]
programRuntimeReps program = concatMap (concat . snd) (grinConstructors program) <> concatMap nodeReps (programNodes program) <> concatMap functionReps (grinFunctions program)
  where
    nodeReps = map grinValueRuntimeRep . grinNodeFields
    functionReps function = grinFunctionResultRep function : map grinVarRuntimeRep (grinFunctionParameters function) <> exprReps (grinFunctionBody function)

exprReps :: GrinExpr -> [GrinRep]
exprReps expression = case expression of
  GrinBind vars value body -> map grinVarRuntimeRep vars <> exprReps value <> exprReps body
  GrinStore node -> nodeReps node
  GrinEnsureHeap requiredWords roots -> grinValueRuntimeRep requiredWords : map grinValueRuntimeRep roots
  GrinStoreUnchecked node -> nodeReps node
  GrinStoreRec bindings body -> concatMap (nodeReps . snd) bindings <> exprReps body
  GrinStoreRecUnchecked bindings body -> concatMap (nodeReps . snd) bindings <> exprReps body
  GrinCase value binder alternatives -> grinValueRuntimeRep value : grinVarRuntimeRep binder : concatMap (exprReps . grinAltRhs) alternatives
  _ -> []
  where
    nodeReps = map grinValueRuntimeRep . grinNodeFields

validateRuntimeRep :: GrinRep -> Either WasmError ()
validateRuntimeRep runtimeRep = case runtimeRep of
  VecRep {} -> Left (WasmUnsupportedRuntimeRep runtimeRep)
  TupleRep reps -> mapM_ validateRuntimeRep reps
  SumRep reps -> mapM_ validateRuntimeRep reps
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

normalizeScalar :: GrinRep -> Integer -> Integer
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
localFunctionLabel index _function = ".Laihc_wasm_function_" <> tshow index

boolInteger :: Bool -> Text
boolInteger True = "1"
boolInteger False = "0"

indent :: [Text] -> [Text]
indent = map ("\t" <>)

tshow :: (Show value) => value -> Text
tshow = T.pack . show
