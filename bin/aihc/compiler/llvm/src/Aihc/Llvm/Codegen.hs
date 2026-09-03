{-# LANGUAGE OverloadedStrings #-}

-- | Lower runtime-explicit GRIN to textual LLVM IR.
--
-- Generated computation and continuation entries use @tailcc@. Every CPS
-- transfer is a @musttail@ call followed immediately by @ret void@, so LLVM
-- verifies the no-growing-stack invariant instead of merely optimizing it by
-- convention. Dynamic entries are backend-generated stubs that unpack closure
-- fields and perform another guaranteed tail transfer to the typed code entry.
module Aihc.Llvm.Codegen
  ( LlvmError (..),
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
import Control.Monad (forM, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify', runStateT)
import Data.Bits (shiftL)
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import System.Info qualified as System

data LlvmError
  = LlvmMissingGlobal !Text
  | LlvmMissingFunction !FunctionName
  | LlvmMissingConstructor !Text
  | LlvmUnsupportedPrimitive !Text
  | LlvmUnsupportedExpression !Text
  | LlvmUnsupportedValue !Text
  | LlvmUnsupportedRuntimeRep !GrinRep
  deriving (Eq, Show)

data CompilationUnit
  = EntryUnit
  | LibraryUnit
  deriving (Eq)

data CompileEnv = CompileEnv
  { compileFunctionLabels :: !(Map FunctionName Text),
    compileAddrLiteralLabels :: !(Map BS.ByteString Text),
    compileNodeInfoLabels :: !(Map RuntimeInfoKey Text),
    compileRuntimeInfos :: ![RuntimeInfo],
    compileStaticReferences :: !StaticReferences,
    compileSrtLabels :: !(Map FunctionName Text),
    compileAllowUnsupportedPrimitives :: !Bool
  }

data RuntimeInfo = RuntimeInfo
  { runtimeInfoLabel :: !Text,
    runtimeInfoIdentity :: !(Maybe Text),
    runtimeInfoEntry :: !(Maybe Text),
    runtimeInfoFields :: ![GrinRep],
    runtimeInfoRemainingArity :: !Int,
    runtimeInfoNext :: !(Maybe Text),
    runtimeInfoEnter :: !(Maybe RuntimeEnter),
    runtimeInfoFrameKind :: !(Maybe ContinuationFrameKind),
    runtimeInfoObjectKind :: !Int,
    runtimeInfoSrt :: !(Maybe Text)
  }

data RuntimeEnter = RuntimeEnter
  { runtimeEnterTarget :: !Text,
    runtimeEnterStoredCount :: !Int,
    runtimeEnterSuppliedCount :: !Int,
    runtimeEnterIsContinuation :: !Bool
  }

data RuntimeInfoKey
  = ConstructorRuntimeInfo !Text !Int
  | ClosureRuntimeInfo !FunctionName ![GrinRep] ![[GrinRep]]
  | ThunkRuntimeInfo !FunctionName ![GrinRep]
  deriving (Eq, Ord, Show)

data FunctionState = FunctionState
  { functionNextSlot :: !Int,
    functionNextLabel :: !Int,
    functionNextValue :: !Int,
    functionBlocksRev :: ![(Text, [Text])]
  }

data ValueEnv = ValueEnv
  { valueCompileEnv :: !CompileEnv,
    valueLocalSlots :: !(Map GrinVar Int)
  }

type FunctionM = StateT FunctionState (Either LlvmError)

-- | Compile the fixed executable entry unit.
compileEntry :: Either LlvmError Text
compileEntry = do
  gcProgram <- either (Left . LlvmUnsupportedExpression . T.pack . show) Right entryGcProgram
  compileEntryUnit executableEntryName gcProgram

compileEntryUnit :: Text -> GcGrinProgram -> Either LlvmError Text
compileEntryUnit entryName gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  updateLabel <- functionCodeLabel env (gcUpdateFunction gcProgram)
  functions <- mapM (compileFunction env) (grinFunctions program)
  staticGlobals <- renderStaticGlobals env program
  let specialInfos =
        [ specialInfo "aihc_llvm_final_info" "aihc_llvm_final_continuation" [] 1 (Just "aihc_llvm_final_applied_info") (Just (continuationEnter "aihc_llvm_final_continuation" 0 1)) ContinuationFrameStop,
          specialInfo "aihc_llvm_final_applied_info" "aihc_llvm_final_continuation" [BoxedRep Lifted] 0 Nothing Nothing ContinuationFrameStop,
          specialInfo "aihc_llvm_top_info" "aihc_llvm_top_continuation" [BoxedRep Lifted] 1 (Just "aihc_llvm_top_applied_info") (Just (continuationEnter "aihc_llvm_top_continuation" 1 1)) ContinuationFrameNormal,
          specialInfo "aihc_llvm_top_applied_info" "aihc_llvm_top_continuation" [BoxedRep Lifted, BoxedRep Lifted] 0 Nothing Nothing ContinuationFrameNormal,
          specialInfo "aihc_llvm_update_info" updateLabel [BoxedRep Lifted, BoxedRep Lifted] 1 (Just "aihc_llvm_update_applied_info") (Just (continuationEnter updateLabel 2 1)) ContinuationFrameUpdate,
          specialInfo "aihc_llvm_update_applied_info" updateLabel [BoxedRep Lifted, BoxedRep Lifted, BoxedRep Lifted] 0 Nothing Nothing ContinuationFrameUpdate,
          specialInfo "aihc_llvm_thread_done_info" "aihc_llvm_thread_done_continuation" [] 1 (Just "aihc_llvm_thread_done_applied_info") (Just (continuationEnter "aihc_llvm_thread_done_continuation" 0 1)) ContinuationFrameStop,
          specialInfo "aihc_llvm_thread_done_applied_info" "aihc_llvm_thread_done_continuation" [BoxedRep Lifted] 0 Nothing Nothing ContinuationFrameStop
        ]
      source =
        llvmPreamble
          <> [ "@aihc_machine = global ptr null, align 8",
               "@" <> renderLinkedGlobalSymbol entryName <> " = external global i8",
               ""
             ]
          <> renderRuntimeDeclarations
          <> renderForeignDeclarations program
          <> renderExternalFunctionDeclarations env program
          <> renderAddrLiterals env
          <> renderRuntimeInfos (compileRuntimeInfos env <> specialInfos)
          <> staticGlobals
          <> renderStaticReferenceTables env
          <> renderLinkedLocals functions
          <> renderEnterStubs (compileRuntimeInfos env <> specialInfos)
          <> concatMap compiledFunctionLines functions
          <> renderNativeControlFunctions
          <> renderSpecialFunctions
          <> renderMain entryName
  pure (T.unlines source)
  where
    program = gcGrinProgram gcProgram
    env = compileEnvironment EntryUnit (gcContinuationFunctions gcProgram) (gcContinuationFrames gcProgram) program
    specialInfo label entry fields remaining next enter frameKind = RuntimeInfo label Nothing (Just entry) fields remaining next enter (Just frameKind) runtimeObjectClosure Nothing
    continuationEnter target stored supplied = RuntimeEnter target stored supplied True

compileModule :: GcGrinProgram -> Either LlvmError Text
compileModule gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  functions <- mapM (compileFunction env) (grinFunctions program)
  staticGlobals <- renderStaticGlobals env program
  let source =
        llvmPreamble
          <> ["@aihc_machine = external global ptr", ""]
          <> renderRuntimeDeclarations
          <> renderForeignDeclarations program
          <> renderExternalFunctionDeclarations env program
          <> renderAddrLiterals env
          <> renderRuntimeInfos (compileRuntimeInfos env)
          <> staticGlobals
          <> renderStaticReferenceTables env
          <> renderLinkedLocals functions
          <> renderEnterStubs (compileRuntimeInfos env)
          <> concatMap compiledFunctionLines functions
          <> renderNativeControlFunctions
  pure (T.unlines source)
  where
    program = gcGrinProgram gcProgram
    env = compileEnvironment LibraryUnit (gcContinuationFunctions gcProgram) (gcContinuationFrames gcProgram) program

validateProgramPrimitives :: GrinProgram -> Either LlvmError ()
validateProgramPrimitives = validatePrimitiveNames . map (grinVarName . fst) . grinPrimitives

validatePrimitiveNames :: [Text] -> Either LlvmError ()
validatePrimitiveNames = mapM_ $ \name ->
  if name `elem` supportedNativePrimitiveNames
    then Right ()
    else Left (LlvmUnsupportedPrimitive name)

compileEnvironment :: CompilationUnit -> Set.Set FunctionName -> Map FunctionName ContinuationFrameKind -> GrinProgram -> CompileEnv
compileEnvironment unitKind continuationFunctions continuationFrames program =
  CompileEnv
    { compileFunctionLabels = functionLabels,
      compileAddrLiteralLabels = Map.fromList [(bytes, llvmLabel label) | (bytes, label) <- buildAddrLiteralPool program],
      compileNodeInfoLabels = Map.fromList [(key, label) | (key, label, _) <- constructorEntries <> functionEntries],
      compileRuntimeInfos = map third (constructorEntries <> functionEntries),
      compileStaticReferences = staticReferences,
      compileSrtLabels = srtLabels,
      compileAllowUnsupportedPrimitives = unitKind == LibraryUnit
    }
  where
    constructorLayouts = grinConstructors program
    functionLabels =
      Map.fromList
        [ (grinFunctionName function, localFunctionLabel index function)
        | (index, function) <- zip [0 :: Int ..] (grinFunctions program)
        ]
    staticReferences = programStaticReferences program
    srtLabels =
      Map.fromList
        [ (name, "aihc_llvm_srt_" <> tshow index)
        | (index, name) <- zip [0 :: Int ..] (Map.keys (staticReferenceTables staticReferences))
        ]
    constructorEntries =
      [ (key, label, RuntimeInfo label (Just (renderLinkedConstructorInfoSymbol name 0)) Nothing fields remaining next Nothing Nothing (runtimeInfoKeyObjectKind key) Nothing)
      | (name, layouts) <- constructorLayouts,
        let arity = length layouts,
        remaining <- [arity, arity - 1 .. 0],
        let key = ConstructorRuntimeInfo name remaining,
        key `Set.member` requiredConstructorInfos,
        let label = renderLinkedConstructorInfoSymbol name remaining
            fields = concat (take (arity - remaining) layouts)
            next = if remaining == 0 then Nothing else Just (renderLinkedConstructorInfoSymbol name (remaining - 1))
      ]
    requiredConstructorInfos =
      Set.fromList
        ( declaredConstructorInfos
            <> concatMap requiredNodeConstructorInfos (programNodes program)
        )
    declaredConstructorInfos =
      [ConstructorRuntimeInfo name 0 | (name, layouts) <- constructorLayouts, null layouts]
    infoKeys =
      [ key
      | key <- Set.toAscList (Set.fromList (concatMap runtimeInfoKeyStages (programNodes program))),
        Just functionName <- [runtimeInfoFunctionName key],
        functionName `Map.member` functionLabels
      ]
    functionEntries =
      [ ( key,
          label,
          RuntimeInfo
            label
            Nothing
            (runtimeInfoFunctionName key >>= (`Map.lookup` functionLabels))
            (runtimeInfoKeyFields key)
            (runtimeInfoKeyRemainingArity key)
            (runtimeInfoKeyNext key >>= (`Map.lookup` infoLabels))
            (runtimeEnter key)
            (runtimeInfoFunctionName key >>= (`Map.lookup` continuationFrames))
            (runtimeInfoKeyObjectKind key)
            (runtimeInfoFunctionName key >>= (`Map.lookup` srtLabels))
        )
      | (index, key) <- zip [0 :: Int ..] infoKeys,
        let label = "aihc_llvm_function_info_" <> tshow index
      ]
    infoLabels = Map.fromList [(key, "aihc_llvm_function_info_" <> tshow index) | (index, key) <- zip [0 :: Int ..] infoKeys]
    runtimeEnter key =
      case key of
        ClosureRuntimeInfo functionName fields [supplied] -> do
          target <- Map.lookup functionName functionLabels
          pure
            RuntimeEnter
              { runtimeEnterTarget = target,
                runtimeEnterStoredCount = length fields,
                runtimeEnterSuppliedCount = length supplied,
                runtimeEnterIsContinuation = functionName `Set.member` continuationFunctions
              }
        ThunkRuntimeInfo functionName fields -> do
          target <- Map.lookup functionName functionLabels
          pure
            RuntimeEnter
              { runtimeEnterTarget = target,
                runtimeEnterStoredCount = length fields,
                runtimeEnterSuppliedCount = 0,
                runtimeEnterIsContinuation = False
              }
        _ -> Nothing
    third (_, _, value) = value

requiredNodeConstructorInfos :: GrinNode -> [RuntimeInfoKey]
requiredNodeConstructorInfos node =
  case grinNodeTag node of
    GrinConstructor name remaining -> [ConstructorRuntimeInfo name stage | stage <- [remaining, remaining - 1 .. 0]]
    GrinClosure {} -> []
    GrinThunk {} -> []

compileFunction :: CompileEnv -> GrinFunction -> Either LlvmError CompiledFunction
compileFunction env function = do
  label <- functionCodeLabel env (grinFunctionName function)
  let slots = functionLocalSlots function
      initial = FunctionState (Map.size slots) 0 0 []
      valueEnv = ValueEnv env slots
  (_, final) <- runStateT (compileExpr valueEnv [] "body" (grinFunctionBody function)) initial
  let slotCount = functionNextSlot final
      parameters = grinFunctionParameters function
      parameterNames = ["%arg_" <> tshow index | index <- [0 .. length parameters - 1]]
      parameterStores =
        [ "  store i64 " <> parameterName <> ", ptr " <> localSlotRef slot <> ", align 8"
        | (parameterName, parameter) <- zip parameterNames parameters,
          Just slot <- [Map.lookup parameter slots]
        ]
      header =
        [ "define " <> functionLinkage function <> "tailcc void @" <> label <> "(" <> renderParameters parameterNames <> ") {",
          "entry:"
        ]
          <> ["  " <> localSlotRef slot <> " = alloca i64, align 8" | slot <- [0 .. slotCount - 1]]
          <> parameterStores
          <> currentSrtStore
          <> ["  br label %body"]
      -- Every function publishes its own table, including the empty one, so a
      -- collection never sees a table left behind by a function that has
      -- already transferred control away.
      currentSrtStore =
        [ "  store ptr "
            <> maybe "null" ("@" <>) (Map.lookup (grinFunctionName function) (compileSrtLabels env))
            <> ", ptr @aihc_current_srt, align 8"
        ]
      blocks = concatMap renderBlock (reverse (functionBlocksRev final))
  pure (CompiledFunction (header <> blocks <> ["}", ""]) slotCount)
  where
    renderParameters names = T.intercalate ", " ("ptr %machine" : map ("i64 " <>) names)

data CompiledFunction = CompiledFunction
  { compiledFunctionLines :: [Text],
    compiledFunctionSlots :: !Int
  }

compileExpr :: ValueEnv -> [Text] -> Text -> GrinExpr -> FunctionM ()
compileExpr env prefix label expression =
  case expression of
    GrinBind vars valueExpression body -> do
      direct <- compileDirectBinding env vars valueExpression
      compileExpr env (prefix <> direct) label body
    GrinStoreRec bindings body -> compileStoreRec False bindings body
    GrinStoreRecUnchecked bindings body -> compileStoreRec True bindings body
    GrinCpsEval runtimeRep value continuation updateContinuation -> do
      (valueLines, valueOperand) <- materializeValue env value
      (continuationLines, continuationOperand) <- materializeValue env continuation
      (updateLines, updateOperand) <- materializeValue env updateContinuation
      (pointerLines, pointerOperands) <- pointerArguments [valueOperand, continuationOperand, updateOperand]
      transfer <- case pointerOperands of
        [valuePointer, continuationPointer, updatePointer] ->
          pure
            [ "  musttail call tailcc void @aihc_llvm_eval(ptr %machine, ptr "
                <> valuePointer
                <> ", i64 "
                <> boolInteger (isLiftedRuntimeRep runtimeRep)
                <> ", ptr "
                <> continuationPointer
                <> ", ptr "
                <> updatePointer
                <> ")"
            ]
        _ -> lift (Left (LlvmUnsupportedExpression "internal CPS evaluation pointer arity"))
      terminal label (prefix <> valueLines <> continuationLines <> updateLines <> pointerLines <> transfer)
    GrinCall _ functionName arguments -> do
      target <- liftEither (functionCodeLabel (valueCompileEnv env) functionName)
      (lines', operands) <- materializeValues env arguments
      terminal label (prefix <> lines' <> directTailCall target operands)
    GrinCpsPrimitiveCall _ name arguments continuation -> compileCpsPrimitive env prefix label name arguments continuation
    GrinCpsApply _ function arguments continuation -> do
      (functionLines, functionOperand) <- materializeValue env function
      (continuationLines, continuationOperand) <- materializeValue env continuation
      (argumentLines, argumentOperands) <- materializeValues env arguments
      (pointerLines, pointerOperands) <- pointerArguments [functionOperand, continuationOperand]
      case pointerOperands of
        [functionPointer, continuationPointer] ->
          compileApplyTransfer
            (prefix <> functionLines <> continuationLines <> argumentLines <> pointerLines)
            label
            functionPointer
            argumentOperands
            continuationPointer
        _ -> lift (Left (LlvmUnsupportedExpression "internal CPS apply pointer arity"))
    GrinContinue continuation values -> do
      (continuationLines, continuationOperand) <- materializeValue env continuation
      (valueLines, valueOperands) <- materializeValues env values
      (pointerLines, pointerOperands) <- pointerArguments [continuationOperand]
      transfer <- case pointerOperands of
        [continuationPointer] -> compileContinueTransfer continuationPointer valueOperands
        _ -> lift (Left (LlvmUnsupportedExpression "internal continuation pointer arity"))
      terminal label (prefix <> continuationLines <> valueLines <> pointerLines <> transfer)
    GrinCpsRaise exception continuation -> do
      (lines', operands) <- materializeValues env [exception, continuation]
      (pointerLines, pointerOperands) <- pointerArguments operands
      case pointerOperands of
        [exceptionPointer, continuationPointer] -> do
          resume <- freshValue
          terminal
            label
            ( prefix
                <> lines'
                <> pointerLines
                <> [ "  " <> resume <> " = call ptr @aihc_raise(ptr %machine, ptr " <> exceptionPointer <> ", ptr " <> continuationPointer <> ")",
                     "  musttail call tailcc void @aihc_llvm_resume(ptr %machine, ptr " <> resume <> ")"
                   ]
            )
        _ -> lift (Left (LlvmUnsupportedExpression "internal CPS raise pointer arity"))
    GrinHalt {} -> do
      entry <- freshValue
      terminal
        label
        ( prefix
            <> ["  " <> entry <> " = call ptr @aihc_halt(ptr %machine)"]
            <> ["  musttail call tailcc void " <> entry <> "(ptr %machine)"]
        )
    GrinExit status -> do
      (statusLines, statusOperand) <- materializeValue env status
      terminal
        label
        ( prefix
            <> statusLines
            <> [ "  call void @aihc_exit_process(i64 " <> statusOperand <> ")",
                 "  unreachable"
               ]
        )
    GrinCase scrutinee binder alternatives -> compileCase env prefix label scrutinee binder alternatives
    GrinConstant {} -> unsupported "direct-style constant return after CPS"
    GrinStore {} -> unsupported "direct-style store return after CPS"
    GrinEnsureHeap {} -> unsupported "unbound heap reservation"
    GrinStoreUnchecked {} -> unsupported "unbound unchecked store"
    GrinUpdate {} -> unsupported "direct-style update after CPS"
    GrinUpdateBlackhole {} -> unsupported "unbound blackhole update"
    GrinEval {} -> unsupported "direct-style eval after CPS"
    GrinPrimitiveCall {} -> unsupported "unbound primitive call after CPS"
    GrinApply {} -> unsupported "direct-style apply after CPS"
    GrinThrow {} -> unsupported "throw"
    GrinCatch {} -> unsupported "catch"
    GrinForeignCallExpr {} -> unsupported "unbound foreign call after CPS"
  where
    unsupported = lift . Left . LlvmUnsupportedExpression
    terminal blockLabel lines' = addBlock blockLabel (lines' <> ["  ret void"])
    compileStoreRec unchecked bindings body = do
      allocations <- fmap concat . forM bindings $ \(var, node) -> do
        destination <- localSlot env var
        (lines', operand) <- materializeNode env unchecked node
        pure (lines' <> [storeLocal destination operand])
      fields <- fmap concat . forM bindings $ \(var, node) -> do
        slot <- localSlot env var
        (sourceLines, source) <- loadLocal slot
        fieldLines <- initializeLocalFields env source node
        pure (sourceLines <> fieldLines)
      compileExpr env (prefix <> allocations <> fields) label body

compileApplyTransfer :: [Text] -> Text -> Text -> [Text] -> Text -> FunctionM ()
compileApplyTransfer prefix label function arguments continuation = do
  fastLabel <- freshLabel "apply_fast"
  slowLabel <- freshLabel "apply_slow"
  (infoLines, kind, info) <- loadValueInfo function
  arityPointer <- freshValue
  arity <- freshValue
  isClosure <- freshValue
  isSaturated <- freshValue
  isFast <- freshValue
  addBlock
    label
    ( prefix
        <> infoLines
        <> [ "  " <> arityPointer <> " = getelementptr %AihcInfo, ptr " <> info <> ", i32 0, i32 3",
             "  " <> arity <> " = load i64, ptr " <> arityPointer <> ", align 8",
             "  " <> isClosure <> " = icmp eq i64 " <> kind <> ", 1",
             "  " <> isSaturated <> " = icmp eq i64 " <> arity <> ", 1",
             "  " <> isFast <> " = and i1 " <> isClosure <> ", " <> isSaturated,
             "  br i1 " <> isFast <> ", label %" <> fastLabel <> ", label %" <> slowLabel
           ]
    )
  enterPointer <- freshValue
  enter <- freshValue
  addBlock
    fastLabel
    [ "  " <> enterPointer <> " = getelementptr %AihcInfo, ptr " <> info <> ", i32 0, i32 6",
      "  " <> enter <> " = load ptr, ptr " <> enterPointer <> ", align 8",
      "  musttail call tailcc void "
        <> enter
        <> "("
        <> T.intercalate ", " (["ptr %machine", "ptr " <> function, "ptr " <> continuation] <> map ("i64 " <>) arguments)
        <> ")",
      "  ret void"
    ]
  (arrayLines, array) <- storeOperandArray arguments
  continuationSlot <- freshValue
  applied <- freshValue
  adjustedContinuation <- freshValue
  appliedInteger <- freshValue
  continueLines <- compileContinueTransfer adjustedContinuation [appliedInteger]
  addBlock
    slowLabel
    ( arrayLines
        <> [ "  " <> continuationSlot <> " = alloca ptr, align 8",
             "  store ptr " <> continuation <> ", ptr " <> continuationSlot <> ", align 8",
             "  "
               <> applied
               <> " = call ptr @aihc_apply_slow(ptr %machine, ptr "
               <> function
               <> ", i64 "
               <> tshow (length arguments)
               <> ", ptr "
               <> array
               <> ", ptr "
               <> continuationSlot
               <> ")",
             "  " <> adjustedContinuation <> " = load ptr, ptr " <> continuationSlot <> ", align 8",
             "  " <> appliedInteger <> " = ptrtoint ptr " <> applied <> " to i64"
           ]
        <> continueLines
        <> ["  ret void"]
    )

compileContinueTransfer :: Text -> [Text] -> FunctionM [Text]
compileContinueTransfer continuation values = do
  (infoLines, _, info) <- loadValueInfo continuation
  enterPointer <- freshValue
  enter <- freshValue
  pure
    ( infoLines
        <> [ "  " <> enterPointer <> " = getelementptr %AihcInfo, ptr " <> info <> ", i32 0, i32 6",
             "  " <> enter <> " = load ptr, ptr " <> enterPointer <> ", align 8",
             "  musttail call tailcc void "
               <> enter
               <> "("
               <> T.intercalate ", " (["ptr %machine", "ptr " <> continuation] <> map ("i64 " <>) values)
               <> ")"
           ]
    )

loadValueInfo :: Text -> FunctionM ([Text], Text, Text)
loadValueInfo value = do
  header <- freshValue
  info <- freshValue
  kindPointer <- freshValue
  kind <- freshValue
  pure
    ( [ "  " <> header <> " = load i64, ptr " <> value <> ", align 8",
        "  " <> info <> " = inttoptr i64 " <> header <> " to ptr",
        "  " <> kindPointer <> " = getelementptr %AihcInfo, ptr " <> info <> ", i32 0, i32 8",
        "  " <> kind <> " = load i64, ptr " <> kindPointer <> ", align 8"
      ],
      kind,
      info
    )

compileCpsPrimitive :: ValueEnv -> [Text] -> Text -> Text -> [GrinValue] -> GrinValue -> FunctionM ()
compileCpsPrimitive env prefix label name arguments continuation =
  case (name, arguments) of
    ("awaitIO#", [request]) -> resume "aihc_await_io" [request, continuation]
    ("fork#", [action]) -> do
      (lines', operands) <- materializeValues env [action, continuation]
      (pointerLines, pointerOperands) <- pointerArguments operands
      case pointerOperands of
        [actionPointer, continuationPointer] -> do
          threadId <- freshValue
          continueLines <- compileContinueTransfer continuationPointer [threadId]
          addBlock
            label
            ( prefix
                <> lines'
                <> pointerLines
                <> ["  " <> threadId <> " = call i64 @aihc_fork(ptr %machine, ptr " <> actionPointer <> ")"]
                <> continueLines
                <> ["  ret void"]
            )
        _ -> lift (Left (LlvmUnsupportedExpression "internal fork pointer arity"))
    ("newMVar#", []) -> do
      (continuationLines, continuationOperand) <- materializeValue env continuation
      (pointerLines, pointerOperands) <- pointerArguments [continuationOperand]
      case pointerOperands of
        [continuationPointer] -> do
          mvar <- freshValue
          mvarInteger <- freshValue
          continueLines <- compileContinueTransfer continuationPointer [mvarInteger]
          addBlock
            label
            ( prefix
                <> continuationLines
                <> pointerLines
                <> [ "  " <> mvar <> " = call ptr @aihc_mvar_new(ptr %machine)",
                     "  " <> mvarInteger <> " = ptrtoint ptr " <> mvar <> " to i64"
                   ]
                <> continueLines
                <> ["  ret void"]
            )
        _ -> lift (Left (LlvmUnsupportedExpression "internal newMVar# pointer arity"))
    (operation, [mvar])
      | Just runtimeFunction <- lookup operation [("readMVar#", "aihc_mvar_read"), ("takeMVar#", "aihc_mvar_take")] ->
          resume runtimeFunction [mvar, continuation]
    ("putMVar#", [mvar, value]) -> do
      (lines', operands) <- materializeValues env [mvar, value, continuation]
      case operands of
        [mvarOperand, valueOperand, continuationOperand] -> do
          (pointerLines, pointerOperands) <- pointerArguments [mvarOperand, continuationOperand]
          case pointerOperands of
            [mvarPointer, continuationPointer] -> do
              result <- freshValue
              addBlock
                label
                ( prefix
                    <> lines'
                    <> pointerLines
                    <> [ "  "
                           <> result
                           <> " = call ptr @aihc_mvar_put(ptr %machine, ptr "
                           <> mvarPointer
                           <> ", i64 "
                           <> valueOperand
                           <> ", ptr "
                           <> continuationPointer
                           <> ")",
                         "  musttail call tailcc void @aihc_llvm_resume(ptr %machine, ptr " <> result <> ")",
                         "  ret void"
                       ]
                )
            _ -> lift (Left (LlvmUnsupportedExpression "internal putMVar# pointer arity"))
        _ -> lift (Left (LlvmUnsupportedExpression "internal putMVar# operand arity"))
    ("yield#", []) -> resume "aihc_yield" [continuation]
    _
      | compileAllowUnsupportedPrimitives (valueCompileEnv env) ->
          addBlock label (prefix <> ["  call void @aihc_unsupported_primitive()", "  unreachable"])
    _ -> lift (Left (LlvmUnsupportedExpression ("CPS primitive call " <> name)))
  where
    resume function values = do
      (lines', operands) <- materializeValues env values
      (pointerLines, pointerOperands) <- pointerArguments operands
      result <- freshValue
      let callArguments = T.intercalate ", " ("ptr %machine" : map ("ptr " <>) pointerOperands)
      addBlock
        label
        ( prefix
            <> lines'
            <> pointerLines
            <> [ "  " <> result <> " = call ptr @" <> function <> "(" <> callArguments <> ")",
                 "  musttail call tailcc void @aihc_llvm_resume(ptr %machine, ptr " <> result <> ")",
                 "  ret void"
               ]
        )

compileDirectBinding :: ValueEnv -> [GrinVar] -> GrinExpr -> FunctionM [Text]
compileDirectBinding env vars expression =
  case expression of
    GrinConstant values
      | length vars == length values -> fmap concat . forM (zip vars values) $ \(var, value) -> do
          destination <- localSlot env var
          (lines', operand) <- materializeValue env value
          pure (lines' <> [storeLocal destination operand])
    GrinStore node -> materializeNode env False node >>= storeOne
    GrinEnsureHeap requiredWords roots
      | length vars == length roots -> do
          (requiredLines, requiredOperand) <- materializeValue env requiredWords
          (rootLines, rootOperands) <- materializeValues env roots
          rootsArray <- freshValue
          elementStores <- fmap concat . forM (zip [0 :: Int ..] rootOperands) $ \(index, operand) -> do
            element <- freshValue
            pure
              [ "  " <> element <> " = getelementptr [" <> tshow (length roots) <> " x i64], ptr " <> rootsArray <> ", i64 0, i64 " <> tshow index,
                "  store i64 " <> operand <> ", ptr " <> element <> ", align 8"
              ]
          relocated <- fmap concat . forM (zip [0 :: Int ..] vars) $ \(index, var) -> do
            destination <- localSlot env var
            element <- freshValue
            value <- freshValue
            pure
              [ "  " <> element <> " = getelementptr [" <> tshow (length roots) <> " x i64], ptr " <> rootsArray <> ", i64 0, i64 " <> tshow index,
                "  " <> value <> " = load i64, ptr " <> element <> ", align 8",
                storeLocal destination value
              ]
          pure
            ( requiredLines
                <> rootLines
                <> ["  " <> rootsArray <> " = alloca [" <> tshow (max 1 (length roots)) <> " x i64], align 8"]
                <> elementStores
                <> ["  call void @aihc_ensure_heap(ptr %machine, i64 " <> requiredOperand <> ", i64 " <> tshow (length roots) <> ", ptr " <> rootsArray <> ")"]
                <> relocated
            )
    GrinStoreUnchecked node -> materializeNode env True node >>= storeOne
    GrinUpdate pointer value -> update "aihc_update" False pointer value
    GrinUpdateBlackhole pointer value -> update "aihc_update_blackhole" True pointer value
    GrinPrimitiveCall IntRep name [left, right]
      | Just instruction <- lookup name [("+#", "add"), ("-#", "sub"), ("*#", "mul")] ->
          binaryPrimitive instruction left right
    GrinPrimitiveCall WordRep name [left, right]
      | Just instruction <-
          lookup
            name
            [ ("plusWord#", "add"),
              ("minusWord#", "sub"),
              ("timesWord#", "mul"),
              ("quotWord#", "udiv"),
              ("remWord#", "urem"),
              ("and#", "and"),
              ("or#", "or"),
              ("xor#", "xor")
            ] ->
          binaryPrimitive instruction left right
    GrinPrimitiveCall _ name [left, right]
      | Just predicate <-
          lookup
            name
            [ ("<#", "slt"),
              ("==#", "eq"),
              (">#", "sgt"),
              (">=#", "sge"),
              ("<=#", "sle"),
              ("/=#", "ne"),
              ("eqWord#", "eq"),
              ("neWord#", "ne"),
              ("ltWord#", "ult"),
              ("leWord#", "ule"),
              ("gtWord#", "ugt"),
              ("geWord#", "uge"),
              ("eqWord64#", "eq"),
              ("neWord64#", "ne"),
              ("ltWord64#", "ult"),
              ("leWord64#", "ule"),
              ("gtWord64#", "ugt"),
              ("geWord64#", "uge")
            ] ->
          comparisonPrimitive predicate left right
    GrinPrimitiveCall _ name [left, right]
      | name `elem` ["addIntC#", "subIntC#", "addWordC#", "subWordC#"] ->
          carryPrimitive name left right
    GrinPrimitiveCall _ "timesWord2#" [left, right] -> do
      (lines', operands) <- materializeValues env [left, right]
      case operands of
        [leftOperand, rightOperand] -> do
          wideLeft <- freshValue
          wideRight <- freshValue
          wideProduct <- freshValue
          highWide <- freshValue
          high <- freshValue
          low <- freshValue
          storePair
            ( lines'
                <> [ "  " <> wideLeft <> " = zext i64 " <> leftOperand <> " to i128",
                     "  " <> wideRight <> " = zext i64 " <> rightOperand <> " to i128",
                     "  " <> wideProduct <> " = mul i128 " <> wideLeft <> ", " <> wideRight,
                     "  " <> highWide <> " = lshr i128 " <> wideProduct <> ", 64",
                     "  " <> high <> " = trunc i128 " <> highWide <> " to i64",
                     "  " <> low <> " = trunc i128 " <> wideProduct <> " to i64"
                   ]
            )
            high
            low
        _ -> internalArity "timesWord2#"
    GrinPrimitiveCall _ "quotRemWord#" [left, right] -> do
      (lines', operands) <- materializeValues env [left, right]
      case operands of
        [leftOperand, rightOperand] -> do
          quotient <- freshValue
          remainder <- freshValue
          storePair
            ( lines'
                <> [ "  " <> quotient <> " = udiv i64 " <> leftOperand <> ", " <> rightOperand,
                     "  " <> remainder <> " = urem i64 " <> leftOperand <> ", " <> rightOperand
                   ]
            )
            quotient
            remainder
        _ -> internalArity "quotRemWord#"
    GrinPrimitiveCall _ "quotRemWord2#" [high, low, divisor] -> do
      (lines', operands) <- materializeValues env [high, low, divisor]
      case operands of
        [highOperand, lowOperand, divisorOperand] -> do
          wideHigh <- freshValue
          shiftedHigh <- freshValue
          wideLow <- freshValue
          dividend <- freshValue
          wideDivisor <- freshValue
          wideQuotient <- freshValue
          wideRemainder <- freshValue
          quotient <- freshValue
          remainder <- freshValue
          storePair
            ( lines'
                <> [ "  " <> wideHigh <> " = zext i64 " <> highOperand <> " to i128",
                     "  " <> shiftedHigh <> " = shl i128 " <> wideHigh <> ", 64",
                     "  " <> wideLow <> " = zext i64 " <> lowOperand <> " to i128",
                     "  " <> dividend <> " = or i128 " <> shiftedHigh <> ", " <> wideLow,
                     "  " <> wideDivisor <> " = zext i64 " <> divisorOperand <> " to i128",
                     "  " <> wideQuotient <> " = udiv i128 " <> dividend <> ", " <> wideDivisor,
                     "  " <> wideRemainder <> " = urem i128 " <> dividend <> ", " <> wideDivisor,
                     "  " <> quotient <> " = trunc i128 " <> wideQuotient <> " to i64",
                     "  " <> remainder <> " = trunc i128 " <> wideRemainder <> " to i64"
                   ]
            )
            quotient
            remainder
        _ -> internalArity "quotRemWord2#"
    GrinPrimitiveCall _ "not#" [value] -> do
      (lines', operand) <- materializeValue env value
      result <- freshValue
      storeOne (lines' <> ["  " <> result <> " = xor i64 " <> operand <> ", -1"], result)
    GrinPrimitiveCall _ name [value, amount]
      | Just instruction <- lookup name [("uncheckedShiftL#", "shl"), ("uncheckedShiftRL#", "lshr")] ->
          binaryPrimitive instruction value amount
    GrinPrimitiveCall _ name [value]
      | name `elem` ["int2Word#", "word2Int#", "word8ToWord#", "word32ToWord#", "word64ToWord#", "wordToWord64#", "word16ToWord#", "ord#", "chr#", "unsafeFreezeArray#", "unsafeThawArray#", "unsafeFreezeByteArray#", "unsafeThawByteArray#", "castFloatToWord32#", "castWord32ToFloat#", "castDoubleToWord64#", "castWord64ToDouble#"] ->
          materializeValue env value >>= storeOne
    GrinPrimitiveCall _ "newArray#" [size, initial] -> do
      (lines', operands) <- materializeValues env [size, initial]
      case operands of
        [sizeOperand, initialOperand] -> do
          resultPointer <- freshValue
          result <- freshValue
          storeOne
            ( lines'
                <> [ "  " <> resultPointer <> " = call ptr @aihc_array_new(ptr %machine, i64 " <> sizeOperand <> ", i64 " <> initialOperand <> ")",
                     "  " <> result <> " = ptrtoint ptr " <> resultPointer <> " to i64"
                   ],
              result
            )
        _ -> internalArity "boxed-array allocation"
    GrinPrimitiveCall _ "newMutVar#" [initial] -> do
      (lines', initialOperand) <- materializeValue env initial
      resultPointer <- freshValue
      result <- freshValue
      storeOne
        ( lines'
            <> [ "  " <> resultPointer <> " = call ptr @aihc_mutvar_new(ptr %machine, i64 " <> initialOperand <> ")",
                 "  " <> result <> " = ptrtoint ptr " <> resultPointer <> " to i64"
               ],
          result
        )
    GrinPrimitiveCall _ "makeStableName#" [value] -> do
      (lines', operand) <- materializeValue env value
      valuePointer <- freshValue
      namePointer <- freshValue
      result <- freshValue
      storeOne
        ( lines'
            <> [ "  " <> valuePointer <> " = inttoptr i64 " <> operand <> " to ptr",
                 "  " <> namePointer <> " = call ptr @aihc_stable_name_make(ptr %machine, ptr " <> valuePointer <> ")",
                 "  " <> result <> " = ptrtoint ptr " <> namePointer <> " to i64"
               ],
          result
        )
    GrinPrimitiveCall IntRep "compareInt#" [left, right] -> do
      (lines', operands) <- materializeValues env [left, right]
      case operands of
        [leftOperand, rightOperand] -> do
          less <- freshValue
          greater <- freshValue
          lessInt <- freshValue
          greaterInt <- freshValue
          result <- freshValue
          storeOne
            ( lines'
                <> [ "  " <> less <> " = icmp slt i64 " <> leftOperand <> ", " <> rightOperand,
                     "  " <> greater <> " = icmp sgt i64 " <> leftOperand <> ", " <> rightOperand,
                     "  " <> lessInt <> " = zext i1 " <> less <> " to i64",
                     "  " <> greaterInt <> " = zext i1 " <> greater <> " to i64",
                     "  " <> result <> " = sub i64 " <> greaterInt <> ", " <> lessInt
                   ],
              result
            )
        _ -> internalArity "compareInt#"
    GrinPrimitiveCall _ "nullAddr#" [] -> storeOne ([], "0")
    GrinPrimitiveCall runtimeRep "realWorld#" []
      | null vars && null (runtimeRepComponents runtimeRep) -> pure []
    GrinPrimitiveCall _ "casMutVar#" [reference, expected, replacement]
      | Just swapCall <- nativeRuntimePrimitiveCall "casMutVar#",
        Just readCall <- nativeRuntimePrimitiveCall "readMutVar#",
        [flag, current] <- vars -> do
          (swapLines, flagOperand) <- compileForeignCall env (nativeRuntimeCallForeignCall swapCall) [reference, expected, replacement]
          flagDestination <- localSlot env flag
          (readLines, currentOperand) <- compileForeignCall env (nativeRuntimeCallForeignCall readCall) [reference]
          currentDestination <- localSlot env current
          pure (swapLines <> [storeLocal flagDestination flagOperand] <> readLines <> [storeLocal currentDestination currentOperand])
    GrinPrimitiveCall _ name arguments
      | Just splitCalls <- nativeSplitRuntimePrimitiveCall name,
        length splitCalls == length vars ->
          concat
            <$> mapM
              ( \(var, splitCall) -> do
                  (callLines, operand) <- compileForeignCall env (nativeRuntimeCallForeignCall splitCall) arguments
                  destination <- localSlot env var
                  pure (callLines <> [storeLocal destination operand])
              )
              (zip vars splitCalls)
    GrinPrimitiveCall _ name arguments
      | Just runtimeCall <- nativeRuntimePrimitiveCall name -> do
          result <- compileForeignCall env (nativeRuntimeCallForeignCall runtimeCall) arguments
          case vars of
            [] -> pure (fst result)
            [_] -> storeOne result
            _ -> lift (Left (LlvmUnsupportedExpression ("byte array primitive result arity " <> name)))
    GrinPrimitiveCall {}
      | compileAllowUnsupportedPrimitives (valueCompileEnv env) -> do
          zeros <- forM vars $ \var -> do
            destination <- localSlot env var
            pure (storeLocal destination "0")
          pure (["  call void @aihc_unsupported_primitive()"] <> zeros)
    GrinPrimitiveCall _ name _ -> lift (Left (LlvmUnsupportedExpression ("primitive call " <> name)))
    GrinForeignCallExpr foreignCall arguments -> compileForeignCall env foreignCall arguments >>= storeOne
    _ -> lift (Left (LlvmUnsupportedExpression "non-direct expression remained in a CPS bind"))
  where
    storeOne (lines', operand) =
      case vars of
        [var] -> do
          destination <- localSlot env var
          pure (lines' <> [storeLocal destination operand])
        _ -> lift (Left (LlvmUnsupportedExpression "direct expression result arity"))
    storePair lines' firstOperand secondOperand =
      case vars of
        [first, second] -> do
          firstDestination <- localSlot env first
          secondDestination <- localSlot env second
          pure
            ( lines'
                <> [ storeLocal firstDestination firstOperand,
                     storeLocal secondDestination secondOperand
                   ]
            )
        _ -> lift (Left (LlvmUnsupportedExpression "direct expression pair result arity"))
    binaryPrimitive instruction left right = do
      (lines', operands) <- materializeValues env [left, right]
      case operands of
        [leftOperand, rightOperand] -> do
          result <- freshValue
          storeOne (lines' <> ["  " <> result <> " = " <> instruction <> " i64 " <> leftOperand <> ", " <> rightOperand], result)
        _ -> internalArity "binary primitive"
    comparisonPrimitive predicate left right = do
      (lines', operands) <- materializeValues env [left, right]
      case operands of
        [leftOperand, rightOperand] -> do
          comparison <- freshValue
          result <- freshValue
          storeOne
            ( lines'
                <> [ "  " <> comparison <> " = icmp " <> predicate <> " i64 " <> leftOperand <> ", " <> rightOperand,
                     "  " <> result <> " = zext i1 " <> comparison <> " to i64"
                   ],
              result
            )
        _ -> internalArity "comparison primitive"
    carryPrimitive name left right = do
      (lines', operands) <- materializeValues env [left, right]
      case operands of
        [leftOperand, rightOperand] -> do
          result <- freshValue
          let instruction = if name `elem` ["addIntC#", "addWordC#"] then "add" else "sub"
          case name of
            "addIntC#" -> do
              resultLeft <- freshValue
              resultRight <- freshValue
              overflowBits <- freshValue
              flag <- freshValue
              storePair
                ( lines'
                    <> [ "  " <> result <> " = add i64 " <> leftOperand <> ", " <> rightOperand,
                         "  " <> resultLeft <> " = xor i64 " <> result <> ", " <> leftOperand,
                         "  " <> resultRight <> " = xor i64 " <> result <> ", " <> rightOperand,
                         "  " <> overflowBits <> " = and i64 " <> resultLeft <> ", " <> resultRight,
                         "  " <> flag <> " = lshr i64 " <> overflowBits <> ", 63"
                       ]
                )
                result
                flag
            "subIntC#" -> do
              leftRight <- freshValue
              leftResult <- freshValue
              overflowBits <- freshValue
              flag <- freshValue
              storePair
                ( lines'
                    <> [ "  " <> result <> " = sub i64 " <> leftOperand <> ", " <> rightOperand,
                         "  " <> leftRight <> " = xor i64 " <> leftOperand <> ", " <> rightOperand,
                         "  " <> leftResult <> " = xor i64 " <> leftOperand <> ", " <> result,
                         "  " <> overflowBits <> " = and i64 " <> leftRight <> ", " <> leftResult,
                         "  " <> flag <> " = lshr i64 " <> overflowBits <> ", 63"
                       ]
                )
                result
                flag
            _ -> do
              carry <- freshValue
              flag <- freshValue
              let (predicate, firstOperand, secondOperand) =
                    if name == "addWordC#"
                      then ("ult", result, leftOperand)
                      else ("ult", leftOperand, rightOperand)
              storePair
                ( lines'
                    <> [ "  " <> result <> " = " <> instruction <> " i64 " <> leftOperand <> ", " <> rightOperand,
                         "  " <> carry <> " = icmp " <> predicate <> " i64 " <> firstOperand <> ", " <> secondOperand,
                         "  " <> flag <> " = zext i1 " <> carry <> " to i64"
                       ]
                )
                result
                flag
        _ -> internalArity "carry primitive"
    update function passMachine pointer value = do
      (lines', operands) <- materializeValues env [pointer, value]
      case operands of
        [pointerOperand, valueOperand] -> do
          (pointerLines, pointerOperands) <- pointerArguments [pointerOperand, valueOperand]
          case pointerOperands of
            [pointerPointer, valuePointer] ->
              storeOne
                ( lines'
                    <> pointerLines
                    <> [ "  call void @"
                           <> function
                           <> "("
                           <> T.intercalate ", " (["ptr %machine" | passMachine] <> ["ptr " <> pointerPointer, "ptr " <> valuePointer])
                           <> ")"
                       ],
                  valueOperand
                )
            _ -> internalArity "update pointer"
        _ -> internalArity "update"
    internalArity name = lift (Left (LlvmUnsupportedExpression ("internal " <> name <> " arity")))

compileForeignCall :: ValueEnv -> GrinForeignCall -> [GrinValue] -> FunctionM ([Text], Text)
compileForeignCall env foreignCall arguments = do
  let signature = grinForeignCallSignature foreignCall
      argumentTypes = grinForeignArgumentTypes signature
  if length arguments /= length (grinForeignOperandReps signature)
    then lift (Left (LlvmUnsupportedExpression "foreign call arity mismatch"))
    else do
      (lines', operands) <- materializeValues env arguments
      converted <- zipWithM convertForeignArgument argumentTypes operands
      let conversionLines = concatMap fst converted
          callArguments = T.intercalate ", " (zipWith (\foreignType' (_, operand) -> llvmForeignType foreignType' <> " " <> operand) argumentTypes converted)
          resultType = grinForeignResultType signature
      callResult <- freshValue
      (resultLines, resultOperand) <- convertForeignResult resultType callResult
      pure
        ( lines'
            <> conversionLines
            <> ["  " <> callResult <> " = call " <> llvmForeignType resultType <> " @" <> grinForeignCallSymbol foreignCall <> "(" <> callArguments <> ")"]
            <> resultLines,
          resultOperand
        )

compileCase :: ValueEnv -> [Text] -> Text -> GrinValue -> GrinVar -> [GrinAlt] -> FunctionM ()
compileCase env prefix label scrutinee binder alternatives = do
  resultSlot <- freshSlot
  dispatch <- freshLabel "case_dispatch"
  (valueLines, valueOperand) <- materializeValue env scrutinee
  addBlock label (prefix <> valueLines <> [storeLocal resultSlot valueOperand, "  br label %" <> dispatch])
  binderSlot <- localSlot env binder
  targets <- forM alternatives $ \alternative -> do
    target <- freshLabel "case_alt"
    alternativeLines <- alternativePrefix env resultSlot alternative
    compileExpr env alternativeLines target (grinAltRhs alternative)
    pure (alternative, target)
  (resultLines, result) <- loadLocal resultSlot
  discriminatorLines <-
    if isPointerRuntimeRep (grinValueRuntimeRep scrutinee)
      then pointerIdentity result
      else pure ([], result)
  checks <- caseSwitch (snd discriminatorLines) targets
  addBlock dispatch (resultLines <> [storeLocal binderSlot result] <> fst discriminatorLines <> checks)

alternativePrefix :: ValueEnv -> Int -> GrinAlt -> FunctionM [Text]
alternativePrefix env resultSlot alternative =
  case grinAltCon alternative of
    GrinDataAlt _ -> do
      (resultLines, result) <- loadLocal resultSlot
      bindings <- forM (zip [0 :: Int ..] (grinAltBinders alternative)) $ \(index, binder) -> do
        destination <- localSlot env binder
        fieldPointer <- freshValue
        field <- freshValue
        (objectLines, object) <- intToPtr result
        pure
          ( objectLines
              <> [ "  " <> fieldPointer <> " = getelementptr i64, ptr " <> object <> ", i64 " <> tshow (index + 1),
                   "  " <> field <> " = load i64, ptr " <> fieldPointer <> ", align 8",
                   storeLocal destination field
                 ]
          )
      pure (resultLines <> concat bindings)
    GrinLitAlt _ -> pure []
    GrinDefaultAlt -> do
      (resultLines, result) <- loadLocal resultSlot
      bindings <- forM (grinAltBinders alternative) $ \binder -> do
        destination <- localSlot env binder
        pure (storeLocal destination result)
      pure (resultLines <> bindings)

caseSwitch :: Text -> [(GrinAlt, Text)] -> FunctionM [Text]
caseSwitch discriminator targets = do
  allCases <- fmap concat . forM nonDefault $ \(alternative, target) ->
    case grinAltCon alternative of
      GrinDataAlt name ->
        pure [("ptrtoint (ptr @" <> renderLinkedConstructorInfoSymbol name 0 <> " to i64)", target)]
      GrinLitAlt literal ->
        case normalizedLiteralInteger literal of
          Just integer -> pure [(renderI64 integer, target)]
          Nothing -> lift (Left (LlvmUnsupportedValue "string case alternative"))
      GrinDefaultAlt -> pure []
  let cases = firstCases allCases
  defaultLabel <-
    case [target | (alternative, target) <- targets, grinAltCon alternative == GrinDefaultAlt] of
      target : _ -> pure target
      [] -> do
        missing <- freshLabel "case_no_match"
        addBlock missing ["  call void @aihc_no_match()", "  unreachable"]
        pure missing
  renderChecks defaultLabel cases
  where
    nonDefault = [(alternative, target) | (alternative, target) <- targets, grinAltCon alternative /= GrinDefaultAlt]

    firstCases = go Set.empty
      where
        go _ [] = []
        go seen (entry@(discriminant, _) : rest)
          | discriminant `Set.member` seen = go seen rest
          | otherwise = entry : go (Set.insert discriminant seen) rest

    renderChecks defaultLabel choices =
      case choices of
        [] -> pure ["  br label %" <> defaultLabel]
        (value, target) : rest -> do
          comparison <- freshValue
          next <-
            case rest of
              [] -> pure defaultLabel
              _ -> freshLabel "case_check"
          case rest of
            [] -> pure ()
            _ -> renderChecks defaultLabel rest >>= addBlock next
          pure
            [ "  " <> comparison <> " = icmp eq i64 " <> discriminator <> ", " <> value,
              "  br i1 " <> comparison <> ", label %" <> target <> ", label %" <> next
            ]

materializeValues :: ValueEnv -> [GrinValue] -> FunctionM ([Text], [Text])
materializeValues env values = do
  materialized <- mapM (materializeValue env) values
  pure (concatMap fst materialized, map snd materialized)

materializeValue :: ValueEnv -> GrinValue -> FunctionM ([Text], Text)
materializeValue env value =
  case value of
    GrinVarValue var ->
      case Map.lookup var (valueLocalSlots env) of
        Just slot -> loadLocal slot
        Nothing -> materializeGlobal (grinVarName var)
    GrinGlobalValue name -> materializeGlobal name
    GrinLitValue literal -> materializeLiteral (valueCompileEnv env) literal

materializeLiteral :: CompileEnv -> GrinLiteral -> FunctionM ([Text], Text)
materializeLiteral env literal =
  case literal of
    GrinLitAddr bytes ->
      case Map.lookup bytes (compileAddrLiteralLabels env) of
        Nothing -> lift (Left (LlvmUnsupportedValue "unregistered Addr# literal"))
        Just label -> do
          result <- freshValue
          pure (["  " <> result <> " = ptrtoint ptr @" <> label <> " to i64"], result)
    _ ->
      case normalizedLiteralInteger literal of
        Just integer -> pure ([], renderI64 integer)
        Nothing -> lift (Left (LlvmUnsupportedValue "string literal"))

materializeGlobal :: Text -> FunctionM ([Text], Text)
materializeGlobal name = do
  result <- freshValue
  pure (["  " <> result <> " = ptrtoint ptr @" <> renderLinkedGlobalSymbol name <> " to i64"], result)

materializeNode :: ValueEnv -> Bool -> GrinNode -> FunctionM ([Text], Text)
materializeNode env unchecked node = do
  info <- liftEither (nodeHeader (valueCompileEnv env) node)
  object <- freshValue
  objectInteger <- freshValue
  fields <- initializeLocalFields env objectInteger node
  pure
    ( [ "  "
          <> object
          <> " = call ptr @"
          <> (if unchecked then "aihc_make_node_unchecked" else "aihc_make_node")
          <> "(ptr %machine, ptr @"
          <> info
          <> ")",
        "  " <> objectInteger <> " = ptrtoint ptr " <> object <> " to i64"
      ]
        <> fields,
      objectInteger
    )

initializeLocalFields :: ValueEnv -> Text -> GrinNode -> FunctionM [Text]
initializeLocalFields env objectOperand node =
  fmap concat . forM (zip [0 :: Int ..] (grinNodeFields node)) $ \(index, field) -> do
    (lines', operand) <- materializeValue env field
    (objectLines, object) <- intToPtr objectOperand
    pure (lines' <> objectLines <> ["  call void @aihc_set_field(ptr " <> object <> ", i64 " <> tshow index <> ", i64 " <> operand <> ")"])

nodeHeader :: CompileEnv -> GrinNode -> Either LlvmError Text
nodeHeader env node = lookupRuntimeInfoLabel env key
  where
    fields = map grinValueRuntimeRep (grinNodeFields node)
    key =
      case grinNodeTag node of
        GrinConstructor name remaining -> ConstructorRuntimeInfo name remaining
        GrinClosure functionName layouts -> ClosureRuntimeInfo functionName fields layouts
        GrinThunk functionName -> ThunkRuntimeInfo functionName fields

-- | Render each static object, and a root entry for the objects the collector
-- has to mark. The entries are private constants that no code references, so
-- @llvm.used@ must keep them. Without it, global dead-code elimination removes
-- every entry and the collector cannot tell a static object from a heap
-- pointer.
--
-- Objects that can neither move nor retain anything - nullary constructors -
-- get no entry. The collector then leaves a pointer to one alone, which is
-- what it would do after marking it anyway.
renderStaticGlobals :: CompileEnv -> GrinProgram -> Either LlvmError [Text]
renderStaticGlobals env program = do
  rendered <- mapM renderGlobal objects
  pure (concat rendered <> usedRoots)
  where
    objects = programStaticObjects program
    tracedObjects = filter staticObjectTraced objects
    usedRoots
      | null tracedObjects = []
      | otherwise =
          [ "@llvm.used = appending global ["
              <> tshow (length tracedObjects)
              <> " x ptr] ["
              <> T.intercalate ", " ["ptr @" <> renderLinkedGlobalSymbol (staticObjectName object) <> "_root" | object <- tracedObjects]
              <> "], section \"llvm.metadata\"",
            ""
          ]
    renderGlobal object = do
      let node = staticObjectNode object
      info <- staticNodeInfo node
      fields <- mapM renderStaticValue (grinNodeFields node)
      let symbol = renderLinkedGlobalSymbol (staticObjectName object)
          payload = if null fields && isThunk node then ["i64 0"] else fields
          values = "i64 ptrtoint (ptr @" <> info <> " to i64)" : payload
          count = length values
      pure $
        [ "@" <> symbol <> " = global [" <> tshow count <> " x i64] [" <> T.intercalate ", " values <> "], align 8"
        ]
          <> [ "@" <> symbol <> "_root = private constant ptr @" <> symbol <> ", section \"" <> nativeDataSection "aihc_roots" <> "\", align 8"
             | staticObjectTraced object
             ]
          <> [""]
    staticNodeInfo node =
      case grinNodeTag node of
        GrinConstructor name remaining -> pure (renderLinkedConstructorInfoSymbol name remaining)
        GrinClosure functionName layouts -> lookupRuntimeInfoLabel env (ClosureRuntimeInfo functionName fields layouts)
        GrinThunk functionName -> lookupRuntimeInfoLabel env (ThunkRuntimeInfo functionName fields)
      where
        fields = map grinValueRuntimeRep (grinNodeFields node)
    renderStaticValue value =
      case value of
        GrinVarValue var -> pure (globalAddress (grinVarName var))
        GrinGlobalValue name -> pure (globalAddress name)
        GrinLitValue literal ->
          case literal of
            GrinLitAddr bytes ->
              maybe
                (Left (LlvmUnsupportedValue "unregistered Addr# literal"))
                (pure . ("i64 ptrtoint (ptr @" <>) . (<> " to i64)"))
                (Map.lookup bytes (compileAddrLiteralLabels env))
            _ -> maybe (Left (LlvmUnsupportedValue "string literal")) (pure . ("i64 " <>) . renderI64) (normalizedLiteralInteger literal)
    globalAddress name = "i64 ptrtoint (ptr @" <> renderLinkedGlobalSymbol name <> " to i64)"
    isThunk node =
      case grinNodeTag node of
        GrinThunk {} -> True
        _ -> False

renderStaticReferenceTables :: CompileEnv -> [Text]
renderStaticReferenceTables env =
  concatMap renderTable (Map.toList (staticReferenceTables (compileStaticReferences env)))
  where
    renderTable (name, table) =
      case Map.lookup name (compileSrtLabels env) of
        Nothing -> []
        Just label ->
          [ "@"
              <> label
              <> " = internal global ["
              <> tshow (3 + length words')
              <> " x i64] [i64 0, i64 "
              <> tshow (length (srtObjects table))
              <> ", i64 "
              <> tshow (length (srtChildren table))
              <> T.concat [", i64 " <> word | word <- words']
              <> "], align 8",
            ""
          ]
      where
        words' =
          [ "ptrtoint (ptr @" <> renderLinkedGlobalSymbol object <> " to i64)"
          | object <- srtObjects table
          ]
            <> [ "ptrtoint (ptr @" <> childLabel <> " to i64)"
               | child <- srtChildren table,
                 Just childLabel <- [Map.lookup child (compileSrtLabels env)]
               ]

renderLinkedLocals :: [CompiledFunction] -> [Text]
renderLinkedLocals functions =
  [ "@aihc_llvm_linked_locals = private constant i64 "
      <> tshow (maximum (2 : map compiledFunctionSlots functions))
      <> ", section \""
      <> nativeDataSection "aihc_locals"
      <> "\", align 8",
    ""
  ]

renderMain :: Text -> [Text]
renderMain entryName =
  [ "define i32 @main(i32 %argc, ptr %argv) {",
    "entry:",
    "  call void @aihc_program_arguments_initialize(i32 %argc, ptr %argv)",
    "  %machine = call ptr @aihc_machine_new(i64 0)",
    "  store ptr %machine, ptr @aihc_machine, align 8"
  ]
    <> [ "  call void @aihc_ensure_heap(ptr %machine, i64 7, i64 0, ptr null)",
         "  %final = call ptr @aihc_make_node_unchecked(ptr %machine, ptr @aihc_llvm_final_info)",
         "  %top = call ptr @aihc_make_node_unchecked(ptr %machine, ptr @aihc_llvm_top_info)",
         "  %final_i64 = ptrtoint ptr %final to i64",
         "  call void @aihc_set_field(ptr %top, i64 0, i64 %final_i64)",
         "  %update = call ptr @aihc_make_node_unchecked(ptr %machine, ptr @aihc_llvm_update_info)",
         "  %root = ptrtoint ptr @" <> renderLinkedGlobalSymbol entryName <> " to i64",
         "  %top_i64 = ptrtoint ptr %top to i64",
         "  call void @aihc_set_field(ptr %update, i64 0, i64 %top_i64)",
         "  call void @aihc_set_field(ptr %update, i64 1, i64 %root)",
         "  %thread_done = call ptr @aihc_make_node_unchecked(ptr %machine, ptr @aihc_llvm_thread_done_info)",
         "  call void @aihc_set_thread_done_continuation(ptr %machine, ptr %thread_done)",
         "  %exit_field = getelementptr %AihcMachinePrefix, ptr %machine, i32 0, i32 2",
         "  store ptr @aihc_llvm_exit, ptr %exit_field, align 8",
         "  %root_ptr = inttoptr i64 %root to ptr",
         "  call tailcc void @aihc_llvm_eval(ptr %machine, ptr %root_ptr, i64 1, ptr %top, ptr %update)",
         "  ret i32 0",
         "}",
         ""
       ]

renderSpecialFunctions :: [Text]
renderSpecialFunctions =
  renderSpecial "aihc_llvm_top_continuation" 2 topBody
    <> renderSpecial "aihc_llvm_thread_done_continuation" 1 threadDoneBody
    <> renderSpecial "aihc_llvm_final_continuation" 1 finalBody
    <> [ "define internal tailcc void @aihc_llvm_exit(ptr %machine) {",
         "entry:",
         "  ret void",
         "}",
         ""
       ]
  where
    renderSpecial name arity body =
      [ "define internal tailcc void @" <> name <> "(" <> renderDefinitionArguments arity <> ") {",
        "entry:"
      ]
        <> body
        <> ["}", ""]
    topBody =
      [ "  %function = inttoptr i64 %arg_1 to ptr",
        "  %continuation = inttoptr i64 %arg_0 to ptr",
        "  musttail call tailcc void @aihc_llvm_apply_0(ptr %machine, ptr %function, ptr %continuation)",
        "  ret void"
      ]
    threadDoneBody =
      [ "  %resume = call ptr @aihc_thread_done(ptr %machine)",
        "  musttail call tailcc void @aihc_llvm_resume(ptr %machine, ptr %resume)",
        "  ret void"
      ]
    finalBody =
      [ "  %exit = call ptr @aihc_halt(ptr %machine)",
        "  musttail call tailcc void %exit(ptr %machine)",
        "  ret void"
      ]

renderNativeControlFunctions :: [Text]
renderNativeControlFunctions =
  [ "define internal tailcc void @aihc_llvm_continue_0(ptr %machine, ptr %continuation) {",
    "entry:",
    "  %header = load i64, ptr %continuation, align 8",
    "  %info = inttoptr i64 %header to ptr",
    "  %entry_slot = getelementptr %AihcInfo, ptr %info, i32 0, i32 6",
    "  %target = load ptr, ptr %entry_slot, align 8",
    "  musttail call tailcc void %target(ptr %machine, ptr %continuation)",
    "  ret void",
    "}",
    "",
    "define internal tailcc void @aihc_llvm_continue_1(ptr %machine, ptr %continuation, i64 %value) {",
    "entry:",
    "  %header = load i64, ptr %continuation, align 8",
    "  %info = inttoptr i64 %header to ptr",
    "  %entry_slot = getelementptr %AihcInfo, ptr %info, i32 0, i32 6",
    "  %target = load ptr, ptr %entry_slot, align 8",
    "  musttail call tailcc void %target(ptr %machine, ptr %continuation, i64 %value)",
    "  ret void",
    "}",
    "",
    "define internal tailcc void @aihc_llvm_apply_0(ptr %machine, ptr %function, ptr %continuation) {",
    "entry:",
    "  br label %loop",
    "loop:",
    "  %current = phi ptr [ %function, %entry ], [ %indirected, %indirection ]",
    "  %header = load i64, ptr %current, align 8",
    "  %info = inttoptr i64 %header to ptr",
    "  %arity_slot = getelementptr %AihcInfo, ptr %info, i32 0, i32 3",
    "  %arity = load i64, ptr %arity_slot, align 8",
    "  %kind_slot = getelementptr %AihcInfo, ptr %info, i32 0, i32 8",
    "  %kind = load i64, ptr %kind_slot, align 8",
    "  %is_indirection = icmp eq i64 %kind, 4",
    "  br i1 %is_indirection, label %indirection, label %apply",
    "indirection:",
    "  %field_slot = getelementptr i64, ptr %current, i64 1",
    "  %field = load i64, ptr %field_slot, align 8",
    "  %indirected = inttoptr i64 %field to ptr",
    "  br label %loop",
    "apply:",
    "  %is_closure = icmp eq i64 %kind, 1",
    "  %is_saturated = icmp eq i64 %arity, 1",
    "  %is_fast = and i1 %is_closure, %is_saturated",
    "  br i1 %is_fast, label %fast, label %slow",
    "fast:",
    "  %entry_slot = getelementptr %AihcInfo, ptr %info, i32 0, i32 6",
    "  %target = load ptr, ptr %entry_slot, align 8",
    "  musttail call tailcc void %target(ptr %machine, ptr %current, ptr %continuation)",
    "  ret void",
    "slow:",
    "  %continuation_slot = alloca ptr, align 8",
    "  store ptr %continuation, ptr %continuation_slot, align 8",
    "  %applied = call ptr @aihc_apply_slow(ptr %machine, ptr %current, i64 0, ptr null, ptr %continuation_slot)",
    "  %adjusted_continuation = load ptr, ptr %continuation_slot, align 8",
    "  %applied_i64 = ptrtoint ptr %applied to i64",
    "  musttail call tailcc void @aihc_llvm_continue_1(ptr %machine, ptr %adjusted_continuation, i64 %applied_i64)",
    "  ret void",
    "}",
    "",
    "define internal tailcc void @aihc_llvm_apply_1(ptr %machine, ptr %function, ptr %continuation, i64 %value) {",
    "entry:",
    "  br label %loop",
    "loop:",
    "  %current = phi ptr [ %function, %entry ], [ %indirected, %indirection ]",
    "  %header = load i64, ptr %current, align 8",
    "  %info = inttoptr i64 %header to ptr",
    "  %arity_slot = getelementptr %AihcInfo, ptr %info, i32 0, i32 3",
    "  %arity = load i64, ptr %arity_slot, align 8",
    "  %kind_slot = getelementptr %AihcInfo, ptr %info, i32 0, i32 8",
    "  %kind = load i64, ptr %kind_slot, align 8",
    "  %is_indirection = icmp eq i64 %kind, 4",
    "  br i1 %is_indirection, label %indirection, label %apply",
    "indirection:",
    "  %field_slot = getelementptr i64, ptr %current, i64 1",
    "  %field = load i64, ptr %field_slot, align 8",
    "  %indirected = inttoptr i64 %field to ptr",
    "  br label %loop",
    "apply:",
    "  %is_closure = icmp eq i64 %kind, 1",
    "  %is_saturated = icmp eq i64 %arity, 1",
    "  %is_fast = and i1 %is_closure, %is_saturated",
    "  br i1 %is_fast, label %fast, label %slow",
    "fast:",
    "  %entry_slot = getelementptr %AihcInfo, ptr %info, i32 0, i32 6",
    "  %target = load ptr, ptr %entry_slot, align 8",
    "  musttail call tailcc void %target(ptr %machine, ptr %current, ptr %continuation, i64 %value)",
    "  ret void",
    "slow:",
    "  %arguments = alloca [1 x i64], align 8",
    "  %argument = getelementptr [1 x i64], ptr %arguments, i64 0, i64 0",
    "  store i64 %value, ptr %argument, align 8",
    "  %continuation_slot = alloca ptr, align 8",
    "  store ptr %continuation, ptr %continuation_slot, align 8",
    "  %applied = call ptr @aihc_apply_slow(ptr %machine, ptr %current, i64 1, ptr %arguments, ptr %continuation_slot)",
    "  %adjusted_continuation = load ptr, ptr %continuation_slot, align 8",
    "  %applied_i64 = ptrtoint ptr %applied to i64",
    "  musttail call tailcc void @aihc_llvm_continue_1(ptr %machine, ptr %adjusted_continuation, i64 %applied_i64)",
    "  ret void",
    "}",
    "",
    "define internal tailcc void @aihc_llvm_resume(ptr %machine, ptr %resume) {",
    "entry:",
    "  %kind_slot = getelementptr %AihcResume, ptr %resume, i32 0, i32 0",
    "  %function_slot = getelementptr %AihcResume, ptr %resume, i32 0, i32 1",
    "  %continuation_slot = getelementptr %AihcResume, ptr %resume, i32 0, i32 2",
    "  %value_slot = getelementptr %AihcResume, ptr %resume, i32 0, i32 3",
    "  %count_slot = getelementptr %AihcResume, ptr %resume, i32 0, i32 4",
    "  %kind = load i64, ptr %kind_slot, align 8",
    "  %function = load ptr, ptr %function_slot, align 8",
    "  %continuation = load ptr, ptr %continuation_slot, align 8",
    "  %value = load i64, ptr %value_slot, align 8",
    "  %count = load i64, ptr %count_slot, align 8",
    "  store %AihcResume zeroinitializer, ptr %resume, align 8",
    "  switch i64 %kind, label %invalid [ i64 1, label %apply i64 2, label %continue i64 3, label %raise ]",
    "apply:",
    "  %apply_has_one = icmp eq i64 %count, 1",
    "  br i1 %apply_has_one, label %apply_one, label %apply_zero_check",
    "apply_zero_check:",
    "  %apply_has_zero = icmp eq i64 %count, 0",
    "  br i1 %apply_has_zero, label %apply_zero, label %invalid",
    "apply_zero:",
    "  musttail call tailcc void @aihc_llvm_apply_0(ptr %machine, ptr %function, ptr %continuation)",
    "  ret void",
    "apply_one:",
    "  musttail call tailcc void @aihc_llvm_apply_1(ptr %machine, ptr %function, ptr %continuation, i64 %value)",
    "  ret void",
    "continue:",
    "  %has_value = icmp eq i64 %count, 1",
    "  br i1 %has_value, label %continue_one, label %continue_zero_check",
    "continue_zero_check:",
    "  %has_no_values = icmp eq i64 %count, 0",
    "  br i1 %has_no_values, label %continue_zero, label %invalid",
    "continue_zero:",
    "  musttail call tailcc void @aihc_llvm_continue_0(ptr %machine, ptr %function)",
    "  ret void",
    "continue_one:",
    "  musttail call tailcc void @aihc_llvm_continue_1(ptr %machine, ptr %function, i64 %value)",
    "  ret void",
    "raise:",
    "  %raised_resume = call ptr @aihc_raise(ptr %machine, ptr %function, ptr %continuation)",
    "  musttail call tailcc void @aihc_llvm_resume(ptr %machine, ptr %raised_resume)",
    "  ret void",
    "invalid:",
    "  call void @aihc_no_match()",
    "  unreachable",
    "}",
    "",
    "define internal tailcc void @aihc_llvm_eval(ptr %machine, ptr %value, i64 %result_is_lifted, ptr %continuation, ptr %update_continuation) {",
    "entry:",
    "  br label %loop",
    "loop:",
    "  %current = phi ptr [ %value, %entry ], [ %indirected, %indirection_lifted ]",
    "  %header = load i64, ptr %current, align 8",
    "  %info = inttoptr i64 %header to ptr",
    "  %kind_slot = getelementptr %AihcInfo, ptr %info, i32 0, i32 8",
    "  %kind = load i64, ptr %kind_slot, align 8",
    "  switch i64 %kind, label %ready [ i64 2, label %thunk i64 4, label %indirection i64 5, label %blackhole ]",
    "thunk:",
    "  call void @aihc_begin_blackhole(ptr %machine, ptr %current)",
    "  %thunk_entry_slot = getelementptr %AihcInfo, ptr %info, i32 0, i32 6",
    "  %thunk_entry = load ptr, ptr %thunk_entry_slot, align 8",
    "  musttail call tailcc void %thunk_entry(ptr %machine, ptr %current, ptr %update_continuation)",
    "  ret void",
    "indirection:",
    "  %field_slot = getelementptr i64, ptr %current, i64 1",
    "  %field = load i64, ptr %field_slot, align 8",
    "  %indirected = inttoptr i64 %field to ptr",
    "  %is_lifted = icmp ne i64 %result_is_lifted, 0",
    "  br i1 %is_lifted, label %indirection_lifted, label %indirection_unlifted",
    "indirection_lifted:",
    "  br label %loop",
    "indirection_unlifted:",
    "  musttail call tailcc void @aihc_llvm_continue_1(ptr %machine, ptr %continuation, i64 %field)",
    "  ret void",
    "blackhole:",
    "  %resume = call ptr @aihc_block_on_blackhole(ptr %machine, ptr %current, ptr %continuation)",
    "  musttail call tailcc void @aihc_llvm_resume(ptr %machine, ptr %resume)",
    "  ret void",
    "ready:",
    "  %ready_i64 = ptrtoint ptr %current to i64",
    "  musttail call tailcc void @aihc_llvm_continue_1(ptr %machine, ptr %continuation, i64 %ready_i64)",
    "  ret void",
    "}",
    ""
  ]

directTailCall :: Text -> [Text] -> [Text]
directTailCall target operands =
  ["  musttail call tailcc void @" <> target <> "(" <> renderCallArguments "%machine" operands <> ")"]

renderCallArguments :: Text -> [Text] -> Text
renderCallArguments machine operands = T.intercalate ", " ("ptr " <> machine : map ("i64 " <>) operands)

renderDefinitionArguments :: Int -> Text
renderDefinitionArguments count =
  T.intercalate ", " ("ptr %machine" : ["i64 %arg_" <> tshow index | index <- [0 .. count - 1]])

storeOperandArray :: [Text] -> FunctionM ([Text], Text)
storeOperandArray [] = pure ([], "null")
storeOperandArray operands = do
  array <- freshValue
  stores <- fmap concat . forM (zip [0 :: Int ..] operands) $ \(index, operand) -> do
    element <- freshValue
    pure
      [ "  " <> element <> " = getelementptr [" <> tshow (length operands) <> " x i64], ptr " <> array <> ", i64 0, i64 " <> tshow index,
        "  store i64 " <> operand <> ", ptr " <> element <> ", align 8"
      ]
  pure (["  " <> array <> " = alloca [" <> tshow (length operands) <> " x i64], align 8"] <> stores, array)

pointerIdentity :: Text -> FunctionM ([Text], Text)
pointerIdentity operand = do
  (objectLines, object) <- intToPtr operand
  header <- freshValue
  info <- freshValue
  identity <- freshValue
  pure
    ( objectLines
        <> [ "  " <> header <> " = load i64, ptr " <> object <> ", align 8",
             "  " <> info <> " = inttoptr i64 " <> header <> " to ptr",
             "  " <> identity <> " = load i64, ptr " <> info <> ", align 8"
           ],
      identity
    )

convertForeignArgument :: GrinForeignType -> Text -> FunctionM ([Text], Text)
convertForeignArgument foreignType operand =
  case foreignType of
    GrinForeignInt -> pure ([], operand)
    GrinForeignInt32 -> do
      converted <- freshValue
      pure (["  " <> converted <> " = trunc i64 " <> operand <> " to i32"], converted)
    GrinForeignWord64 -> pure ([], operand)
    GrinForeignAddr -> intToPtr operand

convertForeignResult :: GrinForeignType -> Text -> FunctionM ([Text], Text)
convertForeignResult foreignType operand =
  case foreignType of
    GrinForeignInt -> pure ([], operand)
    GrinForeignInt32 -> do
      converted <- freshValue
      pure (["  " <> converted <> " = sext i32 " <> operand <> " to i64"], converted)
    GrinForeignWord64 -> pure ([], operand)
    GrinForeignAddr -> do
      converted <- freshValue
      pure (["  " <> converted <> " = ptrtoint ptr " <> operand <> " to i64"], converted)

intToPtr :: Text -> FunctionM ([Text], Text)
intToPtr operand = do
  result <- freshValue
  pure (["  " <> result <> " = inttoptr i64 " <> operand <> " to ptr"], result)

pointerArguments :: [Text] -> FunctionM ([Text], [Text])
pointerArguments operands = do
  converted <- mapM intToPtr operands
  pure (concatMap fst converted, map snd converted)

loadLocal :: Int -> FunctionM ([Text], Text)
loadLocal slot = do
  result <- freshValue
  pure (["  " <> result <> " = load i64, ptr " <> localSlotRef slot <> ", align 8"], result)

localSlot :: ValueEnv -> GrinVar -> FunctionM Int
localSlot env var = maybe (lift (Left (LlvmUnsupportedExpression ("missing local slot for " <> grinVarName var)))) pure (Map.lookup var (valueLocalSlots env))

freshSlot :: FunctionM Int
freshSlot = do
  state <- get
  let result = functionNextSlot state
  modify' $ \current -> current {functionNextSlot = result + 1}
  pure result

freshLabel :: Text -> FunctionM Text
freshLabel kind = do
  state <- get
  let identifier = functionNextLabel state
  modify' $ \current -> current {functionNextLabel = identifier + 1}
  pure (kind <> "_" <> tshow identifier)

freshValue :: FunctionM Text
freshValue = do
  state <- get
  let identifier = functionNextValue state
  modify' $ \current -> current {functionNextValue = identifier + 1}
  pure ("%v" <> tshow identifier)

addBlock :: Text -> [Text] -> FunctionM ()
addBlock label lines' = modify' $ \state -> state {functionBlocksRev = (label, lines') : functionBlocksRev state}

renderBlock :: (Text, [Text]) -> [Text]
renderBlock (label, lines') = (label <> ":") : lines'

storeLocal :: Int -> Text -> Text
storeLocal slot operand = "  store i64 " <> operand <> ", ptr " <> localSlotRef slot <> ", align 8"

localSlotRef :: Int -> Text
localSlotRef slot = "%slot_" <> tshow slot

renderEnterStubs :: [RuntimeInfo] -> [Text]
renderEnterStubs = concatMap renderStub
  where
    renderStub info =
      case runtimeInfoEnter info of
        Nothing -> []
        Just enter ->
          let suppliedNames = ["%supplied_" <> tshow index | index <- [0 .. runtimeEnterSuppliedCount enter - 1]]
              parameters =
                ["ptr %machine", "ptr %closure"]
                  <> ["ptr %continuation" | not (runtimeEnterIsContinuation enter)]
                  <> map ("i64 " <>) suppliedNames
              loadStored index =
                [ "  %stored_" <> tshow index <> "_ptr = getelementptr i64, ptr %closure, i64 " <> tshow (index + 1),
                  "  %stored_" <> tshow index <> " = load i64, ptr %stored_" <> tshow index <> "_ptr, align 8"
                ]
              storedNames = ["%stored_" <> tshow index | index <- [0 .. runtimeEnterStoredCount enter - 1]]
              continuationConversion =
                ["  %continuation_i64 = ptrtoint ptr %continuation to i64" | not (runtimeEnterIsContinuation enter)]
              targetArguments = storedNames <> suppliedNames <> ["%continuation_i64" | not (runtimeEnterIsContinuation enter)]
           in [ "define internal tailcc void @"
                  <> enterEntryLabel info
                  <> "("
                  <> T.intercalate ", " parameters
                  <> ") {",
                "entry:"
              ]
                <> concatMap loadStored [0 .. runtimeEnterStoredCount enter - 1]
                <> continuationConversion
                <> [ "  musttail call tailcc void @"
                       <> runtimeEnterTarget enter
                       <> "("
                       <> renderCallArguments "%machine" targetArguments
                       <> ")",
                     "  ret void",
                     "}",
                     ""
                   ]

enterEntryLabel :: RuntimeInfo -> Text
enterEntryLabel info = runtimeInfoLabel info <> "_enter"

renderRuntimeInfos :: [RuntimeInfo] -> [Text]
renderRuntimeInfos infos = concatMap bitmap infos <> map definition infos <> [""]
  where
    bitmap info
      | null (runtimeInfoFields info) = []
      | otherwise =
          [ "@"
              <> runtimeInfoLabel info
              <> "_bitmap = internal constant ["
              <> tshow (length (runtimeInfoFields info))
              <> " x i8] ["
              <> T.intercalate ", " ["i8 " <> if isPointerRuntimeRep field then "1" else "0" | field <- runtimeInfoFields info]
              <> "]"
          ]
    definition info =
      "@"
        <> runtimeInfoLabel info
        <> " = "
        <> (if "aihc_constructor_" `T.isPrefixOf` runtimeInfoLabel info then "" else "internal ")
        <> "constant %AihcInfo { i64 "
        <> maybe "0" (\symbol -> "ptrtoint (ptr @" <> symbol <> " to i64)") (runtimeInfoIdentity info)
        <> ", ptr "
        <> maybe "null" ("@" <>) (runtimeInfoEntry info)
        <> ", i64 "
        <> tshow (length (runtimeInfoFields info))
        <> ", i64 "
        <> tshow (runtimeInfoRemainingArity info)
        <> ", ptr "
        <> (if null (runtimeInfoFields info) then "null" else "@" <> runtimeInfoLabel info <> "_bitmap")
        <> ", ptr "
        <> maybe "null" ("@" <>) (runtimeInfoNext info)
        <> ", ptr "
        <> maybe "null" (const ("@" <> enterEntryLabel info)) (runtimeInfoEnter info)
        <> ", i64 "
        <> tshow (continuationFrameKindCode (runtimeInfoFrameKind info))
        <> ", i64 "
        <> tshow (runtimeInfoObjectKind info)
        <> ", ptr "
        <> maybe "null" ("@" <>) (runtimeInfoSrt info)
        <> " }, align 8"

renderForeignDeclarations :: GrinProgram -> [Text]
renderForeignDeclarations program =
  [ "declare "
      <> llvmForeignType (grinForeignResultType signature)
      <> " @"
      <> grinForeignCallSymbol foreignCall
      <> "("
      <> T.intercalate ", " (["ptr" | passMachine] <> map llvmForeignType (grinForeignArgumentTypes signature))
      <> ")"
  | (passMachine, foreignCall) <- foreignCalls,
    let signature = grinForeignCallSignature foreignCall
  ]
    <> ["" | not (null foreignCalls)]
  where
    foreignCalls =
      Map.elems . Map.fromList $
        [ (grinForeignCallSymbol foreignCall, call)
        | call@(_, foreignCall) <- [(False, programForeignCall) | programForeignCall <- grinForeignCalls program] <> runtimePrimitiveCalls
        ]
    runtimePrimitiveCalls =
      [ (nativeRuntimeCallPassMachine runtimeCall, nativeRuntimeCallForeignCall runtimeCall)
      | primitive <- supportedNativePrimitiveNames,
        runtimeCall <-
          maybeToList (nativeRuntimePrimitiveCall primitive)
            <> concat (maybeToList (nativeSplitRuntimePrimitiveCall primitive))
      ]

renderExternalFunctionDeclarations :: CompileEnv -> GrinProgram -> [Text]
renderExternalFunctionDeclarations env program =
  ["@" <> renderLinkedGlobalSymbol name <> " = external global i8" | name <- externalGlobals]
    <> ["@" <> label <> " = external constant %AihcInfo" | label <- externalConstructorInfos]
    <> ["" | not (null externalGlobals && null externalConstructorInfos)]
  where
    definedGlobals = Set.fromList (map fst (grinGlobals program) <> [name | (name, layouts) <- grinConstructors program, null layouts])
    -- Static reference tables name imported objects as well as local ones, so
    -- their names need declarations here even when no expression in this
    -- module mentions them explicitly.
    tableObjects =
      Set.fromList (concatMap srtObjects (Map.elems (staticReferenceTables (compileStaticReferences env))))
    externalGlobals =
      Set.toAscList ((Set.fromList (grinProgramGlobalReferences program) <> tableObjects) `Set.difference` definedGlobals)
    definedInfos = Set.fromList (map runtimeInfoLabel (compileRuntimeInfos env))
    externalConstructorInfos = Set.toAscList (programConstructorReferences program `Set.difference` definedInfos)

programConstructorReferences :: GrinProgram -> Set.Set Text
programConstructorReferences program =
  Set.fromList
    ( [renderLinkedConstructorInfoSymbol name remaining | GrinNode (GrinConstructor name remaining) _ <- programNodes program]
        <> concatMap (exprConstructorReferences . grinFunctionBody) (grinFunctions program)
    )

exprConstructorReferences :: GrinExpr -> [Text]
exprConstructorReferences expression =
  case expression of
    GrinBind _ value body -> exprConstructorReferences value <> exprConstructorReferences body
    GrinStoreRec bindings body -> concatMap (nodeReference . snd) bindings <> exprConstructorReferences body
    GrinStoreRecUnchecked bindings body -> concatMap (nodeReference . snd) bindings <> exprConstructorReferences body
    GrinCase _ _ alternatives -> concatMap alternativeReferences alternatives
    _ -> []
  where
    nodeReference (GrinNode (GrinConstructor name remaining) _) = [renderLinkedConstructorInfoSymbol name remaining]
    nodeReference _ = []
    alternativeReferences alternative =
      case grinAltCon alternative of
        GrinDataAlt name -> renderLinkedConstructorInfoSymbol name 0 : exprConstructorReferences (grinAltRhs alternative)
        _ -> exprConstructorReferences (grinAltRhs alternative)

renderAddrLiterals :: CompileEnv -> [Text]
renderAddrLiterals env =
  [ "@"
      <> label
      <> " = internal constant ["
      <> tshow (BS.length bytes + 1)
      <> " x i8] ["
      <> T.intercalate ", " ["i8 " <> tshow byte | byte <- BS.unpack bytes <> [0]]
      <> "]"
  | (bytes, label) <- Map.toAscList (compileAddrLiteralLabels env)
  ]
    <> ["" | not (Map.null (compileAddrLiteralLabels env))]

llvmPreamble :: [Text]
llvmPreamble =
  [ "; Generated by AIHC's LLVM backend.",
    "%AihcInfo = type { i64, ptr, i64, i64, ptr, ptr, ptr, i64, i64, ptr }",
    "%AihcResume = type { i64, ptr, ptr, i64, i64 }",
    "%AihcMachinePrefix = type { ptr, i64, ptr }",
    ""
  ]

renderRuntimeDeclarations :: [Text]
renderRuntimeDeclarations =
  [ "@aihc_current_srt = external global ptr",
    "declare ptr @aihc_machine_new(i64)",
    "declare void @aihc_program_arguments_initialize(i32, ptr)",
    "declare ptr @aihc_make_node(ptr, ptr)",
    "declare ptr @aihc_make_node_unchecked(ptr, ptr)",
    "declare void @aihc_ensure_heap(ptr, i64, i64, ptr)",
    "declare void @aihc_set_field(ptr, i64, i64)",
    "declare void @aihc_update(ptr, ptr)",
    "declare void @aihc_update_blackhole(ptr, ptr, ptr)",
    "declare ptr @aihc_apply_slow(ptr, ptr, i64, ptr, ptr)",
    "declare void @aihc_begin_blackhole(ptr, ptr)",
    "declare ptr @aihc_block_on_blackhole(ptr, ptr, ptr)",
    "declare ptr @aihc_raise(ptr, ptr, ptr)",
    "declare i64 @aihc_fork(ptr, ptr)",
    "declare ptr @aihc_mvar_new(ptr)",
    "declare ptr @aihc_mvar_read(ptr, ptr, ptr)",
    "declare ptr @aihc_mvar_take(ptr, ptr, ptr)",
    "declare ptr @aihc_mvar_put(ptr, ptr, i64, ptr)",
    "declare ptr @aihc_yield(ptr, ptr)",
    "declare ptr @aihc_await_io(ptr, ptr, ptr)",
    "declare ptr @aihc_thread_done(ptr)",
    "declare void @aihc_set_thread_done_continuation(ptr, ptr)",
    "declare void @aihc_exit_process(i64) noreturn",
    "declare ptr @aihc_halt(ptr)",
    "declare void @aihc_no_match()",
    "declare void @aihc_unsupported_primitive()",
    ""
  ]

llvmForeignType :: GrinForeignType -> Text
llvmForeignType foreignType =
  case foreignType of
    GrinForeignInt -> "i64"
    GrinForeignInt32 -> "i32"
    GrinForeignWord64 -> "i64"
    GrinForeignAddr -> "ptr"

lookupRuntimeInfoLabel :: CompileEnv -> RuntimeInfoKey -> Either LlvmError Text
lookupRuntimeInfoLabel env key =
  case Map.lookup key (compileNodeInfoLabels env) of
    Just label -> Right label
    Nothing -> case key of
      ConstructorRuntimeInfo name remaining -> Right (renderLinkedConstructorInfoSymbol name remaining)
      ClosureRuntimeInfo functionName _ _ -> Left (LlvmMissingFunction functionName)
      ThunkRuntimeInfo functionName _ -> Left (LlvmMissingFunction functionName)

functionCodeLabel :: CompileEnv -> FunctionName -> Either LlvmError Text
functionCodeLabel env name = maybe (Left (LlvmMissingFunction name)) Right (Map.lookup name (compileFunctionLabels env))

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

functionLocalSlots :: GrinFunction -> Map GrinVar Int
functionLocalSlots function = snd (foldl assignGroup (0, Map.empty) groups)
  where
    groups = grinFunctionParameters function : boundVarGroups (grinFunctionBody function)
    assignGroup = foldl $ \(next, slots) var -> case Map.lookup var slots of
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

validateRuntimeRep :: GrinRep -> Either LlvmError ()
validateRuntimeRep runtimeRep = case runtimeRep of
  VecRep {} -> Left (LlvmUnsupportedRuntimeRep runtimeRep)
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
    modulus width = 1 `shiftL` width
    unsigned width = integer `mod` modulus width
    signed width =
      let value = unsigned width
          sign = 1 `shiftL` (width - 1)
       in if value >= sign then value - modulus width else value

renderI64 :: Integer -> Text
renderI64 integer = tshow (integer `mod` (2 ^ (64 :: Int)))

localFunctionLabel :: Int -> GrinFunction -> Text
localFunctionLabel index _function = "aihc_llvm_function_" <> tshow index

llvmLabel :: Text -> Text
llvmLabel = T.map (\character -> if character `elem` ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ['_'] then character else '_')

functionLinkage :: GrinFunction -> Text
functionLinkage _function = "internal "

boolInteger :: Bool -> Text
boolInteger True = "1"
boolInteger False = "0"

nativeDataSection :: Text -> Text
nativeDataSection name
  | System.os == "darwin" = "__DATA,__" <> name
  | otherwise = name

tshow :: (Show value) => value -> Text
tshow = T.pack . show

liftEither :: Either LlvmError value -> FunctionM value
liftEither = either (lift . Left) pure
