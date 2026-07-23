{-# LANGUAGE OverloadedStrings #-}

-- | Lower runtime-explicit GRIN to textual LLVM IR.
--
-- Generated computation and continuation entries use @tailcc@. Every CPS
-- transfer is a @musttail@ call followed immediately by @ret void@, so LLVM
-- verifies the no-growing-stack invariant instead of merely optimizing it by
-- convention. Runtime control helpers return an entry and an argument buffer;
-- the backend expands that buffer back into LLVM call operands before the
-- guaranteed tail transfer.
module Aihc.Llvm.Codegen
  ( LlvmError (..),
    compileModule,
    compileProgram,
    compileProgramWithDependencies,
    validatePrimitiveNames,
    validateProgramPrimitives,
  )
where

import Aihc.Grin.Gc (GcGrinProgram, gcGrinProgram, gcUpdateFunction)
import Aihc.Grin.Syntax
import Aihc.Native
  ( LinkLayout (..),
    buildAddrLiteralPool,
    buildLinkLayout,
    nativeRuntimePrimitiveCall,
    supportedNativePrimitiveNames,
  )
import Aihc.Tc.Types (Levity (..), RuntimeRep (..))
import Control.Monad (forM, replicateM, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify', runStateT)
import Data.Bits (shiftL)
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)

data LlvmError
  = LlvmMissingEntry !Text
  | LlvmMissingGlobal !Text
  | LlvmMissingFunction !FunctionName
  | LlvmMissingConstructor !Text
  | LlvmUnsupportedPrimitive !Text
  | LlvmUnsupportedExpression !Text
  | LlvmUnsupportedValue !Text
  | LlvmUnsupportedRuntimeRep !RuntimeRep
  deriving (Eq, Show)

data CompilationUnit
  = ExecutableUnit
  | LibraryUnit
  deriving (Eq)

data CompileEnv = CompileEnv
  { compileConstructorIds :: !(Map Text Int),
    compileConstructorArities :: !(Map Text Int),
    compileGlobalSlots :: !(Map Text Int),
    compileFunctionLabels :: !(Map FunctionName Text),
    compileAddrLiteralLabels :: !(Map BS.ByteString Text),
    compileNodeInfoLabels :: !(Map RuntimeInfoKey Text),
    compileRuntimeInfos :: ![RuntimeInfo],
    compileArgumentCapacity :: !Int,
    compileAllowUnsupportedPrimitives :: !Bool
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

compileProgram :: Text -> GcGrinProgram -> Either LlvmError Text
compileProgram entryName gcProgram =
  compileProgramWithDependencies (buildLinkLayout [program]) [] entryName gcProgram
  where
    program = gcGrinProgram gcProgram

compileProgramWithDependencies :: LinkLayout -> [Text] -> Text -> GcGrinProgram -> Either LlvmError Text
compileProgramWithDependencies layout dependencyInitializers entryName gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  validateProgramPrimitives program
  rootSlot <- maybe (Left (LlvmMissingEntry entryName)) Right (Map.lookup entryName (compileGlobalSlots env))
  updateLabel <- functionCodeLabel env (gcUpdateFunction gcProgram)
  functions <- mapM (compileFunction env) (grinFunctions program)
  (constructorInitialization, initialization) <- runInitializer $ do
    constructors <- compileConstructorInitializers env
    globals <- compileInitializers env program
    pure (constructors, globals)
  let specialInfos =
        [ specialInfo "aihc_llvm_final_info" "aihc_llvm_final_continuation" [] 1 (Just "aihc_llvm_final_applied_info"),
          specialInfo "aihc_llvm_final_applied_info" "aihc_llvm_final_continuation" [BoxedRep Lifted] 0 Nothing,
          specialInfo "aihc_llvm_top_info" "aihc_llvm_top_continuation" [BoxedRep Lifted] 1 (Just "aihc_llvm_top_applied_info"),
          specialInfo "aihc_llvm_top_applied_info" "aihc_llvm_top_continuation" [BoxedRep Lifted, BoxedRep Lifted] 0 Nothing,
          specialInfo "aihc_llvm_update_info" updateLabel [BoxedRep Lifted, BoxedRep Lifted] 1 (Just "aihc_llvm_update_applied_info"),
          specialInfo "aihc_llvm_update_applied_info" updateLabel [BoxedRep Lifted, BoxedRep Lifted, BoxedRep Lifted] 0 Nothing,
          specialInfo "aihc_llvm_thread_done_info" "aihc_llvm_thread_done_continuation" [] 1 (Just "aihc_llvm_thread_done_applied_info"),
          specialInfo "aihc_llvm_thread_done_applied_info" "aihc_llvm_thread_done_continuation" [BoxedRep Lifted] 0 Nothing
        ]
      source =
        llvmPreamble
          <> [ "@aihc_machine = global ptr null, align 8",
               argumentBufferDefinition env,
               ""
             ]
          <> renderRuntimeDeclarations
          <> renderForeignDeclarations program
          <> renderExternalFunctionDeclarations env program
          <> ["declare void @" <> initializer <> "()" | initializer <- dependencyInitializers]
          <> ["" | not (null dependencyInitializers)]
          <> renderAddrLiterals env
          <> renderRuntimeInfos (compileRuntimeInfos env <> specialInfos)
          <> concatMap compiledFunctionLines functions
          <> renderSpecialFunctions env
          <> renderMain env rootSlot dependencyInitializers constructorInitialization initialization
  pure (T.unlines source)
  where
    program = gcGrinProgram gcProgram
    env = compileEnvironment ExecutableUnit layout program
    specialInfo label entry = RuntimeInfo label Nothing (Just entry)

compileModule :: LinkLayout -> Text -> GcGrinProgram -> Either LlvmError Text
compileModule layout initializerSymbol gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  functions <- mapM (compileFunction env) (grinFunctions program)
  initialization <- runInitializer (compileInitializers env program)
  let source =
        llvmPreamble
          <> [ "@aihc_machine = external global ptr",
               argumentBufferDefinition env,
               ""
             ]
          <> renderRuntimeDeclarations
          <> renderForeignDeclarations program
          <> renderExternalFunctionDeclarations env program
          <> renderAddrLiterals env
          <> renderRuntimeInfos (compileRuntimeInfos env)
          <> concatMap compiledFunctionLines functions
          <> [ "define void @" <> initializerSymbol <> "() {",
               "entry:",
               "  %machine = load ptr, ptr @aihc_machine, align 8"
             ]
          <> indent initialization
          <> ["  ret void", "}", ""]
  pure (T.unlines source)
  where
    program = gcGrinProgram gcProgram
    env = compileEnvironment LibraryUnit layout program

validateProgramPrimitives :: GrinProgram -> Either LlvmError ()
validateProgramPrimitives = validatePrimitiveNames . map (grinVarName . fst) . grinPrimitives

validatePrimitiveNames :: [Text] -> Either LlvmError ()
validatePrimitiveNames = mapM_ $ \name ->
  if name `elem` supportedNativePrimitiveNames
    then Right ()
    else Left (LlvmUnsupportedPrimitive name)

compileEnvironment :: CompilationUnit -> LinkLayout -> GrinProgram -> CompileEnv
compileEnvironment unitKind layout program =
  CompileEnv
    { compileConstructorIds = Map.fromList (zip (map fst constructors) [1 ..]),
      compileConstructorArities = Map.fromList constructors,
      compileGlobalSlots = Map.fromList (zip (linkGlobalNames layout) [0 ..]),
      compileFunctionLabels = functionLabels,
      compileAddrLiteralLabels = Map.fromList [(bytes, llvmLabel label) | (bytes, label) <- buildAddrLiteralPool program],
      compileNodeInfoLabels = Map.fromList [(key, label) | (key, label, _) <- constructorEntries <> functionEntries],
      compileRuntimeInfos = map third (constructorEntries <> functionEntries),
      compileArgumentCapacity = max 3 (linkMaximumArgumentSlots layout + 1),
      compileAllowUnsupportedPrimitives = unitKind == LibraryUnit
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
        let label = "aihc_llvm_constructor_info_" <> tshow identifier <> "_remaining_" <> tshow remaining
            fields = concat (take (arity - remaining) layouts)
            next = if remaining == 0 then Nothing else Just ("aihc_llvm_constructor_info_" <> tshow identifier <> "_remaining_" <> tshow (remaining - 1))
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
        let label = "aihc_llvm_function_info_" <> tshow index
      ]
    infoLabels = Map.fromList [(key, "aihc_llvm_function_info_" <> tshow index) | (index, key) <- zip [0 :: Int ..] infoKeys]
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
          <> ["  br label %body"]
      blocks = concatMap renderBlock (reverse (functionBlocksRev final))
  pure (CompiledFunction (header <> blocks <> ["}", ""]))
  where
    renderParameters names = T.intercalate ", " ("ptr %machine" : map ("i64 " <>) names)

newtype CompiledFunction = CompiledFunction
  { compiledFunctionLines :: [Text]
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
      transfer <-
        case pointerOperands of
          [valuePointer, continuationPointer, updatePointer] ->
            callPortableTransfer
              env
              "aihc_portable_eval_cps"
              [ "ptr %machine",
                argumentBufferOperand (valueCompileEnv env),
                "ptr " <> valuePointer,
                "i64 " <> boolInteger (isLiftedRuntimeRep runtimeRep),
                "ptr " <> continuationPointer,
                "ptr " <> updatePointer
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
      (arrayLines, arrayOperand) <- storeOperandArray argumentOperands
      (pointerLines, pointerOperands) <- pointerArguments [functionOperand, continuationOperand]
      transfer <-
        case pointerOperands of
          [functionPointer, continuationPointer] ->
            callPortableTransfer
              env
              "aihc_portable_apply_cps"
              [ "ptr %machine",
                argumentBufferOperand (valueCompileEnv env),
                "ptr " <> functionPointer,
                "i64 " <> tshow (length arguments),
                "ptr " <> arrayOperand,
                "ptr " <> continuationPointer
              ]
          _ -> lift (Left (LlvmUnsupportedExpression "internal CPS apply pointer arity"))
      terminal label (prefix <> functionLines <> continuationLines <> argumentLines <> arrayLines <> pointerLines <> transfer)
    GrinContinue continuation values -> do
      (continuationLines, continuationOperand) <- materializeValue env continuation
      (valueLines, valueOperands) <- materializeValues env values
      (arrayLines, arrayOperand) <- storeOperandArray valueOperands
      (pointerLines, pointerOperands) <- pointerArguments [continuationOperand]
      transfer <-
        case pointerOperands of
          [continuationPointer] ->
            callPortableTransfer
              env
              "aihc_portable_continue_values"
              [ "ptr %machine",
                argumentBufferOperand (valueCompileEnv env),
                "ptr " <> continuationPointer,
                "i64 " <> tshow (length values),
                "ptr " <> arrayOperand
              ]
          _ -> lift (Left (LlvmUnsupportedExpression "internal continuation pointer arity"))
      terminal label (prefix <> continuationLines <> valueLines <> arrayLines <> pointerLines <> transfer)
    GrinHalt {} -> do
      entry <- freshValue
      let zeroArguments = replicate (compileArgumentCapacity (valueCompileEnv env)) "0"
      terminal
        label
        ( prefix
            <> ["  " <> entry <> " = call ptr @aihc_halt(ptr %machine)"]
            <> indirectTailCall entry zeroArguments
        )
    GrinCase scrutinee binder alternatives -> compileCase env prefix label scrutinee binder alternatives
    GrinConstant {} -> unsupported "direct-style constant return after CPS"
    GrinStore {} -> unsupported "direct-style store return after CPS"
    GrinEnsureHeap {} -> unsupported "unbound heap reservation"
    GrinStoreUnchecked {} -> unsupported "unbound unchecked store"
    GrinFetch {} -> unsupported "direct-style fetch return after CPS"
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

compileCpsPrimitive :: ValueEnv -> [Text] -> Text -> Text -> [GrinValue] -> GrinValue -> FunctionM ()
compileCpsPrimitive env prefix label name arguments continuation =
  case (name, arguments) of
    ("awaitIO#", [request]) -> transfer "aihc_portable_await_io_cps" [request, continuation]
    ("fork#", [action]) -> transfer "aihc_portable_fork_cps" [action, continuation]
    ("yield#", []) -> transfer "aihc_portable_yield_cps" [continuation]
    _
      | compileAllowUnsupportedPrimitives (valueCompileEnv env) ->
          addBlock label (prefix <> ["  call void @aihc_unsupported_primitive()", "  unreachable"])
    _ -> lift (Left (LlvmUnsupportedExpression ("CPS primitive call " <> name)))
  where
    transfer function values = do
      (lines', operands) <- materializeValues env values
      (pointerLines, pointerOperands) <- pointerArguments operands
      transferLines <-
        callPortableTransfer
          env
          function
          (["ptr %machine", argumentBufferOperand (valueCompileEnv env)] <> map ("ptr " <>) pointerOperands)
      addBlock label (prefix <> lines' <> pointerLines <> transferLines <> ["  ret void"])

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
            ( rootLines
                <> ["  " <> rootsArray <> " = alloca [" <> tshow (max 1 (length roots)) <> " x i64], align 8"]
                <> elementStores
                <> ["  call void @aihc_ensure_heap(ptr %machine, i64 " <> tshow requiredWords <> ", i64 " <> tshow (length roots) <> ", ptr " <> rootsArray <> ")"]
                <> relocated
            )
    GrinStoreUnchecked node -> materializeNode env True node >>= storeOne
    GrinFetch _ pointer -> do
      (pointerLines, pointerOperand) <- materializeValue env pointer
      (objectLines, object) <- intToPtr pointerOperand
      fieldPointer <- freshValue
      value <- freshValue
      storeOne
        ( pointerLines
            <> objectLines
            <> [ "  " <> fieldPointer <> " = getelementptr i64, ptr " <> object <> ", i64 1",
                 "  " <> value <> " = load i64, ptr " <> fieldPointer <> ", align 8"
               ],
          value
        )
    GrinUpdate pointer value -> update "aihc_update" False pointer value
    GrinUpdateBlackhole pointer value -> update "aihc_update_blackhole" True pointer value
    GrinPrimitiveCall IntRep "+#" [left, right] -> do
      (lines', operands) <- materializeValues env [left, right]
      case operands of
        [leftOperand, rightOperand] -> do
          result <- freshValue
          storeOne (lines' <> ["  " <> result <> " = add i64 " <> leftOperand <> ", " <> rightOperand], result)
        _ -> internalArity "binary Int# primitive"
    GrinPrimitiveCall runtimeRep "realWorld#" []
      | null vars && null (runtimeRepComponents runtimeRep) -> pure []
    GrinPrimitiveCall _ name [value]
      | name `elem` ["unsafeFreezeByteArray#", "unsafeThawByteArray#"] ->
          materializeValue env value >>= storeOne
    GrinPrimitiveCall _ name arguments
      | Just foreignCall <- nativeRuntimePrimitiveCall name -> do
          result <- compileForeignCall env foreignCall arguments
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
  checks <- caseSwitch env (snd discriminatorLines) targets
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

caseSwitch :: ValueEnv -> Text -> [(GrinAlt, Text)] -> FunctionM [Text]
caseSwitch env discriminator targets = do
  cases <- fmap concat . forM nonDefault $ \(alternative, target) ->
    case grinAltCon alternative of
      GrinDataAlt name -> do
        identifier <- liftEither (constructorId (valueCompileEnv env) name)
        pure [(toInteger identifier, target)]
      GrinLitAlt literal ->
        case normalizedLiteralInteger literal of
          Just integer -> pure [(integer, target)]
          Nothing -> lift (Left (LlvmUnsupportedValue "string case alternative"))
      GrinDefaultAlt -> pure []
  defaultLabel <-
    case [target | (alternative, target) <- targets, grinAltCon alternative == GrinDefaultAlt] of
      target : _ -> pure target
      [] -> do
        missing <- freshLabel "case_no_match"
        addBlock missing ["  call void @aihc_no_match()", "  unreachable"]
        pure missing
  pure
    ( ["  switch i64 " <> discriminator <> ", label %" <> defaultLabel <> " ["]
        <> ["    i64 " <> renderI64 integer <> ", label %" <> target | (integer, target) <- cases]
        <> ["  ]"]
    )
  where
    nonDefault = [(alternative, target) | (alternative, target) <- targets, grinAltCon alternative /= GrinDefaultAlt]

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
        Nothing -> do
          slot <- liftEither (globalSlot (valueCompileEnv env) (grinVarName var))
          loadGlobal slot
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

materializeNode :: ValueEnv -> Bool -> GrinNode -> FunctionM ([Text], Text)
materializeNode env unchecked node = do
  (tag, info) <- liftEither (nodeHeader (valueCompileEnv env) node)
  object <- freshValue
  objectInteger <- freshValue
  fields <- initializeLocalFields env objectInteger node
  pure
    ( [ "  "
          <> object
          <> " = call ptr @"
          <> (if unchecked then "aihc_make_node_unchecked" else "aihc_make_node")
          <> "(ptr %machine, i64 "
          <> tshow tag
          <> ", ptr @"
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

nodeHeader :: CompileEnv -> GrinNode -> Either LlvmError (Int, Text)
nodeHeader env node = do
  info <- lookupRuntimeInfoLabel env key
  pure (tag, info)
  where
    fields = map grinValueRuntimeRep (grinNodeFields node)
    (tag, key) =
      case grinNodeTag node of
        GrinConstructor name remaining -> (if remaining == 0 then 0 else 3, ConstructorRuntimeInfo name remaining)
        GrinClosure functionName layouts -> (1, ClosureRuntimeInfo functionName fields layouts)
        GrinThunk functionName -> (2, ThunkRuntimeInfo functionName fields)

compileConstructorInitializers :: CompileEnv -> FunctionM [Text]
compileConstructorInitializers env = fmap concat . forM nullary $ \(name, _) -> do
  slot <- liftEither (globalSlot env name)
  info <- liftEither (lookupRuntimeInfoLabel env (ConstructorRuntimeInfo name 0))
  object <- freshValue
  objectInteger <- freshValue
  globalStore <- storeGlobal slot objectInteger
  pure
    ( [ "  " <> object <> " = call ptr @aihc_make_node(ptr %machine, i64 0, ptr @" <> info <> ")",
        "  " <> objectInteger <> " = ptrtoint ptr " <> object <> " to i64"
      ]
        <> globalStore
    )
  where
    nullary =
      [ (name, identifier)
      | (name, identifier) <- Map.toAscList (compileConstructorIds env),
        Map.lookup name (compileConstructorArities env) == Just 0
      ]

compileInitializers :: CompileEnv -> GrinProgram -> FunctionM [Text]
compileInitializers env program = do
  cafAllocations <- fmap concat . forM (grinCafs program) $ \(var, node) -> do
    slot <- liftEither (globalSlot env (grinVarName var))
    (tag, info) <- liftEither (nodeHeader env node)
    object <- freshValue
    objectInteger <- freshValue
    globalStore <- storeGlobal slot objectInteger
    pure
      ( [ "  " <> object <> " = call ptr @aihc_make_node(ptr %machine, i64 " <> tshow tag <> ", ptr @" <> info <> ")",
          "  " <> objectInteger <> " = ptrtoint ptr " <> object <> " to i64"
        ]
          <> globalStore
      )
  whnfs <- fmap concat . forM (grinWhnfGlobals program) $ \(var, node) -> do
    slot <- liftEither (globalSlot env (grinVarName var))
    (lines', operand) <- materializeNode (ValueEnv env Map.empty) False node
    globalStore <- storeGlobal slot operand
    pure (lines' <> globalStore)
  cafFields <- fmap concat . forM (grinCafs program) $ \(var, node) -> do
    slot <- liftEither (globalSlot env (grinVarName var))
    (objectLines, object) <- loadGlobal slot
    fields <- initializeLocalFields (ValueEnv env Map.empty) object node
    pure (objectLines <> fields)
  pure (cafAllocations <> whnfs <> cafFields)

runInitializer :: FunctionM value -> Either LlvmError value
runInitializer action = fst <$> runStateT action (FunctionState 0 0 0 [])

renderMain :: CompileEnv -> Int -> [Text] -> [Text] -> [Text] -> [Text]
renderMain env rootSlot dependencyInitializers constructorInitialization initialization =
  [ "define i32 @main() {",
    "entry:",
    "  %machine = call ptr @aihc_machine_new(i64 " <> tshow (Map.size (compileGlobalSlots env)) <> ")",
    "  store ptr %machine, ptr @aihc_machine, align 8"
  ]
    <> constructorInitialization
    <> ["  call void @" <> initializer <> "()" | initializer <- dependencyInitializers]
    <> initialization
    <> [ "  call void @aihc_ensure_heap(ptr %machine, i64 7, i64 0, ptr null)",
         "  %final = call ptr @aihc_make_node_unchecked(ptr %machine, i64 1, ptr @aihc_llvm_final_info)",
         "  %top = call ptr @aihc_make_node_unchecked(ptr %machine, i64 1, ptr @aihc_llvm_top_info)",
         "  %final_i64 = ptrtoint ptr %final to i64",
         "  call void @aihc_set_field(ptr %top, i64 0, i64 %final_i64)",
         "  %update = call ptr @aihc_make_node_unchecked(ptr %machine, i64 1, ptr @aihc_llvm_update_info)",
         "  %globals_field = getelementptr %AihcMachinePrefix, ptr %machine, i32 0, i32 0",
         "  %globals = load ptr, ptr %globals_field, align 8",
         "  %root_slot = getelementptr i64, ptr %globals, i64 " <> tshow rootSlot,
         "  %root = load i64, ptr %root_slot, align 8",
         "  call void @aihc_set_field(ptr %update, i64 0, i64 %root)",
         "  %top_i64 = ptrtoint ptr %top to i64",
         "  call void @aihc_set_field(ptr %update, i64 1, i64 %top_i64)",
         "  %thread_done = call ptr @aihc_make_node_unchecked(ptr %machine, i64 1, ptr @aihc_llvm_thread_done_info)",
         "  %root_ptr = inttoptr i64 %root to ptr",
         "  %transfer = call { ptr, ptr } @aihc_portable_start(ptr %machine, "
           <> argumentBufferOperand env
           <> ", ptr %root_ptr, ptr %top, ptr %update, ptr %thread_done, ptr @aihc_llvm_exit)",
         "  %entry_function = extractvalue { ptr, ptr } %transfer, 0",
         "  %entry_arguments = extractvalue { ptr, ptr } %transfer, 1"
       ]
    <> loadTransferArguments env "%entry_arguments" "%entry_arg_"
    <> [ "  call tailcc void %entry_function(" <> renderCallArguments "%machine" ["%entry_arg_" <> tshow index | index <- [0 .. compileArgumentCapacity env - 1]] <> ")",
         "  ret i32 0",
         "}",
         ""
       ]

renderSpecialFunctions :: CompileEnv -> [Text]
renderSpecialFunctions env =
  renderSpecial "aihc_llvm_top_continuation" 2 topBody
    <> renderSpecial "aihc_llvm_thread_done_continuation" 0 threadDoneBody
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
        "  %transfer = call { ptr, ptr } @aihc_portable_apply_cps(ptr %machine, "
          <> argumentBufferOperand env
          <> ", ptr %function, i64 0, ptr null, ptr %continuation)"
      ]
        <> dispatchNamedTransfer env "%transfer" "%top"
        <> ["  ret void"]
    threadDoneBody =
      ["  %transfer = call { ptr, ptr } @aihc_portable_thread_done(ptr %machine, " <> argumentBufferOperand env <> ")"]
        <> dispatchNamedTransfer env "%transfer" "%thread_done"
        <> ["  ret void"]
    finalBody =
      ["  %exit = call ptr @aihc_halt(ptr %machine)"]
        <> indirectTailCall "%exit" (replicate (compileArgumentCapacity env) "0")
        <> ["  ret void"]

callPortableTransfer :: ValueEnv -> Text -> [Text] -> FunctionM [Text]
callPortableTransfer env function arguments = do
  transfer <- freshValue
  entry <- freshValue
  buffer <- freshValue
  argumentNames <- replicateM (compileArgumentCapacity compileEnv) freshValue
  let loads =
        concat
          [ [ "  " <> pointer <> " = getelementptr i64, ptr " <> buffer <> ", i64 " <> tshow index,
              "  " <> argument <> " = load i64, ptr " <> pointer <> ", align 8"
            ]
          | (index, argument) <- zip [0 :: Int ..] argumentNames,
            let pointer = argument <> "_ptr"
          ]
  pure
    ( [ "  " <> transfer <> " = call { ptr, ptr } @" <> function <> "(" <> T.intercalate ", " arguments <> ")",
        "  " <> entry <> " = extractvalue { ptr, ptr } " <> transfer <> ", 0",
        "  " <> buffer <> " = extractvalue { ptr, ptr } " <> transfer <> ", 1"
      ]
        <> loads
        <> indirectTailCall entry argumentNames
    )
  where
    compileEnv = valueCompileEnv env

dispatchNamedTransfer :: CompileEnv -> Text -> Text -> [Text]
dispatchNamedTransfer env transfer prefix =
  [ "  " <> prefix <> "_entry = extractvalue { ptr, ptr } " <> transfer <> ", 0",
    "  " <> prefix <> "_arguments = extractvalue { ptr, ptr } " <> transfer <> ", 1"
  ]
    <> loadTransferArguments env (prefix <> "_arguments") (prefix <> "_arg_")
    <> [ "  musttail call tailcc void "
           <> prefix
           <> "_entry("
           <> renderCallArguments "%machine" [prefix <> "_arg_" <> tshow index | index <- [0 .. compileArgumentCapacity env - 1]]
           <> ")"
       ]

loadTransferArguments :: CompileEnv -> Text -> Text -> [Text]
loadTransferArguments env buffer prefix =
  concat
    [ [ "  " <> prefix <> tshow index <> "_ptr = getelementptr i64, ptr " <> buffer <> ", i64 " <> tshow index,
        "  " <> prefix <> tshow index <> " = load i64, ptr " <> prefix <> tshow index <> "_ptr, align 8"
      ]
    | index <- [0 .. compileArgumentCapacity env - 1]
    ]

directTailCall :: Text -> [Text] -> [Text]
directTailCall target operands =
  ["  musttail call tailcc void @" <> target <> "(" <> renderCallArguments "%machine" operands <> ")"]

indirectTailCall :: Text -> [Text] -> [Text]
indirectTailCall target operands =
  ["  musttail call tailcc void " <> target <> "(" <> renderCallArguments "%machine" operands <> ")"]

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
  infoInteger <- freshValue
  info <- freshValue
  identity <- freshValue
  pure
    ( objectLines
        <> [ "  " <> header <> " = load i64, ptr " <> object <> ", align 8",
             "  " <> infoInteger <> " = and i64 " <> header <> ", -8",
             "  " <> info <> " = inttoptr i64 " <> infoInteger <> " to ptr",
             "  " <> identity <> " = load i64, ptr " <> info <> ", align 8"
           ],
      identity
    )

convertForeignArgument :: GrinForeignType -> Text -> FunctionM ([Text], Text)
convertForeignArgument foreignType operand =
  case foreignType of
    GrinForeignInt32 -> do
      converted <- freshValue
      pure (["  " <> converted <> " = trunc i64 " <> operand <> " to i32"], converted)
    GrinForeignWord64 -> pure ([], operand)
    GrinForeignAddr -> intToPtr operand

convertForeignResult :: GrinForeignType -> Text -> FunctionM ([Text], Text)
convertForeignResult foreignType operand =
  case foreignType of
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

loadGlobal :: Int -> FunctionM ([Text], Text)
loadGlobal slot = do
  globalsField <- freshValue
  globals <- freshValue
  globalPointer <- freshValue
  result <- freshValue
  pure
    ( [ "  " <> globalsField <> " = getelementptr %AihcMachinePrefix, ptr %machine, i32 0, i32 0",
        "  " <> globals <> " = load ptr, ptr " <> globalsField <> ", align 8",
        "  " <> globalPointer <> " = getelementptr i64, ptr " <> globals <> ", i64 " <> tshow slot,
        "  " <> result <> " = load i64, ptr " <> globalPointer <> ", align 8"
      ],
      result
    )

storeGlobal :: Int -> Text -> FunctionM [Text]
storeGlobal slot operand = do
  globalsField <- freshValue
  globals <- freshValue
  globalPointer <- freshValue
  pure
    [ "  " <> globalsField <> " = getelementptr %AihcMachinePrefix, ptr %machine, i32 0, i32 0",
      "  " <> globals <> " = load ptr, ptr " <> globalsField <> ", align 8",
      "  " <> globalPointer <> " = getelementptr i64, ptr " <> globals <> ", i64 " <> tshow slot,
      "  store i64 " <> operand <> ", ptr " <> globalPointer <> ", align 8"
    ]

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

argumentBufferDefinition :: CompileEnv -> Text
argumentBufferDefinition env =
  "@aihc_llvm_arguments = internal global ["
    <> tshow (compileArgumentCapacity env)
    <> " x i64] zeroinitializer, align 8"

argumentBufferOperand :: CompileEnv -> Text
argumentBufferOperand _ = "ptr @aihc_llvm_arguments"

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
        <> " = internal constant %AihcInfo { i64 "
        <> maybe "0" tshow (runtimeInfoIdentity info)
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
        <> ", ptr null }, align 8"

renderForeignDeclarations :: GrinProgram -> [Text]
renderForeignDeclarations program =
  [ "declare "
      <> llvmForeignType (grinForeignResultType signature)
      <> " @"
      <> grinForeignCallSymbol foreignCall
      <> "("
      <> T.intercalate ", " (map llvmForeignType (grinForeignArgumentTypes signature))
      <> ")"
  | foreignCall <- foreignCalls,
    let signature = grinForeignCallSignature foreignCall
  ]
    <> ["" | not (null foreignCalls)]
  where
    foreignCalls =
      Map.elems . Map.fromList $
        [ (grinForeignCallSymbol foreignCall, foreignCall)
        | foreignCall <- grinForeignCalls program <> runtimePrimitiveCalls
        ]
    runtimePrimitiveCalls =
      [ foreignCall
      | primitive <- supportedNativePrimitiveNames,
        Just foreignCall <- [nativeRuntimePrimitiveCall primitive]
      ]

renderExternalFunctionDeclarations :: CompileEnv -> GrinProgram -> [Text]
renderExternalFunctionDeclarations env program =
  [ "declare tailcc void @"
      <> label
      <> "("
      <> T.intercalate ", " ("ptr" : replicate (length (concat (grinCodeParameterLayouts info))) "i64")
      <> ")"
  | info <- grinExternalFunctions program,
    Just label <- [Map.lookup (grinCodeFunctionName info) (compileFunctionLabels env)]
  ]
    <> ["" | not (null (grinExternalFunctions program))]

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
    "%AihcInfo = type { i64, ptr, i64, i64, ptr, ptr, ptr }",
    "%AihcMachinePrefix = type { ptr }",
    ""
  ]

renderRuntimeDeclarations :: [Text]
renderRuntimeDeclarations =
  [ "declare ptr @aihc_machine_new(i64)",
    "declare ptr @aihc_make_node(ptr, i64, ptr)",
    "declare ptr @aihc_make_node_unchecked(ptr, i64, ptr)",
    "declare void @aihc_ensure_heap(ptr, i64, i64, ptr)",
    "declare void @aihc_set_field(ptr, i64, i64)",
    "declare void @aihc_update(ptr, ptr)",
    "declare void @aihc_update_blackhole(ptr, ptr, ptr)",
    "declare ptr @aihc_halt(ptr)",
    "declare void @aihc_no_match()",
    "declare void @aihc_unsupported_primitive()",
    "declare { ptr, ptr } @aihc_portable_apply_cps(ptr, ptr, ptr, i64, ptr, ptr)",
    "declare { ptr, ptr } @aihc_portable_eval_cps(ptr, ptr, ptr, i64, ptr, ptr)",
    "declare { ptr, ptr } @aihc_portable_continue_values(ptr, ptr, ptr, i64, ptr)",
    "declare { ptr, ptr } @aihc_portable_fork_cps(ptr, ptr, ptr, ptr)",
    "declare { ptr, ptr } @aihc_portable_yield_cps(ptr, ptr, ptr)",
    "declare { ptr, ptr } @aihc_portable_await_io_cps(ptr, ptr, ptr, ptr)",
    "declare { ptr, ptr } @aihc_portable_thread_done(ptr, ptr)",
    "declare { ptr, ptr } @aihc_portable_start(ptr, ptr, ptr, ptr, ptr, ptr, ptr)",
    ""
  ]

llvmForeignType :: GrinForeignType -> Text
llvmForeignType foreignType =
  case foreignType of
    GrinForeignInt32 -> "i32"
    GrinForeignWord64 -> "i64"
    GrinForeignAddr -> "ptr"

globalSlot :: CompileEnv -> Text -> Either LlvmError Int
globalSlot env name = maybe (Left (LlvmMissingGlobal name)) Right (Map.lookup name (compileGlobalSlots env))

constructorId :: CompileEnv -> Text -> Either LlvmError Int
constructorId env name = maybe (Left (LlvmMissingConstructor name)) Right (Map.lookup name (compileConstructorIds env))

lookupRuntimeInfoLabel :: CompileEnv -> RuntimeInfoKey -> Either LlvmError Text
lookupRuntimeInfoLabel env key =
  case Map.lookup key (compileNodeInfoLabels env) of
    Just label -> Right label
    Nothing -> case key of
      ConstructorRuntimeInfo name _ -> Left (LlvmMissingConstructor name)
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

validateRuntimeRep :: RuntimeRep -> Either LlvmError ()
validateRuntimeRep runtimeRep = case runtimeRep of
  VecRep {} -> Left (LlvmUnsupportedRuntimeRep runtimeRep)
  TupleRep reps -> mapM_ validateRuntimeRep reps
  SumRep reps -> mapM_ validateRuntimeRep reps
  RuntimeRepVar {} -> Left (LlvmUnsupportedRuntimeRep runtimeRep)
  RuntimeRepMeta {} -> Left (LlvmUnsupportedRuntimeRep runtimeRep)
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
    modulus width = 1 `shiftL` width
    unsigned width = integer `mod` modulus width
    signed width =
      let value = unsigned width
          sign = 1 `shiftL` (width - 1)
       in if value >= sign then value - modulus width else value

renderI64 :: Integer -> Text
renderI64 integer = tshow (integer `mod` (2 ^ (64 :: Int)))

localFunctionLabel :: Int -> GrinFunction -> Text
localFunctionLabel index function = maybe ("aihc_llvm_function_" <> tshow index) linkedFunctionLabel (grinFunctionLinkName function)

linkedFunctionLabel :: Text -> Text
linkedFunctionLabel name = "aihc_entry_" <> T.concatMap (\character -> T.pack (showHex (ord character) "_")) name

llvmLabel :: Text -> Text
llvmLabel = T.map (\character -> if character `elem` ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> ['_'] then character else '_')

functionLinkage :: GrinFunction -> Text
functionLinkage function = maybe "internal " (const "") (grinFunctionLinkName function)

boolInteger :: Bool -> Text
boolInteger True = "1"
boolInteger False = "0"

indent :: [Text] -> [Text]
indent = map ("  " <>)

tshow :: (Show value) => value -> Text
tshow = T.pack . show

liftEither :: Either LlvmError value -> FunctionM value
liftEither = either (lift . Left) pure
