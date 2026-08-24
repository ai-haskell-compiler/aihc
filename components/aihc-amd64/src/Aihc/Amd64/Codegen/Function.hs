{-# LANGUAGE OverloadedStrings #-}

-- | Lower individual CPS GRIN functions to AMD64 basic blocks.
module Aihc.Amd64.Codegen.Function
  ( compileFunction,
    reserveLocalsLines,
  )
where

import Aihc.Amd64.Codegen.Runtime
import Aihc.Amd64.RegisterAllocate qualified as RegisterAllocate
import Aihc.Grin.Syntax
import Aihc.Native
  ( NativeCpsCall (..),
    NativeCpsTransfer (..),
    NativeRuntimeCall (..),
    nativeCpsPrimitiveCall,
    nativeRuntimePrimitiveCall,
  )
import Aihc.Native.BlockLayout qualified as BlockLayout
import Aihc.Native.RegisterAllocate (Location (..), grinExprFreeVariables)
import Control.Monad (forM, forM_, replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (execStateT, get, modify')
import Data.Either (fromRight)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)

data MoveSource
  = MoveRegister !Text
  | MoveSpill !Int
  | MoveValue !GrinValue
  deriving (Eq)

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
exportLines env _function label
  | compileExposeAllFunctions env = [".globl " <> label]
  | otherwise = []

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
    GrinStoreRec bindings body -> compileStoreRec allocateNode bindings body
    GrinStoreRecUnchecked bindings body -> compileStoreRec allocateNodeUnchecked bindings body
    GrinFetch {} -> unsupportedExpression "direct-style fetch return after CPS"
    GrinUpdate {} -> unsupportedExpression "direct-style update return after CPS"
    GrinUpdateBlackhole {} -> unsupportedExpression "unbound blackhole update"
    GrinEval {} -> unsupportedExpression "direct-style eval after CPS"
    GrinCpsEval runtimeRep value continuation updateContinuation -> do
      valueSlot <- freshSlot
      continuationSlot <- freshSlot
      updateSlot <- freshSlot
      storedLines <-
        materializeIntoSlots env [(value, valueSlot), (continuation, continuationSlot), (updateContinuation, updateSlot)]
      addBlock
        label
        ( prefix
            <> storedLines
            <> [ loadAt applyFunctionRegister "r14" valueSlot,
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
            then compileTransfer target arguments []
            else case reverse arguments of
              continuation : reversedValues -> do
                let values = reverse reversedValues
                continuationLines <- liftEither (materializeValueTo env applyContinuationRegister continuation)
                compileTransfer target values continuationLines
              [] -> unsupportedExpression "direct CPS call has no continuation"
    GrinPrimitiveCall {} -> unsupportedExpression "unbound primitive call after CPS"
    GrinCpsPrimitiveCall _ name arguments continuation ->
      compileCpsPrimitive env prefix label name arguments continuation
    GrinApply {} -> unsupportedExpression "direct-style apply after CPS"
    GrinCpsApply _ function arguments continuation -> do
      scratch <- freshSlot
      continuationSlot <- freshSlot
      slowLabel <- freshLabel (valueLabelPrefix env) "apply_slow"
      argumentSlots <- freshSlots (length arguments)
      storedLines <-
        materializeIntoSlots env $
          zip (function : continuation : arguments) (scratch : continuationSlot : argumentSlots)
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
            <> storedLines
            <> [ loadAt applyFunctionRegister "r14" scratch,
                 loadAt applyContinuationRegister "r14" continuationSlot
               ]
            <> [loadAt register "r14" slot | (register, slot) <- zip applyArgumentRegisters argumentSlots]
            <> saveApplyOverflowLines "r14" argumentSlots
            <> [ "  mov r11, QWORD PTR [" <> applyFunctionRegister <> "]",
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
    GrinCpsRaise exception continuation -> do
      exceptionSlot <- freshSlot
      continuationSlot <- freshSlot
      storedLines <- materializeIntoSlots env [(exception, exceptionSlot), (continuation, continuationSlot)]
      addBlock
        label
        ( prefix
            <> storedLines
            <> [ loadAt "rsi" "r14" exceptionSlot,
                 loadAt "rdx" "r14" continuationSlot,
                 "  mov rdi, r15",
                 "  call aihc_raise"
               ]
        )
        (BlockLayout.Jump ".Laihc_resume")
    GrinHalt _ ->
      addBlock
        label
        (prefix <> ["  mov rdi, r15", "  call aihc_halt", "  jmp rax"])
        BlockLayout.Exit
    GrinExit status -> do
      statusLines <- liftEither (materializeValueTo env "rdi" status)
      addBlock
        label
        ( prefix
            <> statusLines
            <> [ "  call aihc_exit_process",
                 "  ud2"
               ]
        )
        BlockLayout.Exit
    GrinCase scrutinee binder alternatives ->
      compileCase env prefix label scrutinee binder alternatives
    GrinThrow {} -> unsupportedExpression "direct-style throw after CPS"
    GrinCatch {} -> unsupportedExpression "direct-style catch after CPS"
    GrinForeignCallExpr {} -> unsupportedExpression "unbound foreign call after CPS"
  where
    unsupportedExpression name = lift (Left (Amd64UnsupportedExpression name))
    compileTransfer target values extraLines = do
      overflowLines <- liftEither (saveValueOverflowLines env values)
      registerLines <- liftEither (moveValuesToRegisters env values applyArgumentRegisters)
      addBlock
        label
        (prefix <> overflowLines <> extraLines <> registerLines <> moveDirectOverflowLines "r14" (length values))
        (BlockLayout.Jump target)
    compileStoreRec allocate bindings body = do
      allocationLines <- fmap concat . forM bindings $ \(var, node) -> do
        location <- liftEither (variableLocation env var)
        nodeLines <- liftEither (allocate env node)
        pure (nodeLines <> storeLocation "rax" location)
      initializationLines <- fmap concat . forM bindings $ \(var, node) -> do
        location <- liftEither (variableLocation env var)
        fieldLines <- liftEither (initializeNodeFields env node)
        pure (loadLocation "r13" location <> fieldLines)
      compileExpr env (prefix <> allocationLines <> initializationLines) label body

compileCpsPrimitive :: ValueEnv -> [Text] -> Text -> Text -> [GrinValue] -> GrinValue -> FunctionM ()
compileCpsPrimitive env prefix label name arguments continuation =
  case nativeCpsPrimitiveCall name of
    Just runtimeCall
      | nativeCpsCallOperandCount runtimeCall == length arguments,
        1 + length arguments + fromEnum (nativeCpsCallPassContinuation runtimeCall) <= length foreignArgumentRegisters ->
          compileRuntimeCall runtimeCall
    _ -> unsupportedCpsPrimitive
  where
    compileRuntimeCall runtimeCall = do
      continuationSlot <- freshSlot
      argumentSlots <- freshSlots (length arguments)
      storedLines <-
        materializeIntoSlots env $
          zip (arguments <> [continuation]) (argumentSlots <> [continuationSlot])
      let (returnLines, successor) =
            case nativeCpsCallTransfer runtimeCall of
              NativeCpsEnterContinuation ->
                ([loadAt applyFunctionRegister "r14" continuationSlot], BlockLayout.Jump ".Laihc_enter")
              NativeCpsResumeScheduler ->
                ([], BlockLayout.Jump ".Laihc_resume")
      addBlock
        label
        ( prefix
            <> storedLines
            <> renderCpsCallArguments runtimeCall argumentSlots continuationSlot
            <> ["  call " <> nativeCpsCallSymbol runtimeCall]
            <> returnLines
        )
        successor

    unsupportedCpsPrimitive =
      lift (Left (Amd64UnsupportedExpression ("CPS primitive call " <> name)))

renderCpsCallArguments :: NativeCpsCall -> [Int] -> Int -> [Text]
renderCpsCallArguments runtimeCall operandSlots continuationSlot =
  ["  mov rdi, r15"]
    <> [loadAt register "r14" slot | (register, slot) <- zip (drop 1 foreignArgumentRegisters) operandSlots]
    <> [ loadAt (foreignArgumentRegisters !! (length operandSlots + 1)) "r14" continuationSlot
       | nativeCpsCallPassContinuation runtimeCall
       ]

compileDirectBinding :: ValueEnv -> [GrinVar] -> GrinExpr -> FunctionM [Text]
compileDirectBinding env vars expression =
  case expression of
    GrinConstant values
      | length vars == length values ->
          fmap concat . forM (zip vars values) $ \(var, value) -> do
            location <- liftEither (variableLocation env var)
            valueLines <- liftEither (materializeValue env value)
            pure (valueLines <> storeLocation "rax" location)
    GrinStore node -> liftEither (materializeNode env node) >>= storeSingleResult
    GrinEnsureHeap requiredWords roots
      | length vars == length roots -> do
          (argumentLines, argumentSlots) <- materializeIntoFreshSlots env (requiredWords : roots)
          case argumentSlots of
            requiredSlot : rootSlots -> do
              resultLines <-
                fmap concat . forM (zip vars rootSlots) $ \(var, slot) -> do
                  location <- liftEither (variableLocation env var)
                  pure ([loadAt "r11" "r14" slot] <> storeLocation "r11" location)
              pure
                ( argumentLines
                    <> [ "  mov rdi, r15",
                         loadAt "rsi" "r14" requiredSlot,
                         immediate "rdx" (length roots),
                         slotPointer "rcx" rootSlots,
                         "  call aihc_ensure_heap"
                       ]
                    <> resultLines
                )
            [] -> lift (Left (Amd64UnsupportedExpression "heap reservation size"))
      | otherwise -> lift (Left (Amd64UnsupportedExpression "heap reservation result arity"))
    GrinStoreUnchecked node -> liftEither (materializeNodeUnchecked env node) >>= storeSingleResult
    GrinFetch _ pointer -> liftEither (materializeValue env pointer) >>= storeSingleResult
    GrinUpdate pointer value -> compileUpdateBinding False "aihc_update" pointer value
    GrinUpdateBlackhole pointer value -> compileUpdateBinding True "aihc_update_blackhole" pointer value
    GrinPrimitiveCall _ name [left, right]
      | Just instructions <- lookup name singleResultBinaryPrimitives ->
          compileBinary "r10" "rax" storeSingleResult instructions left right
      | Just (firstRegister, secondRegister, instructions) <- lookup name pairResultBinaryPrimitives ->
          compileBinary "r10" "rax" (storeTwoResults firstRegister secondRegister) instructions left right
      | Just instructions <- lookup name singleResultDividendPrimitives ->
          compileBinary "rax" "r10" storeSingleResult instructions left right
      | Just (firstRegister, secondRegister, instructions) <- lookup name pairResultDividendPrimitives ->
          compileBinary "rax" "r10" (storeTwoResults firstRegister secondRegister) instructions left right
    GrinPrimitiveCall _ "quotRemWord2#" arguments@[_, _, _] -> do
      (argumentLines, argumentSlots) <- materializeIntoFreshSlots env arguments
      case argumentSlots of
        [highSlot, lowSlot, divisorSlot] ->
          storeTwoResults
            "r10"
            "r11"
            ( argumentLines
                <> [ loadAt "rdx" "r14" highSlot,
                     loadAt "rax" "r14" lowSlot,
                     loadAt "r10" "r14" divisorSlot,
                     "  div r10",
                     "  mov r10, rax",
                     "  mov r11, rdx"
                   ]
            )
        _ -> lift (Left (Amd64UnsupportedExpression "internal quotRemWord2# arity"))
    GrinPrimitiveCall _ name [value, amount]
      | Just instruction <- lookup name [("uncheckedShiftL#", "shl"), ("uncheckedShiftRL#", "shr")] -> do
          savedCountRegister <- freshSlot
          valueLines <- liftEither (materializeValueTo env "r10" value)
          amountLines <- liftEither (materializeValue env amount)
          storeSingleResult
            ( [storeAt "rcx" "r14" savedCountRegister]
                <> valueLines
                <> amountLines
                <> ["  mov rcx, rax", "  " <> instruction <> " r10, cl", "  mov rax, r10", loadAt "rcx" "r14" savedCountRegister]
            )
    GrinPrimitiveCall runtimeRep name arguments
      | name == "realWorld#",
        null arguments,
        null vars,
        null (runtimeRepComponents runtimeRep) ->
          pure []
    GrinPrimitiveCall _ name [value]
      | Just instructions <- lookup name unaryPrimitives -> do
          valueLines <- liftEither (materializeValue env value)
          storeSingleResult (valueLines <> instructions)
    GrinPrimitiveCall _ name arguments
      | Just runtimeCall <- nativeRuntimePrimitiveCall name -> do
          callLines <- compileRuntimeCallLines env runtimeCall arguments
          case nativeRuntimeCallResultCount runtimeCall of
            0 | null vars -> pure callLines
            1 -> storeSingleResult callLines
            _ -> lift (Left (Amd64UnsupportedExpression ("runtime primitive result arity " <> name)))
      | compileAllowUnsupportedPrimitives (valueCompileEnv env) ->
          pure ["  call aihc_unsupported_primitive"]
      | otherwise -> lift (Left (Amd64UnsupportedExpression ("primitive call " <> name)))
    GrinForeignCallExpr foreignCall arguments ->
      compileForeignCallLines env foreignCall arguments >>= storeSingleResult
    _ -> lift (Left (Amd64UnsupportedExpression "non-direct expression remained in a CPS bind"))
  where
    storeSingleResult lines' =
      case vars of
        [var] -> do
          location <- liftEither (variableLocation env var)
          pure (lines' <> storeLocation "rax" location)
        _ -> lift (Left (Amd64UnsupportedExpression "direct expression result arity"))
    storeTwoResults firstRegister secondRegister lines' =
      case vars of
        [first, second] -> do
          firstLocation <- liftEither (variableLocation env first)
          secondLocation <- liftEither (variableLocation env second)
          pure (lines' <> storeLocation firstRegister firstLocation <> storeLocation secondRegister secondLocation)
        _ -> lift (Left (Amd64UnsupportedExpression "direct expression pair result arity"))
    compileBinary leftRegister rightRegister store instructions left right = do
      leftLines <- liftEither (materializeValueTo env leftRegister left)
      rightLines <- liftEither (materializeValueTo env rightRegister right)
      store (leftLines <> rightLines <> instructions)
    compileUpdateBinding passMachine symbol pointer value = do
      pointerSlot <- freshSlot
      valueSlot <- freshSlot
      storedLines <- materializeIntoSlots env [(pointer, pointerSlot), (value, valueSlot)]
      resultLines <- storeSingleResult [loadAt "rax" "r14" valueSlot]
      pure
        ( storedLines
            <> [ loadAt (if passMachine then "rsi" else "rdi") "r14" pointerSlot,
                 loadAt (if passMachine then "rdx" else "rsi") "r14" valueSlot
               ]
            <> ["  mov rdi, r15" | passMachine]
            <> ["  call " <> symbol]
            <> resultLines
        )

    singleResultBinaryPrimitives =
      concat
        [ binary "add" ["+#", "plusWord#"],
          binary "sub" ["-#", "minusWord#"],
          binary "imul" ["*#", "timesWord#"],
          binary "and" ["and#"],
          binary "or" ["or#"],
          binary "xor" ["xor#"],
          comparison "e" ["==#", "eqWord#"],
          comparison "l" ["<#"],
          comparison "ne" ["neWord#"],
          comparison "b" ["ltWord#"],
          comparison "be" ["leWord#"],
          comparison "a" ["gtWord#"],
          comparison "ae" ["geWord#"]
        ]
        <> [ ("compareInt#", ["  cmp r10, rax", "  setg al", "  setl r10b", "  movzx rax, al", "  movzx r10, r10b", "  sub rax, r10"])
           ]
    pairResultBinaryPrimitives =
      [ carry "addIntC#" "add" "o",
        carry "subIntC#" "sub" "o",
        carry "addWordC#" "add" "c",
        carry "subWordC#" "sub" "c"
      ]
    singleResultDividendPrimitives =
      [ ("quotWord#", ["  xor rdx, rdx", "  div r10"]),
        ("remWord#", ["  xor rdx, rdx", "  div r10", "  mov rax, rdx"])
      ]
    pairResultDividendPrimitives =
      [ ("timesWord2#", ("rdx", "rax", ["  mul r10"])),
        ("quotRemWord#", ("r10", "r11", ["  xor rdx, rdx", "  div r10", "  mov r10, rax", "  mov r11, rdx"]))
      ]
    unaryPrimitives =
      ("not#", ["  not rax"])
        : [ (name, [])
          | name <- ["int2Word#", "word2Int#", "word8ToWord#", "word32ToWord#", "word64ToWord#", "ord#", "chr#", "unsafeFreezeArray#", "unsafeThawArray#", "unsafeFreezeByteArray#", "unsafeThawByteArray#"]
          ]
    binary instruction names =
      [(name, ["  " <> instruction <> " r10, rax", "  mov rax, r10"]) | name <- names]
    comparison condition names =
      [(name, ["  cmp r10, rax", "  set" <> condition <> " al", "  movzx rax, al"]) | name <- names]
    carry name instruction condition =
      ( name,
        ( "r10",
          "r11",
          ["  " <> instruction <> " r10, rax", "  set" <> condition <> " r11b", "  movzx r11, r11b"]
        )
      )

compileForeignCallLines :: ValueEnv -> GrinForeignCall -> [GrinValue] -> FunctionM [Text]
compileForeignCallLines env = compileCallLines env False

compileRuntimeCallLines :: ValueEnv -> NativeRuntimeCall -> [GrinValue] -> FunctionM [Text]
compileRuntimeCallLines env runtimeCall =
  compileCallLines env (nativeRuntimeCallPassMachine runtimeCall) (nativeRuntimeCallForeignCall runtimeCall)

compileCallLines :: ValueEnv -> Bool -> GrinForeignCall -> [GrinValue] -> FunctionM [Text]
compileCallLines env passMachine foreignCall arguments = do
  let signature = grinForeignCallSignature foreignCall
      operandArity = length (grinForeignArgumentTypes signature)
      abiArity = operandArity + fromEnum passMachine
      expectedArity = length (grinForeignOperandReps signature)
  if length arguments /= expectedArity
    then lift (Left (Amd64UnsupportedExpression "foreign call arity mismatch"))
    else
      if abiArity > length foreignArgumentRegisters
        then lift (Left (Amd64UnsupportedExpression "foreign calls with more than six arguments"))
        else do
          (argumentLines, argumentSlots) <- materializeIntoFreshSlots env arguments
          let argumentRegisters = drop (fromEnum passMachine) foreignArgumentRegisters
              loadAbiArguments =
                [ loadAt register "r14" slot
                | (register, slot) <- zip argumentRegisters argumentSlots
                ]
              callLines =
                argumentLines
                  <> ["  mov rdi, r15" | passMachine]
                  <> loadAbiArguments
                  <> ["  call " <> grinForeignCallSymbol foreignCall]
                  <> normalizeForeignResult (grinForeignResultType signature)
          pure callLines

materializeIntoFreshSlots :: ValueEnv -> [GrinValue] -> FunctionM ([Text], [Int])
materializeIntoFreshSlots env values = do
  slots <- freshSlots (length values)
  lines' <- materializeIntoSlots env (zip values slots)
  pure (lines', slots)

materializeIntoSlots :: ValueEnv -> [(GrinValue, Int)] -> FunctionM [Text]
materializeIntoSlots env = fmap concat . mapM store
  where
    store (value, slot) = do
      lines' <- liftEither (materializeValue env value)
      pure (lines' <> [storeAt "rax" "r14" slot])

normalizeForeignResult :: GrinForeignType -> [Text]
normalizeForeignResult foreignType =
  case foreignType of
    GrinForeignInt -> []
    GrinForeignInt32 -> ["  movsxd rax, eax"]
    GrinForeignWord64 -> []
    GrinForeignAddr -> []

foreignArgumentRegisters :: [Text]
foreignArgumentRegisters = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"]

compileCase :: ValueEnv -> [Text] -> Text -> GrinValue -> GrinVar -> [GrinAlt] -> FunctionM ()
compileCase env prefix label scrutinee binder alternatives = do
  (resultLocation, scrutineeLines) <- case scrutinee of
    GrinVarValue var | Just location <- Map.lookup var (valueLocations env) -> pure (location, [])
    _ -> materializedScrutinee
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
    GrinDataAlt _ -> do
      fields <- fmap concat . forM liveIndexedBinders $ \(index, binder) -> do
        location <- liftEither (variableLocation env binder)
        pure ([loadByteOffset "r10" "r11" (8 + index * 8)] <> storeLocation "r10" location)
      pure (if null fields then [] else loadLocation "r11" resultLocation <> fields)
    GrinLitAlt _ -> pure []
    GrinDefaultAlt ->
      fmap concat . forM (filter isLive (grinAltBinders alternative)) $ \binder -> do
        location <- liftEither (variableLocation env binder)
        pure (loadLocation "r11" resultLocation <> storeLocation "r11" location)
  where
    isLive binder = binder `Set.member` grinExprFreeVariables (grinAltRhs alternative)
    liveIndexedBinders = filter (isLive . snd) (zip [0 ..] (grinAltBinders alternative))

caseChecks :: ValueEnv -> Location Text -> Bool -> [(GrinAlt, Text)] -> FunctionM ([Text], BlockLayout.Terminator Text)
caseChecks env resultLocation scrutineeIsPointer targets = do
  let nonDefault = [(alternative, label) | (alternative, label) <- targets, grinAltCon alternative /= GrinDefaultAlt]
      defaultTarget = [label | (alternative, label) <- targets, grinAltCon alternative == GrinDefaultAlt]
  checks <- fmap concat . forM nonDefault $ \(alternative, target) ->
    case grinAltCon alternative of
      GrinDataAlt _
        | not scrutineeIsPointer ->
            lift (Left (Amd64UnsupportedExpression "constructor case on an unboxed value"))
      GrinDataAlt name -> do
        identifier <- liftEither (constructorId (valueCompileEnv env) name)
        pure $
          loadLocation "r11" resultLocation
            <> [ loadByteOffset "r10" "r11" 0,
                 loadByteOffset "r10" "r10" 0,
                 immediate "r11" identifier,
                 "  cmp r10, r11",
                 "  je " <> target
               ]
      GrinLitAlt _
        | scrutineeIsPointer ->
            lift (Left (Amd64UnsupportedExpression "literal case on a lifted value"))
      GrinLitAlt literal ->
        case normalizedLiteralInteger literal of
          Just integer ->
            pure $
              loadLocation "r10" resultLocation
                <> [immediate "r11" integer, "  cmp r10, r11", "  je " <> target]
          Nothing -> lift (Left (Amd64UnsupportedValue "string case alternative"))
      GrinDefaultAlt -> pure []
  pure $ case defaultTarget of
    target : _ -> (checks, BlockLayout.Jump target)
    [] -> (checks <> ["  call aihc_no_match", "  ud2"], BlockLayout.Exit)

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
      (stores, slots) <- materializeIntoFreshSlots env values
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
        GrinGlobalValue {} -> False
        GrinLitValue {} -> False

moveSource :: ValueEnv -> GrinValue -> MoveSource
moveSource env value =
  case value of
    GrinVarValue var ->
      case Map.lookup var (valueLocations env) of
        Just (InRegister register) -> MoveRegister register
        Just (InHeapSpill slot) -> MoveSpill slot
        Nothing -> MoveValue value
    GrinGlobalValue {} -> MoveValue value
    GrinLitValue {} -> MoveValue value

renderRegisterMoves :: ValueEnv -> [(Text, MoveSource)] -> Either Amd64Error [Text]
renderRegisterMoves env = renderRegisterMovesWith emitMove
  where
    emitMove destination source =
      case source of
        MoveRegister register -> pure ["  mov " <> destination <> ", " <> register]
        MoveSpill slot -> pure [loadAt destination "r14" slot]
        MoveValue value -> materializeValueTo env destination value

renderRegisterMovesWithoutValues :: [(Text, MoveSource)] -> [Text]
renderRegisterMovesWithoutValues = fromRight [] . renderRegisterMovesWith emitMove
  where
    emitMove destination source =
      case source of
        MoveRegister register -> pure ["  mov " <> destination <> ", " <> register]
        MoveSpill slot -> pure [loadAt destination "r14" slot]
        MoveValue {} -> Left (Amd64UnsupportedExpression "value in entry register transfer")

renderRegisterMovesWith :: (Text -> MoveSource -> Either Amd64Error [Text]) -> [(Text, MoveSource)] -> Either Amd64Error [Text]
renderRegisterMovesWith emitMove = go
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

variableLocation :: ValueEnv -> GrinVar -> Either Amd64Error (Location Text)
variableLocation env var =
  maybe
    (Left (Amd64UnsupportedExpression ("missing location for " <> grinVarName var)))
    Right
    (Map.lookup var (valueLocations env))

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

liftEither :: Either Amd64Error value -> FunctionM value
liftEither = lift
