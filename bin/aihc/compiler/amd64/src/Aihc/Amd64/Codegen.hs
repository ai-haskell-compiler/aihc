{-# LANGUAGE OverloadedStrings #-}

-- | Lower runtime-explicit GRIN to AMD64 ELF objects for Linux.
-- Generated Haskell entries transfer only with branches; calls are reserved
-- for the C runtime and foreign functions.
module Aihc.Amd64.Codegen
  ( Amd64Error (..),
    assembleObject,
    compileEnvironmentWith,
    compileEntryObject,
    compileModuleObject,
    mainEpilogue,
    mainPrologue,
    nonExecutableStack,
    programRuntimeReps,
    renderCompiledSupport,
    renderStaticGlobals,
    supportedNativePrimitiveNames,
    threadDoneContinuation,
    threadDoneRuntimeInfos,
    validateProgramPrimitives,
    validatePrimitiveNames,
    validateRuntimeRep,
  )
where

import Aihc.Amd64.Assemble
  ( Amd64BinarySource (..),
    Amd64Instruction (..),
    Amd64JumpTarget (..),
    Amd64Memory (..),
    Amd64MoveSource (..),
    Amd64Register (..),
    Amd64Rm (..),
    Amd64Statement,
    Amd64StoreSource (..),
    amd64Align,
    amd64Global,
    amd64Instruction,
    amd64Label,
    amd64Quad,
    amd64QuadSymbol,
    amd64Section,
    assembleElf,
  )
import Aihc.Amd64.Codegen.Function
import Aihc.Amd64.Codegen.Runtime
import Aihc.Grin.Cps (ContinuationFrameKind (..))
import Aihc.Grin.Gc
  ( GcGrinProgram,
    entryGcProgram,
    gcContinuationFrames,
    gcContinuationFunctions,
    gcGrinProgram,
    gcUpdateFunction,
  )
import Aihc.Grin.Srt
import Aihc.Grin.Syntax
import Aihc.Native
  ( buildAddrLiteralPool,
    executableEntryName,
    renderLinkedGlobalSymbol,
    supportedNativePrimitiveNames,
  )
import Aihc.Native.Object (SectionRole (..))
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | Compile the fixed executable entry unit.
compileEntryObject :: Either Amd64Error BL.ByteString
compileEntryObject = do
  gcProgram <- either (Left . Amd64UnsupportedExpression . T.pack . show) Right entryGcProgram
  statements <- compileEntryUnit executableEntryName gcProgram
  assembleObject statements

-- | Reject primitives that reachable native code would not execute correctly.
-- Relocatable library objects may carry dormant primitive declarations, but
-- the linked program is checked after whole-program dead-code elimination.
validateProgramPrimitives :: GrinProgram -> Either Amd64Error ()
validateProgramPrimitives program =
  validatePrimitiveNames (map (grinVarName . fst) (grinPrimitives program))

validatePrimitiveNames :: [Text] -> Either Amd64Error ()
validatePrimitiveNames = mapM_ (validatePrimitiveName False)

-- | Compile one library module to a relocatable object.
compileModuleObject :: GcGrinProgram -> Either Amd64Error BL.ByteString
compileModuleObject gcProgram = compileModuleStatements gcProgram >>= assembleObject

assembleObject :: [Amd64Statement] -> Either Amd64Error BL.ByteString
assembleObject = either (Left . Amd64ObjectError . T.pack . show) pure . assembleElf

compileModuleStatements :: GcGrinProgram -> Either Amd64Error [Amd64Statement]
compileModuleStatements gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  staticGlobals <- renderStaticGlobals compileEnv program
  pure $
    staticGlobals
      <> renderStaticReferenceTables compileEnv
      <> renderCompiledSupport compileEnv functions []
      <> nonExecutableStack
  where
    program = gcGrinProgram gcProgram
    compileEnv =
      (compileEnvironment (gcContinuationFrames gcProgram) program)
        { compileAllowUnsupportedPrimitives = True,
          compileContinuationFunctions = gcContinuationFunctions gcProgram
        }

compileEntryUnit :: Text -> GcGrinProgram -> Either Amd64Error [Amd64Statement]
compileEntryUnit entryName gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  staticGlobals <- renderStaticGlobals compileEnv program
  updateLabel <- functionCodeLabel compileEnv (gcUpdateFunction gcProgram)
  pure $
    mainPrologue 0
      <> reserveLocalsLines
      <> [ amd64Instruction (AmdMov RDI (Amd64MoveRegister R15)),
           immediate RSI (7 :: Int),
           amd64Instruction (AmdXor (Amd64RmRegister EDX) (Amd64BinaryRegister EDX)),
           amd64Instruction (AmdXor (Amd64RmRegister ECX) (Amd64BinaryRegister ECX)),
           amd64Instruction (AmdCall "aihc_ensure_heap")
         ]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_final_info")
      <> [amd64Instruction (AmdMov R13 (Amd64MoveRegister RAX))]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_top_info")
      <> [ amd64Instruction (AmdMov R12 (Amd64MoveRegister RAX)),
           amd64Instruction (AmdMov RDI (Amd64MoveRegister R12)),
           amd64Instruction (AmdXor (Amd64RmRegister ESI) (Amd64BinaryRegister ESI)),
           amd64Instruction (AmdMov RDX (Amd64MoveRegister R13)),
           amd64Instruction (AmdCall "aihc_set_field")
         ]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_update_info")
      <> [ storeAt RAX R14 0,
           amd64Instruction (AmdMov RDX (Amd64MoveRegister R12)),
           loadAt RDI R14 0,
           amd64Instruction (AmdXor (Amd64RmRegister ESI) (Amd64BinaryRegister ESI)),
           amd64Instruction (AmdCall "aihc_set_field"),
           loadAt RDI R14 0,
           amd64Instruction (AmdMov ESI (Amd64MoveImmediate 1)),
           address RDX (renderLinkedGlobalSymbol entryName),
           amd64Instruction (AmdCall "aihc_set_field")
         ]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_thread_done_info")
      <> [ amd64Instruction (AmdMov R10 (Amd64MoveRegister RAX)),
           amd64Instruction (AmdMov RSI (Amd64MoveRegister R10)),
           amd64Instruction (AmdMov RDI (Amd64MoveRegister R15)),
           amd64Instruction (AmdCall "aihc_set_thread_done_continuation"),
           address R11 ".Laihc_exit",
           amd64Instruction (AmdStore (Amd64Memory R15 16) (Amd64StoreRegister R11)),
           address applyFunctionRegister (renderLinkedGlobalSymbol entryName),
           loadAt RAX R14 0,
           amd64Instruction (AmdMov R13 (Amd64MoveMemory (Amd64Memory RAX 8))),
           amd64Instruction (AmdMov R11 (Amd64MoveImmediate 1)),
           amd64Instruction (AmdJmp (Amd64JumpLabel ".Laihc_eval"))
         ]
      <> [ amd64Align 3,
           amd64Label ".Laihc_top_continuation",
           amd64Instruction (AmdMov R13 (Amd64MoveRegister RAX)),
           amd64Instruction (AmdMov R12 (Amd64MoveRegister RDI)),
           amd64Instruction (AmdJmp (Amd64JumpLabel ".Laihc_enter"))
         ]
      <> threadDoneContinuation
      <> [ amd64Align 3,
           amd64Label ".Laihc_final_continuation",
           amd64Instruction (AmdJmp (Amd64JumpLabel ".Laihc_exit"))
         ]
      <> [ amd64Label ".Laihc_exit",
           amd64Instruction (AmdXor (Amd64RmRegister EAX) (Amd64BinaryRegister EAX))
         ]
      <> mainEpilogue
      <> staticGlobals
      <> renderStaticReferenceTables compileEnv
      <> renderCompiledSupport compileEnv functions (programRuntimeInfos updateLabel)
      <> nonExecutableStack
  where
    program = gcGrinProgram gcProgram
    compileEnv = (compileEnvironmentWith False (gcContinuationFrames gcProgram) program) {compileContinuationFunctions = gcContinuationFunctions gcProgram}
    pointerRep = BoxedRep Lifted
    programRuntimeInfos updateLabel =
      continuationRuntimeInfos
        ContinuationFrameStop
        ".Laihc_final_info"
        ".Laihc_final_applied_info"
        ".Laihc_final_continuation"
        []
        [pointerRep]
        <> continuationRuntimeInfos
          ContinuationFrameNormal
          ".Laihc_top_info"
          ".Laihc_top_applied_info"
          ".Laihc_top_continuation"
          [pointerRep]
          [pointerRep]
        <> continuationRuntimeInfos
          ContinuationFrameUpdate
          ".Laihc_update_info"
          ".Laihc_update_applied_info"
          updateLabel
          [pointerRep, pointerRep]
          [pointerRep]
        <> threadDoneRuntimeInfos

mainPrologue :: Int -> [Amd64Statement]
mainPrologue globalCount =
  entryPrologue "main"
    <> [ amd64Instruction (AmdCall "aihc_program_arguments_initialize"),
         immediate RDI globalCount,
         amd64Instruction (AmdCall "aihc_machine_new"),
         amd64Instruction (AmdMov R15 (Amd64MoveRegister RAX))
       ]

entryPrologue :: Text -> [Amd64Statement]
entryPrologue symbol =
  [ amd64Section TextSection,
    amd64Align 4,
    amd64Global symbol,
    amd64Label symbol,
    amd64Instruction (AmdPush RBP),
    amd64Instruction (AmdMov RBP (Amd64MoveRegister RSP)),
    amd64Instruction (AmdPush R12),
    amd64Instruction (AmdPush R13),
    amd64Instruction (AmdPush R14),
    amd64Instruction (AmdPush R15)
  ]

mainEpilogue :: [Amd64Statement]
mainEpilogue =
  [ amd64Instruction (AmdPop R15),
    amd64Instruction (AmdPop R14),
    amd64Instruction (AmdPop R13),
    amd64Instruction (AmdPop R12),
    amd64Instruction (AmdPop RBP),
    amd64Instruction AmdRet
  ]

threadDoneContinuation :: [Amd64Statement]
threadDoneContinuation =
  [ amd64Align 3,
    amd64Label ".Laihc_thread_done_continuation",
    amd64Instruction (AmdMov RDI (Amd64MoveRegister R15)),
    amd64Instruction (AmdCall "aihc_thread_done"),
    amd64Instruction (AmdJmp (Amd64JumpLabel ".Laihc_resume"))
  ]

threadDoneRuntimeInfos :: [RuntimeInfo]
threadDoneRuntimeInfos =
  continuationRuntimeInfos
    ContinuationFrameStop
    ".Laihc_thread_done_info"
    ".Laihc_thread_done_applied_info"
    ".Laihc_thread_done_continuation"
    []
    [BoxedRep Lifted]

renderCompiledSupport :: CompileEnv -> [CompiledFunction] -> [RuntimeInfo] -> [Amd64Statement]
renderCompiledSupport env functions runtimeInfos =
  renderNativeControl
    <> concatMap compiledFunctionLines functions
    <> renderRuntimeSupport env runtimeInfos

nonExecutableStack :: [Amd64Statement]
nonExecutableStack = [amd64Section NoExecuteStackSection]

compileEnvironment :: Map.Map FunctionName ContinuationFrameKind -> GrinProgram -> CompileEnv
compileEnvironment = compileEnvironmentWith False

compileEnvironmentWith :: Bool -> Map.Map FunctionName ContinuationFrameKind -> GrinProgram -> CompileEnv
compileEnvironmentWith exposeAllFunctions continuationFrames program =
  CompileEnv
    { compileFunctionLabels = functionLabelMap,
      compileAddrLiteralLabels =
        Map.fromList (buildAddrLiteralPool program),
      compileNodeInfoLabels = constructorInfoLabels <> functionInfoLabels,
      compileRuntimeInfos = map third constructorInfoEntries <> functionInfos,
      compileStaticReferences = staticReferences,
      compileSrtLabels = srtLabels,
      compileContinuationFunctions = Set.empty,
      compileExposeAllFunctions = exposeAllFunctions,
      compileAllowUnsupportedPrimitives = False
    }
  where
    constructorLayouts = grinConstructors program
    staticReferences = programStaticReferences program
    srtLabels =
      Map.fromList
        [ (name, ".Laihc_srt_" <> tshow index)
        | (index, name) <- zip [0 :: Int ..] (Map.keys (staticReferenceTables staticReferences))
        ]
    constructorInfoEntries =
      [ ( key,
          label,
          RuntimeInfo label (InfoConstructor (constructorStageLabel name 0)) fields remaining next Nothing Nothing (runtimeInfoKeyObjectKind key) Nothing
        )
      | (name, layouts) <- constructorLayouts,
        let arity = length layouts,
        remaining <- [arity, arity - 1 .. 0],
        let key = ConstructorRuntimeInfo name remaining
            label = constructorStageLabel name remaining
            fields = concat (take (arity - remaining) layouts)
            next = if remaining == 0 then Nothing else Just (constructorStageLabel name (remaining - 1))
      ]
    constructorInfoLabels = Map.fromList [(key, label) | (key, label, _) <- constructorInfoEntries]
    functionLabels =
      [ (grinFunctionName function, localFunctionLabelWith exposeAllFunctions index function)
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
          (Map.lookup functionName continuationFrames)
          (runtimeInfoKeyObjectKind key)
          (Map.lookup functionName srtLabels)
      | (key, functionName) <- functionInfoKeys,
        let label = functionInfoLabels Map.! key
            target = functionLabelMap Map.! functionName
      ]
    third (_, _, value) = value

renderStaticGlobals :: CompileEnv -> GrinProgram -> Either Amd64Error [Amd64Statement]
renderStaticGlobals env program = fmap concat (mapM renderGlobal (programStaticObjects program))
  where
    renderGlobal object = do
      let node = staticObjectNode object
      info <- staticNodeInfo node
      fields <- mapM renderStaticValue (grinNodeFields node)
      let symbol = renderLinkedGlobalSymbol (staticObjectName object)
          payload = if null fields && isThunk node then [amd64Quad 0] else fields
      pure $
        [ amd64Section DataSection,
          amd64Align 3,
          amd64Global symbol,
          amd64Label symbol,
          amd64QuadSymbol info
        ]
          <> payload
    staticNodeInfo node =
      case grinNodeTag node of
        GrinConstructor name remaining -> pure (constructorStageLabel name remaining)
        GrinClosure functionName layouts -> lookupRuntimeInfoLabel env (ClosureRuntimeInfo functionName fields layouts)
        GrinThunk functionName -> lookupRuntimeInfoLabel env (ThunkRuntimeInfo functionName fields)
      where
        fields = map grinValueRuntimeRep (grinNodeFields node)
    renderStaticValue value =
      case value of
        GrinVarValue var -> pure (amd64QuadSymbol (renderLinkedGlobalSymbol (grinVarName var)))
        GrinGlobalValue name -> pure (amd64QuadSymbol (renderLinkedGlobalSymbol name))
        GrinLitValue literal ->
          case literal of
            GrinLitAddr bytes ->
              maybe (Left (Amd64UnsupportedValue "unregistered Addr# literal")) (pure . amd64QuadSymbol) (Map.lookup bytes (compileAddrLiteralLabels env))
            _ -> maybe (Left (Amd64UnsupportedValue "string literal")) (pure . amd64Quad . fromIntegral) (normalizedLiteralInteger literal)
    isThunk node =
      case grinNodeTag node of
        GrinThunk {} -> True
        _ -> False

validatePrimitiveName :: Bool -> Text -> Either Amd64Error ()
validatePrimitiveName allowUnsupported name
  | name `elem` supportedNativePrimitiveNames = Right ()
  | allowUnsupported = Right ()
  | otherwise = Left (Amd64UnsupportedPrimitive name)

validateRuntimeRep :: GrinRep -> Either Amd64Error ()
validateRuntimeRep runtimeRep =
  case runtimeRep of
    VecRep {} -> Left (Amd64UnsupportedRuntimeRep runtimeRep)
    TupleRep fieldReps -> mapM_ validateRuntimeRep fieldReps
    SumRep alternativeReps -> mapM_ validateRuntimeRep alternativeReps
    _ -> Right ()

programRuntimeReps :: GrinProgram -> [GrinRep]
programRuntimeReps program =
  concatMap (concat . snd) (grinConstructors program)
    <> map (grinVarRuntimeRep . fst) (grinPrimitives program)
    <> concatMap bindingRuntimeReps (grinGlobals program)
    <> concatMap functionRuntimeReps (grinFunctions program)
  where
    bindingRuntimeReps (_, node) = nodeRuntimeReps node
    functionRuntimeReps function =
      grinFunctionResultRep function
        : map grinVarRuntimeRep (grinFunctionParameters function)
          <> exprRuntimeReps (grinFunctionBody function)

programNodes :: GrinProgram -> [GrinNode]
programNodes program =
  map snd (grinGlobals program)
    <> concatMap (exprNodes . grinFunctionBody) (grinFunctions program)

exprNodes :: GrinExpr -> [GrinNode]
exprNodes expression =
  case expression of
    GrinBind _ valueExpression body -> exprNodes valueExpression <> exprNodes body
    GrinStore node -> [node]
    GrinStoreUnchecked node -> [node]
    GrinStoreRec bindings body -> storedNodes bindings body
    GrinStoreRecUnchecked bindings body -> storedNodes bindings body
    GrinCase _ _ alternatives -> concatMap (exprNodes . grinAltRhs) alternatives
    GrinConstant {} -> []
    GrinEnsureHeap {} -> []
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
    GrinCpsRaise {} -> []
    GrinHalt {} -> []
    GrinExit {} -> []
    GrinThrow {} -> []
    GrinCatch {} -> []
    GrinForeignCallExpr {} -> []
  where
    storedNodes bindings body = map snd bindings <> exprNodes body

exprRuntimeReps :: GrinExpr -> [GrinRep]
exprRuntimeReps expression =
  case expression of
    GrinConstant values -> concatMap valueRuntimeReps values
    GrinBind vars valueExpression body ->
      map grinVarRuntimeRep vars <> exprRuntimeReps valueExpression <> exprRuntimeReps body
    GrinStore node -> nodeRuntimeReps node
    GrinEnsureHeap requiredWords roots -> valueRuntimeReps requiredWords <> concatMap valueRuntimeReps roots
    GrinStoreUnchecked node -> nodeRuntimeReps node
    GrinStoreRec bindings body -> storedRuntimeReps bindings body
    GrinStoreRecUnchecked bindings body -> storedRuntimeReps bindings body
    GrinUpdate pointer value -> updatedRuntimeReps pointer value
    GrinUpdateBlackhole pointer value -> updatedRuntimeReps pointer value
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
    GrinCpsRaise exception continuation ->
      valueRuntimeReps exception <> valueRuntimeReps continuation
    GrinHalt values -> concatMap valueRuntimeReps values
    GrinExit status -> valueRuntimeReps status
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
    storedRuntimeReps bindings body =
      concatMap (\(var, node) -> grinVarRuntimeRep var : nodeRuntimeReps node) bindings
        <> exprRuntimeReps body
    updatedRuntimeReps pointer value = valueRuntimeReps pointer <> valueRuntimeReps value

valueRuntimeReps :: GrinValue -> [GrinRep]
valueRuntimeReps value = [grinValueRuntimeRep value]

nodeRuntimeReps :: GrinNode -> [GrinRep]
nodeRuntimeReps node = concatMap valueRuntimeReps (grinNodeFields node)
