{-# LANGUAGE OverloadedStrings #-}

-- | Lower runtime-explicit GRIN to AArch64 Mach-O objects for Darwin.
-- Generated Haskell entries transfer only with branches; calls are reserved
-- for the C runtime and foreign functions.
module Aihc.Arm64.Codegen
  ( Arm64Error (..),
    assembleObject,
    compileEnvironmentWith,
    compileEntryObject,
    compileModuleObject,
    entryEpilogue,
    mainPrologue,
    programRuntimeReps,
    renderCompiledSupport,
    renderLinkedLocals,
    renderStaticGlobals,
    supportedNativePrimitiveNames,
    threadDoneContinuation,
    threadDoneRuntimeInfos,
    validateProgramPrimitives,
    validatePrimitiveNames,
    validateRuntimeRep,
  )
where

import Aihc.Arm64.Assemble
  ( Arm64Address (..),
    Arm64Instruction (..),
    Arm64Register (..),
    Arm64Statement,
    Arm64Value (..),
    arm64Align,
    arm64Global,
    arm64Instruction,
    arm64Label,
    arm64Quad,
    arm64QuadSymbol,
    arm64Section,
    assembleMachO,
  )
import Aihc.Arm64.Codegen.Function
import Aihc.Arm64.Codegen.Runtime
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
compileEntryObject :: Either Arm64Error BL.ByteString
compileEntryObject = do
  gcProgram <- either (Left . Arm64UnsupportedExpression . T.pack . show) Right entryGcProgram
  statements <- compileEntryUnit executableEntryName gcProgram
  assembleObject statements

-- | Compile one library module to a relocatable object.
compileModuleObject :: GcGrinProgram -> Either Arm64Error BL.ByteString
compileModuleObject gcProgram = compileModuleStatements gcProgram >>= assembleObject

assembleObject :: [Arm64Statement] -> Either Arm64Error BL.ByteString
assembleObject = either (Left . Arm64ObjectError . T.pack . show) pure . assembleMachO

compileModuleStatements :: GcGrinProgram -> Either Arm64Error [Arm64Statement]
compileModuleStatements gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  staticGlobals <- renderStaticGlobals compileEnv program
  pure (staticGlobals <> renderStaticReferenceTables compileEnv <> renderLinkedLocals functions <> renderCompiledSupport compileEnv functions [])
  where
    program = gcGrinProgram gcProgram
    compileEnv =
      (compileEnvironment (gcContinuationFrames gcProgram) program)
        { compileAllowUnsupportedPrimitives = True,
          compileContinuationFunctions = gcContinuationFunctions gcProgram
        }

compileEntryUnit :: Text -> GcGrinProgram -> Either Arm64Error [Arm64Statement]
compileEntryUnit entryName gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  staticGlobals <- renderStaticGlobals compileEnv program
  updateLabel <- functionCodeLabel compileEnv (gcUpdateFunction gcProgram)
  pure $
    mainPrologue 0
      <> [arm64Instruction (ArmMov X0 (Arm64RegisterValue X22)), arm64Instruction (ArmBl "_aihc_alloc_linked_locals"), arm64Instruction (ArmMov X19 (Arm64RegisterValue X0))]
      <> [ arm64Instruction (ArmMov X0 (Arm64RegisterValue X22)),
           immediate X1 (7 :: Int),
           arm64Instruction (ArmMov X2 (Arm64RegisterValue XZR)),
           arm64Instruction (ArmMov X3 (Arm64RegisterValue XZR)),
           arm64Instruction (ArmBl "_aihc_ensure_heap")
         ]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_final_info")
      <> [arm64Instruction (ArmMov X21 (Arm64RegisterValue X0))]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_top_info")
      <> [ arm64Instruction (ArmMov X20 (Arm64RegisterValue X0)),
           arm64Instruction (ArmMov X2 (Arm64RegisterValue X21)),
           arm64Instruction (ArmMov X1 (Arm64ImmediateValue 0)),
           arm64Instruction (ArmBl "_aihc_set_field")
         ]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_update_info")
      <> [ storeAt X0 X19 0,
           arm64Instruction (ArmMov X2 (Arm64RegisterValue X20)),
           loadAt X0 X19 0,
           arm64Instruction (ArmMov X1 (Arm64ImmediateValue 0)),
           arm64Instruction (ArmBl "_aihc_set_field")
         ]
      <> address X2 ("_" <> renderLinkedGlobalSymbol entryName)
      <> [ loadAt X0 X19 0,
           arm64Instruction (ArmMov X1 (Arm64ImmediateValue 1)),
           arm64Instruction (ArmBl "_aihc_set_field")
         ]
      <> makeNodeUncheckedLines (InfoAddress ".Laihc_thread_done_info")
      <> [ arm64Instruction (ArmMov X10 (Arm64RegisterValue X0)),
           arm64Instruction (ArmMov X1 (Arm64RegisterValue X10)),
           arm64Instruction (ArmMov X0 (Arm64RegisterValue X22)),
           arm64Instruction (ArmBl "_aihc_set_thread_done_continuation"),
           arm64Instruction (ArmAdr X9 ".Laihc_exit"),
           arm64Instruction (ArmStr X9 (Arm64Offset X22 16))
         ]
      <> address applyFunctionRegister ("_" <> renderLinkedGlobalSymbol entryName)
      <> [ loadAt X0 X19 0,
           arm64Instruction (ArmLdr X21 (Arm64Offset X0 8)),
           arm64Instruction (ArmMov X8 (Arm64ImmediateValue 1)),
           arm64Instruction (ArmB ".Laihc_eval")
         ]
      <> [ arm64Align 3,
           arm64Label ".Laihc_top_continuation",
           arm64Instruction (ArmMov X21 (Arm64RegisterValue X0)),
           arm64Instruction (ArmMov X20 (Arm64RegisterValue X1)),
           arm64Instruction (ArmB ".Laihc_enter")
         ]
      <> threadDoneContinuation
      <> [ arm64Align 3,
           arm64Label ".Laihc_final_continuation",
           arm64Instruction (ArmB ".Laihc_exit")
         ]
      <> [ arm64Label ".Laihc_exit",
           arm64Instruction (ArmMov W0 (Arm64ImmediateValue 0))
         ]
      <> entryEpilogue
      <> staticGlobals
      <> renderStaticReferenceTables compileEnv
      <> renderLinkedLocals functions
      <> renderCompiledSupport compileEnv functions (programRuntimeInfos updateLabel)
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

compileEnvironment :: Map.Map FunctionName ContinuationFrameKind -> GrinProgram -> CompileEnv
compileEnvironment = compileEnvironmentWith False

compileEnvironmentWith :: Bool -> Map.Map FunctionName ContinuationFrameKind -> GrinProgram -> CompileEnv
compileEnvironmentWith exposeAllFunctions continuationFrames program =
  CompileEnv
    { compileFunctionLabels =
        functionLabelMap,
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

renderStaticGlobals :: CompileEnv -> GrinProgram -> Either Arm64Error [Arm64Statement]
renderStaticGlobals env program = fmap concat (mapM renderGlobal (programStaticObjects program))
  where
    renderGlobal object = do
      let node = staticObjectNode object
      info <- staticNodeInfo node
      fields <- mapM renderStaticValue (grinNodeFields node)
      let symbol = "_" <> renderLinkedGlobalSymbol (staticObjectName object)
          payload = if null fields && isThunk node then [arm64Quad 0] else fields
      pure $
        [ arm64Section DataSection,
          arm64Align 3,
          arm64Global symbol,
          arm64Label symbol,
          arm64QuadSymbol info
        ]
          <> payload
          -- Only objects the collector has to mark get an entry. A nullary
          -- constructor has no fields, so it can neither move nor retain
          -- anything, and the collector leaves a pointer to one alone whether
          -- or not it knows the object is static.
          <> [ statement
             | staticObjectTraced object,
               statement <-
                 [ arm64Section RootsSection,
                   arm64Align 3,
                   arm64QuadSymbol symbol
                 ]
             ]
    staticNodeInfo node =
      case grinNodeTag node of
        GrinConstructor name remaining -> pure (constructorStageLabel name remaining)
        GrinClosure functionName layouts -> lookupRuntimeInfoLabel env (ClosureRuntimeInfo functionName fields layouts)
        GrinThunk functionName -> lookupRuntimeInfoLabel env (ThunkRuntimeInfo functionName fields)
      where
        fields = map grinValueRuntimeRep (grinNodeFields node)
    renderStaticValue value =
      case value of
        GrinVarValue var -> pure (arm64QuadSymbol ("_" <> renderLinkedGlobalSymbol (grinVarName var)))
        GrinGlobalValue name -> pure (arm64QuadSymbol ("_" <> renderLinkedGlobalSymbol name))
        GrinLitValue literal ->
          case literal of
            GrinLitAddr bytes ->
              maybe (Left (Arm64UnsupportedValue "unregistered Addr# literal")) (pure . arm64QuadSymbol) (Map.lookup bytes (compileAddrLiteralLabels env))
            _ -> maybe (Left (Arm64UnsupportedValue "string literal")) (pure . arm64Quad . fromIntegral) (normalizedLiteralInteger literal)
    isThunk node =
      case grinNodeTag node of
        GrinThunk {} -> True
        _ -> False

renderLinkedLocals :: [CompiledFunction] -> [Arm64Statement]
renderLinkedLocals functions =
  [ arm64Section LocalsSection,
    arm64Align 3,
    arm64Quad (fromIntegral (maximum (2 : map compiledFunctionSlots functions)))
  ]

mainPrologue :: Int -> [Arm64Statement]
mainPrologue globalCount =
  entryPrologue "_main"
    <> [ arm64Instruction (ArmBl "_aihc_program_arguments_initialize"),
         immediate X0 globalCount,
         arm64Instruction (ArmBl "_aihc_machine_new"),
         arm64Instruction (ArmMov X22 (Arm64RegisterValue X0))
       ]

entryPrologue :: Text -> [Arm64Statement]
entryPrologue symbol =
  [ arm64Section TextSection,
    arm64Align 2,
    arm64Global symbol,
    arm64Label symbol,
    arm64Instruction (ArmStp X29 X30 (Arm64PreIndex SP (-48))),
    arm64Instruction (ArmMov X29 (Arm64RegisterValue SP)),
    arm64Instruction (ArmStp X19 X20 (Arm64Offset SP 16)),
    arm64Instruction (ArmStp X21 X22 (Arm64Offset SP 32))
  ]

entryEpilogue :: [Arm64Statement]
entryEpilogue =
  [ arm64Instruction (ArmLdp X21 X22 (Arm64Offset SP 32)),
    arm64Instruction (ArmLdp X19 X20 (Arm64Offset SP 16)),
    arm64Instruction (ArmLdp X29 X30 (Arm64PostIndex SP 48)),
    arm64Instruction ArmRet
  ]

threadDoneContinuation :: [Arm64Statement]
threadDoneContinuation =
  [ arm64Align 3,
    arm64Label ".Laihc_thread_done_continuation",
    arm64Instruction (ArmMov X0 (Arm64RegisterValue X22)),
    arm64Instruction (ArmBl "_aihc_thread_done"),
    arm64Instruction (ArmB ".Laihc_resume")
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

renderCompiledSupport :: CompileEnv -> [CompiledFunction] -> [RuntimeInfo] -> [Arm64Statement]
renderCompiledSupport env functions runtimeInfos =
  renderNativeControl
    <> concatMap renderFunction functions
    <> renderRuntimeSupport env runtimeInfos
  where
    renderFunction = compiledFunctionLines

-- | Reject primitives that reachable native code would not execute correctly.
-- Relocatable library objects may carry dormant primitive declarations, but
-- the linked program is checked after whole-program dead-code elimination.
validateProgramPrimitives :: GrinProgram -> Either Arm64Error ()
validateProgramPrimitives program =
  validatePrimitiveNames (map (grinVarName . fst) (grinPrimitives program))

validatePrimitiveNames :: [Text] -> Either Arm64Error ()
validatePrimitiveNames = mapM_ (validatePrimitiveName False)

validatePrimitiveName :: Bool -> Text -> Either Arm64Error ()
validatePrimitiveName allowUnsupported name
  | name `elem` supportedNativePrimitiveNames = Right ()
  | allowUnsupported = Right ()
  | otherwise = Left (Arm64UnsupportedPrimitive name)

validateRuntimeRep :: GrinRep -> Either Arm64Error ()
validateRuntimeRep runtimeRep =
  case runtimeRep of
    VecRep {} -> Left (Arm64UnsupportedRuntimeRep runtimeRep)
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
      runtimeRep : concatMap valueRuntimeReps [value, continuation, updateContinuation]
    GrinCall runtimeRep _ arguments -> runtimeRep : concatMap valueRuntimeReps arguments
    GrinPrimitiveCall runtimeRep _ arguments -> runtimeRep : concatMap valueRuntimeReps arguments
    GrinCpsPrimitiveCall runtimeRep _ arguments continuation ->
      runtimeRep : concatMap valueRuntimeReps arguments <> valueRuntimeReps continuation
    GrinApply runtimeRep function arguments ->
      runtimeRep : valueRuntimeReps function <> concatMap valueRuntimeReps arguments
    GrinCpsApply runtimeRep function arguments continuation ->
      runtimeRep : valueRuntimeReps function <> concatMap valueRuntimeReps arguments <> valueRuntimeReps continuation
    GrinContinue continuation values -> valueRuntimeReps continuation <> concatMap valueRuntimeReps values
    GrinCpsRaise exception continuation -> valueRuntimeReps exception <> valueRuntimeReps continuation
    GrinHalt values -> concatMap valueRuntimeReps values
    GrinExit status -> valueRuntimeReps status
    GrinCase scrutinee binder alternatives ->
      valueRuntimeReps scrutinee <> (grinVarRuntimeRep binder : concatMap altRuntimeReps alternatives)
    GrinThrow exception -> valueRuntimeReps exception
    GrinCatch runtimeRep action handler state -> runtimeRep : concatMap valueRuntimeReps (action : handler : state)
    GrinForeignCallExpr foreignCall arguments ->
      grinForeignCallResultReps (grinForeignCallSignature foreignCall) <> concatMap valueRuntimeReps arguments
  where
    altRuntimeReps alternative =
      map grinVarRuntimeRep (grinAltBinders alternative) <> exprRuntimeReps (grinAltRhs alternative)
    storedRuntimeReps bindings body =
      concatMap (\(var, node) -> grinVarRuntimeRep var : nodeRuntimeReps node) bindings
        <> exprRuntimeReps body
    updatedRuntimeReps pointer value = valueRuntimeReps pointer <> valueRuntimeReps value

valueRuntimeReps :: GrinValue -> [GrinRep]
valueRuntimeReps value = [grinValueRuntimeRep value]

nodeRuntimeReps :: GrinNode -> [GrinRep]
nodeRuntimeReps node = concatMap valueRuntimeReps (grinNodeFields node)
