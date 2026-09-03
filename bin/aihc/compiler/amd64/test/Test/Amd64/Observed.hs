{-# LANGUAGE OverloadedStrings #-}

module Test.Amd64.Observed
  ( compileObservedFunction,
  )
where

import Aihc.Amd64.Assemble
import Aihc.Amd64.Codegen
  ( assembleObject,
    compileEnvironmentWith,
    mainEpilogue,
    mainPrologue,
    nonExecutableStack,
    programRuntimeReps,
    renderCompiledSupport,
    renderLinkedLocals,
    renderStaticGlobals,
    threadDoneContinuation,
    threadDoneRuntimeInfos,
    validateProgramPrimitives,
    validateRuntimeRep,
  )
import Aihc.Amd64.Codegen.Function (compileFunction)
import Aihc.Amd64.Codegen.Runtime
import Aihc.Grin.Cps (ContinuationFrameKind (..))
import Aihc.Grin.Gc
import Aihc.Grin.Syntax
import Data.List (find)
import Data.Map.Strict qualified as Map
import Test.Native.Observed

-- | Compile a nullary function with a test driver that records its raw result.
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
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  staticGlobals <- renderStaticGlobals compileEnv program
  metadata <-
    renderObservedMetadata
      Amd64UnsupportedRuntimeRep
      (functionCodeLabel compileEnv)
      (`constructorStageLabel` 0)
      id
      program
      resultReps
  let resultCount = length resultReps
      statements =
        mainPrologue 0
          <> [amd64Instruction (AmdMov RDI (Amd64MoveRegister R15)), amd64Instruction (AmdCall "aihc_alloc_linked_locals"), amd64Instruction (AmdMov R14 (Amd64MoveRegister RAX))]
          <> makeNodeLines (InfoAddress ".Laihc_thread_done_info")
          <> [ amd64Instruction (AmdMov RDI (Amd64MoveRegister R15)),
               amd64Instruction (AmdMov RSI (Amd64MoveRegister RAX)),
               amd64Instruction (AmdCall "aihc_set_thread_done_continuation")
             ]
          <> makeNodeLines (InfoAddress ".Laihc_snapshot_info")
          <> [ amd64Instruction (AmdMov R13 (Amd64MoveRegister RAX)),
               amd64Instruction (AmdMov RDI (Amd64MoveRegister R15)),
               amd64Instruction (AmdCall "aihc_reset_allocation_count"),
               amd64Instruction (AmdJmp (Amd64JumpLabel entryLabel)),
               amd64Align 3,
               amd64Label ".Laihc_snapshot_result"
             ]
          <> [storeAt register R14 index | (index, register) <- zip [0 :: Int ..] applyArgumentRegisters, index < resultCount]
          <> [ amd64Instruction (AmdMov RSI (Amd64MoveRegister R14)),
               amd64Instruction (AmdMov RDX (Amd64MoveRegister R15)),
               immediate RDI resultCount,
               amd64Instruction (AmdCall "aihc_snapshot_dump_result"),
               amd64Instruction (AmdXor (Amd64RmRegister EAX) (Amd64BinaryRegister EAX))
             ]
          <> mainEpilogue
          <> threadDoneContinuation
          <> staticGlobals
          <> renderStaticReferenceTables compileEnv
          <> renderLinkedLocals functions
          <> renderCompiledSupport compileEnv functions observedRuntimeInfos
          <> nonExecutableStack
  object <- assembleObject statements
  pure ObservedProgram {observedObject = object, observedMetadataSource = metadata}
  where
    program = gcGrinProgram gcProgram
    compileEnv = (compileEnvironmentWith True (gcContinuationFrames gcProgram) program) {compileContinuationFunctions = gcContinuationFunctions gcProgram}
    resultReps =
      maybe [] (runtimeRepComponents . grinFunctionResultRep) $
        findFunction entryName (grinFunctions program)
    observedRuntimeInfos =
      threadDoneRuntimeInfos
        <> continuationRuntimeInfos
          ContinuationFrameStop
          ".Laihc_snapshot_info"
          ".Laihc_snapshot_applied_info"
          ".Laihc_snapshot_result"
          []
          resultReps

findFunction :: FunctionName -> [GrinFunction] -> Maybe GrinFunction
findFunction name = find ((== name) . grinFunctionName)
