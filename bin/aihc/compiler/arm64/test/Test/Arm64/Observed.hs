{-# LANGUAGE OverloadedStrings #-}

module Test.Arm64.Observed
  ( compileObservedFunction,
  )
where

import Aihc.Arm64.Assemble
import Aihc.Arm64.Codegen
  ( assembleObject,
    compileEnvironmentWith,
    entryEpilogue,
    mainPrologue,
    programRuntimeReps,
    renderCompiledSupport,
    renderLinkedLocals,
    renderStaticGlobals,
    threadDoneContinuation,
    threadDoneRuntimeInfos,
    validateProgramPrimitives,
    validateRuntimeRep,
  )
import Aihc.Arm64.Codegen.Function (compileFunction)
import Aihc.Arm64.Codegen.Runtime
import Aihc.Grin.Cps (ContinuationFrameKind (..))
import Aihc.Grin.Gc
import Aihc.Grin.Syntax
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Test.Native.Observed

-- | Compile a nullary function with a test driver that records its raw result.
compileObservedFunction :: FunctionName -> GcGrinProgram -> Either Arm64Error ObservedProgram
compileObservedFunction entryName gcProgram = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  validateProgramPrimitives program
  entryFunction <-
    maybe (Left (Arm64MissingFunction entryName)) Right $
      findFunction entryName (grinFunctions program)
  case Map.lookup entryName (gcFunctionContinuations gcProgram) of
    Just continuation
      | grinFunctionParameters entryFunction == [continuation] -> pure ()
    _ -> Left (Arm64UnsupportedExpression "observed entry function must have only its CPS continuation")
  entryLabel <- functionCodeLabel compileEnv entryName
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  staticGlobals <- renderStaticGlobals compileEnv program
  metadata <-
    renderObservedMetadata
      Arm64UnsupportedRuntimeRep
      (functionCodeLabel compileEnv)
      (`constructorStageLabel` 0)
      (T.drop 1)
      program
      resultReps
  let resultCount = length resultReps
      statements =
        mainPrologue 0
          <> [arm64Instruction (ArmMov X0 (Arm64RegisterValue X22)), arm64Instruction (ArmBl "_aihc_alloc_linked_locals"), arm64Instruction (ArmMov X19 (Arm64RegisterValue X0))]
          <> makeNodeLines (InfoAddress ".Laihc_thread_done_info")
          <> [ arm64Instruction (ArmMov X1 (Arm64RegisterValue X0)),
               arm64Instruction (ArmMov X0 (Arm64RegisterValue X22)),
               arm64Instruction (ArmBl "_aihc_set_thread_done_continuation")
             ]
          <> makeNodeLines (InfoAddress ".Laihc_snapshot_info")
          <> [ arm64Instruction (ArmMov X21 (Arm64RegisterValue X0)),
               arm64Instruction (ArmMov X0 (Arm64RegisterValue X22)),
               arm64Instruction (ArmBl "_aihc_reset_allocation_count"),
               arm64Instruction (ArmB entryLabel),
               arm64Align 3,
               arm64Label ".Laihc_snapshot_result"
             ]
          <> [storeAt register X19 index | (index, register) <- zip [0 :: Int ..] applyArgumentRegisters, index < resultCount]
          <> [ arm64Instruction (ArmMov X1 (Arm64RegisterValue X19)),
               arm64Instruction (ArmMov X2 (Arm64RegisterValue X22)),
               immediate X0 resultCount,
               arm64Instruction (ArmBl "_aihc_snapshot_dump_result"),
               arm64Instruction (ArmMov W0 (Arm64ImmediateValue 0))
             ]
          <> entryEpilogue
          <> threadDoneContinuation
          <> staticGlobals
          <> renderStaticReferenceTables compileEnv
          <> renderLinkedLocals functions
          <> renderCompiledSupport compileEnv functions observedRuntimeInfos
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
