{-# LANGUAGE OverloadedStrings #-}

-- | Compile a nullary GRIN function through Lir with a test driver that
-- records its raw result through the heap snapshot runtime.
module Test.Arm64.LirObserved
  ( lowerObservedProgram,
  )
where

import Aihc.Grin.Cps (ContinuationFrameKind (..))
import Aihc.Grin.Gc
import Aihc.Grin.Syntax
import Aihc.Lir.Lower
import Aihc.Lir.Syntax
import Control.Monad (forM, forM_)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Test.Native.Observed (renderObservedMetadata)

-- | The Lir module of the observed program and the C metadata of the
-- snapshot runtime.
lowerObservedProgram :: FunctionName -> GcGrinProgram -> Either LowerError (Module, Text)
lowerObservedProgram entryName gcProgram = do
  entryFunction <- maybe (Left (LowerMissingFunction entryName)) Right (find ((== entryName) . grinFunctionName) (grinFunctions program))
  case Map.lookup entryName (gcFunctionContinuations gcProgram) of
    Just continuation | grinFunctionParameters entryFunction == [continuation] -> pure ()
    _ -> Left (LowerUnsupportedExpression "observed entry function must have only its CPS continuation")
  let resultReps = runtimeRepComponents (grinFunctionResultRep entryFunction)
      resultTypes = map repType resultReps
  metadata <-
    renderObservedMetadata
      LowerUnsupportedRuntimeRep
      (pure . unSymbol . functionSymbol)
      (\name -> unSymbol (constructorInfoSymbol name 0))
      id
      program
      resultReps
  (_, items) <- runLower options gcProgram $ \env -> do
    lowerUnitItems env
    continuationInfoItems (ContinuationSpec threadDoneInfo (Symbol "aihc_lir_thread_done_applied_info") threadDoneTarget [] [Ptr] ContinuationFrameStop)
    threadDoneContinuation threadDoneTarget
    continuationInfoItems (ContinuationSpec snapshotInfo (Symbol "aihc_lir_snapshot_applied_info") snapshotTarget [] resultTypes ContinuationFrameStop)
    snapshotContinuation resultTypes
    observedMain
  pure (Module items, metadata)
  where
    program = gcGrinProgram gcProgram
    options = LowerOptions {lowerUnitKind = LibraryUnit, lowerExposeFunctions = True}
    threadDoneInfo = Symbol "aihc_lir_thread_done_info"
    threadDoneTarget = Symbol "aihc_lir_thread_done_continuation"
    snapshotInfo = Symbol "aihc_lir_snapshot_info"
    snapshotTarget = Symbol "aihc_lir_snapshot_result"
    -- The snapshot continuation stores the result values in a buffer and
    -- hands them to the snapshot runtime, then returns to main.
    snapshotContinuation resultTypes = do
      machine <- fresh "machine"
      values <- forM resultTypes $ \ty -> (,ty) <$> fresh "value"
      beginBlock (Label "entry") []
      buffer <- fresh "buffer"
      emit [buffer] (StackAlloc (toInteger (8 * max 1 (length resultTypes))) 8)
      forM_ (zip [0 :: Int ..] values) $ \(index, (var, ty)) ->
        emit [] (Store ty (OperandVar var) (Address (OperandVar buffer) (toInteger (8 * index))) 8)
      requireExtern (Symbol "aihc_snapshot_dump_result") [I64, Ptr, Ptr] []
      emit [] (Call (Symbol "aihc_snapshot_dump_result") [OperandLiteral (LitInt (toInteger (length resultTypes))), OperandVar buffer, OperandVar machine])
      terminate (Return [])
      finishFunction snapshotTarget Internal ((machine, Ptr) : values) [] AihcConvention
    observedMain = do
      argc <- fresh "argc"
      argv <- fresh "argv"
      beginBlock (Label "entry") []
      requireExtern (Symbol "aihc_program_arguments_initialize") [I32, Ptr] []
      emit [] (Call (Symbol "aihc_program_arguments_initialize") [OperandVar argc, OperandVar argv])
      requireExtern (Symbol "aihc_machine_new") [I64] [Ptr]
      machine <- fresh "machine"
      emit [machine] (Call (Symbol "aihc_machine_new") [OperandLiteral (LitInt 0)])
      requireExtern (Symbol "aihc_make_node") [Ptr, Ptr] [Ptr]
      threadDone <- fresh "thread_done"
      emit [threadDone] (Call (Symbol "aihc_make_node") [OperandVar machine, OperandLiteral (LitSymbol threadDoneInfo)])
      requireExtern (Symbol "aihc_set_thread_done_continuation") [Ptr, Ptr] []
      emit [] (Call (Symbol "aihc_set_thread_done_continuation") [OperandVar machine, OperandVar threadDone])
      snapshot <- fresh "snapshot"
      emit [snapshot] (Call (Symbol "aihc_make_node") [OperandVar machine, OperandLiteral (LitSymbol snapshotInfo)])
      requireExtern (Symbol "aihc_reset_allocation_count") [Ptr] []
      emit [] (Call (Symbol "aihc_reset_allocation_count") [OperandVar machine])
      emit [] (Call (functionSymbol entryName) [OperandVar machine, OperandVar snapshot])
      terminate (Return [OperandLiteral (LitInt 0)])
      finishFunction (Symbol "main") Export [(argc, I32), (argv, Ptr)] [I32] CConvention
