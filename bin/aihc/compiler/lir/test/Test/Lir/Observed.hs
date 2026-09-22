{-# LANGUAGE OverloadedStrings #-}

-- | Lower a nullary GRIN function through Lir with a test driver that
-- records its raw result through the heap snapshot runtime.
module Test.Lir.Observed
  ( lowerObservedProgram,
    forceCollection,
  )
where

import Aihc.Grin.Cps (ContinuationFrameKind (..))
import Aihc.Grin.Gc
import Aihc.Grin.Syntax
import Aihc.Lir.Lower
import Aihc.Lir.Syntax
import Control.Monad (forM, forM_, when)
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Test.Native.Observed (renderObservedMetadata)

-- | The Lir module of the observed program and the C metadata of the
-- snapshot runtime.
lowerObservedProgram :: LowerTarget -> Bool -> FunctionName -> GcGrinProgram -> Either LowerError (Module, Text)
lowerObservedProgram target gcStress entryName gcProgram = do
  entryFunction <- maybe (Left (LowerMissingFunction entryName)) Right (find ((== entryName) . grinFunctionName) (grinFunctions program))
  case Map.lookup entryName (gcFunctionContinuations gcProgram) of
    Just continuation | grinFunctionParameters entryFunction == [continuation] -> pure ()
    _ -> Left (LowerUnsupportedExpression "observed entry function must have only its CPS continuation")
  resultReps <-
    maybe
      (Left (LowerUnsupportedExpression "observed entry function must place its result"))
      Right
      (resultRepComponents (grinFunctionResultRep entryFunction))
  let resultTypes = map repType resultReps
  metadata <-
    renderObservedMetadata
      LowerUnsupportedRuntimeRep
      observedFunctionLabel
      (\name -> unSymbol (constructorInfoSymbol name 0))
      id
      program {grinFunctions = filter hasEntry (grinFunctions program)}
      resultReps
  (_, items) <- runLower options gcProgram $ \env -> do
    lowerUnitItems env
    continuationInfoItems (ContinuationSpec threadDoneInfo (Symbol "aihc_lir_thread_done_applied_info") threadDoneTarget [] [Ptr] ContinuationFrameStop)
    threadDoneContinuation threadDoneTarget
    continuationInfoItems (ContinuationSpec snapshotInfo (Symbol "aihc_lir_snapshot_applied_info") snapshotTarget [] resultTypes ContinuationFrameStop)
    snapshotContinuation resultTypes
    observedMain
  let original = Module items
      observed = if gcStress then forceCollection original else original
  when (gcStress && observed == original) $
    Left (LowerUnsupportedExpression "GC stress fixture has no generated reservation")
  pure (observed, metadata)
  where
    program = gcGrinProgram gcProgram
    -- Abstract forwarding frames have info tables but no code entries.
    hasEntry function = Map.lookup (grinFunctionName function) (gcContinuationFrames gcProgram) /= Just ContinuationFrameForward
    options = LowerOptions {lowerUnitKind = LibraryUnit, lowerExposeFunctions = True, lowerTarget = target, lowerCheckPrimBounds = False}
    threadDoneInfo = Symbol "aihc_lir_thread_done_info"
    observedFunctionLabel name = pure (unSymbol (functionSymbol name))
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
      emit [buffer] (StackAlloc (toInteger (8 * max 1 (length resultTypes))) (byteAlignment 8))
      forM_ (zip [0 :: Int ..] values) $ \(index, (var, ty)) ->
        storeSlot ty (OperandVar var) (OperandVar buffer) (toInteger (8 * index))
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
      -- Both continuations capture nothing, so they are one word each: the
      -- driver reserves the two and takes them the way compiled code does.
      requireExtern (Symbol "aihc_ensure_heap") [Ptr, I64, I64, Ptr, Ptr] []
      emit [] (Call (Symbol "aihc_ensure_heap") [OperandVar machine, OperandLiteral (LitInt 2), OperandLiteral (LitInt 0), OperandLiteral LitNull, OperandLiteral LitNull])
      threadDone <- allocateContinuation (OperandVar machine) threadDoneInfo 1
      requireExtern (Symbol "aihc_set_thread_done_continuation") [Ptr, Ptr] []
      emit [] (Call (Symbol "aihc_set_thread_done_continuation") [OperandVar machine, threadDone])
      snapshot <- allocateContinuation (OperandVar machine) snapshotInfo 1
      requireExtern (Symbol "aihc_reset_heap_allocated_bytes") [Ptr] []
      emit [] (Call (Symbol "aihc_reset_heap_allocated_bytes") [OperandVar machine])
      emit [] (Call (functionSymbol entryName) [OperandVar machine, snapshot])
      terminate (Return [OperandLiteral (LitInt 0)])
      finishFunction (Symbol "main") Export [(argc, I32), (argv, Ptr)] [I32] CConvention

-- | Change only test output. Select the collector path of each generated
-- reservation. Identify collector blocks by their call, not their label.
forceCollection :: Module -> Module
forceCollection (Module items) = Module (map forceItem items)
  where
    forceItem (ItemFunction function) =
      ItemFunction function {functionBlocks = map forceBlock (functionBlocks function)}
      where
        collectors =
          [ blockLabel block
          | block <- functionBlocks function,
            Instruction _ (Call (Symbol "aihc_heap_collect") _) <- blockInstructions block
          ]
        forceBlock block =
          case blockTerminator block of
            Branch _ whenTrue whenFalse
              | targetLabel whenFalse `elem` collectors -> block {blockTerminator = Jump whenFalse}
              | targetLabel whenTrue `elem` collectors -> block {blockTerminator = Jump whenTrue}
            _ -> block
    forceItem item = item
