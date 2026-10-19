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
import Data.Text qualified as T
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
    continuationInfoItems (ContinuationSpec threadDoneInfo threadDoneTarget [] [Ptr] ContinuationFrameStop)
    threadDoneContinuation threadDoneTarget
    continuationInfoItems (ContinuationSpec snapshotInfo snapshotTarget [] resultTypes ContinuationFrameStop)
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
    options = LowerOptions {lowerUnitKind = LibraryUnit, lowerExposeFunctions = True, lowerTarget = target, lowerCheckPrimBounds = False, lowerProfileAllocations = False, lowerProfileTime = False}
    threadDoneInfo = Symbol "aihc_lir_thread_done_info"
    observedFunctionLabel name = pure (unSymbol (functionSymbol name))
    threadDoneTarget = Symbol "aihc_lir_thread_done_continuation"
    snapshotInfo = Symbol "aihc_lir_snapshot_info"
    snapshotTarget = Symbol "aihc_lir_snapshot_result"
    -- The snapshot continuation stores the result values in a buffer and
    -- hands them to the snapshot runtime, then returns to main. The
    -- snapshot reads the heap, so the machine gets the context first.
    snapshotContinuation resultTypes = do
      (context, contextParameters) <- freshContext
      values <- forM resultTypes $ \ty -> (,ty) <$> fresh "value"
      beginBlock (Label "entry") []
      machine <- capabilityOperand context
      storeContext machine context
      buffer <- fresh "buffer"
      emit [buffer] (StackAlloc (toInteger (8 * max 1 (length resultTypes))) (byteAlignment 8))
      forM_ (zip [0 :: Int ..] values) $ \(index, (var, ty)) ->
        storeSlot AnyRegion ty (OperandVar var) (OperandVar buffer) (toInteger (8 * index))
      requireExtern (Symbol "aihc_snapshot_dump_result") [I64, Ptr, Ptr] []
      emit [] (Call (Symbol "aihc_snapshot_dump_result") [OperandLiteral (LitInt (toInteger (length resultTypes))), OperandVar buffer, machine])
      terminate (Return [])
      finishFunction snapshotTarget Internal (contextParameters <> values) [] AihcConvention
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
      context <- loadContext (OperandVar machine)
      emit [] (Call (functionSymbol entryName) (contextOperands context <> [snapshot]))
      terminate (Return [OperandLiteral (LitInt 0)])
      finishFunction (Symbol "main") Export [(argc, I32), (argv, Ptr)] [I32] CConvention

-- | Change only test output. Select the collector path of each generated
-- reservation and call the explicit collector. A block refill is insufficient.
-- Identify collector blocks by their call, not their label.
--
-- The collector path of a reservation that runs its function again from
-- the entry is a tail call to a replay helper. A forced path there would
-- run the function again without end, so a flag selects it every second
-- time: the helper sets the flag before it enters the function again, and
-- the reservation clears the flag and takes the collector path only when
-- the flag was clear.
forceCollection :: Module -> Module
forceCollection (Module items) =
  Module (map collectorDeclaration (take 1 collectorDeclarations) <> [replayFlagItem | any usesReplay items] <> map forceItem items)
  where
    collectorDeclarations = filter isCollectorDeclaration items
    isCollectorDeclaration (ItemExternFunction external) =
      externFunctionName external `elem` [Symbol "aihc_heap_collect", Symbol "aihc_heap_allocate"]
    isCollectorDeclaration _ = False
    collectorDeclaration (ItemExternFunction external) = ItemExternFunction external {externFunctionName = Symbol "aihc_gc_collect", externFunctionSignature = Signature [Ptr, I64, I64, Ptr, Ptr] [] CConvention}
    collectorDeclaration item = item
    replayFlag = Symbol "aihc_test_replay_collected"
    replayFlagItem = ItemData (DataItem replayFlag Internal True 8 [DataInt I64 0])
    flagAddress = byteAddress (OperandLiteral (LitSymbol replayFlag)) 0
    isReplayHelper symbol = "aihc_lir_collect_replay_" `T.isPrefixOf` unSymbol symbol
    usesReplay (ItemFunction function) = isReplayHelper (functionName function)
    usesReplay _ = False
    forceItem (ItemFunction function)
      | isReplayHelper (functionName function) =
          ItemFunction function {functionBlocks = map setFlag (map forceBlock (functionBlocks function))}
      | otherwise =
          ItemFunction function {functionBlocks = map forceBlock (functionBlocks function)}
      where
        collectors =
          [ blockLabel block
          | block <- functionBlocks function,
            Instruction _ (Call (Symbol "aihc_heap_collect") _) <- blockInstructions block
          ]
        replays =
          [ blockLabel block
          | block <- functionBlocks function,
            TailCall symbol _ <- [blockTerminator block],
            isReplayHelper symbol
          ]
        forceBlock block =
          case blockTerminator block of
            Branch _ whenTrue whenFalse
              | targetLabel whenFalse `elem` collectors -> forced {blockTerminator = Jump whenFalse}
              | targetLabel whenTrue `elem` collectors -> forced {blockTerminator = Jump whenTrue}
              | targetLabel whenFalse `elem` replays -> alternate whenTrue whenFalse
              | targetLabel whenTrue `elem` replays -> alternate whenFalse whenTrue
            _ -> forced
          where
            forced = block {blockInstructions = concatMap forceInstruction (blockInstructions block)}
            flagValue = Var ("stress_flag_" <> unLabel (blockLabel block))
            flagSet = Var ("stress_flag_set_" <> unLabel (blockLabel block))
            alternate reserved collect =
              forced
                { blockInstructions =
                    blockInstructions forced
                      <> [ Instruction [flagValue] (Load I64 flagAddress (byteAlignment 8)),
                           Instruction [flagSet] (Compare Ne I64 (OperandVar flagValue) (OperandLiteral (LitInt 0))),
                           Instruction [] (Store I64 (OperandLiteral (LitInt 0)) flagAddress (byteAlignment 8))
                         ],
                  blockTerminator = Branch (OperandVar flagSet) reserved collect
                }
        setFlag block =
          case blockTerminator block of
            TailCallIndirect {} -> block {blockInstructions = blockInstructions block <> [Instruction [] (Store I64 (OperandLiteral (LitInt 1)) flagAddress (byteAlignment 8))]}
            _ -> block
    forceItem item = item
    forceInstruction (Instruction results (Call (Symbol "aihc_heap_collect") arguments)) =
      [Instruction results (Call (Symbol "aihc_gc_collect") arguments)]
    forceInstruction instruction@(Instruction _ (Call (Symbol "aihc_heap_allocate") (capability : _ : _ : arguments))) =
      [ Instruction [] (Call (Symbol "aihc_gc_collect") (capability : OperandLiteral (LitInt 0) : arguments)),
        instruction
      ]
    forceInstruction instruction = [instruction]
