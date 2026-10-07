{-# LANGUAGE LambdaCase #-}

-- | Fuzz tests for the generational collector.
--
-- No source fixture can drive this test. The collector's input is a heap
-- state together with the stacks of the threads, and a compiled program
-- reaches only the states its own evaluation produces. The test therefore
-- builds random heaps and stacks directly through the runtime interface. A
-- C driver plays the part of compiled code for several threads, and a model
-- in this module predicts what every collection must keep.
--
-- The model knows no collector policy. After every collection it checks
-- that each object and frame it reaches from the roots survived with the
-- same content, that the ages of the survivors grew, and that the scheduler
-- state matches. Only a full collection, which marks from scratch, must
-- keep the reachable set and nothing else. At the end of a gen2 cycle, the
-- gen2 objects that were dead at its snapshot must be gone. The runtime
-- verifier, built into the test runtime under @AIHC_GC_VERIFY@, checks the
-- internal invariants of the collector at the same points.
--
-- A script is a list of epochs. Each epoch reserves nursery space, allocates
-- one block of objects, then runs operations on the heap, the stacks, and
-- the threads, and then collects. The generator threads the model through
-- every choice, so a shrink of an early command regenerates the commands
-- after it against the shrunk state and every shrunk script stays valid.
module Test.Native.GcFuzz
  ( tests,
  )
where

import Aihc.Native (NativeTarget (Llvm), backendCompiler)
import Aihc.Testing.RuntimeArchive (RuntimeBuild (..), cachedRuntimeArchive)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (IOException, SomeException, throwIO, try)
import Control.Monad (forM, replicateM, unless)
import Data.Bits (shiftL, (.|.))
import Data.Char (isSpace)
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Word (Word64)
import Hedgehog (Gen, Property, annotate, classify, cover, evalIO, failure, forAllWith, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Numeric (readHex, showHex)
import System.Directory (removeDirectoryRecursive)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import System.FilePath ((</>))
import System.IO (BufferMode (BlockBuffering), Handle, hClose, hFlush, hGetContents, hGetLine, hPutStr, hSetBuffering)
import System.IO.Error (tryIOError)
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process (CreateProcess (std_err, std_in, std_out), ProcessHandle, StdStream (CreatePipe), createProcess, proc, readProcessWithExitCode, terminateProcess, waitForProcess)
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup, withResource)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

-- * Tests

tests :: TestTree
tests =
  withResource compileDriver (removeDirectoryRecursive . fst) $ \getBuild ->
    withResource (newDriver getBuild) stopDriver $ \getDriver ->
      testGroup
        "generational collector fuzz"
        [ testProperty "collects random heaps and stacks" (prop_collect getDriver),
          testCase "forwards the target of an evaluated static thunk" (checkScript getDriver evaluatedStaticScript),
          testCase "scans the frames below a pop across chunks" (checkScript getDriver deepPopScript),
          testCase "keeps a thunk a waiter of another thread names" (checkScript getDriver waiterScript)
        ]

-- | Evaluate a static thunk into a heap object that only the thunk keeps
-- alive, then force a collection. The thunk must reach the moved object
-- afterwards.
--
-- This fixed script is a unit test for the same reason as the property: no
-- source fixture can force a collection at a chosen heap state, so the
-- script drives the runtime directly.
evaluatedStaticScript :: [Command]
evaluatedStaticScript =
  [ CMachine 0 0 64 0,
    CSrt 0 [0] [],
    CCurrentSrt (Just 0),
    CPush 1 FSStop,
    CReserve 2,
    CNew 2 KNode [] Nothing,
    CSUpdate 0 (VHeap 2),
    CFill 0,
    CCollect
  ]

-- | A stack of frames across several chunks, a gen2 cycle with a small
-- slice, and a pop into a lower chunk followed by new frames. The frames
-- below the pop keep their referents.
deepPopScript :: [Command]
deepPopScript =
  [CMachine 0 0 512 64, CCurrentSrt Nothing, CPush 1 FSStop, CReserve 8, CNew 2 KNode [True] Nothing, CNew 3 KNode [True] Nothing]
    <> [CPush fid (FSNormal (replicate 200 False) Nothing (replicate 200 (VWord 7))) | fid <- [2 .. 9]]
    <> [CPush 10 (FSNormal [True] Nothing [VHeap 2]), CPush 11 (FSNormal [True] Nothing [VHeap 3])]
    <> [CCycle, CEnter 11 Nothing, CEnter 10 Nothing, CEnter 9 Nothing]
    <> [CPush fid (FSNormal (replicate 200 False) Nothing (replicate 200 (VWord 9))) | fid <- [12 .. 14]]
    <> [CReserve 2, CNew 4 KNode [True] Nothing, CSet 4 0 (VHeap 2), CCollect, CCollectGeneration 1, CCollect, CCollectGeneration 2]

-- | A thread blocks on a thunk another thread evaluates. The waiter keeps
-- the continuation frame of the blocked thread, and the update wakes it.
waiterScript :: [Command]
waiterScript =
  [ CMachine 0 0 512 0,
    CCurrentSrt Nothing,
    CPush 1 FSStop,
    CReserve 32,
    CNew 2 KThunk [True] Nothing,
    CNew 3 KNode [] Nothing,
    CNew 4 KClosure [] Nothing,
    CPush 2 (FSNormal [True] Nothing [VHeap 3]),
    CPush 3 (FSUpdate 2),
    CPush 4 (FSNormal [] Nothing []),
    CFork 5 5 (VHeap 4),
    CYield,
    CPush 6 (FSNormal [True] Nothing [VHeap 2]),
    CBlock 2,
    CCollect,
    CEnter 3 (Just (VHeap 3)),
    CCollectGeneration 1,
    CYield,
    CCollect
  ]

-- | Run one fixed script and compare the driver's reports with the model.
checkScript :: IO Driver -> [Command] -> IO ()
checkScript getDriver script = do
  driver <- getDriver
  result <- runScript driver (renderScript script)
  output <- either assertFailure pure result
  reports <- either assertFailure pure (parseReports output)
  let problems = replay script reports
  assertBool ("driver output:\n" <> unlines output <> unlines problems) (null problems)
  assertBool "the script reports a collection" (not (Map.null reports))

prop_collect :: IO Driver -> Property
prop_collect getDriver = property $ do
  script <- forAllWith renderScript genScript
  driver <- evalIO getDriver
  result <- evalIO (runScript driver (renderScript script))
  case result of
    Left message -> do
      annotate message
      failure
    Right output ->
      case parseReports output of
        Left message -> do
          annotate ("driver output:\n" <> unlines output)
          annotate message
          failure
        Right reports -> do
          let problems = replay script reports
              has predicate = any predicate script
              reportHas predicate = any predicate (Map.elems reports)
          classify (fromString "several collections") (Map.size reports > 1)
          classify (fromString "gen1 collection") (reportHas ((== 1) . rCollected))
          cover 2 (fromString "full collection") (reportHas ((== 2) . rCollected))
          cover 2 (fromString "gen2 cycle spans collections") (reportHas (\r -> rFinish r && not (rCycleStart r) && rCollected r < 2))
          cover 2 (fromString "collection at a reservation") (or [Map.member index reports | (index, CReserve _) <- zip [0 ..] script])
          cover 5 (fromString "thunk update") (has (\case CEnter _ (Just _) -> True; _ -> False))
          cover 2 (fromString "static thunk update") (has (\case CSUpdate {} -> True; _ -> False))
          cover 2 (fromString "large array") (has (\case CArray _ count _ -> count >= largeArrayElements; _ -> False))
          cover 5 (fromString "several threads") (has (\case CFork {} -> True; _ -> False))
          cover 2 (fromString "thread blocks on an MVar") (has (\case CTake _ -> True; CRead _ -> True; CPut {} -> True; _ -> False))
          cover 0.5 (fromString "thread blocks on a blackhole") (has (\case CBlock _ -> True; _ -> False))
          cover 1 (fromString "exception") (has (\case CRaise _ -> True; _ -> False))
          cover 2 (fromString "stack of several chunks") (maximum (map deepestStack (scanl (flip applyCommand) emptyModel script)) > 1)
          cover 1 (fromString "thread ends") (has (\case CEnter fid _ -> fid == 1; _ -> False) || has (\case CFork {} -> True; _ -> False))
          classify (fromString "decoy word") (has (\case CSet _ _ (VDecoy _) -> True; _ -> False))
          classify (fromString "stable name") (has (\case CStable _ -> True; _ -> False))
          unless (null problems) $ do
            annotate ("driver output:\n" <> unlines output)
            annotate (unlines problems)
            failure

-- * Model

type Id = Int

type Fid = Int

data Kind = KNode | KClosure | KThunk | KPartial
  deriving (Eq, Show)

-- | A slot value. A decoy is the address of a live object written into a
-- non-pointer field, so the collector must leave it alone. A frame value
-- occurs only in the resumption of a thread and in the waiters of the
-- runtime.
data Value = VNull | VHeap Id | VStatic Int | VWord Word64 | VDecoy Id | VFrame Fid
  deriving (Eq, Show)

-- | A thread blocked on an MVar or a blackhole: the thread, the frame it
-- continues with, and for a putter the value it offers.
data Waiter = Waiter
  { wThread :: Id,
    wFrame :: Fid,
    wValue :: Maybe Value
  }
  deriving (Eq, Show)

-- | How a suspended thread continues when the scheduler selects it.
data Resume
  = RNone
  | RApply Value Fid
  | RContinue Fid (Maybe Value)
  | RRaise Value Fid
  deriving (Eq, Show)

data Object
  = Object
      { oKind :: Kind,
        oPointers :: [Bool],
        oFields :: [Value],
        oSrt :: Maybe Int,
        oBlackholed :: Bool
      }
  | Array [Value] (Maybe Int)
  | Ind Value
  | MVar MVarState
  | Thread ThreadState
  deriving (Eq, Show)

data MVarState = MVarState
  { mvValue :: Maybe Value,
    mvReaders :: [Waiter],
    mvTakers :: [Waiter],
    mvPutters :: [Waiter]
  }
  deriving (Eq, Show)

data ThreadState = ThreadState
  { thResume :: Resume,
    -- | The topmost live frame, or nothing for a thread without frames.
    thTop :: Maybe Fid
  }
  deriving (Eq, Show)

data FrameKind = FNormal | FForward | FCatch | FPrompt | FUpdate | FStop
  deriving (Eq, Show)

-- | A continuation frame. Field zero of a frame is its parent; the fields
-- here are the ones after it.
data Frame = Frame
  { fKind :: FrameKind,
    fPointers :: [Bool],
    fFields :: [Value],
    fSrt :: Maybe Int,
    fParent :: Maybe Fid,
    fThread :: Id
  }
  deriving (Eq, Show)

-- | A static thunk is unevaluated, evaluated to a value, or stale: a
-- collection freed its value because no code reached the slot. No command
-- names a stale slot again.
data StaticThunk = SThunk | SInd Value | SStale
  deriving (Eq, Show)

data Model = Model
  { mHeap :: Map Id Object,
    -- | The live frames of every stack.
    mFrames :: Map Fid Frame,
    mNextId :: Id,
    mNextFid :: Fid,
    mGlobals :: [Value],
    mRoots :: [Value],
    -- | Weak referents of names retained by the driver, newest first.
    mStable :: [Value],
    mRunning :: Id,
    mQueue :: [Id],
    -- | The waiters of each contended thunk, in wake order.
    mBlackholes :: Map Id [Waiter],
    mStaticThunks :: [StaticThunk],
    mStaticThunkSrts :: [Maybe Int],
    mStaticNodes :: [[Value]],
    mStaticNodeSrts :: [Maybe Int],
    mSrts :: Map Int ([Int], [Int]),
    mCurrentSrt :: Maybe Int,
    -- | The generation of each heap object after the last reported
    -- collection. An object the driver has not reported is in the nursery.
    mAges :: Map Id Int,
    -- | While a gen2 cycle is active: the gen2 objects that were dead at
    -- its snapshot. The cycle frees them when it ends.
    mSnapshotDead :: Maybe (Set Id),
    -- | Whether the runtime would have stopped the program: a precondition
    -- of a command did not hold. The generator never emits such a command.
    mFailed :: Bool
  }
  deriving (Show)

staticThunkCount, staticNodeCount, staticNullaryCount, staticRootedCount, staticCount, staticNodeFields :: Int
staticThunkCount = 8
staticNodeCount = 4
staticNullaryCount = 4
staticRootedCount = staticThunkCount + staticNodeCount
staticCount = staticRootedCount + staticNullaryCount
staticNodeFields = 2

-- | The identity of the initial thread.
initialThread :: Id
initialThread = 1

emptyModel :: Model
emptyModel =
  Model
    { mHeap = Map.singleton initialThread (Thread (ThreadState RNone Nothing)),
      mFrames = Map.empty,
      mNextId = initialThread + 1,
      mNextFid = 1,
      mGlobals = [],
      mRoots = [],
      mStable = [],
      mRunning = initialThread,
      mQueue = [],
      mBlackholes = Map.empty,
      mStaticThunks = replicate staticThunkCount SThunk,
      mStaticThunkSrts = replicate staticThunkCount Nothing,
      mStaticNodes = replicate staticNodeCount (replicate staticNodeFields VNull),
      mStaticNodeSrts = replicate staticNodeCount Nothing,
      mSrts = Map.empty,
      mCurrentSrt = Nothing,
      mAges = Map.empty,
      mSnapshotDead = Nothing,
      mFailed = False
    }

-- | The words of an MVar and of a thread record on the 64-bit targets.
recordWords :: Int
recordWords = 9

-- | The words of an MVar waiter, a blackhole waiter, and a stable name.
mvarWaiterWords, blackholeWaiterWords, stableNameWords :: Int
mvarWaiterWords = 5
blackholeWaiterWords = 4
stableNameWords = 4

-- | The words of frames one stack chunk holds.
chunkWords :: Int
chunkWords = (4096 - 64) `div` 8

-- | The number of chunks the deepest stack of the model takes. A frame
-- that does not fit in the rest of a chunk starts the next one, as
-- @aihc_stack_push@ places it.
deepestStack :: Model -> Int
deepestStack model = maximum (1 : [chunks (reverse (chainOf top)) | Thread (ThreadState _ (Just top)) <- Map.elems (mHeap model)])
  where
    chainOf fid = case Map.lookup fid (mFrames model) of
      Just frame -> fid : maybe [] chainOf (fParent frame)
      Nothing -> []
    chunks = go 1 0
    go count _ [] = count
    go count used (fid : rest) =
      let size = 1 + length (fFields (mFrames model Map.! fid)) + (if fKind (mFrames model Map.! fid) == FStop then 0 else 1)
       in if used + size > chunkWords then go (count + 1) size rest else go count (used + size) rest

-- | Follow heap indirections to the value they name.
resolve :: Model -> Value -> Value
resolve model = go (1000 :: Int)
  where
    go 0 _ = error "indirection chain is too long"
    go fuel value = case value of
      VHeap identity
        | Just (Ind target) <- Map.lookup identity (mHeap model) -> go (fuel - 1) target
      _ -> value

setAt :: Int -> a -> [a] -> [a]
setAt index value list = [if position == index then value else old | (position, old) <- zip [0 ..] list]

-- * Commands

-- | The shape of a pushed frame.
data FrameSpec
  = FSNormal [Bool] (Maybe Int) [Value]
  | FSForward [Bool] (Maybe Int) [Value]
  | FSCatch (Maybe Int) Value
  | FSPrompt (Maybe Int) Value
  | FSUpdate Id
  | FSStop
  deriving (Eq, Show)

data Command
  = -- | Globals, root slots, nursery bytes, and the bytes of one mark slice,
    -- or zero for the default slice.
    CMachine Int Int Int Int
  | CSrt Int [Int] [Int]
  | CCurrentSrt (Maybe Int)
  | CSSrt Int (Maybe Int)
  | CFill Int
  | CReserve Int
  | CNew Id Kind [Bool] (Maybe Int)
  | CArray Id Int (Maybe Int)
  | CMVar Id
  | CSet Id Int Value
  | CSUpdate Int Value
  | CSSet Int Int Value
  | CGlobal Int Value
  | CRoot Int Value
  | CStable Value
  | CPush Fid FrameSpec
  | CEnter Fid (Maybe Value)
  | CRaise Value
  | CYield
  | CFork Id Fid Value
  | CBlock Id
  | CTake Id
  | CRead Id
  | CPut Id Value
  | CCollect
  | CCollectGeneration Int
  | -- | Collect the nursery and gen1 and start a gen2 cycle.
    CCycle
  deriving (Eq, Show)

renderValue :: Value -> String
renderValue VNull = "n"
renderValue (VHeap identity) = 'h' : show identity
renderValue (VStatic slot) = 's' : show slot
renderValue (VWord word) = 'w' : showHex word ""
renderValue (VDecoy identity) = 'a' : show identity
renderValue (VFrame fid) = 'f' : show fid

renderSrt :: Maybe Int -> String
renderSrt = maybe "-1" show

renderBitmap :: [Bool] -> String
renderBitmap [] = "-"
renderBitmap pointers = map (\p -> if p then '1' else '0') pointers

renderCommand :: Command -> String
renderCommand command = unwords $ case command of
  CMachine globals roots bytes slice -> ["machine", show globals, show roots, show bytes, show slice]
  CSrt index objects children -> ["srt", show index, show (length objects), show (length children)] <> map (('s' :) . show) objects <> map show children
  CCurrentSrt srt -> ["current_srt", renderSrt srt]
  CSSrt slot srt -> ["ssrt", show slot, renderSrt srt]
  CFill keep -> ["fill", show keep]
  CReserve count -> ["reserve", show count]
  CNew identity kind pointers srt -> ["new", show identity, kindName kind, renderBitmap pointers, renderSrt srt]
  CArray identity count srt -> ["array", show identity, show count, renderSrt srt]
  CMVar identity -> ["mvar", show identity]
  CSet identity index value -> ["set", show identity, show index, renderValue value]
  CSUpdate slot value -> ["supdate", show slot, renderValue value]
  CSSet slot index value -> ["sset", show slot, show index, renderValue value]
  CGlobal index value -> ["global", show index, renderValue value]
  CRoot index value -> ["root", show index, renderValue value]
  CStable value -> ["stable", renderValue value]
  CPush fid spec -> ["push", show fid] <> renderSpec spec
  CEnter fid value -> ["enter", show fid] <> maybe [] (pure . renderValue) value
  CRaise value -> ["raise", renderValue value]
  CYield -> ["yield"]
  CFork identity fid action -> ["fork", show identity, show fid, renderValue action]
  CBlock identity -> ["block", renderValue (VHeap identity)]
  CTake identity -> ["take", renderValue (VHeap identity)]
  CRead identity -> ["read", renderValue (VHeap identity)]
  CPut identity value -> ["put", renderValue (VHeap identity), renderValue value]
  CCollect -> ["collect"]
  CCollectGeneration generation -> ["collect", show generation]
  CCycle -> ["cycle"]
  where
    renderSpec spec = case spec of
      FSNormal pointers srt values -> ["normal", renderBitmap pointers, renderSrt srt] <> map renderValue values
      FSForward pointers srt values -> ["forward", renderBitmap pointers, renderSrt srt] <> map renderValue values
      FSCatch srt handler -> ["catch", renderSrt srt, renderValue handler]
      FSPrompt srt tag -> ["prompt", renderSrt srt, renderValue tag]
      FSUpdate thunk -> ["update", renderValue (VHeap thunk)]
      FSStop -> ["stop"]

kindName :: Kind -> String
kindName KNode = "node"
kindName KClosure = "closure"
kindName KThunk = "thunk"
kindName KPartial = "partial"

frameKindName :: FrameKind -> String
frameKindName FNormal = "normal"
frameKindName FForward = "forward"
frameKindName FCatch = "catch"
frameKindName FPrompt = "prompt"
frameKindName FUpdate = "update"
frameKindName FStop = "stop"

renderScript :: [Command] -> String
renderScript = unlines . map renderCommand

-- * The step function

-- | Apply the effect of one command. A collection changes nothing in the
-- model: the replay reads what it kept from the report of the driver. A
-- command whose precondition does not hold sets the failure flag.
applyCommand :: Command -> Model -> Model
applyCommand command model = case command of
  CMachine globals roots _ _ -> emptyModel {mGlobals = replicate globals VNull, mRoots = replicate roots VNull}
  CSrt index objects children -> model {mSrts = Map.insert index (objects, children) (mSrts model)}
  CCurrentSrt srt -> model {mCurrentSrt = srt}
  CSSrt slot srt
    | slot < staticThunkCount -> model {mStaticThunkSrts = setAt slot srt (mStaticThunkSrts model)}
    | otherwise -> model {mStaticNodeSrts = setAt (slot - staticThunkCount) srt (mStaticNodeSrts model)}
  CFill _ -> model
  CReserve _ -> model
  CNew identity kind pointers srt ->
    insertObject identity (Object kind pointers [if p then VNull else VWord 0 | p <- pointers] srt False)
  CArray identity count srt -> insertObject identity (Array (replicate count VNull) srt)
  CMVar identity -> insertObject identity (MVar (MVarState Nothing [] [] []))
  CSet identity index value -> adjustObject identity $ \case
    Object kind pointers fields srt blackholed -> Object kind pointers (setAt index value fields) srt blackholed
    Array elements srt -> Array (setAt index value elements) srt
    other -> error ("set on " <> show other)
  CSUpdate slot value -> model {mStaticThunks = setAt slot (SInd value) (mStaticThunks model)}
  CSSet slot index value ->
    let node = slot - staticThunkCount
     in model {mStaticNodes = setAt node (setAt index value (mStaticNodes model !! node)) (mStaticNodes model)}
  CGlobal index value -> model {mGlobals = setAt index value (mGlobals model)}
  CRoot index value -> model {mRoots = setAt index value (mRoots model)}
  -- The runtime gives one name to an object, and it looks the object up
  -- through the indirections of the names it holds.
  CStable value
    | resolve model value `elem` map (resolve model) (mStable model) -> model
    | otherwise -> model {mStable = value : mStable model}
  CPush fid spec -> pushFrame fid spec model
  CEnter fid value -> case Map.lookup fid (mFrames model) of
    Just frame | fThread frame == mRunning model -> continueInto (mRunning model) fid value model
    _ -> failed
  CRaise exception -> case runningTop model of
    Just top -> raise (mRunning model) exception top model
    Nothing -> failed
  CYield -> case runningTop model of
    Just top -> schedule (enqueue (mRunning model) (suspend (mRunning model) (RContinue top Nothing) model))
    Nothing -> failed
  CFork identity fid action ->
    let thread = Thread (ThreadState (RApply action fid) (Just fid))
        frame = Frame FStop [] [] Nothing Nothing identity
     in enqueue identity (insertObject identity thread) {mFrames = Map.insert fid frame (mFrames model), mNextFid = max (mNextFid model) (fid + 1)}
  CBlock thunk -> case (Map.lookup thunk (mHeap model), runningTop model) of
    (Just (Object KThunk _ _ _ True), Just top)
      | evaluates (mRunning model) thunk model -> failed
      | otherwise ->
          let waiter = Waiter (mRunning model) top Nothing
           in schedule model {mBlackholes = Map.insertWith (flip (<>)) thunk [waiter] (mBlackholes model)}
    _ -> failed
  CTake identity -> withMVar identity $ \mvar top -> case mvValue mvar of
    Nothing -> schedule (storeMVar identity mvar {mvTakers = mvTakers mvar <> [Waiter (mRunning model) top Nothing]} model)
    Just value -> case mvPutters mvar of
      [] -> continueInto (mRunning model) top (Just value) (storeMVar identity mvar {mvValue = Nothing} model)
      putter : rest ->
        continueInto (mRunning model) top (Just value) $
          wake putter Nothing (storeMVar identity mvar {mvValue = wValue putter, mvPutters = rest} model)
  CRead identity -> withMVar identity $ \mvar top -> case mvValue mvar of
    Nothing -> schedule (storeMVar identity mvar {mvReaders = mvReaders mvar <> [Waiter (mRunning model) top Nothing]} model)
    Just value -> continueInto (mRunning model) top (Just value) model
  CPut identity value -> withMVar identity $ \mvar top -> case mvValue mvar of
    Just _ -> schedule (storeMVar identity mvar {mvPutters = mvPutters mvar <> [Waiter (mRunning model) top (Just value)]} model)
    Nothing ->
      let woken = foldl' (\m reader -> wake reader (Just value) m) model (mvReaders mvar)
          (mvar', afterTaker) = case mvTakers mvar of
            [] -> (mvar {mvValue = Just value, mvReaders = []}, woken)
            taker : rest -> (mvar {mvReaders = [], mvTakers = rest}, wake taker (Just value) woken)
       in continueInto (mRunning model) top Nothing (storeMVar identity mvar' afterTaker)
  CCollect -> model
  CCollectGeneration generation -> staleStatics (generation == 2) model
  CCycle -> staleStatics True model
  where
    failed = model {mFailed = True}
    insertObject identity object =
      model {mHeap = Map.insert identity object (mHeap model), mNextId = max (mNextId model) (identity + 1)}
    adjustObject identity change = model {mHeap = Map.adjust change identity (mHeap model)}
    withMVar identity continue = case (Map.lookup identity (mHeap model), runningTop model) of
      (Just (MVar mvar), Just top) -> continue mvar top
      _ -> failed

storeMVar :: Id -> MVarState -> Model -> Model
storeMVar identity mvar model = model {mHeap = Map.insert identity (MVar mvar) (mHeap model)}

threadOf :: Id -> Model -> ThreadState
threadOf identity model = case Map.lookup identity (mHeap model) of
  Just (Thread thread) -> thread
  _ -> error ("thread " <> show identity <> " is not in the model")

storeThread :: Id -> ThreadState -> Model -> Model
storeThread identity thread model = model {mHeap = Map.insert identity (Thread thread) (mHeap model)}

runningTop :: Model -> Maybe Fid
runningTop model = thTop (threadOf (mRunning model) model)

-- | Whether a thread has an update frame for a thunk on its stack.
evaluates :: Id -> Id -> Model -> Bool
evaluates thread thunk model = any updates (Map.elems (mFrames model))
  where
    updates frame = fThread frame == thread && fKind frame == FUpdate && fFields frame == [VHeap thunk]

-- | Push a frame on the stack of the running thread.
pushFrame :: Fid -> FrameSpec -> Model -> Model
pushFrame fid spec model = case (spec, runningTop model) of
  (FSStop, Nothing) -> push (Frame FStop [] [] Nothing Nothing running)
  (FSStop, Just _) -> failed
  (_, Nothing) -> failed
  (FSNormal pointers srt values, Just top) -> push (Frame FNormal pointers values srt (Just top) running)
  (FSForward pointers srt values, Just top) -> push (Frame FForward pointers values srt (Just top) running)
  (FSCatch srt handler, Just top) -> push (Frame FCatch [True] [handler] srt (Just top) running)
  (FSPrompt srt tag, Just top) -> push (Frame FPrompt [True] [tag] srt (Just top) running)
  (FSUpdate thunk, Just top) -> case Map.lookup thunk (mHeap model) of
    Just (Object KThunk pointers fields srt False) ->
      pushOn
        model {mHeap = Map.insert thunk (Object KThunk pointers fields srt True) (mHeap model)}
        (Frame FUpdate [True] [VHeap thunk] Nothing (Just top) running)
    _ -> failed
  where
    running = mRunning model
    failed = model {mFailed = True}
    push = pushOn model
    pushOn before frame =
      let thread = threadOf running before
       in (storeThread running thread {thTop = Just fid} before)
            { mFrames = Map.insert fid frame (mFrames before),
              mNextFid = max (mNextFid before) (fid + 1)
            }

-- | Pop the frames of a thread from its top down to a frame, inclusive.
popThrough :: Id -> Fid -> Model -> Model
popThrough thread target model =
  let frame = fromMaybe (error "popped frame is not live") (Map.lookup target (mFrames model))
      above = takeWhile (/= target) (chain (thTop (threadOf thread model)))
      chain Nothing = []
      chain (Just fid) = fid : chain (fParent =<< Map.lookup fid (mFrames model))
      record = threadOf thread model
   in (storeThread thread record {thTop = fParent frame} model)
        { mFrames = foldr Map.delete (mFrames model) (target : above)
        }

-- | Pop the frames of a thread above a frame, which becomes its top.
popAbove :: Id -> Fid -> Model -> Model
popAbove thread target model =
  let above = takeWhile (/= target) (chain (thTop (threadOf thread model)))
      chain Nothing = []
      chain (Just fid) = fid : chain (fParent =<< Map.lookup fid (mFrames model))
      record = threadOf thread model
   in (storeThread thread record {thTop = Just target} model)
        { mFrames = foldr Map.delete (mFrames model) above
        }

-- | The frame a continue helper enters for a continuation: the first frame
-- of its chain that is not a forward frame.
enteredFrame :: Fid -> Model -> Maybe Fid
enteredFrame fid model = case Map.lookup fid (mFrames model) of
  Just frame | fKind frame == FForward -> fParent frame >>= \parent -> enteredFrame parent model
  Just _ -> Just fid
  Nothing -> Nothing

-- | Continue into a frame of a thread with an optional value, as the entry
-- of the frame would: an update frame updates its thunk with the value and
-- wakes the waiters of the thunk, a stop frame ends the thread, and every
-- other frame runs its code, which the script spells out.
continueInto :: Id -> Fid -> Maybe Value -> Model -> Model
continueInto thread fid value model = case enteredFrame fid model of
  Nothing -> failed
  Just target ->
    let frame = fromMaybe (error "entered frame is not live") (Map.lookup target (mFrames model))
        popped = popThrough thread target model
     in case fKind frame of
          -- A thunk whose value leads back to itself is a loop, which no
          -- program reaches: the update would make an indirection cycle.
          FUpdate -> case (fFields frame, value) of
            ([VHeap thunk], Just result) | result /= VNull && resolve model result /= VHeap thunk -> update thunk result popped
            _ -> failed
          FStop -> schedule (storeThread thread (ThreadState RNone Nothing) popped)
          _ -> popped
  where
    failed = model {mFailed = True}
    update thunk result after =
      let waiters = Map.findWithDefault [] thunk (mBlackholes after)
          updated = after {mHeap = Map.insert thunk (Ind result) (mHeap after), mBlackholes = Map.delete thunk (mBlackholes after)}
       in foldl' (\m waiter -> wake waiter (Just result) m) updated waiters

-- | Give a woken waiter its continuation and put its thread on the run
-- queue.
wake :: Waiter -> Maybe Value -> Model -> Model
wake waiter value model = enqueue (wThread waiter) (suspend (wThread waiter) (RContinue (wFrame waiter) value) model)

suspend :: Id -> Resume -> Model -> Model
suspend thread resume model = storeThread thread (threadOf thread model) {thResume = resume} model

enqueue :: Id -> Model -> Model
enqueue thread model = model {mQueue = mQueue model <> [thread]}

-- | Select the next runnable thread and give it its resumption.
schedule :: Model -> Model
schedule model = case mQueue model of
  [] -> model {mFailed = True}
  thread : rest ->
    let record = threadOf thread model
        selected = (storeThread thread record {thResume = RNone} model) {mQueue = rest, mRunning = thread}
     in case thResume record of
          RNone -> model {mFailed = True}
          RApply _ continuation -> popAbove thread continuation selected
          RContinue continuation value -> continueInto thread continuation value selected
          RRaise exception continuation -> raise thread exception continuation selected

-- | Raise an exception on a thread from a frame: the walk pops frames down
-- to a catch frame, abandons the thunks of the update frames it passes,
-- and applies the handler under the parent of the catch frame.
raise :: Id -> Value -> Fid -> Model -> Model
raise thread exception = go
  where
    go fid model = case Map.lookup fid (mFrames model) of
      Nothing -> model {mFailed = True}
      Just frame -> case fKind frame of
        FCatch -> case fParent frame of
          Just parent -> popAbove thread parent model
          Nothing -> model {mFailed = True}
        FUpdate -> case (fFields frame, fParent frame) of
          ([VHeap thunk], Just parent) -> go parent (abandon thunk model)
          _ -> model {mFailed = True}
        FStop -> model {mFailed = True}
        _ -> maybe model {mFailed = True} (`go` model) (fParent frame)
    abandon thunk model =
      let waiters = Map.findWithDefault [] thunk (mBlackholes model)
          plain = case Map.lookup thunk (mHeap model) of
            Just (Object kind pointers fields srt _) -> Object kind pointers fields srt False
            other -> error ("abandoned thunk is " <> show other)
          cleared = model {mHeap = Map.insert thunk plain (mHeap model), mBlackholes = Map.delete thunk (mBlackholes model)}
       in foldl' (\m waiter -> enqueue (wThread waiter) (suspend (wThread waiter) (RRaise exception (wFrame waiter)) m)) cleared waiters

-- | Mark the evaluated static thunks that a full collection or a gen2
-- cycle frees the value of: the slots no live code reaches. A cycle
-- judges them at its snapshot, which is now. The model is conservative
-- for a cycle that ends later: it stops naming the slot at once.
staleStatics :: Bool -> Model -> Model
staleStatics full model
  | not full = model
  | otherwise =
      let live = liveStatics (liveness model)
       in model {mStaticThunks = [if state /= SThunk && not (Set.member slot live) then SStale else state | (slot, state) <- zip [0 ..] (mStaticThunks model)]}

applyCommands :: [Command] -> Model -> Model
applyCommands commands model = foldl' (flip applyCommand) model commands

-- * Liveness

data Live = Live
  { liveHeap :: Set Id,
    liveFrames :: Set Fid,
    -- | The indirections a path from the roots passes through.
    liveInds :: Set Id,
    -- | The static objects live code reaches: through the current table,
    -- the tables of the live objects and frames, and their fields.
    liveStatics :: Set Int
  }

data Item = IHeap Id | IFrame Fid | IStatic Int | ISrt Int

-- | The heap objects, frames, and static objects a collection keeps:
-- everything the roots reach. The roots are the globals, the root slots,
-- the running thread and its continuation, the run queue, the blackhole
-- table, and the table of the running code. A minor collection does not
-- trace the static objects, but it scans every updated static thunk from
-- the remembered set, so what the model reaches through a static object
-- survives a minor collection as well.
liveness :: Model -> Live
liveness = livenessFrom False

-- | The objects the snapshot of a gen2 cycle reaches. The snapshot scans
-- the top chunk of every stack, so the frames of a thread that nothing
-- reaches are roots of the snapshot as well, and what they name survives
-- the cycle.
snapshotLiveness :: Model -> Live
snapshotLiveness = livenessFrom True

livenessFrom :: Bool -> Model -> Live
livenessFrom stacksAreRoots model = go initial (Live Set.empty Set.empty Set.empty Set.empty) Set.empty
  where
    rootValues =
      mGlobals model
        <> mRoots model
        <> [VHeap (mRunning model)]
        <> maybe [] (pure . VFrame) (runningTop model)
        <> map VHeap (mQueue model)
        <> concat [VHeap thunk : concatMap waiterValues waiters | (thunk, waiters) <- Map.toList (mBlackholes model)]
        <> [VFrame top | stacksAreRoots, Thread (ThreadState _ (Just top)) <- Map.elems (mHeap model)]
    initial = concatMap fromValue rootValues <> fromSrt (mCurrentSrt model)
    fromValue value = case value of
      VHeap identity -> [IHeap identity]
      VFrame fid -> [IFrame fid]
      VStatic slot -> [IStatic slot]
      _ -> []
    fromSrt = maybe [] (pure . ISrt)
    go [] live _ = live
    go (item : rest) live seenSrts = case item of
      IHeap identity
        | Set.member identity (liveHeap live) || Set.member identity (liveInds live) -> go rest live seenSrts
        | otherwise -> case Map.lookup identity (mHeap model) of
            Just (Ind target) -> go (fromValue target <> rest) live {liveInds = Set.insert identity (liveInds live)} seenSrts
            Just object -> go (concatMap fromValue (objectChildren object) <> fromSrt (objectSrt object) <> rest) live {liveHeap = Set.insert identity (liveHeap live)} seenSrts
            Nothing -> error ("live object " <> show identity <> " is not in the model")
      IFrame fid
        | Set.member fid (liveFrames live) -> go rest live seenSrts
        | otherwise -> case Map.lookup fid (mFrames model) of
            Just frame -> go (concatMap fromValue (frameChildren frame) <> fromSrt (fSrt frame) <> rest) live {liveFrames = Set.insert fid (liveFrames live)} seenSrts
            Nothing -> error ("live frame " <> show fid <> " is not in the model")
      IStatic slot
        | Set.member slot (liveStatics live) -> go rest live seenSrts
        | otherwise ->
            let children
                  | slot < staticThunkCount = case mStaticThunks model !! slot of
                      SThunk -> fromSrt (mStaticThunkSrts model !! slot)
                      SInd target -> fromValue target
                      -- The generator keeps a stale slot out of every path.
                      SStale -> []
                  | slot < staticRootedCount =
                      let node = slot - staticThunkCount
                       in concatMap fromValue (mStaticNodes model !! node) <> fromSrt (mStaticNodeSrts model !! node)
                  | otherwise = []
             in go (children <> rest) live {liveStatics = Set.insert slot (liveStatics live)} seenSrts
      ISrt index
        | Set.member index seenSrts -> go rest live seenSrts
        | otherwise ->
            let (objects, children) = fromMaybe ([], []) (Map.lookup index (mSrts model))
             in go (map IStatic objects <> map ISrt children <> rest) live (Set.insert index seenSrts)

waiterValues :: Waiter -> [Value]
waiterValues waiter = [VHeap (wThread waiter), VFrame (wFrame waiter)] <> maybe [] pure (wValue waiter)

objectChildren :: Object -> [Value]
objectChildren object = case object of
  Object _ pointers fields _ _ -> [field | (True, field) <- zip pointers fields]
  Array elements _ -> elements
  Ind target -> [target]
  MVar (MVarState value readers takers putters) -> maybe [] pure value <> concatMap waiterValues (readers <> takers <> putters)
  Thread (ThreadState resume _) -> case resume of
    RNone -> []
    RApply function continuation -> [function, VFrame continuation]
    RContinue continuation value -> VFrame continuation : maybe [] pure value
    RRaise exception continuation -> [exception, VFrame continuation]

objectSrt :: Object -> Maybe Int
objectSrt object = case object of
  Object _ _ _ srt _ -> srt
  Array _ srt -> srt
  _ -> Nothing

frameChildren :: Frame -> [Value]
frameChildren frame = maybe [] (pure . VFrame) (fParent frame) <> [field | (True, field) <- zip (fPointers frame) (fFields frame)]

-- * Reports

data RValue = RNull | RHeap Id | RFrame Fid | RStatic Int | RWord Word64 | ROld
  deriving (Eq, Show)

data RWaiter = RWaiter RValue RValue (Maybe RValue)
  deriving (Eq, Show)

data RObject
  = RPlain String [RValue]
  | RMVar (Maybe RValue) [RWaiter] [RWaiter] [RWaiter]
  | RThread String [RValue]
  deriving (Eq, Show)

data RStackFrame = RStackFrame Fid String [RValue]
  deriving (Eq, Show)

data RStatic = RThunk | RInd RValue | RNode [RValue]
  deriving (Eq, Show)

data Report = Report
  { rCollected :: Int,
    rAges :: Map Id Int,
    rObjects :: Map Id RObject,
    rRunning :: RValue,
    rQueue :: [RValue],
    -- | Each live stack: its thread, its top, and its frames from the top.
    rStacks :: [(RValue, RValue, [RStackFrame])],
    rGlobals :: [RValue],
    rRoots :: [RValue],
    rStable :: [RValue],
    rBlackholes :: Map Id [(RValue, RValue)],
    rStatics :: Map Int RStatic,
    rViolations :: [String],
    -- | A gen2 cycle took its snapshot at this collection.
    rCycleStart :: Bool,
    -- | A gen2 cycle ended at this collection.
    rFinish :: Bool
  }
  deriving (Show)

emptyReport :: Report
emptyReport = Report 0 Map.empty Map.empty RNull [] [] [] [] [] Map.empty Map.empty [] False False

parseRValue :: String -> Either String RValue
parseRValue token = case token of
  "n" -> Right RNull
  'h' : rest -> RHeap <$> readNumber rest
  'f' : rest -> RFrame <$> readNumber rest
  's' : rest -> RStatic <$> readNumber rest
  'w' : rest -> RWord <$> readHexWord rest
  "o" -> Right ROld
  _ -> Left ("invalid report value " <> token)

readNumber :: String -> Either String Int
readNumber text = case reads text of
  [(value, "")] -> Right value
  _ -> Left ("invalid number " <> text)

readHexWord :: String -> Either String Word64
readHexWord text = case readHex text of
  [(value, "")] -> Right value
  _ -> Left ("invalid hex word " <> text)

-- | Parse a waiter list: a count and then the entries.
parseWaiters :: Bool -> [String] -> Either String ([RWaiter], [String])
parseWaiters withValue (count : rest) = do
  n <- readNumber count
  go n rest
  where
    go 0 remaining = Right ([], remaining)
    go n remaining = case splitAt (if withValue then 3 else 2) remaining of
      (entry, remaining')
        | length entry == (if withValue then 3 else 2) -> do
            values <- traverse parseRValue entry
            (more, final) <- go (n - 1 :: Int) remaining'
            let waiter = case values of
                  [thread, frame] -> RWaiter thread frame Nothing
                  [thread, frame, value] -> RWaiter thread frame (Just value)
                  _ -> error "waiter width"
            pure (waiter : more, final)
      _ -> Left "truncated waiter list"
parseWaiters _ [] = Left "missing waiter count"

-- | Parse the driver's output into reports keyed by command index.
parseReports :: [String] -> Either String (Map Int Report)
parseReports = go Map.empty
  where
    go reports [] = Right reports
    go reports (line : rest) = case words line of
      ["collection", index, generation] -> do
        command <- readNumber index
        collected <- readNumber generation
        (report, remaining) <- block emptyReport {rCollected = collected} rest
        go (Map.insert command report reports) remaining
      _ -> Left ("unexpected driver line " <> line)
    block _ [] = Left "report without end"
    block report (line : rest) = case words line of
      ["endcollection"] -> Right (report {rStacks = reverse (rStacks report)}, rest)
      ["cycle", "start"] -> block report {rCycleStart = True} rest
      ["finish"] -> block report {rFinish = True} rest
      ["age", identity, generation] -> do
        key <- readNumber identity
        age <- readNumber generation
        block report {rAges = Map.insert key age (rAges report)} rest
      "obj" : identity : "mvar" : state -> do
        key <- readNumber identity
        (value, afterValue) <- case state of
          "full" : v : "readers" : more -> (\parsed -> (Just parsed, more)) <$> parseRValue v
          "empty" : "readers" : more -> Right (Nothing, more)
          _ -> Left ("invalid mvar line " <> line)
        (readers, afterReaders) <- parseWaiters False afterValue
        afterTakersWord <- expectWord "takers" afterReaders
        (takers, afterTakers) <- parseWaiters False afterTakersWord
        afterPuttersWord <- expectWord "putters" afterTakers
        (putters, final) <- parseWaiters True afterPuttersWord
        unless (null final) (Left ("trailing mvar tokens in " <> line))
        block report {rObjects = Map.insert key (RMVar value readers takers putters) (rObjects report)} rest
      "obj" : identity : "thread" : kind : values -> do
        key <- readNumber identity
        parsed <- traverse parseRValue values
        block report {rObjects = Map.insert key (RThread kind parsed) (rObjects report)} rest
      "obj" : identity : kind : _count : values -> do
        key <- readNumber identity
        parsed <- traverse parseRValue values
        block report {rObjects = Map.insert key (RPlain kind parsed) (rObjects report)} rest
      ["running", value] -> do
        parsed <- parseRValue value
        block report {rRunning = parsed} rest
      "queue" : values -> do
        parsed <- traverse parseRValue values
        block report {rQueue = parsed} rest
      ["stack", thread, "top", top] -> do
        parsedThread <- parseRValue thread
        parsedTop <- parseRValue top
        block report {rStacks = (parsedThread, parsedTop, []) : rStacks report} rest
      "frame" : identity : kind : _count : values -> do
        key <- readNumber identity
        parsed <- traverse parseRValue values
        case rStacks report of
          (thread, top, frames) : others -> block report {rStacks = (thread, top, frames <> [RStackFrame key kind parsed]) : others} rest
          [] -> Left "frame line before a stack line"
      ["global", _, value] -> do
        parsed <- parseRValue value
        block report {rGlobals = rGlobals report <> [parsed]} rest
      ["root", _, value] -> do
        parsed <- parseRValue value
        block report {rRoots = rRoots report <> [parsed]} rest
      ["stable", value] -> do
        parsed <- parseRValue value
        block report {rStable = rStable report <> [parsed]} rest
      "blackhole" : identity : _count : values -> do
        key <- readNumber identity
        parsed <- traverse parseRValue values
        let pairs [] = Right []
            pairs (thread : frame : more) = ((thread, frame) :) <$> pairs more
            pairs _ = Left "odd blackhole waiter list"
        waiters <- pairs parsed
        block report {rBlackholes = Map.insert key waiters (rBlackholes report)} rest
      ["static", slot, "thunk"] -> insertStatic slot RThunk
      ["static", slot, "ind", value] -> parseRValue value >>= insertStatic slot . RInd
      "static" : slot : "node" : values -> traverse parseRValue values >>= insertStatic slot . RNode
      "violation" : message -> block report {rViolations = rViolations report <> [unwords message]} rest
      _ -> Left ("unexpected report line " <> line)
      where
        insertStatic slot state = do
          key <- readNumber slot
          block report {rStatics = Map.insert key state (rStatics report)} rest
        expectWord expected (token : more) | token == expected = Right more
        expectWord expected _ = Left ("expected " <> expected <> " in " <> line)

-- * Replay

-- | Run the script against the model and check every reported collection.
replay :: [Command] -> Map Int Report -> [String]
replay script reports = go (zip [0 ..] script) emptyModel <> extra
  where
    extra = ["report for command " <> show index <> " which is not in the script" | index <- Map.keys reports, index >= length script]
    go [] _ = []
    go ((index, command) : rest) model = case Map.lookup index reports of
      Nothing
        | mustCollect command -> ("command " <> show index <> ": the command did not report a collection") : go rest (step model)
        | otherwise -> go rest (step model)
      Just report
        | collects command ->
            let -- A full collection and a cycle free the values of the
                -- static thunks no code reaches, which the command marks
                -- in the model before the check.
                checked = step model
                problems = generationProblems command report <> checkReport checked report
                -- The report says where each survivor lives. The model
                -- takes the ages for the next report, and the snapshot of
                -- a new cycle.
                live = snapshotLiveness checked
                reported = Map.keysSet (rAges report)
                deadNow = Set.fromList [identity | (identity, age) <- Map.toList (rAges report), age == 2, not (Set.member identity (liveHeap live)), not (Set.member identity (liveInds live))]
                snapshot
                  | rCycleStart report = Just deadNow
                  | rFinish report = Nothing
                  | otherwise = mSnapshotDead model
                -- A name the driver dropped stays dropped: its referent
                -- was unmarked at the end of a cycle.
                settledStable = zipWith (\value actual -> if actual == RNull then VNull else value) (mStable checked) (rStable report)
                settled = checked {mAges = Map.restrictKeys (rAges report) reported, mSnapshotDead = snapshot, mStable = settledStable}
             in map (\p -> "command " <> show index <> ": " <> p) problems <> go rest settled
        | otherwise -> ("command " <> show index <> ": collection at a command that cannot collect") : go rest (step model)
      where
        step before =
          let after = applyCommand command before
           in if mFailed after && not (mFailed before) then error ("command " <> show index <> " fails its precondition: " <> renderCommand command) else after
    collects (CReserve _) = True
    collects command = mustCollect command
    mustCollect CCollect = True
    mustCollect (CCollectGeneration _) = True
    mustCollect CCycle = True
    mustCollect _ = False
    generationProblems command report = case command of
      CCollectGeneration generation
        | rCollected report /= generation -> ["collected generation " <> show (rCollected report) <> " instead of " <> show generation]
      CCollectGeneration _ -> []
      CCycle
        | rCollected report /= 1 -> ["cycle collected generation " <> show (rCollected report) <> " instead of 1"]
      CCycle -> []
      _
        | rCollected report /= 0 -> ["a reservation collected generation " <> show (rCollected report)]
        | otherwise -> []

matchesValue :: Value -> RValue -> Bool
matchesValue VNull RNull = True
matchesValue (VHeap expected) (RHeap actual) = expected == actual
matchesValue (VFrame expected) (RFrame actual) = expected == actual
matchesValue (VStatic expected) (RStatic actual) = expected == actual
matchesValue (VWord expected) (RWord actual) = expected == actual
matchesValue (VDecoy _) (RWord _) = True
matchesValue _ _ = False

checkValues :: String -> [Value] -> [RValue] -> [String]
checkValues what expected actual
  | length expected /= length actual = [what <> ": expected " <> show expected <> " but the driver reported " <> show actual]
  | and (zipWith matchesValue expected actual) = []
  | otherwise = [what <> ": expected " <> show expected <> " but the driver reported " <> show actual]

-- | Compare one reported collection with the model at the collection.
checkReport :: Model -> Report -> [String]
checkReport model report =
  map ("violation: " <>) (rViolations report)
    <> ageProblems
    <> objectProblems
    <> frameProblems
    <> schedulerProblems
    <> checkValues "globals" (map deep (mGlobals model)) (rGlobals report)
    <> checkValues "roots" (map deep (mRoots model)) (rRoots report)
    <> stableProblems
    <> blackholeProblems
    <> staticProblems
    <> snapshotProblems
  where
    live = liveness model
    collected = rCollected report
    full = collected == 2
    -- The driver follows every indirection when it reports a value.
    deep = resolve model
    -- An object of a collected generation moves up at least one generation
    -- and at most to gen2. An older object stays where it is.
    ageProblems = concatMap ageProblem (Map.toList (rAges report))
    ageProblem (identity, age) =
      let before = Map.findWithDefault 0 identity (mAges model)
       in if before <= collected
            then ["object " <> show identity <> " moved from generation " <> show before <> " to " <> show age | age < min 2 (before + 1) || age > 2]
            else ["object " <> show identity <> " of generation " <> show before <> " moved to " <> show age | age /= before]
    expectedObject object = case object of
      Object kind pointers fields _ blackholed -> RPlain (if blackholed then "blackhole" else kindName kind) (zipWith (\pointer field -> if pointer then rvalue (deep field) else rvalue field) pointers fields)
      Array elements _ -> RPlain "array" (map (rvalue . deep) elements)
      MVar (MVarState value readers takers putters) -> RMVar (rvalue . deep <$> value) (map rwaiter readers) (map rwaiter takers) (map rwaiter putters)
      Thread (ThreadState resume _) -> case resume of
        RNone -> RThread "none" [RNull, RNull]
        RApply function continuation -> RThread "apply" [rvalue (deep function), RFrame continuation]
        RContinue continuation value -> RThread "continue" ([RFrame continuation, RNull] <> maybe [] (pure . rvalue . deep) value)
        RRaise exception continuation -> RThread "raise" [rvalue (deep exception), RFrame continuation]
      Ind _ -> error "an indirection is not an object"
    rwaiter waiter = RWaiter (RHeap (wThread waiter)) (RFrame (wFrame waiter)) (rvalue . deep <$> wValue waiter)
    rvalue value = case value of
      VNull -> RNull
      VHeap identity -> RHeap identity
      VFrame fid -> RFrame fid
      VStatic slot -> RStatic slot
      VWord word -> RWord word
      VDecoy _ -> ROld
    -- A decoy word may hold any value, so the comparison is by position.
    sameObject expected actual = case (expected, actual) of
      (RPlain kind values, RPlain kind' values') -> kind == kind' && length values == length values' && and (zipWith sameValue values values')
      _ -> expected == actual
    sameValue ROld (RWord _) = True
    sameValue expected actual = expected == actual
    objectProblems =
      ["object " <> show identity <> " survived a full collection but is not live in the model" | full, identity <- Map.keys (rObjects report), not (Set.member identity (liveHeap live))]
        <> ["object " <> show identity <> " survived a full collection but is an indirection the collection follows" | full, identity <- Map.keys (rAges report), Set.member identity (liveInds live)]
        <> concat
          [ case Map.lookup identity (rObjects report) of
              Nothing -> ["object " <> show identity <> " is live in the model but did not survive"]
              Just actual
                | sameObject expected actual -> []
                | otherwise -> ["object " <> show identity <> ": expected " <> show expected <> " but the driver reported " <> show actual]
          | identity <- Set.toList (liveHeap live),
            let expected = expectedObject (mHeap model Map.! identity)
          ]
    -- Every live thread with a frame has a stack in the report, and the
    -- frames of the stack are the frames of the model from the top down.
    liveThreads = [(identity, thread) | identity <- Set.toList (liveHeap live), Just (Thread thread) <- [Map.lookup identity (mHeap model)]]
    frameProblems =
      concat
        [ case [(top, frames) | (RHeap owner, top, frames) <- rStacks report, owner == identity] of
            [] -> ["thread " <> show identity <> " is live in the model but has no stack in the report"]
            [(top, frames)] -> checkStack identity (thTop thread) top frames
            _ -> ["thread " <> show identity <> " has several stacks in the report"]
        | (identity, thread) <- liveThreads
        ]
        <> ["stack of thread " <> show thread <> " survived a full collection but the thread is not live" | full, (RHeap thread, _, _) <- rStacks report, not (Set.member thread (liveHeap live))]
    checkStack identity top actualTop frames =
      let chain Nothing = []
          chain (Just fid) = fid : chain (fParent =<< Map.lookup fid (mFrames model))
          expectedChain = chain top
       in checkValues ("top of thread " <> show identity) [maybe VNull VFrame top] [actualTop]
            <> (if length expectedChain /= length frames then ["thread " <> show identity <> ": expected frames " <> show expectedChain <> " but the driver reported " <> show [fid | RStackFrame fid _ _ <- frames]] else concat (zipWith (checkFrame identity) expectedChain frames))
    checkFrame identity fid (RStackFrame actualFid actualKind actualFields) =
      let frame = mFrames model Map.! fid
          -- A stop frame has no parent field.
          expectedFields = [maybe VNull VFrame (fParent frame) | fKind frame /= FStop] <> zipWith (\pointer field -> if pointer then deep field else field) (fPointers frame) (fFields frame)
       in ["thread " <> show identity <> ": expected frame " <> show fid <> " but the driver reported " <> show actualFid | fid /= actualFid]
            <> ["frame " <> show fid <> ": expected kind " <> frameKindName (fKind frame) <> " but the driver reported " <> actualKind | frameKindName (fKind frame) /= actualKind]
            <> checkValues ("frame " <> show fid <> " fields") expectedFields actualFields
    schedulerProblems =
      checkValues "running thread" [VHeap (mRunning model)] [rRunning report]
        <> checkValues "run queue" (map VHeap (mQueue model)) (rQueue report)
    -- A name whose referent is dead in the model is weak either way: the
    -- model says null, and the driver may still hold the object while it
    -- floats.
    stableProblems
      | length (mStable model) /= length (rStable report) = ["stable names: expected " <> show (mStable model) <> " but the driver reported " <> show (rStable report)]
      | and (zipWith stableMatches (mStable model) (rStable report)) = []
      | otherwise = ["stable names: expected " <> show (mStable model) <> " but the driver reported " <> show (rStable report)]
    -- A name on a static object is dropped by a full collection or the
    -- end of a cycle when no code reached the object.
    stableMatches value actual = case deep value of
      VHeap identity | Set.member identity (liveHeap live) -> actual == RHeap identity
      VHeap _ | full -> actual == RNull
      VHeap identity -> actual == RNull || actual == RHeap identity
      VStatic slot | full || rFinish report -> actual == RNull || actual == RStatic slot
      other -> matchesValue other actual
    blackholeProblems =
      concat
        [ case Map.lookup thunk (rBlackholes report) of
            Nothing -> ["thunk " <> show thunk <> " has waiters in the model but not in the report"]
            Just actual -> checkValues ("waiters of thunk " <> show thunk) (concat [[VHeap (wThread waiter), VFrame (wFrame waiter)] | waiter <- waiters]) (concat [[thread, frame] | (thread, frame) <- actual])
        | (thunk, waiters) <- Map.toList (mBlackholes model)
        ]
        <> ["thunk " <> show thunk <> " has waiters in the report but not in the model" | thunk <- Map.keys (rBlackholes report), not (Map.member thunk (mBlackholes model))]
    staticProblems = concatMap thunkProblem (zip [0 ..] (mStaticThunks model)) <> concatMap nodeProblem (zip [0 ..] (mStaticNodes model))
    thunkProblem (slot, state) = case (state, Map.lookup slot (rStatics report)) of
      (_, Nothing) -> ["static " <> show slot <> " is missing from the report"]
      (SStale, _) -> []
      (SThunk, Just RThunk) -> []
      (SInd value, Just (RInd actual)) -> checkValues ("static " <> show slot) [deep value] [actual]
      (_, Just actual) -> ["static " <> show slot <> ": expected " <> show state <> " but the driver reported " <> show actual]
    nodeProblem (node, fields) = case Map.lookup (staticThunkCount + node) (rStatics report) of
      Just (RNode actual) -> checkValues ("static node " <> show node) fields actual
      other -> ["static node " <> show node <> ": expected fields but the driver reported " <> show other]
    -- The end of a cycle frees the gen2 objects that were dead at its
    -- snapshot. A full collection gives the cycle up and frees everything
    -- dead, which the object check covers.
    snapshotProblems = case mSnapshotDead model of
      Just dead | rFinish report && not full -> ["object " <> show identity <> " was dead at the snapshot of the cycle but survived its end" | identity <- Set.toList dead, Map.member identity (rAges report)]
      _ -> []

-- * Generation

-- | Knobs that shape one script. Each script draws its own profile, so the
-- suite covers sparse and dense heaps, pointer-heavy and word-heavy objects,
-- deep and flat stacks, one thread and many, and frequent and rare
-- collections.
data Profile = Profile
  { pBlockMax :: Int,
    pFieldMax :: Int,
    pArrayMax :: Int,
    pPointerPercent :: Int,
    pNullPercent :: Int,
    pStaticPercent :: Int,
    pDecoyPercent :: Int,
    pArrayWeight :: Int,
    pThunkWeight :: Int,
    pMVarWeight :: Int,
    pOpsMax :: Int,
    pCollectPercent :: Int,
    pEpochMax :: Int,
    pFillPercent :: Int,
    -- | How often a root slot is pointed at a fresh object of the block.
    pRootPercent :: Int,
    -- | The weight of the stack operations among the operations.
    pStackWeight :: Int,
    -- | The weight of the thread operations among the operations.
    pThreadWeight :: Int,
    -- | The largest number of payload words of a pushed frame.
    pFrameMax :: Int,
    -- | The weight of a descent of several frames across chunks.
    pDescentWeight :: Int
  }

-- | A profile with every knob drawn at random.
genRandomProfile :: Gen Profile
genRandomProfile =
  Profile
    <$> Gen.element [1, 2, 4, 8, 16, 32, 64, 128]
    <*> Gen.element [0, 1, 2, 3, 6]
    <*> Gen.element [0, 1, 2, 4, 8, 32]
    <*> Gen.element [0, 25, 50, 75, 100]
    <*> Gen.element [0, 10, 30, 60]
    <*> Gen.element [0, 10, 30]
    <*> Gen.element [0, 20]
    <*> Gen.element [0, 1, 3]
    <*> Gen.element [0, 1, 3]
    <*> Gen.element [0, 1, 2]
    <*> Gen.element [0, 4, 8, 16, 32]
    <*> Gen.element [0, 30, 60, 100]
    <*> Gen.element [1, 2, 4, 6]
    <*> Gen.element [0, 30, 100]
    <*> Gen.element [0, 30, 70]
    <*> Gen.element [0, 1, 3, 6]
    <*> Gen.element [0, 1, 3, 6]
    <*> Gen.element [0, 2, 8, 200]
    <*> Gen.element [0, 1, 3]

-- | The named profiles of the focused tests. Each one starves nothing but
-- weights one corner of the collector: deep stacks, many threads, the
-- static objects, or the gen2 cycle. The general property draws one of
-- them in a quarter of its cases.
focusedProfiles :: [(String, Profile)]
focusedProfiles =
  [ ("deep stacks", base {pStackWeight = 12, pFrameMax = 200, pDescentWeight = 6, pOpsMax = 32, pThreadWeight = 1, pCollectPercent = 100}),
    ("threads", base {pThreadWeight = 12, pMVarWeight = 3, pStackWeight = 4, pThunkWeight = 3, pOpsMax = 32}),
    ("statics", base {pStaticPercent = 60, pThunkWeight = 3, pOpsMax = 16}),
    ("cycles", base {pCollectPercent = 100, pOpsMax = 16, pStackWeight = 4, pThreadWeight = 2, pEpochMax = 6})
  ]
  where
    base =
      Profile
        { pBlockMax = 8,
          pFieldMax = 2,
          pArrayMax = 4,
          pPointerPercent = 75,
          pNullPercent = 10,
          pStaticPercent = 10,
          pDecoyPercent = 0,
          pArrayWeight = 1,
          pThunkWeight = 1,
          pMVarWeight = 1,
          pOpsMax = 16,
          pCollectPercent = 60,
          pEpochMax = 4,
          pFillPercent = 30,
          pRootPercent = 30,
          pStackWeight = 3,
          pThreadWeight = 3,
          pFrameMax = 8,
          pDescentWeight = 1
        }

genProfile :: Gen Profile
genProfile = Gen.frequency ((3, genRandomProfile) : [(1, pure profile) | (_, profile) <- focusedProfiles])

percent :: Int -> Gen Bool
percent p = (< p) <$> Gen.int (Range.constant 0 99)

-- | A weighted choice that drops alternatives with weight zero. Hedgehog's
-- 'Gen.frequency' can select a zero-weight alternative while it shrinks.
weighted :: [(Int, Gen a)] -> Gen a
weighted alternatives = Gen.frequency [(weight, gen) | (weight, gen) <- alternatives, weight > 0]

elementOr :: a -> [a] -> Gen a
elementOr fallback [] = pure fallback
elementOr _ list = Gen.element list

genScript :: Gen [Command]
genScript = do
  profile <- genProfile
  globals <- Gen.int (Range.linear 0 8)
  roots <- Gen.int (Range.linear 0 8)
  spaceWords <- Gen.element [16, 32, 64, 256, 1024, 8192]
  srtCount <- Gen.int (Range.linear 0 4)
  srts <- forM [0 .. srtCount - 1] $ \index ->
    CSrt index
      <$> Gen.list (Range.linear 0 3) (Gen.int (Range.constant 0 (staticRootedCount - 1)))
      <*> Gen.list (Range.linear 0 3) (Gen.int (Range.constant 0 (srtCount - 1)))
  staticSrts <- forM [0 .. staticRootedCount - 1] $ \slot -> CSSrt slot <$> genSrt srtCount
  current <- CCurrentSrt <$> genSrt srtCount
  -- A small mark slice spreads a gen2 cycle over several collections.
  slice <- Gen.element [0, 64, 256, 4096]
  let setup = [CMachine globals roots (8 * spaceWords) slice] <> srts <> staticSrts <> [current, CPush 1 FSStop]
  epochCount <- Gen.int (Range.constant 1 (pEpochMax profile))
  epochs <- genEpochs profile (applyCommands setup emptyModel) epochCount
  pure (setup <> epochs)

genSrt :: Int -> Gen (Maybe Int)
genSrt 0 = pure Nothing
genSrt count = weighted [(1, pure Nothing), (2, Just <$> Gen.int (Range.constant 0 (count - 1)))]

genEpochs :: Profile -> Model -> Int -> Gen [Command]
genEpochs _ _ 0 = pure []
genEpochs profile model count = do
  (commands, next) <- genEpoch profile model
  rest <- genEpochs profile next (count - 1)
  pure (commands <> rest)

data Shape = ShapeObject Kind [Bool] | ShapeArray Int | ShapeMVar

-- | The smallest element count of a large array: with its two header words
-- it reaches the large object bound of 32 KiB less the pinned block header.
largeArrayElements :: Int
largeArrayElements = 4094

isLargeShape :: Shape -> Bool
isLargeShape (ShapeArray count) = count >= largeArrayElements
isLargeShape _ = False

shapeWords :: Shape -> Int
shapeWords (ShapeObject kind pointers)
  | kind == KThunk = 1 + max 1 (length pointers)
  | kind == KPartial = 2 + length pointers
  | otherwise = 1 + length pointers
shapeWords (ShapeArray count) = 2 + count
shapeWords ShapeMVar = recordWords

genShape :: Profile -> Gen Shape
genShape profile =
  weighted
    [ (3, object KNode),
      (1, object KClosure),
      (pThunkWeight profile + 1, object KThunk),
      (1, object KPartial),
      (pArrayWeight profile, ShapeArray <$> arrayLength),
      (pMVarWeight profile, pure ShapeMVar)
    ]
  where
    -- One array in thirty-two is large: it gets regions of its own, a
    -- card for each run of elements, and the ages of a pinned block.
    arrayLength = Gen.frequency [(31, Gen.int (Range.linear 0 (pArrayMax profile))), (1, Gen.int (Range.constant largeArrayElements (largeArrayElements + 300)))]
    object kind = do
      count <- Gen.int (Range.linear 0 (pFieldMax profile))
      ShapeObject kind <$> replicateM count (percent (pPointerPercent profile))

-- | The objects and static slots that later commands may name. Heap objects
-- are the live set, so no command resurrects garbage. Static slots exclude
-- stale slots, because the collector reads through the invalid target of
-- any static object that becomes live again.
data Pool = Pool
  { poolHeap :: [Id],
    poolStatics :: [Int],
    poolSrts :: [Int]
  }

-- | The pool of the current model state.
poolOf :: Model -> Pool
poolOf model =
  let live = liveness model
      stale = taintedStatics model
      usableSrts = [index | index <- Map.keys (mSrts model), Set.null (Set.intersection stale (srtClosure model index))]
      values = [identity | identity <- Set.toList (liveHeap live), isValue (mHeap model Map.! identity)]
      isValue Thread {} = False
      isValue _ = True
   in Pool values [slot | slot <- [0 .. staticCount - 1], not (Set.member slot stale)] usableSrts

genEpoch :: Profile -> Model -> Gen ([Command], Model)
genEpoch profile start = do
  let pool0 = poolOf start
  blockCount <- Gen.int (Range.constant 0 (pBlockMax profile))
  let identities = [mNextId start .. mNextId start + blockCount - 1]
  shapes <- replicateM blockCount (genShape profile)
  srts <- replicateM blockCount (elementOr Nothing (Nothing : map Just (poolSrts pool0)))
  -- The runtime gives a large array regions outside the nursery without a
  -- collection, so the reservation of the block covers the small objects
  -- alone.
  let newCommands = zipWith3 newCommand identities shapes srts
      blockWords = sum [shapeWords shape | shape <- shapes, not (isLargeShape shape)]
      afterBlock = applyCommands newCommands start
      pool = poolOf afterBlock
  fill <- do
    wanted <- percent (pFillPercent profile)
    if wanted
      then do
        keep <- Gen.choice [pure blockWords, pure (blockWords + 1), pure (max 0 (blockWords - 1)), Gen.int (Range.linear 0 64)]
        pure [CFill keep]
      else pure []
  initial <- concat <$> forM (zip identities shapes) (genInitial profile pool)
  rooting <-
    if null identities
      then pure []
      else
        concat
          <$> forM
            ([CGlobal index | index <- [0 .. length (mGlobals start) - 1]] <> [CRoot index | index <- [0 .. length (mRoots start) - 1]])
            ( \slot -> do
                wanted <- percent (pRootPercent profile)
                if wanted then (\identity -> [slot (VHeap identity)]) <$> Gen.element identities else pure []
            )
  let afterInitial = applyCommands (initial <> rooting) afterBlock
  opCount <- Gen.int (Range.constant 0 (pOpsMax profile))
  (ops, afterOps) <- genOps profile afterInitial opCount
  collect <- percent (pCollectPercent profile)
  -- A collection the script asks for names the oldest generation to copy,
  -- leaves the choice to a reservation that does not fit, or starts a gen2
  -- cycle after a gen1 collection.
  collectCommand <- Gen.frequency [(1, pure CCollect), (3, CCollectGeneration <$> Gen.int (Range.constant 0 2)), (2, pure CCycle)]
  let final = if collect then applyCommand collectCommand afterOps else afterOps
      opWords = sum (map commandWords ops)
  pure (fill <> [CReserve (blockWords + opWords)] <> newCommands <> initial <> rooting <> ops <> [collectCommand | collect], final)
  where
    newCommand identity (ShapeObject kind pointers) srt = CNew identity kind pointers srt
    newCommand identity (ShapeArray count) srt = CArray identity count srt
    newCommand identity ShapeMVar _ = CMVar identity

-- | The words an operation can take from the reservation of its epoch.
commandWords :: Command -> Int
commandWords command = case command of
  CStable _ -> stableNameWords
  CFork {} -> recordWords
  CBlock _ -> blackholeWaiterWords
  CTake _ -> mvarWaiterWords
  CRead _ -> mvarWaiterWords
  CPut {} -> mvarWaiterWords
  _ -> 0

-- | The static slots that lead to a stale slot: the stale slots themselves,
-- static nodes with a field that names one, evaluated static thunks whose
-- target names one, and static objects whose reference table names one. The
-- collector reads the invalid target of a stale slot when any of them becomes
-- live, so no script names them again.
taintedStatics :: Model -> Set Int
taintedStatics model = grow (Set.fromList [slot | (slot, SStale) <- zip [0 ..] (mStaticThunks model)])
  where
    grow tainted =
      let next = Set.union tainted (Set.fromList [slot | slot <- [0 .. staticRootedCount - 1], leads tainted slot])
       in if next == tainted then tainted else grow next
    leads tainted slot = any (`Set.member` tainted) (edges slot)
    edges slot
      | slot < staticThunkCount = case mStaticThunks model !! slot of
          SThunk -> srtStatics (mStaticThunkSrts model !! slot)
          SInd (VStatic target) -> [target]
          _ -> []
      | otherwise =
          let node = slot - staticThunkCount
           in [target | VStatic target <- mStaticNodes model !! node] <> srtStatics (mStaticNodeSrts model !! node)
    srtStatics = maybe [] (Set.toList . srtClosure model)

srtClosure :: Model -> Int -> Set Int
srtClosure model = go Set.empty Set.empty . pure
  where
    go _ statics [] = statics
    go seen statics (index : rest)
      | Set.member index seen = go seen statics rest
      | otherwise =
          let (objects, children) = fromMaybe ([], []) (Map.lookup index (mSrts model))
           in go (Set.insert index seen) (Set.union statics (Set.fromList objects)) (children <> rest)

genPointer :: Profile -> Pool -> Gen Value
genPointer profile pool =
  weighted
    [ (max 1 (pNullPercent profile), pure VNull),
      (pStaticPercent profile, VStatic <$> elementOr 0 (poolStatics pool)),
      (if null (poolHeap pool) then 0 else 100, VHeap <$> Gen.element (poolHeap pool))
    ]

-- | A pointer to an object, for slots that must not hold null.
genTarget :: Profile -> Pool -> (Value -> Bool) -> Gen (Maybe Value)
genTarget profile pool allowed = do
  let heapChoices = [VHeap identity | identity <- poolHeap pool, allowed (VHeap identity)]
      staticChoices = [VStatic slot | slot <- poolStatics pool, allowed (VStatic slot)]
  if null heapChoices && null staticChoices
    then pure Nothing
    else
      Just
        <$> weighted
          [ (if null heapChoices then 0 else 100, Gen.element heapChoices),
            (if null staticChoices then 0 else max 1 (pStaticPercent profile), Gen.element staticChoices)
          ]

genWord :: Profile -> Pool -> Gen Value
genWord profile pool = do
  decoy <- percent (pDecoyPercent profile)
  if decoy && not (null (poolHeap pool))
    then VDecoy <$> Gen.element (poolHeap pool)
    else
      VWord
        <$> Gen.choice
          [ pure 0,
            fromIntegral <$> Gen.int (Range.linear 0 1000),
            Gen.word64 Range.constantBounded,
            -- A word that looks like a heap address: aligned and in a typical
            -- allocation range on both supported platforms.
            (\high low -> (high `shiftL` 32) .|. (low `shiftL` 3)) <$> Gen.element [0x1, 0x6, 0x7f, 0x5555] <*> Gen.word64 (Range.constant 0 0x1fffffff)
          ]

genInitial :: Profile -> Pool -> (Id, Shape) -> Gen [Command]
genInitial profile pool (identity, shape) = case shape of
  ShapeObject _ pointers -> concat <$> forM (zip [0 ..] pointers) field
  ShapeArray count -> concat <$> forM (arrayIndices count) element
  ShapeMVar -> pure []
  where
    -- A large array gets a store into a sample of its cards, not into each
    -- element: the operations of the epoch reach the other elements.
    arrayIndices count
      | count <= 64 = [0 .. count - 1]
      | otherwise = [0, 97 .. count - 1] <> [count - 1]
    field (index, True) = element index
    field (index, False) = do
      value <- genWord profile pool
      pure [CSet identity index value | value /= VWord 0]
    element index = do
      value <- genPointer profile pool
      pure [CSet identity index value | value /= VNull]

genOps :: Profile -> Model -> Int -> Gen ([Command], Model)
genOps _ model 0 = pure ([], model)
genOps profile model count = do
  commands <- genOp profile model
  let next = applyCommands commands model
  (rest, final) <- genOps profile next (count - 1)
  pure (commands <> rest, final)

-- | The frames of the running thread from its top down.
runningChain :: Model -> [(Fid, Frame)]
runningChain model = go (runningTop model)
  where
    go Nothing = []
    go (Just fid) = case Map.lookup fid (mFrames model) of
      Just frame -> (fid, frame) : go (fParent frame)
      Nothing -> []

-- | One change to the heap, the stacks, or the threads. Only mutable
-- objects change: arrays, thunks through their update frames, static
-- thunks, static nodes, and root slots.
genOp :: Profile -> Model -> Gen [Command]
genOp profile model = do
  let choices = [(weight, gen) | (weight, Just gen) <- options, weight > 0]
  commands <- if null choices then pure [] else weighted choices
  -- The preconditions above cover the common cases. The model decides the
  -- rest: an operation the runtime would refuse is dropped.
  pure (if mFailed (applyCommands commands model) then [] else commands)
  where
    pool = poolOf model
    object identity = Map.lookup identity (mHeap model)
    chain = runningChain model
    topKind = case chain of
      (_, frame) : _ -> Just (fKind frame)
      [] -> Nothing
    -- The frame a continue with no value enters: it must not be an update
    -- frame, which needs the result of its thunk.
    entersPlain = case runningTop model >>= (`enteredFrame` model) of
      Just entered -> case fKind <$> Map.lookup entered (mFrames model) of
        Just FUpdate -> False
        Just FStop -> runnable
        Just _ -> True
        Nothing -> False
      Nothing -> False
    plainThunks = [identity | identity <- poolHeap pool, Just (Object KThunk _ _ _ False) <- [object identity]]
    blackholed = [identity | identity <- poolHeap pool, not (evaluates (mRunning model) identity model), Just (Object KThunk _ _ _ True) <- [object identity]]
    arrays = [(identity, length elements) | identity <- poolHeap pool, Just (Array elements _) <- [object identity], not (null elements)]
    mvars = [identity | identity <- poolHeap pool, Just MVar {} <- [object identity]]
    blockedMVars = [identity | identity <- poolHeap pool, Just (MVar (MVarState Nothing readers takers _)) <- [object identity], not (null readers && null takers)]
    -- A name on an object whose value another name resolves to would be
    -- the same name in the runtime, so the generator names each value once.
    unnamed = [identity | identity <- poolHeap pool, VHeap identity `notElem` map (resolve model) (mStable model)]
    staticThunks = [slot | (slot, SThunk) <- zip [0 ..] (mStaticThunks model), slot `elem` poolStatics pool]
    staticNodes = [slot | slot <- [staticThunkCount .. staticRootedCount - 1], slot `elem` poolStatics pool]
    runnable = not (null (mQueue model))
    hasCatch = any ((== FCatch) . fKind . snd) (drop 1 chain) || any ((== FCatch) . fKind . snd) chain
    withTarget build = do
      target <- genTarget profile pool (const True)
      pure (maybe [] (pure . build) target)
    nonEmpty list gen = if null list then Nothing else Just gen
    whenever condition gen = if condition then Just gen else Nothing
    fid = mNextFid model
    nextId = mNextId model
    -- A continue with a value into the top frame: the result of a thunk
    -- for an update frame, a plain value for any other.
    enterTop = case chain of
      (top, _) : _ -> Just $ do
        target <- genTarget profile pool (const True)
        case target of
          Just value -> pure [CEnter top (Just value)]
          Nothing -> pure []
      [] -> Nothing
    options =
      [ ( 2,
          nonEmpty arrays $ do
            (identity, count) <- Gen.element arrays
            index <- Gen.int (Range.constant 0 (count - 1))
            value <- genPointer profile pool
            pure [CSet identity index value]
        ),
        ( 1,
          nonEmpty staticThunks $ do
            slot <- Gen.element staticThunks
            target <- genTarget profile pool (const True)
            pure (maybe [] (pure . CSUpdate slot) target)
        ),
        ( 1,
          nonEmpty staticNodes $ do
            slot <- Gen.element staticNodes
            index <- Gen.int (Range.constant 0 (staticNodeFields - 1))
            value <- weighted [(1, pure VNull), (3, VStatic <$> elementOr 0 (poolStatics pool))]
            pure [CSSet slot index value]
        ),
        ( 1,
          nonEmpty (mGlobals model) $ do
            index <- Gen.int (Range.constant 0 (length (mGlobals model) - 1))
            value <- genPointer profile pool
            pure [CGlobal index value]
        ),
        ( 1,
          nonEmpty (mRoots model) $ do
            index <- Gen.int (Range.constant 0 (length (mRoots model) - 1))
            value <- genPointer profile pool
            pure [CRoot index value]
        ),
        (1, nonEmpty unnamed (CStable . VHeap <$> Gen.element unnamed <&> pure)),
        ( 1,
          Just $ do
            srt <- elementOr Nothing (Nothing : map Just (poolSrts pool))
            pure [CCurrentSrt srt]
        ),
        -- Stack operations.
        ( pStackWeight profile,
          whenever (isJust topKind) $ do
            count <- Gen.int (Range.linear 0 (pFrameMax profile))
            pointers <- replicateM count (percent (pPointerPercent profile))
            values <- forM pointers $ \pointer -> if pointer then genPointer profile pool else genWord profile pool
            srt <- elementOr Nothing (Nothing : map Just (poolSrts pool))
            forward <- percent 20
            pure [CPush fid ((if forward then FSForward else FSNormal) pointers srt values)]
        ),
        ( pDescentWeight profile,
          whenever (isJust topKind) $ do
            depth <- Gen.int (Range.linear 2 12)
            payload <- Gen.int (Range.linear 100 250)
            pure [CPush (fid + offset) (FSNormal (replicate payload False) Nothing (replicate payload (VWord (fromIntegral offset)))) | offset <- [0 .. depth - 1]]
        ),
        -- A churn: a descent of several chunks, then pops below where it
        -- started. The pops cross the chunks a gen2 cycle scanned and
        -- deferred.
        ( pDescentWeight profile,
          whenever (isJust topKind) $ do
            depth <- Gen.int (Range.linear 2 8)
            payload <- Gen.int (Range.linear 100 250)
            extra <- Gen.int (Range.linear 0 (max 0 (length chain - 2)))
            let pushes = [CPush (fid + offset) (FSNormal (replicate payload False) Nothing (replicate payload (VWord (fromIntegral offset)))) | offset <- [0 .. depth - 1]]
                pops = [CEnter (fid + offset) Nothing | offset <- reverse [0 .. depth - 1]] <> [CEnter below Nothing | (below, frame) <- take extra chain, fKind frame /= FStop, fKind frame /= FUpdate]
            pure (pushes <> pops)
        ),
        -- A collection in the middle of an epoch, so a gen2 cycle and its
        -- slices interleave with the operations.
        (max 1 (pCollectPercent profile `div` 20), Just (Gen.frequency [(2, pure [CCollect]), (1, pure [CCollectGeneration 1]), (2, pure [CCycle])])),
        ( pStackWeight profile,
          whenever (isJust topKind) $ do
            srt <- elementOr Nothing (Nothing : map Just (poolSrts pool))
            target <- genTarget profile pool (const True)
            catch <- Gen.bool
            pure (maybe [] (\value -> [CPush fid (if catch then FSCatch srt value else FSPrompt srt value)]) target)
        ),
        ( pStackWeight profile + pThunkWeight profile,
          whenever (isJust topKind && not (null plainThunks)) $ do
            thunk <- Gen.element plainThunks
            pure [CPush fid (FSUpdate thunk)]
        ),
        (pStackWeight profile, whenever (topKind == Just FUpdate) (fromMaybe (pure []) enterTop)),
        ( pStackWeight profile,
          whenever (isJust topKind && topKind /= Just FStop && entersPlain) $ case chain of
            (top, _) : _ -> pure [CEnter top Nothing]
            [] -> pure []
        ),
        (pStackWeight profile, whenever (isJust topKind && topKind /= Just FStop) (fromMaybe (pure []) enterTop)),
        (max 1 (pStackWeight profile `div` 2), whenever hasCatch (withTarget CRaise)),
        -- Thread operations.
        ( pThreadWeight profile,
          whenever (isJust topKind) $ do
            target <- genTarget profile pool (const True)
            pure (maybe [] (\action -> [CFork nextId fid action]) target)
        ),
        (pThreadWeight profile, whenever (isJust topKind && entersPlain) (pure [CYield])),
        (pThreadWeight profile, whenever (isJust topKind && entersPlain && runnable) (pure [CYield])),
        ( pThreadWeight profile,
          whenever (isJust topKind && runnable && not (null blackholed)) $ do
            thunk <- Gen.element blackholed
            pure [CBlock thunk]
        ),
        ( pThreadWeight profile,
          whenever (isJust topKind && not (null mvars)) $ do
            identity <- Gen.element mvars
            target <- genTarget profile pool (const True)
            let full = case object identity of
                  Just (MVar (MVarState (Just _) _ _ _)) -> True
                  _ -> False
            Gen.frequency
              [ (2, pure [CTake identity | full || runnable]),
                (1, pure [CRead identity | full || runnable]),
                ( 2,
                  pure
                    ( case target of
                        Just value | entersPlain && (not full || runnable) -> [CPut identity value]
                        _ -> []
                    )
                )
              ]
        ),
        -- A wake churn: a collection promotes the waiters of an MVar, a
        -- put wakes one, and the woken thread pops its frame and pushes
        -- new ones before the next collection. A dead waiter that still
        -- names the popped frame then meets the collector.
        ( pThreadWeight profile,
          whenever (isJust topKind && not (null blockedMVars)) $ do
            identity <- Gen.element blockedMVars
            target <- genTarget profile pool (const True)
            case target of
              Nothing -> pure []
              Just value ->
                let start = [CCollect, CPut identity value, CYield]
                    after = applyCommands start model
                    woken = case Map.lookup identity (mHeap model) of
                      Just (MVar (MVarState _ readers takers _)) -> map wThread (readers <> takers)
                      _ -> []
                    tail'
                      | mFailed after || mRunning after `notElem` woken = []
                      | otherwise = case runningTop after of
                          Just top
                            | Just entered <- enteredFrame top after,
                              maybe False (\f -> fKind f `elem` [FNormal, FCatch, FPrompt]) (Map.lookup entered (mFrames after)) ->
                                [CEnter top Nothing, CPush (mNextFid after) (FSNormal [] Nothing []), CPush (mNextFid after + 1) (FSNormal [] Nothing []), CCollect]
                          _ -> []
                 in pure (start <> tail')
        ),
        -- The running thread ends when another thread can run.
        ( max 1 (pThreadWeight profile `div` 2),
          whenever (topKind == Just FStop && runnable) $ case chain of
            (top, _) : _ -> pure [CEnter top Nothing]
            [] -> pure []
        )
      ]

-- * Driver

-- | One driver process per property. The process stays alive across cases
-- and restarts after a crash.
data Driver = Driver
  { driverExecutable :: FilePath,
    driverProcess :: MVar (Maybe Process)
  }

data Process = Process
  { processInput :: Handle,
    processOutput :: Handle,
    processError :: Handle,
    processHandle :: ProcessHandle
  }

-- | Compile the driver against a runtime with the verifier. Sanitizers are
-- used when the C compiler supports them and the sanitized driver runs
-- here, so the runtime archive is instrumented for this test rather than
-- taken from the store.
compileDriver :: IO (FilePath, FilePath)
compileDriver = do
  root <- lookupEnv "AIHC_TEST_ROOT" >>= maybe (throwIO (userError "AIHC_TEST_ROOT is not set")) pure
  temporary <- getCanonicalTemporaryDirectory
  directory <- createTempDirectory temporary "aihc-gc-fuzz"
  let source = root </> "bin" </> "aihc" </> "compiler" </> "native" </> "test" </> "gc-fuzz" </> "aihc_gc_fuzz.c"
      executable = directory </> "aihc-gc-fuzz"
      base = ["-std=c11", "-O1", "-g", "-Wall", "-Wextra", "-Werror", "-DAIHC_GC_VERIFY"]
      -- The instrumented and the plain runtime differ in their C arguments,
      -- so each attempt gets its own cached archive.
      buildAndLink extra = do
        attempt <- tryIOError $ do
          build <- cachedRuntimeArchive Llvm (extra <> base)
          -- Link with the driver that built the archive, so the sanitizer
          -- runtime of the driver matches the instrumented runtime objects.
          (compiler, _targetArguments) <- backendCompiler Llvm
          let arguments =
                extra
                  <> base
                  <> concatMap (\include -> ["-I", include]) (runtimeBuildIncludeDirectories build)
                  <> [source, runtimeBuildArchive build, "-lm", "-o", executable]
          readProcessWithExitCode compiler arguments ""
        pure $ case attempt of
          Left err -> Left (show err)
          Right (ExitSuccess, _, _) -> Right ()
          Right (ExitFailure _, _, message) -> Left message
      compilePlain =
        buildAndLink []
          >>= either (\message -> throwIO (userError ("cannot compile the collector fuzz driver:\n" <> message))) pure
  sanitized <- buildAndLink ["-fsanitize=address,undefined", "-fno-sanitize-recover=all"]
  case sanitized of
    Left _ -> compilePlain
    Right () -> do
      usable <- driverAnswers executable
      unless usable compilePlain
  pure (directory, executable)

-- | Whether a freshly compiled driver starts and answers a trivial script.
--
-- Compiling with the sanitizers is not enough to know that they work here.
-- Inside the Nix sandbox on macOS the AddressSanitizer runtime never
-- finishes reserving its shadow memory: it stops in
-- @FindDynamicShadowStart@, so every sanitized binary hangs before @main@,
-- down to a hello world. Without this check the driver answers nothing and
-- each script waits out the time limit in 'runScript' instead. Ask the
-- driver a question it can answer immediately, and fall back to the plain
-- build when it cannot.
driverAnswers :: FilePath -> IO Bool
driverAnswers executable = do
  outcome <- timeout (10 * 1000000) (try (readProcessWithExitCode executable [] "end\n"))
  pure $ case outcome :: Maybe (Either SomeException (ExitCode, String, String)) of
    Just (Right (ExitSuccess, output, _)) -> "done" `elem` lines output
    _ -> False

newDriver :: IO (FilePath, FilePath) -> IO Driver
newDriver getBuild = do
  (_, executable) <- getBuild
  Driver executable <$> newMVar Nothing

stopDriver :: Driver -> IO ()
stopDriver driver = modifyMVar (driverProcess driver) $ \state -> do
  mapM_ killProcess state
  pure (Nothing, ())

startProcess :: Driver -> IO Process
startProcess driver = do
  (Just input, Just output, Just errors, handle) <-
    createProcess (proc (driverExecutable driver) []) {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  hSetBuffering input (BlockBuffering Nothing)
  pure (Process input output errors handle)

killProcess :: Process -> IO String
killProcess process = do
  terminateProcess (processHandle process)
  _ <- try (hClose (processInput process)) :: IO (Either IOException ())
  _ <- waitForProcess (processHandle process)
  errors <- try (hGetContents (processError process) >>= \text -> length text `seq` pure text) :: IO (Either IOException String)
  pure (fromRight "" errors)

-- | Run one script and return the driver's report lines.
runScript :: Driver -> String -> IO (Either String [String])
runScript driver script = modifyMVar (driverProcess driver) $ \state -> do
  process <- maybe (startProcess driver) pure state
  outcome <- try (exchange process) :: IO (Either SomeException (Maybe (Either String [String])))
  case outcome of
    Right (Just (Right output)) -> pure (Just process, Right output)
    Right (Just (Left message)) -> failed process message
    Right Nothing -> failed process "the driver did not answer within the time limit"
    Left exception -> failed process ("the driver stopped: " <> show exception)
  where
    -- The driver reports collections while it still reads the script, so
    -- a long script and its reports can fill both pipes at once. A thread
    -- of its own writes the script while this one reads the reports.
    exchange process = timeout (60 * 1000000) $ do
      written <- newEmptyMVar
      _ <- forkIO $ do
        outcome <- try (hPutStr (processInput process) script >> hPutStr (processInput process) "end\n" >> hFlush (processInput process)) :: IO (Either IOException ())
        putMVar written outcome
      result <- collectLines (processOutput process) []
      _ <- takeMVar written
      pure result
    collectLines output acc = do
      line <- hGetLine output
      case words line of
        ["done"] -> pure (Right (reverse acc))
        ("fail" : message) -> pure (Left ("the driver rejected the script: " <> unwords message))
        _ -> collectLines output (line : acc)
    failed process message = do
      errors <- killProcess process
      let detail = if all isSpace errors then "" else "\ndriver stderr:\n" <> errors
      pure (Nothing, Left (message <> detail))
