{-# LANGUAGE OverloadedStrings #-}

-- | Lower GC-GRIN to Lir.
--
-- Every GRIN function becomes a Lir function with the @aihc@ convention. The
-- first four parameters are the context of the running thread: the heap
-- pointer, the heap limit, the stack pointer, and the stack limit.
-- The stack chunk header gives the capability pointer before each C call.
-- A pointer representation becomes @ptr@, an
-- address becomes @ptr@, and every other scalar becomes @i64@. Floats travel
-- as their bit patterns, like in the native runtime ABI.
--
-- Control transfer is explicit: a CPS transfer is a @tailcall@, a runtime
-- helper is a @call@ of an extern C function, and dynamic entries go through
-- the @backend_entry@ field of an info table. That field has the signature
-- @(ptr, ptr, ptr, ptr, ptr, ptr, T...) -> ()@ with the context, the
-- object, the continuation, and the supplied values. The
-- runtime defines fixed helpers and common argument shapes. Other shapes
-- remain in the module.
--
-- The context lives in values, not in the capability. The capability holds a copy
-- only around a C call that can read or change it. See 'Context'.
module Aihc.Lir.Lower
  ( LowerError (..),
    LowerOptions (..),
    ModuleSettings (..),
    defaultModuleSettings,
    LowerTarget (..),
    HostKind (..),
    UnitKind (..),
    posixTarget64,
    appleArm64Target,
    wasip3Target,
    lowerEntry,
    lowerModule,
    lowerModuleTo,
    lowerProgramWith,

    -- * Building blocks for harnesses
    LowerM,
    Typed (..),
    Context (..),
    contextOperands,
    freshContext,
    capabilityOperand,
    loadContext,
    storeContext,
    ContinuationSpec (..),
    runLower,
    lowerUnitItems,
    continuationInfoItems,
    functionSymbol,
    threadDoneContinuation,
    allocateContinuation,
    constructorInfoSymbol,
    repType,
    beginBlock,
    emit,
    terminate,
    fresh,
    finishFunction,
    requireExtern,
    requireExternData,
    requireHelper,
    Helper (..),
    helperSymbol,
    runtimeCallSignature,
    loadSlot,
    storeSlot,
  )
where

import Aihc.Grin.Analysis (freeExprVars)
import Aihc.Grin.Cps (ContinuationFrameKind (..), continuationFrameKindCode)
import Aihc.Grin.Gc (GcGrinProgram, entryGcProgram, gcContinuationFrames, gcContinuationFunctions, gcFunctionContinuations, gcGrinProgram, nodeWords)
import Aihc.Grin.Primitive (uncheckedByteArrayPrimitive, uncheckedByteArrayWords)
import Aihc.Grin.Srt
import Aihc.Grin.Syntax
import Aihc.Lir.Simplify (eliminateCommonSubexpressions, simplifyBooleanBranches, simplifyMultiplications, simplifyRotations, simplifyWidths)
import Aihc.Lir.Syntax
import Aihc.Native
  ( NativeCpsCall (..),
    NativeCpsTransfer (..),
    NativeRuntimeCall (..),
    buildAddrLiteralPool,
    executableEntryName,
    nativeCpsPrimitiveCall,
    nativeRuntimePrimitiveCall,
    renderLinkedConstructorInfoSymbol,
    renderLinkedFunctionSymbol,
    renderLinkedGlobalSymbol,
    renderLinkedPartialConstructorInfoSymbol,
  )
import Control.Monad (foldM, forM, forM_, unless, when, zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, gets, modify', put, runStateT)
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Foldable (for_)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

data LowerError
  = LowerMissingFunction !FunctionName
  | LowerUnsupportedExpression !Text
  | LowerUnsupportedValue !Text
  | LowerUnsupportedRuntimeRep !GrinRep
  | LowerUnsupportedPrimitive !Text
  | LowerCpsError !Text
  deriving (Eq, Show)

data UnitKind
  = -- | A library module.
    LibraryUnit
  | -- | The executable entry unit with @main@.
    ExecutableUnit
  deriving (Eq, Show)

-- | The host that starts the program and owns the IO loop.
data HostKind
  = -- | A POSIX process. The entry unit defines @main@.
    PosixHost
  | -- | A WASI P3 component. The entry unit exports the start and resume
    -- functions that the P3 driver calls.
    Wasip3Host
  deriving (Eq, Show)

-- | The properties of the target that the lowering depends on. Heap slots
-- are 8 bytes on every target; the word size decides the layout of the
-- info tables, the static reference tables, and the resume records.
data LowerTarget = LowerTarget
  { lowerWordSize :: !Int,
    lowerHost :: !HostKind,
    -- | Apple ARM64 packs narrow C stack arguments without integer promotion.
    lowerPackedCStack :: !Bool
  }
  deriving (Eq, Show)

-- | A 64-bit POSIX target with promoted narrow C arguments.
posixTarget64 :: LowerTarget
posixTarget64 = LowerTarget {lowerWordSize = 8, lowerHost = PosixHost, lowerPackedCStack = False}

appleArm64Target :: LowerTarget
appleArm64Target = posixTarget64 {lowerPackedCStack = True}

-- | The 32-bit WASI P3 target.
wasip3Target :: LowerTarget
wasip3Target = LowerTarget {lowerWordSize = 4, lowerHost = Wasip3Host, lowerPackedCStack = False}

data LowerOptions = LowerOptions
  { lowerUnitKind :: !UnitKind,
    -- | Export every function symbol and every constructor table. Test
    -- harnesses use the symbols.
    lowerExposeFunctions :: !Bool,
    lowerTarget :: !LowerTarget,
    -- | Check the index of every array primitive against the length, as
    -- GHC does under @-fcheck-prim-bounds@. Off, an access is an unchecked
    -- load or store, as in GHC by default.
    lowerCheckPrimBounds :: !Bool,
    -- | Count the heap objects that the code allocates, by info table, for
    -- the allocation profile of the runtime statistics. A library unit
    -- defines the counters and their names, and the executable entry
    -- registers them with the runtime. Only a whole program can ask for
    -- it, because the counters of two units would have the same symbols.
    lowerProfileAllocations :: !Bool,
    -- | Give the capability the code and the continuation of each function
    -- at its entry, so the time profiler of the runtime can sample the
    -- Haskell stack. A library unit defines the name table of its
    -- functions and info tables, and the executable entry registers it with
    -- the runtime. Only a whole program can ask for it, for the same reason
    -- as 'lowerProfileAllocations'.
    lowerProfileTime :: !Bool
  }
  deriving (Eq, Show)

-- | What a build asks of the lowering of a module.
data ModuleSettings = ModuleSettings
  { -- | See 'lowerCheckPrimBounds'.
    moduleCheckPrimBounds :: !Bool,
    -- | See 'lowerProfileAllocations'.
    moduleProfileAllocations :: !Bool,
    -- | See 'lowerProfileTime'.
    moduleProfileTime :: !Bool
  }
  deriving (Eq, Show)

-- | No bounds checks and no profile.
defaultModuleSettings :: ModuleSettings
defaultModuleSettings = ModuleSettings {moduleCheckPrimBounds = False, moduleProfileAllocations = False, moduleProfileTime = False}

libraryOptions :: LowerTarget -> ModuleSettings -> LowerOptions
libraryOptions target settings =
  LowerOptions
    { lowerUnitKind = LibraryUnit,
      lowerExposeFunctions = False,
      lowerTarget = target,
      lowerCheckPrimBounds = moduleCheckPrimBounds settings,
      lowerProfileAllocations = moduleProfileAllocations settings,
      lowerProfileTime = moduleProfileTime settings
    }

-- | Lower one library module with the settings of the build.
lowerModule :: LowerTarget -> ModuleSettings -> GcGrinProgram -> Either LowerError Module
lowerModule target settings =
  lowerProgramWith (libraryOptions target settings)

-- | Lower the fixed executable entry unit. With the flags, @main@ registers
-- the allocation counters and the time profile tables of the program with
-- the runtime.
lowerEntry :: LowerTarget -> Bool -> Bool -> Either LowerError Module
lowerEntry target profileAllocations profileTime = do
  gcProgram <- either (Left . LowerCpsError . T.pack . show) Right entryGcProgram
  lowerProgramWith LowerOptions {lowerUnitKind = ExecutableUnit, lowerExposeFunctions = False, lowerTarget = target, lowerCheckPrimBounds = False, lowerProfileAllocations = profileAllocations, lowerProfileTime = profileTime} gcProgram

lowerProgramWith :: LowerOptions -> GcGrinProgram -> Either LowerError Module
lowerProgramWith options gcProgram =
  Module . snd <$> runLower options gcProgram (lowerProgramItems options gcProgram)

-- | Supply each complete item to the consumer before conversion proceeds.
lowerModuleTo :: (Monad m) => LowerTarget -> ModuleSettings -> (Map Symbol Signature -> Item -> m ()) -> GcGrinProgram -> m (Either LowerError ())
lowerModuleTo target settings output gcProgram =
  consume (initialLowerState options gcProgram) Set.empty (lowerUnitActions env (gcGrinProgram gcProgram))
  where
    options = libraryOptions target settings
    env = lowerEnvironment options gcProgram
    consume state done actions = case actions of
      action : rest -> case runStateT action state of
        Left err -> pure (Left err)
        Right ((), next) -> do
          let signatures = stateSignatures next <> stateExterns next
          mapM_ (output signatures) (reverse (stateItemsRev next))
          consume next {stateItemsRev = []} done rest
      [] ->
        let pending = stateHelpers state `Set.difference` done
         in if Set.null pending
              then mapM_ (output (stateSignatures state <> stateExterns state)) (externItems state) >> pure (Right ())
              else consume state (done <> pending) (map (generateHelper env) (Set.toAscList pending))

lowerProgramItems :: LowerOptions -> GcGrinProgram -> LowerUnit -> LowerM ()
lowerProgramItems options gcProgram env = do
  lowerUnitItems env
  when (lowerUnitKind options == ExecutableUnit) $
    case lowerHost (lowerTarget options) of
      PosixHost -> lowerExecutableMain gcProgram
      Wasip3Host -> lowerWasip3Entry gcProgram

-- Types

-- | The Lir type of one runtime representation.
repType :: GrinRep -> Type
repType runtimeRep =
  case runtimeRep of
    BoxedRep _ -> Ptr
    AddrRep -> Ptr
    _ -> I64

-- | An operand together with its Lir type.
data Typed = Typed
  { typedOperand :: !Operand,
    typedType :: !Type
  }
  deriving (Eq, Show)

-- Environment

-- | Which of the two info tables of one constructor an object uses. A
-- constructor that still wants arguments shares a single info table across
-- every stage and records how much it holds in the object itself, so the
-- number of arguments outstanding is not part of its identity here.
data ConstructorStage
  = SaturatedConstructor
  | PartialConstructor
  deriving (Eq, Ord, Show)

data RuntimeInfoKey
  = ConstructorRuntimeInfo !Text !ConstructorStage
  | ClosureRuntimeInfo !FunctionName ![GrinRep] ![[GrinRep]]
  | ThunkRuntimeInfo !FunctionName ![GrinRep]
  deriving (Eq, Ord, Show)

-- | The stage a node tag names. GRIN counts the arguments a constructor still
-- wants; zero of them means the constructor is finished.
constructorStage :: Int -> ConstructorStage
constructorStage remaining
  | remaining == 0 = SaturatedConstructor
  | otherwise = PartialConstructor

data RuntimeEnter = RuntimeEnter
  { enterTarget :: !Symbol,
    -- | The stored fields start after this many words of the payload: one
    -- for the owner word of a thunk, and zero otherwise.
    enterStoredOffset :: !Int,
    enterStored :: ![Type],
    enterSupplied :: ![Type],
    enterTargetParameters :: ![Type],
    enterPassesContinuation :: !Bool,
    -- | The target loads its fields from the frame.
    enterDirect :: !Bool
  }

data RuntimeInfo = RuntimeInfo
  { infoSymbol :: !Symbol,
    infoLinkage :: !Linkage,
    infoIdentity :: !DataField,
    infoFields :: ![GrinRep],
    -- | The slots of the object once it wants no more arguments: the stored
    -- fields, then each slot that an application can still supply. The
    -- pointer bitmap describes all of them, so the runtime finds the kind of
    -- an arriving slot in the table of the object that takes it, and the
    -- last stage needs no table of its own.
    infoLayout :: ![GrinRep],
    infoRemainingArity :: !Int,
    infoNext :: !(Maybe Symbol),
    infoEnter :: !(Maybe RuntimeEnter),
    infoFrameKind :: !(Maybe ContinuationFrameKind),
    infoObjectKind :: !Int,
    infoSrt :: !(Maybe Symbol),
    -- | The index of a constructor in its data declaration, and zero for
    -- every other object.
    infoConstructorIndex :: !Int
  }

-- | Function bodies are separate from the shared conversion metadata.
data LowerUnit = LowerUnit !LowerEnv !GrinProgram

data LowerEnv = LowerEnv
  { envOptions :: !LowerOptions,
    envFunctionSymbols :: !(Map FunctionName Symbol),
    envFunctionParameters :: !(Map FunctionName [Type]),
    envContinuationFunctions :: !(Set FunctionName),
    -- | The field count of each body that accepts a frame directly.
    envFrameEntries :: !(Map FunctionName Int),
    -- | The continuation parameter of each function that takes one.
    envFunctionContinuations :: !(Map FunctionName GrinVar),
    envForwardingFunctions :: !(Set FunctionName),
    envInfoSymbols :: !(Map RuntimeInfoKey Symbol),
    envInfos :: ![RuntimeInfo],
    envStaticReferences :: !StaticReferences,
    envSrtSymbols :: !(Map FunctionName Symbol),
    envAddrLiterals :: !(Map BS.ByteString Symbol)
  }

-- | Shared functions that lowered code tail-calls.
data Helper
  = HelperEval
  | -- | Evaluation without an update frame, for a single-entry thunk.
    HelperEvalSingleEntry
  | HelperResume
  | HelperExit
  | HelperCStringLength
  | HelperContinue ![Type]
  | -- | Apply one to 'grinApplyGroupLimit' argument groups to a function.
    HelperApply ![[Type]]
  | -- | The code of the frame that an over-saturated application pushes: it
    -- applies the function that arrives to the argument groups the frame
    -- holds.
    HelperApplyFrame ![[Type]]
  | -- | Continue with one value that arrives as a raw slot: the runtime
    -- does not know whether it is a pointer, so the info table decides.
    HelperContinueSlot
  | HelperApplySlot
  | -- | Collect for a reservation of a function whose code before the
    -- reservation has no effect, then enter that function again with its
    -- relocated arguments. Each argument says whether it is a root. See
    -- 'Replay'.
    HelperCollectReplay ![(Type, Bool)]
  | -- | The slow branch of a WHNF test that pushes the frame of a
    -- continuation and evaluates the value. It takes the value, the info
    -- table of the frame, and the fields of the frame, which have these
    -- types. See 'compileForce'.
    HelperForce !GrinEvalUpdate ![Type]
  deriving (Eq, Ord, Show)

helperSymbol :: Helper -> Symbol
helperSymbol helper =
  Symbol $ case helper of
    HelperEval -> "aihc_lir_eval"
    HelperEvalSingleEntry -> "aihc_lir_eval_single_entry"
    HelperResume -> "aihc_lir_resume"
    HelperExit -> "aihc_lir_exit"
    HelperCStringLength -> "aihc_lir_cstring_length"
    HelperContinue shape -> "aihc_lir_continue_" <> shapeName shape
    HelperApply groups -> "aihc_lir_apply_" <> groupsName groups
    HelperApplyFrame groups -> "aihc_lir_apply_frame_" <> groupsName groups
    HelperContinueSlot -> "aihc_lir_continue_slot"
    HelperApplySlot -> "aihc_lir_apply_slot"
    HelperCollectReplay shape -> "aihc_lir_collect_replay_" <> replayShapeName shape
    HelperForce update shape ->
      "aihc_lir_force_"
        <> (case update of EvalUpdate -> ""; EvalSingleEntry -> "single_")
        <> replayShapeName [(ty, True) | ty <- shape]

shapeName :: [Type] -> Text
shapeName = T.pack . map letter
  where
    letter ty = if ty == Ptr then 'p' else 'i'

-- | A root is @p@, another pointer is @a@, a code address is @c@, and
-- every other type has its own letter.
replayShapeName :: [(Type, Bool)] -> Text
replayShapeName shape = if null shape then "v" else T.pack (map letter shape)
  where
    letter (ty, root) = case ty of
      Ptr -> if root then 'p' else 'a'
      Code -> 'c'
      I1 -> 'b'
      I8 -> 'h'
      I16 -> 's'
      I32 -> 'w'
      I64 -> 'i'
      F32 -> 'f'
      F64 -> 'd'

-- | One group keeps the name of its shape. Several groups separate their
-- shapes with underscores and name an empty group @v@, so the two forms
-- cannot give the same name.
groupsName :: [[Type]] -> Text
groupsName groups =
  case groups of
    [shape] -> shapeName shape
    _ -> T.intercalate "_" (map groupName groups)
  where
    groupName shape = if null shape then "v" else shapeName shape

-- | The info table of the frame an over-saturated application pushes for
-- the given argument groups.
applyFrameInfoSymbol :: [[Type]] -> Symbol
applyFrameInfoSymbol groups = Symbol ("aihc_lir_apply_frame_" <> groupsName groups <> "_info")

-- | An open block under construction.
data OpenBlock = OpenBlock
  { openLabel :: !Label,
    openParameters :: ![(Var, Type)],
    openInstructionsRev :: ![Instruction],
    openCold :: !Bool
  }

data LowerState = LowerState
  { stateNext :: !Int,
    stateTarget :: !LowerTarget,
    -- | See 'lowerProfileAllocations'.
    stateProfileAllocations :: !Bool,
    -- | See 'lowerProfileTime'.
    stateProfileTime :: !Bool,
    -- | The allocation counter of each info table so far: its index and
    -- the name the profile shows.
    stateProfileSites :: !(Map Symbol (Int, Text)),
    stateExterns :: !(Map Symbol Signature),
    stateExternData :: !(Set Symbol),
    stateHelpers :: !(Set Helper),
    -- | The pointer-bitmap array emitted for each distinct bitmap so far.
    stateBitmaps :: !(Map BS.ByteString Symbol),
    stateDefined :: !(Set Symbol),
    stateSignatures :: !(Map Symbol Signature),
    -- | Items of the current conversion action. The consumer drains this list.
    stateItemsRev :: ![Item],
    stateBlocksRev :: ![Block],
    stateOpen :: !(Maybe OpenBlock),
    -- | The context of the code under construction, when it has one.
    stateContext :: !(Maybe Context),
    -- | The store safepoint whose store comes next. See 'beginSafepoint'.
    stateSafepoint :: !(Maybe Safepoint),
    -- | How to run the function again from its entry, while the code so far
    -- has no effect. See 'Replay'.
    stateReplay :: !(Maybe Replay)
  }

-- | The entry of the function under construction and its arguments. While
-- the code since the entry only reads values, running the function again
-- from its entry with the same arguments gives the same result. Thus a
-- reservation there does not need its own collection path: the cold path
-- gives the arguments to a shared helper, which collects with the arguments
-- as roots and enters the function again with the relocated arguments. The
-- function then makes the reservation again. The relocated arguments name
-- the same values, so the code before the reservation reads the same values
-- again. A read from a frame that the entry popped is not such a read,
-- because a collection does not relocate the slots of that frame.
-- The fields are the code of the function, each argument with its type and
-- whether it is a root, and for a body that takes its frame directly, the
-- frame and its size. Such a body runs after the entry popped its frame, so
-- the collection pushes the frame again: the stack pointer goes back above
-- the frame, and the frame is a root. The collection then relocates the
-- slots of the frame, and the body loads them again.
data Replay = Replay !Symbol ![(Operand, Type, Bool)] !(Maybe (Operand, Integer))

-- | The block at a store safepoint and the roots it binds. When the barrier
-- of the store asks for a collection, the cold path spills the roots,
-- collects, and jumps back to the block with the relocated roots, so the
-- store runs again.
data Safepoint = Safepoint
  { safepointLabel :: !Label,
    safepointRoots :: ![Operand],
    -- | The top frame of the thread, when GC lowering knows it.
    safepointTop :: !(Maybe Operand)
  }

-- | The heap and stack context of the running thread. Every function of the
-- aihc convention that runs Haskell code receives it first, and gives it to
-- each function it transfers control to. Thus the register
-- allocator keeps it in registers.
--
-- The capability holds a copy of the heap pointer and the stack pointer only
-- while a C function can read them: the code stores them before such a
-- call. Lir code never changes the heap limit, and C code computes the stack
-- limit when it needs it, so the code does not store the two limits. After a
-- C call that can allocate or collect, the code loads the heap pointer and
-- the heap limit again. The stack pointer changes only at a push and at the
-- entry of a frame, and a C call made from compiled code does not change it.
--
-- A stack chunk is 'stackChunkBytes' long and has that alignment, so the
-- stack limit is the end of the chunk that holds the last pushed byte. A
-- push fits when the new stack pointer is not above the limit.
data Context = Context
  { contextHeap :: !Operand,
    contextHeapLimit :: !Operand,
    contextStack :: !Operand,
    contextStackLimit :: !Operand
  }
  deriving (Eq, Show)

-- | The context values in the order of the parameters.
contextOperands :: Context -> [Operand]
contextOperands context = [contextHeap context, contextHeapLimit context, contextStack context, contextStackLimit context]

-- | The types of the context parameters.
contextTypes :: [Type]
contextTypes = [Ptr, Ptr, Ptr, Ptr]

type LowerM = StateT LowerState (Either LowerError)

failWith :: LowerError -> LowerM value
failWith = lift . Left

-- | Run a lowering action for one program and collect the emitted items, the
-- shared helpers, and the extern declarations.
runLower :: LowerOptions -> GcGrinProgram -> (LowerUnit -> LowerM value) -> Either LowerError (value, [Item])
runLower options gcProgram action = do
  let env = lowerEnvironment options gcProgram
  (value, final) <- runStateT (action (LowerUnit env (gcGrinProgram gcProgram)) <* generateHelpers env Set.empty) (initialLowerState options gcProgram)
  pure (value, externItems final <> reverse (stateItemsRev final))

initialLowerState :: LowerOptions -> GcGrinProgram -> LowerState
initialLowerState options gcProgram =
  LowerState
    { stateNext = 0,
      stateTarget = lowerTarget options,
      stateProfileAllocations = lowerProfileAllocations options,
      stateProfileTime = lowerProfileTime options,
      stateProfileSites = Map.empty,
      stateExterns = Map.empty,
      stateExternData = Set.empty,
      stateHelpers = Set.empty,
      stateBitmaps = Map.empty,
      stateDefined = Set.empty,
      stateSignatures = Map.fromList [(functionSymbol (grinFunctionName function), Signature (contextTypes <> map (repType . grinVarRuntimeRep) (grinFunctionParameters function)) [] AihcConvention) | function <- grinFunctions (gcGrinProgram gcProgram)],
      stateItemsRev = [],
      stateBlocksRev = [],
      stateOpen = Nothing,
      stateContext = Nothing,
      stateSafepoint = Nothing,
      stateReplay = Nothing
    }

externItems :: LowerState -> [Item]
externItems state =
  [ItemExternFunction (ExternFunction symbol signature) | (symbol, signature) <- Map.toAscList (stateExterns state), symbol `Set.notMember` stateDefined state]
    <> [ItemExternData symbol | symbol <- Set.toAscList (stateExternData state), symbol `Set.notMember` stateDefined state]

lowerEnvironment :: LowerOptions -> GcGrinProgram -> LowerEnv
lowerEnvironment options gcProgram =
  LowerEnv
    { envOptions = options,
      envFunctionSymbols = functionSymbols,
      envFunctionParameters = functionParameters,
      envContinuationFunctions = continuationFunctions,
      envFrameEntries = frameEntries,
      envFunctionContinuations = gcFunctionContinuations gcProgram,
      envForwardingFunctions = forwardingFunctions,
      envInfoSymbols = Map.fromList [(key, infoSymbol info) | (key, info) <- constructorEntries <> functionEntries],
      envInfos = map snd (constructorEntries <> functionEntries),
      envStaticReferences = staticReferences,
      envSrtSymbols = srtSymbols,
      envAddrLiterals = Map.fromList [(bytes, Symbol ("aihc_lir_addr_" <> T.pack (show index))) | (index, (bytes, _)) <- zip [0 :: Int ..] (buildAddrLiteralPool program)]
    }
  where
    program = gcGrinProgram gcProgram
    continuationFunctions = gcContinuationFunctions gcProgram
    continuationFrames = gcContinuationFrames gcProgram
    forwardingFunctions = Map.keysSet (Map.filter (== ContinuationFrameForward) continuationFrames)
    functionSymbols = Map.fromList [(grinFunctionName function, functionSymbol (grinFunctionName function)) | function <- grinFunctions program]
    functionParameters = Map.fromList [(grinFunctionName function, map (repType . grinVarRuntimeRep) (grinFunctionParameters function)) | function <- grinFunctions program]
    staticReferences = programStaticReferences program
    srtSymbols =
      Map.fromList
        [ (name, Symbol ("aihc_lir_srt_" <> T.pack (show index)))
        | (index, name) <- zip [0 :: Int ..] (Map.keys (staticReferenceTables staticReferences))
        ]
    constructorLayouts =
      [ (grinConstructorName constructor, grinConstructorLayouts constructor)
      | constructor <- grinConstructors program
      ]
    constructorIndices =
      Map.fromList
        [ (grinConstructorName constructor, grinConstructorIndex constructor)
        | constructor <- grinConstructors program
        ]
    -- Only another unit that can name a constructor needs its tables by
    -- name. A whole program makes every constructor private.
    constructorLinkage =
      Map.fromList
        [ (grinConstructorName constructor, if grinConstructorVis constructor == GrinPub || lowerExposeFunctions options then Export else Internal)
        | constructor <- grinConstructors program
        ]
    -- The program that declares a constructor defines its info tables even
    -- when it builds no node of its own: another module that builds one has
    -- only this program to link its node against.
    requiredConstructorInfos =
      Set.fromList
        ( concatMap declaredConstructorInfos constructorLayouts
            <> concatMap (requiredNodeConstructorInfos . staticObjectNode) (programStaticObjects program)
            <> concatMap requiredNodeConstructorInfos (programNodes program)
            <> [ConstructorRuntimeInfo name SaturatedConstructor | name <- programIdentityCaseConstructors program]
        )
    -- A constructor that another unit can name keeps its tables. A
    -- constructor that no other unit can name gets a table only from the
    -- items of this unit that name it: a node, a static object, and a case
    -- that compares the identity of a table. A case that selects its
    -- alternative from the constructor index names no table.
    declaredConstructorInfos (name, layouts)
      | Map.lookup name constructorLinkage == Just Internal = []
      | null layouts = [ConstructorRuntimeInfo name SaturatedConstructor]
      | otherwise = [ConstructorRuntimeInfo name SaturatedConstructor, ConstructorRuntimeInfo name PartialConstructor]
    -- One constructor needs at most two info tables: the saturated object,
    -- and one shared by every stage that still wants arguments. The partial
    -- table carries the saturated one as its next stage, which is where the
    -- runtime reads the full width from. Both tables have the pointer map of
    -- the full width.
    constructorEntries =
      [ ( key,
          RuntimeInfo
            { infoSymbol = symbol,
              infoLinkage = Map.findWithDefault Export name constructorLinkage,
              infoIdentity = DataSymbol (constructorInfoSymbol name 0) 0,
              -- Both tables describe the saturated slots. A partial object
              -- has filled a prefix of them.
              infoFields = concat layouts,
              infoLayout = concat layouts,
              infoRemainingArity = if stage == SaturatedConstructor then 0 else length layouts,
              infoNext = if stage == SaturatedConstructor then Nothing else Just (constructorInfoSymbol name 0),
              infoEnter = Nothing,
              infoFrameKind = Nothing,
              infoObjectKind = runtimeInfoKeyObjectKind key,
              infoSrt = Nothing,
              infoConstructorIndex = Map.findWithDefault 0 name constructorIndices
            }
        )
      | (name, layouts) <- constructorLayouts,
        stage <- [SaturatedConstructor, PartialConstructor],
        let key = ConstructorRuntimeInfo name stage,
        let symbol = constructorStageSymbol name stage,
        key `Set.member` requiredConstructorInfos
      ]
    infoKeys =
      [ key
      | key <- Set.toAscList (Set.fromList (concatMap runtimeInfoKeyStages (programNodes program))),
        Just name <- [runtimeInfoFunctionName key],
        name `Map.member` functionSymbols
      ]
    infoSymbols = Map.fromList [(key, Symbol ("aihc_lir_info_" <> T.pack (show index))) | (index, key) <- zip [0 :: Int ..] infoKeys]
    functionEntries =
      [ ( key,
          RuntimeInfo
            { infoSymbol = symbol,
              infoLinkage = Internal,
              infoIdentity = DataCode (if name `Set.member` forwardingFunctions then Nothing else Just target),
              infoFields = runtimeInfoKeyFields key,
              infoLayout = runtimeInfoKeyLayout key,
              infoRemainingArity = runtimeInfoKeyRemainingArity key,
              infoNext = runtimeInfoKeyNext key >>= (`Map.lookup` infoSymbols),
              infoEnter = if name `Set.member` forwardingFunctions then Nothing else runtimeEnter target name key,
              infoFrameKind = Map.lookup name continuationFrames,
              infoObjectKind = runtimeInfoKeyObjectKind key,
              infoSrt = Map.lookup name srtSymbols,
              infoConstructorIndex = 0
            }
        )
      | (key, symbol) <- Map.toAscList infoSymbols,
        Just name <- [runtimeInfoFunctionName key],
        Just target <- [Map.lookup name functionSymbols]
      ]
    frameEntries
      | lowerExposeFunctions options = Map.empty
      | otherwise = directFrameEntries continuationFunctions forwardingFunctions functionParameters infoKeys program
    targetParameters name = Map.findWithDefault [] name functionParameters
    -- A closure stage takes all its remaining groups in one entry. An
    -- application supplies at most 'grinApplyGroupLimit' groups, so a stage
    -- that wants more has no entry: an apply helper never enters it.
    runtimeEnter target name key =
      case key of
        ClosureRuntimeInfo _ fields layouts
          | not (null layouts),
            length layouts <= grinApplyGroupLimit ->
              Just
                RuntimeEnter
                  { enterTarget = target,
                    enterStoredOffset = 0,
                    enterStored = map repType fields,
                    enterSupplied = map repType (concat layouts),
                    enterTargetParameters = targetParameters name,
                    enterPassesContinuation = name `Set.notMember` continuationFunctions,
                    enterDirect = Map.member name frameEntries
                  }
        ThunkRuntimeInfo _ fields ->
          Just
            RuntimeEnter
              { enterTarget = target,
                enterStoredOffset = thunkOwnerWords,
                enterStored = map repType fields,
                enterSupplied = [],
                enterTargetParameters = targetParameters name,
                enterPassesContinuation = True,
                enterDirect = False
              }
        _ -> Nothing

-- | A frame can enter its body when no direct call needs the original ABI.
-- A body with a reservation replays through its frame: see 'Replay'.
directFrameEntries :: Set FunctionName -> Set FunctionName -> Map FunctionName [Type] -> [RuntimeInfoKey] -> GrinProgram -> Map FunctionName Int
directFrameEntries frames forwards parameters keys program =
  Map.fromList
    [ (name, length fields)
    | (name, [ClosureRuntimeInfo _ fields layouts]) <- Map.toList shapes,
      Set.member name frames,
      Set.notMember name forwards,
      Set.notMember name called,
      not (null layouts),
      length layouts <= grinApplyGroupLimit,
      Map.lookup name parameters == Just (map repType (fields <> concat layouts))
    ]
  where
    shapes = Map.fromListWith (<>) [(name, [key]) | key <- keys, Just name <- [runtimeInfoFunctionName key]]
    called = foldMap (calls . grinFunctionBody) (grinFunctions program)
    children expression = case expression of
      GrinBind _ value body -> [value, body]
      GrinStoreRec _ body -> [body]
      GrinStoreRecUnchecked _ body -> [body]
      GrinIfWhnf _ ready slow -> [ready, slow]
      GrinCase _ _ alternatives -> map grinAltRhs alternatives
      _ -> []
    calls expression = case expression of
      GrinCall _ name _ -> Set.singleton name
      _ -> foldMap calls (children expression)

-- | The Lir symbol of one GRIN function.
functionSymbol :: FunctionName -> Symbol
functionSymbol (FunctionName name) = Symbol ("aihc_f_" <> renderLinkedFunctionSymbol name)

constructorInfoSymbol :: Text -> Int -> Symbol
constructorInfoSymbol name remaining = Symbol (renderLinkedConstructorInfoSymbol name remaining)

constructorStageSymbol :: Text -> ConstructorStage -> Symbol
constructorStageSymbol name stage =
  case stage of
    SaturatedConstructor -> constructorInfoSymbol name 0
    PartialConstructor -> Symbol (renderLinkedPartialConstructorInfoSymbol name)

globalSymbol :: Text -> Symbol
globalSymbol = Symbol . renderLinkedGlobalSymbol

-- State helpers

fresh :: Text -> LowerM Var
fresh base = do
  state <- get
  put state {stateNext = stateNext state + 1}
  pure (Var (base <> "_" <> T.pack (show (stateNext state))))

freshLabel :: Text -> LowerM Label
freshLabel base = do
  state <- get
  put state {stateNext = stateNext state + 1}
  pure (Label (base <> "_" <> T.pack (show (stateNext state))))

requireExtern :: Symbol -> [Type] -> [Type] -> LowerM ()
requireExtern symbol parameters results =
  modify' $ \state ->
    state {stateExterns = Map.insertWith (\_ old -> old) symbol (Signature parameters results CConvention) (stateExterns state)}

requireExternData :: Symbol -> LowerM ()
requireExternData symbol = modify' $ \state -> state {stateExternData = Set.insert symbol (stateExternData state)}

requireHelper :: Helper -> LowerM Symbol
requireHelper helper = do
  let symbol = helperSymbol helper
  case sharedHelperSignature helper of
    Just signature ->
      modify' $ \state -> state {stateExterns = Map.insert symbol signature (stateExterns state)}
    Nothing ->
      modify' $ \state -> state {stateHelpers = Set.insert helper (stateHelpers state), stateSignatures = Map.insert symbol (helperSignature helper) (stateSignatures state)}
  pure symbol

-- | A helper that runs Haskell code takes the context first. The resumption
-- of the scheduler takes the capability and its resumption. The exit
-- function takes no parameter.
helperSignature :: Helper -> Signature
helperSignature helper = case helper of
  HelperEval -> running [Ptr, Ptr]
  HelperEvalSingleEntry -> running [Ptr, Ptr]
  HelperResume -> signature [Ptr, Ptr] []
  HelperContinue shape -> running (Ptr : shape)
  HelperApply groups -> running (Ptr : Ptr : concat groups)
  -- The frame stores its parent and the values of the groups. The applied
  -- function arrives after them.
  HelperApplyFrame groups -> running (Ptr : concat groups <> [Ptr])
  HelperExit -> signature [] []
  HelperContinueSlot -> running [Ptr, I64]
  HelperApplySlot -> running [Ptr, Ptr, I64]
  -- The arguments of the function come first, so they can stay where the
  -- function received them. The code, the words, and the static reference
  -- table of the function follow them.
  HelperCollectReplay shape -> running (map fst shape <> [Code, I64, Ptr])
  HelperForce _ shape -> running (Ptr : Ptr : shape)
  HelperCStringLength -> Signature [Ptr] [I64] CConvention
  where
    signature parameters results = Signature parameters results AihcConvention
    running parameters = signature (contextTypes <> parameters) []

-- | The runtime Lir unit defines these fixed signatures.
sharedHelperSignature :: Helper -> Maybe Signature
sharedHelperSignature helper =
  case helper of
    HelperExit -> Nothing
    HelperContinue shape | not (common shape) -> Nothing
    HelperApply [shape] | common shape -> Just (helperSignature helper)
    HelperApply _ -> Nothing
    HelperApplyFrame _ -> Nothing
    HelperCollectReplay _ -> Nothing
    HelperForce _ _ -> Nothing
    _ -> Just (helperSignature helper)
  where
    common shape = shape `elem` [[], [Ptr], [I64]]

emitItem :: Item -> LowerM ()
emitItem item = do
  state <- get
  let next = case item of
        ItemFunction function -> state {stateDefined = Set.insert (functionName function) (stateDefined state), stateSignatures = Map.insert (functionName function) (functionSignature function) (stateSignatures state)}
        ItemData value -> state {stateDefined = Set.insert (dataName value) (stateDefined state)}
        _ -> state
  put next {stateItemsRev = item : stateItemsRev state}

beginBlock :: Label -> [(Var, Type)] -> LowerM ()
beginBlock = beginBlockWith False

-- | Open a slow-path block that runs rarely, such as a collection.
beginColdBlock :: Label -> [(Var, Type)] -> LowerM ()
beginColdBlock = beginBlockWith True

beginBlockWith :: Bool -> Label -> [(Var, Type)] -> LowerM ()
beginBlockWith cold label parameters = do
  state <- get
  case stateOpen state of
    Just _ -> failWith (LowerUnsupportedExpression "internal: block opened inside another block")
    Nothing -> put state {stateOpen = Just (OpenBlock label parameters [] cold)}

emit :: [Var] -> Operation -> LowerM ()
emit results operation = do
  state <- get
  case stateOpen state of
    Nothing -> failWith (LowerUnsupportedExpression "internal: instruction outside a block")
    Just open -> put state {stateOpen = Just open {openInstructionsRev = Instruction results operation : openInstructionsRev open}}

-- | Emit an operation with one fresh result.
emitValue :: Text -> Type -> Operation -> LowerM Typed
emitValue base ty operation = do
  var <- fresh base
  emit [var] operation
  pure (Typed (OperandVar var) ty)

terminate :: Terminator -> LowerM ()
terminate terminator = do
  state <- get
  case stateOpen state of
    Nothing -> failWith (LowerUnsupportedExpression "internal: terminator outside a block")
    Just open ->
      put
        state
          { stateOpen = Nothing,
            stateBlocksRev = Block (openLabel open) (openParameters open) (reverse (openInstructionsRev open)) terminator (openCold open) : stateBlocksRev state
          }

-- | Collect the blocks emitted since the last function into a function item.
finishFunction :: Symbol -> Linkage -> [(Var, Type)] -> [Type] -> CallingConvention -> LowerM ()
finishFunction symbol linkage parameters results convention = do
  state <- get
  when (isJust (stateOpen state)) $ failWith (LowerUnsupportedExpression "internal: function finished with an open block")
  put state {stateBlocksRev = []}
  emitItem
    ( ItemFunction $
        simplifyWidths $
          simplifyRotations $
            simplifyBooleanBranches $
              eliminateCommonSubexpressions $
                simplifyMultiplications
                  Function
                    { functionName = symbol,
                      functionLinkage = linkage,
                      functionParameters = parameters,
                      functionResults = results,
                      functionConvention = convention,
                      functionBlocks = reverse (stateBlocksRev state)
                    }
    )

-- Target layout

targetM :: LowerM LowerTarget
targetM = stateTarget <$> get

-- | The integer type with the width of a capability word.
wordType :: LowerTarget -> Type
wordType target = if lowerWordSize target == 8 then I64 else I32

-- | A slot is 8 bytes on every target. A pointer that lives in a slot
-- travels through @i64@ on a 32-bit target, so the high bytes of the slot
-- are always zero. The region says whether the slot is a field of a heap
-- object or a slot of a frame on the stack of a thread. A slot that can be
-- either, such as the stored field an enter stub of a shared shape loads,
-- is in no region.
loadSlot :: Region -> Text -> Type -> Operand -> Integer -> LowerM Typed
loadSlot region base ty object offset = do
  target <- targetM
  if lowerWordSize target == 8 || ty `notElem` [Ptr, Code]
    then emitValue base ty (Load ty (slotAddress region object offset) (byteAlignment 8))
    else do
      word <- emitValue base I64 (Load I64 (slotAddress region object offset) (byteAlignment 8))
      emitValue base ty (PtrFromInt (typedOperand word))

-- | A slot load that orders each later load through its result: the
-- header and field zero of an object that another capability can update.
-- See "Memory model" in docs/multi-threaded-rts-design.md.
loadSlotDependent :: Region -> Text -> Type -> Operand -> Integer -> LowerM Typed
loadSlotDependent region base ty object offset = do
  target <- targetM
  if lowerWordSize target == 8 || ty `notElem` [Ptr, Code]
    then emitValue base ty (LoadDependent ty (slotAddress region object offset))
    else do
      word <- emitValue base I64 (LoadDependent I64 (slotAddress region object offset))
      emitValue base ty (PtrFromInt (typedOperand word))

storeSlot :: Region -> Type -> Operand -> Operand -> Integer -> LowerM ()
storeSlot region ty value object offset = do
  target <- targetM
  if lowerWordSize target == 8 || ty `notElem` [Ptr, Code]
    then emit [] (Store ty value (slotAddress region object offset) (byteAlignment 8))
    else do
      word <- emitValue "slot" I64 (PtrToInt value)
      emit [] (Store I64 (typedOperand word) (slotAddress region object offset) (byteAlignment 8))

-- | The address of a slot in the given region.
slotAddress :: Region -> Operand -> Integer -> Address
slotAddress region object offset = inRegion region (byteAddress object offset)

-- | The address of a field of a heap object.
heapAddress :: Operand -> Integer -> Address
heapAddress = slotAddress HeapRegion

-- | Thunk headers carry evaluation and waiter bits in their low two bits.
-- Every info table has at least four-byte alignment, including on wasm32.
loadObjectInfo :: Region -> Operand -> LowerM Typed
loadObjectInfo region object = snd <$> loadObjectHeader region object

-- | The header word of an object, with its two low bits, and its info table.
-- The region is the one of the object: a frame is on the stack.
loadObjectHeader :: Region -> Operand -> LowerM (Typed, Typed)
loadObjectHeader region object = loadSlot region "header_word" I64 object 0 >>= headerTable

-- | The header word and the info table of an object that another capability
-- can claim or update. The load orders the loads of field zero after it, as
-- the inline evaluation check needs.
loadObjectHeaderDependent :: Region -> Operand -> LowerM (Typed, Typed)
loadObjectHeaderDependent region object = loadSlotDependent region "header_word" I64 object 0 >>= headerTable

headerTable :: Typed -> LowerM (Typed, Typed)
headerTable header = do
  info <- emitValue "info_word" I64 (Binary And I64 (typedOperand header) (OperandLiteral (LitInt (-4))))
  table <- emitValue "header" Ptr (PtrFromInt (typedOperand info))
  pure (header, table)

-- | Byte field @index@ of an info table as an @i64@. The byte fields follow
-- the word fields. An info table never changes, so the loads of its fields
-- are invariant loads. The header word of the object is not: an update
-- rewrites it.
loadInfoByte :: Text -> Operand -> Int -> LowerM Typed
loadInfoByte base header index = do
  target <- targetM
  let offset = toInteger (lowerWordSize target * infoWordFieldCount + index)
  value <- emitValue base I8 (LoadInvariant I8 (byteAddress header offset) (byteAlignment 1))
  emitValue base I64 (Convert ZExt I8 (typedOperand value) I64)

-- | The constructor index of an info table. It follows the byte fields
-- and one byte of padding. See 'infoConstructorIndexOffset'.
loadInfoConstructorIndex :: Operand -> LowerM Typed
loadInfoConstructorIndex header = do
  target <- targetM
  value <- emitValue "constructor_index" I16 (LoadInvariant I16 (byteAddress header (toInteger (infoConstructorIndexOffset target))) (byteAlignment 2))
  emitValue "constructor_index" I64 (Convert ZExt I16 (typedOperand value) I64)

-- | The byte offset of the constructor index in an info table: after the
-- five word fields, the five byte fields, and one byte of padding.
infoConstructorIndexOffset :: LowerTarget -> Int
infoConstructorIndexOffset target = lowerWordSize target * infoWordFieldCount + infoByteFieldCount + 1

-- | A @code@ field of an info table.
loadInfoCode :: Text -> Operand -> Int -> LowerM Typed
loadInfoCode base header index = do
  target <- targetM
  let width = lowerWordSize target
  emitValue base Code (LoadInvariant Code (byteAddress header (toInteger (width * index))) (wordAlignment 1))

-- | A @ptr@ field of an info table.
loadInfoPointer :: Text -> Operand -> Int -> LowerM Typed
loadInfoPointer base header index = do
  target <- targetM
  let width = lowerWordSize target
  emitValue base Ptr (LoadInvariant Ptr (byteAddress header (toInteger (width * index))) (wordAlignment 1))

-- | A word-sized integer field of a data object.
wordField :: LowerTarget -> Integer -> DataField
wordField target = DataInt (wordType target)

-- | A pointer stored in an 8-byte slot of a static object.
slotPointerFields :: LowerTarget -> DataField -> [DataField]
slotPointerFields target field
  | lowerWordSize target == 8 = [field]
  | otherwise = [field, DataInt I32 0]

-- | The exit-code field follows the machine pointer.
capabilityExitCodeOffset :: LowerTarget -> Integer
capabilityExitCodeOffset = toInteger . lowerWordSize

-- | The bump pointer of the heap, the field after the exit code. A pointer
-- field, so its offset follows the target word size.
capabilityHeapNextOffset :: LowerTarget -> Integer
capabilityHeapNextOffset target = capabilityExitCodeOffset target + toInteger (lowerWordSize target)

-- | The end of the space the bump pointer runs into, the field after it. A
-- reservation compares the two itself and only calls the runtime when the
-- words it wants do not fit.
capabilityHeapLimitOffset :: LowerTarget -> Integer
capabilityHeapLimitOffset target = capabilityHeapNextOffset target + toInteger (lowerWordSize target)

-- | The first free byte of the stack of the running thread, the field after
-- the end of the space. The field holds the stack pointer only while a C
-- call can read it. See 'Context'.
capabilityStackNextOffset :: LowerTarget -> Integer
capabilityStackNextOffset target = capabilityHeapLimitOffset target + toInteger (lowerWordSize target)

-- | The top frame of a promotion, after the nursery size. See
-- 'storePauseTop'.
capabilityPauseTopOffset :: LowerTarget -> Integer
capabilityPauseTopOffset target = 6 * toInteger (lowerWordSize target) + 16

-- | The code of the function that started last, after the top frame of a
-- promotion. Only a time profile stores it. See 'storeProfileEntry'.
capabilityProfileCodeOffset :: LowerTarget -> Integer
capabilityProfileCodeOffset target = capabilityPauseTopOffset target + toInteger (lowerWordSize target)

-- | The continuation of the function that started last, after its code.
capabilityProfileFrameOffset :: LowerTarget -> Integer
capabilityProfileFrameOffset target = capabilityProfileCodeOffset target + toInteger (lowerWordSize target)

-- | The nursery base follows the stack pointer.
capabilityHeapStartOffset :: LowerTarget -> Integer
capabilityHeapStartOffset target = 5 * toInteger (lowerWordSize target)

-- | The nursery size follows seven pointer fields, with 8-byte alignment.
capabilityNurseryBytesOffset :: LowerTarget -> Integer
capabilityNurseryBytesOffset target = 6 * toInteger (lowerWordSize target) + 8

-- | The size and the alignment of a thread stack chunk. See
-- @aihc_runtime_internal.h@.
stackChunkBytes :: Integer
stackChunkBytes = 4096

-- | The bytes of the header of a stack chunk. The first frame of a chunk
-- follows it.
stackChunkHeaderBytes :: Integer
stackChunkHeaderBytes = 64

-- Context

-- | Read the capability pointer from the active stack chunk.
capabilityOperand :: Context -> LowerM Operand
capabilityOperand context = do
  target <- targetM
  let offset = 4 * toInteger (lowerWordSize target) + 24 - stackChunkBytes
  typedOperand <$> emitValue "capability" Ptr (Load Ptr (byteAddress (contextStackLimit context) offset) (wordAlignment 1))

currentCapability :: LowerM Operand
currentCapability = currentContext >>= capabilityOperand

-- | The write barrier before a pointer store into an existing object.
-- A store into the local nursery needs no runtime call.
-- Its aligned base and size come from the capability of the stack chunk.
-- The call allocates nothing and reads no context, so it needs no sync.
-- | The write barrier of a store into an object. The cold call also takes
-- the value that the site stores, so the runtime can count an escape.
emitWriteBarrier :: FunctionCtx -> Operand -> Operand -> LowerM Operand
emitWriteBarrier ctx object = emitBarrier ctx object Nothing

-- | The write barrier of a store into a boxed array. The cold call takes
-- the index of the element: a large array keeps a card for each run of
-- elements, and the runtime marks the card of the store.
emitWriteBarrierAt :: FunctionCtx -> Operand -> Operand -> Operand -> LowerM Operand
emitWriteBarrierAt ctx object index = emitBarrier ctx object (Just index)

-- | The result is the value to store: the value, or its global copy when
-- the store is an escape. When the promotion of the value needs a
-- collection, the cold path collects at the safepoint of the store and
-- runs the store again.
emitBarrier :: FunctionCtx -> Operand -> Maybe Operand -> Operand -> LowerM Operand
emitBarrier ctx object index value = do
  target <- targetM
  capability <- currentCapability
  objectWord <- emitValue "object_word" I64 (PtrToInt object)
  start <- emitValue "nursery_start" Ptr (Load Ptr (byteAddress capability (capabilityHeapStartOffset target)) (wordAlignment 1))
  startWord <- emitValue "nursery_start_word" I64 (PtrToInt (typedOperand start))
  offset <- emitValue "nursery_offset" I64 (Binary Xor I64 (typedOperand objectWord) (typedOperand startWord))
  bytes <- emitValue "nursery_bytes" I64 (Load I64 (byteAddress capability (capabilityNurseryBytesOffset target)) (byteAlignment 8))
  young <- emitValue "young" I1 (Compare LtU I64 (typedOperand offset) (typedOperand bytes))
  old <- freshLabel "barrier_old"
  done <- freshLabel "barrier_done"
  context <- currentContext
  terminate (Branch (typedOperand young) (Target done [value, contextHeapLimit context]) (Target old []))
  beginColdBlock old []
  storePauseTop capability
  stored <- case index of
    Nothing -> callRuntime "aihc_write_barrier_store" [Ptr, Ptr, I64] [I64] [capability, object, value]
    Just element -> callRuntime "aihc_write_barrier_store_at" [Ptr, Ptr, I64, I64] [I64] [capability, object, element, value]
  collect <- emitValue "barrier_escape" I1 (Compare Eq I64 stored (OperandLiteral (LitInt 1)))
  escapeCollectWhen ctx collect
  -- The cold path lowers the heap limit when the barrier work reaches its
  -- budget, so the next reservation collects. See "Remembered set and mark
  -- buffer" in docs/multi-threaded-rts-design.md.
  reloaded <- emitValue "hp_limit" Ptr (Load Ptr (byteAddress capability (capabilityHeapLimitOffset target)) (wordAlignment 1))
  terminate (Jump (Target done [stored, typedOperand reloaded]))
  storedVar <- fresh "stored"
  limit <- fresh "hp_limit"
  beginBlock done [(storedVar, I64), (limit, Ptr)]
  setContext context {contextHeapLimit = OperandVar limit}
  pure (OperandVar storedVar)

-- | The block of a store safepoint. Its parameters are the live roots and
-- the heap context, and the store follows it. See 'Safepoint'.
beginSafepoint :: FunctionCtx -> ValueEnv -> [GrinVar] -> Maybe GrinValue -> [GrinValue] -> LowerM ValueEnv
beginSafepoint ctx env vars top roots = do
  rootOperands <- mapM (pointerValue ctx env) roots
  -- A frame does not move, so the operand stays valid after a collection.
  topOperand <- traverse (pointerValue ctx env) top
  context <- currentContext
  label <- freshLabel "safepoint"
  parameters <- mapM (fresh . varBase) vars
  heap <- fresh "hp"
  heapLimit <- fresh "hp_limit"
  terminate (Jump (Target label (rootOperands <> [contextHeap context, contextHeapLimit context])))
  beginBlock label ([(parameter, Ptr) | parameter <- parameters] <> [(heap, Ptr), (heapLimit, Ptr)])
  setContext context {contextHeap = OperandVar heap, contextHeapLimit = OperandVar heapLimit}
  modify' (\state -> state {stateSafepoint = Just (Safepoint label (map OperandVar parameters) topOperand)})
  pure (Map.fromList [(var, Typed (OperandVar parameter) Ptr) | (var, parameter) <- zip vars parameters] `Map.union` env)

-- | Give the capability the top frame of the thread before a call whose
-- promotion runs the pause walk from it. A null top frame makes the
-- promotion collect for a young thunk.
storePauseTop :: Operand -> LowerM ()
storePauseTop capability = do
  target <- targetM
  top <- gets (maybe Nothing safepointTop . stateSafepoint)
  emit [] (Store Ptr (fromMaybe (OperandLiteral LitNull) top) (byteAddress capability (capabilityPauseTopOffset target)) (wordAlignment 1))

-- | When the flag is set, collect at the safepoint of the store: spill its
-- roots, run the collection that moves every young survivor into gen2,
-- and run the store again from the safepoint with the relocated roots.
escapeCollectWhen :: FunctionCtx -> Typed -> LowerM ()
escapeCollectWhen ctx flag = do
  safepoint <- gets stateSafepoint >>= maybe (failWith (LowerUnsupportedExpression "internal: an escaping store without a safepoint")) pure
  context <- currentContext
  collect <- freshLabel "escape_collect"
  continue <- freshLabel "escape_done"
  terminate (Branch (typedOperand flag) (Target collect []) (Target continue []))
  beginColdBlock collect []
  let roots = safepointRoots safepoint
  array <-
    case (roots, ctxRoots ctx) of
      ([], _) -> pure (OperandLiteral LitNull)
      (_, Just array) -> pure array
      _ -> failWith (LowerUnsupportedExpression "internal: roots without a root array")
  forM_ (zip [0 :: Int ..] roots) $ \(index, root) ->
    storeSlot AnyRegion Ptr root array (toInteger (8 * index))
  capability <- capabilityOperand context
  storeContext capability context
  _ <- callRuntime "aihc_escape_collect" [Ptr, I64, Ptr, Ptr] [] [capability, OperandLiteral (LitInt (toInteger (length roots))), array, ctxSrt ctx]
  relocated <- forM (zip [0 :: Int ..] roots) $ \(index, _) ->
    loadSlot AnyRegion "root" Ptr array (toInteger (8 * index))
  collected <- loadHeapContext capability context
  terminate (Jump (Target (safepointLabel safepoint) (map typedOperand relocated <> [contextHeap collected, contextHeapLimit collected])))
  beginBlock continue []
  modify' (\state -> state {stateSafepoint = Nothing})

-- | Fresh parameters for a context.
freshContext :: LowerM (Context, [(Var, Type)])
freshContext = do
  vars <- mapM fresh ["hp", "hp_limit", "sp", "sp_limit"]
  case map OperandVar vars of
    [heap, heapLimit, stack, stackLimit] -> pure (Context heap heapLimit stack stackLimit, zip vars contextTypes)
    _ -> failWith (LowerUnsupportedExpression "internal: context arity")

-- | The context of the code under construction.
currentContext :: LowerM Context
currentContext = gets stateContext >>= maybe (failWith (LowerUnsupportedExpression "internal: code without a context")) pure

setContext :: Context -> LowerM ()
setContext context = modify' (\state -> state {stateContext = Just context})

-- | Compile one branch. The next branch starts with the context this branch
-- started with.
branchWith :: LowerM value -> LowerM value
branchWith action = do
  context <- gets stateContext
  replay <- gets stateReplay
  result <- action
  modify' (\state -> state {stateContext = context, stateReplay = replay})
  pure result

-- | The code under construction can have an effect from here on, so the
-- function cannot run again from its entry. See 'Replay'.
endReplay :: LowerM ()
endReplay = modify' (\state -> state {stateReplay = Nothing})

-- | Give the capability the heap pointer and the stack pointer before a C call
-- that can read them.
storeContext :: Operand -> Context -> LowerM ()
storeContext capability context = do
  target <- targetM
  emit [] (Store Ptr (contextHeap context) (byteAddress capability (capabilityHeapNextOffset target)) (wordAlignment 1))
  emit [] (Store Ptr (contextStack context) (byteAddress capability (capabilityStackNextOffset target)) (wordAlignment 1))

-- | Load the heap pointer and the heap limit of the capability after a C call
-- that can allocate or collect.
loadHeapContext :: Operand -> Context -> LowerM Context
loadHeapContext capability context = do
  target <- targetM
  heap <- emitValue "hp" Ptr (Load Ptr (byteAddress capability (capabilityHeapNextOffset target)) (wordAlignment 1))
  heapLimit <- emitValue "hp_limit" Ptr (Load Ptr (byteAddress capability (capabilityHeapLimitOffset target)) (wordAlignment 1))
  pure context {contextHeap = typedOperand heap, contextHeapLimit = typedOperand heapLimit}

-- | Load the whole context of the capability, where C code gives control to
-- Haskell code.
loadContext :: Operand -> LowerM Context
loadContext capability = do
  target <- targetM
  stack <- emitValue "sp" Ptr (Load Ptr (byteAddress capability (capabilityStackNextOffset target)) (wordAlignment 1))
  -- The stack pointer can be the end of a full chunk, so the limit comes
  -- from the byte before it.
  below <- emitValue "sp_below" Ptr (PtrAdd (typedOperand stack) (OperandLiteral (LitInt (-1))))
  stackLimit <- chunkEnd (typedOperand below)
  loadHeapContext capability (Context (OperandLiteral LitNull) (OperandLiteral LitNull) (typedOperand stack) stackLimit)

-- | The end of the stack chunk that holds an address.
chunkEnd :: Operand -> LowerM Operand
chunkEnd address = do
  word <- emitValue "chunk_word" I64 (PtrToInt address)
  last' <- emitValue "chunk_last" I64 (Binary Or I64 (typedOperand word) (OperandLiteral (LitInt (stackChunkBytes - 1))))
  end <- emitValue "chunk_end" I64 (Binary Add I64 (typedOperand last') (OperandLiteral (LitInt 1)))
  typedOperand <$> emitValue "sp_limit" Ptr (PtrFromInt (typedOperand end))

-- | Store the context of the code under construction to the capability.
syncCapability :: Operand -> LowerM ()
syncCapability capability = currentContext >>= storeContext capability

-- | Load the heap pointer and the heap limit into the context of the code
-- under construction.
reloadHeap :: Operand -> LowerM ()
reloadHeap capability = currentContext >>= loadHeapContext capability >>= setContext

-- | A C call that can allocate, collect, or read the stack pointer. The
-- capability gets the context before the call, and the heap pointer and the
-- heap limit come back after it. A C call without the capability cannot see
-- the context, so it needs neither step.
callSynced :: Operand -> Text -> [Type] -> [Type] -> [Operand] -> LowerM Operand
callSynced capability name parameters results arguments = do
  syncCapability capability
  result <- callRuntime name parameters results arguments
  reloadHeap capability
  pure result

-- | The context, the first arguments of each transfer to Haskell code.
runningArguments :: LowerM [Operand]
runningArguments = contextOperands <$> currentContext

-- Coercion

-- | Convert a typed operand to a type. Pointers and words convert both ways.
-- Narrow C integers widen with sign extension and narrow with truncation.
coerce :: Type -> Typed -> LowerM Operand
coerce ty (Typed operand actual)
  | ty == actual = pure operand
  | otherwise =
      case (actual, ty) of
        (Ptr, I64) -> result (PtrToInt operand)
        (I64, Ptr) -> result (PtrFromInt operand)
        (I64, I8) -> result (Convert Trunc I64 operand I8)
        (I64, I16) -> result (Convert Trunc I64 operand I16)
        (I64, I32) -> result (Convert Trunc I64 operand I32)
        (I32, I64) -> result (Convert SExt I32 operand I64)
        (Ptr, I32) -> do
          word <- result (PtrToInt operand)
          result (Convert Trunc I64 word I32)
        (I32, Ptr) -> do
          word <- result (Convert SExt I32 operand I64)
          result (PtrFromInt word)
        -- A Float# value travels as its bit pattern in the low 32 bits of an
        -- integer slot, and a Double# value as its 64-bit pattern, so the C
        -- ABI reads them as floats rather than converting the number.
        (I64, F64) -> result (Convert Bitcast I64 operand F64)
        (I64, F32) -> do
          narrow <- typedOperand <$> emitValue "coerced" I32 (Convert Trunc I64 operand I32)
          result (Convert Bitcast I32 narrow F32)
        _ -> failWith (LowerUnsupportedValue ("cannot convert " <> T.pack (show actual) <> " to " <> T.pack (show ty)))
  where
    result operation = typedOperand <$> emitValue "coerced" ty operation

coerceTo :: Type -> Typed -> LowerM Typed
coerceTo ty typed = (`Typed` ty) <$> coerce ty typed

-- Unit lowering

-- | Lower the functions, the static objects, the info tables, the enter
-- stubs, the static reference tables, and the address literals of the
-- program.
lowerUnitItems :: LowerUnit -> LowerM ()
lowerUnitItems (LowerUnit env program) = sequence_ (lowerUnitActions env program)

-- | Each action produces one function or data item, or an info table and stub.
lowerUnitActions :: LowerEnv -> GrinProgram -> [LowerM ()]
lowerUnitActions env program@(GrinProgram _ _ _ _ functions) =
  [mapM_ validateRuntimeRep (programRuntimeReps program), mapM_ validateSlotRep (programSlotReps program)]
    <> [lowerFunction env function | function <- functions, grinFunctionName function `Set.notMember` envForwardingFunctions env]
    <> map (lowerStaticObject env) (programStaticObjects program)
    <> map lowerInfo (envInfos env)
    <> [lowerCallbackPool call signature | call <- grinForeignCalls program, GrinForeignWrapper signature <- [grinForeignCallTarget call]]
    <> [lowerStaticReferenceTables env]
    <> [emitItem (ItemData (DataItem symbol Internal False 1 [DataBytes bytes, DataInt I8 0])) | (bytes, symbol) <- Map.toAscList (envAddrLiterals env)]
    <> [lowerAllocationProfile | lowerProfileAllocations (envOptions env), lowerUnitKind (envOptions env) == LibraryUnit]
    <> [lowerTimeProfile env functions | lowerProfileTime (envOptions env), lowerUnitKind (envOptions env) == LibraryUnit]

-- | Aggregate representations describe results, never individual slots.
validateSlotRep :: GrinRep -> LowerM ()
validateSlotRep representation =
  case representation of
    SumRep {} -> failWith (LowerUnsupportedRuntimeRep representation)
    TupleRep {} -> failWith (LowerUnsupportedRuntimeRep representation)
    _ -> validateRuntimeRep representation

validateRuntimeRep :: GrinRep -> LowerM ()
validateRuntimeRep runtimeRep =
  case runtimeRep of
    VecRep {} -> failWith (LowerUnsupportedRuntimeRep runtimeRep)
    TupleRep reps -> mapM_ validateRuntimeRep reps
    SumRep reps -> mapM_ validateRuntimeRep reps
    _ -> pure ()

-- | The pointer bitmap one info table describes.
infoBitmap :: RuntimeInfo -> BS.ByteString
infoBitmap info = BS.pack [if isPointerRuntimeRep field then 1 else 0 | field <- infoLayout info]

-- | The array holding one pointer bitmap, emitted once per distinct bitmap.
--
-- Info tables share an array whenever their bitmaps agree, which is often:
-- the application stages of an arity-n constructor ask between them for every
-- prefix of one layout, and those prefixes repeat across every constructor
-- built the same way. Naming the arrays after the info tables that wanted
-- them would keep one array per stage, and each such name spells out the
-- mangled constructor name -- hundreds of bytes for a wide tuple, where the
-- symbol costs far more than the handful of bytes it points at.
internBitmap :: BS.ByteString -> LowerM (Maybe Symbol)
internBitmap bytes
  | BS.null bytes = pure Nothing
  | otherwise = do
      known <- gets stateBitmaps
      case Map.lookup bytes known of
        Just symbol -> pure (Just symbol)
        Nothing -> do
          let symbol = Symbol ("aihc_lir_bitmap_" <> T.pack (show (Map.size known)))
          modify' (\state -> state {stateBitmaps = Map.insert bytes symbol (stateBitmaps state)})
          emitItem (ItemData (DataItem symbol Internal False 1 [DataBytes bytes]))
          pure (Just symbol)

lowerInfo :: RuntimeInfo -> LowerM ()
lowerInfo info = do
  target <- targetM
  let fields = infoFields info
      byte = DataInt I8 . toInteger
  when (length fields > infoByteFieldLimit) $
    failWith (LowerUnsupportedValue ("an object with more than 255 fields: " <> unSymbol (infoSymbol info)))
  when (infoRemainingArity info > infoByteFieldLimit) $
    failWith (LowerUnsupportedValue ("a function with more than 255 arguments: " <> unSymbol (infoSymbol info)))
  when (infoConstructorIndex info > infoConstructorIndexLimit) $
    failWith (LowerUnsupportedValue ("a data type with more than 65536 constructors: " <> unSymbol (infoSymbol info)))
  bitmap <- internBitmap (infoBitmap info)
  entry <- traverse (enterFunction (infoSymbol info)) (infoEnter info)
  emitItem
    ( ItemData
        DataItem
          { dataName = infoSymbol info,
            dataLinkage = infoLinkage info,
            dataMutable = False,
            dataAlignment = toInteger (lowerWordSize target),
            dataFields =
              [ infoIdentity info,
                maybe DataNull (`DataSymbol` 0) bitmap,
                maybe DataNull (`DataSymbol` 0) (infoNext info),
                DataCode entry,
                maybe DataNull (`DataSymbol` 0) (infoSrt info),
                byte (length fields),
                byte (infoRemainingArity info),
                byte (continuationFrameKindCode (infoFrameKind info)),
                byte (infoObjectKind info),
                DataInt I8 (objectKindNeedsEval (infoObjectKind info)),
                DataInt I8 0,
                DataInt I16 (toInteger (infoConstructorIndex info))
              ]
          }
    )

-- | The function that enters one object: the code itself when the code
-- takes the object directly, and otherwise a stub of its own. The stub loads
-- the stored fields and calls the code directly. Thus an entry through the
-- info table is one indirect call, and each kind of object has its own
-- branch history.
enterFunction :: Symbol -> RuntimeEnter -> LowerM Symbol
enterFunction _ enter | enterDirect enter = pure (enterTarget enter)
enterFunction info enter = do
  let stub = Symbol (unSymbol info <> "_e")
  lowerEnterStub stub enter
  pure stub

-- | The dynamic entry of one enterable object. It loads the stored fields,
-- takes the supplied values as parameters, and tail-calls the code.
lowerEnterStub :: Symbol -> RuntimeEnter -> LowerM ()
lowerEnterStub stub enter = do
  (context, contextParameters) <- freshContext
  object <- fresh "object"
  continuation <- fresh "continuation"
  supplied <- forM (enterSupplied enter) $ \ty -> (,ty) <$> fresh "supplied"
  beginBlock (Label "entry") []
  stored <- forM (zip [0 :: Int ..] (enterStored enter)) $ \(index, ty) ->
    loadSlot AnyRegion "stored" ty (OperandVar object) (toInteger (8 * (index + 1 + enterStoredOffset enter)))
  let values = stored <> [Typed (OperandVar var) ty | (var, ty) <- supplied] <> [Typed (OperandVar continuation) Ptr | enterPassesContinuation enter]
      parameters = enterTargetParameters enter
  when (length parameters /= length values) $
    failWith (LowerUnsupportedExpression ("enter stub arity mismatch for " <> unSymbol (enterTarget enter) <> T.pack (": " <> show (length parameters) <> " parameters, " <> show (length (enterStored enter)) <> " stored, " <> show (length (enterSupplied enter)) <> " supplied, continuation " <> show (enterPassesContinuation enter))))
  arguments <- zipWithM coerce parameters values
  terminate (TailCall (enterTarget enter) (contextOperands context <> arguments))
  finishFunction stub Internal (contextParameters <> [(object, Ptr), (continuation, Ptr)] <> supplied) [] AihcConvention

-- | A continuation object kind for an entry or a harness: the info table,
-- and the stub that enters the target function with the stored fields and
-- the supplied values.
data ContinuationSpec = ContinuationSpec
  { continuationInfo :: !Symbol,
    continuationTarget :: !Symbol,
    continuationStored :: ![Type],
    continuationSupplied :: ![Type],
    continuationFrame :: !ContinuationFrameKind
  }

continuationInfoItems :: ContinuationSpec -> LowerM ()
continuationInfoItems spec = lowerInfo info
  where
    stored = map typeRep (continuationStored spec)
    supplied = map typeRep (continuationSupplied spec)
    info =
      RuntimeInfo
        { infoSymbol = continuationInfo spec,
          infoLinkage = Internal,
          infoIdentity = DataCode (Just (continuationTarget spec)),
          infoFields = stored,
          infoLayout = stored <> supplied,
          infoRemainingArity = 1,
          infoNext = Nothing,
          infoEnter =
            Just
              RuntimeEnter
                { enterTarget = continuationTarget spec,
                  enterStoredOffset = 0,
                  enterStored = continuationStored spec,
                  enterSupplied = continuationSupplied spec,
                  enterTargetParameters = continuationStored spec <> continuationSupplied spec,
                  enterPassesContinuation = False,
                  enterDirect = False
                },
          infoFrameKind = Just (continuationFrame spec),
          infoObjectKind = runtimeObjectClosure,
          infoSrt = Nothing,
          infoConstructorIndex = 0
        }
    -- Info tables carry pointer bitmaps, so recover a representation from
    -- the Lir type.
    typeRep ty = if ty == Ptr then BoxedRep Lifted else IntRep

-- | A static object has the layout of a heap object: an 8-byte header slot
-- and 8-byte field slots. A pointer occupies the low bytes of its slot.
lowerStaticObject :: LowerEnv -> StaticObject -> LowerM ()
lowerStaticObject env object = do
  target <- targetM
  header <- slotPointerFields target . (`DataSymbol` 0) <$> nodeInfoSymbol env node
  fields <- concat <$> mapM (staticField target) (grinNodeFields node)
  let applied = [DataInt I64 (toInteger (length (grinNodeFields node))) | isPartialConstructorNode node]
      -- The owner word of a static thunk starts as zero.
      owner = [DataZero (8 * toInteger thunkOwnerWords) | isThunk]
  emitItem (ItemData (DataItem (globalSymbol (staticObjectName object)) linkage True 8 (header <> applied <> owner <> fields)))
  where
    node = staticObjectNode object
    -- A private object is named only from inside this object file, so give
    -- it internal linkage and let the assembler and the linker see that.
    linkage = case staticObjectVis object of
      GrinPub -> Export
      GrinPrivate -> Internal
    isThunk = case grinNodeTag node of
      GrinThunk {} -> True
      _ -> False
    staticField target value =
      case value of
        GrinVarValue var -> referenceGlobal target (grinVarName var)
        GrinGlobalValue name -> referenceGlobal target name
        GrinLitValue literal ->
          case literal of
            GrinLitAddr bytes -> do
              symbol <- addrLiteralSymbol env bytes
              pure (slotPointerFields target (DataSymbol symbol 0))
            _ -> maybe (failWith (LowerUnsupportedValue "string literal")) (pure . (: []) . DataInt I64) (normalizedLiteralInteger literal)
    referenceGlobal target name = do
      let symbol = globalSymbol name
      requireExternData symbol
      pure (slotPointerFields target (DataSymbol symbol 0))

nodeInfoSymbol :: LowerEnv -> GrinNode -> LowerM Symbol
nodeInfoSymbol env node =
  case grinNodeTag node of
    GrinConstructor name remaining -> lookupInfo (ConstructorRuntimeInfo name (constructorStage remaining))
    GrinClosure name layouts -> lookupInfo (ClosureRuntimeInfo name fields layouts)
    GrinThunk name -> lookupInfo (ThunkRuntimeInfo name fields)
  where
    fields = map grinValueRuntimeRep (grinNodeFields node)
    lookupInfo key =
      case Map.lookup key (envInfoSymbols env) of
        Just symbol -> pure symbol
        Nothing ->
          case key of
            ConstructorRuntimeInfo name stage -> do
              let symbol = constructorStageSymbol name stage
              requireExternData symbol
              pure symbol
            ClosureRuntimeInfo name _ _ -> failWith (LowerMissingFunction name)
            ThunkRuntimeInfo name _ -> failWith (LowerMissingFunction name)

addrLiteralSymbol :: LowerEnv -> BS.ByteString -> LowerM Symbol
addrLiteralSymbol env bytes =
  maybe (failWith (LowerUnsupportedValue "unregistered Addr# literal")) pure (Map.lookup bytes (envAddrLiterals env))

lowerStaticReferenceTables :: LowerEnv -> LowerM ()
lowerStaticReferenceTables env = do
  target <- targetM
  forM_ (Map.toList (staticReferenceTables (envStaticReferences env))) $ \(name, table) ->
    forM_ (Map.lookup name (envSrtSymbols env)) $ \symbol -> do
      let objects = map globalSymbol (srtObjects table)
          children = [child | name' <- srtChildren table, Just child <- [Map.lookup name' (envSrtSymbols env)]]
          word = wordField target
      mapM_ requireExternData objects
      emitItem
        ( ItemData
            ( DataItem
                symbol
                Internal
                True
                (toInteger (lowerWordSize target))
                ( [word 0, word (toInteger (length objects)), word (toInteger (length children))]
                    <> [DataSymbol object 0 | object <- objects]
                    <> [DataSymbol child 0 | child <- children]
                )
            )
        )

-- Functions

data FunctionCtx = FunctionCtx
  { ctxEnv :: !LowerEnv,
    ctxFunctionName :: !FunctionName,
    -- | The function's static reference table, or null when its code reaches
    -- no traced static object. A running function has no heap object to
    -- carry its table, so its safepoints pass it to the collector.
    ctxSrt :: !Operand,
    ctxCodeSlot :: !(Maybe Operand),
    ctxForeignFrame :: !(Maybe Operand),
    ctxRoots :: !(Maybe Operand)
  }

type ValueEnv = Map GrinVar Typed

lowerFunction :: LowerEnv -> GrinFunction -> LowerM ()
lowerFunction env function = do
  (context, contextParameters) <- freshContext
  setContext context
  originalParameters <- forM (grinFunctionParameters function) $ \var -> do
    lirVar <- fresh (varBase var)
    pure (var, lirVar, repType (grinVarRuntimeRep var))
  let direct = Map.lookup (grinFunctionName function) (envFrameEntries env)
  (entryParameters, parameters, frameReplay) <- case direct of
    Nothing -> do
      beginBlock (Label "entry") []
      pure ([(lirVar, ty) | (_, lirVar, ty) <- originalParameters], originalParameters, Nothing)
    Just count -> do
      object <- fresh "frame"
      unused <- fresh "unused"
      beginBlock (Label "entry") []
      let (stored, supplied) = splitAt count originalParameters
      loaded <- forM (zip [1 :: Int ..] stored) $ \(index, (var, _, ty)) -> do
        value <- loadSlot StackRegion "stored" ty (OperandVar object) (toInteger (8 * index))
        case typedOperand value of
          OperandVar name -> pure (var, name, ty)
          _ -> failWith (LowerUnsupportedExpression "frame field has no variable")
      let replayArguments =
            (OperandVar object, Ptr, True)
              : (OperandVar unused, Ptr, False)
              : [(OperandVar lirVar, ty, isPointerRuntimeRep (grinVarRuntimeRep var)) | (var, lirVar, ty) <- supplied]
      pure
        ( (object, Ptr) : (unused, Ptr) : [(lirVar, ty) | (_, lirVar, ty) <- supplied],
          loaded <> supplied,
          Just (Replay (functionSymbol (grinFunctionName function)) replayArguments (Just (OperandVar object, toInteger (8 * (count + 1)))))
        )
  when (lowerProfileTime (envOptions env)) $
    storeProfileEntry (grinFunctionName function) (profileContinuation env function [lirVar | (_, lirVar, _) <- parameters])
  let srt = maybe (OperandLiteral LitNull) (OperandLiteral . LitSymbol) (Map.lookup (grinFunctionName function) (envSrtSymbols env))
  roots <-
    case maximumRoots (grinFunctionBody function) of
      0 -> pure Nothing
      count -> Just . typedOperand <$> emitValue "roots" Ptr (StackAlloc (toInteger (8 * count)) (byteAlignment 8))
  foreignFrame <- if hasForeignCall needsForeignFrame (grinFunctionBody function) then Just . typedOperand <$> emitValue "foreign_frame" Ptr (StackAlloc 40 (byteAlignment 8)) else pure Nothing
  codeSlot <- if hasForeignCall needsCodeSlot (grinFunctionBody function) then Just . typedOperand <$> emitValue "function_pointer" Ptr (StackAlloc 8 (byteAlignment 8)) else pure Nothing
  let ctx = FunctionCtx {ctxEnv = env, ctxFunctionName = grinFunctionName function, ctxSrt = srt, ctxRoots = roots, ctxCodeSlot = codeSlot, ctxForeignFrame = foreignFrame}
      valueEnv = Map.fromList [(var, Typed (OperandVar lirVar) ty) | (var, lirVar, ty) <- parameters]
      replay = Replay (functionSymbol (grinFunctionName function)) [(OperandVar lirVar, ty, isPointerRuntimeRep (grinVarRuntimeRep var)) | (var, lirVar, ty) <- parameters] Nothing
  modify' (\state -> state {stateReplay = Just (fromMaybe replay frameReplay)})
  compileExpr ctx valueEnv (grinFunctionBody function)
  modify' (\state -> state {stateContext = Nothing, stateReplay = Nothing})
  finishFunction
    (functionSymbol (grinFunctionName function))
    (if lowerExposeFunctions (envOptions env) then Export else Internal)
    (contextParameters <> entryParameters)
    []
    AihcConvention

-- | The continuation of a function as the time profiler sees it: the
-- hidden continuation parameter of a computation, or the parent of a
-- continuation frame, which the frame gives as the first parameter. See
-- 'lowerProfileTime'.
profileContinuation :: LowerEnv -> GrinFunction -> [Var] -> Maybe Var
profileContinuation env function parameters =
  case Map.lookup name (envFunctionContinuations env) of
    Just continuation -> lookup continuation (zip (grinFunctionParameters function) parameters)
    Nothing
      | name `Set.member` envContinuationFunctions env -> listToMaybe parameters
      | otherwise -> Nothing
  where
    name = grinFunctionName function

-- | Give the capability the continuation and the code of the function that
-- starts, in that order. The time profiler reads the two fields when a
-- sample interrupts the thread, so the code that it sees never belongs to
-- an older function than the continuation. See 'lowerProfileTime'.
storeProfileEntry :: FunctionName -> Maybe Var -> LowerM ()
storeProfileEntry name continuation = do
  target <- targetM
  capability <- currentCapability
  emit [] (Store Ptr (maybe (OperandLiteral LitNull) OperandVar continuation) (byteAddress capability (capabilityProfileFrameOffset target)) (wordAlignment 1))
  emit [] (Store Ptr (OperandLiteral (LitSymbol (functionSymbol name))) (byteAddress capability (capabilityProfileCodeOffset target)) (wordAlignment 1))

varBase :: GrinVar -> Text
varBase var = T.filter (\character -> character /= '"' && character /= '\\') (grinVarName var)

-- | The largest root list of a reservation in the expression.
maximumRoots :: GrinExpr -> Int
maximumRoots expression =
  case expression of
    GrinBind _ (GrinEnsureHeap _ roots) body -> max (length roots) (maximumRoots body)
    GrinBind _ (GrinAllocate _ _ roots) body -> max (length roots) (maximumRoots body)
    GrinBind _ (GrinSafepoint _ roots) body -> max (length roots) (maximumRoots body)
    GrinBind _ GrinForeignCallExpr {} body -> max (Set.size (Set.filter (isPointerRuntimeRep . grinVarRuntimeRep) (freeExprVars expression))) (maximumRoots body)
    GrinBind _ value body -> max (maximumRoots value) (maximumRoots body)
    GrinStoreRec _ body -> maximumRoots body
    GrinStoreRecUnchecked _ body -> maximumRoots body
    GrinIfWhnf _ ready slow -> max (maximumRoots ready) (maximumRoots slow)
    GrinCase _ _ alternatives -> maximum (0 : map (maximumRoots . grinAltRhs) alternatives)
    GrinEnsureHeap _ roots -> length roots
    GrinAllocate _ _ roots -> length roots
    _ -> 0

-- | A function pointer passes through a stack slot between a data pointer
-- and a code pointer, which are distinct Lir types.
needsCodeSlot :: GrinForeignTarget -> Bool
needsCodeSlot target = case target of
  GrinForeignDynamic -> True
  GrinForeignUnsafeDynamic -> True
  GrinForeignFunctionAddress _ -> True
  _ -> False

needsForeignFrame :: GrinForeignTarget -> Bool
needsForeignFrame target = case target of
  GrinForeignAddress -> False
  GrinForeignFunctionAddress _ -> False
  GrinForeignWrapper _ -> False
  _ -> True

hasForeignCall :: (GrinForeignTarget -> Bool) -> GrinExpr -> Bool
hasForeignCall predicate expression = case expression of
  GrinForeignCallExpr call _ -> predicate (grinForeignCallTarget call)
  GrinBind _ value body -> hasForeignCall predicate value || hasForeignCall predicate body
  GrinStoreRecUnchecked _ body -> hasForeignCall predicate body
  GrinIfWhnf _ ready slow -> hasForeignCall predicate ready || hasForeignCall predicate slow
  GrinCase _ _ alternatives -> any (hasForeignCall predicate . grinAltRhs) alternatives
  _ -> False

compileExpr :: FunctionCtx -> ValueEnv -> GrinExpr -> LowerM ()
compileExpr ctx env expression =
  case expression of
    GrinBind vars value body -> do
      let liveEnv = case value of
            GrinForeignCallExpr {} -> Map.restrictKeys env (freeExprVars expression)
            _ -> env
          -- A reservation ends the replay itself, after its check.
          onlyReads = case value of
            GrinConstant {} -> True
            GrinEnsureHeap {} -> True
            GrinFetch tag _ -> not (isFrameNode (ctxEnv ctx) (GrinNode tag []))
            _ -> False
      unless onlyReads endReplay
      env' <- compileBinding ctx liveEnv vars value
      compileExpr ctx env' body
    GrinStoreRec {} -> unsupported "store-rec without a heap reservation"
    GrinStoreRecUnchecked bindings body -> do
      when (any (isFrameNode (ctxEnv ctx) . snd) bindings) $
        unsupported "continuation frame in a recursive store group"
      endReplay
      allocated <- forM bindings $ \(var, node) -> do
        object <- allocateNode ctx node
        pure (var, object)
      let env' = Map.fromList allocated `Map.union` env
      forM_ allocated $ \(var, object) ->
        for_ (lookup var bindings) (initializeFields ctx env' object)
      compileExpr ctx env' body
    GrinCpsEval update _ value continuation -> do
      valueOperand <- pointerValue ctx env value
      continuationOperand <- pointerValue ctx env continuation
      eval <- requireHelper $ case update of
        EvalUpdate -> HelperEval
        EvalSingleEntry -> HelperEvalSingleEntry
      running <- runningArguments
      terminate (TailCall eval (running <> [valueOperand, continuationOperand]))
    GrinCall _ name arguments -> do
      target <- functionTarget (ctxEnv ctx) name
      parameters <- maybe (failWith (LowerMissingFunction name)) pure (Map.lookup name (envFunctionParameters (ctxEnv ctx)))
      when (length parameters /= length arguments) $ failWith (LowerUnsupportedExpression ("call arity mismatch for " <> unFunctionName name))
      values <- mapM (materialize ctx env) arguments
      operands <- zipWithM coerce parameters values
      running <- runningArguments
      terminate (TailCall target (running <> operands))
    GrinCpsPrimitiveCall runtimeRep name arguments continuation -> compileCpsPrimitive ctx env runtimeRep name arguments continuation
    GrinCpsApply _ function groups continuation -> do
      when (null groups || length groups > grinApplyGroupLimit) $
        unsupported "an application with no argument group or with too many argument groups"
      functionOperand <- pointerValue ctx env function
      continuationOperand <- pointerValue ctx env continuation
      values <- mapM (mapM (materialize ctx env)) groups
      apply <- requireHelper (HelperApply (map (map typedType) values))
      running <- runningArguments
      terminate (TailCall apply (running <> (functionOperand : continuationOperand : map typedOperand (concat values))))
    GrinContinue continuation values -> do
      continuationOperand <- pointerValue ctx env continuation
      typedValues <- mapM (materialize ctx env) values
      continueTransfer continuationOperand typedValues
    GrinCpsRaise exception continuation -> do
      exceptionOperand <- pointerValue ctx env exception
      continuationOperand <- pointerValue ctx env continuation
      capability <- currentCapability
      syncCapability capability
      resume <- callRuntime "aihc_raise" [Ptr, Ptr, Ptr] [Ptr] [capability, exceptionOperand, continuationOperand]
      resumeTransfer capability resume
    -- The exit function and the statistics read the context of the capability.
    GrinHalt _ -> do
      capability <- currentCapability
      syncCapability capability
      entry <- callRuntime "aihc_halt" [Ptr] [Code] [capability]
      terminate (TailCallIndirect entry [] (Signature [] [] AihcConvention))
    -- A POSIX process exits at once. A WASI P3 component records the
    -- status and halts, so the driver reports it when the capability returns.
    GrinExit status -> do
      statusOperand <- materialize ctx env status >>= coerce I64
      target <- targetM
      capability <- currentCapability
      syncCapability capability
      case lowerHost target of
        PosixHost -> do
          _ <- callRuntime "aihc_exit_process" [I64] [] [statusOperand]
          terminate (Trap "unreachable")
        Wasip3Host -> do
          _ <- callRuntime "aihc_set_exit_status" [Ptr, I64] [] [capability, statusOperand]
          entry <- callRuntime "aihc_halt" [Ptr] [Code] [capability]
          terminate (TailCallIndirect entry [] (Signature [] [] AihcConvention))
    GrinIfWhnf (GrinVarValue var) ready (GrinBind [frame] (GrinStoreUnchecked node) (GrinCpsEval update _ (GrinVarValue evaluated) (GrinVarValue continuation)))
      | isFrameNode (ctxEnv ctx) node,
        evaluated == var,
        continuation == frame ->
          compileForce ctx env var update node ready
    GrinIfWhnf value ready slow -> compileIfWhnf ctx env value ready slow
    GrinCase scrutinee binder alternatives -> compileCase ctx env scrutinee binder alternatives
    GrinConstant {} -> unsupported "direct-style constant return after CPS"
    GrinStore {} -> unsupported "direct-style store return after CPS"
    GrinEnsureHeap {} -> unsupported "unbound heap reservation"
    GrinAllocate {} -> unsupported "unbound dynamic allocation"
    GrinSafepoint {} -> unsupported "unbound store safepoint"
    GrinStoreUnchecked {} -> unsupported "unbound unchecked store"
    GrinUpdate {} -> unsupported "direct-style update after CPS"
    GrinUpdateBlackhole {} -> unsupported "unbound blackhole update"
    GrinEval {} -> unsupported "direct-style eval after CPS"
    GrinFetch {} -> unsupported "unbound fetch after CPS"
    GrinPrimitiveCall {} -> unsupported "unbound primitive call after CPS"
    GrinApply {} -> unsupported "direct-style apply after CPS"
    GrinForward -> unsupported "forward outside a forwarding continuation"
    GrinThrow {} -> unsupported "throw"
    GrinCatch {} -> unsupported "catch"
    GrinForeignCallExpr {} -> unsupported "unbound foreign call after CPS"
  where
    unsupported = failWith . LowerUnsupportedExpression

-- | The needs_eval byte of the info table is zero for a value. Thus the
-- ready branch needs one load and one branch after the header. The
-- info-table address removes the evaluation bits of the header, and a
-- thunk under evaluation keeps its thunk table. Thus that thunk goes to
-- the slow branch. An evaluated thunk keeps its thunk table with bit
-- zero, and field zero is its value. A claimed thunk has bit zero too,
-- and field zero names its owner: a thread record or a blackhole queue.
-- A collection makes an indirection of neither, and it follows both
-- kinds of object. The check follows the indirections and the evaluated
-- thunks of a variable to their target, so the ready branch gets the
-- value and no continuation is allocated. That test is only on the path
-- of an object that is not a value.
-- The blocks of that test are cold, so a native backend puts them after
-- the hot code and the ready branch follows the check directly.
compileIfWhnf :: FunctionCtx -> ValueEnv -> GrinValue -> GrinExpr -> GrinExpr -> LowerM ()
compileIfWhnf ctx env value ready slow = compileIfWhnfWith ctx env value ready (\env' _ -> compileExpr ctx env' slow)

-- | A WHNF test whose slow branch the given action compiles, with the
-- variable of the value bound to its target.
compileIfWhnfWith :: FunctionCtx -> ValueEnv -> GrinValue -> GrinExpr -> (ValueEnv -> (Typed, Typed) -> LowerM ()) -> LowerM ()
compileIfWhnfWith ctx env value ready slow = do
  object <- pointerValue ctx env value
  checkLabel <- freshLabel "eval_check"
  readyLabel <- freshLabel "eval_ready"
  slowLabel <- freshLabel "eval_slow"
  current <- fresh "current"
  terminate (Jump (Target checkLabel [object]))
  beginBlock checkLabel [(current, Ptr)]
  (headerWord, header) <- loadObjectHeaderDependent HeapRegion (OperandVar current)
  needsEval <- loadInfoByte "needs_eval" (typedOperand header) infoNeedsEvalByte
  notValue <- emitValue "not_value" I1 (Compare Ne I64 (typedOperand needsEval) (OperandLiteral (LitInt 0)))
  -- Only a variable can take the target. Another value keeps the slow
  -- branch for an indirection.
  let follows = case value of
        GrinVarValue var -> Just var
        _ -> Nothing
      env' = maybe env (\var -> Map.insert var (Typed (OperandVar current) Ptr) env) follows
  case follows of
    Nothing -> terminate (Branch (typedOperand notValue) (Target slowLabel []) (Target readyLabel []))
    Just _ -> do
      notValueLabel <- freshLabel "eval_not_value"
      followLabel <- freshLabel "eval_follow"
      terminate (Branch (typedOperand notValue) (Target notValueLabel []) (Target readyLabel []))
      thunkLabel <- freshLabel "eval_thunk"
      ownerLabel <- freshLabel "eval_owner"
      beginColdBlock notValueLabel []
      isIndirection <- emitValue "indirection" I1 (Compare Eq I64 (typedOperand needsEval) (OperandLiteral (LitInt needsEvalFollow)))
      terminate (Branch (typedOperand isIndirection) (Target followLabel []) (Target thunkLabel []))
      beginColdBlock thunkLabel []
      tag <- emitValue "tag" I64 (Binary And I64 (typedOperand headerWord) (OperandLiteral (LitInt 3)))
      claimed <- emitValue "claimed" I1 (Compare Eq I64 (typedOperand tag) (OperandLiteral (LitInt 1)))
      terminate (Branch (typedOperand claimed) (Target ownerLabel []) (Target slowLabel []))
      beginColdBlock ownerLabel []
      word <- loadSlotDependent HeapRegion "word" Ptr (OperandVar current) 8
      (_, wordInfo) <- loadObjectHeaderDependent HeapRegion (typedOperand word)
      wordKind <- loadInfoByte "word_kind" (typedOperand wordInfo) infoObjectKindByte
      isThread <- emitValue "thread" I1 (Compare Eq I64 (typedOperand wordKind) (OperandLiteral (LitInt runtimeObjectThread)))
      isQueue <- emitValue "queue" I1 (Compare Eq I64 (typedOperand wordKind) (OperandLiteral (LitInt runtimeObjectBlackholeQueue)))
      owned <- emitValue "owned" I1 (Binary Or I1 (typedOperand isThread) (typedOperand isQueue))
      terminate (Branch (typedOperand owned) (Target slowLabel []) (Target checkLabel [typedOperand word]))
      beginColdBlock followLabel []
      next <- loadSlotDependent HeapRegion "next" Ptr (OperandVar current) 8
      terminate (Jump (Target checkLabel [typedOperand next]))
  beginBlock readyLabel []
  branchWith (compileExpr ctx env' ready)
  beginBlock slowLabel []
  branchWith (slow env' (headerWord, header))

-- | A WHNF test whose slow branch pushes the frame of a continuation and
-- evaluates the value. The test and the ready branch stay in the function,
-- as 'compileIfWhnf' compiles them. When the value is a thunk that no
-- thread claimed and the frames fit in the stack chunk, the slow branch
-- pushes the frame of the continuation and the update frame, and enters
-- the thunk with one indirect call. Thus each force site has its own branch
-- history for the entry of the thunk. Any other value, and a push that
-- needs a new chunk, goes to a shared helper of the shape of the fields,
-- which pushes the frame and evaluates the value.
compileForce :: FunctionCtx -> ValueEnv -> GrinVar -> GrinEvalUpdate -> GrinNode -> GrinExpr -> LowerM ()
compileForce ctx env var update node ready =
  compileIfWhnfWith ctx env (GrinVarValue var) ready $ \env' (headerWord, header) -> do
    object <- pointerValue ctx env' (GrinVarValue var)
    -- The fields have the types that 'initializeFields' stores them with.
    fields <- mapM (materialize ctx env') (grinNodeFields node)
    info <- nodeInfoSymbol (ctxEnv ctx) node
    helper <- requireHelper (HelperForce update (map typedType fields))
    context <- currentContext
    let frameWords = 1 + length fields
        updateWords = case update of
          EvalUpdate -> 3
          EvalSingleEntry -> 0
        stack = contextStack context
    kind <- loadInfoByte "kind" (typedOperand header) infoObjectKindByte
    tag <- emitValue "tag" I64 (Binary And I64 (typedOperand headerWord) (OperandLiteral (LitInt 3)))
    isThunk <- emitValue "thunk" I1 (Compare Eq I64 (typedOperand kind) (OperandLiteral (LitInt (toInteger runtimeObjectThunk))))
    unclaimed <- emitValue "unclaimed" I1 (Compare Eq I64 (typedOperand tag) (OperandLiteral (LitInt 0)))
    plain <- emitValue "plain" I1 (Binary And I1 (typedOperand isThunk) (typedOperand unclaimed))
    end <- emitValue "sp" Ptr (PtrAdd stack (OperandLiteral (LitInt (8 * toInteger (frameWords + updateWords)))))
    fits <- emitValue "stack_fits" I1 (Compare LeU Ptr (typedOperand end) (contextStackLimit context))
    enter <- emitValue "enter" I1 (Binary And I1 (typedOperand plain) (typedOperand fits))
    enterLabel <- freshLabel "force_enter"
    helperLabel <- freshLabel "force_helper"
    terminate (Branch (typedOperand enter) (Target enterLabel []) (Target helperLabel []))
    beginColdBlock helperLabel []
    terminate (TailCall helper (contextOperands context <> (object : OperandLiteral (LitSymbol info) : map typedOperand fields)))
    beginBlock enterLabel []
    storeSlot StackRegion Ptr (OperandLiteral (LitSymbol info)) stack 0
    forM_ (zip [0 :: Int ..] fields) $ \(index, field) ->
      storeSlot StackRegion (typedType field) (typedOperand field) stack (toInteger (8 * (index + 1)))
    continuation <- case update of
      EvalSingleEntry -> pure stack
      EvalUpdate -> do
        let updateInfo = Symbol "aihc_lir_cps_update_info"
        requireExternData updateInfo
        frame <- typedOperand <$> emitValue "update" Ptr (PtrAdd stack (OperandLiteral (LitInt (8 * toInteger frameWords))))
        storeSlot StackRegion Ptr (OperandLiteral (LitSymbol updateInfo)) frame 0
        storeSlot StackRegion Ptr stack frame 8
        storeSlot StackRegion Ptr object frame 16
        pure frame
    entry <- loadInfoCode "entry" (typedOperand header) infoBackendEntryIndex
    terminate
      ( TailCallIndirect
          (typedOperand entry)
          (contextOperands context {contextStack = typedOperand end} <> [object, continuation])
          (Signature (contextTypes <> [Ptr, Ptr]) [] AihcConvention)
      )

-- | The order of two byte ranges of one length, as minus one, zero, or one,
-- with the sign of @memcmp@. The loop compares eight bytes at a time while
-- eight or more remain, so it never reads past either range, and then one
-- byte at a time. At the first word that differs, the lowest set bit of the
-- difference gives the first byte that differs, because the loads are
-- little endian.
compareBytesInline :: Operand -> Operand -> Operand -> LowerM Operand
compareBytesInline left right count = do
  wordLoop <- freshLabel "compare_words"
  wordBody <- freshLabel "compare_word"
  wordNext <- freshLabel "compare_word_next"
  wordDiff <- freshLabel "compare_word_diff"
  byteLoop <- freshLabel "compare_bytes"
  byteBody <- freshLabel "compare_byte"
  byteNext <- freshLabel "compare_byte_next"
  byteDiff <- freshLabel "compare_byte_diff"
  done <- freshLabel "compare_done"
  let int value = OperandLiteral (LitInt value)
      at base index = do
        address <- emitValue "address" Ptr (PtrAdd base index)
        pure (byteAddress (typedOperand address) 0)
  terminate (Jump (Target wordLoop [int 0]))
  wordIndex <- fresh "index"
  beginBlock wordLoop [(wordIndex, I64)]
  remaining <- emitValue "remaining" I64 (Binary Sub I64 count (OperandVar wordIndex))
  wide <- emitValue "wide" I1 (Compare GeU I64 (typedOperand remaining) (int 8))
  terminate (Branch (typedOperand wide) (Target wordBody []) (Target byteLoop [OperandVar wordIndex]))
  beginBlock wordBody []
  leftAddress <- at left (OperandVar wordIndex)
  rightAddress <- at right (OperandVar wordIndex)
  leftWord <- emitValue "left_word" I64 (Load I64 leftAddress (byteAlignment 1))
  rightWord <- emitValue "right_word" I64 (Load I64 rightAddress (byteAlignment 1))
  sameWord <- emitValue "same_word" I1 (Compare Eq I64 (typedOperand leftWord) (typedOperand rightWord))
  terminate (Branch (typedOperand sameWord) (Target wordNext []) (Target wordDiff []))
  beginBlock wordNext []
  nextWord <- emitValue "index" I64 (Binary Add I64 (OperandVar wordIndex) (int 8))
  terminate (Jump (Target wordLoop [typedOperand nextWord]))
  beginBlock wordDiff []
  difference <- emitValue "difference" I64 (Binary Xor I64 (typedOperand leftWord) (typedOperand rightWord))
  lowest <- emitValue "lowest" I64 (Unary Ctz I64 (typedOperand difference))
  shift <- emitValue "shift" I64 (Binary And I64 (typedOperand lowest) (int (-8)))
  leftShifted <- emitValue "left_shifted" I64 (Binary ShrU I64 (typedOperand leftWord) (typedOperand shift))
  rightShifted <- emitValue "right_shifted" I64 (Binary ShrU I64 (typedOperand rightWord) (typedOperand shift))
  leftDiffByte <- emitValue "left_byte" I64 (Binary And I64 (typedOperand leftShifted) (int 255))
  rightDiffByte <- emitValue "right_byte" I64 (Binary And I64 (typedOperand rightShifted) (int 255))
  wordLess <- emitValue "less" I1 (Compare LtU I64 (typedOperand leftDiffByte) (typedOperand rightDiffByte))
  wordOrder <- emitValue "order" I64 (Select I64 (typedOperand wordLess) (int (-1)) (int 1))
  terminate (Jump (Target done [typedOperand wordOrder]))
  byteIndex <- fresh "index"
  beginBlock byteLoop [(byteIndex, I64)]
  finished <- emitValue "finished" I1 (Compare Eq I64 (OperandVar byteIndex) count)
  terminate (Branch (typedOperand finished) (Target done [int 0]) (Target byteBody []))
  beginBlock byteBody []
  leftByteAddress <- at left (OperandVar byteIndex)
  rightByteAddress <- at right (OperandVar byteIndex)
  leftByte <- emitValue "left_byte" I8 (Load I8 leftByteAddress (byteAlignment 1))
  rightByte <- emitValue "right_byte" I8 (Load I8 rightByteAddress (byteAlignment 1))
  sameByte <- emitValue "same_byte" I1 (Compare Eq I8 (typedOperand leftByte) (typedOperand rightByte))
  terminate (Branch (typedOperand sameByte) (Target byteNext []) (Target byteDiff []))
  beginBlock byteNext []
  nextByte <- emitValue "index" I64 (Binary Add I64 (OperandVar byteIndex) (int 1))
  terminate (Jump (Target byteLoop [typedOperand nextByte]))
  beginBlock byteDiff []
  byteLess <- emitValue "less" I1 (Compare LtU I8 (typedOperand leftByte) (typedOperand rightByte))
  byteOrder <- emitValue "order" I64 (Select I64 (typedOperand byteLess) (int (-1)) (int 1))
  terminate (Jump (Target done [typedOperand byteOrder]))
  order <- fresh "order"
  beginBlock done [(order, I64)]
  pure (OperandVar order)

functionTarget :: LowerEnv -> FunctionName -> LowerM Symbol
functionTarget env name = maybe (failWith (LowerMissingFunction name)) pure (Map.lookup name (envFunctionSymbols env))

-- | The single result of one extern C call.
callRuntime :: Text -> [Type] -> [Type] -> [Operand] -> LowerM Operand
callRuntime name parameters results arguments = do
  let symbol = Symbol name
  requireExtern symbol parameters results
  case results of
    [ty] -> typedOperand <$> emitValue "result" ty (Call symbol arguments)
    _ -> do
      vars <- mapM (const (fresh "result")) results
      emit vars (Call symbol arguments)
      pure (OperandLiteral LitNull)

-- | Give values to a continuation frame. The usual frame has an entry and is
-- in the chunk of the stack pointer, and the code enters it here with one
-- indirect call: this return site then has its own branch history. A
-- forward frame, or a frame in a lower chunk, goes to the continue helper in
-- a cold block.
continueTransfer :: Operand -> [Typed] -> LowerM ()
continueTransfer continuation values = do
  let shape = map typedType values
  continue <- requireHelper (HelperContinue shape)
  context <- currentContext
  header <- typedOperand <$> loadObjectInfo StackRegion continuation
  entry <- loadInfoCode "entry" header infoBackendEntryIndex
  stackLimit <- chunkEnd continuation
  slow <- freshLabel "continue_slow"
  checkChunk <- freshLabel "continue_chunk"
  enter <- freshLabel "continue_enter"
  isForward <- emitValue "forward" I1 (Compare Eq Code (typedOperand entry) (OperandLiteral LitNull))
  terminate (Branch (typedOperand isForward) (Target slow []) (Target checkChunk []))
  beginBlock checkChunk []
  sameChunk <- emitValue "same_chunk" I1 (Compare Eq Ptr stackLimit (contextStackLimit context))
  terminate (Branch (typedOperand sameChunk) (Target enter []) (Target slow []))
  beginColdBlock slow []
  terminate (TailCall continue (contextOperands context <> (continuation : map typedOperand values)))
  beginBlock enter []
  -- Entering a frame pops it and every frame above it: the frame is the
  -- new stack pointer.
  let entered = context {contextStack = continuation}
  terminate
    ( TailCallIndirect
        (typedOperand entry)
        (contextOperands entered <> (continuation : OperandLiteral LitNull : map typedOperand values))
        (Signature (contextTypes <> (Ptr : Ptr : shape)) [] AihcConvention)
    )

-- | Give control to the scheduler. The C call that selected the resumption
-- had the context of the capability, and the resumption loads the context of
-- the thread it runs.
resumeTransfer :: Operand -> Operand -> LowerM ()
resumeTransfer capability resume = do
  helper <- requireHelper HelperResume
  terminate (TailCall helper [capability, resume])

compileCpsPrimitive :: FunctionCtx -> ValueEnv -> GrinRep -> Text -> [GrinValue] -> GrinValue -> LowerM ()
compileCpsPrimitive ctx env runtimeRep name arguments continuation =
  case nativeCpsPrimitiveCall name of
    Just runtimeCall | nativeCpsCallOperandCount runtimeCall == length arguments -> do
      continuationOperand <- pointerValue ctx env continuation
      values <- mapM (materialize ctx env) arguments
      let (parameterTypes, resultType) = cpsCallSignature name
      operands <- zipWithM coerce parameterTypes values
      capability <- currentCapability
      let symbol = nativeCpsCallSymbol runtimeCall
          callArguments = capability : operands <> [continuationOperand | nativeCpsCallPassContinuation runtimeCall]
          callParameters = Ptr : parameterTypes <> [Ptr | nativeCpsCallPassContinuation runtimeCall]
      -- A CPS runtime call may collect, and it does so without the calling
      -- function's static reference table: the only code that runs after the
      -- call is the transfer below, which passes heap objects and touches no
      -- static object of this function. Keep it that way.
      syncCapability capability
      result <- callRuntime symbol callParameters [resultType] callArguments
      case nativeCpsCallTransfer runtimeCall of
        NativeCpsEnterContinuation -> do
          reloadHeap capability
          let resultRep = case runtimeRepComponents runtimeRep of
                rep : _ -> rep
                [] -> IntRep
          value <- coerceTo (repType resultRep) (Typed result resultType)
          continueTransfer continuationOperand [value]
        NativeCpsResumeScheduler -> resumeTransfer capability result
    _ -> failWith (LowerUnsupportedExpression ("CPS primitive call " <> name))

-- | The C parameter types after the capability, and the C result type, of each
-- CPS runtime call.
cpsCallSignature :: Text -> ([Type], Type)
cpsCallSignature name =
  case name of
    "fork#" -> ([Ptr], I64)
    "newMVar#" -> ([], Ptr)
    "putMVar#" -> ([Ptr, I64], Ptr)
    "readMVar#" -> ([Ptr], Ptr)
    "takeMVar#" -> ([Ptr], Ptr)
    "yield#" -> ([], Ptr)
    "noDuplicate#" -> ([], Ptr)
    "aihcControl0#" -> ([Ptr, Ptr], Ptr)
    "aihcResume#" -> ([Ptr, Ptr], Ptr)
    _ -> ([Ptr], Ptr)

compileBinding :: FunctionCtx -> ValueEnv -> [GrinVar] -> GrinExpr -> LowerM ValueEnv
compileBinding ctx env vars expression =
  case expression of
    GrinConstant values
      | length vars == length values -> do
          bound <- forM (zip vars values) $ \(var, value) -> do
            typed <- materialize ctx env value >>= coerceTo (repType (grinVarRuntimeRep var))
            pure (var, typed)
          pure (Map.fromList bound `Map.union` env)
    GrinStore {} -> failWith (LowerUnsupportedExpression "store without a heap reservation")
    GrinStoreUnchecked node -> do
      object <- allocateNode ctx node
      initializeFields ctx env object node
      bindResults [object]
    GrinEnsureHeap requiredWords roots
      | length vars == length roots -> do
          words' <- materialize ctx env requiredWords >>= coerce I64
          rootOperands <- mapM (pointerValue ctx env) roots
          array <- rootArray roots
          reserveHeap ctx env vars requiredWords words' roots rootOperands array
      | otherwise -> failWith (LowerUnsupportedExpression "heap reservation result arity")
    GrinAllocate requiredWords pinned roots -> case vars of
      memory : relocated | length relocated == length roots -> do
        words' <- materialize ctx env requiredWords >>= coerce I64
        pinned' <- materialize ctx env pinned >>= coerce I64
        rootOperands <- mapM (pointerValue ctx env) roots
        array <- rootArray roots
        forM_ (zip [0 :: Int ..] rootOperands) $ \(index, root) ->
          storeSlot AnyRegion Ptr root array (toInteger (8 * index))
        context <- currentContext
        capability <- capabilityOperand context
        storeContext capability context
        address <- callRuntime "aihc_heap_allocate" [Ptr, I64, I64, I64, Ptr, Ptr] [Ptr] [capability, words', pinned', OperandLiteral (LitInt (toInteger (length roots))), array, ctxSrt ctx]
        moved <- forM (zip [0 :: Int ..] relocated) $ \(index, var) ->
          (var,) <$> loadSlot AnyRegion (varBase var) Ptr array (toInteger (8 * index))
        collected <- loadHeapContext capability context
        setContext collected
        pure (Map.fromList ((memory, Typed address Ptr) : moved) `Map.union` env)
      _ -> failWith (LowerUnsupportedExpression "dynamic allocation result arity")
    GrinSafepoint top roots
      | length vars == length roots -> beginSafepoint ctx env vars top roots
      | otherwise -> failWith (LowerUnsupportedExpression "store safepoint result arity")
    GrinUpdate pointer value -> update "aihc_update" pointer value
    GrinUpdateBlackhole pointer value -> update "aihc_update_blackhole" pointer value
    GrinPrimitiveCall runtimeRep name arguments -> compilePrimitive ctx env vars runtimeRep name arguments
    GrinForeignCallExpr foreignCall arguments -> do
      (results, relocated) <- protectedForeignCall ctx env foreignCall arguments
      bindVars relocated vars results
    GrinFetch tag value -> do
      object <- pointerValue ctx env value
      -- The fields start where 'initializeFields' writes them.
      let payloadShift = nodePayloadShift (GrinNode tag [])
      fields <- forM (zip [0 :: Int ..] vars) $ \(index, var) -> do
        typed <- loadSlot (nodeRegion (ctxEnv ctx) (GrinNode tag [])) (varBase var) (repType (grinVarRuntimeRep var)) object (toInteger (8 * (index + 1 + payloadShift)))
        pure (var, typed)
      pure (Map.fromList fields `Map.union` env)
    _ -> failWith (LowerUnsupportedExpression "non-direct expression remained in a CPS bind")
  where
    -- An update neither allocates nor reads the stack pointer, so the
    -- capability does not need the context for it. The update gives null
    -- when it is an escape whose promotion needs a collection.
    update symbol pointer value = do
      capability <- currentCapability
      pointerOperand <- pointerValue ctx env pointer
      valueTyped <- materialize ctx env value
      valueOperand <- coerce Ptr valueTyped
      let arguments = [capability, pointerOperand, valueOperand]
      storePauseTop capability
      stored <- callRuntime symbol (map (const Ptr) arguments) [Ptr] arguments
      failed <- emitValue "update_escape" I1 (Compare Eq Ptr stored (OperandLiteral LitNull))
      escapeCollectWhen ctx failed
      bindResults [valueTyped]
    rootArray roots = case (roots, ctxRoots ctx) of
      ([], _) -> pure (OperandLiteral LitNull)
      (_, Just array) -> pure array
      _ -> failWith (LowerUnsupportedExpression "internal: roots without a root array")
    bindResults = bindVars env vars

-- | Take the words of a reservation from the current space. The fast path is
-- the compare of the heap pointer against the heap limit and the branch
-- alone: it keeps every root and the context in the registers they already
-- sit in, so a safepoint that does not collect costs the same whatever is
-- live across it. Only the slow path spills the roots to the root array,
-- gives the capability the context, calls the collector, and reloads the roots
-- it moved and the heap context. The two paths meet at a block whose
-- parameters carry the roots and the heap context, which are the names the
-- body uses.
reserveHeap ::
  FunctionCtx ->
  ValueEnv ->
  [GrinVar] ->
  GrinValue ->
  Operand ->
  [GrinValue] ->
  [Operand] ->
  Operand ->
  LowerM ValueEnv
reserveHeap ctx env vars requiredWords words' roots rootOperands array = do
  context <- currentContext
  fits <- case requiredWords of
    -- A reservation of zero words is the safepoint before a frame push. It
    -- fails when a stack growth put the heap limit below the heap pointer.
    GrinLitValue (GrinLitInt _ 0) ->
      emitValue "heap_fits" I1 (Compare LeU Ptr (contextHeap context) (contextHeapLimit context))
    -- A fixed size is small, so the end of the reservation cannot wrap, and
    -- the reservation fits when that end is not above the heap limit.
    GrinLitValue (GrinLitInt _ requested) -> do
      end <- emitValue "heap_end" Ptr (PtrAdd (contextHeap context) (OperandLiteral (LitInt (8 * requested))))
      emitValue "heap_fits" I1 (Compare LeU Ptr (typedOperand end) (contextHeapLimit context))
    _ -> do
      nextWord <- emitValue "heap_word" I64 (PtrToInt (contextHeap context))
      limitWord <- emitValue "heap_end_word" I64 (PtrToInt (contextHeapLimit context))
      -- A large allocation lowers the heap limit, and the stores of an
      -- earlier reservation can then pass it, so the room is valid only
      -- when the limit is not below the heap pointer.
      below <- emitValue "heap_below_limit" I1 (Compare LeU I64 (typedOperand nextWord) (typedOperand limitWord))
      room <- emitValue "heap_room" I64 (Binary Sub I64 (typedOperand limitWord) (typedOperand nextWord))
      -- A dynamic size brings the room down to words rather than the words up
      -- to bytes: a reservation the address space cannot hold then fails the
      -- compare instead of wrapping past it into the unchecked store behind.
      roomWords <- emitValue "heap_room_words" I64 (Binary ShrU I64 (typedOperand room) (OperandLiteral (LitInt 3)))
      enough <- emitValue "heap_enough" I1 (Compare GeU I64 (typedOperand roomWords) words')
      emitValue "heap_fits" I1 (Binary And I1 (typedOperand below) (typedOperand enough))
  collect <- freshLabel "gc_collect"
  reserved <- freshLabel "gc_reserved"
  parameters <- forM vars $ \var -> do
    parameter <- fresh (varBase var)
    pure (var, parameter)
  heap <- fresh "hp"
  heapLimit <- fresh "hp_limit"
  let heapArguments current = [contextHeap current, contextHeapLimit current]
  terminate (Branch (typedOperand fits) (Target reserved (rootOperands <> heapArguments context)) (Target collect []))
  beginColdBlock collect []
  replay <- gets stateReplay
  endReplay
  case replay of
    -- The code before the reservation only read values, so the function
    -- can run again from its entry after the collection. See 'Replay'.
    Just (Replay code arguments repush) -> do
      helper <- requireHelper (HelperCollectReplay [(ty, root) | (_, ty, root) <- arguments])
      replayContext <- case repush of
        Nothing -> pure context
        Just (frame, bytes) -> do
          stack <- emitValue "sp" Ptr (PtrAdd frame (OperandLiteral (LitInt bytes)))
          pure context {contextStack = typedOperand stack}
      terminate (TailCall helper (contextOperands replayContext <> [operand | (operand, _, _) <- arguments] <> [OperandLiteral (LitSymbol code), words', ctxSrt ctx]))
    Nothing -> do
      forM_ (zip [0 :: Int ..] rootOperands) $ \(index, root) ->
        storeSlot AnyRegion Ptr root array (toInteger (8 * index))
      -- The compare above is the reservation, so this is the collector rather
      -- than a second reservation that would repeat it. The function's table
      -- travels as an argument: this is the only place a collection runs on
      -- behalf of a running compiled function.
      capability <- capabilityOperand context
      storeContext capability context
      _ <- callRuntime "aihc_heap_collect" [Ptr, I64, I64, Ptr, Ptr] [] [capability, words', OperandLiteral (LitInt (toInteger (length roots))), array, ctxSrt ctx]
      relocated <- forM (zip [0 :: Int ..] vars) $ \(index, var) ->
        loadSlot AnyRegion (varBase var) Ptr array (toInteger (8 * index))
      collected <- loadHeapContext capability context
      terminate (Jump (Target reserved (map typedOperand relocated <> heapArguments collected)))
  beginBlock reserved ([(parameter, Ptr) | (_, parameter) <- parameters] <> [(heap, Ptr), (heapLimit, Ptr)])
  setContext context {contextHeap = OperandVar heap, contextHeapLimit = OperandVar heapLimit}
  pure (Map.fromList [(var, Typed (OperandVar parameter) Ptr) | (var, parameter) <- parameters] `Map.union` env)

-- | Bind the result variables of a direct expression, converting each value
-- to the representation of its variable.
bindVars :: ValueEnv -> [GrinVar] -> [Typed] -> LowerM ValueEnv
bindVars env vars values
  | length vars /= length values = failWith (LowerUnsupportedExpression "direct expression result arity")
  | otherwise = do
      bound <- forM (zip vars values) $ \(var, value) -> do
        typed <- coerceTo (repType (grinVarRuntimeRep var)) value
        pure (var, typed)
      pure (Map.fromList bound `Map.union` env)

-- | One object of a reservation the code before it has already made, or a
-- continuation frame, which goes on the stack of the thread and needs no
-- reservation.
allocateNode :: FunctionCtx -> GrinNode -> LowerM Typed
allocateNode ctx node = do
  info <- nodeInfoSymbol (ctxEnv ctx) node
  let region = nodeRegion (ctxEnv ctx) node
  object <-
    if isFrameNode (ctxEnv ctx) node
      then pushFrame (nodeWords node)
      else do
        profiling <- gets stateProfileAllocations
        when profiling (countAllocation info (grinNodeTag node) (nodeWords node))
        bumpAllocate (nodeWords node)
  storeSlot region Ptr (OperandLiteral (LitSymbol info)) (typedOperand object) 0
  -- The shared info table of an unsaturated constructor does not say how wide
  -- this stage is, so the object records the count itself.
  when (isPartialConstructorNode node) $
    storeSlot region I64 (OperandLiteral (LitInt (toInteger (length (grinNodeFields node))))) (typedOperand object) 8
  pure object

-- | Add one object of the given words to the allocation counter of an info
-- table. The counters are pairs of words, the objects and the words, in the
-- order the info tables first occur. See 'lowerProfileAllocations'.
countAllocation :: Symbol -> GrinNodeTag -> Int -> LowerM ()
countAllocation info tag words' = do
  sites <- gets stateProfileSites
  index <- case Map.lookup info sites of
    Just (index, _) -> pure index
    Nothing -> do
      let index = Map.size sites
      modify' (\state -> state {stateProfileSites = Map.insert info (index, profileName tag) sites})
      pure index
  let counters = OperandLiteral (LitSymbol allocationCountsSymbol)
      offset = toInteger (16 * index)
  objects <- loadSlot AnyRegion "objects" I64 counters offset
  objects' <- emitValue "objects" I64 (Binary Add I64 (typedOperand objects) (OperandLiteral (LitInt 1)))
  storeSlot AnyRegion I64 (typedOperand objects') counters offset
  allocated <- loadSlot AnyRegion "words" I64 counters (offset + 8)
  allocated' <- emitValue "words" I64 (Binary Add I64 (typedOperand allocated) (OperandLiteral (LitInt (toInteger words'))))
  storeSlot AnyRegion I64 (typedOperand allocated') counters (offset + 8)
  where
    -- A linked name separates its package, module and name with control
    -- characters, which a C string cannot hold.
    profileName current =
      T.map (\c -> if c < ' ' then ':' else c) $
        case current of
          GrinConstructor name remaining -> "C " <> name <> (if remaining == 0 then "" else "/" <> T.pack (show remaining))
          GrinClosure name layouts -> "P " <> unFunctionName name <> "/" <> T.pack (show (length layouts))
          GrinThunk name -> "F " <> unFunctionName name

-- | The symbols of the allocation profile: the counters, the names, and
-- the number of counters.
allocationCountsSymbol, allocationNamesSymbol, allocationSizeSymbol :: Symbol
allocationCountsSymbol = Symbol "aihc_allocation_profile_counts"
allocationNamesSymbol = Symbol "aihc_allocation_profile_names"
allocationSizeSymbol = Symbol "aihc_allocation_profile_size"

-- | The data of the allocation profile of a unit, after its code: the
-- zeroed counters, the name of each, and their number. A unit with no
-- allocation still defines all three, because the entry names them.
lowerAllocationProfile :: LowerM ()
lowerAllocationProfile = do
  target <- targetM
  sites <- gets (List.sortOn fst . Map.elems . stateProfileSites)
  names <- forM sites $ \(index, name) -> do
    let symbol = Symbol ("aihc_allocation_profile_name_" <> T.pack (show index))
    emitItem (ItemData (DataItem symbol Internal False 1 [DataBytes (TE.encodeUtf8 name), DataInt I8 0]))
    pure symbol
  emitItem (ItemData (DataItem allocationCountsSymbol Export True 8 (if null sites then [DataInt I64 0] else concat [[DataInt I64 0, DataInt I64 0] | _ <- sites])))
  emitItem (ItemData (DataItem allocationNamesSymbol Export False (toInteger (lowerWordSize target)) (if null names then [DataNull] else [DataSymbol symbol 0 | symbol <- names])))
  emitItem (ItemData (DataItem allocationSizeSymbol Export False 8 [DataInt I64 (toInteger (length sites))]))

-- | The symbols of the time profile tables: the code of each function, its
-- name, and their number, then the info tables of the functions and their
-- number. The runtime names a sampled code from the first table. It reads a
-- frame only when its info table is in the second table.
timeProfileCodesSymbol, timeProfileNamesSymbol, timeProfileCodeCountSymbol, timeProfileInfosSymbol, timeProfileInfoCountSymbol :: Symbol
timeProfileCodesSymbol = Symbol "aihc_time_profile_codes"
timeProfileNamesSymbol = Symbol "aihc_time_profile_names"
timeProfileCodeCountSymbol = Symbol "aihc_time_profile_code_count"
timeProfileInfosSymbol = Symbol "aihc_time_profile_infos"
timeProfileInfoCountSymbol = Symbol "aihc_time_profile_info_count"

-- | The time profile tables of a unit. See 'lowerProfileTime'.
lowerTimeProfile :: LowerEnv -> [GrinFunction] -> LowerM ()
lowerTimeProfile env functions = do
  target <- targetM
  let names = [grinFunctionName function | function <- functions, grinFunctionName function `Set.notMember` envForwardingFunctions env]
      infos = [infoSymbol info | info <- envInfos env, DataCode (Just _) <- [infoIdentity info]]
      word = toInteger (lowerWordSize target)
  nameSymbols <- forM (zip [0 :: Int ..] names) $ \(index, name) -> do
    let symbol = Symbol ("aihc_time_profile_name_" <> T.pack (show index))
    emitItem (ItemData (DataItem symbol Internal False 1 [DataBytes (TE.encodeUtf8 (profileFunctionName name)), DataInt I8 0]))
    pure symbol
  emitItem (ItemData (DataItem timeProfileCodesSymbol Export False word (if null names then [DataNull] else [DataCode (Just (functionSymbol name)) | name <- names])))
  emitItem (ItemData (DataItem timeProfileNamesSymbol Export False word (if null nameSymbols then [DataNull] else [DataSymbol symbol 0 | symbol <- nameSymbols])))
  emitItem (ItemData (DataItem timeProfileCodeCountSymbol Export False 8 [DataInt I64 (toInteger (length names))]))
  emitItem (ItemData (DataItem timeProfileInfosSymbol Export False word (if null infos then [DataNull] else [DataSymbol symbol 0 | symbol <- infos])))
  emitItem (ItemData (DataItem timeProfileInfoCountSymbol Export False 8 [DataInt I64 (toInteger (length infos))]))

-- | The name of a function in a profile. A linked name separates its
-- package, module and name with control characters, which a C string
-- cannot hold.
profileFunctionName :: FunctionName -> Text
profileFunctionName = T.map (\c -> if c < ' ' then ':' else c) . unFunctionName

-- | Take the given words from heap that a reservation has already made: the
-- object starts at the heap pointer, and the heap pointer advances past it.
-- The caller writes the header and every field, so nothing zeroes the slots.
bumpAllocate :: Int -> LowerM Typed
bumpAllocate words' = do
  context <- currentContext
  next <- emitValue "hp" Ptr (PtrAdd (contextHeap context) (OperandLiteral (LitInt (8 * toInteger words'))))
  setContext context {contextHeap = typedOperand next}
  pure (Typed (contextHeap context) Ptr)

-- | Whether a node is a continuation frame: a closure of a function that the
-- CPS pass made a continuation.
isFrameNode :: LowerEnv -> GrinNode -> Bool
isFrameNode env node =
  case grinNodeTag node of
    GrinClosure name _ -> name `Set.member` envContinuationFunctions env
    _ -> False

-- | The region of the slots of a node: a continuation frame is on the
-- stack of the thread, and every other node is a heap object.
nodeRegion :: LowerEnv -> GrinNode -> Region
nodeRegion env node = if isFrameNode env node then StackRegion else HeapRegion

-- | Push the given words on the stack of the running thread and give back
-- the frame. The frame fits in the current chunk when the new stack pointer
-- is not above the stack limit. Otherwise the runtime continues the stack in
-- the next chunk, and the stack limit becomes the end of that chunk. Neither
-- path collects, so no root moves, and neither path reads the capability copy
-- of the context. The runtime charges a new young chunk to the nursery, so
-- the heap limit can go down on the second path. The caller writes the
-- header and every field.
pushFrame :: Int -> LowerM Typed
pushFrame words' = do
  context <- currentContext
  (frame, pushed) <- pushStackFrame context words'
  setContext pushed
  pure (Typed frame Ptr)

-- | Push a frame on a stack with the stack pointer, the stack limit, and the
-- heap limit of the given context. Give back the frame and the context after
-- the push.
pushStackFrame :: Context -> Int -> LowerM (Operand, Context)
pushStackFrame context words' = do
  target <- targetM
  let bytes = 8 * toInteger words'
      stack = contextStack context
      stackLimit = contextStackLimit context
      heapLimit = contextHeapLimit context
  end <- emitValue "sp" Ptr (PtrAdd stack (OperandLiteral (LitInt bytes)))
  fits <- emitValue "stack_fits" I1 (Compare LeU Ptr (typedOperand end) stackLimit)
  growLabel <- freshLabel "stack_grow"
  pushedLabel <- freshLabel "stack_pushed"
  terminate (Branch (typedOperand fits) (Target pushedLabel [stack, typedOperand end, stackLimit, heapLimit]) (Target growLabel []))
  beginColdBlock growLabel []
  capability <- capabilityOperand context
  grown <- callRuntime "aihc_stack_grow" [Ptr, Ptr, I64] [Ptr] [capability, stack, OperandLiteral (LitInt (toInteger words'))]
  grownEnd <- emitValue "sp" Ptr (PtrAdd grown (OperandLiteral (LitInt bytes)))
  -- The runtime gives the first frame of a chunk, so the rest of the chunk
  -- follows the frame.
  grownLimit <- emitValue "sp_limit" Ptr (PtrAdd grown (OperandLiteral (LitInt (stackChunkBytes - stackChunkHeaderBytes))))
  -- Compiled code never stores the heap limit, so the capability copy is the
  -- current limit.
  grownHeapLimit <- emitValue "hp_limit" Ptr (Load Ptr (byteAddress capability (capabilityHeapLimitOffset target)) (wordAlignment 1))
  terminate (Jump (Target pushedLabel [grown, typedOperand grownEnd, typedOperand grownLimit, typedOperand grownHeapLimit]))
  pushed <- fresh "frame"
  pushedStack <- fresh "sp"
  pushedLimit <- fresh "sp_limit"
  pushedHeapLimit <- fresh "hp_limit"
  beginBlock pushedLabel [(pushed, Ptr), (pushedStack, Ptr), (pushedLimit, Ptr), (pushedHeapLimit, Ptr)]
  pure
    ( OperandVar pushed,
      context
        { contextStack = OperandVar pushedStack,
          contextStackLimit = OperandVar pushedLimit,
          contextHeapLimit = OperandVar pushedHeapLimit
        }
    )

-- | The words before the first field of a node. An unsaturated constructor
-- spends field zero on its applied count. A thunk spends it on its owner
-- word. The payload of each of them starts one slot later.
nodePayloadShift :: GrinNode -> Int
nodePayloadShift node
  | isPartialConstructorNode node = 1
  | GrinThunk {} <- grinNodeTag node = thunkOwnerWords
  | otherwise = 0

-- | The owner word of a thunk is field zero. The runtime keeps the state of
-- an evaluation there. A new thunk does not initialize it, so its info table
-- marks it as a word that is not a pointer. See "Thunk layout" in
-- docs/multi-threaded-rts-design.md.
thunkOwnerWords :: Int
thunkOwnerWords = 1

-- | An unsaturated constructor spends field zero on its applied count, so its
-- payload starts one slot later than every other object's.
isPartialConstructorNode :: GrinNode -> Bool
isPartialConstructorNode node =
  case grinNodeTag node of
    GrinConstructor _ remaining -> constructorStage remaining == PartialConstructor
    _ -> False

initializeFields :: FunctionCtx -> ValueEnv -> Typed -> GrinNode -> LowerM ()
initializeFields ctx env object node =
  forM_ (zip [0 :: Int ..] (grinNodeFields node)) $ \(index, field) -> do
    typed <- materialize ctx env field
    storeSlot (nodeRegion (ctxEnv ctx) node) (typedType typed) (typedOperand typed) (typedOperand object) (toInteger (8 * (index + 1 + payloadShift)))
  where
    payloadShift = nodePayloadShift node

-- | A GRIN value as a typed Lir operand.
materialize :: FunctionCtx -> ValueEnv -> GrinValue -> LowerM Typed
materialize ctx env value =
  case value of
    GrinVarValue var ->
      case Map.lookup var env of
        Just typed -> pure typed
        Nothing -> globalReference (grinVarName var)
    GrinGlobalValue name -> globalReference name
    GrinLitValue literal ->
      case literal of
        GrinLitAddr bytes -> do
          symbol <- addrLiteralSymbol (ctxEnv ctx) bytes
          pure (Typed (OperandLiteral (LitSymbol symbol)) Ptr)
        _ -> case normalizedLiteralInteger literal of
          Just integer -> pure (Typed (OperandLiteral (LitInt integer)) I64)
          Nothing -> failWith (LowerUnsupportedValue "string literal")
  where
    globalReference name = do
      let symbol = globalSymbol name
      requireExternData symbol
      pure (Typed (OperandLiteral (LitSymbol symbol)) Ptr)

pointerValue :: FunctionCtx -> ValueEnv -> GrinValue -> LowerM Operand
pointerValue ctx env value = materialize ctx env value >>= coerce Ptr

-- | A foreign call gives one value, or none for a C procedure.
compileForeignCall :: FunctionCtx -> ValueEnv -> GrinForeignCall -> [GrinValue] -> LowerM [Typed]
compileForeignCall ctx env foreignCall arguments =
  case grinForeignCallTarget foreignCall of
    -- An address import materializes the symbol address instead of calling it.
    GrinForeignAddress
      | null arguments -> do
          let symbol = Symbol (grinForeignCallSymbol foreignCall)
          requireExternData symbol
          pure [Typed (OperandLiteral (LitSymbol symbol)) Ptr]
      | otherwise -> failWith (LowerUnsupportedExpression "address foreign import with arguments")
    -- The function is declared with its own signature, because a target
    -- such as wasm tells a function from data when it links.
    GrinForeignFunctionAddress pointee
      | null arguments -> do
          target <- targetM
          let symbol = Symbol (grinForeignCallSymbol foreignCall)
              (parameters, results) = runtimeCallSignatureFor target False pointee
          requireExtern symbol parameters results
          slot <- maybe (failWith (LowerUnsupportedExpression "function address has no code slot")) pure (ctxCodeSlot ctx)
          emit [] (Store Code (OperandLiteral (LitSymbol symbol)) (byteAddress slot 0) (wordAlignment 1))
          address <- emitValue "function_address" Ptr (Load Ptr (byteAddress slot 0) (wordAlignment 1))
          pure [Typed (typedOperand address) Ptr]
      | otherwise -> failWith (LowerUnsupportedExpression "address foreign import with arguments")
    GrinForeignFunction -> compileCCall ctx env False foreignCall arguments
    GrinForeignUnsafeFunction -> compileCCall ctx env False foreignCall arguments
    GrinForeignDynamic -> compileCCall ctx env False foreignCall arguments
    GrinForeignUnsafeDynamic -> compileCCall ctx env False foreignCall arguments
    -- A callback registration neither allocates nor reads the stack
    -- pointer.
    GrinForeignWrapper _ -> case arguments of
      [closure] -> do
        operand <- pointerValue ctx env closure
        capability <- currentCapability
        result <-
          callRuntime
            "aihc_callback_create"
            [Ptr, Ptr, Ptr, I64]
            [Ptr]
            [capability, operand, OperandLiteral (LitSymbol (callbackPoolSymbol foreignCall)), OperandLiteral (LitInt callbackPoolSize)]
        pure [Typed result Ptr]
      _ -> failWith (LowerUnsupportedExpression "callback creation requires one closure")

-- | Spill live heap pointers while C can call Haskell and collect.
protectedForeignCall :: FunctionCtx -> ValueEnv -> GrinForeignCall -> [GrinValue] -> LowerM ([Typed], ValueEnv)
protectedForeignCall ctx env call arguments = case grinForeignCallTarget call of
  GrinForeignAddress -> (,env) <$> compileForeignCall ctx env call arguments
  GrinForeignFunctionAddress _ -> (,env) <$> compileForeignCall ctx env call arguments
  GrinForeignWrapper _ -> (,env) <$> compileForeignCall ctx env call arguments
  target -> do
    let roots = [(var, value) | (var, value) <- Map.toAscList env, isPointerRuntimeRep (grinVarRuntimeRep var)]
        allowed = target `notElem` [GrinForeignUnsafeFunction, GrinForeignUnsafeDynamic]
    array <- case (roots, ctxRoots ctx) of
      ([], _) -> pure (OperandLiteral LitNull)
      (_, Just array) -> pure array
      _ -> failWith (LowerUnsupportedExpression "foreign call has no root array")
    mapM_ (\(index, (_, value)) -> storeSlot AnyRegion Ptr (typedOperand value) array (8 * index)) (zip [0 ..] roots)
    frame <- maybe (failWith (LowerUnsupportedExpression "foreign call has no stack frame")) pure (ctxForeignFrame ctx)
    -- A callback runs Haskell code on the stack of this thread, above the
    -- stack pointer, and it can allocate and collect. It gives the stack
    -- back as it found it, so only the heap context comes back.
    capability <- currentCapability
    syncCapability capability
    _ <-
      callRuntime
        "aihc_foreign_enter"
        [Ptr, Ptr, Ptr, I64, Ptr, I64]
        []
        [capability, frame, array, OperandLiteral (LitInt (toInteger (length roots))), ctxSrt ctx, OperandLiteral (LitInt (if allowed then 1 else 0))]
    results <- compileForeignCall ctx env call arguments
    _ <- callRuntime "aihc_foreign_leave" [Ptr, Ptr] [] [capability, frame]
    reloadHeap capability
    relocated <- mapM (\(index, (var, _)) -> (var,) <$> loadSlot AnyRegion "foreign_root" Ptr array (8 * index)) (zip [0 ..] roots)
    pure (results, Map.fromList relocated `Map.union` env)

-- | Code and data pointers have the same ABI width, but distinct Lir types.
addressAsCode :: FunctionCtx -> Operand -> LowerM Operand
addressAsCode ctx pointer = do
  slot <- maybe (failWith (LowerUnsupportedExpression "dynamic call has no code slot")) pure (ctxCodeSlot ctx)
  emit [] (Store Ptr pointer (byteAddress slot 0) (wordAlignment 1))
  typedOperand <$> emitValue "function_code" Code (Load Code (byteAddress slot 0) (wordAlignment 1))

callbackPoolSize :: Integer
callbackPoolSize = 64

callbackPoolSymbol :: GrinForeignCall -> Symbol
callbackPoolSymbol call = Symbol ("aihc_callbacks_" <> renderLinkedFunctionSymbol (grinForeignCallName call))

-- | Each entry has a distinct address and a slot with a protected closure.
lowerCallbackPool :: GrinForeignCall -> GrinForeignSignature -> LowerM ()
lowerCallbackPool call signature = do
  target <- targetM
  let Symbol pool = callbackPoolSymbol call
      stop = Symbol (pool <> "_stop")
      info = Symbol (pool <> "_info")
      resultTypes = map repType (grinForeignCallResultReps signature)
      rawTypes = map repType (grinForeignOperandReps signature)
      (parameters, results) = runtimeCallSignatureFor target False signature
      entries = [Symbol (pool <> "_" <> T.pack (show index)) | index <- [0 .. callbackPoolSize - 1]]
  emitItem (ItemData (DataItem (Symbol pool) Internal True (toInteger (lowerWordSize target)) (concatMap (\entry -> [DataCode (Just entry), DataNull, DataNull, DataNull]) entries)))
  continuationInfoItems (ContinuationSpec info stop [] resultTypes ContinuationFrameStop)
  -- The stop frame gives the context back to the capability: the foreign call
  -- that ran the callback loads the heap context from there.
  do
    (context, contextParameters) <- freshContext
    values <- mapM (\ty -> (,ty) <$> fresh "result") resultTypes
    beginBlock (Label "entry") []
    result <- case values of
      [(value, ty)] -> coerce I64 (Typed (OperandVar value) ty)
      [] -> pure (OperandLiteral (LitInt 0))
      _ -> failWith (LowerUnsupportedExpression "a callback has too many results")
    capability <- capabilityOperand context
    storeContext capability context
    _ <- callRuntime "aihc_callback_return" [Ptr, I64] [] [capability, result]
    terminate (Return [])
    finishFunction stop Internal (contextParameters <> values) [] AihcConvention
  forM_ entries $ \entry -> do
    arguments <- mapM (\ty -> (,ty) <$> fresh "argument") parameters
    beginBlock (Label "entry") []
    frame <- typedOperand <$> emitValue "callback_frame" Ptr (StackAlloc 48 (byteAlignment 8))
    _ <- callRuntime "aihc_callback_enter" [Ptr, Code, Ptr] [] [frame, OperandLiteral (LitSymbol entry), OperandLiteral (LitSymbol info)]
    capability <- callRuntime "aihc_callback_machine" [Ptr] [Ptr] [frame]
    closure <- callRuntime "aihc_callback_closure" [Ptr] [Ptr] [frame]
    continuation <- callRuntime "aihc_callback_continuation" [Ptr] [Ptr] [frame]
    converted <- zipWithM (\foreignTy (value, ty) -> extendForeignResult foreignTy (Typed (OperandVar value) ty)) (grinForeignArgumentTypes signature) arguments
    apply <- requireHelper (HelperApply [rawTypes])
    context <- loadContext capability
    emit [] (Call apply (contextOperands context <> (closure : continuation : map typedOperand converted)))
    result <- callRuntime "aihc_callback_leave" [Ptr] [I64] [frame]
    returned <- mapM (\ty -> coerce ty (Typed result I64)) results
    terminate (Return returned)
    finishFunction entry Internal arguments results CConvention

compileRuntimeCall :: FunctionCtx -> ValueEnv -> NativeRuntimeCall -> [GrinValue] -> LowerM Typed
compileRuntimeCall ctx env runtimeCall arguments = do
  results <- compileCCall ctx env (nativeRuntimeCallPassMachine runtimeCall) (nativeRuntimeCallForeignCall runtimeCall) arguments
  case results of
    [result] -> pure result
    _ -> failWith (LowerUnsupportedExpression "runtime call without a result")

compileCCall :: FunctionCtx -> ValueEnv -> Bool -> GrinForeignCall -> [GrinValue] -> LowerM [Typed]
compileCCall ctx env passMachine foreignCall arguments = do
  target <- targetM
  let signature = grinForeignCallSignature foreignCall
      dynamic = grinForeignCallTarget foreignCall `elem` [GrinForeignDynamic, GrinForeignUnsafeDynamic]
      (parameters, results) =
        if dynamic
          then
            let (parameters', results') = runtimeCallSignatureFor target False signature {grinForeignArgumentTypes = drop 1 (grinForeignArgumentTypes signature)}
             in (Ptr : parameters', results')
          else runtimeCallSignatureFor target passMachine signature
  when (length arguments /= length (grinForeignArgumentTypes signature)) $ failWith (LowerUnsupportedExpression "foreign call arity mismatch")
  values <- mapM (materialize ctx env) arguments
  operands <- zipWithM coerce (drop (fromEnum passMachine) parameters) values
  resultOperand <-
    if grinForeignCallTarget foreignCall `elem` [GrinForeignDynamic, GrinForeignUnsafeDynamic]
      then case operands of
        pointer : rest -> do
          code <- addressAsCode ctx pointer
          case results of
            [] -> emit [] (CallIndirect code rest (Signature (drop 1 parameters) results CConvention)) >> pure (OperandLiteral (LitInt 0))
            [resultType] -> typedOperand <$> emitValue "dynamic_result" resultType (CallIndirect code rest (Signature (drop 1 parameters) results CConvention))
            _ -> failWith (LowerUnsupportedExpression "a dynamic call has too many results")
        _ -> failWith (LowerUnsupportedExpression "a dynamic call requires a function pointer")
      else do
        capability <- if passMachine then currentCapability else pure (OperandLiteral LitNull)
        if passMachine && grinForeignCallSymbol foreignCall `Set.notMember` contextFreeRuntimeCalls
          then callSynced capability (grinForeignCallSymbol foreignCall) parameters results (capability : operands)
          else callRuntime (grinForeignCallSymbol foreignCall) parameters results ([capability | passMachine] <> operands)
  case results of
    [result] -> (: []) <$> extendForeignResult (grinForeignResultType signature) (Typed resultOperand result)
    _ -> pure []

-- | The runtime calls that take the capability but neither allocate nor read
-- the stack pointer. The capability does not need the context for them.
contextFreeRuntimeCalls :: Set Text
contextFreeRuntimeCalls = Set.fromList ["aihc_my_thread_id", "aihc_array_copy"]

-- | The Lir signature of a C runtime or foreign function.
runtimeCallSignature :: Bool -> GrinForeignSignature -> ([Type], [Type])
runtimeCallSignature = runtimeCallSignatureFor posixTarget64

runtimeCallSignatureFor :: LowerTarget -> Bool -> GrinForeignSignature -> ([Type], [Type])
runtimeCallSignatureFor target passMachine signature =
  ([Ptr | passMachine] <> argumentTypes (fromEnum passMachine) (grinForeignArgumentTypes signature), maybeToList (foreignResultType (grinForeignResultType signature)))
  where
    argumentTypes _ [] = []
    argumentTypes integers (ty : rest) =
      let ordinary = foreignType ty
          next = integers + if isFloatType ordinary then 0 else 1
          actual
            | lowerPackedCStack target && integers >= 8 = case ty of
                GrinForeignInt8 -> I8
                GrinForeignWord8 -> I8
                GrinForeignInt16 -> I16
                GrinForeignWord16 -> I16
                _ -> ordinary
            | otherwise = ordinary
       in actual : argumentTypes next rest

-- | Narrow C register arguments use extension to 32 bits. GRIN stores them
-- with extension to 64 bits, so their low 32 bits hold the C value.
-- Apple ARM64 stack arguments retain their original width instead.
foreignType :: GrinForeignType -> Type
foreignType ty =
  case ty of
    GrinForeignInt -> I64
    GrinForeignInt8 -> I32
    GrinForeignInt16 -> I32
    GrinForeignInt32 -> I32
    GrinForeignInt64 -> I64
    GrinForeignWord -> I64
    GrinForeignWord8 -> I32
    GrinForeignWord16 -> I32
    GrinForeignWord32 -> I32
    GrinForeignWord64 -> I64
    GrinForeignFloat -> F32
    GrinForeignDouble -> F64
    GrinForeignAddr -> Ptr
    GrinForeignClosure -> Ptr
    GrinForeignVoid -> I32

foreignResultType :: GrinForeignType -> Maybe Type
foreignResultType ty =
  case ty of
    GrinForeignVoid -> Nothing
    _ -> Just (foreignType ty)

-- | Extend a narrow C result to 64 bits from its own width, because the high
-- bits of a narrow result register are unspecified.
extendForeignResult :: GrinForeignType -> Typed -> LowerM Typed
extendForeignResult foreignTy (Typed operand actual) =
  case (foreignTy, actual) of
    (GrinForeignInt8, I8) -> emitValue "foreign_result" I64 (Convert SExt I8 operand I64)
    (GrinForeignInt16, I16) -> emitValue "foreign_result" I64 (Convert SExt I16 operand I64)
    (GrinForeignWord8, I8) -> emitValue "foreign_result" I64 (Convert ZExt I8 operand I64)
    (GrinForeignWord16, I16) -> emitValue "foreign_result" I64 (Convert ZExt I16 operand I64)
    (GrinForeignInt8, I32) -> extend SExt I8
    (GrinForeignInt16, I32) -> extend SExt I16
    (GrinForeignInt32, I32) -> emitValue "foreign_result" I64 (Convert SExt I32 operand I64)
    (GrinForeignWord8, I32) -> extend ZExt I8
    (GrinForeignWord16, I32) -> extend ZExt I16
    (GrinForeignWord32, I32) -> emitValue "foreign_result" I64 (Convert ZExt I32 operand I64)
    -- The float result returns to the integer slot it came from.
    (GrinForeignDouble, F64) -> emitValue "foreign_result" I64 (Convert Bitcast F64 operand I64)
    (GrinForeignFloat, F32) -> do
      bits <- emitValue "foreign_bits" I32 (Convert Bitcast F32 operand I32)
      emitValue "foreign_result" I64 (Convert ZExt I32 (typedOperand bits) I64)
    _ -> pure (Typed operand actual)
  where
    extend op narrowTy = do
      narrowed <- emitValue "foreign_narrow" narrowTy (Convert Trunc I32 operand narrowTy)
      emitValue "foreign_result" I64 (Convert op narrowTy (typedOperand narrowed) I64)

-- Primitives

compilePrimitive :: FunctionCtx -> ValueEnv -> [GrinVar] -> GrinRep -> Text -> [GrinValue] -> LowerM ValueEnv
compilePrimitive ctx env vars runtimeRep name arguments =
  case (name, arguments) of
    -- A reservation before the call covers the array, so the array starts
    -- at the heap pointer. The header has the same six slots that
    -- aihc_byte_array_initialize writes: the info table, the size, the
    -- contents address, the pinned flag, the alignment, and the charge.
    -- The payload follows the header, and it stays uninitialized.
    (_, [GrinLitValue (GrinLitInt _ size)])
      | name == uncheckedByteArrayPrimitive -> do
          let words' = uncheckedByteArrayWords size
          array <- bumpAllocate (fromInteger words')
          let object = typedOperand array
              literal = OperandLiteral . LitInt
              info = Symbol "aihc_byte_array_info"
          requireExternData info
          contents <- emitValue "contents" Ptr (PtrAdd object (literal 48))
          storeSlot HeapRegion Ptr (OperandLiteral (LitSymbol info)) object 0
          storeSlot HeapRegion I64 (literal size) object 8
          storeSlot HeapRegion Ptr (typedOperand contents) object 16
          storeSlot HeapRegion I64 (literal 0) object 24
          storeSlot HeapRegion I64 (literal 8) object 32
          storeSlot HeapRegion I64 (literal words') object 40
          bind [array]
    ("divInt#", [left, right]) -> do
      leftOperand <- word left
      rightOperand <- word right
      quotient <- emitValue "quotient" I64 (Binary DivS I64 leftOperand rightOperand)
      remainder <- emitValue "remainder" I64 (Binary RemS I64 leftOperand rightOperand)
      signs <- emitValue "signs" I64 (Binary Xor I64 leftOperand rightOperand)
      opposite <- emitValue "opposite" I1 (Compare LtS I64 (typedOperand signs) (OperandLiteral (LitInt 0)))
      nonzero <- emitValue "nonzero" I1 (Compare Ne I64 (typedOperand remainder) (OperandLiteral (LitInt 0)))
      adjust <- emitValue "adjust" I1 (Binary And I1 (typedOperand opposite) (typedOperand nonzero))
      rounded <- emitValue "rounded" I64 (Binary Sub I64 (typedOperand quotient) (OperandLiteral (LitInt 1)))
      result <- emitValue "result" I64 (Select I64 (typedOperand adjust) (typedOperand rounded) (typedOperand quotient))
      bind [result]
    (_, [left, right])
      | Just op <- lookup name binaryPrimitives -> do
          leftOperand <- word left
          rightOperand <- word right
          result <- emitValue "result" I64 (Binary op I64 leftOperand rightOperand)
          bind [result]
      | Just (op, ty, extend) <- lookup name narrowBinaryPrimitives -> do
          leftOperand <- word left
          rightOperand <- word right
          if keepsWidth op extend
            then do
              result <- emitValue "result" I64 (Binary op I64 leftOperand rightOperand)
              bind [result]
            else do
              wide <- emitValue "wide" I64 (Binary op I64 leftOperand rightOperand)
              narrow <- emitValue "narrow" ty (Convert Trunc I64 (typedOperand wide) ty)
              result <- emitValue "result" I64 (Convert extend ty (typedOperand narrow) I64)
              bind [result]
      | Just op <- lookup name comparisonPrimitives -> do
          leftOperand <- word left
          rightOperand <- word right
          flag <- emitValue "flag" I1 (Compare op I64 leftOperand rightOperand)
          result <- widen flag
          bind [result]
      | Just op <- lookup name addressComparisonPrimitives -> do
          leftOperand <- pointerValue ctx env left
          rightOperand <- pointerValue ctx env right
          flag <- emitValue "flag" I1 (Compare op Ptr leftOperand rightOperand)
          result <- widen flag
          bind [result]
      | Just (op, ty) <- lookup name floatBinaryPrimitives -> do
          leftOperand <- floatOperand ty left
          rightOperand <- floatOperand ty right
          result <- emitValue "result" ty (FloatBinary op ty leftOperand rightOperand)
          bits <- floatBits ty result
          bind [bits]
      | Just (op, ty) <- lookup name floatComparisonPrimitives -> do
          leftOperand <- floatOperand ty left
          rightOperand <- floatOperand ty right
          flag <- emitValue "flag" I1 (Compare op ty leftOperand rightOperand)
          result <- widen flag
          bind [result]
    ("compareByteArrays#", [left, leftOffset, right, rightOffset, count])
      | not checkPrimBounds -> do
          leftArray <- pointerValue ctx env left
          rightArray <- pointerValue ctx env right
          leftContents <- loadSlot HeapRegion "left_contents" Ptr leftArray byteArrayContentsOffset
          rightContents <- loadSlot HeapRegion "right_contents" Ptr rightArray byteArrayContentsOffset
          leftIndex <- word leftOffset
          rightIndex <- word rightOffset
          leftBytes <- emitValue "left_bytes" Ptr (PtrAdd (typedOperand leftContents) leftIndex)
          rightBytes <- emitValue "right_bytes" Ptr (PtrAdd (typedOperand rightContents) rightIndex)
          lengthOperand <- word count
          result <- compareBytesInline (typedOperand leftBytes) (typedOperand rightBytes) lengthOperand
          bind [Typed result I64]
    ("compareInt#", [left, right]) -> do
      leftOperand <- word left
      rightOperand <- word right
      less <- emitValue "less" I1 (Compare LtS I64 leftOperand rightOperand) >>= widen
      greater <- emitValue "greater" I1 (Compare GtS I64 leftOperand rightOperand) >>= widen
      result <- emitValue "result" I64 (Binary Sub I64 (typedOperand greater) (typedOperand less))
      bind [result]
    (_, [value])
      | Just mask <- lookup name narrowNotPrimitives -> do
          operand <- word value
          result <- emitValue "result" I64 (Binary Xor I64 operand (OperandLiteral (LitInt mask)))
          bind [result]
    -- The argument is an evaluated value, so its info table is the one of
    -- its constructor, and the table holds the constructor index.
    (_, [value])
      | name `elem` ["dataToTagSmall#", "dataToTagLarge#"] -> do
          object <- pointerValue ctx env value
          header <- loadObjectInfo HeapRegion object
          index <- loadInfoConstructorIndex (typedOperand header)
          bind [index]
    ("not#", [value]) -> do
      operand <- word value
      result <- emitValue "result" I64 (Binary Xor I64 operand (OperandLiteral (LitInt (-1))))
      bind [result]
    ("not64#", [value]) -> do
      operand <- word value
      result <- emitValue "result" I64 (Binary Xor I64 operand (OperandLiteral (LitInt (-1))))
      bind [result]
    ("notI#", [value]) -> do
      operand <- word value
      result <- emitValue "result" I64 (Binary Xor I64 operand (OperandLiteral (LitInt (-1))))
      bind [result]
    -- A bit reversal is a byte swap that continues down to single bits.
    ("bitReverse#", [value]) -> do
      operand <- word value
      result <- foldM swapStage operand [32, 16, 8, 4, 2, 1]
      bind [Typed result I64]
    ("negateInt#", [value]) -> do
      operand <- word value
      result <- emitValue "result" I64 (Binary Sub I64 (OperandLiteral (LitInt 0)) operand)
      bind [result]
    ("addIntC#", [left, right]) -> signedCarry Add left right
    ("subIntC#", [left, right]) -> signedCarry Sub left right
    ("addWordC#", [left, right]) -> unsignedCarry AddCarry left right
    -- The carry comes first, as a word.
    ("plusWord2#", [left, right]) -> do
      leftOperand <- word left
      rightOperand <- word right
      low <- fresh "low"
      carry <- fresh "carry"
      emit [low, carry] (Wide AddCarry I64 leftOperand rightOperand)
      high <- widen (Typed (OperandVar carry) I1)
      bind [high, Typed (OperandVar low) I64]
    ("subWordC#", [left, right]) -> unsignedCarry SubBorrow left right
    ("timesWord2#", [left, right]) -> do
      leftOperand <- word left
      rightOperand <- word right
      low <- fresh "low"
      high <- fresh "high"
      emit [low, high] (Wide MulWideU I64 leftOperand rightOperand)
      bind [Typed (OperandVar high) I64, Typed (OperandVar low) I64]
    -- The high half is necessary when it is not the sign extension of the low
    -- half.
    ("timesInt2#", [left, right]) -> do
      leftOperand <- word left
      rightOperand <- word right
      low <- fresh "low"
      high <- fresh "high"
      emit [low, high] (Wide MulWideS I64 leftOperand rightOperand)
      sign <- emitValue "sign" I64 (Binary ShrS I64 (OperandVar low) (OperandLiteral (LitInt 63)))
      needed <- emitValue "needed" I1 (Compare Ne I64 (OperandVar high) (typedOperand sign))
      neededWord <- widen needed
      bind [neededWord, Typed (OperandVar high) I64, Typed (OperandVar low) I64]
    ("quotRemWord#", [left, right]) -> do
      leftOperand <- word left
      rightOperand <- word right
      quotient <- emitValue "quotient" I64 (Binary DivU I64 leftOperand rightOperand)
      remainder <- emitValue "remainder" I64 (Binary RemU I64 leftOperand rightOperand)
      bind [quotient, remainder]
    ("quotRemWord2#", [high, low, divisor]) -> do
      highOperand <- word high
      lowOperand <- word low
      divisorOperand <- word divisor
      quotient <- fresh "quotient"
      remainder <- fresh "remainder"
      emit [quotient, remainder] (QuotRemWide I64 highOperand lowOperand divisorOperand)
      bind [Typed (OperandVar quotient) I64, Typed (OperandVar remainder) I64]
    ("nullAddr#", []) -> bind [Typed (OperandLiteral LitNull) Ptr]
    ("realWorld#", [])
      | null vars && null (runtimeRepComponents runtimeRep) -> pure env
    ("plusAddr#", [address, offset]) -> do
      base <- pointerValue ctx env address
      delta <- word offset
      result <- emitValue "address" Ptr (PtrAdd base delta)
      bind [result]
    ("minusAddr#", [left, right]) -> do
      leftOperand <- word left
      rightOperand <- word right
      result <- emitValue "result" I64 (Binary Sub I64 leftOperand rightOperand)
      bind [result]
    ("addr2Int#", [address]) -> do
      operand <- word address
      bind [Typed operand I64]
    ("int2Addr#", [value]) -> do
      operand <- pointerValue ctx env value
      bind [Typed operand Ptr]
    ("cstringLength#", [address]) -> do
      operand <- pointerValue ctx env address
      helper <- requireHelper HelperCStringLength
      result <- emitValue "length" I64 (Call helper [operand])
      bind [result]
    -- GRIN GC used this operand to preserve its lifetime.
    -- No runtime instruction is necessary.
    ("touch#", [_])
      | null vars -> pure env
    ("float2Double#", [value]) -> do
      operand <- floatOperand F32 value
      result <- emitValue "result" F64 (Convert FpExt F32 operand F64)
      bits <- floatBits F64 result
      bind [bits]
    ("double2Float#", [value]) -> do
      operand <- floatOperand F64 value
      result <- emitValue "result" F32 (Convert FpTrunc F64 operand F32)
      bits <- floatBits F32 result
      bind [bits]
    (_, [address, index])
      | Just (ty, scale, extend) <- lookup name addressLoadPrimitives -> do
          target <- addressElement address index scale
          value <- emitValue "value" ty (Load ty (byteAddress target 0) (byteAlignment 1))
          result <- if ty == I64 then pure value else emitValue "value" I64 (Convert extend ty (typedOperand value) I64)
          bind [result]
    (_, [address, index, value])
      | Just (ty, scale) <- lookup name addressStorePrimitives -> do
          target <- addressElement address index scale
          operand <- word value
          narrow <- if ty == I64 then pure operand else typedOperand <$> emitValue "narrow" ty (Convert Trunc I64 operand ty)
          emit [] (Store ty narrow (byteAddress target 0) (byteAlignment 1))
          bind []
    -- The freeze and the thaw of a byte array change only its info table.
    -- A promotion copies a frozen byte array and collects for a mutable one.
    (_, [value])
      | Just table <- lookup name byteArrayFreezePrimitives -> do
          typed <- materialize ctx env value
          let info = Symbol table
          requireExternData info
          storeSlot AnyRegion Ptr (OperandLiteral (LitSymbol info)) (typedOperand typed) 0
          bind [typed]
      | name `elem` identityPrimitives -> do
          typed <- materialize ctx env value
          bind [typed]
      | Just (ty, op) <- lookup name narrowPrimitives -> do
          operand <- word value
          narrow <- emitValue "narrow" ty (Convert Trunc I64 operand ty)
          result <- emitValue "result" I64 (Convert op ty (typedOperand narrow) I64)
          bind [result]
      | Just shift <- lookup name byteSwapPrimitives -> do
          operand <- word value
          result <- byteSwap shift operand
          bind [result]
      | Just op <- lookup name bitCountPrimitives -> do
          operand <- word value
          result <- emitValue "count" I64 (Unary op I64 operand)
          bind [result]
      | Just (op, bits) <- lookup name narrowBitCountPrimitives -> do
          operand <- word value
          -- The bits above the width must not count. A leading count sees
          -- them as zeros and removes them from the count. A trailing count
          -- stops at a set bit just above the width.
          prepared <-
            if op == Ctz
              then emitValue "bounded" I64 (Binary Or I64 operand (OperandLiteral (LitInt (2 ^ bits))))
              else emitValue "masked" I64 (Binary And I64 operand (OperandLiteral (LitInt (2 ^ bits - 1))))
          count <- emitValue "count" I64 (Unary op I64 (typedOperand prepared))
          result <-
            if op == Clz
              then emitValue "count" I64 (Binary Sub I64 (typedOperand count) (OperandLiteral (LitInt (64 - bits))))
              else pure count
          bind [result]
      | Just (op, ty) <- lookup name floatUnaryPrimitives -> do
          operand <- floatOperand ty value
          result <- emitValue "result" ty (FloatUnary op ty operand)
          bits <- floatBits ty result
          bind [bits]
      | Just ty <- lookup name intToFloatPrimitives -> do
          operand <- word value
          result <- emitValue "result" ty (Convert IToFS I64 operand ty)
          bits <- floatBits ty result
          bind [bits]
      | Just ty <- lookup name floatToIntPrimitives -> do
          operand <- floatOperand ty value
          result <- emitValue "result" I64 (Convert FToIS ty operand I64)
          bind [result]
    -- The runtime objects below keep the layouts that docs/lir.md fixes
    -- for the Lir runtime units, so their accessors are loads and stores
    -- rather than runtime calls. Every field is one eight-byte slot on
    -- every target.
    -- The contents of a reference can be stored by another capability.
    ("readMutVar#", [reference]) -> do
      base <- pointerValue ctx env reference
      value <- emitValue "field" I64 (LoadDependent I64 (heapAddress base mutVarContentsOffset))
      bind [value]
    (_, [object])
      | Just (ty, offset) <- lookup name objectFieldPrimitives -> do
          base <- pointerValue ctx env object
          value <- emitValue "field" ty (Load ty (heapAddress base offset) (byteAlignment 8))
          bind [value]
    ("writeMutVar#", [reference, value]) -> do
      base <- pointerValue ctx env reference
      operand <- word value >>= emitWriteBarrier ctx base
      emit [] (StoreAtomic Release I64 operand (heapAddress base mutVarContentsOffset))
      bind []
    -- casMutVar# gives a failure flag, one when the contents differed from
    -- the expected value, and the final contents.
    ("casMutVar#", [reference, expected, replacement]) -> do
      base <- pointerValue ctx env reference
      expectedOperand <- word expected
      replacementOperand <- word replacement >>= emitWriteBarrier ctx base
      current <- emitValue "current" I64 (CompareSwap SeqCst I64 expectedOperand replacementOperand (heapAddress base mutVarContentsOffset))
      matches <- emitValue "matches" I1 (Compare Eq I64 (typedOperand current) expectedOperand)
      final <- emitValue "final" I64 (Select I64 (typedOperand matches) replacementOperand (typedOperand current))
      unchanged <- emitValue "unchanged" I1 (Compare Ne I64 (typedOperand current) expectedOperand) >>= widen
      bind [unchanged, final]
    ("isEmptyMVar#", [mvar]) -> do
      full <- mvarFull mvar
      empty <- emitValue "empty" I1 (Compare Eq I64 (typedOperand full) (OperandLiteral (LitInt 0))) >>= widen
      bind [empty]
    -- tryReadMVar# and tryTakeMVar# each give a flag and the contents. The
    -- runtime call gives the contents, or null for an empty variable, so
    -- both come from one state of the variable while another capability
    -- can change it. The caller must not look at the null placeholder, but
    -- the collector may find it in a frame slot.
    (_, [mvar])
      | name `elem` ["tryReadMVar#", "tryTakeMVar#"],
        Just tryCall <- nativeRuntimePrimitiveCall name -> do
          contents <- compileRuntimeCall ctx env tryCall [mvar]
          flag <- emitValue "flag" I1 (Compare Ne I64 (typedOperand contents) (OperandLiteral (LitInt 0))) >>= widen
          bind [flag, contents]
    -- The atomic primitives of an Int# element are sequentially consistent.
    ("atomicReadIntArray#", [array, index]) -> do
      address <- byteArrayElement array index I64 ElementIndex
      value <- emitValue "value" I64 (LoadAtomic SeqCst I64 (byteAddress address 0))
      bind [value]
    ("atomicWriteIntArray#", [array, index, value]) -> do
      address <- byteArrayElement array index I64 ElementIndex
      operand <- word value
      emit [] (StoreAtomic SeqCst I64 operand (byteAddress address 0))
      bind []
    (_, [array, index])
      | name `elem` arrayLoadPrimitives -> do
          slot <- arrayElement array index
          value <- emitValue "element" I64 (LoadDependent I64 (heapAddress slot arrayElementsOffset))
          bind [value]
      | Just (ty, indexing, extension) <- lookup name byteArrayLoadPrimitives -> do
          address <- byteArrayElement array index ty indexing
          value <- emitValue "value" ty (Load ty (byteAddress address 0) (byteAlignment 1))
          result <- if ty == I64 then pure value else emitValue "value" I64 (Convert extension ty (typedOperand value) I64)
          bind [result]
    (_, [array, index, value])
      | name `elem` arrayStorePrimitives -> do
          base <- pointerValue ctx env array
          indexOperand <- word index
          operand <- word value >>= emitWriteBarrierAt ctx base indexOperand
          slot <- arrayElement array index
          emit [] (StoreAtomic Release I64 operand (heapAddress slot arrayElementsOffset))
          bind []
      | Just (ty, indexing) <- lookup name byteArrayStorePrimitives -> do
          address <- byteArrayElement array index ty indexing
          operand <- word value
          narrow <- if ty == I64 then pure operand else typedOperand <$> emitValue "narrow" ty (Convert Trunc I64 operand ty)
          emit [] (Store ty narrow (byteAddress address 0) (byteAlignment 1))
          bind []
    -- casArray# and casSmallArray# give a failure flag and the final
    -- element, as casMutVar# does for the contents of a reference.
    (_, [array, index, expected, replacement])
      | name `elem` arrayCasPrimitives -> do
          base <- pointerValue ctx env array
          indexOperand <- word index
          expectedOperand <- word expected
          replacementOperand <- word replacement >>= emitWriteBarrierAt ctx base indexOperand
          slot <- arrayElement array index
          current <- emitValue "current" I64 (CompareSwap SeqCst I64 expectedOperand replacementOperand (heapAddress slot arrayElementsOffset))
          matches <- emitValue "matches" I1 (Compare Eq I64 (typedOperand current) expectedOperand)
          final <- emitValue "final" I64 (Select I64 (typedOperand matches) replacementOperand (typedOperand current))
          unchanged <- emitValue "unchanged" I1 (Compare Ne I64 (typedOperand current) expectedOperand) >>= widen
          bind [unchanged, final]
    _
      | Just runtimeCall <- nativeRuntimePrimitiveCall name -> do
          let sentinel = lookup name escapingRuntimeSentinels
          forM_ sentinel $ \_ -> currentCapability >>= storePauseTop
          result <- compileRuntimeCall ctx env runtimeCall arguments
          -- A runtime call that can be an escape gives a sentinel when its
          -- promotion needs a collection, and it stored nothing then.
          forM_ sentinel $ \value -> do
            flag <- emitValue "runtime_escape" I1 (Compare Eq (typedType result) (typedOperand result) value)
            escapeCollectWhen ctx flag
          case nativeRuntimeCallResultCount runtimeCall of
            0 | null vars -> pure env
            1 -> bind [result]
            _ -> failWith (LowerUnsupportedExpression ("runtime primitive result arity " <> name))
      | otherwise -> failWith (LowerUnsupportedPrimitive name)
  where
    bind = bindVars env vars
    word value = materialize ctx env value >>= coerce I64
    widen (Typed flag _) = emitValue "wide" I64 (Convert ZExt I1 flag I64)
    -- Whether an MVar holds a value, as a word: its flag is one byte.
    mvarFull mvar = do
      base <- pointerValue ctx env mvar
      flag <- emitValue "full" I8 (LoadAtomic Acquire I8 (heapAddress base mvarFullOffset))
      emitValue "full" I64 (Convert ZExt I8 (typedOperand flag) I64)
    checkPrimBounds = lowerCheckPrimBounds (envOptions (ctxEnv ctx))
    -- Leave the current block for one whose entry means the bounds check
    -- passed. The other successor reports the failure and never returns.
    boundsCheck invalid failure message = do
      failed <- freshLabel "out_of_bounds"
      inside <- freshLabel "inside"
      terminate (Branch invalid (Target failed []) (Target inside []))
      beginColdBlock failed []
      _ <- callRuntime failure [] [] []
      terminate (Trap message)
      beginBlock inside []
    -- The slot of element @index@ of a boxed array. As in GHC, the index
    -- is unchecked unless the bounds checks are on; then a negative index
    -- is a huge unsigned one, thus one comparison rejects both it and an
    -- index at or beyond the length.
    arrayElement array index = do
      base <- pointerValue ctx env array
      offset <- word index
      when checkPrimBounds $ do
        count <- emitValue "count" I64 (Load I64 (heapAddress base arrayLengthOffset) (byteAlignment 8))
        invalid <- emitValue "invalid" I1 (Compare GeU I64 offset (typedOperand count))
        boundsCheck (typedOperand invalid) "aihc_array_bounds_fail" "boxed-array index is out of bounds"
      scaled <- emitValue "offset" I64 (Binary Mul I64 offset (OperandLiteral (LitInt 8)))
      typedOperand <$> emitValue "slot" Ptr (PtrAdd base (typedOperand scaled))
    -- The address of an element of a byte array. An element index counts
    -- elements of the width of @ty@, so it scales by that width; a byte
    -- offset is used as it is. As in GHC, neither is checked unless the
    -- bounds checks are on; then an element index is in bounds below the
    -- size divided by the width, and a byte offset when the element it
    -- starts fits before the size. The contents are a separate allocation,
    -- so the address is read last.
    byteArrayElement array index ty indexing = do
      base <- pointerValue ctx env array
      offset <- word index
      let width = typeWidth ty
          literal = OperandLiteral . LitInt
      when checkPrimBounds $ do
        size <- emitValue "size" I64 (Load I64 (heapAddress base byteArraySizeOffset) (byteAlignment 8))
        invalid <-
          case indexing of
            ElementIndex
              | width == 1 -> emitValue "invalid" I1 (Compare GeU I64 offset (typedOperand size))
              | otherwise -> do
                  limit <- emitValue "limit" I64 (Binary DivU I64 (typedOperand size) (literal width))
                  emitValue "invalid" I1 (Compare GeU I64 offset (typedOperand limit))
            ByteOffset
              | width == 1 -> emitValue "invalid" I1 (Compare GeU I64 offset (typedOperand size))
              | otherwise -> do
                  pastEnd <- emitValue "past_end" I1 (Compare GtU I64 offset (typedOperand size))
                  remaining <- emitValue "remaining" I64 (Binary Sub I64 (typedOperand size) offset)
                  tooLong <- emitValue "too_long" I1 (Compare GtU I64 (literal width) (typedOperand remaining))
                  emitValue "invalid" I1 (Binary Or I1 (typedOperand pastEnd) (typedOperand tooLong))
        boundsCheck (typedOperand invalid) "aihc_byte_array_bounds_fail" "byte array access is out of bounds"
      byteOffset <-
        if indexing == ByteOffset || width == 1
          then pure offset
          else typedOperand <$> emitValue "offset" I64 (Binary Mul I64 offset (literal width))
      contents <- emitValue "contents" Ptr (Load Ptr (heapAddress base byteArrayContentsOffset) (byteAlignment 8))
      typedOperand <$> emitValue "address" Ptr (PtrAdd (typedOperand contents) byteOffset)
    -- The address of element @index@ of the given width. Every access uses
    -- alignment one, because the source can give an unaligned address.
    addressElement address index scale = do
      base <- pointerValue ctx env address
      offset <- word index
      scaled <-
        if scale == 1
          then pure offset
          else typedOperand <$> emitValue "offset" I64 (Binary Mul I64 offset (OperandLiteral (LitInt scale)))
      typedOperand <$> emitValue "address" Ptr (PtrAdd base scaled)
    -- A Float# value travels as its bit pattern in the low 32 bits and a
    -- Double# value as its 64-bit pattern.
    floatOperand ty value = do
      operand <- word value
      case ty of
        F32 -> do
          narrow <- emitValue "bits" I32 (Convert Trunc I64 operand I32)
          typedOperand <$> emitValue "float" F32 (Convert Bitcast I32 (typedOperand narrow) F32)
        _ -> typedOperand <$> emitValue "double" F64 (Convert Bitcast I64 operand F64)
    floatBits ty (Typed operand _) =
      case ty of
        F32 -> do
          bits <- emitValue "bits" I32 (Convert Bitcast F32 operand I32)
          emitValue "result" I64 (Convert ZExt I32 (typedOperand bits) I64)
        _ -> emitValue "result" I64 (Convert Bitcast F64 operand I64)
    -- A byte swap of a narrow value moves the value to the high bytes first,
    -- thus one 64-bit swap gives every width.
    byteSwap shift operand = do
      shifted <-
        if shift == 0
          then pure operand
          else typedOperand <$> emitValue "shifted" I64 (Binary Shl I64 operand (OperandLiteral (LitInt shift)))
      result <- foldM swapStage shifted [32, 16, 8]
      pure (Typed result I64)
    swapStage value stage = do
      let mask = OperandLiteral (LitInt (byteSwapMask stage))
      high <- emitValue "swapped" I64 (Binary ShrU I64 value (OperandLiteral (LitInt stage)))
      highPart <- emitValue "swapped" I64 (Binary And I64 (typedOperand high) mask)
      lowPart <- emitValue "swapped" I64 (Binary And I64 value mask)
      low <- emitValue "swapped" I64 (Binary Shl I64 (typedOperand lowPart) (OperandLiteral (LitInt stage)))
      typedOperand <$> emitValue "swapped" I64 (Binary Or I64 (typedOperand highPart) (typedOperand low))
    signedCarry op left right = do
      leftOperand <- word left
      rightOperand <- word right
      result <- emitValue "result" I64 (Binary op I64 leftOperand rightOperand)
      -- Signed overflow: the sign of the result differs from both operands
      -- for addition, and from the left operand and the negated right operand
      -- for subtraction.
      firstBits <- emitValue "bits" I64 (Binary Xor I64 (if op == Add then typedOperand result else leftOperand) (if op == Add then leftOperand else rightOperand))
      secondBits <- emitValue "bits" I64 (Binary Xor I64 (if op == Add then typedOperand result else leftOperand) (if op == Add then rightOperand else typedOperand result))
      overflow <- emitValue "overflow" I64 (Binary And I64 (typedOperand firstBits) (typedOperand secondBits))
      flag <- emitValue "flag" I64 (Binary ShrU I64 (typedOperand overflow) (OperandLiteral (LitInt 63)))
      bind [result, flag]
    unsignedCarry op left right = do
      leftOperand <- word left
      rightOperand <- word right
      result <- fresh "result"
      carry <- fresh "carry"
      emit [result, carry] (Wide op I64 leftOperand rightOperand)
      flag <- widen (Typed (OperandVar carry) I1)
      bind [Typed (OperandVar result) I64, flag]

-- | The mask of one stage of a 64-bit byte or bit swap. The mask keeps every
-- other block of @stage@ bits.
byteSwapMask :: Integer -> Integer
byteSwapMask stage = sum [(2 ^ stage - 1) * 2 ^ (2 * stage * block) | block <- [0 .. 64 `div` (2 * stage) - 1]]

binaryPrimitives :: [(Text, BinaryOp)]
binaryPrimitives =
  [ ("+#", Add),
    ("-#", Sub),
    ("*#", Mul),
    ("plusWord#", Add),
    ("minusWord#", Sub),
    ("timesWord#", Mul),
    ("quotWord#", DivU),
    ("remWord#", RemU),
    ("quotInt#", DivS),
    ("remInt#", RemS),
    ("and#", And),
    ("or#", Or),
    ("xor#", Xor),
    ("and64#", And),
    ("or64#", Or),
    ("xor64#", Xor),
    ("plusWord64#", Add),
    ("subWord64#", Sub),
    ("timesWord64#", Mul),
    ("andI#", And),
    ("orI#", Or),
    ("xorI#", Xor),
    ("uncheckedShiftL#", Shl),
    ("uncheckedShiftRL#", ShrU),
    ("uncheckedIShiftL#", Shl),
    ("uncheckedIShiftRA#", ShrS),
    ("uncheckedIShiftRL#", ShrU),
    ("uncheckedShiftL64#", Shl),
    ("uncheckedShiftRL64#", ShrU),
    ("pdep#", Pdep),
    ("pext#", Pext)
  ]

-- | Binary operations whose result is a sized integer. The operation runs
-- at the width of a word and the result keeps only its low bits, which is
-- how a @Word16#@ or @Word32#@ shift or a sized addition wraps. The low
-- bits then widen again: a sized word widens with zeros and a sized int
-- widens with its sign. The operations that 'keepsWidth' names skip the
-- narrow and the widen.
-- | Whether a sized operation on two values of its width gives a value
-- of that width without a narrow. A sized value sits in a word register
-- extended to the word, with zeros for a sized word and with its sign for
-- a sized int. A bitwise operation on two such values gives the same
-- extension, and a logical right shift of a zero-extended value keeps its
-- zeros. An addition, a multiplication or a left shift can carry into the
-- bits above the width, so it narrows.
keepsWidth :: BinaryOp -> ConvertOp -> Bool
keepsWidth op extend =
  case op of
    And -> True
    Or -> True
    Xor -> True
    ShrU -> extend == ZExt
    _ -> False

-- | The complement of a sized word, with the mask of its width. The
-- exclusive or with the mask flips the bits of the width and keeps the
-- bits above it zero.
narrowNotPrimitives :: [(Text, Integer)]
narrowNotPrimitives = [("notWord8#", 0xff), ("notWord16#", 0xffff), ("notWord32#", 0xffffffff)]

narrowBinaryPrimitives :: [(Text, (BinaryOp, Type, ConvertOp))]
narrowBinaryPrimitives =
  [(name, (op, ty, ZExt)) | (name, (op, ty)) <- unsignedPrimitives]
    <> [(name, (op, ty, SExt)) | (name, (op, ty)) <- signedPrimitives]
  where
    unsignedPrimitives =
      [ ("uncheckedShiftLWord8#", (Shl, I8)),
        ("uncheckedShiftRLWord8#", (ShrU, I8)),
        ("uncheckedShiftLWord16#", (Shl, I16)),
        ("uncheckedShiftRLWord16#", (ShrU, I16)),
        ("uncheckedShiftLWord32#", (Shl, I32)),
        ("uncheckedShiftRLWord32#", (ShrU, I32)),
        ("andWord8#", (And, I8)),
        ("orWord8#", (Or, I8)),
        ("xorWord8#", (Xor, I8)),
        ("andWord16#", (And, I16)),
        ("orWord16#", (Or, I16)),
        ("xorWord16#", (Xor, I16)),
        ("andWord32#", (And, I32)),
        ("orWord32#", (Or, I32)),
        ("xorWord32#", (Xor, I32))
      ]
        <> [ (prefix <> "Word" <> width <> "#", (op, ty))
           | (width, ty) <- [("8", I8), ("16", I16), ("32", I32)],
             (prefix, op) <- [("plus", Add), ("sub", Sub), ("times", Mul)]
           ]
    signedPrimitives =
      [ (prefix <> "Int" <> width <> "#", (op, ty))
      | (width, ty) <- [("8", I8), ("16", I16), ("32", I32)],
        (prefix, op) <- [("plus", Add), ("sub", Sub), ("times", Mul)]
      ]

comparisonPrimitives :: [(Text, CompareOp)]
comparisonPrimitives =
  [ (prefix <> "Word" <> width <> "#", op)
  | width <- ["8", "16", "32"],
    (prefix, op) <- [("eq", Eq), ("ne", Ne), ("lt", LtU), ("le", LeU), ("gt", GtU), ("ge", GeU)]
  ]
    <> [ ("<#", LtS),
         ("==#", Eq),
         (">#", GtS),
         (">=#", GeS),
         ("<=#", LeS),
         ("/=#", Ne),
         ("eqWord#", Eq),
         ("neWord#", Ne),
         ("ltWord#", LtU),
         ("leWord#", LeU),
         ("gtWord#", GtU),
         ("geWord#", GeU),
         ("eqWord64#", Eq),
         ("neWord64#", Ne),
         ("ltWord64#", LtU),
         ("leWord64#", LeU),
         ("gtWord64#", GtU),
         ("geWord64#", GeU),
         ("eqChar#", Eq),
         ("neChar#", Ne),
         ("ltChar#", LtU),
         ("leChar#", LeU),
         ("gtChar#", GtU),
         ("geChar#", GeU)
       ]

-- | Comparisons of two addresses. An address compares as an unsigned number.
-- The identity tests of the mutable heap objects belong here too: two
-- arrays or two references are the same exactly when they are one object,
-- so the test is a pointer comparison and needs no runtime call.
addressComparisonPrimitives :: [(Text, CompareOp)]
addressComparisonPrimitives =
  [ ("reallyUnsafePtrEquality#", Eq),
    ("sameMutableArray#", Eq),
    ("sameSmallMutableArray#", Eq),
    ("sameMutableByteArray#", Eq),
    ("sameMutVar#", Eq),
    ("sameTVar#", Eq),
    ("sameMVar#", Eq),
    ("eqStableName#", Eq),
    ("eqAddr#", Eq),
    ("neAddr#", Ne),
    ("ltAddr#", LtU),
    ("leAddr#", LeU),
    ("gtAddr#", GtU),
    ("geAddr#", GeU)
  ]

-- The layouts of the runtime objects, as docs/lir.md fixes them for the
-- Lir runtime units. A boxed array holds its length in its first field and
-- its elements after it; a mutable reference is a boxed array of one
-- element. A byte array holds its size and then the address of its
-- contents. An MVar holds a one-byte full flag and then its value, which
-- aihc_runtime.c asserts. A stable name holds its hash in its third slot.

arrayLengthOffset, arrayElementsOffset, mutVarContentsOffset :: Integer
arrayLengthOffset = 8
arrayElementsOffset = 16
mutVarContentsOffset = arrayElementsOffset

byteArraySizeOffset, byteArrayContentsOffset, byteArrayPinnedOffset :: Integer
byteArraySizeOffset = 8
byteArrayContentsOffset = 16
byteArrayPinnedOffset = 24

mvarFullOffset :: Integer
mvarFullOffset = 8

stableNameHashOffset :: Integer
stableNameHashOffset = 16

-- | The number of a thread. It is directly after the header of the thread
-- object, and both fields are eight bytes on every target.
threadIdOffset :: Integer
threadIdOffset = 8

-- | Reads of one field of a runtime object. Each entry gives the type of
-- the field and its byte offset.
objectFieldPrimitives :: [(Text, (Type, Integer))]
objectFieldPrimitives =
  [ ("sizeofArray#", (I64, arrayLengthOffset)),
    ("sizeofMutableArray#", (I64, arrayLengthOffset)),
    ("sizeofSmallArray#", (I64, arrayLengthOffset)),
    ("sizeofSmallMutableArray#", (I64, arrayLengthOffset)),
    ("getSizeofSmallMutableArray#", (I64, arrayLengthOffset)),
    ("sizeofByteArray#", (I64, byteArraySizeOffset)),
    ("sizeofMutableByteArray#", (I64, byteArraySizeOffset)),
    ("getSizeofMutableByteArray#", (I64, byteArraySizeOffset)),
    ("byteArrayContents#", (Ptr, byteArrayContentsOffset)),
    ("mutableByteArrayContents#", (Ptr, byteArrayContentsOffset)),
    ("isByteArrayPinned#", (I64, byteArrayPinnedOffset)),
    ("isMutableByteArrayPinned#", (I64, byteArrayPinnedOffset)),
    ("stableNameToInt#", (I64, stableNameHashOffset)),
    ("aihcThreadIdNumber#", (I64, threadIdOffset))
  ]

-- | Reads and writes of one element of a boxed array. The small-array
-- family shares the boxed-array representation.
arrayLoadPrimitives, arrayStorePrimitives :: [Text]
arrayLoadPrimitives = ["indexArray#", "readArray#", "indexSmallArray#", "readSmallArray#"]
arrayStorePrimitives = ["writeArray#", "writeSmallArray#"]

-- | The runtime calls that can be an escape, and the result each gives when
-- its promotion needs a collection. GC-GRIN puts a store safepoint before
-- each of them.
escapingRuntimeSentinels :: [(Text, Operand)]
escapingRuntimeSentinels =
  [ ("tryPutMVar#", OperandLiteral (LitInt 2)),
    ("makeStableName#", OperandLiteral LitNull),
    ("copyArray#", OperandLiteral (LitInt 1)),
    ("copyMutableArray#", OperandLiteral (LitInt 1)),
    ("copySmallArray#", OperandLiteral (LitInt 1)),
    ("copySmallMutableArray#", OperandLiteral (LitInt 1)),
    ("stmCommit#", OperandLiteral (LitInt 2))
  ]

-- | The compare and swap of one element of a boxed array.
arrayCasPrimitives :: [Text]
arrayCasPrimitives = ["casArray#", "casSmallArray#"]

-- | How a byte-array primitive names an element: by an index that counts
-- elements of the element width, or by a byte offset.
data ByteArrayIndexing = ElementIndex | ByteOffset
  deriving (Eq, Show)

-- | Each byte-array read gives its element width, index form, and extension.
byteArrayLoadPrimitives :: [(Text, (Type, ByteArrayIndexing, ConvertOp))]
byteArrayLoadPrimitives =
  [ ("indexWordArray#", (I64, ElementIndex, ZExt)),
    ("readWordArray#", (I64, ElementIndex, ZExt)),
    ("indexIntArray#", (I64, ElementIndex, SExt)),
    ("readIntArray#", (I64, ElementIndex, SExt)),
    ("indexInt8Array#", (I8, ElementIndex, SExt)),
    ("readInt8Array#", (I8, ElementIndex, SExt)),
    ("indexInt16Array#", (I16, ElementIndex, SExt)),
    ("readInt16Array#", (I16, ElementIndex, SExt)),
    ("indexInt32Array#", (I32, ElementIndex, SExt)),
    ("readInt32Array#", (I32, ElementIndex, SExt)),
    ("indexInt64Array#", (I64, ElementIndex, SExt)),
    ("readInt64Array#", (I64, ElementIndex, SExt)),
    ("indexWord8Array#", (I8, ElementIndex, ZExt)),
    ("readWord8Array#", (I8, ElementIndex, ZExt)),
    ("indexWord16Array#", (I16, ElementIndex, ZExt)),
    ("readWord16Array#", (I16, ElementIndex, ZExt)),
    ("indexWord32Array#", (I32, ElementIndex, ZExt)),
    ("readWord32Array#", (I32, ElementIndex, ZExt)),
    ("indexWord64Array#", (I64, ElementIndex, ZExt)),
    ("readWord64Array#", (I64, ElementIndex, ZExt)),
    ("indexCharArray#", (I8, ByteOffset, ZExt)),
    ("readCharArray#", (I8, ByteOffset, ZExt)),
    ("indexWord8ArrayAsWord16#", (I16, ByteOffset, ZExt)),
    ("readWord8ArrayAsWord16#", (I16, ByteOffset, ZExt)),
    ("indexWord8ArrayAsWord32#", (I32, ByteOffset, ZExt)),
    ("readWord8ArrayAsWord32#", (I32, ByteOffset, ZExt)),
    ("indexWord8ArrayAsWord64#", (I64, ByteOffset, ZExt)),
    ("readWord8ArrayAsWord64#", (I64, ByteOffset, ZExt)),
    ("indexWideCharArray#", (I32, ElementIndex, ZExt)),
    ("readWideCharArray#", (I32, ElementIndex, ZExt)),
    -- A Float# value travels as its bit pattern in the low 32 bits and a
    -- Double# value as its 64-bit pattern, as for the address reads.
    ("indexFloatArray#", (I32, ElementIndex, ZExt)),
    ("readFloatArray#", (I32, ElementIndex, ZExt)),
    ("indexDoubleArray#", (I64, ElementIndex, ZExt)),
    ("readDoubleArray#", (I64, ElementIndex, ZExt)),
    ("indexAddrArray#", (I64, ElementIndex, ZExt)),
    ("readAddrArray#", (I64, ElementIndex, ZExt)),
    ("indexStablePtrArray#", (I64, ElementIndex, ZExt)),
    ("readStablePtrArray#", (I64, ElementIndex, ZExt))
  ]

-- | Writes of one element of a byte array, with the widths and indexing of
-- 'byteArrayLoadPrimitives'.
byteArrayStorePrimitives :: [(Text, (Type, ByteArrayIndexing))]
byteArrayStorePrimitives =
  [ ("writeWordArray#", (I64, ElementIndex)),
    ("writeIntArray#", (I64, ElementIndex)),
    ("writeInt8Array#", (I8, ElementIndex)),
    ("writeInt16Array#", (I16, ElementIndex)),
    ("writeInt32Array#", (I32, ElementIndex)),
    ("writeInt64Array#", (I64, ElementIndex)),
    ("writeWord8Array#", (I8, ElementIndex)),
    ("writeWord16Array#", (I16, ElementIndex)),
    ("writeWord32Array#", (I32, ElementIndex)),
    ("writeWord64Array#", (I64, ElementIndex)),
    ("writeCharArray#", (I8, ByteOffset)),
    ("writeWord8ArrayAsWord16#", (I16, ByteOffset)),
    ("writeWord8ArrayAsWord32#", (I32, ByteOffset)),
    ("writeWord8ArrayAsWord64#", (I64, ByteOffset)),
    ("writeWideCharArray#", (I32, ElementIndex)),
    ("writeFloatArray#", (I32, ElementIndex)),
    ("writeDoubleArray#", (I64, ElementIndex)),
    ("writeAddrArray#", (I64, ElementIndex)),
    ("writeStablePtrArray#", (I64, ElementIndex))
  ]

-- | The width in bytes of an integer element.
typeWidth :: Type -> Integer
typeWidth ty =
  case ty of
    I8 -> 1
    I16 -> 2
    I32 -> 4
    _ -> 8

-- | Reads of memory at an address. Each entry gives the width of the value,
-- the size of one index step in bytes, and how the value widens to a word:
-- a signed element sign-extends, every other element zero-extends.
addressLoadPrimitives :: [(Text, (Type, Integer, ConvertOp))]
addressLoadPrimitives =
  [ ("indexWord8OffAddr#", (I8, 1, ZExt)),
    ("readWord8OffAddr#", (I8, 1, ZExt)),
    ("indexWord16OffAddr#", (I16, 2, ZExt)),
    ("readWord16OffAddr#", (I16, 2, ZExt)),
    ("indexWord32OffAddr#", (I32, 4, ZExt)),
    ("readWord32OffAddr#", (I32, 4, ZExt)),
    ("indexWord64OffAddr#", (I64, 8, ZExt)),
    ("readWord64OffAddr#", (I64, 8, ZExt)),
    ("indexWord8OffAddrAsWord16#", (I16, 1, ZExt)),
    ("readWord8OffAddrAsWord16#", (I16, 1, ZExt)),
    ("indexWord8OffAddrAsWord32#", (I32, 1, ZExt)),
    ("readWord8OffAddrAsWord32#", (I32, 1, ZExt)),
    ("indexWord8OffAddrAsWord64#", (I64, 1, ZExt)),
    ("readWord8OffAddrAsWord64#", (I64, 1, ZExt)),
    -- A Float# value travels as its bit pattern in the low 32 bits and a
    -- Double# value as its 64-bit pattern, thus the float accessors reuse
    -- the word accessors of the same width.
    ("indexWord8OffAddrAsFloat#", (I32, 1, ZExt)),
    ("readWord8OffAddrAsFloat#", (I32, 1, ZExt)),
    ("indexWord8OffAddrAsDouble#", (I64, 1, ZExt)),
    ("readWord8OffAddrAsDouble#", (I64, 1, ZExt)),
    ("indexFloatOffAddr#", (I32, 4, ZExt)),
    ("readFloatOffAddr#", (I32, 4, ZExt)),
    ("indexDoubleOffAddr#", (I64, 8, ZExt)),
    ("readDoubleOffAddr#", (I64, 8, ZExt)),
    ("indexWordOffAddr#", (I64, 8, ZExt)),
    ("readWordOffAddr#", (I64, 8, ZExt)),
    ("indexIntOffAddr#", (I64, 8, ZExt)),
    ("readIntOffAddr#", (I64, 8, ZExt)),
    ("indexInt64OffAddr#", (I64, 8, ZExt)),
    ("readInt64OffAddr#", (I64, 8, ZExt)),
    ("indexAddrOffAddr#", (I64, 8, ZExt)),
    ("readAddrOffAddr#", (I64, 8, ZExt)),
    ("indexStablePtrOffAddr#", (I64, 8, ZExt)),
    ("readStablePtrOffAddr#", (I64, 8, ZExt)),
    ("indexWideCharOffAddr#", (I32, 4, ZExt)),
    ("readWideCharOffAddr#", (I32, 4, ZExt)),
    ("indexInt8OffAddr#", (I8, 1, SExt)),
    ("readInt8OffAddr#", (I8, 1, SExt)),
    ("indexInt16OffAddr#", (I16, 2, SExt)),
    ("readInt16OffAddr#", (I16, 2, SExt)),
    ("indexInt32OffAddr#", (I32, 4, SExt)),
    ("readInt32OffAddr#", (I32, 4, SExt))
  ]

-- | Writes of memory at an address, with the same widths and index steps as
-- the reads.
addressStorePrimitives :: [(Text, (Type, Integer))]
addressStorePrimitives =
  [ ("writeWord8OffAddr#", (I8, 1)),
    ("writeWord16OffAddr#", (I16, 2)),
    ("writeWord32OffAddr#", (I32, 4)),
    ("writeWord64OffAddr#", (I64, 8)),
    ("writeWord8OffAddrAsWord16#", (I16, 1)),
    ("writeWord8OffAddrAsWord32#", (I32, 1)),
    ("writeWord8OffAddrAsWord64#", (I64, 1)),
    ("writeWord8OffAddrAsFloat#", (I32, 1)),
    ("writeWord8OffAddrAsDouble#", (I64, 1)),
    ("writeInt8OffAddr#", (I8, 1)),
    ("writeInt16OffAddr#", (I16, 2)),
    ("writeInt32OffAddr#", (I32, 4)),
    ("writeInt64OffAddr#", (I64, 8)),
    ("writeIntOffAddr#", (I64, 8)),
    ("writeWordOffAddr#", (I64, 8)),
    ("writeAddrOffAddr#", (I64, 8)),
    ("writeStablePtrOffAddr#", (I64, 8)),
    ("writeFloatOffAddr#", (I32, 4)),
    ("writeDoubleOffAddr#", (I64, 8)),
    ("writeWideCharOffAddr#", (I32, 4))
  ]

-- | Conversions to a narrow integer. The result keeps the width of a word.
-- A word narrows without a sign and an integer keeps its sign.
narrowPrimitives :: [(Text, (Type, ConvertOp))]
narrowPrimitives =
  [ ("wordToWord8#", (I8, ZExt)),
    ("wordToWord16#", (I16, ZExt)),
    ("wordToWord32#", (I32, ZExt)),
    ("intToInt8#", (I8, SExt)),
    ("intToInt16#", (I16, SExt)),
    ("intToInt32#", (I32, SExt))
  ]

-- | Byte swaps. The value moves left by the given number of bits first, thus
-- one 64-bit swap gives the result of every width.
-- | The bit counts of a @Word#@. Lir has one operation for each, so no
-- target calls the runtime for them.
bitCountPrimitives :: [(Text, UnaryOp)]
bitCountPrimitives =
  [ ("clz#", Clz),
    ("ctz#", Ctz),
    ("popCnt#", Popcount),
    ("clz64#", Clz),
    ("ctz64#", Ctz),
    ("popCnt64#", Popcount)
  ]

-- | The bit counts of the low bits of a @Word#@, with the width in bits.
narrowBitCountPrimitives :: [(Text, (UnaryOp, Integer))]
narrowBitCountPrimitives =
  [ (name <> width <> "#", (op, bits))
  | (width, bits) <- [("8", 8), ("16", 16), ("32", 32)],
    (name, op) <- [("clz", Clz), ("ctz", Ctz), ("popCnt", Popcount)]
  ]

byteSwapPrimitives :: [(Text, Integer)]
byteSwapPrimitives =
  [ ("byteSwap16#", 48),
    ("byteSwap32#", 32),
    ("byteSwap64#", 0),
    ("byteSwap#", 0)
  ]

floatBinaryPrimitives :: [(Text, (FloatBinaryOp, Type))]
floatBinaryPrimitives =
  [ ("plusFloat#", (FAdd, F32)),
    ("minusFloat#", (FSub, F32)),
    ("timesFloat#", (FMul, F32)),
    ("+##", (FAdd, F64)),
    ("-##", (FSub, F64)),
    ("*##", (FMul, F64)),
    ("divideFloat#", (FDiv, F32)),
    ("/##", (FDiv, F64))
  ]

floatUnaryPrimitives :: [(Text, (FloatUnaryOp, Type))]
floatUnaryPrimitives =
  [ ("negateFloat#", (FNeg, F32)),
    ("fabsFloat#", (FAbs, F32)),
    ("negateDouble#", (FNeg, F64)),
    ("fabsDouble#", (FAbs, F64)),
    ("sqrtFloat#", (FSqrt, F32)),
    ("sqrtDouble#", (FSqrt, F64))
  ]

floatComparisonPrimitives :: [(Text, (CompareOp, Type))]
floatComparisonPrimitives =
  [ ("gtFloat#", (FGt, F32)),
    ("geFloat#", (FGe, F32)),
    ("ltFloat#", (FLt, F32)),
    ("leFloat#", (FLe, F32)),
    ("eqFloat#", (Eq, F32)),
    ("neFloat#", (Ne, F32)),
    (">##", (FGt, F64)),
    (">=##", (FGe, F64)),
    ("<##", (FLt, F64)),
    ("<=##", (FLe, F64)),
    ("==##", (Eq, F64)),
    ("/=##", (Ne, F64))
  ]

intToFloatPrimitives :: [(Text, Type)]
intToFloatPrimitives =
  [ ("int2Float#", F32),
    ("int2Double#", F64)
  ]

-- | Conversions of a float to an integer. GHC gives no result outside the
-- range of an integer. Lir has no undefined behavior, thus the conversion
-- traps there.
floatToIntPrimitives :: [(Text, Type)]
floatToIntPrimitives =
  [ ("float2Int#", F32),
    ("double2Int#", F64)
  ]

-- | The info table that each freeze or thaw of a byte array stores.
byteArrayFreezePrimitives :: [(Text, Text)]
byteArrayFreezePrimitives =
  [ ("unsafeFreezeByteArray#", "aihc_frozen_byte_array_info"),
    ("unsafeThawByteArray#", "aihc_byte_array_info")
  ]

identityPrimitives :: [Text]
identityPrimitives =
  [ "int2Word#",
    "word2Int#",
    "word8ToWord#",
    "word32ToWord#",
    "word64ToWord#",
    "wordToWord64#",
    "word64ToInt64#",
    "int64ToWord64#",
    "word16ToWord#",
    "ord#",
    "chr#",
    "unsafeFreezeArray#",
    "unsafeThawArray#",
    "unsafeFreezeSmallArray#",
    "unsafeThawSmallArray#",
    "castFloatToWord32#",
    "castWord32ToFloat#",
    "castDoubleToWord64#",
    "castWord64ToDouble#",
    "int8ToInt#",
    "int16ToInt#",
    "int32ToInt#",
    "intToInt64#",
    "int64ToInt#"
  ]

-- Case

compileCase :: FunctionCtx -> ValueEnv -> GrinValue -> GrinVar -> [GrinAlt] -> LowerM ()
-- A case without alternatives fails on each value.
compileCase _ _ _ _ [] = do
  _ <- callRuntime "aihc_no_match" [] [] []
  terminate (Trap "no matching case alternative")
-- A case of one alternative matches each value with no test.
compileCase ctx env scrutinee binder [alternative] = do
  typed <- materialize ctx env scrutinee
  binderValue <- coerceTo (repType (grinVarRuntimeRep binder)) typed
  env' <- bindCaseAlternative alternative typed (Map.insert binder binderValue env)
  compileExpr ctx env' (grinAltRhs alternative)
compileCase ctx env scrutinee binder alternatives = do
  typed <- materialize ctx env scrutinee
  binderValue <- coerceTo (repType (grinVarRuntimeRep binder)) typed
  let env' = Map.insert binder binderValue env
      isPointer = isPointerRuntimeRep (grinValueRuntimeRep scrutinee)
  targets <- forM alternatives $ \alternative -> do
    label <- freshLabel "alt"
    pure (alternative, label)
  let explicit = [(alternative, label) | (alternative, label) <- targets, grinAltCon alternative /= GrinDefaultAlt]
      -- A case without a default matches each value that gets to it, so
      -- its last alternative needs no test.
      (fallback, tested) =
        case [label | (alternative, label) <- targets, grinAltCon alternative == GrinDefaultAlt] of
          label : _ -> (label, explicit)
          [] -> (snd (last explicit), init explicit)
  if isPointer
    then do
      constructors <- forM tested $ \(alternative, label) ->
        case grinAltCon alternative of
          GrinDataAlt name index -> pure (name, index, label)
          _ -> failWith (LowerUnsupportedExpression "literal case on a lifted value")
      if null constructors
        then terminate (Jump (Target fallback []))
        else
          if length explicit >= constructorSwitchThreshold
            then do
              let indices = [index | (_, index, _) <- constructors]
              when (Set.size (Set.fromList indices) /= length indices) $
                failWith (LowerUnsupportedExpression "two alternatives of one case give the same constructor index")
              header <- loadObjectInfo HeapRegion (typedOperand typed)
              index <- loadInfoConstructorIndex (typedOperand header)
              terminate (Switch I64 (typedOperand index) [SwitchCase (toInteger constructorIndex) (Target label []) | (_, constructorIndex, label) <- constructors] (Just (Target fallback [])))
            else do
              header <- loadObjectInfo HeapRegion (typedOperand typed)
              identity <- loadInfoPointer "identity" (typedOperand header) 0
              checks <- forM constructors $ \(name, _, label) -> do
                let symbol = constructorInfoSymbol name 0
                unless (Map.member (ConstructorRuntimeInfo name SaturatedConstructor) (envInfoSymbols (ctxEnv ctx))) (requireExternData symbol)
                pure (symbol, label)
              pointerChecks identity checks fallback
    else do
      wordValue <- coerce I64 typed
      cases <- forM tested $ \(alternative, label) ->
        case grinAltCon alternative of
          GrinLitAlt literal ->
            case normalizedLiteralInteger literal of
              Just integer -> pure (SwitchCase integer (Target label []))
              Nothing -> failWith (LowerUnsupportedValue "string case alternative")
          _ -> failWith (LowerUnsupportedExpression "constructor case on an unboxed value")
      if null cases
        then terminate (Jump (Target fallback []))
        else terminate (Switch I64 wordValue (firstCases cases) (Just (Target fallback [])))
  forM_ targets $ \(alternative, label) -> branchWith $ do
    beginBlock label []
    env'' <- bindCaseAlternative alternative typed env'
    compileExpr ctx env'' (grinAltRhs alternative)
  where
    pointerChecks identity checks fallback =
      case checks of
        [] -> terminate (Jump (Target fallback []))
        (symbol, label) : rest -> do
          matches <- emitValue "matches" I1 (Compare Eq Ptr (typedOperand identity) (OperandLiteral (LitSymbol symbol)))
          next <- if null rest then pure fallback else freshLabel "check"
          terminate (Branch (typedOperand matches) (Target label []) (Target next []))
          unless (null rest) $ do
            beginBlock next []
            pointerChecks identity rest fallback
    firstCases = go Set.empty
      where
        go _ [] = []
        go seen (switchCase : rest)
          | switchCaseValue switchCase `Set.member` seen = go seen rest
          | otherwise = switchCase : go (Set.insert (switchCaseValue switchCase) seen) rest

-- | The environment of an alternative: its fields, loaded from the
-- scrutinee when the alternative uses them.
bindCaseAlternative :: GrinAlt -> Typed -> ValueEnv -> LowerM ValueEnv
bindCaseAlternative alternative typed env' =
  case grinAltCon alternative of
    GrinDataAlt {} -> do
      let live = freeExprVars (grinAltRhs alternative)
      bound <- forM [(index, field) | (index, field) <- zip [0 :: Int ..] (grinAltBinders alternative), field `Set.member` live] $ \(index, field) -> do
        let ty = repType (grinVarRuntimeRep field)
        value <- loadSlot HeapRegion (varBase field) ty (typedOperand typed) (toInteger (8 * (index + 1)))
        pure (field, value)
      pure (Map.fromList bound `Map.union` env')
    GrinLitAlt _ -> pure env'
    GrinDefaultAlt -> do
      bound <- forM (grinAltBinders alternative) $ \field -> do
        value <- coerceTo (repType (grinVarRuntimeRep field)) typed
        pure (field, value)
      pure (Map.fromList bound `Map.union` env')

-- | A case with this many constructor alternatives or more selects its
-- alternative from the constructor index of the info table. A case with
-- fewer compares the identity of the info table with each constructor.
-- An identity compare loads the address of the constructor table, and the
-- index load is one load for the full case.
constructorSwitchThreshold :: Int
constructorSwitchThreshold = 4

-- Executable entry

-- | The @main@ of the executable and the special continuations: the top
-- continuation applies the evaluated entry, the final continuation halts,
-- the update continuation is the GC-GRIN update function, and the thread
-- done continuation returns to the scheduler. @main@ hands the arguments
-- and the environment to the runtime before the capability starts, and it
-- reports the runtime statistics when the capability halts. An exit through
-- @aihc_exit_process@ reports them itself.
lowerExecutableMain :: GcGrinProgram -> LowerM ()
lowerExecutableMain gcProgram = do
  entryItems gcProgram
  argc <- fresh "argc"
  argv <- fresh "argv"
  beginBlock (Label "entry") []
  capability <- callRuntime "aihc_machine_initialize" [] [Ptr] []
  _ <- callRuntime "aihc_program_arguments_initialize" [I32, Ptr] [] [OperandVar argc, OperandVar argv]
  _ <- callRuntime "aihc_program_environment_initialize" [] [] []
  profiling <- gets stateProfileAllocations
  when profiling $ do
    mapM_ requireExternData [allocationNamesSymbol, allocationCountsSymbol, allocationSizeSymbol]
    _ <- callRuntime "aihc_allocation_profile_register" [Ptr, Ptr, Ptr] [] (map (OperandLiteral . LitSymbol) [allocationNamesSymbol, allocationCountsSymbol, allocationSizeSymbol])
    pure ()
  profilingTime <- gets stateProfileTime
  when profilingTime $ do
    let tables = [timeProfileCodesSymbol, timeProfileNamesSymbol, timeProfileCodeCountSymbol, timeProfileInfosSymbol, timeProfileInfoCountSymbol]
    mapM_ requireExternData tables
    _ <- callRuntime "aihc_time_profile_register" (replicate 6 Ptr) [] (capability : map (OperandLiteral . LitSymbol) tables)
    pure ()
  _ <- startMachine
  _ <- callRuntime "aihc_runtime_statistics_report" [] [] []
  terminate (Return [OperandLiteral (LitInt 0)])
  finishFunction (Symbol "main") Export [(argc, I32), (argv, Ptr)] [I32] CConvention

-- | The WASI P3 entry unit. The P3 driver initializes the program
-- arguments, calls @aihc_lir_program_start@, and calls
-- @aihc_lir_program_resume@ with the scheduler resumption of every
-- completed IO request. Both return one when the program has halted and zero
-- when every thread waits for IO.
lowerWasip3Entry :: GcGrinProgram -> LowerM ()
lowerWasip3Entry gcProgram = do
  entryItems gcProgram
  do
    beginBlock (Label "entry") []
    _ <- startMachine
    finished <- loadFinished
    terminate (Return [finished])
    finishFunction (Symbol "aihc_lir_program_start") Export [] [I32] CConvention
  do
    resume <- fresh "resume"
    beginBlock (Label "entry") []
    helper <- requireHelper HelperResume
    capability <- callRuntime "aihc_machine_initialize" [] [Ptr] []
    emit [] (Call helper [capability, OperandVar resume])
    finished <- loadFinished
    terminate (Return [finished])
    finishFunction (Symbol "aihc_lir_program_resume") Export [(resume, Ptr)] [I32] CConvention
  where
    loadFinished = do
      flag <- emitValue "finished" I64 (Load I64 (byteAddress (OperandLiteral (LitSymbol finishedSymbol)) 0) (byteAlignment 8))
      typedOperand <$> emitValue "finished" I32 (Convert Trunc I64 (typedOperand flag) I32)

-- | The word the exit function sets when the capability halts.
finishedSymbol :: Symbol
finishedSymbol = Symbol "aihc_lir_finished"

-- | The special continuations of an executable: the top continuation
-- applies the evaluated entry, the final continuation halts, and the thread done
-- continuation returns to the scheduler.
entryItems :: GcGrinProgram -> LowerM ()
entryItems _ = do
  requireExternData (globalSymbol executableEntryName)
  continuationInfoItems (ContinuationSpec finalInfo finalTarget [] [Ptr] ContinuationFrameStop)
  continuationInfoItems (ContinuationSpec topInfo topTarget [Ptr] [Ptr] ContinuationFrameNormal)
  continuationInfoItems (ContinuationSpec threadDoneInfo threadDoneTarget [] [Ptr] ContinuationFrameStop)
  emitItem (ItemData (DataItem finishedSymbol Internal True 8 [DataInt I64 0]))
  -- The top continuation applies the evaluated entry action to no arguments
  -- with the final continuation.
  do
    (context, contextParameters) <- freshContext
    final <- fresh "final"
    result <- fresh "result"
    beginBlock (Label "entry") []
    apply <- requireHelper (HelperApply [[]])
    terminate (TailCall apply (contextOperands context <> [OperandVar result, OperandVar final]))
    finishFunction topTarget Internal (contextParameters <> [(final, Ptr), (result, Ptr)]) [] AihcConvention
  -- The exit function and the statistics read the context of the capability.
  do
    (context, contextParameters) <- freshContext
    value <- fresh "value"
    beginBlock (Label "entry") []
    capability <- capabilityOperand context
    storeContext capability context
    entry <- callRuntime "aihc_halt" [Ptr] [Code] [capability]
    terminate (TailCallIndirect entry [] (Signature [] [] AihcConvention))
    finishFunction finalTarget Internal (contextParameters <> [(value, Ptr)]) [] AihcConvention
  threadDoneContinuation threadDoneTarget
  where
    finalTarget = Symbol "aihc_lir_final_continuation"
    topTarget = Symbol "aihc_lir_top_continuation"
    threadDoneTarget = Symbol "aihc_lir_thread_done_continuation"

finalInfo, topInfo, threadDoneInfo :: Symbol
finalInfo = Symbol "aihc_lir_final_info"
topInfo = Symbol "aihc_lir_top_info"
threadDoneInfo = Symbol "aihc_lir_thread_done_info"

-- | Create the capability and its continuations and evaluate the entry. The
-- call returns when the capability halts or when every thread waits for IO.
startMachine :: LowerM Operand
startMachine = do
  target <- targetM
  exit <- requireHelper HelperExit
  let entryGlobal = globalSymbol executableEntryName
  capability <- callRuntime "aihc_machine_new" [I64] [Ptr] [OperandLiteral (LitInt 0)]
  final <- allocateContinuation capability finalInfo 1
  top <- allocateContinuation capability topInfo 2
  storeSlot StackRegion Ptr final top 8
  threadDone <- allocateContinuation capability threadDoneInfo 1
  _ <- callRuntime "aihc_set_thread_done_continuation" [Ptr, Ptr] [] [capability, threadDone]
  -- The halt path returns through the exit function to the caller.
  emit [] (Store Code (OperandLiteral (LitSymbol exit)) (byteAddress capability (capabilityExitCodeOffset target)) (wordAlignment 1))
  emit [] (Store I64 (OperandLiteral (LitInt 0)) (byteAddress (OperandLiteral (LitSymbol finishedSymbol)) 0) (byteAlignment 8))
  eval <- requireHelper HelperEval
  context <- loadContext capability
  emit [] (Call eval (contextOperands context <> [OperandLiteral (LitSymbol entryGlobal), top]))
  pure capability

-- | One continuation of an entry, pushed on the stack of the running
-- thread: an info-table pointer and the captured slots the caller fills.
-- The push goes through the capability, so it comes before the code loads the
-- context. Exported for harnesses that build their own entry.
allocateContinuation :: Operand -> Symbol -> Int -> LowerM Operand
allocateContinuation capability info words' = do
  object <- callRuntime "aihc_stack_push" [Ptr, I64] [Ptr] [capability, OperandLiteral (LitInt (toInteger words'))]
  storeSlot StackRegion Ptr (OperandLiteral (LitSymbol info)) object 0
  pure object

-- | The continuation that a finished thread enters. Exported for harnesses
-- that build their own entry.
threadDoneContinuation :: Symbol -> LowerM ()
threadDoneContinuation target = do
  (context, contextParameters) <- freshContext
  value <- fresh "value"
  beginBlock (Label "entry") []
  capability <- capabilityOperand context
  storeContext capability context
  resume <- callRuntime "aihc_thread_done" [Ptr] [Ptr] [capability]
  helper <- requireHelper HelperResume
  terminate (TailCall helper [capability, resume])
  finishFunction target Internal (contextParameters <> [(value, Ptr)]) [] AihcConvention

-- Helpers

-- | Generate every requested helper. A helper can request further helpers,
-- so repeat until no request is new.
generateHelpers :: LowerEnv -> Set Helper -> LowerM ()
generateHelpers env done = do
  requested <- stateHelpers <$> get
  let pending = Set.toAscList (requested `Set.difference` done)
  unless (null pending) $ do
    mapM_ (generateHelper env) pending
    generateHelpers env (done `Set.union` Set.fromList pending)

generateHelper :: LowerEnv -> Helper -> LowerM ()
generateHelper env helper =
  case helper of
    -- The exit function records that the capability halted and returns to the
    -- function that started the capability.
    HelperExit -> do
      beginBlock (Label "entry") []
      when (lowerUnitKind (envOptions env) == ExecutableUnit) $
        emit [] (Store I64 (OperandLiteral (LitInt 1)) (byteAddress (OperandLiteral (LitSymbol finishedSymbol)) 0) (byteAlignment 8))
      terminate (Return [])
      finishFunction symbol Internal [] [] AihcConvention
    HelperContinue shape -> do
      (context, contextParameters) <- freshContext
      continuation <- fresh "continuation"
      values <- forM shape $ \ty -> (,ty) <$> fresh "value"
      beginBlock (Label "entry") []
      terminate (Jump (Target (Label "loop") [OperandVar continuation]))
      current <- fresh "current"
      beginBlock (Label "loop") [(current, Ptr)]
      header <- loadHeader StackRegion (OperandVar current)
      -- A continuation is never an indirection, because frames live on
      -- thread stacks and only a thunk is updated. The one frame without an
      -- entry is a forward frame, which passes the values to its parent,
      -- so a null entry is the only check.
      entry <- loadInfoCode "entry" header infoBackendEntryIndex
      isForward <- emitValue "forward" I1 (Compare Eq Code (typedOperand entry) (OperandLiteral LitNull))
      terminate (Branch (typedOperand isForward) (Target (Label "forward") []) (Target (Label "enter") []))
      beginColdBlock (Label "forward") []
      frame <- loadInfoByte "frame" header infoFrameKindByte
      isForwardFrame <- emitValue "forward_frame" I1 (Compare Eq I64 (typedOperand frame) (OperandLiteral (LitInt (toInteger (continuationFrameKindCode (Just ContinuationFrameForward))))))
      terminate (Branch (typedOperand isForwardFrame) (Target (Label "parent") []) (Target (Label "invalid") []))
      beginColdBlock (Label "parent") []
      parent <- loadSlot StackRegion "parent" Ptr (OperandVar current) 8
      terminate (Jump (Target (Label "loop") [typedOperand parent]))
      beginColdBlock (Label "invalid") []
      terminate (Trap "continuation has no entry")
      beginBlock (Label "enter") []
      -- Entering a frame pops it and every frame above it: the frame is
      -- the new stack pointer, and its chunk gives the stack limit. The
      -- stack pointer is a value of its own, so the frame can stay in the
      -- register of the object argument and only the copy moves.
      stack <- emitValue "sp" Ptr (PtrAdd (OperandVar current) (OperandLiteral (LitInt 0)))
      stackLimit <- chunkEnd (OperandVar current)
      -- A frame in a lower chunk than the stack pointer was in makes that
      -- chunk young again: new frames go above it. See aihc_stack_enter_chunk.
      sameChunk <- emitValue "same_chunk" I1 (Compare Eq Ptr stackLimit (contextStackLimit context))
      enterFrame <- freshLabel "enter_frame"
      enterChunk <- freshLabel "enter_chunk"
      terminate (Branch (typedOperand sameChunk) (Target enterFrame []) (Target enterChunk []))
      beginColdBlock enterChunk []
      capability <- capabilityOperand context
      _ <- callRuntime "aihc_stack_enter_chunk" [Ptr, Ptr] [] [capability, OperandVar current]
      terminate (Jump (Target enterFrame []))
      beginBlock enterFrame []
      let entered = context {contextStack = typedOperand stack, contextStackLimit = stackLimit}
      terminate
        ( TailCallIndirect
            (typedOperand entry)
            (contextOperands entered <> (OperandVar current : OperandLiteral LitNull : [OperandVar var | (var, _) <- values]))
            (Signature (contextTypes <> (Ptr : Ptr : shape)) [] AihcConvention)
        )
      finishFunction symbol Internal (contextParameters <> ((continuation, Ptr) : values)) [] AihcConvention
    HelperApply [shape] -> do
      target <- targetM
      let word = toInteger (lowerWordSize target)
      (context, contextParameters) <- freshContext
      function <- fresh "function"
      continuation <- fresh "continuation"
      values <- forM shape $ \ty -> (,ty) <$> fresh "value"
      beginBlock (Label "entry") []
      arguments <- if null shape then pure (OperandLiteral LitNull) else typedOperand <$> emitValue "arguments" Ptr (StackAlloc (toInteger (8 * length shape)) (byteAlignment 8))
      continuationSlot <- typedOperand <$> emitValue "slot" Ptr (StackAlloc word (wordAlignment 1))
      terminate (Jump (Target (Label "loop") [OperandVar function]))
      current <- fresh "current"
      beginBlock (Label "loop") [(current, Ptr)]
      header <- loadHeader HeapRegion (OperandVar current)
      kind <- loadInfoByte "kind" header infoObjectKindByte
      isIndirection <- emitValue "indirection" I1 (Compare Eq I64 (typedOperand kind) (OperandLiteral (LitInt runtimeObjectIndirection)))
      terminate (Branch (typedOperand isIndirection) (Target (Label "indirection") []) (Target (Label "apply") []))
      beginBlock (Label "indirection") []
      next <- loadSlot HeapRegion "next" Ptr (OperandVar current) 8
      terminate (Jump (Target (Label "loop") [typedOperand next]))
      beginBlock (Label "apply") []
      arity <- loadInfoByte "arity" header infoRemainingArityByte
      isClosure <- emitValue "closure" I1 (Compare Eq I64 (typedOperand kind) (OperandLiteral (LitInt (toInteger runtimeObjectClosure))))
      isSaturated <- emitValue "saturated" I1 (Compare Eq I64 (typedOperand arity) (OperandLiteral (LitInt 1)))
      isFast <- emitValue "fast" I1 (Binary And I1 (typedOperand isClosure) (typedOperand isSaturated))
      terminate (Branch (typedOperand isFast) (Target (Label "fast") []) (Target (Label "slow") []))
      beginBlock (Label "fast") []
      entry <- loadInfoCode "entry" header infoBackendEntryIndex
      terminate
        ( TailCallIndirect
            (typedOperand entry)
            (contextOperands context <> (OperandVar current : OperandVar continuation : [OperandVar var | (var, _) <- values]))
            (Signature (contextTypes <> (Ptr : Ptr : shape)) [] AihcConvention)
        )
      beginBlock (Label "slow") []
      forM_ (zip [0 :: Int ..] values) $ \(index, (var, ty)) ->
        storeSlot AnyRegion ty (OperandVar var) arguments (toInteger (8 * index))
      capability <- capabilityOperand context
      applySlow capability context current continuation continuationSlot arguments 1 (length shape)
      finishFunction symbol Internal (contextParameters <> ((function, Ptr) : (continuation, Ptr) : values)) [] AihcConvention
    -- Several groups, as the eval/apply model of GHC does it. A closure
    -- whose remaining arity equals the group count takes every value in its
    -- entry. A closure that takes k < n groups gets the first k groups, and
    -- a frame that holds the other groups becomes its continuation. Any
    -- other function goes to the runtime, which builds one partial
    -- application.
    HelperApply groups -> do
      target <- targetM
      let word = toInteger (lowerWordSize target)
          count = length groups
      (context, contextParameters) <- freshContext
      function <- fresh "function"
      continuation <- fresh "continuation"
      valueGroups <- forM groups (mapM (\ty -> (,ty) <$> fresh "value"))
      let values = concat valueGroups
      beginBlock (Label "entry") []
      arguments <- if null values then pure (OperandLiteral LitNull) else typedOperand <$> emitValue "arguments" Ptr (StackAlloc (toInteger (8 * length values)) (byteAlignment 8))
      continuationSlot <- typedOperand <$> emitValue "slot" Ptr (StackAlloc word (wordAlignment 1))
      terminate (Jump (Target (Label "loop") [OperandVar function]))
      current <- fresh "current"
      beginBlock (Label "loop") [(current, Ptr)]
      header <- loadHeader HeapRegion (OperandVar current)
      kind <- loadInfoByte "kind" header infoObjectKindByte
      isIndirection <- emitValue "indirection" I1 (Compare Eq I64 (typedOperand kind) (OperandLiteral (LitInt runtimeObjectIndirection)))
      terminate (Branch (typedOperand isIndirection) (Target (Label "indirection") []) (Target (Label "apply") []))
      beginBlock (Label "indirection") []
      next <- loadSlot HeapRegion "next" Ptr (OperandVar current) 8
      terminate (Jump (Target (Label "loop") [typedOperand next]))
      beginBlock (Label "apply") []
      isClosure <- emitValue "closure" I1 (Compare Eq I64 (typedOperand kind) (OperandLiteral (LitInt (toInteger runtimeObjectClosure))))
      terminate (Branch (typedOperand isClosure) (Target (Label "arity") []) (Target (Label "slow") []))
      beginBlock (Label "arity") []
      arity <- loadInfoByte "arity" header infoRemainingArityByte
      terminate
        ( Switch
            I64
            (typedOperand arity)
            ( SwitchCase (toInteger count) (Target (Label "exact") [])
                : [SwitchCase (toInteger taken) (Target (overLabel taken) []) | taken <- [1 .. count - 1]]
            )
            (Just (Target (Label "slow") []))
        )
      beginBlock (Label "exact") []
      entry <- loadInfoCode "entry" header infoBackendEntryIndex
      terminate
        ( TailCallIndirect
            (typedOperand entry)
            (contextOperands context <> (OperandVar current : OperandVar continuation : [OperandVar var | (var, _) <- values]))
            (Signature (contextTypes <> (Ptr : Ptr : concat groups)) [] AihcConvention)
        )
      forM_ [1 .. count - 1] $ \taken -> do
        beginBlock (overLabel taken) []
        let (supplied, held) = splitAt taken valueGroups
        applyOverSaturated context (OperandVar current) (OperandVar continuation) header supplied held
      beginBlock (Label "slow") []
      forM_ (zip [0 :: Int ..] values) $ \(index, (var, ty)) ->
        storeSlot AnyRegion ty (OperandVar var) arguments (toInteger (8 * index))
      capability <- capabilityOperand context
      applySlow capability context current continuation continuationSlot arguments count (length values)
      finishFunction symbol Internal (contextParameters <> ((function, Ptr) : (continuation, Ptr) : values)) [] AihcConvention
    -- The frame applies the function that arrives to the groups it holds,
    -- with the parent as the continuation.
    HelperApplyFrame groups -> do
      continuationInfoItems (ContinuationSpec (applyFrameInfoSymbol groups) symbol (Ptr : concat groups) [Ptr] ContinuationFrameNormal)
      (context, contextParameters) <- freshContext
      parent <- fresh "parent"
      values <- forM (concat groups) $ \ty -> (,ty) <$> fresh "value"
      applied <- fresh "applied"
      beginBlock (Label "entry") []
      apply <- requireHelper (HelperApply groups)
      terminate (TailCall apply (contextOperands context <> (OperandVar applied : OperandVar parent : [OperandVar var | (var, _) <- values])))
      finishFunction symbol Internal (contextParameters <> ((parent, Ptr) : values <> [(applied, Ptr)])) [] AihcConvention
    -- The roots are the arguments of the function. Every other argument
    -- does not move. The function makes its reservation again when it runs.
    HelperCollectReplay shape -> do
      (context, contextParameters) <- freshContext
      values <- forM shape $ \(ty, root) -> (,ty,root) <$> fresh "value"
      code <- fresh "code"
      words' <- fresh "words"
      srt <- fresh "srt"
      beginBlock (Label "entry") []
      let roots = [OperandVar var | (var, _, True) <- values]
      array <-
        if null roots
          then pure (OperandLiteral LitNull)
          else typedOperand <$> emitValue "roots" Ptr (StackAlloc (toInteger (8 * length roots)) (byteAlignment 8))
      forM_ (zip [0 :: Int ..] roots) $ \(index, root) ->
        storeSlot AnyRegion Ptr root array (toInteger (8 * index))
      capability <- capabilityOperand context
      storeContext capability context
      _ <- callRuntime "aihc_heap_collect" [Ptr, I64, I64, Ptr, Ptr] [] [capability, OperandVar words', OperandLiteral (LitInt (toInteger (length roots))), array, OperandVar srt]
      relocated <- relocate array (0 :: Int) values
      collected <- loadHeapContext capability context
      terminate (TailCallIndirect (OperandVar code) (contextOperands collected <> relocated) (Signature (contextTypes <> [ty | (_, ty, _) <- values]) [] AihcConvention))
      finishFunction symbol Internal (contextParameters <> [(var, ty) | (var, ty, _) <- values] <> [(code, Code), (words', I64), (srt, Ptr)]) [] AihcConvention
      where
        relocate array index values = case values of
          [] -> pure []
          (_, _, True) : rest -> do
            moved <- loadSlot AnyRegion "value" Ptr array (toInteger (8 * index))
            (typedOperand moved :) <$> relocate array (index + 1) rest
          (var, _, False) : rest -> (OperandVar var :) <$> relocate array index rest
    -- See 'compileForce'.
    HelperForce update shape -> do
      (context, contextParameters) <- freshContext
      value <- fresh "value"
      info <- fresh "info"
      fields <- forM shape $ \ty -> (,ty) <$> fresh "field"
      beginBlock (Label "entry") []
      (frame, pushed) <- pushStackFrame context (1 + length shape)
      storeSlot StackRegion Ptr (OperandVar info) frame 0
      forM_ (zip [0 :: Int ..] fields) $ \(index, (field, ty)) ->
        storeSlot StackRegion ty (OperandVar field) frame (toInteger (8 * (index + 1)))
      eval <- requireHelper $ case update of
        EvalUpdate -> HelperEval
        EvalSingleEntry -> HelperEvalSingleEntry
      terminate (TailCall eval (contextOperands pushed <> [OperandVar value, frame]))
      finishFunction symbol Internal (contextParameters <> [(value, Ptr), (info, Ptr)] <> fields) [] AihcConvention
    _ -> failWith (LowerUnsupportedExpression "internal: shared helper requested a local definition")
  where
    symbol = helperSymbol helper
    loadHeader region object = typedOperand <$> loadObjectInfo region object
    overLabel taken = Label ("over_" <> T.pack (show taken))

-- | The slow path of an apply helper: the runtime applies the function to
-- the values in the argument array, and the result goes to the continuation
-- the runtime leaves in the continuation slot. The runtime can allocate and
-- collect, so the capability gets the context first.
applySlow :: Operand -> Context -> Var -> Var -> Operand -> Operand -> Int -> Int -> LowerM ()
applySlow capability context current continuation continuationSlot arguments groups count = do
  -- The continuation slot is a C pointer variable, not a heap slot.
  emit [] (Store Ptr (OperandVar continuation) (byteAddress continuationSlot 0) (wordAlignment 1))
  storeContext capability context
  applied <- callRuntime "aihc_apply_slow" [Ptr, Ptr, I64, I64, Ptr, Ptr] [Ptr] [capability, OperandVar current, OperandLiteral (LitInt (toInteger groups)), OperandLiteral (LitInt (toInteger count)), arguments, continuationSlot]
  collected <- loadHeapContext capability context
  adjusted <- emitValue "adjusted" Ptr (Load Ptr (byteAddress continuationSlot 0) (wordAlignment 1))
  continue <- requireHelper (HelperContinue [Ptr])
  terminate (TailCall continue (contextOperands collected <> [typedOperand adjusted, applied]))

-- | The block of an apply helper for a closure that takes fewer groups than
-- the application supplies. It pushes a frame that holds the other groups
-- on the stack of the thread, then enters the closure with the groups it
-- takes. A push does not collect, so no value moves.
applyOverSaturated :: Context -> Operand -> Operand -> Operand -> [[(Var, Type)]] -> [[(Var, Type)]] -> LowerM ()
applyOverSaturated context function continuation header supplied held = do
  let heldTypes = map (map snd) held
      heldValues = concat held
  _ <- requireHelper (HelperApplyFrame heldTypes)
  (frame, pushed) <- pushStackFrame context (2 + length heldValues)
  storeSlot StackRegion Ptr (OperandLiteral (LitSymbol (applyFrameInfoSymbol heldTypes))) frame 0
  storeSlot StackRegion Ptr continuation frame 8
  forM_ (zip [0 :: Int ..] heldValues) $ \(index, (var, ty)) ->
    storeSlot StackRegion ty (OperandVar var) frame (toInteger (8 * (index + 2)))
  entry <- loadInfoCode "entry" header infoBackendEntryIndex
  terminate
    ( TailCallIndirect
        (typedOperand entry)
        (contextOperands pushed <> (function : frame : [OperandVar var | (var, _) <- concat supplied]))
        (Signature (contextTypes <> (Ptr : Ptr : map snd (concat supplied))) [] AihcConvention)
    )

-- | The word fields of an info table precede its byte fields. See the
-- "Info tables" section of @docs/lir.md@.
infoWordFieldCount, infoBackendEntryIndex :: Int
infoWordFieldCount = 5
infoBackendEntryIndex = 3

-- | The byte fields of an info table, as indices from the first byte field.
infoRemainingArityByte, infoFrameKindByte, infoObjectKindByte, infoNeedsEvalByte :: Int
infoRemainingArityByte = 1
infoFrameKindByte = 2
infoObjectKindByte = 3
infoNeedsEvalByte = 4

-- | The number of byte fields of an info table.
infoByteFieldCount :: Int
infoByteFieldCount = 5

-- | The largest constructor index that an info table holds.
infoConstructorIndexLimit :: Int
infoConstructorIndexLimit = 65535

-- | The needs_eval byte of an info table: 'needsEvalFollow' for an
-- indirection, 'needsEvalEnter' for a thunk and a blackhole, and 0 for a
-- value.
objectKindNeedsEval :: Int -> Integer
objectKindNeedsEval kind
  | toInteger kind == runtimeObjectIndirection = needsEvalFollow
  | toInteger kind `elem` [toInteger runtimeObjectThunk, runtimeObjectBlackhole] = needsEvalEnter
  | otherwise = 0

needsEvalEnter, needsEvalFollow :: Integer
needsEvalEnter = 1
needsEvalFollow = 2

-- | The largest count a byte field of an info table holds.
infoByteFieldLimit :: Int
infoByteFieldLimit = 255

runtimeObjectNode, runtimeObjectClosure, runtimeObjectThunk, runtimeObjectPartialConstructor :: Int
runtimeObjectNode = 0
runtimeObjectClosure = 1
runtimeObjectThunk = 2
runtimeObjectPartialConstructor = 3

runtimeObjectIndirection, runtimeObjectBlackhole, runtimeObjectThread, runtimeObjectBlackholeQueue :: Integer
runtimeObjectIndirection = 4
runtimeObjectBlackhole = 5
runtimeObjectThread = 7
runtimeObjectBlackholeQueue = 15

-- Program queries

programNodes :: GrinProgram -> [GrinNode]
programNodes program = map grinGlobalNode (grinGlobals program) <> concatMap (exprNodes . grinFunctionBody) (grinFunctions program)

exprNodes :: GrinExpr -> [GrinNode]
exprNodes expression =
  case expression of
    GrinBind _ value body -> exprNodes value <> exprNodes body
    GrinStore node -> [node]
    GrinStoreUnchecked node -> [node]
    GrinStoreRec bindings body -> map snd bindings <> exprNodes body
    GrinStoreRecUnchecked bindings body -> map snd bindings <> exprNodes body
    GrinIfWhnf _ ready slow -> exprNodes ready <> exprNodes slow
    GrinCase _ _ alternatives -> concatMap (exprNodes . grinAltRhs) alternatives
    _ -> []

-- | The constructors whose saturated table a case compares the identity of
-- an object against. 'compileCase' does this when the case has fewer than
-- 'constructorSwitchThreshold' constructor alternatives.
programIdentityCaseConstructors :: GrinProgram -> [Text]
programIdentityCaseConstructors program = concatMap (exprConstructors . grinFunctionBody) (grinFunctions program)
  where
    exprConstructors expression =
      case expression of
        GrinBind _ value body -> exprConstructors value <> exprConstructors body
        GrinStoreRec _ body -> exprConstructors body
        GrinStoreRecUnchecked _ body -> exprConstructors body
        GrinIfWhnf _ ready slow -> exprConstructors ready <> exprConstructors slow
        GrinCase _ _ alternatives ->
          let names = [name | GrinDataAlt name _ <- map grinAltCon alternatives]
              tested
                | any ((== GrinDefaultAlt) . grinAltCon) alternatives = names
                | otherwise = take (length names - 1) names
           in [name | length names < constructorSwitchThreshold, name <- tested]
                <> concatMap (exprConstructors . grinAltRhs) alternatives
        _ -> []

programRuntimeReps :: GrinProgram -> [GrinRep]
programRuntimeReps program =
  [rep | function <- grinFunctions program, ResultRep rep <- [grinFunctionResultRep function]] <> programSlotReps program

programSlotReps :: GrinProgram -> [GrinRep]
programSlotReps program =
  map (grinValueRuntimeRep . GrinLitValue) (grinProgramLiterals program)
    <> [rep | node <- programNodes program, GrinClosure _ layouts <- [grinNodeTag node], layout <- layouts, rep <- layout]
    <> concatMap (concat . grinConstructorLayouts) (grinConstructors program)
    <> concatMap (map grinValueRuntimeRep . grinNodeFields) (programNodes program)
    <> concatMap functionReps (grinFunctions program)
  where
    functionReps function = map grinVarRuntimeRep (grinFunctionParameters function) <> exprReps (grinFunctionBody function)
    exprReps expression =
      case expression of
        GrinBind vars value body -> map grinVarRuntimeRep vars <> exprReps value <> exprReps body
        GrinStoreRec bindings body -> map (grinVarRuntimeRep . fst) bindings <> exprReps body
        GrinStoreRecUnchecked bindings body -> map (grinVarRuntimeRep . fst) bindings <> exprReps body
        GrinIfWhnf value ready slow -> grinValueRuntimeRep value : (exprReps ready <> exprReps slow)
        GrinCase value binder alternatives -> grinValueRuntimeRep value : grinVarRuntimeRep binder : concatMap (\alternative -> map grinVarRuntimeRep (grinAltBinders alternative) <> exprReps (grinAltRhs alternative)) alternatives
        _ -> []

-- | The info tables one node needs. An unsaturated constructor names the
-- saturated table too: its own table points at it, and the runtime reads the
-- full width and the pointer map from there.
requiredNodeConstructorInfos :: GrinNode -> [RuntimeInfoKey]
requiredNodeConstructorInfos node =
  case grinNodeTag node of
    GrinConstructor name remaining
      | constructorStage remaining == SaturatedConstructor -> [ConstructorRuntimeInfo name SaturatedConstructor]
      | otherwise ->
          [ ConstructorRuntimeInfo name PartialConstructor,
            ConstructorRuntimeInfo name SaturatedConstructor
          ]
    _ -> []

runtimeInfoKeyStages :: GrinNode -> [RuntimeInfoKey]
runtimeInfoKeyStages node =
  case grinNodeTag node of
    GrinConstructor name remaining -> [ConstructorRuntimeInfo name (constructorStage remaining)]
    GrinClosure name layouts -> stages fields layouts
      where
        -- A stage that wants one group is the last one. The application
        -- that supplies the group enters the function, so no object of the
        -- stage after it exists.
        stages current remaining =
          ClosureRuntimeInfo name current remaining : case remaining of
            layout : rest@(_ : _) -> stages (current <> layout) rest
            _ -> []
    GrinThunk name -> [ThunkRuntimeInfo name fields]
  where
    fields = map grinValueRuntimeRep (grinNodeFields node)

runtimeInfoFunctionName :: RuntimeInfoKey -> Maybe FunctionName
runtimeInfoFunctionName key =
  case key of
    ConstructorRuntimeInfo {} -> Nothing
    ClosureRuntimeInfo name _ _ -> Just name
    ThunkRuntimeInfo name _ -> Just name

runtimeInfoKeyFields :: RuntimeInfoKey -> [GrinRep]
runtimeInfoKeyFields key =
  case key of
    ConstructorRuntimeInfo {} -> []
    ClosureRuntimeInfo _ fields _ -> fields
    -- The owner word is field zero of the payload.
    ThunkRuntimeInfo _ fields -> replicate thunkOwnerWords IntRep <> fields

-- | See 'infoLayout'.
runtimeInfoKeyLayout :: RuntimeInfoKey -> [GrinRep]
runtimeInfoKeyLayout key =
  case key of
    ClosureRuntimeInfo _ fields layouts -> fields <> concat layouts
    _ -> runtimeInfoKeyFields key

runtimeInfoKeyRemainingArity :: RuntimeInfoKey -> Int
runtimeInfoKeyRemainingArity key =
  case key of
    ConstructorRuntimeInfo {} -> 0
    ClosureRuntimeInfo _ _ layouts -> length layouts
    ThunkRuntimeInfo {} -> 0

runtimeInfoKeyObjectKind :: RuntimeInfoKey -> Int
runtimeInfoKeyObjectKind key =
  case key of
    ConstructorRuntimeInfo _ SaturatedConstructor -> runtimeObjectNode
    ConstructorRuntimeInfo _ PartialConstructor -> runtimeObjectPartialConstructor
    ClosureRuntimeInfo {} -> runtimeObjectClosure
    ThunkRuntimeInfo {} -> runtimeObjectThunk

runtimeInfoKeyNext :: RuntimeInfoKey -> Maybe RuntimeInfoKey
runtimeInfoKeyNext key =
  case key of
    ConstructorRuntimeInfo name PartialConstructor -> Just (ConstructorRuntimeInfo name SaturatedConstructor)
    ConstructorRuntimeInfo {} -> Nothing
    ClosureRuntimeInfo name fields (layout : rest@(_ : _)) -> Just (ClosureRuntimeInfo name (fields <> layout) rest)
    ClosureRuntimeInfo {} -> Nothing
    ThunkRuntimeInfo {} -> Nothing

normalizedLiteralInteger :: GrinLiteral -> Maybe Integer
normalizedLiteralInteger literal =
  case literal of
    GrinLitInt runtimeRep value -> Just (normalizeScalar runtimeRep value)
    GrinLitChar _ value -> Just (toInteger (ord value))
    _ -> Nothing

normalizeScalar :: GrinRep -> Integer -> Integer
normalizeScalar runtimeRep integer =
  case runtimeRep of
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
    modulus width = 2 ^ (width :: Int)
    unsigned width = integer `mod` modulus width
    signed width =
      let value = unsigned width
          sign = 2 ^ (width - 1)
       in if value >= sign then value - modulus width else value
