{-# LANGUAGE OverloadedStrings #-}

-- | Lower GC-GRIN to Lir.
--
-- Every GRIN function becomes a Lir function with the @aihc@ convention. The
-- first parameter is the machine. The next four parameters are the context
-- of the running thread: the heap pointer, the heap limit, the stack
-- pointer, and the stack limit. A pointer representation becomes @ptr@, an
-- address becomes @ptr@, and every other scalar becomes @i64@. Floats travel
-- as their bit patterns, like in the native runtime ABI.
--
-- Control transfer is explicit: a CPS transfer is a @tailcall@, a runtime
-- helper is a @call@ of an extern C function, and dynamic entries go through
-- the @backend_entry@ field of an info table. That field has the signature
-- @(ptr, ptr, ptr, ptr, ptr, ptr, ptr, T...) -> ()@ with the machine, the
-- context, the object, the continuation, and the supplied values. The
-- runtime defines fixed helpers and common argument shapes. Other shapes
-- remain in the module.
--
-- The context lives in values, not in the machine. The machine holds a copy
-- only around a C call that can read or change it. See 'Context'.
module Aihc.Lir.Lower
  ( LowerError (..),
    LowerOptions (..),
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
import Aihc.Grin.Gc (GcGrinProgram, entryGcProgram, gcContinuationFrames, gcContinuationFunctions, gcGrinProgram, nodeWords)
import Aihc.Grin.Srt
import Aihc.Grin.Syntax
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
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, maybeToList)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data LowerError
  = LowerMissingFunction !FunctionName
  | LowerUnsupportedExpression !Text
  | LowerUnsupportedValue !Text
  | LowerUnsupportedRuntimeRep !GrinRep
  | LowerUnsupportedPrimitive !Text
  | LowerCpsError !Text
  deriving (Eq, Show)

data UnitKind
  = -- | A library module. Unsupported primitives become runtime failures.
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
    lowerCheckPrimBounds :: !Bool
  }
  deriving (Eq, Show)

-- | Lower one library module. The flag selects the bounds checks of
-- 'lowerCheckPrimBounds'.
lowerModule :: LowerTarget -> Bool -> GcGrinProgram -> Either LowerError Module
lowerModule target checkPrimBounds =
  lowerProgramWith LowerOptions {lowerUnitKind = LibraryUnit, lowerExposeFunctions = False, lowerTarget = target, lowerCheckPrimBounds = checkPrimBounds}

-- | Lower the fixed executable entry unit.
lowerEntry :: LowerTarget -> Either LowerError Module
lowerEntry target = do
  gcProgram <- either (Left . LowerCpsError . T.pack . show) Right entryGcProgram
  lowerProgramWith LowerOptions {lowerUnitKind = ExecutableUnit, lowerExposeFunctions = False, lowerTarget = target, lowerCheckPrimBounds = False} gcProgram

lowerProgramWith :: LowerOptions -> GcGrinProgram -> Either LowerError Module
lowerProgramWith options gcProgram =
  Module . snd <$> runLower options gcProgram (lowerProgramItems options gcProgram)

-- | Supply each complete item to the consumer before conversion proceeds.
lowerModuleTo :: (Monad m) => LowerTarget -> Bool -> (Map Symbol Signature -> Item -> m ()) -> GcGrinProgram -> m (Either LowerError ())
lowerModuleTo target checkPrimBounds output gcProgram =
  consume (initialLowerState options gcProgram) Set.empty (lowerUnitActions env (gcGrinProgram gcProgram))
  where
    options = LowerOptions LibraryUnit False target checkPrimBounds
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
    enterStored :: ![Type],
    enterSupplied :: ![Type],
    enterTargetParameters :: ![Type],
    enterPassesContinuation :: !Bool
  }

data RuntimeInfo = RuntimeInfo
  { infoSymbol :: !Symbol,
    infoLinkage :: !Linkage,
    infoIdentity :: !DataField,
    infoFields :: ![GrinRep],
    infoRemainingArity :: !Int,
    infoNext :: !(Maybe Symbol),
    infoEnter :: !(Maybe RuntimeEnter),
    infoFrameKind :: !(Maybe ContinuationFrameKind),
    infoObjectKind :: !Int,
    infoSrt :: !(Maybe Symbol)
  }

-- | Function bodies are separate from the shared conversion metadata.
data LowerUnit = LowerUnit !LowerEnv !GrinProgram

data LowerEnv = LowerEnv
  { envOptions :: !LowerOptions,
    envFunctionSymbols :: !(Map FunctionName Symbol),
    envFunctionParameters :: !(Map FunctionName [Type]),
    envContinuationFunctions :: !(Set FunctionName),
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
  | HelperQuotRem2
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
  deriving (Eq, Ord, Show)

helperSymbol :: Helper -> Symbol
helperSymbol helper =
  Symbol $ case helper of
    HelperEval -> "aihc_lir_eval"
    HelperEvalSingleEntry -> "aihc_lir_eval_single_entry"
    HelperResume -> "aihc_lir_resume"
    HelperExit -> "aihc_lir_exit"
    HelperQuotRem2 -> "aihc_lir_quotrem2"
    HelperCStringLength -> "aihc_lir_cstring_length"
    HelperContinue shape -> "aihc_lir_continue_" <> shapeName shape
    HelperApply groups -> "aihc_lir_apply_" <> groupsName groups
    HelperApplyFrame groups -> "aihc_lir_apply_frame_" <> groupsName groups
    HelperContinueSlot -> "aihc_lir_continue_slot"
    HelperApplySlot -> "aihc_lir_apply_slot"

shapeName :: [Type] -> Text
shapeName = T.pack . map letter
  where
    letter ty = if ty == Ptr then 'p' else 'i'

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

-- | The two info tables of the frame an over-saturated application pushes
-- for the given argument groups.
applyFrameInfoSymbol, applyFrameAppliedInfoSymbol :: [[Type]] -> Symbol
applyFrameInfoSymbol groups = Symbol ("aihc_lir_apply_frame_" <> groupsName groups <> "_info")
applyFrameAppliedInfoSymbol groups = Symbol ("aihc_lir_apply_frame_" <> groupsName groups <> "_applied_info")

-- | An open block under construction.
data OpenBlock = OpenBlock
  { openLabel :: !Label,
    openParameters :: ![(Var, Type)],
    openInstructionsRev :: ![Instruction]
  }

data LowerState = LowerState
  { stateNext :: !Int,
    stateTarget :: !LowerTarget,
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
    stateContext :: !(Maybe Context)
  }

-- | The heap and stack context of the running thread. Every function of the
-- aihc convention that runs Haskell code receives it after the machine, and
-- gives it to each function it transfers control to. Thus the register
-- allocator keeps it in registers.
--
-- The machine holds a copy of the heap pointer and the stack pointer only
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
      stateExterns = Map.empty,
      stateExternData = Set.empty,
      stateHelpers = Set.empty,
      stateBitmaps = Map.empty,
      stateDefined = Set.empty,
      stateSignatures = Map.fromList [(functionSymbol (grinFunctionName function), Signature (Ptr : contextTypes <> map (repType . grinVarRuntimeRep) (grinFunctionParameters function)) [] AihcConvention) | function <- grinFunctions (gcGrinProgram gcProgram)],
      stateItemsRev = [],
      stateBlocksRev = [],
      stateOpen = Nothing,
      stateContext = Nothing
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
            <> concatMap requiredNodeConstructorInfos (programNodes program)
        )
    -- Every declared constructor keeps its tables, whatever its visibility:
    -- a case alternative names one to compare a tag against without
    -- building a node, and 'requiredNodeConstructorInfos' only reads the
    -- nodes. Narrowing this to what the unit names is a separate change.
    declaredConstructorInfos (name, layouts)
      | null layouts = [ConstructorRuntimeInfo name SaturatedConstructor]
      | otherwise = [ConstructorRuntimeInfo name SaturatedConstructor, ConstructorRuntimeInfo name PartialConstructor]
    -- One constructor needs at most two info tables: the saturated object,
    -- and one shared by every stage that still wants arguments. The partial
    -- table carries the saturated one as its next stage, which is where the
    -- runtime reads the full width and the pointer map from.
    constructorEntries =
      [ ( key,
          RuntimeInfo
            { infoSymbol = symbol,
              infoLinkage = Map.findWithDefault Export name constructorLinkage,
              infoIdentity = DataSymbol (constructorInfoSymbol name 0) 0,
              -- Both tables describe the saturated slots. A partial object
              -- has filled a prefix of them.
              infoFields = concat layouts,
              infoRemainingArity = if stage == SaturatedConstructor then 0 else length layouts,
              infoNext = if stage == SaturatedConstructor then Nothing else Just (constructorInfoSymbol name 0),
              infoEnter = Nothing,
              infoFrameKind = Nothing,
              infoObjectKind = runtimeInfoKeyObjectKind key,
              infoSrt = Nothing
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
              infoRemainingArity = runtimeInfoKeyRemainingArity key,
              infoNext = runtimeInfoKeyNext key >>= (`Map.lookup` infoSymbols),
              infoEnter = if name `Set.member` forwardingFunctions then Nothing else runtimeEnter target name key,
              infoFrameKind = Map.lookup name continuationFrames,
              infoObjectKind = runtimeInfoKeyObjectKind key,
              infoSrt = Map.lookup name srtSymbols
            }
        )
      | (key, symbol) <- Map.toAscList infoSymbols,
        Just name <- [runtimeInfoFunctionName key],
        Just target <- [Map.lookup name functionSymbols]
      ]
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
                    enterStored = map repType fields,
                    enterSupplied = map repType (concat layouts),
                    enterTargetParameters = targetParameters name,
                    enterPassesContinuation = name `Set.notMember` continuationFunctions
                  }
        ThunkRuntimeInfo _ fields ->
          Just
            RuntimeEnter
              { enterTarget = target,
                enterStored = map repType fields,
                enterSupplied = [],
                enterTargetParameters = targetParameters name,
                enterPassesContinuation = True
              }
        _ -> Nothing

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

-- | A helper that runs Haskell code takes the machine and the context first.
-- The resumption of the scheduler and the exit function take only the
-- machine: the machine holds the context there.
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
  HelperExit -> signature [Ptr] []
  HelperContinueSlot -> running [Ptr, I64]
  HelperApplySlot -> running [Ptr, Ptr, I64]
  HelperQuotRem2 -> signature [I64, I64, I64] [I64, I64]
  HelperCStringLength -> Signature [Ptr] [I64] CConvention
  where
    signature parameters results = Signature parameters results AihcConvention
    running parameters = signature (Ptr : contextTypes <> parameters) []

-- | The runtime Lir unit defines these fixed signatures.
sharedHelperSignature :: Helper -> Maybe Signature
sharedHelperSignature helper =
  case helper of
    HelperExit -> Nothing
    HelperContinue shape | not (common shape) -> Nothing
    HelperApply [shape] | common shape -> Just (helperSignature helper)
    HelperApply _ -> Nothing
    HelperApplyFrame _ -> Nothing
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
beginBlock label parameters = do
  state <- get
  case stateOpen state of
    Just _ -> failWith (LowerUnsupportedExpression "internal: block opened inside another block")
    Nothing -> put state {stateOpen = Just (OpenBlock label parameters [])}

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
            stateBlocksRev = Block (openLabel open) (openParameters open) (reverse (openInstructionsRev open)) terminator : stateBlocksRev state
          }

-- | Collect the blocks emitted since the last function into a function item.
finishFunction :: Symbol -> Linkage -> [(Var, Type)] -> [Type] -> CallingConvention -> LowerM ()
finishFunction symbol linkage parameters results convention = do
  state <- get
  when (isJust (stateOpen state)) $ failWith (LowerUnsupportedExpression "internal: function finished with an open block")
  put state {stateBlocksRev = []}
  emitItem
    ( ItemFunction
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

-- | The integer type with the width of a machine word.
wordType :: LowerTarget -> Type
wordType target = if lowerWordSize target == 8 then I64 else I32

-- | A heap slot is 8 bytes on every target. A pointer that lives in a slot
-- travels through @i64@ on a 32-bit target, so the high bytes of the slot
-- are always zero.
loadSlot :: Text -> Type -> Operand -> Integer -> LowerM Typed
loadSlot base ty object offset = do
  target <- targetM
  if lowerWordSize target == 8 || ty `notElem` [Ptr, Code]
    then emitValue base ty (Load ty (byteAddress object offset) (byteAlignment 8))
    else do
      word <- emitValue base I64 (Load I64 (byteAddress object offset) (byteAlignment 8))
      emitValue base ty (PtrFromInt (typedOperand word))

storeSlot :: Type -> Operand -> Operand -> Integer -> LowerM ()
storeSlot ty value object offset = do
  target <- targetM
  if lowerWordSize target == 8 || ty `notElem` [Ptr, Code]
    then emit [] (Store ty value (byteAddress object offset) (byteAlignment 8))
    else do
      word <- emitValue "slot" I64 (PtrToInt value)
      emit [] (Store I64 (typedOperand word) (byteAddress object offset) (byteAlignment 8))

-- | Thunk headers carry evaluation and waiter bits in their low two bits.
-- Every info table has at least four-byte alignment, including on wasm32.
loadObjectInfo :: Operand -> LowerM Typed
loadObjectInfo object = do
  header <- loadSlot "header_word" I64 object 0
  info <- emitValue "info_word" I64 (Binary And I64 (typedOperand header) (OperandLiteral (LitInt (-4))))
  emitValue "header" Ptr (PtrFromInt (typedOperand info))

-- | Byte field @index@ of an info table as an @i64@. The byte fields follow
-- the word fields.
loadInfoByte :: Text -> Operand -> Int -> LowerM Typed
loadInfoByte base header index = do
  target <- targetM
  let offset = toInteger (lowerWordSize target * infoWordFieldCount + index)
  value <- emitValue base I8 (Load I8 (byteAddress header offset) (byteAlignment 1))
  emitValue base I64 (Convert ZExt I8 (typedOperand value) I64)

-- | A @code@ field of an info table.
loadInfoCode :: Text -> Operand -> Int -> LowerM Typed
loadInfoCode base header index = do
  target <- targetM
  let width = lowerWordSize target
  emitValue base Code (Load Code (byteAddress header (toInteger (width * index))) (wordAlignment 1))

-- | A @ptr@ field of an info table.
loadInfoPointer :: Text -> Operand -> Int -> LowerM Typed
loadInfoPointer base header index = do
  target <- targetM
  let width = lowerWordSize target
  emitValue base Ptr (Load Ptr (byteAddress header (toInteger (width * index))) (wordAlignment 1))

-- | A word-sized integer field of a data object.
wordField :: LowerTarget -> Integer -> DataField
wordField target = DataInt (wordType target)

-- | A pointer stored in an 8-byte slot of a static object.
slotPointerFields :: LowerTarget -> DataField -> [DataField]
slotPointerFields target field
  | lowerWordSize target == 8 = [field]
  | otherwise = [field, DataInt I32 0]

-- | The exit-code field of the machine, at the same offset on every target.
machineExitCodeOffset :: Integer
machineExitCodeOffset = 16

-- | The bump pointer of the heap, the field after the exit code. A pointer
-- field, so its offset follows the target word size.
machineHeapNextOffset :: LowerTarget -> Integer
machineHeapNextOffset target = machineExitCodeOffset + toInteger (lowerWordSize target)

-- | The end of the space the bump pointer runs into, the field after it. A
-- reservation compares the two itself and only calls the runtime when the
-- words it wants do not fit.
machineHeapLimitOffset :: LowerTarget -> Integer
machineHeapLimitOffset target = machineHeapNextOffset target + toInteger (lowerWordSize target)

-- | The first free byte of the stack of the running thread, the field after
-- the end of the space. The field holds the stack pointer only while a C
-- call can read it. See 'Context'.
machineStackNextOffset :: LowerTarget -> Integer
machineStackNextOffset target = machineHeapLimitOffset target + toInteger (lowerWordSize target)

-- | The size and the alignment of a thread stack chunk. See
-- @aihc_runtime_internal.h@.
stackChunkBytes :: Integer
stackChunkBytes = 4096

-- | The bytes of the header of a stack chunk. The first frame of a chunk
-- follows it.
stackChunkHeaderBytes :: Integer
stackChunkHeaderBytes = 32

-- Context

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
  result <- action
  modify' (\state -> state {stateContext = context})
  pure result

-- | Give the machine the heap pointer and the stack pointer before a C call
-- that can read them.
storeContext :: Operand -> Context -> LowerM ()
storeContext machine context = do
  target <- targetM
  emit [] (Store Ptr (contextHeap context) (byteAddress machine (machineHeapNextOffset target)) (wordAlignment 1))
  emit [] (Store Ptr (contextStack context) (byteAddress machine (machineStackNextOffset target)) (wordAlignment 1))

-- | Load the heap pointer and the heap limit of the machine after a C call
-- that can allocate or collect.
loadHeapContext :: Operand -> Context -> LowerM Context
loadHeapContext machine context = do
  target <- targetM
  heap <- emitValue "hp" Ptr (Load Ptr (byteAddress machine (machineHeapNextOffset target)) (wordAlignment 1))
  heapLimit <- emitValue "hp_limit" Ptr (Load Ptr (byteAddress machine (machineHeapLimitOffset target)) (wordAlignment 1))
  pure context {contextHeap = typedOperand heap, contextHeapLimit = typedOperand heapLimit}

-- | Load the whole context of the machine, where C code gives control to
-- Haskell code.
loadContext :: Operand -> LowerM Context
loadContext machine = do
  target <- targetM
  stack <- emitValue "sp" Ptr (Load Ptr (byteAddress machine (machineStackNextOffset target)) (wordAlignment 1))
  -- The stack pointer can be the end of a full chunk, so the limit comes
  -- from the byte before it.
  below <- emitValue "sp_below" Ptr (PtrAdd (typedOperand stack) (OperandLiteral (LitInt (-1))))
  stackLimit <- chunkEnd (typedOperand below)
  loadHeapContext machine (Context (OperandLiteral LitNull) (OperandLiteral LitNull) (typedOperand stack) stackLimit)

-- | The end of the stack chunk that holds an address.
chunkEnd :: Operand -> LowerM Operand
chunkEnd address = do
  word <- emitValue "chunk_word" I64 (PtrToInt address)
  last' <- emitValue "chunk_last" I64 (Binary Or I64 (typedOperand word) (OperandLiteral (LitInt (stackChunkBytes - 1))))
  end <- emitValue "chunk_end" I64 (Binary Add I64 (typedOperand last') (OperandLiteral (LitInt 1)))
  typedOperand <$> emitValue "sp_limit" Ptr (PtrFromInt (typedOperand end))

-- | Store the context of the code under construction to the machine.
syncMachine :: FunctionCtx -> LowerM ()
syncMachine ctx = currentContext >>= storeContext (ctxMachine ctx)

-- | Load the heap pointer and the heap limit into the context of the code
-- under construction.
reloadHeap :: FunctionCtx -> LowerM ()
reloadHeap ctx = currentContext >>= loadHeapContext (ctxMachine ctx) >>= setContext

-- | A C call that can allocate, collect, or read the stack pointer. The
-- machine gets the context before the call, and the heap pointer and the
-- heap limit come back after it. A C call without the machine cannot see
-- the context, so it needs neither step.
callSynced :: FunctionCtx -> Text -> [Type] -> [Type] -> [Operand] -> LowerM Operand
callSynced ctx name parameters results arguments = do
  syncMachine ctx
  result <- callRuntime name parameters results arguments
  reloadHeap ctx
  pure result

-- | The machine and the context, the first arguments of each transfer to
-- Haskell code.
runningArguments :: FunctionCtx -> LowerM [Operand]
runningArguments ctx = (ctxMachine ctx :) . contextOperands <$> currentContext

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
lowerUnitActions env program@(GrinProgram constructors _ _ globals functions) =
  [mapM_ validateRuntimeRep (programRuntimeReps program), mapM_ validateSlotRep (programSlotReps program)]
    <> [lowerFunction env function | function <- functions, grinFunctionName function `Set.notMember` envForwardingFunctions env]
    <> map (lowerStaticObject env) (programStaticObjects (GrinProgram constructors [] [] globals []))
    <> map lowerInfo (envInfos env)
    <> [lowerCallbackPool call signature | call <- grinForeignCalls program, GrinForeignWrapper signature <- [grinForeignCallTarget call]]
    <> [lowerStaticReferenceTables env]
    <> [emitItem (ItemData (DataItem symbol Internal False 1 [DataBytes bytes, DataInt I8 0])) | (bytes, symbol) <- Map.toAscList (envAddrLiterals env)]

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
infoBitmap info = BS.pack [if isPointerRuntimeRep field then 1 else 0 | field <- infoFields info]

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
                byte (infoObjectKind info)
              ]
          }
    )

-- | The function that enters one object: a shared runtime function when the
-- object has one of the shapes the runtime defines, and otherwise a stub of
-- this module.
enterFunction :: Symbol -> RuntimeEnter -> LowerM Symbol
enterFunction info enter =
  case sharedEnterSymbol enter of
    Just symbol -> do
      let signature = Signature (Ptr : contextTypes <> [Ptr, Ptr] <> enterSupplied enter) [] AihcConvention
      modify' (\state -> state {stateExterns = Map.insert symbol signature (stateExterns state)})
      pure symbol
    Nothing -> do
      let stub = Symbol (unSymbol info <> "_e")
      lowerEnterStub stub enter
      pure stub

-- | The shared enter function of the runtime for one object shape, when the
-- runtime defines one. @aihc_enter.lir@ defines @aihc_lir_enter_S_V@ and
-- @aihc_lir_enter_S_V_k@ for @S@ stored pointers up to 'sharedEnterMaxStored'
-- and @V@ supplied pointers up to 'sharedEnterMaxSupplied'; the @_k@ form
-- passes the continuation to the code. Those functions load the stored
-- fields and tail-call the code through the identity field of the info
-- table, which is what a generated stub does for the same shape. Any value
-- that is not a pointer keeps a generated stub, since WebAssembly checks the
-- signature of an indirect call and a stub coerces such values.
sharedEnterSymbol :: RuntimeEnter -> Maybe Symbol
sharedEnterSymbol enter
  | all (== Ptr) (enterStored enter),
    all (== Ptr) (enterSupplied enter),
    all (== Ptr) (enterTargetParameters enter),
    length (enterTargetParameters enter) == stored + supplied + (if passesContinuation then 1 else 0),
    stored <= sharedEnterMaxStored,
    supplied <= sharedEnterMaxSupplied =
      Just (Symbol (T.pack ("aihc_lir_enter_" <> show stored <> "_" <> show supplied <> (if passesContinuation then "_k" else ""))))
  | otherwise = Nothing
  where
    stored = length (enterStored enter)
    supplied = length (enterSupplied enter)
    passesContinuation = enterPassesContinuation enter

-- | The shapes @aihc_enter.lir@ defines.
sharedEnterMaxStored, sharedEnterMaxSupplied :: Int
sharedEnterMaxStored = 8
sharedEnterMaxSupplied = 4

-- | The dynamic entry of one enterable object. It loads the stored fields,
-- takes the supplied values as parameters, and tail-calls the code.
lowerEnterStub :: Symbol -> RuntimeEnter -> LowerM ()
lowerEnterStub stub enter = do
  machine <- fresh "machine"
  (context, contextParameters) <- freshContext
  object <- fresh "object"
  continuation <- fresh "continuation"
  supplied <- forM (enterSupplied enter) $ \ty -> (,ty) <$> fresh "supplied"
  beginBlock (Label "entry") []
  stored <- forM (zip [0 :: Int ..] (enterStored enter)) $ \(index, ty) ->
    loadSlot "stored" ty (OperandVar object) (toInteger (8 * (index + 1)))
  let values = stored <> [Typed (OperandVar var) ty | (var, ty) <- supplied] <> [Typed (OperandVar continuation) Ptr | enterPassesContinuation enter]
      parameters = enterTargetParameters enter
  when (length parameters /= length values) $
    failWith (LowerUnsupportedExpression ("enter stub arity mismatch for " <> unSymbol (enterTarget enter)))
  arguments <- zipWithM coerce parameters values
  terminate (TailCall (enterTarget enter) (OperandVar machine : contextOperands context <> arguments))
  finishFunction stub Internal ((machine, Ptr) : contextParameters <> [(object, Ptr), (continuation, Ptr)] <> supplied) [] AihcConvention

-- | A continuation object kind for an entry or a harness: the unapplied and
-- the applied info table, and the stub that enters the target function with
-- the stored fields and the supplied values.
data ContinuationSpec = ContinuationSpec
  { continuationInfo :: !Symbol,
    continuationAppliedInfo :: !Symbol,
    continuationTarget :: !Symbol,
    continuationStored :: ![Type],
    continuationSupplied :: ![Type],
    continuationFrame :: !ContinuationFrameKind
  }

continuationInfoItems :: ContinuationSpec -> LowerM ()
continuationInfoItems spec = do
  lowerInfo unapplied
  lowerInfo applied
  where
    stored = map typeRep (continuationStored spec)
    supplied = map typeRep (continuationSupplied spec)
    unapplied =
      RuntimeInfo
        { infoSymbol = continuationInfo spec,
          infoLinkage = Internal,
          infoIdentity = DataCode (Just (continuationTarget spec)),
          infoFields = stored,
          infoRemainingArity = 1,
          infoNext = Just (continuationAppliedInfo spec),
          infoEnter =
            Just
              RuntimeEnter
                { enterTarget = continuationTarget spec,
                  enterStored = continuationStored spec,
                  enterSupplied = continuationSupplied spec,
                  enterTargetParameters = continuationStored spec <> continuationSupplied spec,
                  enterPassesContinuation = False
                },
          infoFrameKind = Just (continuationFrame spec),
          infoObjectKind = runtimeObjectClosure,
          infoSrt = Nothing
        }
    applied =
      unapplied
        { infoSymbol = continuationAppliedInfo spec,
          infoFields = stored <> supplied,
          infoRemainingArity = 0,
          infoNext = Nothing,
          infoEnter = Nothing
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
      payload = if null fields && isThunk then [DataZero 8] else fields
  emitItem (ItemData (DataItem (globalSymbol (staticObjectName object)) linkage True 8 (header <> applied <> payload)))
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
    ctxMachine :: !Operand,
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
  machine <- fresh "machine"
  (context, contextParameters) <- freshContext
  setContext context
  parameters <- forM (grinFunctionParameters function) $ \var -> do
    lirVar <- fresh (varBase var)
    pure (var, lirVar, repType (grinVarRuntimeRep var))
  beginBlock (Label "entry") []
  let srt = maybe (OperandLiteral LitNull) (OperandLiteral . LitSymbol) (Map.lookup (grinFunctionName function) (envSrtSymbols env))
  roots <-
    case maximumRoots (grinFunctionBody function) of
      0 -> pure Nothing
      count -> Just . typedOperand <$> emitValue "roots" Ptr (StackAlloc (toInteger (8 * count)) (byteAlignment 8))
  foreignFrame <- if hasForeignCall needsForeignFrame (grinFunctionBody function) then Just . typedOperand <$> emitValue "foreign_frame" Ptr (StackAlloc 40 (byteAlignment 8)) else pure Nothing
  codeSlot <- if hasForeignCall (`elem` [GrinForeignDynamic, GrinForeignUnsafeDynamic]) (grinFunctionBody function) then Just . typedOperand <$> emitValue "function_pointer" Ptr (StackAlloc 8 (byteAlignment 8)) else pure Nothing
  let ctx = FunctionCtx {ctxEnv = env, ctxMachine = OperandVar machine, ctxFunctionName = grinFunctionName function, ctxSrt = srt, ctxRoots = roots, ctxCodeSlot = codeSlot, ctxForeignFrame = foreignFrame}
      valueEnv = Map.fromList [(var, Typed (OperandVar lirVar) ty) | (var, lirVar, ty) <- parameters]
  compileExpr ctx valueEnv (grinFunctionBody function)
  modify' (\state -> state {stateContext = Nothing})
  finishFunction
    (functionSymbol (grinFunctionName function))
    (if lowerExposeFunctions (envOptions env) then Export else Internal)
    ((machine, Ptr) : contextParameters <> [(lirVar, ty) | (_, lirVar, ty) <- parameters])
    []
    AihcConvention

varBase :: GrinVar -> Text
varBase var = T.filter (\character -> character /= '"' && character /= '\\') (grinVarName var)

-- | The largest root list of a reservation in the expression.
maximumRoots :: GrinExpr -> Int
maximumRoots expression =
  case expression of
    GrinBind _ (GrinEnsureHeap _ roots) body -> max (length roots) (maximumRoots body)
    GrinBind _ GrinForeignCallExpr {} body -> max (Set.size (Set.filter (isPointerRuntimeRep . grinVarRuntimeRep) (freeExprVars expression))) (maximumRoots body)
    GrinBind _ value body -> max (maximumRoots value) (maximumRoots body)
    GrinStoreRec _ body -> maximumRoots body
    GrinStoreRecUnchecked _ body -> maximumRoots body
    GrinIfWhnf _ ready slow -> max (maximumRoots ready) (maximumRoots slow)
    GrinCase _ _ alternatives -> maximum (0 : map (maximumRoots . grinAltRhs) alternatives)
    GrinEnsureHeap _ roots -> length roots
    _ -> 0

needsForeignFrame :: GrinForeignTarget -> Bool
needsForeignFrame target = case target of
  GrinForeignAddress -> False
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
      env' <- compileBinding ctx liveEnv vars value
      compileExpr ctx env' body
    GrinStoreRec {} -> unsupported "store-rec without a heap reservation"
    GrinStoreRecUnchecked bindings body -> do
      when (any (isFrameNode (ctxEnv ctx) . snd) bindings) $
        unsupported "continuation frame in a recursive store group"
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
      running <- runningArguments ctx
      terminate (TailCall eval (running <> [valueOperand, continuationOperand]))
    GrinCall _ name arguments -> do
      target <- functionTarget (ctxEnv ctx) name
      parameters <- maybe (failWith (LowerMissingFunction name)) pure (Map.lookup name (envFunctionParameters (ctxEnv ctx)))
      when (length parameters /= length arguments) $ failWith (LowerUnsupportedExpression ("call arity mismatch for " <> unFunctionName name))
      values <- mapM (materialize ctx env) arguments
      operands <- zipWithM coerce parameters values
      running <- runningArguments ctx
      terminate (TailCall target (running <> operands))
    GrinCpsPrimitiveCall runtimeRep name arguments continuation -> compileCpsPrimitive ctx env runtimeRep name arguments continuation
    GrinCpsApply _ function groups continuation -> do
      when (null groups || length groups > grinApplyGroupLimit) $
        unsupported "an application with no argument group or with too many argument groups"
      functionOperand <- pointerValue ctx env function
      continuationOperand <- pointerValue ctx env continuation
      values <- mapM (mapM (materialize ctx env)) groups
      apply <- requireHelper (HelperApply (map (map typedType) values))
      running <- runningArguments ctx
      terminate (TailCall apply (running <> (functionOperand : continuationOperand : map typedOperand (concat values))))
    GrinContinue continuation values -> do
      continuationOperand <- pointerValue ctx env continuation
      typedValues <- mapM (materialize ctx env) values
      continueTransfer ctx continuationOperand typedValues
    GrinCpsRaise exception continuation -> do
      exceptionOperand <- pointerValue ctx env exception
      continuationOperand <- pointerValue ctx env continuation
      syncMachine ctx
      resume <- callRuntime "aihc_raise" [Ptr, Ptr, Ptr] [Ptr] [ctxMachine ctx, exceptionOperand, continuationOperand]
      resumeTransfer ctx resume
    -- The exit function and the statistics read the context of the machine.
    GrinHalt _ -> do
      syncMachine ctx
      entry <- callRuntime "aihc_halt" [Ptr] [Code] [ctxMachine ctx]
      terminate (TailCallIndirect entry [ctxMachine ctx] (Signature [Ptr] [] AihcConvention))
    -- A POSIX process exits at once. A WASI P3 component records the
    -- status and halts, so the driver reports it when the machine returns.
    GrinExit status -> do
      statusOperand <- materialize ctx env status >>= coerce I64
      target <- targetM
      syncMachine ctx
      case lowerHost target of
        PosixHost -> do
          _ <- callRuntime "aihc_exit_process" [I64] [] [statusOperand]
          terminate (Trap "unreachable")
        Wasip3Host -> do
          _ <- callRuntime "aihc_set_exit_status" [Ptr, I64] [] [ctxMachine ctx, statusOperand]
          entry <- callRuntime "aihc_halt" [Ptr] [Code] [ctxMachine ctx]
          terminate (TailCallIndirect entry [ctxMachine ctx] (Signature [Ptr] [] AihcConvention))
    -- An updated thunk is an indirection until the next collection. The
    -- check follows the indirections of a variable to their target. Thus the
    -- ready branch gets the WHNF value, and no continuation is allocated.
    -- The info-table address removes the evaluating bit of the header, and a
    -- thunk under evaluation has the thunk kind. Thus that thunk goes to the
    -- slow branch.
    GrinIfWhnf value ready slow -> do
      object <- pointerValue ctx env value
      checkLabel <- freshLabel "eval_check"
      readyLabel <- freshLabel "eval_ready"
      slowLabel <- freshLabel "eval_slow"
      current <- fresh "current"
      terminate (Jump (Target checkLabel [object]))
      beginBlock checkLabel [(current, Ptr)]
      header <- loadObjectInfo (OperandVar current)
      kind <- loadInfoByte "kind" (typedOperand header) infoObjectKindByte
      followLabel <- freshLabel "eval_follow"
      -- Only a variable can take the target. Another value keeps the slow
      -- branch for an indirection.
      let slowTarget = Target slowLabel []
          follows = case value of
            GrinVarValue var -> Just var
            _ -> Nothing
          env' = maybe env (\var -> Map.insert var (Typed (OperandVar current) Ptr) env) follows
          indirectionTarget = maybe slowTarget (const (Target followLabel [])) follows
      terminate
        ( Switch
            I64
            (typedOperand kind)
            [ SwitchCase (toInteger runtimeObjectThunk) slowTarget,
              SwitchCase runtimeObjectIndirection indirectionTarget,
              SwitchCase runtimeObjectBlackhole slowTarget
            ]
            (Just (Target readyLabel []))
        )
      for_ follows $ \_ -> do
        beginBlock followLabel []
        next <- loadSlot "next" Ptr (OperandVar current) 8
        terminate (Jump (Target checkLabel [typedOperand next]))
      beginBlock readyLabel []
      branchWith (compileExpr ctx env' ready)
      beginBlock slowLabel []
      branchWith (compileExpr ctx env' slow)
    GrinCase scrutinee binder alternatives -> compileCase ctx env scrutinee binder alternatives
    GrinConstant {} -> unsupported "direct-style constant return after CPS"
    GrinStore {} -> unsupported "direct-style store return after CPS"
    GrinEnsureHeap {} -> unsupported "unbound heap reservation"
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

continueTransfer :: FunctionCtx -> Operand -> [Typed] -> LowerM ()
continueTransfer ctx continuation values = do
  continue <- requireHelper (HelperContinue (map typedType values))
  running <- runningArguments ctx
  terminate (TailCall continue (running <> (continuation : map typedOperand values)))

-- | Give control to the scheduler. The C call that selected the resumption
-- had the context of the machine, and the resumption loads the context of
-- the thread it runs.
resumeTransfer :: FunctionCtx -> Operand -> LowerM ()
resumeTransfer ctx resume = do
  helper <- requireHelper HelperResume
  terminate (TailCall helper [ctxMachine ctx, resume])

compileCpsPrimitive :: FunctionCtx -> ValueEnv -> GrinRep -> Text -> [GrinValue] -> GrinValue -> LowerM ()
compileCpsPrimitive ctx env runtimeRep name arguments continuation =
  case nativeCpsPrimitiveCall name of
    Just runtimeCall | nativeCpsCallOperandCount runtimeCall == length arguments -> do
      continuationOperand <- pointerValue ctx env continuation
      values <- mapM (materialize ctx env) arguments
      let (parameterTypes, resultType) = cpsCallSignature name
      operands <- zipWithM coerce parameterTypes values
      let symbol = nativeCpsCallSymbol runtimeCall
          callArguments = ctxMachine ctx : operands <> [continuationOperand | nativeCpsCallPassContinuation runtimeCall]
          callParameters = Ptr : parameterTypes <> [Ptr | nativeCpsCallPassContinuation runtimeCall]
      -- A CPS runtime call may collect, and it does so without the calling
      -- function's static reference table: the only code that runs after the
      -- call is the transfer below, which passes heap objects and touches no
      -- static object of this function. Keep it that way.
      syncMachine ctx
      result <- callRuntime symbol callParameters [resultType] callArguments
      case nativeCpsCallTransfer runtimeCall of
        NativeCpsEnterContinuation -> do
          reloadHeap ctx
          let resultRep = case runtimeRepComponents runtimeRep of
                rep : _ -> rep
                [] -> IntRep
          value <- coerceTo (repType resultRep) (Typed result resultType)
          continueTransfer ctx continuationOperand [value]
        NativeCpsResumeScheduler -> resumeTransfer ctx result
    _
      | lowerUnitKind (envOptions (ctxEnv ctx)) == LibraryUnit -> do
          _ <- callRuntime "aihc_unsupported_primitive" [] [] []
          terminate (Trap "unreachable")
      | otherwise -> failWith (LowerUnsupportedExpression ("CPS primitive call " <> name))

-- | The C parameter types after the machine, and the C result type, of each
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
          array <-
            case (roots, ctxRoots ctx) of
              ([], _) -> pure (OperandLiteral LitNull)
              (_, Just array) -> pure array
              _ -> failWith (LowerUnsupportedExpression "internal: roots without a root array")
          reserveHeap ctx env vars requiredWords words' roots rootOperands array
      | otherwise -> failWith (LowerUnsupportedExpression "heap reservation result arity")
    GrinUpdate pointer value -> update "aihc_update" False pointer value
    GrinUpdateBlackhole pointer value -> update "aihc_update_blackhole" True pointer value
    GrinPrimitiveCall runtimeRep name arguments -> compilePrimitive ctx env vars runtimeRep name arguments
    GrinForeignCallExpr foreignCall arguments -> do
      (results, relocated) <- protectedForeignCall ctx env foreignCall arguments
      bindVars relocated vars results
    GrinFetch tag value -> do
      object <- pointerValue ctx env value
      -- The fields start where 'initializeFields' writes them.
      let payloadShift = if isPartialConstructorNode (GrinNode tag []) then 1 else 0
      fields <- forM (zip [0 :: Int ..] vars) $ \(index, var) -> do
        typed <- loadSlot (varBase var) (repType (grinVarRuntimeRep var)) object (toInteger (8 * (index + 1 + payloadShift)))
        pure (var, typed)
      pure (Map.fromList fields `Map.union` env)
    _ -> failWith (LowerUnsupportedExpression "non-direct expression remained in a CPS bind")
  where
    -- An update neither allocates nor reads the stack pointer, so the
    -- machine does not need the context for it.
    update symbol passMachine pointer value = do
      pointerOperand <- pointerValue ctx env pointer
      valueTyped <- materialize ctx env value
      valueOperand <- coerce Ptr valueTyped
      let arguments = [ctxMachine ctx | passMachine] <> [pointerOperand, valueOperand]
      _ <- callRuntime symbol (map (const Ptr) arguments) [] arguments
      bindResults [valueTyped]
    bindResults = bindVars env vars

-- | Take the words of a reservation from the current space. The fast path is
-- the compare of the heap pointer against the heap limit and the branch
-- alone: it keeps every root and the context in the registers they already
-- sit in, so a safepoint that does not collect costs the same whatever is
-- live across it. Only the slow path spills the roots to the root array,
-- gives the machine the context, calls the collector, and reloads the roots
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
    -- A fixed size is small, so the end of the reservation cannot wrap, and
    -- the reservation fits when that end is not above the heap limit.
    GrinLitValue (GrinLitInt _ requested) -> do
      end <- emitValue "heap_end" Ptr (PtrAdd (contextHeap context) (OperandLiteral (LitInt (8 * requested))))
      emitValue "heap_fits" I1 (Compare LeU Ptr (typedOperand end) (contextHeapLimit context))
    _ -> do
      nextWord <- emitValue "heap_word" I64 (PtrToInt (contextHeap context))
      limitWord <- emitValue "heap_end_word" I64 (PtrToInt (contextHeapLimit context))
      -- The heap pointer never passes the heap limit, so this subtraction
      -- does not wrap and the free bytes are exact.
      room <- emitValue "heap_room" I64 (Binary Sub I64 (typedOperand limitWord) (typedOperand nextWord))
      -- A dynamic size brings the room down to words rather than the words up
      -- to bytes: a reservation the address space cannot hold then fails the
      -- compare instead of wrapping past it into the unchecked store behind.
      roomWords <- emitValue "heap_room_words" I64 (Binary ShrU I64 (typedOperand room) (OperandLiteral (LitInt 3)))
      emitValue "heap_fits" I1 (Compare GeU I64 (typedOperand roomWords) words')
  collect <- freshLabel "gc_collect"
  reserved <- freshLabel "gc_reserved"
  parameters <- forM vars $ \var -> do
    parameter <- fresh (varBase var)
    pure (var, parameter)
  heap <- fresh "hp"
  heapLimit <- fresh "hp_limit"
  let heapArguments current = [contextHeap current, contextHeapLimit current]
  terminate (Branch (typedOperand fits) (Target reserved (rootOperands <> heapArguments context)) (Target collect []))
  beginBlock collect []
  forM_ (zip [0 :: Int ..] rootOperands) $ \(index, root) ->
    storeSlot Ptr root array (toInteger (8 * index))
  -- The compare above is the reservation, so this is the collector rather
  -- than a second reservation that would repeat it. The function's table
  -- travels as an argument: this is the only place a collection runs on
  -- behalf of a running compiled function.
  storeContext (ctxMachine ctx) context
  _ <- callRuntime "aihc_heap_collect" [Ptr, I64, I64, Ptr, Ptr] [] [ctxMachine ctx, words', OperandLiteral (LitInt (toInteger (length roots))), array, ctxSrt ctx]
  relocated <- forM (zip [0 :: Int ..] vars) $ \(index, var) ->
    loadSlot (varBase var) Ptr array (toInteger (8 * index))
  collected <- loadHeapContext (ctxMachine ctx) context
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
  object <-
    if isFrameNode (ctxEnv ctx) node
      then pushFrame (ctxMachine ctx) (nodeWords node)
      else bumpAllocate (nodeWords node)
  storeSlot Ptr (OperandLiteral (LitSymbol info)) (typedOperand object) 0
  -- The shared info table of an unsaturated constructor does not say how wide
  -- this stage is, so the object records the count itself.
  when (isPartialConstructorNode node) $
    storeSlot I64 (OperandLiteral (LitInt (toInteger (length (grinNodeFields node))))) (typedOperand object) 8
  pure object

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

-- | Push the given words on the stack of the running thread and give back
-- the frame. The frame fits in the current chunk when the new stack pointer
-- is not above the stack limit. Otherwise the runtime continues the stack in
-- the next chunk, and the stack limit becomes the end of that chunk. Neither
-- path collects, so no root moves, and neither path reads the machine copy
-- of the context. The caller writes the header and every field.
pushFrame :: Operand -> Int -> LowerM Typed
pushFrame machine words' = do
  context <- currentContext
  (frame, stack, stackLimit) <- pushStackFrame machine (contextStack context) (contextStackLimit context) words'
  setContext context {contextStack = stack, contextStackLimit = stackLimit}
  pure (Typed frame Ptr)

-- | Push a frame on a stack with the given stack pointer and stack limit.
-- Give back the frame, the new stack pointer, and the new stack limit.
pushStackFrame :: Operand -> Operand -> Operand -> Int -> LowerM (Operand, Operand, Operand)
pushStackFrame machine stack stackLimit words' = do
  let bytes = 8 * toInteger words'
  end <- emitValue "sp" Ptr (PtrAdd stack (OperandLiteral (LitInt bytes)))
  fits <- emitValue "stack_fits" I1 (Compare LeU Ptr (typedOperand end) stackLimit)
  growLabel <- freshLabel "stack_grow"
  pushedLabel <- freshLabel "stack_pushed"
  terminate (Branch (typedOperand fits) (Target pushedLabel [stack, typedOperand end, stackLimit]) (Target growLabel []))
  beginBlock growLabel []
  grown <- callRuntime "aihc_stack_grow" [Ptr, Ptr, I64] [Ptr] [machine, stack, OperandLiteral (LitInt (toInteger words'))]
  grownEnd <- emitValue "sp" Ptr (PtrAdd grown (OperandLiteral (LitInt bytes)))
  -- The runtime gives the first frame of a chunk, so the rest of the chunk
  -- follows the frame.
  grownLimit <- emitValue "sp_limit" Ptr (PtrAdd grown (OperandLiteral (LitInt (stackChunkBytes - stackChunkHeaderBytes))))
  terminate (Jump (Target pushedLabel [grown, typedOperand grownEnd, typedOperand grownLimit]))
  pushed <- fresh "frame"
  pushedStack <- fresh "sp"
  pushedLimit <- fresh "sp_limit"
  beginBlock pushedLabel [(pushed, Ptr), (pushedStack, Ptr), (pushedLimit, Ptr)]
  pure (OperandVar pushed, OperandVar pushedStack, OperandVar pushedLimit)

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
    storeSlot (typedType typed) (typedOperand typed) (typedOperand object) (toInteger (8 * (index + 1 + payloadShift)))
  where
    payloadShift = if isPartialConstructorNode node then 1 else 0

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
    GrinForeignFunction -> compileCCall ctx env False foreignCall arguments
    GrinForeignUnsafeFunction -> compileCCall ctx env False foreignCall arguments
    GrinForeignDynamic -> compileCCall ctx env False foreignCall arguments
    GrinForeignUnsafeDynamic -> compileCCall ctx env False foreignCall arguments
    -- A callback registration neither allocates nor reads the stack
    -- pointer.
    GrinForeignWrapper _ -> case arguments of
      [closure] -> do
        operand <- pointerValue ctx env closure
        result <-
          callRuntime
            "aihc_callback_create"
            [Ptr, Ptr, Ptr, I64]
            [Ptr]
            [ctxMachine ctx, operand, OperandLiteral (LitSymbol (callbackPoolSymbol foreignCall)), OperandLiteral (LitInt callbackPoolSize)]
        pure [Typed result Ptr]
      _ -> failWith (LowerUnsupportedExpression "callback creation requires one closure")

-- | Spill live heap pointers while C can call Haskell and collect.
protectedForeignCall :: FunctionCtx -> ValueEnv -> GrinForeignCall -> [GrinValue] -> LowerM ([Typed], ValueEnv)
protectedForeignCall ctx env call arguments = case grinForeignCallTarget call of
  GrinForeignAddress -> (,env) <$> compileForeignCall ctx env call arguments
  GrinForeignWrapper _ -> (,env) <$> compileForeignCall ctx env call arguments
  target -> do
    let roots = [(var, value) | (var, value) <- Map.toAscList env, isPointerRuntimeRep (grinVarRuntimeRep var)]
        allowed = target `notElem` [GrinForeignUnsafeFunction, GrinForeignUnsafeDynamic]
    array <- case (roots, ctxRoots ctx) of
      ([], _) -> pure (OperandLiteral LitNull)
      (_, Just array) -> pure array
      _ -> failWith (LowerUnsupportedExpression "foreign call has no root array")
    mapM_ (\(index, (_, value)) -> storeSlot Ptr (typedOperand value) array (8 * index)) (zip [0 ..] roots)
    frame <- maybe (failWith (LowerUnsupportedExpression "foreign call has no stack frame")) pure (ctxForeignFrame ctx)
    -- A callback runs Haskell code on the stack of this thread, above the
    -- stack pointer, and it can allocate and collect. It gives the stack
    -- back as it found it, so only the heap context comes back.
    syncMachine ctx
    _ <-
      callRuntime
        "aihc_foreign_enter"
        [Ptr, Ptr, Ptr, I64, Ptr, I64]
        []
        [ctxMachine ctx, frame, array, OperandLiteral (LitInt (toInteger (length roots))), ctxSrt ctx, OperandLiteral (LitInt (if allowed then 1 else 0))]
    results <- compileForeignCall ctx env call arguments
    _ <- callRuntime "aihc_foreign_leave" [Ptr, Ptr] [] [ctxMachine ctx, frame]
    reloadHeap ctx
    relocated <- mapM (\(index, (var, _)) -> (var,) <$> loadSlot "foreign_root" Ptr array (8 * index)) (zip [0 ..] roots)
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
  continuationInfoItems (ContinuationSpec info (Symbol (pool <> "_applied_info")) stop [] resultTypes ContinuationFrameStop)
  -- The stop frame gives the context back to the machine: the foreign call
  -- that ran the callback loads the heap context from there.
  do
    machine <- fresh "machine"
    (context, contextParameters) <- freshContext
    values <- mapM (\ty -> (,ty) <$> fresh "result") resultTypes
    beginBlock (Label "entry") []
    result <- case values of
      [(value, ty)] -> coerce I64 (Typed (OperandVar value) ty)
      [] -> pure (OperandLiteral (LitInt 0))
      _ -> failWith (LowerUnsupportedExpression "a callback has too many results")
    storeContext (OperandVar machine) context
    _ <- callRuntime "aihc_callback_return" [Ptr, I64] [] [OperandVar machine, result]
    terminate (Return [])
    finishFunction stop Internal ((machine, Ptr) : contextParameters <> values) [] AihcConvention
  forM_ entries $ \entry -> do
    arguments <- mapM (\ty -> (,ty) <$> fresh "argument") parameters
    beginBlock (Label "entry") []
    frame <- typedOperand <$> emitValue "callback_frame" Ptr (StackAlloc 48 (byteAlignment 8))
    _ <- callRuntime "aihc_callback_enter" [Ptr, Code, Ptr] [] [frame, OperandLiteral (LitSymbol entry), OperandLiteral (LitSymbol info)]
    machine <- callRuntime "aihc_callback_machine" [Ptr] [Ptr] [frame]
    closure <- callRuntime "aihc_callback_closure" [Ptr] [Ptr] [frame]
    continuation <- callRuntime "aihc_callback_continuation" [Ptr] [Ptr] [frame]
    converted <- zipWithM (\foreignTy (value, ty) -> extendForeignResult foreignTy (Typed (OperandVar value) ty)) (grinForeignArgumentTypes signature) arguments
    apply <- requireHelper (HelperApply [rawTypes])
    context <- loadContext machine
    emit [] (Call apply (machine : contextOperands context <> (closure : continuation : map typedOperand converted)))
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
      else
        if passMachine && grinForeignCallSymbol foreignCall `Set.notMember` contextFreeRuntimeCalls
          then callSynced ctx (grinForeignCallSymbol foreignCall) parameters results (ctxMachine ctx : operands)
          else callRuntime (grinForeignCallSymbol foreignCall) parameters results ([ctxMachine ctx | passMachine] <> operands)
  case results of
    [result] -> (: []) <$> extendForeignResult (grinForeignResultType signature) (Typed resultOperand result)
    _ -> pure []

-- | The runtime calls that take the machine but neither allocate nor read
-- the stack pointer. The machine does not need the context for them.
contextFreeRuntimeCalls :: Set Text
contextFreeRuntimeCalls = Set.fromList ["aihc_my_thread_id"]

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
    (_, [left, right])
      | Just op <- lookup name binaryPrimitives -> do
          leftOperand <- word left
          rightOperand <- word right
          result <- emitValue "result" I64 (Binary op I64 leftOperand rightOperand)
          bind [result]
      | Just (op, ty) <- lookup name narrowBinaryPrimitives -> do
          leftOperand <- word left
          rightOperand <- word right
          wide <- emitValue "wide" I64 (Binary op I64 leftOperand rightOperand)
          narrow <- emitValue "narrow" ty (Convert Trunc I64 (typedOperand wide) ty)
          result <- emitValue "result" I64 (Convert ZExt ty (typedOperand narrow) I64)
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
    ("compareInt#", [left, right]) -> do
      leftOperand <- word left
      rightOperand <- word right
      less <- emitValue "less" I1 (Compare LtS I64 leftOperand rightOperand) >>= widen
      greater <- emitValue "greater" I1 (Compare GtS I64 leftOperand rightOperand) >>= widen
      result <- emitValue "result" I64 (Binary Sub I64 (typedOperand greater) (typedOperand less))
      bind [result]
    ("not#", [value]) -> do
      operand <- word value
      result <- emitValue "result" I64 (Binary Xor I64 operand (OperandLiteral (LitInt (-1))))
      bind [result]
    ("negateInt#", [value]) -> do
      operand <- word value
      result <- emitValue "result" I64 (Binary Sub I64 (OperandLiteral (LitInt 0)) operand)
      bind [result]
    ("addIntC#", [left, right]) -> signedCarry Add left right
    ("subIntC#", [left, right]) -> signedCarry Sub left right
    ("addWordC#", [left, right]) -> unsignedCarry AddCarry left right
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
      operands <- mapM word [high, low, divisor]
      helper <- requireHelper HelperQuotRem2
      quotient <- fresh "quotient"
      remainder <- fresh "remainder"
      emit [quotient, remainder] (Call helper operands)
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
    -- A computation is never duplicated, so noDuplicate# has nothing to
    -- guard against; the state token it threads carries no runtime value.
    ("noDuplicate#", [])
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
    (_, [value])
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
    (_, [object])
      | Just (ty, offset) <- lookup name objectFieldPrimitives -> do
          base <- pointerValue ctx env object
          value <- emitValue "field" ty (Load ty (byteAddress base offset) (byteAlignment 8))
          bind [value]
    ("writeMutVar#", [reference, value]) -> do
      base <- pointerValue ctx env reference
      operand <- word value
      emit [] (Store I64 operand (byteAddress base mutVarContentsOffset) (byteAlignment 8))
      bind []
    -- casMutVar# gives a failure flag, one when the contents differed from
    -- the expected value, and the final contents. The runtime runs one
    -- Haskell thread, so the swap is a plain load, compare, and store.
    ("casMutVar#", [reference, expected, replacement]) -> do
      base <- pointerValue ctx env reference
      expectedOperand <- word expected
      replacementOperand <- word replacement
      current <- emitValue "current" I64 (Load I64 (byteAddress base mutVarContentsOffset) (byteAlignment 8))
      matches <- emitValue "matches" I1 (Compare Eq I64 (typedOperand current) expectedOperand)
      final <- emitValue "final" I64 (Select I64 (typedOperand matches) replacementOperand (typedOperand current))
      emit [] (Store I64 (typedOperand final) (byteAddress base mutVarContentsOffset) (byteAlignment 8))
      unchanged <- emitValue "unchanged" I1 (Compare Ne I64 (typedOperand current) expectedOperand) >>= widen
      bind [unchanged, final]
    ("isEmptyMVar#", [mvar]) -> do
      full <- mvarFull mvar
      empty <- emitValue "empty" I1 (Compare Eq I64 (typedOperand full) (OperandLiteral (LitInt 0))) >>= widen
      bind [empty]
    -- tryReadMVar# and tryTakeMVar# each give a flag and the contents. An
    -- empty variable gives a null placeholder the caller must not look at,
    -- because the collector may find the result in a frame slot. The take
    -- is a runtime call, and the contents are read before it runs.
    ("tryReadMVar#", [mvar]) -> do
      full <- mvarFull mvar
      flag <- emitValue "flag" I1 (Compare Ne I64 (typedOperand full) (OperandLiteral (LitInt 0)))
      contents <- mvarContents mvar flag
      wide <- widen flag
      bind [wide, contents]
    ("tryTakeMVar#", [mvar])
      | Just tryCall <- nativeRuntimePrimitiveCall name -> do
          full <- mvarFull mvar
          flag <- emitValue "flag" I1 (Compare Ne I64 (typedOperand full) (OperandLiteral (LitInt 0)))
          contents <- mvarContents mvar flag
          result <- compileRuntimeCall ctx env tryCall [mvar]
          bind [result, contents]
    (_, [array, index])
      | name `elem` arrayLoadPrimitives -> do
          slot <- arrayElement array index
          value <- emitValue "element" I64 (Load I64 (byteAddress slot arrayElementsOffset) (byteAlignment 8))
          bind [value]
      | Just (ty, indexing) <- lookup name byteArrayLoadPrimitives -> do
          address <- byteArrayElement array index ty indexing
          value <- emitValue "value" ty (Load ty (byteAddress address 0) (byteAlignment 1))
          result <- if ty == I64 then pure value else emitValue "value" I64 (Convert ZExt ty (typedOperand value) I64)
          bind [result]
    (_, [array, index, value])
      | name `elem` arrayStorePrimitives -> do
          slot <- arrayElement array index
          operand <- word value
          emit [] (Store I64 operand (byteAddress slot arrayElementsOffset) (byteAlignment 8))
          bind []
      | Just (ty, indexing) <- lookup name byteArrayStorePrimitives -> do
          address <- byteArrayElement array index ty indexing
          operand <- word value
          narrow <- if ty == I64 then pure operand else typedOperand <$> emitValue "narrow" ty (Convert Trunc I64 operand ty)
          emit [] (Store ty narrow (byteAddress address 0) (byteAlignment 1))
          bind []
    _
      | Just runtimeCall <- nativeRuntimePrimitiveCall name -> do
          result <- compileRuntimeCall ctx env runtimeCall arguments
          case nativeRuntimeCallResultCount runtimeCall of
            0 | null vars -> pure env
            1 -> bind [result]
            _ -> failWith (LowerUnsupportedExpression ("runtime primitive result arity " <> name))
      | lowerUnitKind (envOptions (ctxEnv ctx)) == LibraryUnit -> do
          _ <- callRuntime "aihc_unsupported_primitive" [] [] []
          bind [zeroValue (repType (grinVarRuntimeRep var)) | var <- vars]
      | otherwise -> failWith (LowerUnsupportedPrimitive name)
  where
    bind = bindVars env vars
    word value = materialize ctx env value >>= coerce I64
    widen (Typed flag _) = emitValue "wide" I64 (Convert ZExt I1 flag I64)
    zeroValue ty = Typed (OperandLiteral (if ty == Ptr then LitNull else LitInt 0)) ty
    -- Whether an MVar holds a value, as a word: its flag is one byte.
    mvarFull mvar = do
      base <- pointerValue ctx env mvar
      flag <- emitValue "full" I8 (Load I8 (byteAddress base mvarFullOffset) (byteAlignment 1))
      emitValue "full" I64 (Convert ZExt I8 (typedOperand flag) I64)
    -- The contents of an MVar when @full@ holds, and null otherwise.
    mvarContents mvar (Typed full _) = do
      base <- pointerValue ctx env mvar
      value <- emitValue "value" I64 (Load I64 (byteAddress base mvarValueOffset) (byteAlignment 8))
      emitValue "contents" I64 (Select I64 full (typedOperand value) (OperandLiteral (LitInt 0)))
    checkPrimBounds = lowerCheckPrimBounds (envOptions (ctxEnv ctx))
    -- Leave the current block for one whose entry means the bounds check
    -- passed. The other successor reports the failure and never returns.
    boundsCheck invalid failure message = do
      failed <- freshLabel "out_of_bounds"
      inside <- freshLabel "inside"
      terminate (Branch invalid (Target failed []) (Target inside []))
      beginBlock failed []
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
        count <- emitValue "count" I64 (Load I64 (byteAddress base arrayLengthOffset) (byteAlignment 8))
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
        size <- emitValue "size" I64 (Load I64 (byteAddress base byteArraySizeOffset) (byteAlignment 8))
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
      contents <- emitValue "contents" Ptr (Load Ptr (byteAddress base byteArrayContentsOffset) (byteAlignment 8))
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

-- | The mask of one stage of a 64-bit byte swap. The mask keeps every other
-- block of @stage@ bits.
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
    ("uncheckedShiftL#", Shl),
    ("uncheckedShiftRL#", ShrU),
    ("uncheckedIShiftL#", Shl),
    ("uncheckedIShiftRA#", ShrS),
    ("uncheckedIShiftRL#", ShrU),
    ("uncheckedShiftL64#", Shl),
    ("uncheckedShiftRL64#", ShrU)
  ]

-- | Binary operations whose result is a sized word. The operation runs at
-- the width of a word and the result keeps only its low bits, which is how
-- a @Word16#@ or @Word32#@ shift wraps. The bitwise operations cannot
-- overflow their width, so for them the truncation only restates the width
-- their operands already have.
narrowBinaryPrimitives :: [(Text, (BinaryOp, Type))]
narrowBinaryPrimitives =
  [ ("uncheckedShiftLWord16#", (Shl, I16)),
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

comparisonPrimitives :: [(Text, CompareOp)]
comparisonPrimitives =
  [ ("<#", LtS),
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
    ("eqWord8#", Eq),
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

mvarFullOffset, mvarValueOffset :: Integer
mvarFullOffset = 8
mvarValueOffset = 16

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
  [ ("readMutVar#", (I64, mutVarContentsOffset)),
    ("sizeofArray#", (I64, arrayLengthOffset)),
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

-- | How a byte-array primitive names an element: by an index that counts
-- elements of the element width, or by a byte offset.
data ByteArrayIndexing = ElementIndex | ByteOffset
  deriving (Eq, Show)

-- | Reads of one element of a byte array. Each entry gives the width of
-- the element, which widens to a word by zero extension. The atomic
-- primitives are plain accesses, because the runtime runs one Haskell
-- thread.
byteArrayLoadPrimitives :: [(Text, (Type, ByteArrayIndexing))]
byteArrayLoadPrimitives =
  [ ("indexWordArray#", (I64, ElementIndex)),
    ("readWordArray#", (I64, ElementIndex)),
    ("atomicReadIntArray#", (I64, ElementIndex)),
    ("indexWord8Array#", (I8, ElementIndex)),
    ("readWord8Array#", (I8, ElementIndex)),
    ("indexWord16Array#", (I16, ElementIndex)),
    ("readWord16Array#", (I16, ElementIndex)),
    ("indexWord32Array#", (I32, ElementIndex)),
    ("readWord32Array#", (I32, ElementIndex)),
    ("indexWord64Array#", (I64, ElementIndex)),
    ("readWord64Array#", (I64, ElementIndex)),
    ("indexCharArray#", (I8, ByteOffset)),
    ("readCharArray#", (I8, ByteOffset)),
    ("indexWord8ArrayAsWord16#", (I16, ByteOffset)),
    ("readWord8ArrayAsWord16#", (I16, ByteOffset)),
    ("indexWord8ArrayAsWord32#", (I32, ByteOffset)),
    ("readWord8ArrayAsWord32#", (I32, ByteOffset)),
    ("indexWord8ArrayAsWord64#", (I64, ByteOffset)),
    ("readWord8ArrayAsWord64#", (I64, ByteOffset))
  ]

-- | Writes of one element of a byte array, with the widths and indexing of
-- 'byteArrayLoadPrimitives'.
byteArrayStorePrimitives :: [(Text, (Type, ByteArrayIndexing))]
byteArrayStorePrimitives =
  [ ("writeWordArray#", (I64, ElementIndex)),
    ("atomicWriteIntArray#", (I64, ElementIndex)),
    ("writeWord8Array#", (I8, ElementIndex)),
    ("writeWord16Array#", (I16, ElementIndex)),
    ("writeWord32Array#", (I32, ElementIndex)),
    ("writeWord64Array#", (I64, ElementIndex)),
    ("writeCharArray#", (I8, ByteOffset)),
    ("writeWord8ArrayAsWord16#", (I16, ByteOffset)),
    ("writeWord8ArrayAsWord32#", (I32, ByteOffset)),
    ("writeWord8ArrayAsWord64#", (I64, ByteOffset))
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
    ("popCnt#", Popcount)
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
    ("ltFloat#", (FLt, F32)),
    ("eqFloat#", (Eq, F32)),
    (">##", (FGt, F64)),
    ("<##", (FLt, F64)),
    ("==##", (Eq, F64))
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

identityPrimitives :: [Text]
identityPrimitives =
  [ "int2Word#",
    "word2Int#",
    "word8ToWord#",
    "word32ToWord#",
    "word64ToWord#",
    "wordToWord64#",
    "word16ToWord#",
    "ord#",
    "chr#",
    "unsafeFreezeArray#",
    "unsafeThawArray#",
    "unsafeFreezeSmallArray#",
    "unsafeThawSmallArray#",
    "unsafeFreezeByteArray#",
    "unsafeThawByteArray#",
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
compileCase ctx env scrutinee binder alternatives = do
  typed <- materialize ctx env scrutinee
  binderValue <- coerceTo (repType (grinVarRuntimeRep binder)) typed
  let env' = Map.insert binder binderValue env
      isPointer = isPointerRuntimeRep (grinValueRuntimeRep scrutinee)
  targets <- forM alternatives $ \alternative -> do
    label <- freshLabel "alt"
    pure (alternative, label)
  let defaultTargets = [label | (alternative, label) <- targets, grinAltCon alternative == GrinDefaultAlt]
  fallback <-
    case defaultTargets of
      label : _ -> pure label
      [] -> freshLabel "no_match"
  if isPointer
    then do
      header <- loadObjectInfo (typedOperand typed)
      identity <- loadInfoPointer "identity" (typedOperand header) 0
      checks <- forM [(alternative, label) | (alternative, label) <- targets, grinAltCon alternative /= GrinDefaultAlt] $ \(alternative, label) ->
        case grinAltCon alternative of
          GrinDataAlt name -> do
            let symbol = constructorInfoSymbol name 0
            unless (Map.member (ConstructorRuntimeInfo name SaturatedConstructor) (envInfoSymbols (ctxEnv ctx))) (requireExternData symbol)
            pure (symbol, label)
          _ -> failWith (LowerUnsupportedExpression "literal case on a lifted value")
      pointerChecks identity checks fallback
    else do
      wordValue <- coerce I64 typed
      cases <- forM [(alternative, label) | (alternative, label) <- targets, grinAltCon alternative /= GrinDefaultAlt] $ \(alternative, label) ->
        case grinAltCon alternative of
          GrinLitAlt literal ->
            case normalizedLiteralInteger literal of
              Just integer -> pure (SwitchCase integer (Target label []))
              Nothing -> failWith (LowerUnsupportedValue "string case alternative")
          _ -> failWith (LowerUnsupportedExpression "constructor case on an unboxed value")
      terminate (Switch I64 wordValue (firstCases cases) (Just (Target fallback [])))
  when (null defaultTargets) $ do
    beginBlock fallback []
    _ <- callRuntime "aihc_no_match" [] [] []
    terminate (Trap "no matching case alternative")
  forM_ targets $ \(alternative, label) -> branchWith $ do
    beginBlock label []
    env'' <- bindAlternative alternative typed env'
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
    bindAlternative alternative typed env' =
      case grinAltCon alternative of
        GrinDataAlt _ -> do
          let live = freeExprVars (grinAltRhs alternative)
          bound <- forM [(index, field) | (index, field) <- zip [0 :: Int ..] (grinAltBinders alternative), field `Set.member` live] $ \(index, field) -> do
            let ty = repType (grinVarRuntimeRep field)
            value <- loadSlot (varBase field) ty (typedOperand typed) (toInteger (8 * (index + 1)))
            pure (field, value)
          pure (Map.fromList bound `Map.union` env')
        GrinLitAlt _ -> pure env'
        GrinDefaultAlt -> do
          bound <- forM (grinAltBinders alternative) $ \field -> do
            value <- coerceTo (repType (grinVarRuntimeRep field)) typed
            pure (field, value)
          pure (Map.fromList bound `Map.union` env')

-- Executable entry

-- | The @main@ of the executable and the special continuations: the top
-- continuation applies the evaluated entry, the final continuation halts,
-- the update continuation is the GC-GRIN update function, and the thread
-- done continuation returns to the scheduler. @main@ hands the arguments
-- and the environment to the runtime before the machine starts, and it
-- reports the runtime statistics when the machine halts. An exit through
-- @aihc_exit_process@ reports them itself.
lowerExecutableMain :: GcGrinProgram -> LowerM ()
lowerExecutableMain gcProgram = do
  entryItems gcProgram
  argc <- fresh "argc"
  argv <- fresh "argv"
  beginBlock (Label "entry") []
  _ <- callRuntime "aihc_machine_initialize" [] [Ptr] []
  _ <- callRuntime "aihc_program_arguments_initialize" [I32, Ptr] [] [OperandVar argc, OperandVar argv]
  _ <- callRuntime "aihc_program_environment_initialize" [] [] []
  _ <- startMachine
  _ <- callRuntime "aihc_runtime_statistics_report" [] [] []
  terminate (Return [OperandLiteral (LitInt 0)])
  finishFunction (Symbol "main") Export [(argc, I32), (argv, Ptr)] [I32] CConvention

-- | The WASI P3 entry unit. The P3 driver initializes the program
-- arguments, calls @aihc_lir_program_start@, and calls
-- @aihc_lir_program_resume@ with the scheduler resumption of every
-- completed IO request. Both return one when the program has halted and zero
-- when every thread waits for IO. The machine is published in the C global
-- @aihc_machine@ for the driver.
lowerWasip3Entry :: GcGrinProgram -> LowerM ()
lowerWasip3Entry gcProgram = do
  entryItems gcProgram
  requireExternData wasmMachineSymbol
  do
    beginBlock (Label "entry") []
    machine <- startMachine
    emit [] (Store Ptr machine (byteAddress (OperandLiteral (LitSymbol wasmMachineSymbol)) 0) (wordAlignment 1))
    finished <- loadFinished
    terminate (Return [finished])
    finishFunction (Symbol "aihc_lir_program_start") Export [] [I32] CConvention
  do
    resume <- fresh "resume"
    beginBlock (Label "entry") []
    machine <- emitValue "machine" Ptr (Load Ptr (byteAddress (OperandLiteral (LitSymbol wasmMachineSymbol)) 0) (wordAlignment 1))
    helper <- requireHelper HelperResume
    emit [] (Call helper [typedOperand machine, OperandVar resume])
    finished <- loadFinished
    terminate (Return [finished])
    finishFunction (Symbol "aihc_lir_program_resume") Export [(resume, Ptr)] [I32] CConvention
  where
    loadFinished = do
      flag <- emitValue "finished" I64 (Load I64 (byteAddress (OperandLiteral (LitSymbol finishedSymbol)) 0) (byteAlignment 8))
      typedOperand <$> emitValue "finished" I32 (Convert Trunc I64 (typedOperand flag) I32)

wasmMachineSymbol :: Symbol
wasmMachineSymbol = Symbol "aihc_machine"

-- | The word the exit function sets when the machine halts.
finishedSymbol :: Symbol
finishedSymbol = Symbol "aihc_lir_finished"

-- | The special continuations of an executable: the top continuation
-- applies the evaluated entry, the final continuation halts, and the thread done
-- continuation returns to the scheduler.
entryItems :: GcGrinProgram -> LowerM ()
entryItems _ = do
  requireExternData (globalSymbol executableEntryName)
  continuationInfoItems (ContinuationSpec finalInfo (Symbol "aihc_lir_final_applied_info") finalTarget [] [Ptr] ContinuationFrameStop)
  continuationInfoItems (ContinuationSpec topInfo (Symbol "aihc_lir_top_applied_info") topTarget [Ptr] [Ptr] ContinuationFrameNormal)
  continuationInfoItems (ContinuationSpec threadDoneInfo (Symbol "aihc_lir_thread_done_applied_info") threadDoneTarget [] [Ptr] ContinuationFrameStop)
  emitItem (ItemData (DataItem finishedSymbol Internal True 8 [DataInt I64 0]))
  -- The top continuation applies the evaluated entry action to no arguments
  -- with the final continuation.
  do
    machine <- fresh "machine"
    (context, contextParameters) <- freshContext
    final <- fresh "final"
    result <- fresh "result"
    beginBlock (Label "entry") []
    apply <- requireHelper (HelperApply [[]])
    terminate (TailCall apply (OperandVar machine : contextOperands context <> [OperandVar result, OperandVar final]))
    finishFunction topTarget Internal ((machine, Ptr) : contextParameters <> [(final, Ptr), (result, Ptr)]) [] AihcConvention
  -- The exit function and the statistics read the context of the machine.
  do
    machine <- fresh "machine"
    (context, contextParameters) <- freshContext
    value <- fresh "value"
    beginBlock (Label "entry") []
    storeContext (OperandVar machine) context
    entry <- callRuntime "aihc_halt" [Ptr] [Code] [OperandVar machine]
    terminate (TailCallIndirect entry [OperandVar machine] (Signature [Ptr] [] AihcConvention))
    finishFunction finalTarget Internal ((machine, Ptr) : contextParameters <> [(value, Ptr)]) [] AihcConvention
  threadDoneContinuation threadDoneTarget
  where
    finalTarget = Symbol "aihc_lir_final_continuation"
    topTarget = Symbol "aihc_lir_top_continuation"
    threadDoneTarget = Symbol "aihc_lir_thread_done_continuation"

finalInfo, topInfo, threadDoneInfo :: Symbol
finalInfo = Symbol "aihc_lir_final_info"
topInfo = Symbol "aihc_lir_top_info"
threadDoneInfo = Symbol "aihc_lir_thread_done_info"

-- | Create the machine and its continuations and evaluate the entry. The
-- call returns when the machine halts or when every thread waits for IO.
startMachine :: LowerM Operand
startMachine = do
  exit <- requireHelper HelperExit
  let entryGlobal = globalSymbol executableEntryName
  machine <- callRuntime "aihc_machine_new" [I64] [Ptr] [OperandLiteral (LitInt 0)]
  final <- allocateContinuation machine finalInfo 1
  top <- allocateContinuation machine topInfo 2
  storeSlot Ptr final top 8
  threadDone <- allocateContinuation machine threadDoneInfo 1
  _ <- callRuntime "aihc_set_thread_done_continuation" [Ptr, Ptr] [] [machine, threadDone]
  -- The halt path returns through the exit function to the caller.
  emit [] (Store Code (OperandLiteral (LitSymbol exit)) (byteAddress machine machineExitCodeOffset) (wordAlignment 1))
  emit [] (Store I64 (OperandLiteral (LitInt 0)) (byteAddress (OperandLiteral (LitSymbol finishedSymbol)) 0) (byteAlignment 8))
  eval <- requireHelper HelperEval
  context <- loadContext machine
  emit [] (Call eval (machine : contextOperands context <> [OperandLiteral (LitSymbol entryGlobal), top]))
  pure machine

-- | One continuation of an entry, pushed on the stack of the running
-- thread: an info-table pointer and the captured slots the caller fills.
-- The push goes through the machine, so it comes before the code loads the
-- context. Exported for harnesses that build their own entry.
allocateContinuation :: Operand -> Symbol -> Int -> LowerM Operand
allocateContinuation machine info words' = do
  object <- callRuntime "aihc_stack_push" [Ptr, I64] [Ptr] [machine, OperandLiteral (LitInt (toInteger words'))]
  storeSlot Ptr (OperandLiteral (LitSymbol info)) object 0
  pure object

-- | The continuation that a finished thread enters. Exported for harnesses
-- that build their own entry.
threadDoneContinuation :: Symbol -> LowerM ()
threadDoneContinuation target = do
  machine <- fresh "machine"
  (context, contextParameters) <- freshContext
  value <- fresh "value"
  beginBlock (Label "entry") []
  storeContext (OperandVar machine) context
  resume <- callRuntime "aihc_thread_done" [Ptr] [Ptr] [OperandVar machine]
  helper <- requireHelper HelperResume
  terminate (TailCall helper [OperandVar machine, resume])
  finishFunction target Internal ((machine, Ptr) : contextParameters <> [(value, Ptr)]) [] AihcConvention

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
    -- The exit function records that the machine halted and returns to the
    -- function that started the machine.
    HelperExit -> do
      machine <- fresh "machine"
      beginBlock (Label "entry") []
      when (lowerUnitKind (envOptions env) == ExecutableUnit) $
        emit [] (Store I64 (OperandLiteral (LitInt 1)) (byteAddress (OperandLiteral (LitSymbol finishedSymbol)) 0) (byteAlignment 8))
      terminate (Return [])
      finishFunction symbol Internal [(machine, Ptr)] [] AihcConvention
    HelperContinue shape -> do
      machine <- fresh "machine"
      (context, contextParameters) <- freshContext
      continuation <- fresh "continuation"
      values <- forM shape $ \ty -> (,ty) <$> fresh "value"
      beginBlock (Label "entry") []
      terminate (Jump (Target (Label "loop") [OperandVar continuation]))
      current <- fresh "current"
      beginBlock (Label "loop") [(current, Ptr)]
      header <- loadHeader (OperandVar current)
      kind <- loadInfoByte "kind" header infoObjectKindByte
      isIndirection <- emitValue "indirection" I1 (Compare Eq I64 (typedOperand kind) (OperandLiteral (LitInt runtimeObjectIndirection)))
      frame <- loadInfoByte "frame" header infoFrameKindByte
      isForward <- emitValue "forward" I1 (Compare Eq I64 (typedOperand frame) (OperandLiteral (LitInt (toInteger (continuationFrameKindCode (Just ContinuationFrameForward))))))
      follows <- emitValue "follows" I1 (Binary Or I1 (typedOperand isIndirection) (typedOperand isForward))
      terminate (Branch (typedOperand follows) (Target (Label "indirection") []) (Target (Label "enter") []))
      beginBlock (Label "indirection") []
      next <- loadSlot "next" Ptr (OperandVar current) 8
      terminate (Jump (Target (Label "loop") [typedOperand next]))
      beginBlock (Label "enter") []
      -- Entering a frame pops it and every frame above it: the frame is
      -- the new stack pointer, and its chunk gives the stack limit. The
      -- stack pointer is a value of its own, so the frame can stay in the
      -- register of the object argument and only the copy moves.
      stack <- emitValue "sp" Ptr (PtrAdd (OperandVar current) (OperandLiteral (LitInt 0)))
      stackLimit <- chunkEnd (OperandVar current)
      entry <- loadInfoCode "entry" header infoBackendEntryIndex
      let entered = context {contextStack = typedOperand stack, contextStackLimit = stackLimit}
      terminate
        ( TailCallIndirect
            (typedOperand entry)
            (OperandVar machine : contextOperands entered <> (OperandVar current : OperandLiteral LitNull : [OperandVar var | (var, _) <- values]))
            (Signature (Ptr : contextTypes <> (Ptr : Ptr : shape)) [] AihcConvention)
        )
      finishFunction symbol Internal ((machine, Ptr) : contextParameters <> ((continuation, Ptr) : values)) [] AihcConvention
    HelperApply [shape] -> do
      target <- targetM
      let word = toInteger (lowerWordSize target)
      machine <- fresh "machine"
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
      header <- loadHeader (OperandVar current)
      kind <- loadInfoByte "kind" header infoObjectKindByte
      isIndirection <- emitValue "indirection" I1 (Compare Eq I64 (typedOperand kind) (OperandLiteral (LitInt runtimeObjectIndirection)))
      terminate (Branch (typedOperand isIndirection) (Target (Label "indirection") []) (Target (Label "apply") []))
      beginBlock (Label "indirection") []
      next <- loadSlot "next" Ptr (OperandVar current) 8
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
            (OperandVar machine : contextOperands context <> (OperandVar current : OperandVar continuation : [OperandVar var | (var, _) <- values]))
            (Signature (Ptr : contextTypes <> (Ptr : Ptr : shape)) [] AihcConvention)
        )
      beginBlock (Label "slow") []
      forM_ (zip [0 :: Int ..] values) $ \(index, (var, ty)) ->
        storeSlot ty (OperandVar var) arguments (toInteger (8 * index))
      applySlow machine context current continuation continuationSlot arguments 1 (length shape)
      finishFunction symbol Internal ((machine, Ptr) : contextParameters <> ((function, Ptr) : (continuation, Ptr) : values)) [] AihcConvention
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
      machine <- fresh "machine"
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
      header <- loadHeader (OperandVar current)
      kind <- loadInfoByte "kind" header infoObjectKindByte
      isIndirection <- emitValue "indirection" I1 (Compare Eq I64 (typedOperand kind) (OperandLiteral (LitInt runtimeObjectIndirection)))
      terminate (Branch (typedOperand isIndirection) (Target (Label "indirection") []) (Target (Label "apply") []))
      beginBlock (Label "indirection") []
      next <- loadSlot "next" Ptr (OperandVar current) 8
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
            (OperandVar machine : contextOperands context <> (OperandVar current : OperandVar continuation : [OperandVar var | (var, _) <- values]))
            (Signature (Ptr : contextTypes <> (Ptr : Ptr : concat groups)) [] AihcConvention)
        )
      forM_ [1 .. count - 1] $ \taken -> do
        beginBlock (overLabel taken) []
        let (supplied, held) = splitAt taken valueGroups
        applyOverSaturated machine context (OperandVar current) (OperandVar continuation) header supplied held
      beginBlock (Label "slow") []
      forM_ (zip [0 :: Int ..] values) $ \(index, (var, ty)) ->
        storeSlot ty (OperandVar var) arguments (toInteger (8 * index))
      applySlow machine context current continuation continuationSlot arguments count (length values)
      finishFunction symbol Internal ((machine, Ptr) : contextParameters <> ((function, Ptr) : (continuation, Ptr) : values)) [] AihcConvention
    -- The frame applies the function that arrives to the groups it holds,
    -- with the parent as the continuation.
    HelperApplyFrame groups -> do
      continuationInfoItems (ContinuationSpec (applyFrameInfoSymbol groups) (applyFrameAppliedInfoSymbol groups) symbol (Ptr : concat groups) [Ptr] ContinuationFrameNormal)
      machine <- fresh "machine"
      (context, contextParameters) <- freshContext
      parent <- fresh "parent"
      values <- forM (concat groups) $ \ty -> (,ty) <$> fresh "value"
      applied <- fresh "applied"
      beginBlock (Label "entry") []
      apply <- requireHelper (HelperApply groups)
      terminate (TailCall apply (OperandVar machine : contextOperands context <> (OperandVar applied : OperandVar parent : [OperandVar var | (var, _) <- values])))
      finishFunction symbol Internal ((machine, Ptr) : contextParameters <> ((parent, Ptr) : values <> [(applied, Ptr)])) [] AihcConvention
    _ -> failWith (LowerUnsupportedExpression "internal: shared helper requested a local definition")
  where
    symbol = helperSymbol helper
    loadHeader object = typedOperand <$> loadObjectInfo object
    overLabel taken = Label ("over_" <> T.pack (show taken))

-- | The slow path of an apply helper: the runtime applies the function to
-- the values in the argument array, and the result goes to the continuation
-- the runtime leaves in the continuation slot. The runtime can allocate and
-- collect, so the machine gets the context first.
applySlow :: Var -> Context -> Var -> Var -> Operand -> Operand -> Int -> Int -> LowerM ()
applySlow machine context current continuation continuationSlot arguments groups count = do
  -- The continuation slot is a C pointer variable, not a heap slot.
  emit [] (Store Ptr (OperandVar continuation) (byteAddress continuationSlot 0) (wordAlignment 1))
  storeContext (OperandVar machine) context
  applied <- callRuntime "aihc_apply_slow" [Ptr, Ptr, I64, I64, Ptr, Ptr] [Ptr] [OperandVar machine, OperandVar current, OperandLiteral (LitInt (toInteger groups)), OperandLiteral (LitInt (toInteger count)), arguments, continuationSlot]
  collected <- loadHeapContext (OperandVar machine) context
  adjusted <- emitValue "adjusted" Ptr (Load Ptr (byteAddress continuationSlot 0) (wordAlignment 1))
  continue <- requireHelper (HelperContinue [Ptr])
  terminate (TailCall continue (OperandVar machine : contextOperands collected <> [typedOperand adjusted, applied]))

-- | The block of an apply helper for a closure that takes fewer groups than
-- the application supplies. It pushes a frame that holds the other groups
-- on the stack of the thread, then enters the closure with the groups it
-- takes. A push does not collect, so no value moves.
applyOverSaturated :: Var -> Context -> Operand -> Operand -> Operand -> [[(Var, Type)]] -> [[(Var, Type)]] -> LowerM ()
applyOverSaturated machine context function continuation header supplied held = do
  let heldTypes = map (map snd) held
      heldValues = concat held
  _ <- requireHelper (HelperApplyFrame heldTypes)
  (frame, stack, stackLimit) <- pushStackFrame (OperandVar machine) (contextStack context) (contextStackLimit context) (2 + length heldValues)
  storeSlot Ptr (OperandLiteral (LitSymbol (applyFrameInfoSymbol heldTypes))) frame 0
  storeSlot Ptr continuation frame 8
  forM_ (zip [0 :: Int ..] heldValues) $ \(index, (var, ty)) ->
    storeSlot ty (OperandVar var) frame (toInteger (8 * (index + 2)))
  entry <- loadInfoCode "entry" header infoBackendEntryIndex
  let pushed = context {contextStack = stack, contextStackLimit = stackLimit}
  terminate
    ( TailCallIndirect
        (typedOperand entry)
        (OperandVar machine : contextOperands pushed <> (function : frame : [OperandVar var | (var, _) <- concat supplied]))
        (Signature (Ptr : contextTypes <> (Ptr : Ptr : map snd (concat supplied))) [] AihcConvention)
    )

-- | The word fields of an info table precede its byte fields. See the
-- "Info tables" section of @docs/lir.md@.
infoWordFieldCount, infoBackendEntryIndex :: Int
infoWordFieldCount = 5
infoBackendEntryIndex = 3

-- | The byte fields of an info table, as indices from the first byte field.
infoRemainingArityByte, infoFrameKindByte, infoObjectKindByte :: Int
infoRemainingArityByte = 1
infoFrameKindByte = 2
infoObjectKindByte = 3

-- | The largest count a byte field of an info table holds.
infoByteFieldLimit :: Int
infoByteFieldLimit = 255

runtimeObjectNode, runtimeObjectClosure, runtimeObjectThunk, runtimeObjectPartialConstructor :: Int
runtimeObjectNode = 0
runtimeObjectClosure = 1
runtimeObjectThunk = 2
runtimeObjectPartialConstructor = 3

runtimeObjectIndirection, runtimeObjectBlackhole :: Integer
runtimeObjectIndirection = 4
runtimeObjectBlackhole = 5

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
        stages current remaining =
          ClosureRuntimeInfo name current remaining : case remaining of
            [] -> []
            layout : rest -> stages (current <> layout) rest
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
    ThunkRuntimeInfo _ fields -> fields

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
    ClosureRuntimeInfo name fields (layout : rest) -> Just (ClosureRuntimeInfo name (fields <> layout) rest)
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
