{-# LANGUAGE OverloadedStrings #-}

-- | Architecture-neutral LIR walk for the native backends.
--
-- Each backend supplies encoding and register names. This module walks
-- traps, data, globals, frames, slot elision, blocks, terminators, and
-- instructions.
module Aihc.Native.Lir
  ( BranchTest (..),
    Ctx (..),
    Fused (..),
    Fusion (..),
    Layout (..),
    Location (..),
    MoveSource (..),
    NativeBackend (..),
    NativeM,
    ObjectState (..),
    SlotEffect (..),
    Source (..),
    blockArgumentMoves,
    calleeSignature,
    compileNativeStatements,
    compileNativeStatementsWith,
    compileNativeChunksWith,
    compileNativeTo,
    compileNativeItemTo,
    finishNativeTo,
    initialObjectState,
    displaceSource,
    elideSlotReloadsWith,
    frameBytes,
    freshLabel,
    home,
    literalBits,
    log2,
    operandIn,
    operandTo,
    overflowBytes,
    parallelMove,
    resultIn,
    trapLabel,
    typeBytes,
    unsupported,
  )
where

import Aihc.Lir.Division (lowerWideDivision)
import Aihc.Lir.Inline (prepareCheckedModule, prepareModule)
import Aihc.Lir.Lint (LintError)
import Aihc.Lir.RegAlloc (Allocation (..), Registers, allocateRegistersFor, operationReads, readCounts, terminatorReads)
import Aihc.Lir.Resolve (resolvedSwitchCaseValue, unresolvedConstant)
import Aihc.Lir.Syntax
import Aihc.Native.Move (orderMoves)
import Aihc.Native.Object (Name (..), SectionRole (..))
import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.State.Strict (StateT (..), evalStateT, execStateT, get, mapStateT, modify, modify', put, runState, runStateT)
import Data.ByteString qualified as BS
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Float (castDoubleToWord64, castFloatToWord32, double2Float)

-- Object state

-- | Trap messages of one object, and the next private label index.
data ObjectState = ObjectState
  { objectTraps :: !(Map Text Int),
    -- | The number of the next fresh label, which names it.
    objectNextLabel :: !Int,
    -- | The number of the next private label of any kind.
    objectNextId :: !Int,
    -- | Whether a trap branch goes to a trampoline of the function rather
    -- than to the stub of the object. A backend whose conditional branch
    -- has a short reach keeps the stub within reach this way.
    objectLocalTraps :: !Bool,
    -- | The index of the function being compiled, and the traps it has
    -- referenced so far, by message.
    objectFunctionIndex :: !Int,
    objectFunctionTraps :: !(Map Text (Int, Name))
  }

type NativeM error = StateT ObjectState (Either error)

-- | Encoding and register names of one native backend.
data NativeBackend statement register error = NativeBackend
  { nbLintErrors :: [LintError] -> error,
    nbUnsupported :: Text -> error,
    nbSymbol :: Symbol -> Text,
    -- | The integer argument registers of the C convention.
    nbArgumentRegisters :: ![register],
    -- | The argument registers of the aihc convention. They can differ from
    -- the C registers and there can be more of them.
    nbAihcArgumentRegisters :: ![register],
    nbResultRegisters :: ![register],
    nbPreservedRegisters :: ![register],
    nbScratchLeft :: register,
    nbScratchRight :: register,
    nbCycleScratch :: register,
    nbSlotMoveScratch :: register,
    nbFrameOverhead :: Int,
    nbReturnAddressGap :: Int,
    nbMaxFrameBytes :: !(Maybe Int),
    nbCodeAlign :: Int,
    nbAfterObject :: ![statement],
    nbRegistersFor :: CallingConvention -> Function -> Registers register,
    nbSection :: SectionRole -> statement,
    nbAlign :: Int -> statement,
    nbGlobal :: Text -> statement,
    nbLabel :: Name -> statement,
    nbBytes :: BS.ByteString -> statement,
    nbWord :: Int -> Word64 -> statement,
    nbQuad :: Word64 -> statement,
    nbQuadSymbol :: Text -> statement,
    nbQuadSymbolAddend :: Text -> Int64 -> statement,
    nbAsCode :: statement -> Maybe SlotEffect,
    nbRenderTraps :: [(Text, Int)] -> [statement],
    -- | A trampoline of one function: its local label and an unconditional
    -- branch to the trap stub with the given label. A backend whose
    -- conditional branch reaches the whole object gives 'Nothing'.
    nbTrapTrampoline :: !(Maybe (Name -> Text -> [statement])),
    nbPrologueFrame :: Bool -> Int -> [statement],
    -- | The moves of the C parameters to their homes, including the stack
    -- parameters. The C parameter layout is specific to each target.
    nbCParameterMoves :: Ctx register -> [statement],
    -- | Some C tail calls require a call and return with a saved return address.
    nbTailCallFrame :: Map Symbol Signature -> Function -> Bool,
    nbLeaveFrame :: Ctx register -> Int -> [statement],
    -- | The frame of one block that calls, in a function without a frame
    -- for its full body: the bytes above the stack pointer where the stack
    -- allocations start, and the statements that make and remove a frame
    -- of the given size. The size includes those bytes and is a multiple
    -- of 16. See 'blockFrameLabels'.
    nbBlockFrameBase :: Int,
    nbBlockFrameEnter :: Int -> [statement],
    nbBlockFrameLeave :: Int -> [statement],
    nbSaveReg :: register -> Int -> statement,
    nbReturn :: Ctx register -> [statement],
    nbLoadSlot :: register -> Int -> statement,
    nbStoreSlot :: register -> Int -> statement,
    -- | Store registers at frame slots, and load them, in the order given.
    -- A backend can join two slots that are next to each other.
    nbStoreSlots :: [(register, Int)] -> [statement],
    nbLoadSlots :: [(register, Int)] -> [statement],
    nbMove :: register -> register -> [statement],
    nbLiteralInto :: Type -> register -> Literal -> [statement],
    nbStoreSlotImmediate :: Int -> Integer -> Maybe statement,
    nbCanonicalize :: Type -> register -> [statement],
    nbFloatFromVec :: Type -> Int -> register -> [statement],
    nbFloatToVec :: Type -> register -> Int -> [statement],
    nbJump :: Name -> statement,
    nbCanFuseFloatCompare :: CompareOp -> Bool,
    nbConditionTest :: Ctx register -> Maybe Fused -> Operand -> NativeM error ([statement], BranchTest statement error),
    nbCompareAndBranchEqual :: Ctx register -> Type -> register -> Integer -> Name -> [statement],
    nbCReturnFloat :: CallingConvention -> [Type] -> [statement],
    -- | Select a logical operation with a constant rotated right operand.
    nbRotatedBinary :: Maybe (BinaryOp -> Type -> register -> register -> register -> Int -> [statement]),
    -- | Select an addition of an operand that is shifted left by a constant:
    -- the destination, the left operand, the operand to shift, and the count.
    nbShiftedAdd :: Maybe (register -> register -> register -> Int -> [statement]),
    -- | Select a load from a base plus an index that is shifted left by a
    -- constant. The first function tells if the backend has the form for a
    -- type and a count. The second gives the instructions from the type,
    -- the destination, the base, the index, and the count.
    nbIndexedLoad :: Maybe (Type -> Int -> Bool, Type -> register -> register -> register -> Int -> [statement]),
    -- | Keep the carry of an addition, or the borrow of a subtraction, in the
    -- flags until the next instruction adds it to a value. The first
    -- function gives the operation without its second result, from the
    -- type, the destination, and the two operands. The second function
    -- gives the addition from the destination and the value.
    nbCarryIncrement :: Maybe (WideOp -> Type -> register -> register -> register -> [statement], WideOp -> register -> register -> [statement]),
    nbBinary :: Ctx register -> BinaryOp -> Type -> register -> register -> Operand -> NativeM error [statement],
    nbUnary :: UnaryOp -> Type -> register -> register -> [statement],
    nbWide :: WideOp -> Type -> register -> register -> register -> register -> [statement],
    nbQuotRemWide :: Maybe (Ctx register -> register -> register -> Operand -> Operand -> Operand -> NativeM error [statement]),
    nbCompare :: Ctx register -> CompareOp -> Type -> register -> Operand -> Operand -> [statement],
    nbFloatBinary :: FloatBinaryOp -> Type -> register -> register -> register -> [statement],
    nbFloatUnary :: FloatUnaryOp -> Type -> register -> register -> [statement],
    nbConvert :: Ctx register -> ConvertOp -> Type -> Type -> register -> register -> NativeM error [statement],
    nbSelect :: Ctx register -> Type -> register -> Operand -> Operand -> Operand -> [statement],
    nbLoad :: Ctx register -> Type -> Operand -> Integer -> register -> [statement],
    nbStore :: Ctx register -> Type -> Operand -> Operand -> Integer -> [statement],
    -- | An atomic load and an atomic store with an order, a base, and an
    -- offset. A dependent load is a plain 'nbLoad', which keeps the address
    -- dependency on both native targets.
    nbLoadAtomic :: Ctx register -> MemoryOrder -> Type -> Operand -> Integer -> register -> [statement],
    nbStoreAtomic :: Ctx register -> MemoryOrder -> Type -> Operand -> Operand -> Integer -> [statement],
    -- | A compare and swap: the destination, the expected value, the
    -- replacement, the base, and the offset.
    nbCompareSwap :: Ctx register -> MemoryOrder -> Type -> register -> Operand -> Operand -> Operand -> Integer -> NativeM error [statement],
    nbAtomicRmw :: Ctx register -> AtomicOp -> MemoryOrder -> Type -> register -> Operand -> Operand -> Integer -> NativeM error [statement],
    nbFence :: MemoryOrder -> [statement],
    nbPtrAdd :: Ctx register -> register -> Operand -> register -> [statement],
    nbStackAddr :: register -> Int -> [statement],
    nbGlobalLoad :: register -> Text -> [statement],
    nbGlobalStore :: register -> Text -> [statement],
    nbCall :: Ctx register -> Either Symbol Signature -> [Operand] -> [Var] -> NativeM error [statement],
    nbCallIndirect :: Ctx register -> Operand -> [Operand] -> Signature -> [Var] -> NativeM error [statement],
    nbTailCall :: Ctx register -> Either Text Operand -> CallingConvention -> [Type] -> [Operand] -> NativeM error [statement]
  }

-- | How a branch tests a condition.
data BranchTest statement error = BranchTest
  { btWhen :: Name -> NativeM error [statement],
    btUnless :: Name -> NativeM error [statement]
  }

-- | The frame of one function. Offsets are bytes above the stack pointer
-- after the prologue.
data Layout register = Layout
  { layoutRegisters :: !(Map Var register),
    layoutSlots :: !(Map Var Int),
    layoutSaved :: ![(register, Int)],
    -- | What each cold call saves, by the number of the call in the
    -- function: a register and the place that keeps it during the call.
    layoutColdSaves :: !(IntMap.IntMap [(register, Location register)]),
    layoutAllocs :: !(Map Var (Int, Int)),
    layoutSize :: Int,
    layoutFramed :: Bool,
    -- | The blocks that have a frame of their own, when the function has
    -- no frame for its full body. See 'blockFrameLabels'.
    layoutBlockFrames :: !(Maybe (Set Label))
  }

-- | The bytes between the stack pointer after the prologue and the stack of
-- the caller.
frameBytes :: NativeBackend statement register error -> Layout register -> Int
frameBytes backend layout
  | layoutFramed layout = layoutSize layout + nbFrameOverhead backend
  | otherwise = 0

data Ctx register = Ctx
  { ctxFunction :: !Function,
    ctxLayout :: !(Layout register),
    ctxLabels :: !(Map Label Name),
    ctxBlockParameters :: !(Map Label [(Var, Type)]),
    ctxSignatures :: !(Map Symbol Signature),
    ctxIncomingOverflow :: Int,
    ctxReads :: !(Map Var Int),
    ctxRotations :: !(Map Var Int),
    ctxFusions :: !(Map Var Fusion)
  }

-- | How instruction selection joins the instruction that defines a value
-- with another instruction. See 'selectFusions'.
data Fusion
  = -- | An addition whose right operand is shifted left by the count.
    FusedShift !Int
  | -- | A load of the type from the base plus the index shifted left by the
    -- count. The value is a pointer addition of the base and the index.
    FusedLoad !Type !Int
  | -- | A carry or a borrow that stays in the flags.
    FusedFlags
  | -- | An addition of a carry or a borrow that the flags hold.
    FusedIncrement !WideOp
  deriving (Eq, Show)

-- | Where a value lives: a register, or a frame slot at a byte offset above
-- the stack pointer after the prologue.
data Location register
  = LocRegister !register
  | LocSlot !Int
  deriving (Eq, Ord, Show)

-- | The source of a move: a location, or a literal of a type.
data MoveSource register
  = SourceLocation !(Location register)
  | SourceLiteral !Type !Literal
  deriving (Eq, Show)

-- | A comparison that the branch of the block consumes directly.
data Fused = Fused !CompareOp !Type !Operand !Operand

-- | Where the contents of a register last came from.
data Source
  = FromSlot !Int64
  | FromRegister !Int
  deriving (Eq)

-- | What one instruction does to the registers and frame slots the reload
-- pass tracks.
data SlotEffect
  = -- | Overwrites these general registers and nothing else the pass tracks.
    Writes ![Int]
  | -- | Reads a general register from a literal stack pointer offset.
    LoadsSlot !Int !Int64
  | -- | Writes a general register to a literal stack pointer offset.
    StoresSlot !Int !Int64
  | -- | Writes something else to a literal stack pointer offset.
    WritesSlot !Int64
  | -- | Copies one general register into another.
    MovesRegister !Int !Int
  | -- | Everything the pass knows becomes stale.
    Forgets

unsupported :: NativeBackend statement register error -> Text -> NativeM error value
unsupported backend = lift . Left . nbUnsupported backend

-- | The next number for a label private to the object.
nextLabelId :: NativeM error Int
nextLabelId = do
  state <- get
  let identifier = objectNextId state
  put state {objectNextId = identifier + 1}
  pure identifier

freshLabel :: Text -> NativeM error Name
freshLabel kind = do
  state <- get
  let index = objectNextLabel state
  put state {objectNextLabel = index + 1}
  identifier <- nextLabelId
  pure (LocalName identifier (".Llir_" <> kind <> "_" <> tshow index))

-- | The label of the stub that reports one trap message, or of the
-- trampoline of the function to it.
trapLabel :: Text -> NativeM error Name
trapLabel message = do
  state <- get
  let index = Map.findWithDefault (Map.size (objectTraps state)) message (objectTraps state)
  put state {objectTraps = Map.insert message index (objectTraps state)}
  if objectLocalTraps state
    then case Map.lookup message (objectFunctionTraps state) of
      Just (_, name) -> pure name
      Nothing -> do
        identifier <- nextLabelId
        let name = LocalName identifier (functionTrapLabel (objectFunctionIndex state) index)
        modify (\current -> current {objectFunctionTraps = Map.insert message (index, name) (objectFunctionTraps current)})
        pure name
    else pure (SymbolName (trapStubLabel index))

trapStubLabel :: Int -> Text
trapStubLabel index = ".Llir_trap_" <> tshow index

-- | The trampoline of one function to one trap stub.
functionTrapLabel :: Int -> Int -> Text
functionTrapLabel functionIndex index = ".Llir_trap_" <> tshow functionIndex <> "_" <> tshow index

-- | The trampolines of the function being compiled, one for each trap it
-- referenced, and clear them for the next function.
functionTrapTrampolines :: NativeBackend statement register error -> NativeM error [statement]
functionTrapTrampolines backend = do
  state <- get
  put state {objectFunctionTraps = Map.empty}
  pure
    ( case nbTrapTrampoline backend of
        Just render ->
          concat
            [ render name (trapStubLabel index)
            | (_, (index, name)) <- Map.toAscList (objectFunctionTraps state)
            ]
        Nothing -> []
    )

-- | Lint the module, then walk its items.
compileNativeStatements :: (Ord register) => NativeBackend statement register error -> Module -> Either error [statement]
compileNativeStatements = compileNativeStatementsWith True

-- | Walk the items of the module, after linting it when asked to. The
-- compiler lowers Lir it generated itself and lints it only under
-- @--lint@; a hand-written unit is always linted.
compileNativeStatementsWith :: (Ord register) => Bool -> NativeBackend statement register error -> Module -> Either error [statement]
compileNativeStatementsWith lint backend lirModule = concat <$> sequence (compileNativeChunksWith lint backend lirModule)

-- | The statements of the module in chunks: one per function, in order,
-- and one for the traps, the data and the globals. A chunk is produced
-- only when the consumer asks for it, so an assembler that folds each
-- chunk in before taking the next keeps one function's statements alive
-- rather than the whole module's. A failed chunk is the last one.
compileNativeChunksWith :: (Ord register) => Bool -> NativeBackend statement register error -> Module -> [Either error [statement]]
compileNativeChunksWith lint backend lirModule =
  case prepared of
    Right _ -> functionChunks initialState (zip [0 ..] [function | ItemFunction function <- items])
    Left errors -> [Left (nbLintErrors backend errors)]
  where
    functionChunks state remaining =
      case remaining of
        (index, function) : rest ->
          case runStateT (compileFunction backend signatures index function) state of
            Left err -> [Left err]
            Right (statements, next) -> Right statements : functionChunks next rest
        [] ->
          case runStateT (renderTraps backend) state of
            Left err -> [Left err]
            Right (trapStatements, _) -> [Right (trapStatements <> dataStatements <> globalStatements <> nbAfterObject backend)]
    dataStatements = concatMap (compileData backend) [dataItem | ItemData dataItem <- items]
    globalStatements = concatMap (compileGlobal backend) [global | ItemGlobal global <- items]
    prepared = if lint then prepareCheckedModule wordBytes lirModule else Right (prepareModule wordBytes lirModule)
    Module items = fromRight (Module []) prepared
    initialState = initialObjectState backend
    signatures =
      Map.fromList
        ( [(functionName function, functionSignature function) | ItemFunction function <- items]
            <> [(externFunctionName external, externFunctionSignature external) | ItemExternFunction external <- items]
        )

initialObjectState :: NativeBackend statement register error -> ObjectState
initialObjectState backend =
  ObjectState
    { objectTraps = Map.empty,
      objectNextLabel = 0,
      objectNextId = 0,
      objectLocalTraps = isJust (nbTrapTrampoline backend),
      objectFunctionIndex = 0,
      objectFunctionTraps = Map.empty
    }

-- | Consume each statement before selection proceeds to the next instruction.
compileNativeTo :: (Monad m, Ord register) => Bool -> NativeBackend statement register error -> (statement -> m ()) -> m () -> Module -> m (Either error ())
{-# INLINEABLE compileNativeTo #-}
compileNativeTo lint backend output endFunction lirModule =
  case prepared of
    Left errors -> pure (Left (nbLintErrors backend errors))
    Right _ -> go (initialObjectState backend) functions
  where
    prepared = if lint then prepareCheckedModule wordBytes lirModule else Right (prepareModule wordBytes lirModule)
    Module items = fromRight (Module []) prepared
    functions = [item | item@ItemFunction {} <- items]
    signatures = Map.fromList ([(functionName function, functionSignature function) | ItemFunction function <- items] <> [(externFunctionName external, externFunctionSignature external) | ItemExternFunction external <- items])
    go state remaining = case remaining of
      [] -> do
        result <- finishNativeTo backend output state
        case result of
          Left err -> pure (Left err)
          Right () -> do
            mapM_ (mapM_ output . compileData backend) [value | ItemData value <- items]
            mapM_ (mapM_ output . compileGlobal backend) [value | ItemGlobal value <- items]
            mapM_ output (nbAfterObject backend)
            pure (Right ())
      item : rest -> do
        result <- compileNativeItemTo backend output signatures item state
        either (pure . Left) (\next -> endFunction >> go next rest) result

-- | Compile one item with the declarations available at its boundary.
compileNativeItemTo :: (Monad m, Ord register) => NativeBackend statement register error -> (statement -> m ()) -> Map Symbol Signature -> Item -> ObjectState -> m (Either error ObjectState)
{-# INLINEABLE compileNativeItemTo #-}
compileNativeItemTo backend output signatures item state = case item of
  ItemFunction function -> do
    result <- compileFunctionTo backend output signatures (objectFunctionIndex state) function state
    pure (fmap (\next -> next {objectFunctionIndex = objectFunctionIndex state + 1}) result)
  ItemData value -> mapM_ output (compileData backend value) >> pure (Right state)
  ItemGlobal value -> mapM_ output (compileGlobal backend value) >> pure (Right state)
  _ -> pure (Right state)

finishNativeTo :: (Monad m) => NativeBackend statement register error -> (statement -> m ()) -> ObjectState -> m (Either error ())
finishNativeTo backend output state = case runStateT (renderTraps backend) state of
  Left err -> pure (Left err)
  Right (statements, _) -> mapM_ output statements >> pure (Right ())

renderTraps :: NativeBackend statement register error -> NativeM error [statement]
renderTraps backend = do
  traps <- Map.toAscList . objectTraps <$> get
  pure (if null traps then [] else nbRenderTraps backend traps)

-- Data

compileData :: NativeBackend statement register error -> DataItem -> [statement]
compileData backend dataItem =
  [ nbSection backend (if dataMutable dataItem then DataSection else ReadOnlySection),
    nbAlign backend (log2 (dataAlignment dataItem))
  ]
    <> [nbGlobal backend symbol | dataLinkage dataItem == Export]
    <> [nbLabel backend (SymbolName symbol)]
    <> concatMap field (dataFields dataItem)
  where
    symbol = nbSymbol backend (dataName dataItem)
    field dataField =
      case dataField of
        DataIntConstant _ constant -> unresolvedConstant constant
        DataInt ty value -> [nbWord backend (typeBytes ty) (fromInteger value)]
        DataFloat F32 value -> [nbWord backend 4 (fromIntegral (castFloatToWord32 (double2Float value)))]
        DataFloat _ value -> [nbWord backend 8 (castDoubleToWord64 value)]
        DataSymbol target 0 -> [nbQuadSymbol backend (nbSymbol backend target)]
        DataSymbol target addend -> [nbQuadSymbolAddend backend (nbSymbol backend target) (fromInteger addend)]
        DataNull -> [nbQuad backend 0]
        DataWordConstant constant -> unresolvedConstant constant
        DataWord value -> [nbWord backend 8 (fromInteger value)]
        DataCode Nothing -> [nbQuad backend 0]
        DataCode (Just target) -> [nbQuadSymbol backend (nbSymbol backend target)]
        DataBytes bytes -> [nbBytes backend bytes]
        DataZero count -> zeroBytes count
    zeroBytes count
      | count <= 0 = []
      | otherwise = nbBytes backend (BS.replicate (fromInteger (min 65536 count)) 0) : zeroBytes (count - 65536)

-- | A global is one word in the data section of its module.
compileGlobal :: NativeBackend statement register error -> Global -> [statement]
compileGlobal backend global =
  [ nbSection backend DataSection,
    nbAlign backend 3,
    nbLabel backend (SymbolName (nbSymbol backend (globalName global))),
    nbQuad backend 0
  ]

log2 :: Integer -> Int
log2 value = length (takeWhile (< value) (iterate (* 2) 1))

-- | The native targets are 64-bit, so a word-scaled address offset counts
-- eight bytes.
wordBytes :: Integer
wordBytes = 8

typeBytes :: Type -> Int
typeBytes ty = max 1 (typeBits ty `div` 8)

-- | The bytes of the stack block that carries the aihc arguments after the
-- argument registers.
overflowBytes :: NativeBackend statement register error -> Int -> Int
overflowBytes backend count =
  ((max 0 (count - length (nbAihcArgumentRegisters backend)) * 8 + 15) `div` 16) * 16

-- Functions

compileFunction :: (Ord register) => NativeBackend statement register error -> Map Symbol Signature -> Int -> Function -> NativeM error [statement]
compileFunction backend signatures index function = StateT $ \state ->
  let (result, statements) = runState (compileFunctionTo backend (\statement -> modify' (statement :)) signatures index function state) []
   in fmap (reverse statements,) result

-- | Keep allocation state for one function and emit its instructions directly.
compileFunctionTo :: (Monad m, Ord register) => NativeBackend statement register error -> (statement -> m ()) -> Map Symbol Signature -> Int -> Function -> ObjectState -> m (Either error ObjectState)
{-# INLINEABLE compileFunctionTo #-}
compileFunctionTo backend output signatures index input state =
  evalStateT (runExceptT (execStateT action state)) IntMap.empty
  where
    lowered = lowerWideDivision (isJust (nbQuotRemWide backend)) input
    (rotated, rotations) = if isJust (nbRotatedBinary backend) then selectRotatedOperands lowered else (lowered, Map.empty)
    (function, fusions) = selectFusions backend rotated
    native :: (Monad n) => NativeM err value -> StateT ObjectState (ExceptT err n) value
    native = mapStateT (ExceptT . pure)
    emitStatement statement = do
      held <- get
      let (keep, next) = slotStep held (nbAsCode backend statement)
      put next
      when keep (lift (output statement))
    emitStatements = mapM_ (lift . lift . emitStatement)
    action = do
      (prepared, prefix) <- native (prepareFunction backend signatures index function)
      let ctx = prepared {ctxRotations = rotations, ctxFusions = fusions}
      emitStatements prefix
      let blocks = functionBlocks function
          -- The number of the first instruction of each block. The
          -- allocation numbers the instructions in the source order.
          firsts = scanl (+) 0 (map (length . blockInstructions) blocks)
          placed = layoutBlocks (zip blocks firsts)
      forM_ (zip3 (True : repeat False) placed (map (Just . fst) (drop 1 placed) <> [Nothing])) $ \(entry, (block, first), next) -> do
        let (instructions, fused) = fuseCompare backend ctx (blockInstructions block) (blockTerminator block)
            framed = hasBlockFrame ctx (blockLabel block)
        emitStatements [nbLabel backend (ctxLabels ctx Map.! blockLabel block) | not entry]
        when framed $ emitStatements (enterBlockFrame backend ctx entry block)
        forM_ (zip [first ..] instructions) $ \(number, instruction) ->
          native (coldCallSaves backend ctx number <$> compileInstruction backend ctx instruction) >>= emitStatements
        when framed $ emitStatements (nbBlockFrameLeave backend (layoutSize (ctxLayout ctx)))
        native (compileTerminator backend ctx (blockLabel <$> next) fused (blockTerminator block)) >>= emitStatements
      native (functionTrapTrampolines backend) >>= emitStatements

-- | Put the blocks in the order of emission: the entry block, then the
-- other blocks that are not cold, then the cold blocks. Each group keeps the
-- source order. Thus the hot path is dense in the instruction cache, and a
-- branch to a cold block jumps forward, out of the hot path.
--
-- Only the emission order changes. The allocation sees the source order,
-- because an interval that reaches a cold block at the end of the function
-- would keep its register through all of the hot code.
layoutBlocks :: [(Block, a)] -> [(Block, a)]
layoutBlocks blocks = case blocks of
  entry : rest -> entry : filter (not . blockCold . fst) rest <> filter (blockCold . fst) rest
  [] -> []

prepareFunction :: (Ord register) => NativeBackend statement register error -> Map Symbol Signature -> Int -> Function -> NativeM error (Ctx register, [statement])
prepareFunction backend signatures index function = do
  modify (\state -> state {objectFunctionIndex = index, objectFunctionTraps = Map.empty})
  layout <- functionLayout backend signatures function
  let blocks = functionBlocks function
  labels <-
    Map.fromList
      <$> forM
        (zip [0 :: Int ..] blocks)
        ( \(position, block) -> do
            identifier <- nextLabelId
            pure (blockLabel block, LocalName identifier (".Llir_" <> tshow index <> "_" <> tshow position))
        )
  let ctx =
        Ctx
          { ctxFunction = function,
            ctxLayout = layout,
            ctxLabels = labels,
            ctxBlockParameters = Map.fromList [(blockLabel block, blockParameters block) | block <- blocks],
            ctxSignatures = signatures,
            ctxIncomingOverflow = case functionConvention function of
              AihcConvention -> overflowBytes backend (length (functionParameters function))
              CConvention -> 0,
            ctxReads = readCounts function,
            ctxRotations = Map.empty,
            ctxFusions = Map.empty
          }
  prologue <- functionPrologue backend ctx
  pure
    ( ctx,
      [nbSection backend TextSection, nbAlign backend (nbCodeAlign backend)]
        <> [nbGlobal backend symbol | functionLinkage function == Export]
        <> [nbLabel backend (SymbolName symbol)]
        <> prologue
    )
  where
    symbol = nbSymbol backend (functionName function)

-- | Drop a read that the destination register already holds, and a store
-- of what the slot already holds.
elideSlotReloadsWith :: (statement -> Maybe SlotEffect) -> [statement] -> [statement]
elideSlotReloadsWith asCode = go IntMap.empty
  where
    go _ [] = []
    go held (statement : rest) =
      let (keep, next) = slotStep held (asCode statement)
       in if keep then statement : go next rest else go next rest

slotStep :: IntMap.IntMap Source -> Maybe SlotEffect -> (Bool, IntMap.IntMap Source)
slotStep held effect = case effect of
  Nothing -> (True, IntMap.empty)
  Just Forgets -> (True, IntMap.empty)
  Just (LoadsSlot register offset) -> reads' register (FromSlot offset)
  Just (MovesRegister destination source)
    | IntMap.lookup source held == Just (FromRegister destination) -> (False, held)
    | otherwise -> reads' destination (FromRegister source)
  Just (StoresSlot register offset)
    | IntMap.lookup register held == Just (FromSlot offset) -> (False, held)
    | otherwise -> (True, IntMap.insert register (FromSlot offset) (IntMap.filter (/= FromSlot offset) held))
  Just (WritesSlot offset) -> (True, IntMap.filter (/= FromSlot offset) held)
  Just (Writes registers) -> (True, foldr invalidate held registers)
  where
    reads' register source
      | IntMap.lookup register held == Just source = (False, held)
      | otherwise = (True, IntMap.insert register source (invalidate register held))
    invalidate register = IntMap.filter (/= FromRegister register) . IntMap.delete register

functionLayout :: (Ord register) => NativeBackend statement register error -> Map Symbol Signature -> Function -> NativeM error (Layout register)
functionLayout backend signatures function = do
  let blocks = functionBlocks function
      convention = functionConvention function
      allocation = allocateRegistersFor (nbRegistersFor backend convention function) signatures function
      calls = [operation | block <- blocks, Instruction _ operation <- blockInstructions block, isCall operation]
      callsAihc = any ((== AihcConvention) . callConvention signatures) calls
      savedRegisters =
        case convention of
          AihcConvention -> []
          CConvention
            | callsAihc -> nbPreservedRegisters backend
            | otherwise -> [register | register <- allocationUsed allocation, register `elem` nbPreservedRegisters backend]
      slots = Map.fromList (zip (allocationSpills allocation) [0, 8 ..])
      -- The cold calls share one area of the frame. Each call puts what it
      -- saves there in order, so that the slots are next to each other. A
      -- block frame starts the area where its stack allocations would start.
      coldArea = maximum (0 : [length [() | (_, Nothing) <- saves] | saves <- IntMap.elems (allocationColdSaves allocation)])
      coldBase
        | isJust blockFrames = nbBlockFrameBase backend
        | otherwise = 8 * Map.size slots
      coldSaves = IntMap.map (placeColdSaves coldBase) (allocationColdSaves allocation)
      slotsEnd = 8 * (Map.size slots + coldArea)
      saved = zip savedRegisters [slotsEnd, slotsEnd + 8 ..]
      allocations = [(var, size, alignmentInBytes wordBytes alignment) | block <- take 1 blocks, Instruction [var] (StackAlloc size alignment) <- blockInstructions block]
      tailCallFrame = nbTailCallFrame backend signatures function
      blockFrames
        | convention == AihcConvention,
          Map.null slots,
          not (null calls),
          not tailCallFrame =
            blockFrameLabels (Set.fromList [var | (var, _, _) <- allocations]) blocks
        | otherwise = Nothing
      allocsStart
        | isJust blockFrames = coldBase + 8 * coldArea
        | otherwise = slotsEnd + 8 * length saved
  allocs <- placeAllocations backend allocsStart allocations
  let end = case Map.elems allocs of
        [] -> allocsStart
        placed -> maximum [offset + allocated | (offset, allocated) <- placed]
      size = ((end + 15) `div` 16) * 16
  case nbMaxFrameBytes backend of
    Just limit | size > limit -> unsupported backend ("function " <> unSymbol (functionName function) <> " needs a frame larger than 32000 bytes")
    _ -> pure ()
  pure
    Layout
      { layoutRegisters = allocationRegisters allocation,
        layoutSlots = slots,
        layoutSaved = saved,
        layoutColdSaves = coldSaves,
        layoutAllocs = allocs,
        layoutSize = size,
        layoutFramed = isNothing blockFrames && (size > 0 || not (null calls) || tailCallFrame),
        layoutBlockFrames = blockFrames
      }

-- | The blocks that make a call, when each of them can have a frame of its
-- own instead of a frame for the full function.
--
-- An aihc function preserves no register and ends in a tail call, so it
-- needs a frame only for its spill slots, for the return address across a
-- call, and for the stack allocations. When it has no spill slots, only a
-- block that makes a call needs the frame. That block makes the frame at
-- its start and removes it before its terminator, so the path that does not
-- call has no frame setup at all. Each block frame has the same layout, so
-- a stack allocation is at the same address in each of them.
--
-- An instruction that uses a stack allocation must be in a block that
-- calls, because only there is the memory above the stack pointer. A
-- terminator must not use one. Otherwise, the function keeps a frame for
-- its full body. The memory keeps its contents from one block frame to the
-- next, because the code between two block frames writes nothing below the
-- stack pointer. A signal handler on the same stack can write there, but
-- each producer of stack.alloc writes and reads the memory in one block.
blockFrameLabels :: Set Var -> [Block] -> Maybe (Set Label)
blockFrameLabels allocations blocks
  | all allowed blocks = Just callers
  | otherwise = Nothing
  where
    callers = Set.fromList [blockLabel block | block <- blocks, any (isCall . instructionOperation) (blockInstructions block)]
    allowed block =
      not (any allocation (terminatorReads (blockTerminator block)))
        && ( blockLabel block `Set.member` callers
               || not (any (any allocation . operationReads . instructionOperation) (blockInstructions block))
           )
    allocation = (`Set.member` allocations)

-- | Whether the block has a frame of its own.
hasBlockFrame :: Ctx register -> Label -> Bool
hasBlockFrame ctx label = maybe False (Set.member label) (layoutBlockFrames (ctxLayout ctx))

-- | Make the frame of one block, and compute the address of each stack
-- allocation the block uses. The entry block computes an address where
-- its stack.alloc is, because a parameter can use the register before it.
enterBlockFrame :: NativeBackend statement register error -> Ctx register -> Bool -> Block -> [statement]
enterBlockFrame backend ctx entry block =
  nbBlockFrameEnter backend (layoutSize layout)
    <> concat
      [ nbStackAddr backend register offset
      | not entry,
        (var, (offset, _)) <- Map.toAscList (layoutAllocs layout),
        var `Set.member` used,
        Just register <- [Map.lookup var (layoutRegisters layout)]
      ]
  where
    layout = ctxLayout ctx
    used = Set.fromList (concatMap (operationReads . instructionOperation) (blockInstructions block))

isCall :: Operation -> Bool
isCall operation =
  case operation of
    Call _ _ -> True
    CallIndirect {} -> True
    _ -> False

callConvention :: Map Symbol Signature -> Operation -> CallingConvention
callConvention signatures operation =
  case operation of
    Call symbol _ -> maybe AihcConvention signatureConvention (Map.lookup symbol signatures)
    CallIndirect _ _ signature -> signatureConvention signature
    _ -> AihcConvention

placeAllocations :: NativeBackend statement register error -> Int -> [(Var, Integer, Integer)] -> NativeM error (Map Var (Int, Int))
placeAllocations backend = go Map.empty
  where
    go placed _ [] = pure placed
    go placed next ((var, size, alignment) : rest) = do
      when (alignment > 16) $ unsupported backend "stack.alloc alignment above 16"
      let start = roundUp (fromInteger alignment) next
      go (Map.insert var (start, fromInteger size) placed) (start + fromInteger size) rest
    roundUp alignment value = ((value + alignment - 1) `div` alignment) * alignment

functionPrologue :: (Eq register) => NativeBackend statement register error -> Ctx register -> NativeM error [statement]
functionPrologue backend ctx = do
  let parameters = functionParameters function
      moves =
        case functionConvention function of
          AihcConvention ->
            parallelMove
              backend
              [ (home ctx var, SourceLocation (parameterLocation index))
              | (index, (var, _)) <- zip [0 ..] parameters
              ]
          CConvention -> nbCParameterMoves backend ctx
  pure
    ( nbPrologueFrame backend (layoutFramed layout) (layoutSize layout)
        <> saveRegisters backend ctx
        <> moves
    )
  where
    function = ctxFunction ctx
    layout = ctxLayout ctx
    parameterLocation index
      | index < length (nbAihcArgumentRegisters backend) = LocRegister (nbAihcArgumentRegisters backend !! index)
      | otherwise = LocSlot (frameBytes backend layout + nbReturnAddressGap backend + 8 * (index - length (nbAihcArgumentRegisters backend)))

saveRegisters :: NativeBackend statement register error -> Ctx register -> [statement]
saveRegisters backend ctx =
  [nbSaveReg backend register offset | (register, offset) <- layoutSaved (ctxLayout ctx)]

home :: Ctx register -> Var -> Location register
home ctx var =
  case Map.lookup var (layoutRegisters (ctxLayout ctx)) of
    Just register -> LocRegister register
    Nothing ->
      case Map.lookup var (layoutSlots (ctxLayout ctx)) of
        Just offset -> LocSlot offset
        Nothing -> error ("Aihc.Native.Lir: unknown value " <> T.unpack (unVar var))

operandSource :: Ctx register -> Type -> Operand -> MoveSource register
operandSource ctx ty operand =
  case operand of
    OperandVar var -> SourceLocation (home ctx var)
    OperandLiteral literal -> SourceLiteral ty literal

operandIn :: NativeBackend statement register error -> Ctx register -> Int -> Type -> register -> Operand -> ([statement], register)
operandIn backend ctx displacement ty scratch operand =
  case operand of
    OperandVar var ->
      case home ctx var of
        LocRegister register -> ([], register)
        LocSlot offset -> ([nbLoadSlot backend scratch (offset + displacement)], scratch)
    OperandLiteral literal -> (nbLiteralInto backend ty scratch literal, scratch)

operandTo :: (Eq register) => NativeBackend statement register error -> Ctx register -> Type -> register -> Operand -> [statement]
operandTo backend ctx ty destination operand =
  case operand of
    OperandVar var ->
      case home ctx var of
        LocRegister register -> nbMove backend destination register
        LocSlot offset -> [nbLoadSlot backend destination offset]
    OperandLiteral literal -> nbLiteralInto backend ty destination literal

resultIn :: NativeBackend statement register error -> Ctx register -> register -> Var -> (register, [statement])
resultIn backend ctx scratch var =
  case home ctx var of
    LocRegister register -> (register, [])
    LocSlot offset -> (scratch, [nbStoreSlot backend scratch offset])

literalBits :: Type -> Literal -> Maybe Integer
literalBits ty literal =
  case (ty, literal) of
    (F32, LitFloat value) -> Just (toInteger (castFloatToWord32 (double2Float value)))
    (F32, LitInt value) -> Just (toInteger (castFloatToWord32 (fromInteger value)))
    (F64, LitInt value) -> Just (toInteger (castDoubleToWord64 (fromInteger value)))
    (_, LitFloat value) -> Just (toInteger (castDoubleToWord64 value))
    (_, LitInt value) -> Just (canonicalInteger ty value)
    (_, LitNull) -> Just 0
    (_, LitSymbol _) -> Nothing

canonicalInteger :: Type -> Integer -> Integer
canonicalInteger ty value
  | typeBits ty >= 64 = value `mod` (2 ^ (64 :: Int))
  | otherwise = value `mod` (2 ^ typeBits ty)

tshow :: (Show value) => value -> Text
tshow = T.pack . show

-- Parallel moves

parallelMove :: (Eq register) => NativeBackend statement register error -> [(Location register, MoveSource register)] -> [statement]
parallelMove backend = concatMap emit . orderMoves locationOf SourceLocation (LocRegister (nbCycleScratch backend))
  where
    locationOf source =
      case source of
        SourceLocation location -> Just location
        SourceLiteral _ _ -> Nothing
    emit (destination, source) =
      case (destination, source) of
        (LocRegister target, SourceLocation (LocRegister register)) -> nbMove backend target register
        (LocRegister target, SourceLocation (LocSlot offset)) -> [nbLoadSlot backend target offset]
        (LocRegister target, SourceLiteral ty literal) -> nbLiteralInto backend ty target literal
        (LocSlot offset, SourceLocation (LocRegister register)) -> [nbStoreSlot backend register offset]
        (LocSlot offset, SourceLocation (LocSlot from)) ->
          [nbLoadSlot backend (nbSlotMoveScratch backend) from, nbStoreSlot backend (nbSlotMoveScratch backend) offset]
        (LocSlot offset, SourceLiteral ty literal) ->
          case literalBits ty literal >>= nbStoreSlotImmediate backend offset of
            Just statement -> [statement]
            Nothing -> nbLiteralInto backend ty (nbSlotMoveScratch backend) literal <> [nbStoreSlot backend (nbSlotMoveScratch backend) offset]

displaceSource :: Int -> MoveSource register -> MoveSource register
displaceSource displacement source =
  case source of
    SourceLocation (LocSlot offset) -> SourceLocation (LocSlot (offset + displacement))
    _ -> source

-- Blocks

fuseCompare :: NativeBackend statement register error -> Ctx register -> [Instruction] -> Terminator -> ([Instruction], Maybe Fused)
fuseCompare backend ctx instructions terminator =
  case (reverse instructions, terminator) of
    (Instruction [var] (Compare op ty left right) : before, Branch (OperandVar condition) _ _)
      | condition == var,
        Map.lookup var (ctxReads ctx) == Just 1,
        not (isFloatType ty) || nbCanFuseFloatCompare backend op ->
          (reverse before, Just (Fused op ty left right))
    _ -> (instructions, Nothing)

compileTerminator :: (Eq register) => NativeBackend statement register error -> Ctx register -> Maybe Label -> Maybe Fused -> Terminator -> NativeM error [statement]
compileTerminator backend ctx next fused terminator =
  case terminator of
    Jump target -> do
      moves <- blockArgumentMoves backend ctx target
      pure (moves <> branchTo target)
    Branch condition whenTrue whenFalse -> do
      (setup, test) <- nbConditionTest backend ctx fused condition
      trueMoves <- blockArgumentMoves backend ctx whenTrue
      falseMoves <- blockArgumentMoves backend ctx whenFalse
      if null trueMoves && isNext whenFalse
        then do
          -- The false target follows, so the branch goes to the true target
          -- when the condition holds. The moves of the false edge come after
          -- the branch, so the path to the next block takes no jump.
          branch <- btWhen test (labelOf whenTrue)
          pure (setup <> branch <> falseMoves)
        else do
          falseLabel <- if null falseMoves then pure (labelOf whenFalse) else freshLabel "else"
          unlessBranch <- btUnless test falseLabel
          pure
            ( setup
                <> unlessBranch
                <> trueMoves
                <> [nbJump backend (labelOf whenTrue) | not (null falseMoves) || not (isNext whenTrue)]
                <> (if null falseMoves then [] else nbLabel backend falseLabel : falseMoves <> branchTo whenFalse)
            )
    Switch ty scrutinee cases fallback -> do
      let (loads, register) = operandIn backend ctx 0 ty (nbScratchLeft backend) scrutinee
      edges <- forM cases $ \switchCase -> do
        moves <- blockArgumentMoves backend ctx (switchCaseTarget switchCase)
        label <-
          if null moves
            then pure (labelOf (switchCaseTarget switchCase))
            else freshLabel "case"
        pure (switchCase, label, moves)
      fallbackLines <-
        case fallback of
          Just target -> do
            moves <- blockArgumentMoves backend ctx target
            pure (moves <> branchTo target)
          Nothing -> do
            stub <- trapLabel "switch without a matching case"
            pure [nbJump backend stub]
      let checks =
            concat
              [ nbCompareAndBranchEqual backend ctx ty register (resolvedSwitchCaseValue switchCase) label
              | (switchCase, label, _) <- edges
              ]
          bodies =
            concat
              [ nbLabel backend label : moves <> [nbJump backend (labelOf (switchCaseTarget switchCase))]
              | (switchCase, label, moves) <- edges,
                not (null moves)
              ]
      pure (loads <> checks <> fallbackLines <> bodies)
    Return values -> do
      when (length values > length (nbResultRegisters backend)) $ unsupported backend "return of more than eight values"
      let moves =
            parallelMove
              backend
              [ (LocRegister register, operandSource ctx ty value)
              | (ty, register, value) <- zip3 (functionResults function) (nbResultRegisters backend) values
              ]
      pure (moves <> nbCReturnFloat backend (functionConvention function) (functionResults function) <> nbLeaveFrame backend ctx 0 <> nbReturn backend ctx)
    TailCall symbol arguments ->
      let signature = Map.lookup symbol (ctxSignatures ctx)
       in nbTailCall backend ctx (Left (nbSymbol backend symbol)) (maybe AihcConvention signatureConvention signature) (maybe [] signatureParameters signature) arguments
    TailCallIndirect target arguments signature ->
      nbTailCall backend ctx (Right target) (signatureConvention signature) (signatureParameters signature) arguments
    Trap message -> do
      stub <- trapLabel message
      pure [nbJump backend stub]
  where
    function = ctxFunction ctx
    labelOf target = ctxLabels ctx Map.! targetLabel target
    isNext target = Just (targetLabel target) == next
    branchTo target = [nbJump backend (labelOf target) | not (isNext target)]

blockArgumentMoves :: (Eq register) => NativeBackend statement register error -> Ctx register -> Target -> NativeM error [statement]
blockArgumentMoves backend ctx (Target label arguments) = do
  let parameters = Map.findWithDefault [] label (ctxBlockParameters ctx)
  pure
    ( parallelMove
        backend
        [ (home ctx var, operandSource ctx ty argument)
        | ((var, ty), argument) <- zip parameters arguments
        ]
    )

calleeSignature :: Ctx register -> Either Symbol Signature -> (CallingConvention, [Type], [Type])
calleeSignature ctx callee =
  case callee of
    Left symbol ->
      case Map.lookup symbol (ctxSignatures ctx) of
        Just signature -> (signatureConvention signature, signatureResults signature, signatureParameters signature)
        Nothing -> (AihcConvention, [], [])
    Right signature -> (signatureConvention signature, signatureResults signature, signatureParameters signature)

-- | Give the saves of one cold call that go to the frame the slots of the
-- area from an offset on, in order.
placeColdSaves :: Int -> [(register, Maybe register)] -> [(register, Location register)]
placeColdSaves = go
  where
    go _ [] = []
    go next ((register, Just keeper) : rest) = (register, LocRegister keeper) : go next rest
    go next ((register, Nothing) : rest) = (register, LocSlot next) : go (next + 8) rest

-- | Put the saves of a cold call around its statements. The saves come
-- first, before the argument moves overwrite anything. The values come back
-- last, after the results have left the result registers, which a value
-- may have occupied. A value goes to its own register or slot, so the
-- moves are independent of one another.
coldCallSaves :: (Eq register) => NativeBackend statement register error -> Ctx register -> Int -> [statement] -> [statement]
coldCallSaves backend ctx number statements =
  case IntMap.lookup number (layoutColdSaves (ctxLayout ctx)) of
    Nothing -> statements
    Just saves ->
      concat [nbMove backend keeper register | (register, LocRegister keeper) <- saves]
        <> nbStoreSlots backend [(register, offset) | (register, LocSlot offset) <- saves]
        <> statements
        <> concat [nbMove backend register keeper | (register, LocRegister keeper) <- saves]
        <> nbLoadSlots backend [(register, offset) | (register, LocSlot offset) <- saves]

compileInstruction :: (Eq register) => NativeBackend statement register error -> Ctx register -> Instruction -> NativeM error [statement]
compileInstruction backend ctx (Instruction results operation) =
  case operation of
    Binary op ty left right -> do
      let (loads, a) = operandIn backend ctx 0 ty (nbScratchLeft backend) left
      single $ \dst -> do
        body <- case (results, nbRotatedBinary backend) of
          ([result], Just select)
            | Just count <- Map.lookup result (ctxRotations ctx) ->
                let (rightLoads, b) = operandIn backend ctx 0 ty (nbScratchRight backend) right
                 in pure (rightLoads <> select op ty dst a b count)
          ([result], _)
            | Just (FusedShift count) <- Map.lookup result (ctxFusions ctx),
              Just shiftedAdd <- nbShiftedAdd backend ->
                let (rightLoads, b) = operandIn backend ctx 0 ty (nbScratchRight backend) right
                 in pure (rightLoads <> shiftedAdd dst a b count)
            | Just (FusedIncrement wideOp) <- Map.lookup result (ctxFusions ctx),
              Just (_, increment) <- nbCarryIncrement backend ->
                pure (increment wideOp dst a)
          _ -> nbBinary backend ctx op ty dst a right
        pure (loads <> body)
    Unary op ty value -> do
      let (loads, a) = operandIn backend ctx 0 ty (nbScratchLeft backend) value
      single $ \dst -> pure (loads <> nbUnary backend op ty dst a)
    Wide op ty left right -> do
      let (loads, a) = operandIn backend ctx 0 ty (nbScratchLeft backend) left
          (loads', b) = operandIn backend ctx 0 ty (nbScratchRight backend) right
      case (results, nbCarryIncrement backend) of
        ([_, carry], Just (flags, _))
          | Map.lookup carry (ctxFusions ctx) == Just FusedFlags ->
              pair $ \low _ -> pure (loads <> loads' <> flags op ty low a b)
        _ -> pair $ \low high -> pure (loads <> loads' <> nbWide backend op ty low high a b)
    QuotRemWide _ high low divisor ->
      case nbQuotRemWide backend of
        Just divide -> pair $ \q r -> divide ctx q r high low divisor
        Nothing -> unsupported backend "wide division reached instruction selection"
    Compare op ty left right ->
      single $ \dst -> pure (nbCompare backend ctx op ty dst left right)
    FloatBinary op ty left right -> do
      let (loads, a) = operandIn backend ctx 0 ty (nbScratchLeft backend) left
          (loads', b) = operandIn backend ctx 0 ty (nbScratchRight backend) right
      single $ \dst -> pure (loads <> loads' <> nbFloatBinary backend op ty dst a b)
    FloatUnary op ty value -> do
      let (loads, a) = operandIn backend ctx 0 ty (nbScratchLeft backend) value
      single $ \dst -> pure (loads <> nbFloatUnary backend op ty dst a)
    Convert op from value to -> do
      let (loads, a) = operandIn backend ctx 0 from (nbScratchLeft backend) value
      single $ \dst -> do
        body <- nbConvert backend ctx op from to dst a
        pure (loads <> body)
    PtrToInt value -> single $ \dst -> pure (operandTo backend ctx Ptr dst value)
    PtrFromInt value -> single $ \dst -> pure (operandTo backend ctx Ptr dst value)
    Select ty condition left right -> single $ \dst -> pure (nbSelect backend ctx ty dst condition left right)
    Load ty address _ ->
      single $ \dst -> pure (nbLoad backend ctx ty (addressBase address) (addressByteOffset wordBytes address) dst)
    Store ty value address _ ->
      pure (nbStore backend ctx ty value (addressBase address) (addressByteOffset wordBytes address))
    LoadAtomic order ty address ->
      single $ \dst -> pure (nbLoadAtomic backend ctx order ty (addressBase address) (addressByteOffset wordBytes address) dst)
    LoadDependent ty address ->
      single $ \dst -> pure (nbLoad backend ctx ty (addressBase address) (addressByteOffset wordBytes address) dst)
    LoadInvariant ty address _ ->
      single $ \dst -> pure (nbLoad backend ctx ty (addressBase address) (addressByteOffset wordBytes address) dst)
    StoreAtomic order ty value address ->
      pure (nbStoreAtomic backend ctx order ty value (addressBase address) (addressByteOffset wordBytes address))
    CompareSwap order ty expected replacement address ->
      single $ \dst -> nbCompareSwap backend ctx order ty dst expected replacement (addressBase address) (addressByteOffset wordBytes address)
    AtomicRmw op order ty value address ->
      single $ \dst -> nbAtomicRmw backend ctx op order ty dst value (addressBase address) (addressByteOffset wordBytes address)
    Fence order -> pure (nbFence backend order)
    PtrAdd base offset -> do
      let (loads, a) = operandIn backend ctx 0 Ptr (nbScratchLeft backend) base
      case (results, nbShiftedAdd backend, nbIndexedLoad backend) of
        ([result], Just shiftedAdd, _)
          | Just (FusedShift count) <- Map.lookup result (ctxFusions ctx) ->
              let (loads', b) = operandIn backend ctx 0 I64 (nbScratchRight backend) offset
               in single $ \dst -> pure (loads <> loads' <> shiftedAdd dst a b count)
        ([result], _, Just (_, indexed))
          | Just (FusedLoad ty count) <- Map.lookup result (ctxFusions ctx) ->
              let (loads', b) = operandIn backend ctx 0 I64 (nbScratchRight backend) offset
               in single $ \dst -> pure (loads <> loads' <> indexed ty dst a b count)
        _ -> single $ \dst -> pure (loads <> nbPtrAdd backend ctx a offset dst)
    StackAlloc _ _ ->
      case results of
        [var]
          | Just (offset, _) <- Map.lookup var (layoutAllocs (ctxLayout ctx)) ->
              -- Without a frame in the entry block, the address is not
              -- known yet. Each block that uses it computes it.
              if entryHasFrame
                then single $ \dst -> pure (nbStackAddr backend dst offset)
                else pure []
        _ -> unsupported backend "stack.alloc without a placed result"
    GlobalGet symbol ->
      single $ \dst -> pure (nbGlobalLoad backend dst (nbSymbol backend symbol))
    GlobalSet symbol value -> do
      let (loads, a) = operandIn backend ctx 0 I64 (nbScratchLeft backend) value
      pure (loads <> nbGlobalStore backend a (nbSymbol backend symbol))
    Call symbol arguments -> nbCall backend ctx (Left symbol) arguments results
    CallIndirect target arguments signature -> nbCallIndirect backend ctx target arguments signature results
  where
    entryHasFrame = case (layoutBlockFrames (ctxLayout ctx), functionBlocks (ctxFunction ctx)) of
      (Just labels, entry : _) -> blockLabel entry `Set.member` labels
      _ -> True
    single body =
      case results of
        [var] -> do
          let (dst, store) = resultIn backend ctx (nbScratchLeft backend) var
          lines' <- body dst
          pure (lines' <> store)
        _ -> unsupported backend "instruction result count"
    pair body =
      case results of
        [first, second] -> do
          let (low, storeLow) = resultIn backend ctx (nbScratchLeft backend) first
              (high, storeHigh) = resultIn backend ctx (nbScratchRight backend) second
          lines' <- body low high
          pure (lines' <> storeLow <> storeHigh)
        _ -> unsupported backend "instruction result count"

-- | Join instructions of one block that one machine instruction can do.
--
-- A left shift by a constant that only an addition reads becomes an operand
-- of the addition. A pointer addition that only a load reads becomes the
-- address of the load: the load is then a pointer addition at the position
-- of the load, and the side table gives the type. A carry or a borrow that
-- only an addition reads, through a zero extension, stays in the flags: the
-- addition moves to the position after the operation that makes the carry,
-- and reads the carry directly. The value it adds to must be ready there.
-- Each rewrite keeps the reads that the allocation must see.
selectFusions :: NativeBackend statement register error -> Function -> (Function, Map Var Fusion)
selectFusions backend function = (function {functionBlocks = map fst selected}, Map.unions (map snd selected))
  where
    uses = readCounts function
    useOnce v = Map.lookup v uses == Just 1
    selected = map selectBlock (functionBlocks function)
    selectBlock block =
      let (shifted, shifts) = if isJust (nbShiftedAdd backend) then selectShifts (blockInstructions block) else (blockInstructions block, Map.empty)
          (loaded, table) = maybe (shifted, shifts) (\(accepts, _) -> selectLoads accepts shifts shifted) (nbIndexedLoad backend)
          (carried, carries) = if isJust (nbCarryIncrement backend) then selectCarries loaded else (loaded, Map.empty)
       in (block {blockInstructions = carried}, Map.union table carries)
    selectShifts instructions = (concatMap rewrite instructions, Map.map (\(_, _, count) -> FusedShift count) matches)
      where
        definitions = Map.fromList [(v, operation) | Instruction [v] operation <- instructions]
        shift (OperandVar s)
          | useOnce s,
            Just (Binary Shl I64 index@(OperandVar _) (OperandLiteral (LitInt count))) <- Map.lookup s definitions,
            count >= 1,
            count <= 63 =
              Just (s, index, fromInteger count)
        shift _ = Nothing
        match operation =
          case operation of
            PtrAdd base offset -> do
              (s, index, count) <- shift offset
              Just (PtrAdd base index, s, count)
            Binary Add I64 left right ->
              ( do
                  (s, index, count) <- shift right
                  Just (Binary Add I64 left index, s, count)
              )
                <|> ( do
                        (s, index, count) <- shift left
                        Just (Binary Add I64 right index, s, count)
                    )
            _ -> Nothing
        matches = Map.fromList [(v, found) | Instruction [v] operation <- instructions, Just found <- [match operation]]
        removed = Set.fromList [s | (_, s, _) <- Map.elems matches]
        rewrite (Instruction [v] _)
          | Just (operation, _, _) <- Map.lookup v matches = [Instruction [v] operation]
          | Set.member v removed = []
        rewrite instruction = [instruction]
    selectLoads accepts shifts instructions = (concatMap rewrite instructions, foldr Map.delete (Map.union (Map.map snd matches) shifts) (Set.toList removed))
      where
        definitions = Map.fromList [(v, operation) | Instruction [v] operation@(PtrAdd _ (OperandVar _)) <- instructions]
        match (Load ty (Address (OperandVar address) 0 0 [] _) _)
          | useOnce address,
            not (isFloatType ty),
            ty /= I1,
            Just operation <- Map.lookup address definitions,
            count <- case Map.lookup address shifts of
              Just (FusedShift shift) -> shift
              _ -> 0,
            accepts ty count =
              Just (address, operation, FusedLoad ty count)
        match _ = Nothing
        matches = Map.fromList [(v, (operation, fusion)) | Instruction [v] load <- instructions, Just (_, operation, fusion) <- [match load]]
        removed = Set.fromList [address | Instruction [_] load <- instructions, Just (address, _, _) <- [match load]]
        rewrite (Instruction [v] _)
          | Just (operation, _) <- Map.lookup v matches = [Instruction [v] operation]
          | Set.member v removed = []
        rewrite instruction = [instruction]
    selectCarries instructions = (reverse kept, table)
      where
        defined = Set.fromList (concatMap instructionResults instructions)
        widened = Map.fromList [(c, w) | Instruction [w] (Convert ZExt I1 (OperandVar c) I64) <- instructions, useOnce c, useOnce w]
        additions = Map.fromListWith (<>) [(w, [(r, other)]) | Instruction [r] (Binary Add I64 left right) <- instructions, (OperandVar w, other) <- [(left, right), (right, left)]]
        (kept, table, _, _) = foldl' step ([], Map.empty, Set.empty, Set.empty) instructions
        step (done, fusions, ready, skipped) instruction@(Instruction results operation) =
          case (results, operation) of
            (_, _) | any (`Set.member` skipped) results -> (done, fusions, ready, skipped)
            ([_, c], Wide op I64 _ _)
              | op `elem` [AddCarry, SubBorrow],
                Just w <- Map.lookup c widened,
                (r, value) : _ <- [candidate | candidate@(r, value) <- Map.findWithDefault [] w additions, not (Set.member r skipped), available (Set.union ready (Set.fromList results)) value] ->
                  ( Instruction [r] (Binary Add I64 value (OperandVar c)) : instruction : done,
                    Map.insert c FusedFlags (Map.insert r (FusedIncrement op) fusions),
                    Set.insert r (foldr Set.insert ready results),
                    Set.insert w (Set.insert r skipped)
                  )
            _ -> (instruction : done, fusions, foldr Set.insert ready results, skipped)
        available ready value =
          case value of
            OperandVar v -> Set.member v ready || not (Set.member v defined)
            OperandLiteral _ -> True

-- | Select a single-use rotate as a logical operand within one block.
-- The allocation input reads the rotate source at its new use position.
-- The side table retains the count for instruction emission.
selectRotatedOperands :: Function -> (Function, Map Var Int)
selectRotatedOperands function = (function {functionBlocks = map fst selected}, Map.unions (map snd selected))
  where
    uses = readCounts function
    selected = map selectBlock (functionBlocks function)
    selectBlock block = (block {blockInstructions = concatMap rewrite (blockInstructions block)}, counts)
      where
        definitions = Map.fromList [(v, operation) | Instruction [v] operation <- blockInstructions block]
        matches = Map.fromList [(v, (operation, ty, left, source, rotated, count)) | Instruction [v] (Binary operation ty a b) <- blockInstructions block, operation `elem` [And, Or, Xor], ty `elem` [I32, I64], Just (left, source, rotated, count) <- [match ty a b <|> match ty b a]]
        counts = Map.map (\(_, _, _, _, _, count) -> count) matches
        removed = Set.fromList [rotated | (_, _, _, _, rotated, _) <- Map.elems matches]
        rewrite instruction@(Instruction [v] _) = case Map.lookup v matches of
          Just (operation, ty, left, source, _, _) -> [Instruction [v] (Binary operation ty left source)]
          Nothing | Set.member v removed -> []
          Nothing -> [instruction]
        rewrite instruction = [instruction]
        match ty left (OperandVar rotated) = do
          Binary operation operandTy source (OperandLiteral (LitInt amount)) <- Map.lookup rotated definitions
          if operandTy == ty && operation `elem` [RotL, RotR] && Map.lookup rotated uses == Just 1
            then Just (left, source, rotated, fromInteger ((if operation == RotL then negate amount else amount) `mod` toInteger (typeBits ty)))
            else Nothing
        match _ _ _ = Nothing
