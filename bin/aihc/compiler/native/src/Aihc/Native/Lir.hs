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
    Layout (..),
    Location (..),
    MoveSource (..),
    NativeBackend (..),
    NativeM,
    ObjectState (..),
    SlotEffect (..),
    Source (..),
    blockArgumentMoves,
    cArgumentMoves,
    calleeSignature,
    classify,
    compileNativeStatements,
    compileNativeStatementsWith,
    compileNativeChunksWith,
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

import Aihc.Lir.Lint (LintError, lintModule)
import Aihc.Lir.RegAlloc (Allocation (..), Registers, allocateRegistersFor, readCounts)
import Aihc.Lir.Resolve (resolveConstants, resolvedSwitchCaseValue, unresolvedConstant)
import Aihc.Lir.Syntax
import Aihc.Native.Move (orderMoves)
import Aihc.Native.Object (Name (..), SectionRole (..))
import Control.Monad (forM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify, put, runStateT)
import Data.ByteString qualified as BS
import Data.Int (Int64)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import GHC.Float (castDoubleToWord64, castFloatToWord32, double2Float)

-- Object state

-- | Trap messages of one object, and the next private label index.
data ObjectState = ObjectState
  { objectTraps :: !(Map Text Int),
    objectNextLabel :: !Int,
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
    nbArgumentRegisters :: ![register],
    nbResultRegisters :: ![register],
    nbPreservedRegisters :: ![register],
    nbScratchLeft :: register,
    nbScratchRight :: register,
    nbCycleScratch :: register,
    nbSlotMoveScratch :: register,
    nbFloatArgCount :: Int,
    nbCIntegerLimitWord :: Text,
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
    nbLeaveFrame :: Ctx register -> Int -> [statement],
    nbSaveReg :: register -> Int -> statement,
    nbZeroWord :: Int -> statement,
    nbReturn :: Ctx register -> [statement],
    nbLoadSlot :: register -> Int -> statement,
    nbStoreSlot :: register -> Int -> statement,
    nbMove :: register -> register -> [statement],
    nbLiteralInto :: Type -> register -> Literal -> [statement],
    nbStoreSlotImmediate :: Int -> Integer -> Maybe statement,
    nbCanonicalize :: Type -> register -> [statement],
    nbFloatFromVec :: Type -> Int -> register -> [statement],
    nbFloatToVec :: Type -> register -> Int -> [statement],
    nbCCallExtra :: Int -> [statement],
    nbJump :: Name -> statement,
    nbCanFuseFloatCompare :: CompareOp -> Bool,
    nbConditionTest :: Ctx register -> Maybe Fused -> Operand -> NativeM error ([statement], BranchTest statement error),
    nbCompareAndBranchEqual :: Ctx register -> Type -> register -> Integer -> Name -> [statement],
    nbCReturnFloat :: CallingConvention -> [Type] -> [statement],
    nbBinary :: Ctx register -> BinaryOp -> Type -> register -> register -> Operand -> NativeM error [statement],
    nbUnary :: UnaryOp -> Type -> register -> register -> [statement],
    nbWide :: WideOp -> Type -> register -> register -> register -> register -> [statement],
    nbCompare :: Ctx register -> CompareOp -> Type -> register -> Operand -> Operand -> [statement],
    nbFloatBinary :: FloatBinaryOp -> Type -> register -> register -> register -> [statement],
    nbFloatUnary :: FloatUnaryOp -> Type -> register -> register -> [statement],
    nbConvert :: Ctx register -> ConvertOp -> Type -> Type -> register -> register -> NativeM error [statement],
    nbSelect :: Ctx register -> Type -> register -> Operand -> Operand -> Operand -> [statement],
    nbLoad :: Ctx register -> Type -> Operand -> Integer -> register -> [statement],
    nbStore :: Ctx register -> Type -> Operand -> Operand -> Integer -> [statement],
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
    layoutAllocs :: !(Map Var (Int, Int)),
    layoutSize :: Int,
    layoutFramed :: Bool
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
    ctxReads :: !(Map Var Int)
  }

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
  let index = objectNextLabel state
  put state {objectNextLabel = index + 1}
  pure index

freshLabel :: Text -> NativeM error Name
freshLabel kind = do
  index <- nextLabelId
  pure (LocalName index (".Llir_" <> kind <> "_" <> tshow index))

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
  case if lint then lintModule lirModule else [] of
    [] -> functionChunks initialState (zip [0 ..] [function | ItemFunction function <- items])
    errors -> [Left (nbLintErrors backend errors)]
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
    Module items = resolveConstants lirModule
    initialState =
      ObjectState
        { objectTraps = Map.empty,
          objectNextLabel = 0,
          objectLocalTraps = isJust (nbTrapTrampoline backend),
          objectFunctionIndex = 0,
          objectFunctionTraps = Map.empty
        }
    signatures =
      Map.fromList
        ( [(functionName function, functionSignature function) | ItemFunction function <- items]
            <> [(externFunctionName external, externFunctionSignature external) | ItemExternFunction external <- items]
        )

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
        DataZero count -> [nbBytes backend (BS.replicate (fromInteger count) 0)]

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

typeBytes :: Type -> Int
typeBytes ty = max 1 (typeBits ty `div` 8)

-- | Split the parameters of a C function into the integer class and the
-- float class. Each list pairs the parameter index with its type.
classify :: [Type] -> ([(Int, Type)], [(Int, Type)])
classify types =
  ( [(index, ty) | (index, ty) <- zip [0 ..] types, not (isFloatType ty)],
    [(index, ty) | (index, ty) <- zip [0 ..] types, isFloatType ty]
  )

overflowBytes :: NativeBackend statement register error -> Int -> Int
overflowBytes backend count =
  ((max 0 (count - length (nbArgumentRegisters backend)) * 8 + 15) `div` 16) * 16

-- Functions

compileFunction :: (Ord register) => NativeBackend statement register error -> Map Symbol Signature -> Int -> Function -> NativeM error [statement]
compileFunction backend signatures index function = do
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
            ctxReads = readCounts function
          }
  when (functionConvention function == CConvention) $ do
    let (integers, floats) = classify (map snd (functionParameters function))
    when (length integers > length (nbArgumentRegisters backend)) $
      unsupported backend ("function " <> unSymbol (functionName function) <> " has more than " <> nbCIntegerLimitWord backend <> " integer C parameters")
    when (length floats > nbFloatArgCount backend) $
      unsupported backend ("function " <> unSymbol (functionName function) <> " has more than eight float C parameters")
  prologue <- functionPrologue backend ctx
  body <- concat <$> mapM (compileBlock backend ctx) (zip3 (True : repeat False) blocks (map Just (drop 1 blocks) <> [Nothing]))
  -- The trampolines follow the last block, which ends in a transfer, so
  -- nothing falls into them.
  trampolines <- functionTrapTrampolines backend
  pure
    ( [nbSection backend TextSection, nbAlign backend (nbCodeAlign backend)]
        <> [nbGlobal backend symbol | functionLinkage function == Export]
        <> [nbLabel backend (SymbolName symbol)]
        <> elideSlotReloadsWith (nbAsCode backend) (prologue <> body)
        <> trampolines
    )
  where
    symbol = nbSymbol backend (functionName function)

-- | Drop a read that the destination register already holds, and a store
-- of what the slot already holds.
elideSlotReloadsWith :: (statement -> Maybe SlotEffect) -> [statement] -> [statement]
elideSlotReloadsWith asCode = go IntMap.empty
  where
    go held statements =
      case statements of
        [] -> []
        statement : rest ->
          case asCode statement of
            Nothing -> statement : go IntMap.empty rest
            Just effect ->
              case effect of
                LoadsSlot register offset -> reads' statement rest held register (FromSlot offset)
                MovesRegister destination source
                  | IntMap.lookup source held == Just (FromRegister destination) -> go held rest
                  | otherwise -> reads' statement rest held destination (FromRegister source)
                StoresSlot register offset
                  | IntMap.lookup register held == Just (FromSlot offset) -> go held rest
                  | otherwise ->
                      statement : go (IntMap.insert register (FromSlot offset) (IntMap.filter (/= FromSlot offset) held)) rest
                WritesSlot offset -> statement : go (IntMap.filter (/= FromSlot offset) held) rest
                Writes registers -> statement : go (foldr invalidate held registers) rest
                Forgets -> statement : go IntMap.empty rest
    reads' statement rest held register source
      | IntMap.lookup register held == Just source = go held rest
      | otherwise = statement : go (IntMap.insert register source (invalidate register held)) rest
    invalidate register held = IntMap.filter (/= FromRegister register) (IntMap.delete register held)

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
      slotsEnd = 8 * Map.size slots
      saved = zip savedRegisters [slotsEnd, slotsEnd + 8 ..]
      allocsStart = slotsEnd + 8 * length saved
      allocations = [(var, size, alignment) | block <- take 1 blocks, Instruction [var] (StackAlloc size alignment) <- blockInstructions block]
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
        layoutAllocs = allocs,
        layoutSize = size,
        layoutFramed = size > 0 || not (null calls)
      }

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
          CConvention ->
            let (integers, floats) = classify (map snd parameters)
                names = map fst parameters
             in concat [nbCanonicalize backend ty register | ((_, ty), register) <- zip integers (nbArgumentRegisters backend)]
                  <> parallelMove backend [(home ctx (names !! index), SourceLocation (LocRegister register)) | ((index, _), register) <- zip integers (nbArgumentRegisters backend)]
                  <> concat
                    [ nbFloatFromVec backend ty slot (nbScratchLeft backend)
                        <> nbCanonicalize backend ty (nbScratchLeft backend)
                        <> parallelMove backend [(home ctx (names !! index), SourceLocation (LocRegister (nbScratchLeft backend)))]
                    | ((index, ty), slot) <- zip floats [0 ..]
                    ]
  pure
    ( nbPrologueFrame backend (layoutFramed layout) (layoutSize layout)
        <> saveRegisters backend ctx
        <> concatMap zeroAllocation (Map.elems (layoutAllocs layout))
        <> moves
    )
  where
    function = ctxFunction ctx
    layout = ctxLayout ctx
    parameterLocation index
      | index < length (nbArgumentRegisters backend) = LocRegister (nbArgumentRegisters backend !! index)
      | otherwise = LocSlot (frameBytes backend layout + nbReturnAddressGap backend + 8 * (index - length (nbArgumentRegisters backend)))
    zeroAllocation (offset, size) =
      [nbZeroWord backend (offset + position) | position <- [0, 8 .. size - 1]]

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

compileBlock :: (Eq register) => NativeBackend statement register error -> Ctx register -> (Bool, Block, Maybe Block) -> NativeM error [statement]
compileBlock backend ctx (entry, block, next) = do
  let (instructions, fused) = fuseCompare backend ctx (blockInstructions block) (blockTerminator block)
  lines' <- concat <$> mapM (compileInstruction backend ctx) instructions
  terminator <- compileTerminator backend ctx (blockLabel <$> next) fused (blockTerminator block)
  pure ([nbLabel backend (ctxLabels ctx Map.! blockLabel block) | not entry] <> lines' <> terminator)

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
      if null trueMoves && null falseMoves && isNext whenFalse
        then do
          branch <- btWhen test (labelOf whenTrue)
          pure (setup <> branch)
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

cArgumentMoves :: (Eq register) => NativeBackend statement register error -> Ctx register -> [Type] -> [Operand] -> NativeM error [statement]
cArgumentMoves backend ctx parameterTypes arguments = do
  let (integers, floats) = classify (take (length arguments) (parameterTypes <> repeat I64))
  when (length integers > length (nbArgumentRegisters backend)) $
    unsupported backend ("C call with more than " <> nbCIntegerLimitWord backend <> " integer arguments")
  when (length floats > nbFloatArgCount backend) $
    unsupported backend "C call with more than eight float arguments"
  pure
    ( concat
        [ loads <> nbFloatToVec backend ty register slot
        | ((index, ty), slot) <- zip floats [0 ..],
          let (loads, register) = operandIn backend ctx 0 ty (nbScratchLeft backend) (arguments !! index)
        ]
        <> parallelMove
          backend
          [ (LocRegister register, operandSource ctx ty (arguments !! index))
          | ((index, ty), register) <- zip integers (nbArgumentRegisters backend)
          ]
        <> nbCCallExtra backend (length floats)
    )

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

compileInstruction :: (Eq register) => NativeBackend statement register error -> Ctx register -> Instruction -> NativeM error [statement]
compileInstruction backend ctx (Instruction results operation) =
  case operation of
    Binary op ty left right -> do
      let (loads, a) = operandIn backend ctx 0 ty (nbScratchLeft backend) left
      single $ \dst -> do
        body <- nbBinary backend ctx op ty dst a right
        pure (loads <> body)
    Unary op ty value -> do
      let (loads, a) = operandIn backend ctx 0 ty (nbScratchLeft backend) value
      single $ \dst -> pure (loads <> nbUnary backend op ty dst a)
    Wide op ty left right -> do
      let (loads, a) = operandIn backend ctx 0 ty (nbScratchLeft backend) left
          (loads', b) = operandIn backend ctx 0 ty (nbScratchRight backend) right
      pair $ \low high -> pure (loads <> loads' <> nbWide backend op ty low high a b)
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
    Load ty (Address base offset) _ ->
      single $ \dst -> pure (nbLoad backend ctx ty base offset dst)
    Store ty value (Address base offset) _ ->
      pure (nbStore backend ctx ty value base offset)
    PtrAdd base offset -> do
      let (loads, a) = operandIn backend ctx 0 Ptr (nbScratchLeft backend) base
      single $ \dst -> pure (loads <> nbPtrAdd backend ctx a offset dst)
    StackAlloc _ _ ->
      case results of
        [var]
          | Just (offset, _) <- Map.lookup var (layoutAllocs (ctxLayout ctx)) ->
              single $ \dst -> pure (nbStackAddr backend dst offset)
        _ -> unsupported backend "stack.alloc without a placed result"
    GlobalGet symbol ->
      single $ \dst -> pure (nbGlobalLoad backend dst (nbSymbol backend symbol))
    GlobalSet symbol value -> do
      let (loads, a) = operandIn backend ctx 0 I64 (nbScratchLeft backend) value
      pure (loads <> nbGlobalStore backend a (nbSymbol backend symbol))
    Call symbol arguments -> nbCall backend ctx (Left symbol) arguments results
    CallIndirect target arguments signature -> nbCallIndirect backend ctx target arguments signature results
  where
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
