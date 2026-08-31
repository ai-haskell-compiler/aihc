{-# LANGUAGE OverloadedStrings #-}

-- | AArch64 assembly vocabulary and runtime information-table rendering.
module Aihc.Arm64.Codegen.Runtime
  ( Arm64Error (..),
    CompileEnv (..),
    CompiledFunction (..),
    FunctionM,
    FunctionState (..),
    NodeInfo (..),
    ObservedProgram (..),
    RuntimeEnter (..),
    RuntimeInfo (..),
    RuntimeInfoKey (..),
    ValueEnv (..),
    address,
    applyArgumentRegisters,
    applyContinuationRegister,
    applyFunctionRegister,
    applyStackBytes,
    constructorStageLabel,
    continuationRuntimeInfos,
    functionCodeLabel,
    immediate,
    loadAt,
    loadByteOffset,
    loadLocation,
    localFunctionLabelWith,
    lookupRuntimeInfoLabel,
    makeNodeLines,
    makeNodeUncheckedLines,
    materializeNode,
    materializeNodeUnchecked,
    materializeValue,
    materializeValueTo,
    normalizedLiteralInteger,
    allocateNode,
    allocateNodeUnchecked,
    initializeNodeFields,
    renderAddrLiteralPool,
    renderEnterStubs,
    renderNativeControl,
    renderRuntimeInfos,
    renderRuntimeSupport,
    restoreApplyStackLines,
    runtimeInfoFunctionName,
    runtimeInfoKeyFields,
    runtimeInfoKeyNext,
    runtimeInfoKeyObjectKind,
    runtimeInfoKeyRemainingArity,
    runtimeInfoKeyStages,
    runtimeObjectClosure,
    runtimeObjectNode,
    storeAt,
    storeByteOffset,
    storeLocation,
    tshow,
  )
where

import Aihc.Arm64.Assemble
  ( Arm64Opcode (..),
    Arm64Statement,
    arm64Bytes,
    arm64Global,
    arm64Instruction,
    arm64Label,
    arm64Quad,
  )
import Aihc.Grin.Cps (ContinuationFrameKind, continuationFrameKindCode)
import Aihc.Grin.Syntax
import Aihc.Native (renderLinkedConstructorInfoSymbol, renderLinkedGlobalSymbol)
import Aihc.Native.BlockLayout qualified as BlockLayout
import Aihc.Native.RegisterAllocate (Location (..))
import Control.Monad (forM)
import Control.Monad.Trans.State.Strict (StateT)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data Arm64Error
  = Arm64MissingGlobal !Text
  | Arm64MissingFunction !FunctionName
  | Arm64MissingConstructor !Text
  | Arm64UnsupportedPrimitive !Text
  | Arm64UnsupportedExpression !Text
  | Arm64UnsupportedValue !Text
  | Arm64UnsupportedRuntimeRep !GrinRep
  | Arm64ObjectError !Text
  deriving (Eq, Show)

data CompileEnv = CompileEnv
  { compileFunctionLabels :: !(Map FunctionName Text),
    compileAddrLiteralLabels :: !(Map BS.ByteString Text),
    compileNodeInfoLabels :: !(Map RuntimeInfoKey Text),
    compileRuntimeInfos :: ![RuntimeInfo],
    compileContinuationFunctions :: !(Set.Set FunctionName),
    compileExposeAllFunctions :: !Bool,
    compileAllowUnsupportedPrimitives :: !Bool
  }

data ObservedProgram = ObservedProgram
  { observedObject :: !BL.ByteString,
    observedMetadataSource :: !Text
  }
  deriving (Eq, Show)

data FunctionState = FunctionState
  { functionNextLabel :: !Int,
    functionNextSlot :: !Int,
    functionBlocksRev :: ![BlockLayout.Block Text Arm64Statement]
  }

data CompiledFunction = CompiledFunction
  { compiledFunctionSlots :: !Int,
    compiledFunctionLines :: ![Arm64Statement]
  }

type FunctionM = StateT FunctionState (Either Arm64Error)

data ValueEnv = ValueEnv
  { valueCompileEnv :: !CompileEnv,
    valueLocations :: !(Map GrinVar (Location Text)),
    valueLabelPrefix :: !Text,
    valueFunctionName :: !FunctionName,
    valueFunctionParameters :: ![GrinVar],
    valueBodyLabel :: !Text
  }

data RuntimeInfo = RuntimeInfo
  { runtimeInfoLabel :: !Text,
    runtimeInfoIdentity :: !NodeInfo,
    runtimeInfoFields :: ![GrinRep],
    runtimeInfoRemainingArity :: !Int,
    runtimeInfoNext :: !(Maybe Text),
    runtimeInfoEnter :: !(Maybe RuntimeEnter),
    runtimeInfoFrameKind :: !(Maybe ContinuationFrameKind),
    runtimeInfoObjectKind :: !Int
  }

data RuntimeEnter = RuntimeEnter
  { runtimeEnterTarget :: !Text,
    runtimeEnterStoredCount :: !Int,
    runtimeEnterSuppliedCount :: !Int
  }

data RuntimeInfoKey
  = ConstructorRuntimeInfo !Text !Int
  | ClosureRuntimeInfo !FunctionName ![GrinRep] ![[GrinRep]]
  | ThunkRuntimeInfo !FunctionName ![GrinRep]
  deriving (Eq, Ord, Show)

data NodeInfo
  = InfoImmediate !Int
  | InfoAddress !Text
  | InfoConstructor !Text

makeNodeLines :: NodeInfo -> [Arm64Statement]
makeNodeLines info =
  ["  mov x0, x22"] <> infoLines info <> ["  bl _aihc_make_node"]
  where
    infoLines nodeInfo =
      case nodeInfo of
        InfoImmediate integer -> [immediate "x1" integer]
        InfoAddress label -> address "x1" label
        InfoConstructor label -> address "x1" label

makeNodeUncheckedLines :: NodeInfo -> [Arm64Statement]
makeNodeUncheckedLines info =
  init (makeNodeLines info) <> ["  bl _aihc_make_node_unchecked"]

-- | Describe a unary continuation before and after it receives its result.
-- The result can occupy several machine slots even though it is one GRIN
-- argument, hence the distinct runtime arity and supplied-slot count.
continuationRuntimeInfos :: ContinuationFrameKind -> Text -> Text -> Text -> [GrinRep] -> [GrinRep] -> [RuntimeInfo]
continuationRuntimeInfos frameKind infoLabel appliedInfoLabel target storedFields suppliedFields =
  [ RuntimeInfo
      infoLabel
      (InfoAddress target)
      storedFields
      1
      (Just appliedInfoLabel)
      (Just (RuntimeEnter target (length storedFields) (length suppliedFields)))
      (Just frameKind)
      runtimeObjectClosure,
    RuntimeInfo
      appliedInfoLabel
      (InfoAddress target)
      (storedFields <> suppliedFields)
      0
      Nothing
      Nothing
      (Just frameKind)
      runtimeObjectClosure
  ]

renderEnterStubs :: [RuntimeInfo] -> [Arm64Statement]
renderEnterStubs infos = concatMap renderStub uniqueTransfers
  where
    uniqueTransfers =
      Map.elems . Map.fromList $
        [ (enterTransferShape apply, apply)
        | info <- infos,
          Just apply <- [runtimeInfoEnter info],
          not (isDirectEnter apply)
        ]
    renderStub apply =
      [ ".section __TEXT,__text,regular,pure_instructions",
        ".p2align 3",
        arm64Label (sharedEnterEntryLabel apply)
      ]
        <> moveSupplied apply
        <> moveSuppliedOverflow apply
        <> loadStored apply
        <> restoreApplyStackLines (applyStackBytes (runtimeEnterSuppliedCount apply))
        <> ["  ldr x8, [x20]", "  ldr x8, [x8, #8]", "  br x8"]
    moveSupplied apply =
      concat
        [ placeArgument targetIndex source
        | (sourceIndex, source) <- reverse (zip [0 :: Int ..] applyArgumentRegisters),
          sourceIndex < runtimeEnterSuppliedCount apply,
          let targetIndex = runtimeEnterStoredCount apply + sourceIndex
        ]
    moveSuppliedOverflow apply
      | runtimeEnterSuppliedCount apply <= length applyArgumentRegisters = []
      | otherwise =
          ["  mov x9, sp"]
            <> concat
              [ ["  ldr x8, [x9], #8", storeAt "x8" "x19" targetIndex]
              | sourceIndex <- [length applyArgumentRegisters .. runtimeEnterSuppliedCount apply - 1],
                let targetIndex = runtimeEnterStoredCount apply + sourceIndex
              ]
    loadStored apply =
      concat
        [ if targetIndex < length applyArgumentRegisters
            then [loadByteOffset (applyArgumentRegisters !! targetIndex) applyFunctionRegister ((targetIndex + 1) * 8)]
            else
              [ loadByteOffset "x8" applyFunctionRegister ((targetIndex + 1) * 8),
                storeAt "x8" "x19" targetIndex
              ]
        | targetIndex <- [0 .. runtimeEnterStoredCount apply - 1]
        ]
    placeArgument targetIndex source
      | targetIndex < length applyArgumentRegisters =
          let destination = applyArgumentRegisters !! targetIndex
           in [arm64Instruction ArmMov [destination, source] | destination /= source]
      | otherwise = [storeAt source "x19" targetIndex]

enterEntryLabel :: RuntimeInfo -> Text
enterEntryLabel info =
  case runtimeInfoEnter info of
    Just apply | isDirectEnter apply -> runtimeEnterTarget apply
    Just apply -> sharedEnterEntryLabel apply
    Nothing -> runtimeInfoLabel info <> "_enter"

isDirectEnter :: RuntimeEnter -> Bool
isDirectEnter apply =
  runtimeEnterStoredCount apply == 0
    && runtimeEnterSuppliedCount apply <= length applyArgumentRegisters

enterTransferShape :: RuntimeEnter -> (Int, Int)
enterTransferShape apply = (runtimeEnterStoredCount apply, runtimeEnterSuppliedCount apply)

sharedEnterEntryLabel :: RuntimeEnter -> Text
sharedEnterEntryLabel apply =
  ".Laihc_enter_"
    <> tshow (runtimeEnterStoredCount apply)
    <> "_"
    <> tshow (runtimeEnterSuppliedCount apply)

applyFunctionRegister, applyContinuationRegister :: Text
applyFunctionRegister = "x20"
applyContinuationRegister = "x21"

applyArgumentRegisters :: [Text]
applyArgumentRegisters = ["x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"]

applyStackBytes :: Int -> Int
applyStackBytes suppliedCount = ((overflowCount * 8 + 15) `div` 16) * 16
  where
    overflowCount = max 0 (suppliedCount - length applyArgumentRegisters)

restoreApplyStackLines :: Int -> [Arm64Statement]
restoreApplyStackLines stackBytes
  | stackBytes == 0 = []
  | otherwise = [immediate "x8" stackBytes, "  add sp, sp, x8"]

renderRuntimeInfos :: [RuntimeInfo] -> [Arm64Statement]
renderRuntimeInfos infos = [".section __DATA,__const"] <> concatMap renderInfo infos
  where
    renderInfo info =
      bitmapLines
        <> [arm64Global (runtimeInfoLabel info) | "_aihc_constructor_" `T.isPrefixOf` runtimeInfoLabel info]
        <> [ ".p2align 3",
             arm64Label (runtimeInfoLabel info),
             identityLine (runtimeInfoIdentity info),
             entryLine (runtimeInfoIdentity info),
             arm64Quad (tshow (length fields)),
             arm64Quad (tshow (runtimeInfoRemainingArity info)),
             arm64Quad (if null fields then "0" else bitmapLabel),
             arm64Quad (fromMaybe "0" (runtimeInfoNext info)),
             arm64Quad (maybe "0" (const (enterEntryLabel info)) (runtimeInfoEnter info)),
             arm64Quad (tshow (continuationFrameKindCode (runtimeInfoFrameKind info))),
             arm64Quad (tshow (runtimeInfoObjectKind info))
           ]
      where
        fields = runtimeInfoFields info
        bitmapLabel = runtimeInfoLabel info <> "_bitmap"
        bitmapLines =
          if null fields
            then []
            else
              [ arm64Label bitmapLabel,
                arm64Bytes (BS.pack [if isPointerRuntimeRep runtimeRep then 1 else 0 | runtimeRep <- fields])
              ]
    identityLine nodeInfo =
      case nodeInfo of
        InfoImmediate integer -> arm64Quad (tshow integer)
        InfoAddress label -> arm64Quad label
        InfoConstructor label -> arm64Quad label
    entryLine nodeInfo =
      case nodeInfo of
        InfoImmediate {} -> arm64Quad "0"
        InfoAddress label -> arm64Quad label
        InfoConstructor {} -> arm64Quad "0"

renderRuntimeSupport :: CompileEnv -> [RuntimeInfo] -> [Arm64Statement]
renderRuntimeSupport env extraInfos =
  renderEnterStubs infos
    <> renderAddrLiteralPool env
    <> renderRuntimeInfos infos
  where
    infos = compileRuntimeInfos env <> extraInfos

runtimeObjectNode, runtimeObjectClosure, runtimeObjectThunk, runtimeObjectPartialConstructor :: Int
runtimeObjectNode = 0
runtimeObjectClosure = 1
runtimeObjectThunk = 2
runtimeObjectPartialConstructor = 3

loadLocation :: Text -> Location Text -> [Arm64Statement]
loadLocation destination location =
  case location of
    InRegister source
      | destination == source -> []
      | otherwise -> [arm64Instruction ArmMov [destination, source]]
    InHeapSpill slot -> [loadAt destination "x19" slot]

storeLocation :: Text -> Location Text -> [Arm64Statement]
storeLocation source location =
  case location of
    InRegister destination
      | destination == source -> []
      | otherwise -> [arm64Instruction ArmMov [destination, source]]
    InHeapSpill slot -> [storeAt source "x19" slot]

renderNativeControl :: [Arm64Statement]
renderNativeControl =
  [ ".section __TEXT,__text,regular,pure_instructions",
    ".p2align 3",
    ".Laihc_enter:",
    "  ldr x9, [x20]",
    "  ldr x10, [x9, #64]",
    "  cmp x10, #4",
    "  b.eq .Laihc_enter_indirection",
    "  ldr x9, [x9, #48]",
    "  cbz x9, .Laihc_invalid_enter",
    "  br x9",
    ".Laihc_enter_indirection:",
    "  ldr x20, [x20, #8]",
    "  b .Laihc_enter",
    ".Laihc_resume:",
    "  ldr w9, [x0]",
    "  cmp w9, #1",
    "  b.eq .Laihc_resume_apply",
    "  cmp w9, #2",
    "  b.eq .Laihc_resume_continue",
    "  cmp w9, #3",
    "  b.eq .Laihc_resume_raise",
    "  b .Laihc_invalid_enter",
    ".Laihc_resume_continue:",
    "  ldr x8, [x0, #24]",
    "  ldr x9, [x0, #32]",
    "  ldr x20, [x0, #8]",
    "  stp xzr, xzr, [x0]",
    "  stp xzr, xzr, [x0, #16]",
    "  str xzr, [x0, #32]",
    "  cbz x9, .Laihc_enter",
    "  cmp x9, #1",
    "  b.ne .Laihc_invalid_enter",
    "  mov x0, x8",
    "  b .Laihc_enter",
    ".Laihc_resume_apply:",
    "  ldr x8, [x0, #24]",
    "  ldr x9, [x0, #32]",
    "  ldr x20, [x0, #8]",
    "  ldr x21, [x0, #16]",
    "  stp xzr, xzr, [x0]",
    "  stp xzr, xzr, [x0, #16]",
    "  str xzr, [x0, #32]",
    "  cbz x9, .Laihc_enter",
    "  cmp x9, #1",
    "  b.ne .Laihc_invalid_enter",
    "  mov x0, x8",
    "  b .Laihc_enter",
    ".Laihc_resume_raise:",
    "  ldr x1, [x0, #8]",
    "  ldr x2, [x0, #16]",
    "  stp xzr, xzr, [x0]",
    "  stp xzr, xzr, [x0, #16]",
    "  str xzr, [x0, #32]",
    "  mov x0, x22",
    "  bl _aihc_raise",
    "  b .Laihc_resume",
    ".Laihc_eval:",
    "  str x0, [x19]",
    "  str x8, [x19, #8]",
    ".Laihc_eval_loop:",
    "  ldr x9, [x20]",
    "  ldr x10, [x9, #64]",
    "  cmp x10, #2",
    "  b.eq .Laihc_eval_thunk",
    "  cmp x10, #4",
    "  b.eq .Laihc_eval_indirection",
    "  cmp x10, #5",
    "  b.eq .Laihc_eval_blackhole",
    "  mov x0, x20",
    "  mov x20, x21",
    "  b .Laihc_enter",
    ".Laihc_eval_thunk:",
    "  mov x1, x20",
    "  mov x0, x22",
    "  bl _aihc_begin_blackhole",
    "  ldr x21, [x19]",
    "  b .Laihc_enter",
    ".Laihc_eval_indirection:",
    "  ldr x9, [x19, #8]",
    "  cbz x9, .Laihc_eval_unlifted_indirection",
    "  ldr x20, [x20, #8]",
    "  b .Laihc_eval_loop",
    ".Laihc_eval_unlifted_indirection:",
    "  ldr x0, [x20, #8]",
    "  mov x20, x21",
    "  b .Laihc_enter",
    ".Laihc_eval_blackhole:",
    "  mov x2, x21",
    "  mov x1, x20",
    "  mov x0, x22",
    "  bl _aihc_block_on_blackhole",
    "  b .Laihc_resume",
    ".Laihc_invalid_enter:",
    "  bl _aihc_no_match",
    "  brk #0"
  ]

lookupRuntimeInfoLabel :: CompileEnv -> RuntimeInfoKey -> Either Arm64Error Text
lookupRuntimeInfoLabel env key =
  case Map.lookup key (compileNodeInfoLabels env) of
    Just label -> Right label
    Nothing ->
      case key of
        ConstructorRuntimeInfo name remaining -> Right (constructorStageLabel name remaining)
        ClosureRuntimeInfo functionName _ _ -> Left (Arm64MissingFunction functionName)
        ThunkRuntimeInfo functionName _ -> Left (Arm64MissingFunction functionName)

functionCodeLabel :: CompileEnv -> FunctionName -> Either Arm64Error Text
functionCodeLabel env name =
  maybe (Left (Arm64MissingFunction name)) Right (Map.lookup name (compileFunctionLabels env))

constructorStageLabel :: Text -> Int -> Text
constructorStageLabel name remaining =
  "_" <> renderLinkedConstructorInfoSymbol name remaining

runtimeInfoKeyStages :: GrinNode -> [RuntimeInfoKey]
runtimeInfoKeyStages node =
  case grinNodeTag node of
    GrinConstructor name remaining -> [ConstructorRuntimeInfo name remaining]
    GrinClosure functionName argumentLayouts -> closureStages fields argumentLayouts
      where
        closureStages current remainingLayouts =
          ClosureRuntimeInfo functionName current remainingLayouts
            : case remainingLayouts of
              [] -> []
              layout : rest -> closureStages (current <> layout) rest
    GrinThunk functionName -> [ThunkRuntimeInfo functionName fields]
  where
    fields = map grinValueRuntimeRep (grinNodeFields node)

runtimeInfoFunctionName :: RuntimeInfoKey -> Maybe FunctionName
runtimeInfoFunctionName ConstructorRuntimeInfo {} = Nothing
runtimeInfoFunctionName (ClosureRuntimeInfo functionName _ _) = Just functionName
runtimeInfoFunctionName (ThunkRuntimeInfo functionName _) = Just functionName

runtimeInfoKeyFields :: RuntimeInfoKey -> [GrinRep]
runtimeInfoKeyFields ConstructorRuntimeInfo {} = []
runtimeInfoKeyFields (ClosureRuntimeInfo _ fields _) = fields
runtimeInfoKeyFields (ThunkRuntimeInfo _ fields) = fields

runtimeInfoKeyRemainingArity :: RuntimeInfoKey -> Int
runtimeInfoKeyRemainingArity (ConstructorRuntimeInfo _ remaining) = remaining
runtimeInfoKeyRemainingArity (ClosureRuntimeInfo _ _ argumentLayouts) = length argumentLayouts
runtimeInfoKeyRemainingArity ThunkRuntimeInfo {} = 0

runtimeInfoKeyObjectKind :: RuntimeInfoKey -> Int
runtimeInfoKeyObjectKind (ConstructorRuntimeInfo _ remaining)
  | remaining == 0 = runtimeObjectNode
  | otherwise = runtimeObjectPartialConstructor
runtimeInfoKeyObjectKind ClosureRuntimeInfo {} = runtimeObjectClosure
runtimeInfoKeyObjectKind ThunkRuntimeInfo {} = runtimeObjectThunk

runtimeInfoKeyNext :: RuntimeInfoKey -> Maybe RuntimeInfoKey
runtimeInfoKeyNext (ConstructorRuntimeInfo name remaining)
  | remaining > 0 = Just (ConstructorRuntimeInfo name (remaining - 1))
runtimeInfoKeyNext ConstructorRuntimeInfo {} = Nothing
runtimeInfoKeyNext (ClosureRuntimeInfo functionName fields (layout : rest)) =
  Just (ClosureRuntimeInfo functionName (fields <> layout) rest)
runtimeInfoKeyNext ClosureRuntimeInfo {} = Nothing
runtimeInfoKeyNext ThunkRuntimeInfo {} = Nothing

renderAddrLiteralPool :: CompileEnv -> [Arm64Statement]
renderAddrLiteralPool env =
  case Map.toAscList (compileAddrLiteralLabels env) of
    [] -> []
    literals -> [".section __TEXT,__const"] <> concatMap renderLiteral literals
  where
    renderLiteral (value, label) =
      ["  .p2align 3", arm64Label label]
        <> map (arm64Bytes . BS.pack) (chunksOf 32 (BS.unpack value <> [0]))

    chunksOf _ [] = []
    chunksOf size bytes = take size bytes : chunksOf size (drop size bytes)

functionLabel :: Int -> Text
functionLabel index = ".Laihc_function_" <> tshow index

localFunctionLabelWith :: Bool -> Int -> GrinFunction -> Text
localFunctionLabelWith exposeAllFunctions index _function
  | exposeAllFunctions = "_aihc_snapshot_function_" <> tshow index
  | otherwise = functionLabel index

loadAt :: Text -> Text -> Int -> Arm64Statement
loadAt destination base slot = loadByteOffset destination base (slot * 8)

storeAt :: Text -> Text -> Int -> Arm64Statement
storeAt source base slot = storeByteOffset source base (slot * 8)

loadByteOffset :: Text -> Text -> Int -> Arm64Statement
loadByteOffset destination base offset =
  arm64Instruction ArmLdr [destination, "[" <> base <> ", #" <> tshow offset <> "]"]

storeByteOffset :: Text -> Text -> Int -> Arm64Statement
storeByteOffset source base offset =
  arm64Instruction ArmStr [source, "[" <> base <> ", #" <> tshow offset <> "]"]

immediate :: (Integral value, Show value) => Text -> value -> Arm64Statement
immediate register value
  | integer >= -65536 && integer <= 65535 = arm64Instruction ArmMov [register, "#" <> rendered]
  | otherwise = arm64Instruction ArmLdr [register, "=" <> rendered]
  where
    integer = toInteger value
    rendered = T.pack (show value)

address :: Text -> Text -> [Arm64Statement]
address register label =
  [ arm64Instruction ArmAdrp [register, label <> "@PAGE"],
    arm64Instruction ArmAdd [register, register, label <> "@PAGEOFF"]
  ]

tshow :: (Show value) => value -> Text
tshow = T.pack . show

materializeValue :: ValueEnv -> GrinValue -> Either Arm64Error [Arm64Statement]
materializeValue env = materializeValueTo env "x0"

materializeValueTo :: ValueEnv -> Text -> GrinValue -> Either Arm64Error [Arm64Statement]
materializeValueTo env destination value =
  case value of
    GrinVarValue var ->
      case Map.lookup var (valueLocations env) of
        Just location -> Right (loadLocation destination location)
        Nothing -> Right (address destination ("_" <> renderLinkedGlobalSymbol (grinVarName var)))
    GrinGlobalValue name -> Right (address destination ("_" <> renderLinkedGlobalSymbol name))
    GrinLitValue literal -> materializeLiteralTo destination (valueCompileEnv env) literal

materializeLiteralTo :: Text -> CompileEnv -> GrinLiteral -> Either Arm64Error [Arm64Statement]
materializeLiteralTo destination env literal =
  case literal of
    GrinLitAddr value -> do
      label <-
        maybe
          (Left (Arm64UnsupportedValue "unregistered Addr# literal"))
          Right
          (Map.lookup value (compileAddrLiteralLabels env))
      pure (address destination label)
    _ ->
      case normalizedLiteralInteger literal of
        Just integer -> Right [immediate destination integer]
        Nothing -> Left (Arm64UnsupportedValue "string literal")

normalizedLiteralInteger :: GrinLiteral -> Maybe Integer
normalizedLiteralInteger literal =
  case literal of
    GrinLitInt runtimeRep integer -> Just (normalizeScalar runtimeRep integer)
    GrinLitChar _ character -> Just (normalizeUnsigned 64 (fromIntegral (ord character)))
    GrinLitString {} -> Nothing
    GrinLitAddr {} -> Nothing

normalizeScalar :: GrinRep -> Integer -> Integer
normalizeScalar runtimeRep integer =
  case runtimeRep of
    IntRep -> normalizeSigned 64 integer
    Int8Rep -> normalizeSigned 8 integer
    Int16Rep -> normalizeSigned 16 integer
    Int32Rep -> normalizeSigned 32 integer
    Int64Rep -> normalizeSigned 64 integer
    WordRep -> normalizeUnsigned 64 integer
    Word8Rep -> normalizeUnsigned 8 integer
    Word16Rep -> normalizeUnsigned 16 integer
    Word32Rep -> normalizeUnsigned 32 integer
    Word64Rep -> normalizeUnsigned 64 integer
    _ -> integer

normalizeSigned :: Int -> Integer -> Integer
normalizeSigned bits integer =
  let modulus = 2 ^ bits
      signBit = 2 ^ (bits - 1)
      unsigned = integer `mod` modulus
   in if unsigned >= signBit then unsigned - modulus else unsigned

normalizeUnsigned :: Int -> Integer -> Integer
normalizeUnsigned bits integer = integer `mod` (2 ^ bits)

materializeNode :: ValueEnv -> GrinNode -> Either Arm64Error [Arm64Statement]
materializeNode = materializeNodeWith allocateNode

materializeNodeUnchecked :: ValueEnv -> GrinNode -> Either Arm64Error [Arm64Statement]
materializeNodeUnchecked = materializeNodeWith allocateNodeUnchecked

materializeNodeWith :: (ValueEnv -> GrinNode -> Either Arm64Error [Arm64Statement]) -> ValueEnv -> GrinNode -> Either Arm64Error [Arm64Statement]
materializeNodeWith allocate env node = do
  allocationLines <- allocate env node
  if null (grinNodeFields node)
    then pure allocationLines
    else do
      fieldLines <- initializeNodeFields env node
      pure $ allocationLines <> ["  mov x20, x0"] <> fieldLines <> ["  mov x0, x20"]

allocateNode :: ValueEnv -> GrinNode -> Either Arm64Error [Arm64Statement]
allocateNode = allocateNodeWith makeNodeLines

allocateNodeUnchecked :: ValueEnv -> GrinNode -> Either Arm64Error [Arm64Statement]
allocateNodeUnchecked = allocateNodeWith makeNodeUncheckedLines

allocateNodeWith :: (NodeInfo -> [Arm64Statement]) -> ValueEnv -> GrinNode -> Either Arm64Error [Arm64Statement]
allocateNodeWith make env node = do
  info <- nodeHeader env node
  pure (make info)

initializeNodeFields :: ValueEnv -> GrinNode -> Either Arm64Error [Arm64Statement]
initializeNodeFields env node =
  fmap concat . forM (zip [0 :: Int ..] (grinNodeFields node)) $ \(index, field) -> do
    valueLines <- materializeValue env field
    pure (valueLines <> [storeAt "x0" "x20" (index + 1)])

nodeHeader :: ValueEnv -> GrinNode -> Either Arm64Error NodeInfo
nodeHeader env node =
  case grinNodeTag node of
    GrinConstructor name remaining -> do
      label <- lookupRuntimeInfoLabel compileEnv (ConstructorRuntimeInfo name remaining)
      pure (InfoAddress label)
    GrinClosure functionName argumentLayouts -> do
      label <- lookupRuntimeInfoLabel compileEnv (ClosureRuntimeInfo functionName fields argumentLayouts)
      pure (InfoAddress label)
    GrinThunk functionName -> do
      label <- lookupRuntimeInfoLabel compileEnv (ThunkRuntimeInfo functionName fields)
      pure (InfoAddress label)
  where
    compileEnv = valueCompileEnv env
    fields = map grinValueRuntimeRep (grinNodeFields node)
