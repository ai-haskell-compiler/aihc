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

import Aihc.Grin.Cps (ContinuationFrameKind, continuationFrameKindCode)
import Aihc.Grin.Syntax
import Aihc.Native (renderLinkedConstructorInfoSymbol, renderLinkedGlobalSymbol)
import Aihc.Native.BlockLayout qualified as BlockLayout
import Aihc.Native.RegisterAllocate (Location (..))
import Control.Monad (forM)
import Control.Monad.Trans.State.Strict (StateT)
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data Arm64Error
  = Arm64MissingEntry !Text
  | Arm64MissingGlobal !Text
  | Arm64MissingFunction !FunctionName
  | Arm64MissingConstructor !Text
  | Arm64UnsupportedPrimitive !Text
  | Arm64UnsupportedExpression !Text
  | Arm64UnsupportedValue !Text
  | Arm64UnsupportedRuntimeRep !GrinRep
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
  { observedAssembly :: !Text,
    observedMetadataSource :: !Text
  }
  deriving (Eq, Show)

data FunctionState = FunctionState
  { functionNextLabel :: !Int,
    functionNextSlot :: !Int,
    functionBlocksRev :: ![BlockLayout.Block Text Text]
  }

data CompiledFunction = CompiledFunction
  { compiledFunctionSlots :: !Int,
    compiledFunctionLines :: ![Text]
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

makeNodeLines :: NodeInfo -> [Text]
makeNodeLines info =
  [ "  mov x0, x22",
    infoLine info,
    "  bl _aihc_make_node"
  ]
  where
    infoLine nodeInfo =
      case nodeInfo of
        InfoImmediate integer -> immediate "x1" integer
        InfoAddress label -> address "x1" label
        InfoConstructor label -> address "x1" label

makeNodeUncheckedLines :: NodeInfo -> [Text]
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

renderEnterStubs :: [RuntimeInfo] -> [Text]
renderEnterStubs = concatMap renderStub
  where
    renderStub info =
      case runtimeInfoEnter info of
        Nothing -> []
        Just apply ->
          [ ".section __TEXT,__text,regular,pure_instructions",
            ".p2align 3",
            enterEntryLabel info <> ":"
          ]
            <> moveSupplied apply
            <> moveSuppliedOverflow apply
            <> loadStored apply
            <> restoreApplyStackLines (applyStackBytes (runtimeEnterSuppliedCount apply))
            <> ["  b " <> runtimeEnterTarget apply]
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
            then ["  ldr " <> applyArgumentRegisters !! targetIndex <> ", [" <> applyFunctionRegister <> ", #" <> tshow ((targetIndex + 1) * 8) <> "]"]
            else
              [ "  ldr x8, [" <> applyFunctionRegister <> ", #" <> tshow ((targetIndex + 1) * 8) <> "]",
                storeAt "x8" "x19" targetIndex
              ]
        | targetIndex <- [0 .. runtimeEnterStoredCount apply - 1]
        ]
    placeArgument targetIndex source
      | targetIndex < length applyArgumentRegisters =
          ["  mov " <> applyArgumentRegisters !! targetIndex <> ", " <> source]
      | otherwise = [storeAt source "x19" targetIndex]

enterEntryLabel :: RuntimeInfo -> Text
enterEntryLabel info = runtimeInfoLabel info <> "_enter"

applyFunctionRegister, applyContinuationRegister :: Text
applyFunctionRegister = "x20"
applyContinuationRegister = "x21"

applyArgumentRegisters :: [Text]
applyArgumentRegisters = ["x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7"]

applyStackBytes :: Int -> Int
applyStackBytes suppliedCount = ((overflowCount * 8 + 15) `div` 16) * 16
  where
    overflowCount = max 0 (suppliedCount - length applyArgumentRegisters)

restoreApplyStackLines :: Int -> [Text]
restoreApplyStackLines stackBytes
  | stackBytes == 0 = []
  | otherwise = [immediate "x8" stackBytes, "  add sp, sp, x8"]

renderRuntimeInfos :: [RuntimeInfo] -> [Text]
renderRuntimeInfos infos = [".section __DATA,__const"] <> concatMap renderInfo infos
  where
    renderInfo info =
      bitmapLines
        <> [".globl " <> runtimeInfoLabel info | "_aihc_constructor_" `T.isPrefixOf` runtimeInfoLabel info]
        <> [ ".p2align 3",
             runtimeInfoLabel info <> ":",
             identityLine (runtimeInfoIdentity info),
             entryLine (runtimeInfoIdentity info),
             "  .quad " <> tshow (length fields),
             "  .quad " <> tshow (runtimeInfoRemainingArity info),
             "  .quad " <> if null fields then "0" else bitmapLabel,
             "  .quad " <> fromMaybe "0" (runtimeInfoNext info),
             "  .quad " <> maybe "0" (const (enterEntryLabel info)) (runtimeInfoEnter info),
             "  .quad " <> tshow (continuationFrameKindCode (runtimeInfoFrameKind info)),
             "  .quad " <> tshow (runtimeInfoObjectKind info)
           ]
      where
        fields = runtimeInfoFields info
        bitmapLabel = runtimeInfoLabel info <> "_bitmap"
        bitmapLines =
          if null fields
            then []
            else
              [ bitmapLabel <> ":",
                "  .byte " <> T.intercalate ", " [if isPointerRuntimeRep runtimeRep then "1" else "0" | runtimeRep <- fields]
              ]
    identityLine nodeInfo =
      case nodeInfo of
        InfoImmediate integer -> "  .quad " <> tshow integer
        InfoAddress label -> "  .quad " <> label
        InfoConstructor label -> "  .quad " <> label
    entryLine nodeInfo =
      case nodeInfo of
        InfoImmediate {} -> "  .quad 0"
        InfoAddress label -> "  .quad " <> label
        InfoConstructor {} -> "  .quad 0"

renderRuntimeSupport :: CompileEnv -> [RuntimeInfo] -> [Text]
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

loadLocation :: Text -> Location Text -> [Text]
loadLocation destination location =
  case location of
    InRegister source
      | destination == source -> []
      | otherwise -> ["  mov " <> destination <> ", " <> source]
    InHeapSpill slot -> [loadAt destination "x19" slot]

storeLocation :: Text -> Location Text -> [Text]
storeLocation source location =
  case location of
    InRegister destination
      | destination == source -> []
      | otherwise -> ["  mov " <> destination <> ", " <> source]
    InHeapSpill slot -> [storeAt source "x19" slot]

renderNativeControl :: [Text]
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

renderAddrLiteralPool :: CompileEnv -> [Text]
renderAddrLiteralPool env =
  case Map.toAscList (compileAddrLiteralLabels env) of
    [] -> []
    literals -> [".section __TEXT,__const"] <> concatMap renderLiteral literals
  where
    renderLiteral (value, label) =
      ["  .p2align 3", label <> ":"]
        <> map renderBytes (chunksOf 32 (BS.unpack value <> [0]))

    renderBytes bytes = "  .byte " <> T.intercalate ", " (map tshow bytes)

    chunksOf _ [] = []
    chunksOf size bytes = take size bytes : chunksOf size (drop size bytes)

functionLabel :: Int -> Text
functionLabel index = ".Laihc_function_" <> tshow index

localFunctionLabelWith :: Bool -> Int -> GrinFunction -> Text
localFunctionLabelWith exposeAllFunctions index _function
  | exposeAllFunctions = "_aihc_snapshot_function_" <> tshow index
  | otherwise = functionLabel index

loadAt :: Text -> Text -> Int -> Text
loadAt destination base slot = loadByteOffset destination base (slot * 8)

storeAt :: Text -> Text -> Int -> Text
storeAt source base slot = storeByteOffset source base (slot * 8)

loadByteOffset :: Text -> Text -> Int -> Text
loadByteOffset destination base offset =
  "  ldr " <> destination <> ", [" <> base <> ", #" <> tshow offset <> "]"

storeByteOffset :: Text -> Text -> Int -> Text
storeByteOffset source base offset =
  "  str " <> source <> ", [" <> base <> ", #" <> tshow offset <> "]"

immediate :: (Show value) => Text -> value -> Text
immediate register value = "  ldr " <> register <> ", =" <> T.pack (show value)

address :: Text -> Text -> Text
address register label =
  "  adrp " <> register <> ", " <> label <> "@PAGE\n  add " <> register <> ", " <> register <> ", " <> label <> "@PAGEOFF"

tshow :: (Show value) => value -> Text
tshow = T.pack . show

materializeValue :: ValueEnv -> GrinValue -> Either Arm64Error [Text]
materializeValue env = materializeValueTo env "x0"

materializeValueTo :: ValueEnv -> Text -> GrinValue -> Either Arm64Error [Text]
materializeValueTo env destination value =
  case value of
    GrinVarValue var ->
      case Map.lookup var (valueLocations env) of
        Just location -> Right (loadLocation destination location)
        Nothing -> Right [address destination ("_" <> renderLinkedGlobalSymbol (grinVarName var))]
    GrinGlobalValue name -> Right [address destination ("_" <> renderLinkedGlobalSymbol name)]
    GrinLitValue literal -> materializeLiteralTo destination (valueCompileEnv env) literal

materializeLiteralTo :: Text -> CompileEnv -> GrinLiteral -> Either Arm64Error [Text]
materializeLiteralTo destination env literal =
  case literal of
    GrinLitAddr value -> do
      label <-
        maybe
          (Left (Arm64UnsupportedValue "unregistered Addr# literal"))
          Right
          (Map.lookup value (compileAddrLiteralLabels env))
      pure [address destination label]
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

materializeNode :: ValueEnv -> GrinNode -> Either Arm64Error [Text]
materializeNode = materializeNodeWith allocateNode

materializeNodeUnchecked :: ValueEnv -> GrinNode -> Either Arm64Error [Text]
materializeNodeUnchecked = materializeNodeWith allocateNodeUnchecked

materializeNodeWith :: (ValueEnv -> GrinNode -> Either Arm64Error [Text]) -> ValueEnv -> GrinNode -> Either Arm64Error [Text]
materializeNodeWith allocate env node = do
  allocationLines <- allocate env node
  fieldLines <- initializeNodeFields env node
  pure $ allocationLines <> ["  mov x20, x0"] <> fieldLines <> ["  mov x0, x20"]

allocateNode :: ValueEnv -> GrinNode -> Either Arm64Error [Text]
allocateNode = allocateNodeWith makeNodeLines

allocateNodeUnchecked :: ValueEnv -> GrinNode -> Either Arm64Error [Text]
allocateNodeUnchecked = allocateNodeWith makeNodeUncheckedLines

allocateNodeWith :: (NodeInfo -> [Text]) -> ValueEnv -> GrinNode -> Either Arm64Error [Text]
allocateNodeWith make env node = do
  info <- nodeHeader env node
  pure (make info)

initializeNodeFields :: ValueEnv -> GrinNode -> Either Arm64Error [Text]
initializeNodeFields env node =
  fmap concat . forM (zip [0 :: Int ..] (grinNodeFields node)) $ \(index, field) -> do
    valueLines <- materializeValue env field
    pure $
      valueLines
        <> [ "  mov x2, x0",
             "  mov x0, x20",
             immediate "x1" index,
             "  bl _aihc_set_field"
           ]

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
