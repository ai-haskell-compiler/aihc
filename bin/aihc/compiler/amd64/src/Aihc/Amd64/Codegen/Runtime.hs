{-# LANGUAGE OverloadedStrings #-}

-- | AMD64 assembly vocabulary, value materialization, and runtime metadata.
module Aihc.Amd64.Codegen.Runtime
  ( Amd64Error (..),
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
    allocateNode,
    allocateNodeUnchecked,
    applyArgumentRegisters,
    applyContinuationRegister,
    applyFunctionRegister,
    applyStackBytes,
    continuationRuntimeInfos,
    constructorStageLabel,
    functionCodeLabel,
    immediate,
    initializeNodeFields,
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
    offsetText,
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

import Aihc.Amd64.Assemble
  ( Amd64Opcode (..),
    Amd64Statement,
    amd64Align,
    amd64Bytes,
    amd64Global,
    amd64Instruction,
    amd64Label,
    amd64Quad,
    amd64Section,
  )
import Aihc.Grin.Cps (ContinuationFrameKind, continuationFrameKindCode)
import Aihc.Grin.Syntax
import Aihc.Native (renderLinkedConstructorInfoSymbol, renderLinkedGlobalSymbol)
import Aihc.Native.BlockLayout qualified as BlockLayout
import Aihc.Native.Object (SectionRole (..))
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

data Amd64Error
  = Amd64MissingGlobal !Text
  | Amd64MissingFunction !FunctionName
  | Amd64MissingConstructor !Text
  | Amd64UnsupportedPrimitive !Text
  | Amd64UnsupportedExpression !Text
  | Amd64UnsupportedValue !Text
  | Amd64UnsupportedRuntimeRep !GrinRep
  | Amd64ObjectError !Text
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
    functionBlocksRev :: ![BlockLayout.Block Text Amd64Statement]
  }

data CompiledFunction = CompiledFunction
  { compiledFunctionSlots :: !Int,
    compiledFunctionLines :: ![Amd64Statement]
  }

type FunctionM = StateT FunctionState (Either Amd64Error)

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

materializeValue :: ValueEnv -> GrinValue -> Either Amd64Error [Amd64Statement]
materializeValue env = materializeValueTo env "rax"

materializeValueTo :: ValueEnv -> Text -> GrinValue -> Either Amd64Error [Amd64Statement]
materializeValueTo env destination value =
  case value of
    GrinVarValue var ->
      case Map.lookup var (valueLocations env) of
        Just location -> Right (loadLocation destination location)
        Nothing -> Right [address destination (renderLinkedGlobalSymbol (grinVarName var))]
    GrinGlobalValue name -> Right [address destination (renderLinkedGlobalSymbol name)]
    GrinLitValue literal -> materializeLiteralTo destination (valueCompileEnv env) literal

materializeLiteralTo :: Text -> CompileEnv -> GrinLiteral -> Either Amd64Error [Amd64Statement]
materializeLiteralTo destination env literal =
  case literal of
    GrinLitAddr value -> do
      label <-
        maybe
          (Left (Amd64UnsupportedValue "unregistered Addr# literal"))
          Right
          (Map.lookup value (compileAddrLiteralLabels env))
      pure [address destination label]
    _ ->
      case normalizedLiteralInteger literal of
        Just integer -> Right [immediate destination integer]
        Nothing -> Left (Amd64UnsupportedValue "string literal")

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

materializeNode :: ValueEnv -> GrinNode -> Either Amd64Error [Amd64Statement]
materializeNode = materializeNodeWith allocateNode

materializeNodeUnchecked :: ValueEnv -> GrinNode -> Either Amd64Error [Amd64Statement]
materializeNodeUnchecked = materializeNodeWith allocateNodeUnchecked

materializeNodeWith :: (ValueEnv -> GrinNode -> Either Amd64Error [Amd64Statement]) -> ValueEnv -> GrinNode -> Either Amd64Error [Amd64Statement]
materializeNodeWith allocate env node = do
  allocationLines <- allocate env node
  if null (grinNodeFields node)
    then pure allocationLines
    else do
      fieldLines <- initializeNodeFields env node
      pure $ allocationLines <> [amd64Instruction AmdMov ["r13", "rax"]] <> fieldLines <> [amd64Instruction AmdMov ["rax", "r13"]]

allocateNode :: ValueEnv -> GrinNode -> Either Amd64Error [Amd64Statement]
allocateNode = allocateNodeWith makeNodeLines

allocateNodeUnchecked :: ValueEnv -> GrinNode -> Either Amd64Error [Amd64Statement]
allocateNodeUnchecked = allocateNodeWith makeNodeUncheckedLines

allocateNodeWith :: (NodeInfo -> [Amd64Statement]) -> ValueEnv -> GrinNode -> Either Amd64Error [Amd64Statement]
allocateNodeWith make env node = do
  info <- nodeHeader env node
  pure (make info)

initializeNodeFields :: ValueEnv -> GrinNode -> Either Amd64Error [Amd64Statement]
initializeNodeFields env node =
  fmap concat . forM (zip [0 :: Int ..] (grinNodeFields node)) $ \(index, field) -> do
    valueLines <- materializeValue env field
    pure (valueLines <> [storeAt "rax" "r13" (index + 1)])

nodeHeader :: ValueEnv -> GrinNode -> Either Amd64Error NodeInfo
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

data NodeInfo
  = InfoImmediate !Int
  | InfoAddress !Text
  | InfoConstructor !Text

makeNodeLines :: NodeInfo -> [Amd64Statement]
makeNodeLines info =
  [ amd64Instruction AmdMov ["rdi", "r15"],
    infoLine info,
    amd64Instruction AmdCall ["aihc_make_node"]
  ]
  where
    infoLine nodeInfo =
      case nodeInfo of
        InfoImmediate integer -> immediate "rsi" integer
        InfoAddress label -> address "rsi" label
        InfoConstructor label -> address "rsi" label

makeNodeUncheckedLines :: NodeInfo -> [Amd64Statement]
makeNodeUncheckedLines info =
  init (makeNodeLines info) <> [amd64Instruction AmdCall ["aihc_make_node_unchecked"]]

renderEnterStubs :: [RuntimeInfo] -> [Amd64Statement]
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
      [ amd64Section TextSection,
        amd64Align 4,
        amd64Label (sharedEnterEntryLabel apply)
      ]
        <> moveSupplied apply
        <> moveSuppliedOverflow apply
        <> loadStored apply
        <> restoreApplyStackLines (applyStackBytes (runtimeEnterSuppliedCount apply))
        <> [amd64Instruction AmdMov ["r11", "QWORD PTR [r12]"], amd64Instruction AmdMov ["r11", "QWORD PTR [r11 + 8]"], amd64Instruction AmdJmp ["r11"]]
    moveSupplied apply =
      concat
        [ placeArgument targetIndex source
        | (sourceIndex, source) <- reverse (zip [0 :: Int ..] applyArgumentRegisters),
          sourceIndex < runtimeEnterSuppliedCount apply,
          let targetIndex = runtimeEnterStoredCount apply + sourceIndex
        ]
    moveSuppliedOverflow apply =
      concat
        [ [ loadByteOffset "r11" "rsp" ((sourceIndex - length applyArgumentRegisters) * 8),
            storeAt "r11" "r14" targetIndex
          ]
        | sourceIndex <- [length applyArgumentRegisters .. runtimeEnterSuppliedCount apply - 1],
          let targetIndex = runtimeEnterStoredCount apply + sourceIndex
        ]
    loadStored apply =
      concat
        [ if targetIndex < length applyArgumentRegisters
            then [loadByteOffset (applyArgumentRegisters !! targetIndex) applyFunctionRegister ((targetIndex + 1) * 8)]
            else
              [ loadByteOffset "r11" applyFunctionRegister ((targetIndex + 1) * 8),
                storeAt "r11" "r14" targetIndex
              ]
        | targetIndex <- [0 .. runtimeEnterStoredCount apply - 1]
        ]
    placeArgument targetIndex source
      | targetIndex < length applyArgumentRegisters =
          let destination = applyArgumentRegisters !! targetIndex
           in [amd64Instruction AmdMov [destination, source] | destination /= source]
      | otherwise = [storeAt source "r14" targetIndex]

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
applyFunctionRegister = "r12"
applyContinuationRegister = "r13"

applyArgumentRegisters :: [Text]
applyArgumentRegisters = ["rax", "rdi", "rsi", "rdx", "rcx", "r8", "r9"]

applyStackBytes :: Int -> Int
applyStackBytes suppliedCount =
  ((overflowCount * 8 + 15) `div` 16) * 16
  where
    overflowCount = max 0 (suppliedCount - length applyArgumentRegisters)

restoreApplyStackLines :: Int -> [Amd64Statement]
restoreApplyStackLines stackBytes
  | stackBytes == 0 = []
  | otherwise = [amd64Instruction AmdAdd ["rsp", tshow stackBytes]]

renderRuntimeInfos :: [RuntimeInfo] -> [Amd64Statement]
renderRuntimeInfos infos =
  [amd64Section ReadOnlySection] <> concatMap renderInfo infos
  where
    renderInfo info =
      bitmapLines
        <> [amd64Global (runtimeInfoLabel info) | "aihc_constructor_" `T.isPrefixOf` runtimeInfoLabel info]
        <> [ amd64Align 3,
             amd64Label (runtimeInfoLabel info),
             identityLine (runtimeInfoIdentity info),
             entryLine (runtimeInfoIdentity info),
             amd64Quad (tshow (length fields)),
             amd64Quad (tshow (runtimeInfoRemainingArity info)),
             amd64Quad (if null fields then "0" else bitmapLabel),
             amd64Quad (fromMaybe "0" (runtimeInfoNext info)),
             amd64Quad (maybe "0" (const (enterEntryLabel info)) (runtimeInfoEnter info)),
             amd64Quad (tshow (continuationFrameKindCode (runtimeInfoFrameKind info))),
             amd64Quad (tshow (runtimeInfoObjectKind info))
           ]
      where
        fields = runtimeInfoFields info
        bitmapLabel = runtimeInfoLabel info <> "_bitmap"
        bitmapLines =
          if null fields
            then []
            else
              [ amd64Label bitmapLabel,
                amd64Bytes (BS.pack [if isPointerRuntimeRep runtimeRep then 1 else 0 | runtimeRep <- fields])
              ]
    identityLine nodeInfo =
      case nodeInfo of
        InfoImmediate integer -> amd64Quad (tshow integer)
        InfoAddress label -> amd64Quad label
        InfoConstructor label -> amd64Quad label
    entryLine nodeInfo =
      case nodeInfo of
        InfoImmediate {} -> amd64Quad "0"
        InfoAddress label -> amd64Quad label
        InfoConstructor {} -> amd64Quad "0"

renderRuntimeSupport :: CompileEnv -> [RuntimeInfo] -> [Amd64Statement]
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

loadLocation :: Text -> Location Text -> [Amd64Statement]
loadLocation destination location =
  case location of
    InRegister source
      | destination == source -> []
      | otherwise -> [amd64Instruction AmdMov [destination, source]]
    InHeapSpill slot -> [loadAt destination "r14" slot]

storeLocation :: Text -> Location Text -> [Amd64Statement]
storeLocation source location =
  case location of
    InRegister destination
      | destination == source -> []
      | otherwise -> [amd64Instruction AmdMov [destination, source]]
    InHeapSpill slot -> [storeAt source "r14" slot]

renderNativeControl :: [Amd64Statement]
renderNativeControl =
  [ amd64Section TextSection,
    amd64Align 4,
    amd64Label ".Laihc_enter",
    amd64Instruction AmdMov ["r11", "QWORD PTR [r12]"],
    amd64Instruction AmdMov ["r10", "QWORD PTR [r11 + 64]"],
    amd64Instruction AmdCmp ["r10", "4"],
    amd64Instruction AmdJe [".Laihc_enter_indirection"],
    amd64Instruction AmdMov ["r11", "QWORD PTR [r11 + 48]"],
    amd64Instruction AmdTest ["r11", "r11"],
    amd64Instruction AmdJz [".Laihc_invalid_enter"],
    amd64Instruction AmdJmp ["r11"],
    amd64Label ".Laihc_enter_indirection",
    amd64Instruction AmdMov ["r12", "QWORD PTR [r12 + 8]"],
    amd64Instruction AmdJmp [".Laihc_enter"],
    amd64Label ".Laihc_resume",
    amd64Instruction AmdMov ["r11d", "DWORD PTR [rax]"],
    amd64Instruction AmdCmp ["r11d", "1"],
    amd64Instruction AmdJe [".Laihc_resume_apply"],
    amd64Instruction AmdCmp ["r11d", "2"],
    amd64Instruction AmdJe [".Laihc_resume_continue"],
    amd64Instruction AmdCmp ["r11d", "3"],
    amd64Instruction AmdJe [".Laihc_resume_raise"],
    amd64Instruction AmdJmp [".Laihc_invalid_enter"],
    amd64Label ".Laihc_resume_continue",
    amd64Instruction AmdMov ["r10", "QWORD PTR [rax + 24]"],
    amd64Instruction AmdMov ["r11", "QWORD PTR [rax + 32]"],
    amd64Instruction AmdMov ["r12", "QWORD PTR [rax + 8]"],
    amd64Instruction AmdMov ["QWORD PTR [rax]", "0"],
    amd64Instruction AmdMov ["QWORD PTR [rax + 8]", "0"],
    amd64Instruction AmdMov ["QWORD PTR [rax + 16]", "0"],
    amd64Instruction AmdMov ["QWORD PTR [rax + 24]", "0"],
    amd64Instruction AmdMov ["QWORD PTR [rax + 32]", "0"],
    amd64Instruction AmdTest ["r11", "r11"],
    amd64Instruction AmdJz [".Laihc_enter"],
    amd64Instruction AmdCmp ["r11", "1"],
    amd64Instruction AmdJne [".Laihc_invalid_enter"],
    amd64Instruction AmdMov ["rax", "r10"],
    amd64Instruction AmdJmp [".Laihc_enter"],
    amd64Label ".Laihc_resume_apply",
    amd64Instruction AmdMov ["r10", "QWORD PTR [rax + 24]"],
    amd64Instruction AmdMov ["r11", "QWORD PTR [rax + 32]"],
    amd64Instruction AmdMov ["r12", "QWORD PTR [rax + 8]"],
    amd64Instruction AmdMov ["r13", "QWORD PTR [rax + 16]"],
    amd64Instruction AmdMov ["QWORD PTR [rax]", "0"],
    amd64Instruction AmdMov ["QWORD PTR [rax + 8]", "0"],
    amd64Instruction AmdMov ["QWORD PTR [rax + 16]", "0"],
    amd64Instruction AmdMov ["QWORD PTR [rax + 24]", "0"],
    amd64Instruction AmdMov ["QWORD PTR [rax + 32]", "0"],
    amd64Instruction AmdTest ["r11", "r11"],
    amd64Instruction AmdJz [".Laihc_enter"],
    amd64Instruction AmdCmp ["r11", "1"],
    amd64Instruction AmdJne [".Laihc_invalid_enter"],
    amd64Instruction AmdMov ["rax", "r10"],
    amd64Instruction AmdJmp [".Laihc_enter"],
    amd64Label ".Laihc_resume_raise",
    amd64Instruction AmdMov ["rsi", "QWORD PTR [rax + 8]"],
    amd64Instruction AmdMov ["rdx", "QWORD PTR [rax + 16]"],
    amd64Instruction AmdMov ["QWORD PTR [rax]", "0"],
    amd64Instruction AmdMov ["QWORD PTR [rax + 8]", "0"],
    amd64Instruction AmdMov ["QWORD PTR [rax + 16]", "0"],
    amd64Instruction AmdMov ["QWORD PTR [rax + 24]", "0"],
    amd64Instruction AmdMov ["QWORD PTR [rax + 32]", "0"],
    amd64Instruction AmdMov ["rdi", "r15"],
    amd64Instruction AmdCall ["aihc_raise"],
    amd64Instruction AmdJmp [".Laihc_resume"],
    amd64Label ".Laihc_eval",
    amd64Instruction AmdMov ["QWORD PTR [r14]", "rax"],
    amd64Instruction AmdMov ["QWORD PTR [r14 + 8]", "r11"],
    amd64Label ".Laihc_eval_loop",
    amd64Instruction AmdMov ["r11", "QWORD PTR [r12]"],
    amd64Instruction AmdMov ["r10", "QWORD PTR [r11 + 64]"],
    amd64Instruction AmdCmp ["r10", "2"],
    amd64Instruction AmdJe [".Laihc_eval_thunk"],
    amd64Instruction AmdCmp ["r10", "4"],
    amd64Instruction AmdJe [".Laihc_eval_indirection"],
    amd64Instruction AmdCmp ["r10", "5"],
    amd64Instruction AmdJe [".Laihc_eval_blackhole"],
    amd64Instruction AmdMov ["rax", "r12"],
    amd64Instruction AmdMov ["r12", "r13"],
    amd64Instruction AmdJmp [".Laihc_enter"],
    amd64Label ".Laihc_eval_thunk",
    amd64Instruction AmdMov ["rsi", "r12"],
    amd64Instruction AmdMov ["rdi", "r15"],
    amd64Instruction AmdCall ["aihc_begin_blackhole"],
    amd64Instruction AmdMov ["r13", "QWORD PTR [r14]"],
    amd64Instruction AmdJmp [".Laihc_enter"],
    amd64Label ".Laihc_eval_indirection",
    amd64Instruction AmdCmp ["QWORD PTR [r14 + 8]", "0"],
    amd64Instruction AmdJe [".Laihc_eval_unlifted_indirection"],
    amd64Instruction AmdMov ["r12", "QWORD PTR [r12 + 8]"],
    amd64Instruction AmdJmp [".Laihc_eval_loop"],
    amd64Label ".Laihc_eval_unlifted_indirection",
    amd64Instruction AmdMov ["rax", "QWORD PTR [r12 + 8]"],
    amd64Instruction AmdMov ["r12", "r13"],
    amd64Instruction AmdJmp [".Laihc_enter"],
    amd64Label ".Laihc_eval_blackhole",
    amd64Instruction AmdMov ["rdx", "r13"],
    amd64Instruction AmdMov ["rsi", "r12"],
    amd64Instruction AmdMov ["rdi", "r15"],
    amd64Instruction AmdCall ["aihc_block_on_blackhole"],
    amd64Instruction AmdJmp [".Laihc_resume"],
    amd64Label ".Laihc_invalid_enter",
    amd64Instruction AmdCall ["aihc_no_match"],
    amd64Instruction AmdUd2 []
  ]

lookupRuntimeInfoLabel :: CompileEnv -> RuntimeInfoKey -> Either Amd64Error Text
lookupRuntimeInfoLabel env key =
  case Map.lookup key (compileNodeInfoLabels env) of
    Just label -> Right label
    Nothing ->
      case key of
        ConstructorRuntimeInfo name remaining -> Right (constructorStageLabel name remaining)
        ClosureRuntimeInfo functionName _ _ -> Left (Amd64MissingFunction functionName)
        ThunkRuntimeInfo functionName _ -> Left (Amd64MissingFunction functionName)

functionCodeLabel :: CompileEnv -> FunctionName -> Either Amd64Error Text
functionCodeLabel env name =
  maybe (Left (Amd64MissingFunction name)) Right (Map.lookup name (compileFunctionLabels env))

constructorStageLabel :: Text -> Int -> Text
constructorStageLabel = renderLinkedConstructorInfoSymbol

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

renderAddrLiteralPool :: CompileEnv -> [Amd64Statement]
renderAddrLiteralPool env =
  case Map.toAscList (compileAddrLiteralLabels env) of
    [] -> []
    literals ->
      [amd64Section ReadOnlySection]
        <> concatMap renderLiteral literals
  where
    renderLiteral (value, label) =
      [amd64Align 3, amd64Label label]
        <> map (amd64Bytes . BS.pack) (chunksOf 32 (BS.unpack value <> [0]))

    chunksOf _ [] = []
    chunksOf size bytes = take size bytes : chunksOf size (drop size bytes)

functionLabel :: Int -> Text
functionLabel index = ".Laihc_function_" <> tshow index

localFunctionLabelWith :: Bool -> Int -> GrinFunction -> Text
localFunctionLabelWith exposeAllFunctions index _function
  | exposeAllFunctions = "aihc_snapshot_function_" <> tshow index
  | otherwise = functionLabel index

loadAt :: Text -> Text -> Int -> Amd64Statement
loadAt destination base slot = loadByteOffset destination base (slot * 8)

storeAt :: Text -> Text -> Int -> Amd64Statement
storeAt source base slot = storeByteOffset source base (slot * 8)

loadByteOffset :: Text -> Text -> Int -> Amd64Statement
loadByteOffset destination base offset =
  amd64Instruction AmdMov [destination, "QWORD PTR [" <> base <> offsetText offset <> "]"]

storeByteOffset :: Text -> Text -> Int -> Amd64Statement
storeByteOffset source base offset =
  amd64Instruction AmdMov ["QWORD PTR [" <> base <> offsetText offset <> "]", source]

offsetText :: Int -> Text
offsetText offset
  | offset == 0 = ""
  | offset > 0 = " + " <> tshow offset
  | otherwise = " - " <> tshow (abs offset)

immediate :: (Show value) => Text -> value -> Amd64Statement
immediate register value = amd64Instruction AmdMov [register, T.pack (show value)]

address :: Text -> Text -> Amd64Statement
address register label =
  amd64Instruction AmdLea [register, "[rip + " <> label <> "]"]

tshow :: (Show value) => value -> Text
tshow = T.pack . show
