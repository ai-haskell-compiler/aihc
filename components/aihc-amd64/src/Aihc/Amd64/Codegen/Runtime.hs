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
    constructorId,
    constructorStageLabel,
    functionCodeLabel,
    globalSlot,
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
    storeGlobal,
    storeLocation,
    tshow,
  )
where

import Aihc.Grin.Cps (ContinuationFrameKind, continuationFrameKindCode)
import Aihc.Grin.Syntax
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

data Amd64Error
  = Amd64MissingEntry !Text
  | Amd64MissingGlobal !Text
  | Amd64MissingFunction !FunctionName
  | Amd64MissingConstructor !Text
  | Amd64UnsupportedPrimitive !Text
  | Amd64UnsupportedExpression !Text
  | Amd64UnsupportedValue !Text
  | Amd64UnsupportedRuntimeRep !GrinRep
  deriving (Eq, Show)

data CompileEnv = CompileEnv
  { compileConstructorIds :: !(Map Text Int),
    compileConstructorArities :: !(Map Text Int),
    compileGlobalSlots :: !(Map Text Int),
    compileFunctionLabels :: !(Map FunctionName Text),
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

materializeValue :: ValueEnv -> GrinValue -> Either Amd64Error [Text]
materializeValue env = materializeValueTo env "rax"

materializeValueTo :: ValueEnv -> Text -> GrinValue -> Either Amd64Error [Text]
materializeValueTo env destination value =
  case value of
    GrinVarValue var ->
      case Map.lookup var (valueLocations env) of
        Just location -> Right (loadLocation destination location)
        Nothing -> do
          slot <- globalSlot (valueCompileEnv env) (grinVarName var)
          pure [loadByteOffset "r11" "r15" 0, loadAt destination "r11" slot]
    GrinGlobalValue name -> do
      slot <- globalSlot (valueCompileEnv env) name
      pure [loadByteOffset "r11" "r15" 0, loadAt destination "r11" slot]
    GrinLitValue literal -> materializeLiteralTo destination (valueCompileEnv env) literal

materializeLiteralTo :: Text -> CompileEnv -> GrinLiteral -> Either Amd64Error [Text]
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

materializeNode :: ValueEnv -> GrinNode -> Either Amd64Error [Text]
materializeNode = materializeNodeWith allocateNode

materializeNodeUnchecked :: ValueEnv -> GrinNode -> Either Amd64Error [Text]
materializeNodeUnchecked = materializeNodeWith allocateNodeUnchecked

materializeNodeWith :: (ValueEnv -> GrinNode -> Either Amd64Error [Text]) -> ValueEnv -> GrinNode -> Either Amd64Error [Text]
materializeNodeWith allocate env node = do
  allocationLines <- allocate env node
  fieldLines <- initializeNodeFields env node
  pure $ allocationLines <> ["  mov r13, rax"] <> fieldLines <> ["  mov rax, r13"]

allocateNode :: ValueEnv -> GrinNode -> Either Amd64Error [Text]
allocateNode = allocateNodeWith makeNodeLines

allocateNodeUnchecked :: ValueEnv -> GrinNode -> Either Amd64Error [Text]
allocateNodeUnchecked = allocateNodeWith makeNodeUncheckedLines

allocateNodeWith :: (NodeInfo -> [Text]) -> ValueEnv -> GrinNode -> Either Amd64Error [Text]
allocateNodeWith make env node = do
  info <- nodeHeader env node
  pure (make info)

initializeNodeFields :: ValueEnv -> GrinNode -> Either Amd64Error [Text]
initializeNodeFields env node =
  fmap concat . forM (zip [0 :: Int ..] (grinNodeFields node)) $ \(index, field) -> do
    valueLines <- materializeValue env field
    pure $
      valueLines
        <> [ "  mov rdx, rax",
             "  mov rdi, r13",
             immediate "rsi" index,
             "  call aihc_set_field"
           ]

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

makeNodeLines :: NodeInfo -> [Text]
makeNodeLines info =
  [ "  mov rdi, r15",
    infoLine info,
    "  call aihc_make_node"
  ]
  where
    infoLine nodeInfo =
      case nodeInfo of
        InfoImmediate integer -> immediate "rsi" integer
        InfoAddress label -> address "rsi" label

makeNodeUncheckedLines :: NodeInfo -> [Text]
makeNodeUncheckedLines info =
  init (makeNodeLines info) <> ["  call aihc_make_node_unchecked"]

renderEnterStubs :: [RuntimeInfo] -> [Text]
renderEnterStubs = concatMap renderStub
  where
    renderStub info =
      case runtimeInfoEnter info of
        Nothing -> []
        Just apply ->
          [ ".text",
            ".p2align 4",
            enterEntryLabel info <> ":"
          ]
            <> moveSupplied apply
            <> moveSuppliedOverflow apply
            <> loadStored apply
            <> restoreApplyStackLines (applyStackBytes (runtimeEnterSuppliedCount apply))
            <> ["  jmp " <> runtimeEnterTarget apply]
    moveSupplied apply =
      concat
        [ placeArgument targetIndex source
        | (sourceIndex, source) <- reverse (zip [0 :: Int ..] applyArgumentRegisters),
          sourceIndex < runtimeEnterSuppliedCount apply,
          let targetIndex = runtimeEnterStoredCount apply + sourceIndex
        ]
    moveSuppliedOverflow apply =
      concat
        [ [ "  mov r11, QWORD PTR [rsp + " <> tshow ((sourceIndex - length applyArgumentRegisters) * 8) <> "]",
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
          ["  mov " <> applyArgumentRegisters !! targetIndex <> ", " <> source]
      | otherwise = [storeAt source "r14" targetIndex]

enterEntryLabel :: RuntimeInfo -> Text
enterEntryLabel info = runtimeInfoLabel info <> "_enter"

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

restoreApplyStackLines :: Int -> [Text]
restoreApplyStackLines stackBytes
  | stackBytes == 0 = []
  | otherwise = ["  add rsp, " <> tshow stackBytes]

renderRuntimeInfos :: [RuntimeInfo] -> [Text]
renderRuntimeInfos infos =
  [".section .rodata"] <> concatMap renderInfo infos
  where
    renderInfo info =
      bitmapLines
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
    entryLine nodeInfo =
      case nodeInfo of
        InfoImmediate {} -> "  .quad 0"
        InfoAddress label -> "  .quad " <> label

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
    InHeapSpill slot -> [loadAt destination "r14" slot]

storeLocation :: Text -> Location Text -> [Text]
storeLocation source location =
  case location of
    InRegister destination
      | destination == source -> []
      | otherwise -> ["  mov " <> destination <> ", " <> source]
    InHeapSpill slot -> [storeAt source "r14" slot]

renderNativeControl :: [Text]
renderNativeControl =
  [ ".text",
    ".p2align 4",
    ".Laihc_enter:",
    "  mov r11, QWORD PTR [r12]",
    "  mov r11, QWORD PTR [r11 + 48]",
    "  test r11, r11",
    "  jz .Laihc_invalid_enter",
    "  jmp r11",
    ".Laihc_resume:",
    "  mov r11d, DWORD PTR [rax]",
    "  cmp r11d, 1",
    "  je .Laihc_resume_apply",
    "  cmp r11d, 2",
    "  je .Laihc_resume_continue",
    "  cmp r11d, 3",
    "  je .Laihc_resume_raise",
    "  jmp .Laihc_invalid_enter",
    ".Laihc_resume_continue:",
    "  mov r10, QWORD PTR [rax + 24]",
    "  mov r11, QWORD PTR [rax + 32]",
    "  mov r12, QWORD PTR [rax + 8]",
    "  mov QWORD PTR [rax], 0",
    "  mov QWORD PTR [rax + 8], 0",
    "  mov QWORD PTR [rax + 16], 0",
    "  mov QWORD PTR [rax + 24], 0",
    "  mov QWORD PTR [rax + 32], 0",
    "  test r11, r11",
    "  jz .Laihc_enter",
    "  cmp r11, 1",
    "  jne .Laihc_invalid_enter",
    "  mov rax, r10",
    "  jmp .Laihc_enter",
    ".Laihc_resume_apply:",
    "  mov r10, QWORD PTR [rax + 24]",
    "  mov r11, QWORD PTR [rax + 32]",
    "  mov r12, QWORD PTR [rax + 8]",
    "  mov r13, QWORD PTR [rax + 16]",
    "  mov QWORD PTR [rax], 0",
    "  mov QWORD PTR [rax + 8], 0",
    "  mov QWORD PTR [rax + 16], 0",
    "  mov QWORD PTR [rax + 24], 0",
    "  mov QWORD PTR [rax + 32], 0",
    "  test r11, r11",
    "  jz .Laihc_enter",
    "  cmp r11, 1",
    "  jne .Laihc_invalid_enter",
    "  mov rax, r10",
    "  jmp .Laihc_enter",
    ".Laihc_resume_raise:",
    "  mov rsi, QWORD PTR [rax + 8]",
    "  mov rdx, QWORD PTR [rax + 16]",
    "  mov QWORD PTR [rax], 0",
    "  mov QWORD PTR [rax + 8], 0",
    "  mov QWORD PTR [rax + 16], 0",
    "  mov QWORD PTR [rax + 24], 0",
    "  mov QWORD PTR [rax + 32], 0",
    "  mov rdi, r15",
    "  call aihc_raise",
    "  jmp .Laihc_resume",
    ".Laihc_eval:",
    "  mov QWORD PTR [r14], rax",
    "  mov QWORD PTR [r14 + 8], r11",
    ".Laihc_eval_loop:",
    "  mov r11, QWORD PTR [r12]",
    "  mov r10, QWORD PTR [r11 + 64]",
    "  cmp r10, 2",
    "  je .Laihc_eval_thunk",
    "  cmp r10, 4",
    "  je .Laihc_eval_indirection",
    "  cmp r10, 5",
    "  je .Laihc_eval_blackhole",
    "  mov rax, r12",
    "  mov r12, r13",
    "  jmp .Laihc_enter",
    ".Laihc_eval_thunk:",
    "  mov rsi, r12",
    "  mov rdi, r15",
    "  call aihc_begin_blackhole",
    "  mov r13, QWORD PTR [r14]",
    "  jmp .Laihc_enter",
    ".Laihc_eval_indirection:",
    "  cmp QWORD PTR [r14 + 8], 0",
    "  je .Laihc_eval_unlifted_indirection",
    "  mov r12, QWORD PTR [r12 + 8]",
    "  jmp .Laihc_eval_loop",
    ".Laihc_eval_unlifted_indirection:",
    "  mov rax, QWORD PTR [r12 + 8]",
    "  mov r12, r13",
    "  jmp .Laihc_enter",
    ".Laihc_eval_blackhole:",
    "  mov rdx, r13",
    "  mov rsi, r12",
    "  mov rdi, r15",
    "  call aihc_block_on_blackhole",
    "  jmp .Laihc_resume",
    ".Laihc_invalid_enter:",
    "  call aihc_no_match",
    "  ud2"
  ]

globalSlot :: CompileEnv -> Text -> Either Amd64Error Int
globalSlot env name =
  maybe (Left (Amd64MissingGlobal name)) Right (Map.lookup name (compileGlobalSlots env))

constructorId :: CompileEnv -> Text -> Either Amd64Error Int
constructorId env name =
  maybe (Left (Amd64MissingConstructor name)) Right (Map.lookup name (compileConstructorIds env))

lookupRuntimeInfoLabel :: CompileEnv -> RuntimeInfoKey -> Either Amd64Error Text
lookupRuntimeInfoLabel env key =
  case Map.lookup key (compileNodeInfoLabels env) of
    Just label -> Right label
    Nothing ->
      case key of
        ConstructorRuntimeInfo name _ -> Left (Amd64MissingConstructor name)
        ClosureRuntimeInfo functionName _ _ -> Left (Amd64MissingFunction functionName)
        ThunkRuntimeInfo functionName _ -> Left (Amd64MissingFunction functionName)

functionCodeLabel :: CompileEnv -> FunctionName -> Either Amd64Error Text
functionCodeLabel env name =
  maybe (Left (Amd64MissingFunction name)) Right (Map.lookup name (compileFunctionLabels env))

constructorStageLabel :: Int -> Int -> Text
constructorStageLabel identifier remaining =
  ".Laihc_constructor_info_" <> tshow identifier <> "_remaining_" <> tshow remaining

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
    literals ->
      [".section .rodata"]
        <> concatMap renderLiteral literals
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
  | exposeAllFunctions = "aihc_snapshot_function_" <> tshow index
  | otherwise = functionLabel index

storeGlobal :: Int -> [Text]
storeGlobal slot =
  [ loadByteOffset "r11" "r15" 0,
    storeAt "rax" "r11" slot
  ]

loadAt :: Text -> Text -> Int -> Text
loadAt destination base slot = loadByteOffset destination base (slot * 8)

storeAt :: Text -> Text -> Int -> Text
storeAt source base slot = storeByteOffset source base (slot * 8)

loadByteOffset :: Text -> Text -> Int -> Text
loadByteOffset destination base offset =
  "  mov " <> destination <> ", QWORD PTR [" <> base <> offsetText offset <> "]"

storeByteOffset :: Text -> Text -> Int -> Text
storeByteOffset source base offset =
  "  mov QWORD PTR [" <> base <> offsetText offset <> "], " <> source

offsetText :: Int -> Text
offsetText offset
  | offset == 0 = ""
  | offset > 0 = " + " <> tshow offset
  | otherwise = " - " <> tshow (abs offset)

immediate :: (Show value) => Text -> value -> Text
immediate register value = "  mov " <> register <> ", " <> T.pack (show value)

address :: Text -> Text -> Text
address register label =
  "  lea " <> register <> ", [rip + " <> label <> "]"

tshow :: (Show value) => value -> Text
tshow = T.pack . show
