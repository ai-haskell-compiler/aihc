{-# LANGUAGE OverloadedStrings #-}

-- | Materialize GRIN values and heap nodes as AArch64 instruction sequences.
module Aihc.Arm64.Codegen.Value
  ( allocateNode,
    allocateNodeUnchecked,
    initializeNodeFields,
    materializeNode,
    materializeNodeUnchecked,
    materializeValue,
    materializeValueTo,
    normalizedLiteralInteger,
  )
where

import Aihc.Arm64.Codegen.Runtime
import Aihc.Arm64.Codegen.Types
import Aihc.Grin.Syntax
import Aihc.Tc.Types (RuntimeRep (..))
import Control.Monad (forM)
import Data.Char (ord)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

materializeValue :: ValueEnv -> GrinValue -> Either Arm64Error [Text]
materializeValue env = materializeValueTo env "x0"

materializeValueTo :: ValueEnv -> Text -> GrinValue -> Either Arm64Error [Text]
materializeValueTo env destination value =
  case value of
    GrinVarValue var ->
      case Map.lookup var (valueLocations env) of
        Just location -> Right (loadLocation destination location)
        Nothing -> do
          slot <- globalSlot (valueCompileEnv env) (grinVarName var)
          pure ["  ldr x9, [x22, #0]", loadAt destination "x9" slot]
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
normalizedLiteralInteger literal = do
  integer <- literalInteger literal
  pure $
    case literal of
      GrinLitInt runtimeRep _ -> normalizeScalar runtimeRep integer
      GrinLitChar {} -> normalizeUnsigned 64 integer
      GrinLitString {} -> integer
      GrinLitAddr {} -> integer

normalizeScalar :: RuntimeRep -> Integer -> Integer
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

literalInteger :: GrinLiteral -> Maybe Integer
literalInteger literal =
  case literal of
    GrinLitInt _ integer -> Just integer
    GrinLitChar _ character -> Just (fromIntegral (ord character))
    GrinLitString _ -> Nothing
    GrinLitAddr _ -> Nothing

materializeNode :: ValueEnv -> GrinNode -> Either Arm64Error [Text]
materializeNode env node = do
  allocationLines <- allocateNode env node
  fieldLines <- initializeNodeFields env node
  pure $
    allocationLines
      <> ["  mov x20, x0"]
      <> fieldLines
      <> ["  mov x0, x20"]

materializeNodeUnchecked :: ValueEnv -> GrinNode -> Either Arm64Error [Text]
materializeNodeUnchecked env node = do
  allocationLines <- allocateNodeUnchecked env node
  fieldLines <- initializeNodeFields env node
  pure $
    allocationLines
      <> ["  mov x20, x0"]
      <> fieldLines
      <> ["  mov x0, x20"]

allocateNode :: ValueEnv -> GrinNode -> Either Arm64Error [Text]
allocateNode env node = do
  (tag, info) <- nodeHeader env node
  pure (makeNodeLines tag info)

allocateNodeUnchecked :: ValueEnv -> GrinNode -> Either Arm64Error [Text]
allocateNodeUnchecked env node = do
  (tag, info) <- nodeHeader env node
  pure (makeNodeUncheckedLines tag info)

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

nodeHeader :: ValueEnv -> GrinNode -> Either Arm64Error (Int, NodeInfo)
nodeHeader env node =
  case grinNodeTag node of
    GrinConstructor name remaining -> do
      label <- lookupRuntimeInfoLabel compileEnv (ConstructorRuntimeInfo name remaining)
      pure
        ( if remaining == 0 then runtimeTagNode else runtimeTagPartialConstructor,
          InfoAddress label
        )
    GrinClosure functionName argumentLayouts -> do
      label <- lookupRuntimeInfoLabel compileEnv (ClosureRuntimeInfo functionName fields argumentLayouts)
      pure (runtimeTagClosure, InfoAddress label)
    GrinThunk functionName -> do
      label <- lookupRuntimeInfoLabel compileEnv (ThunkRuntimeInfo functionName fields)
      pure (runtimeTagThunk, InfoAddress label)
  where
    compileEnv = valueCompileEnv env
    fields = map grinValueRuntimeRep (grinNodeFields node)
