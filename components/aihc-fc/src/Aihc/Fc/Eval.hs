{-# LANGUAGE OverloadedStrings #-}

-- | Small evaluator for the System FC subset used by the REPL MVP.
module Aihc.Fc.Eval
  ( EvalError (..),
    Value (..),
    evalProgramBinding,
    evalExpr,
    renderValue,
    renderRawValue,
  )
where

import Aihc.Fc.Newtype (lowerNewtypes)
import Aihc.Fc.Optimize (optimizeProgram)
import Aihc.Fc.Syntax
import Aihc.Tc.Types (RuntimeRep (..), TcType (..), TyCon (..))
import Control.Exception (SomeException, displayException, try)
import Control.Monad (zipWithM, (<=<), (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Foreign.C.Types (CInt (..))
import Foreign.LibFFI (Arg, argCInt, argPtr, argWord64, callFFI, retCInt, retPtr, retVoid, retWord64)
import Foreign.Marshal.Array (newArray0)
import Foreign.Ptr (FunPtr, Ptr)
import System.IO (Handle, hFlush, stdin, stdout)
import System.Posix.DynamicLinker (DL (Default), dlsym)

data EvalError
  = EvalUnboundVariable Text
  | EvalMissingBinding Text
  | EvalApplyNonFunction Value
  | EvalNoMatchingAlternative Value
  | EvalPrimitiveArity Text Int
  | EvalPrimitiveTypeError Text Value
  | EvalForeignArity Text Int Int
  | EvalForeignTypeError Text Value
  | EvalForeignLookupError Text Text
  | EvalInvalidIOResult Value
  | EvalInvalidIOBufferRange Text Integer Integer Int
  | EvalRaisedException Value
  deriving (Eq, Show)

data Value
  = VLit Literal
  | VAddress !(Ptr ())
  | VClosure Env Var FcExpr
  | VConstructor Text [Value]
  | VPrim Text Int [Value]
  | VIOHandle EvalIOHandle
  | VIOBuffer EvalIOBuffer
  | VIORequest EvalIORequest
  | VMutVar EvalMutVar
  | VStateToken
  | VThunk Env FcExpr
  deriving (Eq, Show)

newtype EvalMutVar = EvalMutVar (IORef Value)

instance Eq EvalMutVar where
  EvalMutVar left == EvalMutVar right = left == right

instance Show EvalMutVar where
  show _ = "<mutvar>"

data EvalIOHandle = EvalIOHandle !Int !Handle

instance Eq EvalIOHandle where
  EvalIOHandle left _ == EvalIOHandle right _ = left == right

instance Show EvalIOHandle where
  show _ = "<io-handle>"

newtype EvalIOBuffer = EvalIOBuffer (IORef BS.ByteString)

instance Eq EvalIOBuffer where
  EvalIOBuffer left == EvalIOBuffer right = left == right

instance Show EvalIOBuffer where
  show _ = "<io-buffer>"

data EvalIOOperation
  = EvalRead !EvalIOHandle !EvalIOBuffer !Int !Int
  | EvalWrite !EvalIOHandle !EvalIOBuffer !Int !Int
  deriving (Eq, Show)

data EvalIOState
  = EvalIOSubmitted !EvalIOOperation
  | EvalIOCompleted !Integer
  | EvalIOConsumed
  deriving (Eq, Show)

newtype EvalIORequest = EvalIORequest (IORef EvalIOState)

instance Eq EvalIORequest where
  EvalIORequest left == EvalIORequest right = left == right

instance Show EvalIORequest where
  show _ = "<io-request>"

type Env = Map Text Value

type EvalM = ExceptT EvalError IO

evalProgramBinding :: Text -> FcProgram -> IO (Either EvalError Value)
evalProgramBinding name sourceProgram = runExceptT $
  case Map.lookup name env of
    Just value -> do
      forced <- forceValue value
      if name `Map.member` ioBindings
        then runIOValue forced
        else pure forced
    Nothing -> throwE (EvalMissingBinding name)
  where
    program = optimizeProgram (lowerNewtypes sourceProgram)
    ioBindings =
      Map.fromList
        [ (varName var, ())
        | FcTopBind bind <- fcTopBinds program,
          var <- bindersOf bind,
          isIOType (varType var)
        ]
    env = primitiveTopEnv `Map.union` topEnv `Map.union` builtinConstructorEnv
    primitiveTopEnv = Map.fromList (concatMap primitiveTopBindingValues (fcTopBinds program))
    topEnv = Map.fromList (concatMap topBindingValues (fcTopBinds program))
    primitiveTopBindingValues (FcPrimitive var arity) =
      [(varName var, VPrim (varName var) arity [])]
    primitiveTopBindingValues _ =
      []
    topBindingValues (FcTopBind (FcNonRec var expr)) =
      [(varName var, VThunk env expr)]
    topBindingValues (FcTopBind (FcRec bindings)) =
      [(varName var, VThunk env expr) | (var, expr) <- bindings]
    topBindingValues (FcData _ _ constructors) =
      [(conName, VConstructor conName []) | (conName, _) <- constructors]
    topBindingValues FcNewtype {} =
      []
    topBindingValues FcPrimitive {} =
      []
    topBindingValues FcForeignImport {} =
      []

evalExpr :: FcExpr -> IO (Either EvalError Value)
evalExpr = runExceptT . evalWithEnv builtinConstructorEnv

builtinConstructorEnv :: Env
builtinConstructorEnv =
  Map.fromList
    [ ("C#", VConstructor "C#" []),
      ("[]", VConstructor "[]" []),
      (":", VConstructor ":" []),
      ("()", VConstructor "()" []),
      ("(,)", VConstructor "(,)" []),
      ("(#,#)", VConstructor "(#,#)" [])
    ]

evalWithEnv :: Env -> FcExpr -> EvalM Value
evalWithEnv env expr =
  case expr of
    FcVar var ->
      case Map.lookup (varName var) env of
        Just value -> forceValue value
        Nothing -> throwE (EvalUnboundVariable (varName var))
    FcLit lit ->
      pure (VLit lit)
    FcApp fun arg -> do
      funValue <- evalWithEnv env fun
      applyValue funValue (VThunk env arg)
    FcTyApp inner _ ->
      evalWithEnv env inner
    FcLam var body ->
      pure (VClosure env var body)
    FcTyLam _ body ->
      evalWithEnv env body
    FcLet bind body ->
      evalWithEnv (extendBind env bind) body
    FcCase scrut _ alts -> do
      value <- evalWithEnv env scrut
      matchAlternative env value alts
    FcCast inner _ ->
      evalWithEnv env inner
    FcCallForeign foreignCall arguments -> do
      values <- mapM (evalWithEnv env >=> forceValue) arguments
      executeForeignCall foreignCall values

forceValue :: Value -> EvalM Value
forceValue value =
  case value of
    VThunk env expr -> evalWithEnv env expr
    VPrim name 0 [] -> evalPrimitive name []
    _ -> pure value

runIOValue :: Value -> EvalM Value
runIOValue action = do
  result <- applyValue action VStateToken >>= forceValue
  case result of
    VConstructor "(#,#)" [_state, ioResult] -> forceValue ioResult
    other -> throwE (EvalInvalidIOResult other)

applyValue :: Value -> Value -> EvalM Value
applyValue value arg = do
  forced <- forceValue value
  case forced of
    VClosure closureEnv var body ->
      evalWithEnv (Map.insert (varName var) arg closureEnv) body
    VConstructor name args ->
      pure (VConstructor name (args <> [arg]))
    VPrim name arity args ->
      applyPrimitive name arity (args <> [arg])
    _ ->
      throwE (EvalApplyNonFunction forced)

applyPrimitive :: Text -> Int -> [Value] -> EvalM Value
applyPrimitive name arity args
  | length args < arity = pure (VPrim name arity args)
  | length args == arity = evalPrimitive name args
  | otherwise = throwE (EvalPrimitiveArity name (length args))

evalPrimitive :: Text -> [Value] -> EvalM Value
evalPrimitive "+#" [left, right] =
  evalIntPrim "+#" (+) left right
evalPrimitive "-#" [left, right] =
  evalIntPrim "-#" (-) left right
evalPrimitive "*#" [left, right] =
  evalIntPrim "*#" (*) left right
evalPrimitive "compareInt#" [left, right] =
  evalIntPrim "compareInt#" compareInts left right
evalPrimitive "<#" [left, right] =
  evalIntPrim "<#" (\leftInt rightInt -> if leftInt < rightInt then 1 else 0) left right
evalPrimitive "==#" [left, right] =
  evalIntPrim "==#" (\leftInt rightInt -> if leftInt == rightInt then 1 else 0) left right
evalPrimitive "charToInt#" [value] = do
  charValue <- forceCharPrimitiveArg "charToInt#" value
  pure (VLit (LitInt IntRep (fromIntegral (Char.ord charValue))))
evalPrimitive "intToChar#" [value] = do
  intValue <- forceIntPrimitiveArg "intToChar#" value
  if intValue >= 0 && intValue <= 0x10ffff
    then pure (VLit (LitChar WordRep (Char.chr (fromIntegral intValue))))
    else throwE (EvalPrimitiveTypeError "intToChar#" (VLit (LitInt IntRep intValue)))
evalPrimitive "raise#" [exception] =
  throwE . EvalRaisedException =<< forceValue exception
evalPrimitive "realWorld#" [] =
  pure VStateToken
evalPrimitive "catch#" [action, handler, state] =
  applyValue action state `catchE` handleRaised
  where
    handleRaised (EvalRaisedException exception) = do
      handlerWithException <- applyValue handler exception
      applyValue handlerWithException state
    handleRaised err = throwE err
evalPrimitive "newMutVar#" [initialValue, state] = do
  mutVar <- lift (newIORef initialValue)
  pure (VConstructor "(#,#)" [state, VMutVar (EvalMutVar mutVar)])
evalPrimitive "readMutVar#" [mutVar, state] = do
  EvalMutVar ref <- forceMutVarPrimitiveArg "readMutVar#" mutVar
  value <- lift (readIORef ref)
  pure (VConstructor "(#,#)" [state, value])
evalPrimitive "writeMutVar#" [mutVar, value, state] = do
  EvalMutVar ref <- forceMutVarPrimitiveArg "writeMutVar#" mutVar
  lift (writeIORef ref value)
  pure state
evalPrimitive "awaitIO#" [request, state] = do
  ioRequest <- forceIORequestPrimitiveArg "awaitIO#" request
  completeIORequest ioRequest
  pure state
evalPrimitive name args =
  throwE (EvalPrimitiveArity name (length args))

compareInts :: Integer -> Integer -> Integer
compareInts left right =
  case compare left right of
    LT -> -1
    EQ -> 0
    GT -> 1

evalIntPrim :: Text -> (Integer -> Integer -> Integer) -> Value -> Value -> EvalM Value
evalIntPrim name op left right = do
  leftInt <- forceIntPrimitiveArg name left
  rightInt <- forceIntPrimitiveArg name right
  pure (VLit (LitInt IntRep (op leftInt rightInt)))

forceIntPrimitiveArg :: Text -> Value -> EvalM Integer
forceIntPrimitiveArg name value = do
  forced <- forceValue value
  case forced of
    VLit (LitInt _ intValue) -> pure intValue
    other -> throwE (EvalPrimitiveTypeError name other)

forceMutVarPrimitiveArg :: Text -> Value -> EvalM EvalMutVar
forceMutVarPrimitiveArg name value = do
  forced <- forceValue value
  case forced of
    VMutVar mutVar -> pure mutVar
    other -> throwE (EvalPrimitiveTypeError name other)

forceIORequestPrimitiveArg :: Text -> Value -> EvalM EvalIORequest
forceIORequestPrimitiveArg name value = do
  forced <- forceValue value
  case forced of
    VIORequest request -> pure request
    other -> throwE (EvalPrimitiveTypeError name other)

completeIORequest :: EvalIORequest -> EvalM ()
completeIORequest (EvalIORequest reference) = do
  state <- lift (readIORef reference)
  case state of
    EvalIOSubmitted operation -> do
      result <- performIOOperation operation
      lift (writeIORef reference (EvalIOCompleted result))
    EvalIOCompleted {} -> pure ()
    EvalIOConsumed -> throwE (EvalPrimitiveTypeError "awaitIO#" (VIORequest (EvalIORequest reference)))

performIOOperation :: EvalIOOperation -> EvalM Integer
performIOOperation operation =
  case operation of
    EvalRead (EvalIOHandle _ handle) (EvalIOBuffer reference) offset byteCount -> do
      input <- lift (BS.hGet handle byteCount)
      lift $ modifyIORef' reference $ \buffer -> BS.take offset buffer <> input <> BS.drop (offset + BS.length input) buffer
      pure (toInteger (BS.length input))
    EvalWrite (EvalIOHandle _ handle) (EvalIOBuffer reference) offset byteCount -> do
      buffer <- lift (readIORef reference)
      lift (BS.hPut handle (BS.take byteCount (BS.drop offset buffer)) >> hFlush handle)
      pure (toInteger byteCount)

executeForeignCall :: FcForeignCall -> [Value] -> EvalM Value
executeForeignCall foreignCall arguments
  | actualArity /= expectedArity = throwE (EvalForeignArity name expectedArity actualArity)
  | otherwise =
      case fcForeignEffect signature of
        FcForeignPure -> callForeign foreignCall arguments
        FcForeignRealWorld ->
          case reverse arguments of
            state : reversedAbiArguments -> do
              result <- callForeign foreignCall (reverse reversedAbiArguments)
              pure (VConstructor "(#,#)" [state, result])
            [] -> throwE (EvalForeignArity name expectedArity actualArity)
  where
    name = fcForeignCallName foreignCall
    signature = fcForeignCallSignature foreignCall
    actualArity = length arguments
    expectedArity = length (fcForeignOperandTypes signature)

callForeign :: FcForeignCall -> [Value] -> EvalM Value
callForeign foreignCall args
  | symbol == "aihc_io_stdin",
    [] <- args =
      pure (VIOHandle (EvalIOHandle 0 stdin))
  | symbol == "aihc_io_stdout",
    [] <- args =
      pure (VIOHandle (EvalIOHandle 1 stdout))
  | symbol == "aihc_io_buffer_new",
    [capacityValue] <- args = do
      capacity <- forceInt32 symbol capacityValue
      if capacity < 0
        then throwE (EvalInvalidIOBufferRange symbol 0 capacity 0)
        else VIOBuffer . EvalIOBuffer <$> lift (newIORef (BS.replicate (fromInteger capacity) 0))
  | symbol == "aihc_io_buffer_get",
    [bufferValue, indexValue] <- args = do
      buffer@(EvalIOBuffer reference) <- forceIOBufferForeignArg symbol bufferValue
      index <- forceInt32 symbol indexValue
      (checkedIndex, _) <- checkedIOBufferRange symbol buffer index 1
      bytes <- lift (readIORef reference)
      pure (VLit (LitInt Int32Rep (toInteger (BS.index bytes checkedIndex))))
  | symbol == "aihc_io_buffer_set",
    [bufferValue, indexValue, byteValue] <- args = do
      buffer@(EvalIOBuffer reference) <- forceIOBufferForeignArg symbol bufferValue
      index <- forceInt32 symbol indexValue
      byte <- forceInt32 symbol byteValue
      (checkedIndex, _) <- checkedIOBufferRange symbol buffer index 1
      lift $ modifyIORef' reference $ \bytes -> BS.take checkedIndex bytes <> BS.singleton (fromInteger byte) <> BS.drop (checkedIndex + 1) bytes
      pure (VLit (LitInt Int32Rep 0))
  | symbol == "aihc_io_submit_read",
    [handleValue, bufferValue, offsetValue, lengthValue] <- args = do
      handle <- forceIOHandleForeignArg symbol handleValue
      buffer <- forceIOBufferForeignArg symbol bufferValue
      offset <- forceInt32 symbol offsetValue
      byteCount <- forceInt32 symbol lengthValue
      (checkedOffset, checkedLength) <- checkedIOBufferRange symbol buffer offset byteCount
      VIORequest . EvalIORequest <$> lift (newIORef (EvalIOSubmitted (EvalRead handle buffer checkedOffset checkedLength)))
  | symbol == "aihc_io_submit_write",
    [handleValue, bufferValue, offsetValue, lengthValue] <- args = do
      handle <- forceIOHandleForeignArg symbol handleValue
      buffer <- forceIOBufferForeignArg symbol bufferValue
      offset <- forceInt32 symbol offsetValue
      byteCount <- forceInt32 symbol lengthValue
      (checkedOffset, checkedLength) <- checkedIOBufferRange symbol buffer offset byteCount
      VIORequest . EvalIORequest <$> lift (newIORef (EvalIOSubmitted (EvalWrite handle buffer checkedOffset checkedLength)))
  | symbol == "aihc_io_take_result",
    [request] <- args =
      takeIOResult symbol request
  | otherwise = do
      marshalledArgs <-
        zipWithM
          (marshalForeignArgument (fcForeignCallSymbol foreignCall))
          (fcForeignArgumentTypes (fcForeignCallSignature foreignCall))
          args
      functionPointer <- lookupForeignFunction foreignCall
      case fcForeignResultType (fcForeignCallSignature foreignCall) of
        FcForeignInt32 -> do
          CInt result <- lift (callFFI functionPointer retCInt marshalledArgs)
          pure (VLit (LitInt Int32Rep (toInteger result)))
        FcForeignWord64 -> do
          result <- lift (callFFI functionPointer retWord64 marshalledArgs)
          pure (VLit (LitInt Word64Rep (toInteger result)))
        FcForeignAddr ->
          VAddress <$> lift (callFFI functionPointer (retPtr retVoid) marshalledArgs)
  where
    symbol = fcForeignCallSymbol foreignCall

forceIOHandleForeignArg :: Text -> Value -> EvalM EvalIOHandle
forceIOHandleForeignArg symbol value = do
  forced <- forceValue value
  case forced of
    VIOHandle handle -> pure handle
    _ -> throwE (EvalForeignTypeError symbol forced)

forceIOBufferForeignArg :: Text -> Value -> EvalM EvalIOBuffer
forceIOBufferForeignArg symbol value = do
  forced <- forceValue value
  case forced of
    VIOBuffer buffer -> pure buffer
    _ -> throwE (EvalForeignTypeError symbol forced)

checkedIOBufferRange :: Text -> EvalIOBuffer -> Integer -> Integer -> EvalM (Int, Int)
checkedIOBufferRange symbol (EvalIOBuffer reference) offset byteCount = do
  capacity <- BS.length <$> lift (readIORef reference)
  if offset < 0 || byteCount < 0 || offset > toInteger capacity || byteCount > toInteger capacity - offset
    then throwE (EvalInvalidIOBufferRange symbol offset byteCount capacity)
    else pure (fromInteger offset, fromInteger byteCount)

takeIOResult :: Text -> Value -> EvalM Value
takeIOResult symbol value = do
  forced <- forceValue value
  case forced of
    VIORequest (EvalIORequest reference) -> do
      state <- lift (readIORef reference)
      case state of
        EvalIOCompleted result -> do
          lift (writeIORef reference EvalIOConsumed)
          pure (VLit (LitInt Int32Rep result))
        _ -> throwE (EvalForeignTypeError symbol forced)
    _ -> throwE (EvalForeignTypeError symbol forced)

marshalForeignArgument :: Text -> FcForeignType -> Value -> EvalM Arg
marshalForeignArgument symbol FcForeignInt32 argument = do
  argumentValue <- forceInt32 symbol argument
  pure (argCInt (CInt (fromInteger argumentValue)))
marshalForeignArgument symbol FcForeignWord64 argument = do
  argumentValue <- forceWord64 symbol argument
  pure (argWord64 (fromInteger argumentValue :: Word64))
marshalForeignArgument symbol FcForeignAddr argument = do
  forced <- forceValue argument
  case forced of
    VLit (LitAddr value) -> do
      pointer <- lift (newArray0 0 (BS.unpack value))
      pure (argPtr pointer)
    VAddress pointer -> pure (argPtr pointer)
    other -> throwE (EvalForeignTypeError symbol other)

lookupForeignFunction :: FcForeignCall -> EvalM (FunPtr ())
lookupForeignFunction foreignCall = do
  lookupResult <- lift (tryForeign (dlsym Default (T.unpack (fcForeignCallSymbol foreignCall))))
  case lookupResult of
    Left err ->
      throwE
        ( EvalForeignLookupError
            (fcForeignCallSymbol foreignCall)
            (T.pack (displayException err))
        )
    Right pointer -> pure pointer

tryForeign :: IO a -> IO (Either SomeException a)
tryForeign = try

forceInt32 :: Text -> Value -> EvalM Integer
forceInt32 symbol value = do
  forced <- forceValue value
  case forced of
    VLit (LitInt Int32Rep intValue) -> pure intValue
    other -> throwE (EvalForeignTypeError symbol other)

forceWord64 :: Text -> Value -> EvalM Integer
forceWord64 symbol value = do
  forced <- forceValue value
  case forced of
    VLit (LitInt Word64Rep intValue) -> pure intValue
    other -> throwE (EvalForeignTypeError symbol other)

bindersOf :: FcBind -> [Var]
bindersOf bind =
  case bind of
    FcNonRec var _ -> [var]
    FcRec bindings -> map fst bindings

isIOType :: TcType -> Bool
isIOType ty =
  case ty of
    TcTyCon (TyCon "IO" 1) [_] -> True
    TcForAllTy _ body -> isIOType body
    TcQualTy _ body -> isIOType body
    _ -> False

forceCharPrimitiveArg :: Text -> Value -> EvalM Char
forceCharPrimitiveArg name value = do
  forced <- forceValue value
  case forced of
    VLit (LitChar _ charValue) -> pure charValue
    other -> throwE (EvalPrimitiveTypeError name other)

extendBind :: Env -> FcBind -> Env
extendBind env bind =
  case bind of
    FcNonRec var expr ->
      Map.insert (varName var) (VThunk env expr) env
    FcRec bindings ->
      recEnv
      where
        recEnv = foldr insertBinding env bindings
        insertBinding (var, expr) = Map.insert (varName var) (VThunk recEnv expr)

matchAlternative :: Env -> Value -> [FcAlt] -> EvalM Value
matchAlternative env value =
  go
  where
    go [] = throwE (EvalNoMatchingAlternative value)
    go (alt : rest) =
      case matchAlt value alt of
        Just bindings -> evalWithEnv (bindings <> env) (altRhs alt)
        Nothing -> go rest

matchAlt :: Value -> FcAlt -> Maybe Env
matchAlt value alt =
  case (altCon alt, value) of
    (DefaultAlt, _) ->
      Just (Map.fromList [(varName var, value) | var <- altBinders alt])
    (LitAlt expected, VLit actual)
      | expected == actual ->
          Just Map.empty
    (DataAlt expected, VConstructor actual args)
      | expected == actual,
        length args == length (altBinders alt) ->
          Just (Map.fromList (zipWith (\var arg -> (varName var, arg)) (altBinders alt) args))
    _ ->
      Nothing

renderValue :: Value -> IO (Either EvalError Text)
renderValue = runExceptT . renderValueM

renderValueM :: Value -> EvalM Text
renderValueM value = do
  forced <- forceValue value
  stringValue <- collectString forced
  case stringValue of
    Just text -> pure (T.pack (show (T.unpack text)))
    Nothing -> renderForcedValue forced

renderForcedValue :: Value -> EvalM Text
renderForcedValue value =
  case value of
    VLit lit -> pure (renderLiteral lit)
    VAddress address -> pure (T.pack (show address))
    VConstructor "C#" [char] -> renderBoxedChar char
    VConstructor name [] -> pure name
    VConstructor ":" _ -> do
      listValue <- collectList value
      case listValue of
        Just elems -> do
          renderedElems <- mapM (renderValueM <=< forceValue) elems
          pure ("[" <> T.intercalate ", " renderedElems <> "]")
        Nothing -> renderConstructor value
    VConstructor name args -> do
      renderedArgs <- mapM (renderValueM <=< forceValue) args
      pure (T.unwords (name : renderedArgs))
    VClosure {} -> pure "<function>"
    VPrim {} -> pure "<function>"
    VIOHandle {} -> pure "<io-handle>"
    VIOBuffer {} -> pure "<io-buffer>"
    VIORequest {} -> pure "<io-request>"
    VMutVar {} -> pure "<mutvar>"
    VStateToken -> pure "<state>"
    VThunk {} -> renderValueM value
  where
    renderConstructor (VConstructor name args) = do
      renderedArgs <- mapM (renderValueM <=< forceValue) args
      pure (T.unwords (name : renderedArgs))
    renderConstructor other = renderForcedValue other

collectString :: Value -> EvalM (Maybe Text)
collectString value = do
  listValue <- collectList value
  case listValue of
    Nothing -> pure Nothing
    Just values -> do
      chars <- traverse charValue values
      pure (T.pack <$> sequence chars)
  where
    charValue item = do
      forced <- forceValue item
      case forced of
        VConstructor "C#" [char] -> do
          forcedChar <- forceValue char
          pure $
            case forcedChar of
              VLit (LitChar _ c) -> Just c
              _ -> Nothing
        _ -> pure Nothing

collectList :: Value -> EvalM (Maybe [Value])
collectList value = do
  forced <- forceValue value
  case forced of
    VConstructor "[]" [] -> pure (Just [])
    VConstructor ":" [headValue, tailValue] -> do
      tailValues <- collectList tailValue
      pure ((headValue :) <$> tailValues)
    _ -> pure Nothing

renderLiteral :: Literal -> Text
renderLiteral lit =
  case lit of
    LitInt _ i -> T.pack (show i)
    LitChar _ c -> T.pack (show c) <> "#"
    LitString s -> T.pack (show (T.unpack s))
    LitAddr s -> T.pack (show (map (Char.chr . fromIntegral) (BS.unpack s))) <> "#"

renderBoxedChar :: Value -> EvalM Text
renderBoxedChar value = do
  forced <- forceValue value
  case forced of
    VLit (LitChar _ c) -> pure (T.pack (show c))
    other -> throwE (EvalPrimitiveTypeError "C#" other)

renderRawValue :: Value -> IO (Either EvalError Text)
renderRawValue = runExceptT . renderRawValueM

renderRawValueM :: Value -> EvalM Text
renderRawValueM value = do
  forced <- forceValue value
  case forced of
    VLit lit -> pure (renderLiteral lit)
    VAddress address -> pure (T.pack (show address))
    VConstructor "C#" [char] -> renderBoxedChar char
    VConstructor name [] -> pure name
    VConstructor name args | isTupleConstructor name (length args) -> do
      renderedArgs <- mapM renderRawArg args
      pure ("(" <> T.intercalate "," renderedArgs <> ")")
    VConstructor name args -> do
      renderedArgs <- mapM renderRawArg args
      pure (T.unwords (name : renderedArgs))
    VClosure {} -> pure "<function>"
    VPrim {} -> pure "<function>"
    VIOHandle {} -> pure "<io-handle>"
    VIOBuffer {} -> pure "<io-buffer>"
    VIORequest {} -> pure "<io-request>"
    VMutVar {} -> pure "<mutvar>"
    VStateToken -> pure "<state>"
    VThunk {} -> renderRawValueM forced

renderRawArg :: Value -> EvalM Text
renderRawArg value = do
  forced <- forceValue value
  rendered <- renderRawValueM forced
  pure $
    case forced of
      VConstructor name args | isTupleConstructor name (length args) -> rendered
      VConstructor "C#" [_] -> rendered
      VConstructor _ (_ : _) -> "(" <> rendered <> ")"
      _ -> rendered

isTupleConstructor :: Text -> Int -> Bool
isTupleConstructor name arity =
  arity >= 2
    && ( name == "(" <> T.replicate (arity - 1) "," <> ")"
           || name == "(#" <> T.replicate (arity - 1) "," <> "#)"
       )
