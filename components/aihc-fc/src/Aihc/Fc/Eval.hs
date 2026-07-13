{-# LANGUAGE ForeignFunctionInterface #-}
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

import Aihc.Fc.Syntax
import Control.Exception (SomeException, displayException, try)
import Control.Monad ((<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (FunPtr)
import System.Posix.DynamicLinker (DL (Default), dlsym)

foreign import ccall unsafe "dynamic"
  callCIntToCInt :: FunPtr (CInt -> CInt) -> CInt -> CInt

data EvalError
  = EvalUnboundVariable Text
  | EvalMissingBinding Text
  | EvalApplyNonFunction Value
  | EvalNoMatchingAlternative Value
  | EvalPrimitiveArity Text Int
  | EvalPrimitiveTypeError Text Value
  | EvalForeignTypeError Text Value
  | EvalForeignLookupError Text Text
  | EvalInvalidDictSelect Value Int
  | EvalRaisedException Value
  deriving (Eq, Show)

data Value
  = VLit Literal
  | VClosure Env Var FcExpr
  | VConstructor Text [Value]
  | VDict [Value]
  | VPrim Text Int [Value]
  | VForeign FcForeignCall [Value]
  | VThunk Env FcExpr
  deriving (Eq, Show)

type Env = Map Text Value

type EvalM = ExceptT EvalError IO

evalProgramBinding :: Text -> FcProgram -> IO (Either EvalError Value)
evalProgramBinding name program = runExceptT $
  case Map.lookup name env of
    Just value -> forceValue value
    Nothing -> throwE (EvalMissingBinding name)
  where
    env = foreignTopEnv `Map.union` primitiveTopEnv `Map.union` topEnv `Map.union` builtinConstructorEnv
    foreignTopEnv = Map.fromList (concatMap foreignTopBindingValues (fcTopBinds program))
    primitiveTopEnv = Map.fromList (concatMap primitiveTopBindingValues (fcTopBinds program))
    topEnv = Map.fromList (concatMap topBindingValues (fcTopBinds program))
    foreignTopBindingValues (FcForeignImport foreignCall) =
      [(varName (fcForeignCallVar foreignCall), VForeign foreignCall [])]
    foreignTopBindingValues _ =
      []
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
    topBindingValues (FcNewtype _ _ conName _) =
      [(conName, VConstructor conName [])]
    topBindingValues FcPrimitive {} =
      []
    topBindingValues FcForeignImport {} =
      []

evalExpr :: FcExpr -> IO (Either EvalError Value)
evalExpr = runExceptT . evalWithEnv builtinConstructorEnv

builtinConstructorEnv :: Env
builtinConstructorEnv =
  Map.fromList
    [ ("[]", VConstructor "[]" []),
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
    FcDictApp fun arg -> do
      funValue <- evalWithEnv env fun
      applyValue funValue (VThunk env arg)
    FcTyApp inner _ ->
      evalWithEnv env inner
    FcLam var body ->
      pure (VClosure env var body)
    FcTyLam _ body ->
      evalWithEnv env body
    FcDictLam var body ->
      pure (VClosure env var body)
    FcDict fields ->
      pure (VDict (map (VThunk env) fields))
    FcDictSelect dict index -> do
      dictValue <- evalWithEnv env dict >>= forceValue
      case dictValue of
        VDict fields
          | index >= 0,
            index < length fields ->
              forceValue (fields !! index)
          | otherwise -> throwE (EvalInvalidDictSelect dictValue index)
        _ -> throwE (EvalApplyNonFunction dictValue)
    FcLet bind body ->
      evalWithEnv (extendBind env bind) body
    FcCase scrut _ alts -> do
      value <- evalWithEnv env scrut
      matchAlternative env value alts
    FcCast inner _ ->
      evalWithEnv env inner

forceValue :: Value -> EvalM Value
forceValue value =
  case value of
    VThunk env expr -> evalWithEnv env expr
    _ -> pure value

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
    VForeign foreignCall args ->
      applyForeign foreignCall (args <> [arg])
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
evalPrimitive "raise#" [exception] =
  throwE . EvalRaisedException =<< forceValue exception
evalPrimitive "catch#" [action, handler, state] =
  applyValue action state `catchE` handleRaised
  where
    handleRaised (EvalRaisedException exception) = do
      handlerWithException <- applyValue handler exception
      applyValue handlerWithException state
    handleRaised err = throwE err
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
  pure (VLit (LitInt (op leftInt rightInt)))

forceIntPrimitiveArg :: Text -> Value -> EvalM Integer
forceIntPrimitiveArg name value = do
  forced <- forceValue value
  case forced of
    VLit (LitInt intValue) -> pure intValue
    other -> throwE (EvalPrimitiveTypeError name other)

applyForeign :: FcForeignCall -> [Value] -> EvalM Value
applyForeign foreignCall args =
  case (fcForeignCallAbi foreignCall, args) of
    (FcCIntToCInt, []) -> pure (VForeign foreignCall [])
    (FcCIntToCInt, [argument]) -> evalCIntToCInt foreignCall argument
    _ -> throwE (EvalPrimitiveArity (varName (fcForeignCallVar foreignCall)) (length args))

evalCIntToCInt :: FcForeignCall -> Value -> EvalM Value
evalCIntToCInt foreignCall argument = do
  argumentValue <- forceCInt (fcForeignCallSymbol foreignCall) argument
  lookupResult <-
    lift
      ( try (dlsym Default (T.unpack (fcForeignCallSymbol foreignCall))) ::
          IO (Either SomeException (FunPtr (CInt -> CInt)))
      )
  functionPointer <-
    case lookupResult of
      Left err ->
        throwE
          ( EvalForeignLookupError
              (fcForeignCallSymbol foreignCall)
              (T.pack (displayException err))
          )
      Right pointer -> pure pointer
  let CInt result = callCIntToCInt functionPointer (CInt (fromInteger argumentValue))
  pure (cIntValue (toInteger result))

forceCInt :: Text -> Value -> EvalM Integer
forceCInt symbol value = do
  forced <- forceValue value
  case forced of
    VConstructor "CInt" [int32] -> forceInt32 int32
    other -> throwE (EvalForeignTypeError symbol other)
  where
    forceInt32 int32 = do
      forcedInt32 <- forceValue int32
      case forcedInt32 of
        VConstructor "I32#" [literal] -> do
          forcedLiteral <- forceValue literal
          case forcedLiteral of
            VLit (LitInt intValue) -> pure intValue
            other -> throwE (EvalForeignTypeError symbol other)
        other -> throwE (EvalForeignTypeError symbol other)

cIntValue :: Integer -> Value
cIntValue value =
  VConstructor "CInt" [VConstructor "I32#" [VLit (LitInt value)]]

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
    VDict {} -> pure "<dictionary>"
    VClosure {} -> pure "<function>"
    VPrim {} -> pure "<function>"
    VForeign {} -> pure "<function>"
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
      pure $
        case forced of
          VLit (LitChar c) -> Just c
          _ -> Nothing

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
    LitInt i -> T.pack (show i)
    LitChar c -> T.pack (show c)
    LitString s -> T.pack (show (T.unpack s))

renderRawValue :: Value -> IO (Either EvalError Text)
renderRawValue = runExceptT . renderRawValueM

renderRawValueM :: Value -> EvalM Text
renderRawValueM value = do
  forced <- forceValue value
  case forced of
    VLit lit -> pure (renderLiteral lit)
    VConstructor name [] -> pure name
    VConstructor name args | isTupleConstructor name (length args) -> do
      renderedArgs <- mapM renderRawArg args
      pure ("(" <> T.intercalate "," renderedArgs <> ")")
    VConstructor name args -> do
      renderedArgs <- mapM renderRawArg args
      pure (T.unwords (name : renderedArgs))
    VDict {} -> pure "<dictionary>"
    VClosure {} -> pure "<function>"
    VPrim {} -> pure "<function>"
    VForeign {} -> pure "<function>"
    VThunk {} -> renderRawValueM forced

renderRawArg :: Value -> EvalM Text
renderRawArg value = do
  forced <- forceValue value
  rendered <- renderRawValueM forced
  pure $
    case forced of
      VConstructor name args | isTupleConstructor name (length args) -> rendered
      VConstructor _ (_ : _) -> "(" <> rendered <> ")"
      _ -> rendered

isTupleConstructor :: Text -> Int -> Bool
isTupleConstructor name arity =
  arity >= 2
    && ( name == "(" <> T.replicate (arity - 1) "," <> ")"
           || name == "(#" <> T.replicate (arity - 1) "," <> "#)"
       )
