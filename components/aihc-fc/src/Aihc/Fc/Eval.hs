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
import Control.Monad ((<=<))
import Data.Char qualified as Char
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

data EvalError
  = EvalUnboundVariable Text
  | EvalMissingBinding Text
  | EvalApplyNonFunction Value
  | EvalNoMatchingAlternative Value
  | EvalPrimitiveArity Text Int
  | EvalPrimitiveTypeError Text Value
  | EvalInvalidDictSelect Value Int
  | EvalRaisedException Value
  deriving (Eq, Show)

data Value
  = VLit Literal
  | VClosure Env Var FcExpr
  | VConstructor Text [Value]
  | VDict [Value]
  | VPrim Text Int [Value]
  | VThunk Env FcExpr
  deriving (Eq, Show)

type Env = Map Text Value

evalProgramBinding :: Text -> FcProgram -> Either EvalError Value
evalProgramBinding name program =
  case Map.lookup name env of
    Just value -> forceValue value
    Nothing -> Left (EvalMissingBinding name)
  where
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
    topBindingValues FcPrimitive {} =
      []

evalExpr :: FcExpr -> Either EvalError Value
evalExpr = evalWithEnv builtinConstructorEnv

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

evalWithEnv :: Env -> FcExpr -> Either EvalError Value
evalWithEnv env expr =
  case expr of
    FcVar var ->
      case Map.lookup (varName var) env of
        Just value -> forceValue value
        Nothing -> Left (EvalUnboundVariable (varName var))
    FcLit lit ->
      Right (VLit lit)
    FcApp fun arg -> do
      funValue <- evalWithEnv env fun
      applyValue funValue (VThunk env arg)
    FcDictApp fun arg -> do
      funValue <- evalWithEnv env fun
      applyValue funValue (VThunk env arg)
    FcTyApp inner _ ->
      evalWithEnv env inner
    FcLam var body ->
      Right (VClosure env var body)
    FcTyLam _ body ->
      evalWithEnv env body
    FcDictLam var body ->
      Right (VClosure env var body)
    FcDict fields ->
      VDict <$> mapM (pure . VThunk env) fields
    FcDictSelect dict index -> do
      dictValue <- evalWithEnv env dict >>= forceValue
      case dictValue of
        VDict fields
          | index >= 0,
            index < length fields ->
              forceValue (fields !! index)
          | otherwise -> Left (EvalInvalidDictSelect dictValue index)
        _ -> Left (EvalApplyNonFunction dictValue)
    FcLet bind body ->
      evalWithEnv (extendBind env bind) body
    FcCase scrut _ alts -> do
      value <- evalWithEnv env scrut
      matchAlternative env value alts
    FcCast inner _ ->
      evalWithEnv env inner

forceValue :: Value -> Either EvalError Value
forceValue value =
  case value of
    VThunk env expr -> evalWithEnv env expr
    _ -> Right value

applyValue :: Value -> Value -> Either EvalError Value
applyValue value arg = do
  forced <- forceValue value
  case forced of
    VClosure closureEnv var body ->
      evalWithEnv (Map.insert (varName var) arg closureEnv) body
    VConstructor name args ->
      Right (VConstructor name (args <> [arg]))
    VPrim name arity args ->
      applyPrimitive name arity (args <> [arg])
    _ ->
      Left (EvalApplyNonFunction forced)

applyPrimitive :: Text -> Int -> [Value] -> Either EvalError Value
applyPrimitive name arity args
  | length args < arity = Right (VPrim name arity args)
  | length args == arity = evalPrimitive name args
  | otherwise = Left (EvalPrimitiveArity name (length args))

evalPrimitive :: Text -> [Value] -> Either EvalError Value
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
  pure (VLit (LitInt (fromIntegral (Char.ord charValue))))
evalPrimitive "intToChar#" [value] = do
  intValue <- forceIntPrimitiveArg "intToChar#" value
  if intValue >= 0 && intValue <= 0x10ffff
    then pure (VLit (LitChar (Char.chr (fromIntegral intValue))))
    else Left (EvalPrimitiveTypeError "intToChar#" (VLit (LitInt intValue)))
evalPrimitive "raise#" [exception] =
  Left . EvalRaisedException =<< forceValue exception
evalPrimitive "catch#" [action, handler, state] =
  case applyValue action state of
    Left (EvalRaisedException exception) -> do
      handlerWithException <- applyValue handler exception
      applyValue handlerWithException state
    result -> result
evalPrimitive name args =
  Left (EvalPrimitiveArity name (length args))

compareInts :: Integer -> Integer -> Integer
compareInts left right =
  case compare left right of
    LT -> -1
    EQ -> 0
    GT -> 1

evalIntPrim :: Text -> (Integer -> Integer -> Integer) -> Value -> Value -> Either EvalError Value
evalIntPrim name op left right = do
  leftInt <- forceIntPrimitiveArg name left
  rightInt <- forceIntPrimitiveArg name right
  pure (VLit (LitInt (op leftInt rightInt)))

forceIntPrimitiveArg :: Text -> Value -> Either EvalError Integer
forceIntPrimitiveArg name value = do
  forced <- forceValue value
  case forced of
    VLit (LitInt intValue) -> pure intValue
    other -> Left (EvalPrimitiveTypeError name other)

forceCharPrimitiveArg :: Text -> Value -> Either EvalError Char
forceCharPrimitiveArg name value = do
  forced <- forceValue value
  case forced of
    VLit (LitChar charValue) -> pure charValue
    other -> Left (EvalPrimitiveTypeError name other)

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

matchAlternative :: Env -> Value -> [FcAlt] -> Either EvalError Value
matchAlternative env value =
  go
  where
    go [] = Left (EvalNoMatchingAlternative value)
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

renderValue :: Value -> Either EvalError Text
renderValue value = do
  forced <- forceValue value
  case collectString forced of
    Just text -> pure (T.pack (show (T.unpack text)))
    Nothing -> renderForcedValue forced

renderForcedValue :: Value -> Either EvalError Text
renderForcedValue value =
  case value of
    VLit lit -> pure (renderLiteral lit)
    VConstructor "C#" [char] -> renderBoxedChar char
    VConstructor name [] -> pure name
    VConstructor ":" _ | Just elems <- collectList value -> do
      renderedElems <- mapM (renderValue <=< forceValue) elems
      pure ("[" <> T.intercalate ", " renderedElems <> "]")
    VConstructor name args -> do
      renderedArgs <- mapM (renderValue <=< forceValue) args
      pure (T.unwords (name : renderedArgs))
    VDict {} -> pure "<dictionary>"
    VClosure {} -> pure "<function>"
    VPrim {} -> pure "<function>"
    VThunk {} -> renderValue value

collectString :: Value -> Maybe Text
collectString value = do
  values <- collectList value
  chars <- traverse charValue values
  pure (T.pack chars)
  where
    charValue item =
      case item of
        VConstructor "C#" [char] -> unboxedCharValue char
        VThunk {} -> either (const Nothing) charValue (forceValue item)
        _ -> Nothing

    unboxedCharValue item =
      case item of
        VLit (LitChar c) -> Just c
        VThunk {} -> either (const Nothing) unboxedCharValue (forceValue item)
        _ -> Nothing

collectList :: Value -> Maybe [Value]
collectList value =
  case value of
    VConstructor "[]" [] -> Just []
    VConstructor ":" [headValue, tailValue] -> do
      tailValue' <- either (const Nothing) Just (forceValue tailValue)
      (headValue :) <$> collectList tailValue'
    VThunk {} -> either (const Nothing) collectList (forceValue value)
    _ -> Nothing

renderLiteral :: Literal -> Text
renderLiteral lit =
  case lit of
    LitInt i -> T.pack (show i)
    LitChar c -> T.pack (show c) <> "#"
    LitString s -> T.pack (show (T.unpack s))

renderBoxedChar :: Value -> Either EvalError Text
renderBoxedChar value = do
  forced <- forceValue value
  case forced of
    VLit (LitChar c) -> pure (T.pack (show c))
    other -> Left (EvalPrimitiveTypeError "C#" other)

renderRawValue :: Value -> Either EvalError Text
renderRawValue value = do
  forced <- forceValue value
  case forced of
    VLit lit -> pure (renderLiteral lit)
    VConstructor "C#" [char] -> renderBoxedChar char
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
    VThunk {} -> renderRawValue forced

renderRawArg :: Value -> Either EvalError Text
renderRawArg value = do
  forced <- forceValue value
  rendered <- renderRawValue forced
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
