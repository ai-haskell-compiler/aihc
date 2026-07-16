{-# LANGUAGE OverloadedStrings #-}

-- | Reference interpreter for strict GRIN programs.
module Aihc.Grin.Interpret
  ( InterpretError (..),
    RuntimeValue (..),
    interpretProgramBinding,
  )
where

import Aihc.Grin.Syntax
import Aihc.Tc.Types (RuntimeRep (..))
import Control.Exception (SomeException, displayException, try)
import Control.Monad (zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, gets, modify', runStateT)
import Data.Char qualified as Char
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Foreign.C.Types (CInt (..))
import Foreign.LibFFI (Arg, argCInt, argWord64, callFFI, retCInt, retWord64)
import Foreign.Ptr (FunPtr)
import System.Posix.DynamicLinker (DL (Default), dlsym)

data InterpretError
  = InterpretUnboundVariable !GrinVar
  | InterpretMissingBinding !Text
  | InterpretUnknownFunction !FunctionName
  | InterpretFunctionArity !FunctionName !Int !Int
  | InterpretApplyNonFunction !RuntimeValue
  | InterpretNoMatchingAlternative !RuntimeValue
  | InterpretPrimitiveArity !Text !Int
  | InterpretPrimitiveTypeError !Text !RuntimeValue
  | InterpretForeignArity !Text !Int !Int
  | InterpretForeignTypeError !Text !RuntimeValue
  | InterpretForeignLookupError !Text !Text
  | InterpretInvalidIOResult !RuntimeValue
  | InterpretInvalidDictSelect !RuntimeValue !Int
  | InterpretExpectedLocation !RuntimeValue
  | InterpretInvalidLocation !Int
  | InterpretBlackhole !Int
  | InterpretRaisedException !Text
  deriving (Eq, Show)

data RuntimeValue
  = RuntimeLit !GrinLiteral
  | RuntimeNode !GrinNodeTag ![RuntimeValue]
  | RuntimeLocation !Int
  | RuntimeMutVar !GrinMutVar
  | RuntimeStateToken
  deriving (Eq, Show)

newtype GrinMutVar = GrinMutVar (IORef RuntimeValue)

instance Eq GrinMutVar where
  GrinMutVar left == GrinMutVar right = left == right

instance Show GrinMutVar where
  show _ = "<mutvar>"

type Env = Map GrinVar RuntimeValue

data HeapCell
  = HeapSuspended !Env !GrinNode
  | HeapValue !RuntimeValue
  | HeapRaised !RuntimeValue
  | HeapBlackhole

data Machine = Machine
  { machineProgram :: !GrinProgram,
    machineFunctions :: !(Map FunctionName GrinFunction),
    machineGlobals :: !(Map Text RuntimeValue),
    machineHeap :: !(IntMap HeapCell),
    machineNextLocation :: !Int
  }

data EvalFailure
  = EvalInterpret !InterpretError
  | EvalRaised !RuntimeValue

type EvalM = ExceptT EvalFailure (StateT Machine IO)

-- | Interpret and render a named top-level binding using the raw constructor
-- representation shared by the compiler pipeline evaluation fixtures.
interpretProgramBinding :: Text -> GrinProgram -> IO (Either InterpretError Text)
interpretProgramBinding name program = do
  let machine = initialMachine program
  (result, finalMachine) <- runStateT (runExceptT action) machine
  case result of
    Right rendered -> pure (Right rendered)
    Left (EvalInterpret err) -> pure (Left err)
    Left (EvalRaised exception) -> do
      (renderResult, _) <- runStateT (runExceptT (renderRawValueM exception)) finalMachine
      pure $
        case renderResult of
          Right rendered -> Left (InterpretRaisedException rendered)
          Left _ -> Left (InterpretRaisedException (T.pack (show exception)))
  where
    action = do
      globals <- getsMachine machineGlobals
      value <-
        case Map.lookup name globals of
          Just binding -> pure binding
          Nothing -> throwInterpret (InterpretMissingBinding name)
      forced <- forceValue value
      result <-
        if name `Set.member` grinIoCafs program
          then runIOValue forced
          else pure forced
      renderRawValueM result

initialMachine :: GrinProgram -> Machine
initialMachine program =
  Machine
    { machineProgram = program,
      machineFunctions =
        Map.fromList
          [ (grinFunctionName function, function)
          | function <- grinFunctions program
          ],
      machineGlobals = globals,
      machineHeap =
        IntMap.fromList
          [ (location, HeapSuspended cafEnv node)
          | ((_, node), location) <- zip cafs [0 ..]
          ],
      machineNextLocation = length cafs
    }
  where
    cafs = grinCafs program
    cafLocations =
      [ (var, RuntimeLocation location)
      | ((var, _), location) <- zip cafs [0 ..]
      ]
    cafEnv = Map.fromList cafLocations
    globals =
      Map.unions
        [ Map.fromList [(grinVarName var, value) | (var, value) <- cafLocations],
          Map.fromList
            [ (grinVarName var, RuntimeNode (GrinPrimitive (grinVarName var) arity) [])
            | (var, arity) <- grinPrimitives program
            ],
          Map.fromList
            [ (constructor, RuntimeNode (GrinConstructor constructor) [])
            | constructor <- map fst builtinConstructors <> map fst (grinConstructors program)
            ]
        ]

evalExpr :: Env -> GrinExpr -> EvalM RuntimeValue
evalExpr env expr =
  case expr of
    GrinReturn value -> evalValue env value
    GrinBind var valueExpr body -> do
      value <- evalExpr env valueExpr
      evalExpr (Map.insert var value env) body
    GrinStore node ->
      allocateCell (HeapSuspended env node)
    GrinStoreRec bindings body -> do
      locations <- mapM (const (allocateLocation HeapBlackhole)) bindings
      let recursiveBindings = zip (map fst bindings) (map RuntimeLocation locations)
          recursiveEnv = Map.fromList recursiveBindings `Map.union` env
      mapM_
        (\((_, node), location) -> writeCell location (HeapSuspended recursiveEnv node))
        (zip bindings locations)
      evalExpr recursiveEnv body
    GrinFetch _ pointer ->
      evalValue env pointer >>= fetchValue
    GrinUpdate pointer value -> do
      pointerValue <- evalValue env pointer
      updatedValue <- evalValue env value
      updateValue pointerValue updatedValue
    GrinEval _ value ->
      evalValue env value >>= forceValue
    GrinApply _ function argument -> do
      functionValue <- evalValue env function
      argumentValue <- evalValue env argument
      applyValue functionValue argumentValue
    GrinCase scrutinee binder alternatives -> do
      value <- evalValue env scrutinee >>= forceValue
      matchAlternative (Map.insert binder value env) value alternatives
    GrinDictSelect _ dictionary index -> do
      dictionaryValue <- evalValue env dictionary >>= forceValue
      case dictionaryValue of
        RuntimeNode GrinDictionary fields
          | index >= 0,
            index < length fields ->
              forceValue (fields !! index)
          | otherwise -> throwInterpret (InterpretInvalidDictSelect dictionaryValue index)
        other -> throwInterpret (InterpretInvalidDictSelect other index)
    GrinThrow exception -> do
      exceptionValue <- evalValue env exception >>= forceValue
      throwE (EvalRaised exceptionValue)
    GrinCatch _ action handler state -> do
      actionValue <- evalValue env action
      handlerValue <- evalValue env handler
      stateValue <- evalValue env state
      applyValue actionValue stateValue `catchE` handleRaised handlerValue stateValue
    GrinForeignCallExpr foreignCall arguments -> do
      argumentValues <- mapM (evalValue env) arguments
      executeForeignCall foreignCall argumentValues

handleRaised :: RuntimeValue -> RuntimeValue -> EvalFailure -> EvalM RuntimeValue
handleRaised handler state failure =
  case failure of
    EvalRaised exception -> do
      handlerWithException <- applyValue handler exception
      applyValue handlerWithException state
    EvalInterpret err -> throwE (EvalInterpret err)

evalValue :: Env -> GrinValue -> EvalM RuntimeValue
evalValue env value =
  case value of
    GrinVarValue var ->
      case Map.lookup var env of
        Just runtimeValue -> pure runtimeValue
        Nothing -> do
          globals <- getsMachine machineGlobals
          case Map.lookup (grinVarName var) globals of
            Just runtimeValue -> pure runtimeValue
            Nothing -> throwInterpret (InterpretUnboundVariable var)
    GrinLitValue literal -> pure (RuntimeLit literal)
    GrinNodeValue node -> evalNode env node

evalNode :: Env -> GrinNode -> EvalM RuntimeValue
evalNode env node =
  RuntimeNode (grinNodeTag node) <$> mapM (evalValue env) (grinNodeFields node)

allocateCell :: HeapCell -> EvalM RuntimeValue
allocateCell cell = RuntimeLocation <$> allocateLocation cell

allocateLocation :: HeapCell -> EvalM Int
allocateLocation cell = do
  location <- getsMachine machineNextLocation
  modifyMachine $ \machine ->
    machine
      { machineHeap = IntMap.insert location cell (machineHeap machine),
        machineNextLocation = location + 1
      }
  pure location

readCell :: Int -> EvalM HeapCell
readCell location = do
  heap <- getsMachine machineHeap
  case IntMap.lookup location heap of
    Just cell -> pure cell
    Nothing -> throwInterpret (InterpretInvalidLocation location)

writeCell :: Int -> HeapCell -> EvalM ()
writeCell location cell = do
  heap <- getsMachine machineHeap
  if IntMap.member location heap
    then modifyMachine $ \machine -> machine {machineHeap = IntMap.insert location cell heap}
    else throwInterpret (InterpretInvalidLocation location)

fetchValue :: RuntimeValue -> EvalM RuntimeValue
fetchValue value =
  case value of
    RuntimeLocation location -> do
      cell <- readCell location
      case cell of
        HeapSuspended nodeEnv node -> evalNode nodeEnv node
        HeapValue result -> pure result
        HeapRaised exception -> throwE (EvalRaised exception)
        HeapBlackhole -> throwInterpret (InterpretBlackhole location)
    other -> throwInterpret (InterpretExpectedLocation other)

updateValue :: RuntimeValue -> RuntimeValue -> EvalM RuntimeValue
updateValue pointer value =
  case pointer of
    RuntimeLocation location -> writeCell location (HeapValue value) >> pure value
    other -> throwInterpret (InterpretExpectedLocation other)

forceValue :: RuntimeValue -> EvalM RuntimeValue
forceValue value =
  case value of
    RuntimeLocation location -> forceLocation location
    RuntimeNode (GrinPrimitive name 0) [] -> evalPrimitive name []
    _ -> pure value

forceLocation :: Int -> EvalM RuntimeValue
forceLocation location = do
  cell <- readCell location
  case cell of
    HeapSuspended nodeEnv node -> do
      nodeValue <- evalNode nodeEnv node
      case nodeValue of
        RuntimeNode (GrinThunk functionName) fields -> do
          writeCell location HeapBlackhole
          result <- (Right <$> callFunction functionName fields) `catchE` (pure . Left)
          case result of
            Right value -> do
              writeCell location (HeapValue value)
              forceValue value
            Left failure@(EvalRaised exception) -> do
              writeCell location (HeapRaised exception)
              throwE failure
            Left failure@(EvalInterpret _) -> do
              writeCell location cell
              throwE failure
        other -> pure other
    HeapValue result -> forceValue result
    HeapRaised exception -> throwE (EvalRaised exception)
    HeapBlackhole -> throwInterpret (InterpretBlackhole location)

applyValue :: RuntimeValue -> RuntimeValue -> EvalM RuntimeValue
applyValue function argument = do
  forcedFunction <- forceValue function
  case forcedFunction of
    RuntimeNode (GrinClosure functionName) fields ->
      callFunction functionName (fields <> [argument])
    RuntimeNode constructor@(GrinConstructor _) fields ->
      pure (RuntimeNode constructor (fields <> [argument]))
    RuntimeNode (GrinPrimitive name arity) fields ->
      applyPrimitive name arity (fields <> [argument])
    other -> throwInterpret (InterpretApplyNonFunction other)

callFunction :: FunctionName -> [RuntimeValue] -> EvalM RuntimeValue
callFunction functionName arguments = do
  functions <- getsMachine machineFunctions
  case Map.lookup functionName functions of
    Nothing -> throwInterpret (InterpretUnknownFunction functionName)
    Just function ->
      let parameters = grinFunctionParameters function
       in if length parameters == length arguments
            then evalExpr (Map.fromList (zip parameters arguments)) (grinFunctionBody function)
            else throwInterpret (InterpretFunctionArity functionName (length parameters) (length arguments))

applyPrimitive :: Text -> Int -> [RuntimeValue] -> EvalM RuntimeValue
applyPrimitive name arity arguments
  | length arguments < arity = pure (RuntimeNode (GrinPrimitive name arity) arguments)
  | length arguments == arity = evalPrimitive name arguments
  | otherwise = throwInterpret (InterpretPrimitiveArity name (length arguments))

evalPrimitive :: Text -> [RuntimeValue] -> EvalM RuntimeValue
evalPrimitive "+#" [left, right] = evalIntPrimitive "+#" (+) left right
evalPrimitive "-#" [left, right] = evalIntPrimitive "-#" (-) left right
evalPrimitive "*#" [left, right] = evalIntPrimitive "*#" (*) left right
evalPrimitive "compareInt#" [left, right] = evalIntPrimitive "compareInt#" compareInts left right
evalPrimitive "<#" [left, right] =
  evalIntPrimitive "<#" (\leftInt rightInt -> if leftInt < rightInt then 1 else 0) left right
evalPrimitive "==#" [left, right] =
  evalIntPrimitive "==#" (\leftInt rightInt -> if leftInt == rightInt then 1 else 0) left right
evalPrimitive "charToInt#" [value] = do
  charValue <- forceCharPrimitiveArgument "charToInt#" value
  pure (RuntimeLit (GrinLitInt IntRep (fromIntegral (Char.ord charValue))))
evalPrimitive "intToChar#" [value] = do
  intValue <- forceIntPrimitiveArgument "intToChar#" value
  if intValue >= 0 && intValue <= 0x10ffff
    then pure (RuntimeLit (GrinLitChar WordRep (Char.chr (fromIntegral intValue))))
    else throwInterpret (InterpretPrimitiveTypeError "intToChar#" (RuntimeLit (GrinLitInt IntRep intValue)))
evalPrimitive "realWorld#" [] = pure RuntimeStateToken
evalPrimitive "raise#" [exception] =
  forceValue exception >>= throwE . EvalRaised
evalPrimitive "catch#" [action, handler, state] =
  applyValue action state `catchE` handleRaised handler state
evalPrimitive "newMutVar#" [initialValue, state] = do
  mutVar <- GrinMutVar <$> liftEvalIO (newIORef initialValue)
  pure (RuntimeNode (GrinConstructor "(#,#)") [state, RuntimeMutVar mutVar])
evalPrimitive "readMutVar#" [mutVar, state] = do
  GrinMutVar reference <- forceMutVarPrimitiveArgument "readMutVar#" mutVar
  value <- liftEvalIO (readIORef reference)
  pure (RuntimeNode (GrinConstructor "(#,#)") [state, value])
evalPrimitive "writeMutVar#" [mutVar, value, state] = do
  GrinMutVar reference <- forceMutVarPrimitiveArgument "writeMutVar#" mutVar
  liftEvalIO (writeIORef reference value)
  pure state
evalPrimitive name arguments =
  throwInterpret (InterpretPrimitiveArity name (length arguments))

evalIntPrimitive :: Text -> (Integer -> Integer -> Integer) -> RuntimeValue -> RuntimeValue -> EvalM RuntimeValue
evalIntPrimitive name operation left right = do
  leftInt <- forceIntPrimitiveArgument name left
  rightInt <- forceIntPrimitiveArgument name right
  pure (RuntimeLit (GrinLitInt IntRep (operation leftInt rightInt)))

forceIntPrimitiveArgument :: Text -> RuntimeValue -> EvalM Integer
forceIntPrimitiveArgument name value = do
  forced <- forceValue value
  case forced of
    RuntimeLit (GrinLitInt _ intValue) -> pure intValue
    other -> throwInterpret (InterpretPrimitiveTypeError name other)

forceCharPrimitiveArgument :: Text -> RuntimeValue -> EvalM Char
forceCharPrimitiveArgument name value = do
  forced <- forceValue value
  case forced of
    RuntimeLit (GrinLitChar _ charValue) -> pure charValue
    other -> throwInterpret (InterpretPrimitiveTypeError name other)

forceMutVarPrimitiveArgument :: Text -> RuntimeValue -> EvalM GrinMutVar
forceMutVarPrimitiveArgument name value = do
  forced <- forceValue value
  case forced of
    RuntimeMutVar mutVar -> pure mutVar
    other -> throwInterpret (InterpretPrimitiveTypeError name other)

compareInts :: Integer -> Integer -> Integer
compareInts left right =
  case compare left right of
    LT -> -1
    EQ -> 0
    GT -> 1

executeForeignCall :: GrinForeignCall -> [RuntimeValue] -> EvalM RuntimeValue
executeForeignCall foreignCall arguments
  | actualArity /= expectedArity = throwInterpret (InterpretForeignArity name expectedArity actualArity)
  | otherwise =
      case grinForeignEffect signature of
        GrinForeignPure -> callForeign foreignCall arguments
        GrinForeignRealWorld ->
          case reverse arguments of
            state : reversedAbiArguments -> do
              result <- callForeign foreignCall (reverse reversedAbiArguments)
              pure (RuntimeNode (GrinConstructor "(#,#)") [state, result])
            [] -> throwInterpret (InterpretForeignArity name expectedArity actualArity)
  where
    name = grinForeignCallName foreignCall
    signature = grinForeignCallSignature foreignCall
    actualArity = length arguments
    expectedArity = length (grinForeignOperandReps signature)

callForeign :: GrinForeignCall -> [RuntimeValue] -> EvalM RuntimeValue
callForeign foreignCall arguments = do
  marshalledArguments <-
    zipWithM
      (marshalForeignArgument (grinForeignCallSymbol foreignCall))
      (grinForeignArgumentTypes (grinForeignCallSignature foreignCall))
      arguments
  functionPointer <- lookupForeignFunction foreignCall
  case grinForeignResultType (grinForeignCallSignature foreignCall) of
    GrinForeignInt32 -> do
      CInt result <- liftEvalIO (callFFI functionPointer retCInt marshalledArguments)
      pure (RuntimeLit (GrinLitInt Int32Rep (toInteger result)))
    GrinForeignWord64 -> do
      result <- liftEvalIO (callFFI functionPointer retWord64 marshalledArguments)
      pure (RuntimeLit (GrinLitInt Word64Rep (toInteger result)))

marshalForeignArgument :: Text -> GrinForeignType -> RuntimeValue -> EvalM Arg
marshalForeignArgument symbol GrinForeignInt32 argument = do
  argumentValue <- forceInt32 symbol argument
  pure (argCInt (CInt (fromInteger argumentValue)))
marshalForeignArgument symbol GrinForeignWord64 argument = do
  argumentValue <- forceWord64 symbol argument
  pure (argWord64 (fromInteger argumentValue :: Word64))

lookupForeignFunction :: GrinForeignCall -> EvalM (FunPtr ())
lookupForeignFunction foreignCall = do
  lookupResult <- liftEvalIO (tryForeign (dlsym Default (T.unpack (grinForeignCallSymbol foreignCall))))
  case lookupResult of
    Left err ->
      throwInterpret
        ( InterpretForeignLookupError
            (grinForeignCallSymbol foreignCall)
            (T.pack (displayException err))
        )
    Right pointer -> pure pointer

tryForeign :: IO value -> IO (Either SomeException value)
tryForeign = try

forceInt32 :: Text -> RuntimeValue -> EvalM Integer
forceInt32 symbol value = do
  forced <- forceValue value
  case forced of
    RuntimeLit (GrinLitInt Int32Rep intValue) -> pure intValue
    other -> throwInterpret (InterpretForeignTypeError symbol other)

forceWord64 :: Text -> RuntimeValue -> EvalM Integer
forceWord64 symbol value = do
  forced <- forceValue value
  case forced of
    RuntimeLit (GrinLitInt Word64Rep intValue) -> pure intValue
    other -> throwInterpret (InterpretForeignTypeError symbol other)

runIOValue :: RuntimeValue -> EvalM RuntimeValue
runIOValue action = do
  result <- applyValue action RuntimeStateToken >>= forceValue
  case result of
    RuntimeNode (GrinConstructor "(#,#)") [_state, ioResult] -> forceValue ioResult
    other -> throwInterpret (InterpretInvalidIOResult other)

matchAlternative :: Env -> RuntimeValue -> [GrinAlt] -> EvalM RuntimeValue
matchAlternative env value alternatives =
  case alternatives of
    [] -> throwInterpret (InterpretNoMatchingAlternative value)
    alt : rest ->
      case matchAlt value alt of
        Just bindings -> evalExpr (bindings `Map.union` env) (grinAltRhs alt)
        Nothing -> matchAlternative env value rest

matchAlt :: RuntimeValue -> GrinAlt -> Maybe Env
matchAlt value alt =
  case (grinAltCon alt, value) of
    (GrinDefaultAlt, _) ->
      Just (Map.fromList [(var, value) | var <- grinAltBinders alt])
    (GrinLitAlt expected, RuntimeLit actual)
      | expected == actual -> Just Map.empty
    (GrinDataAlt expected, RuntimeNode (GrinConstructor actual) fields)
      | expected == actual,
        length fields == length (grinAltBinders alt) ->
          Just (Map.fromList (zip (grinAltBinders alt) fields))
    _ -> Nothing

renderRawValueM :: RuntimeValue -> EvalM Text
renderRawValueM value = do
  forced <- forceValue value
  case forced of
    RuntimeLit literal -> pure (renderLiteral literal)
    RuntimeNode (GrinConstructor "C#") [char] -> renderBoxedChar char
    RuntimeNode (GrinConstructor name) [] -> pure name
    RuntimeNode (GrinConstructor name) arguments
      | isTupleConstructor name (length arguments) -> do
          renderedArguments <- mapM renderRawArgument arguments
          pure ("(" <> T.intercalate "," renderedArguments <> ")")
    RuntimeNode (GrinConstructor name) arguments -> do
      renderedArguments <- mapM renderRawArgument arguments
      pure (T.unwords (name : renderedArguments))
    RuntimeNode GrinDictionary _ -> pure "<dictionary>"
    RuntimeNode GrinClosure {} _ -> pure "<function>"
    RuntimeNode GrinPrimitive {} _ -> pure "<function>"
    RuntimeNode GrinThunk {} _ -> pure "<thunk>"
    RuntimeLocation _ -> renderRawValueM forced
    RuntimeMutVar {} -> pure "<mutvar>"
    RuntimeStateToken -> pure "<state>"

renderRawArgument :: RuntimeValue -> EvalM Text
renderRawArgument value = do
  forced <- forceValue value
  rendered <- renderRawValueM forced
  pure $
    case forced of
      RuntimeNode (GrinConstructor name) arguments
        | isTupleConstructor name (length arguments) -> rendered
      RuntimeNode (GrinConstructor "C#") [_] -> rendered
      RuntimeNode (GrinConstructor _) (_ : _) -> "(" <> rendered <> ")"
      _ -> rendered

renderLiteral :: GrinLiteral -> Text
renderLiteral literal =
  case literal of
    GrinLitInt _ value -> T.pack (show value)
    GrinLitChar _ value -> T.pack (show value) <> "#"
    GrinLitString value -> T.pack (show (T.unpack value))

renderBoxedChar :: RuntimeValue -> EvalM Text
renderBoxedChar value = do
  forced <- forceValue value
  case forced of
    RuntimeLit (GrinLitChar _ charValue) -> pure (T.pack (show charValue))
    other -> throwInterpret (InterpretPrimitiveTypeError "C#" other)

isTupleConstructor :: Text -> Int -> Bool
isTupleConstructor name arity =
  arity >= 2
    && ( name == "(" <> T.replicate (arity - 1) "," <> ")"
           || name == "(#" <> T.replicate (arity - 1) "," <> "#)"
       )

throwInterpret :: InterpretError -> EvalM value
throwInterpret = throwE . EvalInterpret

liftEvalIO :: IO value -> EvalM value
liftEvalIO = lift . lift

getsMachine :: (Machine -> value) -> EvalM value
getsMachine = lift . gets

modifyMachine :: (Machine -> Machine) -> EvalM ()
modifyMachine = lift . modify'
