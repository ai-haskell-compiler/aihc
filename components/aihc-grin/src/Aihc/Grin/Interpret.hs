{-# LANGUAGE OverloadedStrings #-}

-- | Reference interpreter for strict GRIN programs.
module Aihc.Grin.Interpret
  ( InterpretError (..),
    RuntimeValue (..),
    interpretProgramBinding,
    interpretProgramIoBinding,
    interpretProgramFunctionSnapshot,
  )
where

import Aihc.Grin.Snapshot
import Aihc.Grin.Syntax
import Aihc.Tc.Types (RuntimeRep (..))
import Control.Exception (SomeException, displayException, try)
import Control.Monad (zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (State, StateT, execState, get, gets, modify', runState, runStateT)
import Data.Char qualified as Char
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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
  | InterpretResultArity !Int !Int
  | InterpretInvalidThunkResult ![RuntimeValue]
  | InterpretInvalidThunkResultRep !FunctionName !RuntimeRep
  | InterpretInvalidUpdateValue !RuntimeValue
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
  = HeapSuspended !FunctionName ![RuntimeValue]
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

data SnapshotBuild = SnapshotBuild
  { snapshotBuildSource :: !(IntMap HeapCell),
    snapshotBuildLocations :: !(IntMap Int),
    snapshotBuildSources :: !(IntMap Int),
    snapshotBuildNextLocation :: !Int,
    snapshotBuildCells :: !(IntMap SnapshotCell)
  }

-- | Interpret and render a named top-level binding using the raw constructor
-- representation shared by the compiler pipeline evaluation fixtures.
interpretProgramBinding :: Text -> GrinProgram -> IO (Either InterpretError Text)
interpretProgramBinding = interpretProgramBindingWith pure

-- | Interpret a named top-level binding and explicitly run its value as an IO
-- action. The caller, rather than GRIN, owns the decision to use this entry
-- point.
interpretProgramIoBinding :: Text -> GrinProgram -> IO (Either InterpretError Text)
interpretProgramIoBinding = interpretProgramBindingWith runIOValue

-- | Execute a nullary GRIN function and snapshot its raw return values and
-- reachable heap. Snapshotting reads cells but never enters a thunk or forces
-- a location; only 'GrinEval' nodes executed by the function may do that.
interpretProgramFunctionSnapshot :: FunctionName -> GrinProgram -> IO (Either InterpretError HeapSnapshot)
interpretProgramFunctionSnapshot functionName program = do
  let machine = initialMachine program
  (result, finalMachine) <- runStateT (runExceptT (callFunction functionName [])) machine
  pure $
    case result of
      Right values -> Right (buildHeapSnapshot (machineHeap finalMachine) values)
      Left (EvalInterpret err) -> Left err
      Left (EvalRaised exception) -> Left (InterpretRaisedException (T.pack (show exception)))

interpretProgramBindingWith :: (RuntimeValue -> EvalM RuntimeValue) -> Text -> GrinProgram -> IO (Either InterpretError Text)
interpretProgramBindingWith enterValue name program = do
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
      result <- enterValue forced
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
          [ (location, storedCell (staticNode node))
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
    staticGlobals =
      Map.fromList
        [ (grinVarName var, staticNode node)
        | (var, node) <- grinWhnfGlobals program
        ]
    globals =
      Map.unions
        [ Map.fromList [(grinVarName var, value) | (var, value) <- cafLocations],
          staticGlobals,
          Map.fromList
            [ (constructor, RuntimeNode (GrinConstructor constructor) [])
            | (constructor, arity) <- builtinConstructors <> [(name, length fields) | (name, fields) <- grinConstructors program],
              arity == 0
            ]
        ]
    staticNode (GrinNode tag fields) = RuntimeNode tag (map staticValue fields)
    staticValue value =
      case value of
        GrinVarValue var ->
          case Map.lookup (grinVarName var) globals of
            Just runtimeValue -> runtimeValue
            Nothing -> error ("GRIN interpreter found an unbound static global " <> T.unpack (grinVarName var))
        GrinLitValue literal -> RuntimeLit literal

evalExpr :: Env -> GrinExpr -> EvalM [RuntimeValue]
evalExpr env expr =
  case expr of
    GrinConstant values -> mapM (materializeValue env) values
    GrinBind vars valueExpr body -> do
      values <- evalExpr env valueExpr
      if length vars == length values
        then evalExpr (Map.fromList (zip vars values) `Map.union` env) body
        else throwInterpret (InterpretResultArity (length vars) (length values))
    GrinStore node ->
      pure <$> (materializeNode env node >>= allocateCell . storedCell)
    GrinStoreRec bindings body -> do
      locations <- mapM (const (allocateLocation HeapBlackhole)) bindings
      let recursiveBindings = zip (map fst bindings) (map RuntimeLocation locations)
          recursiveEnv = Map.fromList recursiveBindings `Map.union` env
      runtimeNodes <- mapM (materializeNode recursiveEnv . snd) bindings
      mapM_ (uncurry writeCell) (zip locations (map storedCell runtimeNodes))
      evalExpr recursiveEnv body
    GrinFetch _ pointer ->
      (: []) <$> (materializeValue env pointer >>= fetchValue)
    GrinUpdate pointer value -> do
      pointerValue <- materializeValue env pointer
      updatedValue <- materializeValue env value
      pure <$> updateValue pointerValue updatedValue
    GrinEval _ value ->
      (: []) <$> (materializeValue env value >>= forceValue)
    GrinCall _ functionName arguments ->
      callFunction functionName =<< mapM (materializeValue env) arguments
    GrinPrimitiveCall _ name arguments ->
      evalPrimitive name =<< mapM (materializeValue env) arguments
    GrinApply _ function arguments -> do
      functionValue <- materializeValue env function
      argumentValues <- mapM (materializeValue env) arguments
      applyValue functionValue argumentValues
    GrinCase scrutinee binder alternatives -> do
      value <- materializeValue env scrutinee >>= forceValue
      matchAlternative (Map.insert binder value env) value alternatives
    GrinThrow exception -> do
      exceptionValue <- materializeValue env exception >>= forceValue
      throwE (EvalRaised exceptionValue)
    GrinCatch runtimeRep action handler state -> do
      actionValue <- materializeValue env action
      handlerValue <- materializeValue env handler
      stateValues <- mapM (materializeValue env) state
      results <- applyValue actionValue stateValues `catchE` handleRaised handlerValue stateValues
      let expectedCount = length (runtimeRepComponents runtimeRep)
      case length results - expectedCount of
        0 -> pure results
        -- Shared evaluator fixtures type the otherwise zero-width State#
        -- result as lifted. Its delayed placeholder is the one leading value.
        1 -> pure (drop 1 results)
        _ -> throwInterpret (InterpretResultArity expectedCount (length results))
    GrinForeignCallExpr foreignCall arguments -> do
      argumentValues <- mapM (materializeValue env) arguments
      executeForeignCall foreignCall argumentValues

handleRaised :: RuntimeValue -> [RuntimeValue] -> EvalFailure -> EvalM [RuntimeValue]
handleRaised handler state failure =
  case failure of
    EvalRaised exception -> do
      handlerWithException <- expectSingle =<< applyValue handler [exception]
      applyValue handlerWithException state
    EvalInterpret err -> throwE (EvalInterpret err)

-- | Resolve an atomic GRIN value into its runtime representation. This only
-- captures variables and constructs nodes; it never forces a heap location or
-- enters a thunk.
materializeValue :: Env -> GrinValue -> EvalM RuntimeValue
materializeValue env value =
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

materializeNode :: Env -> GrinNode -> EvalM RuntimeValue
materializeNode env node =
  RuntimeNode (grinNodeTag node) <$> mapM (materializeValue env) (grinNodeFields node)

storedCell :: RuntimeValue -> HeapCell
storedCell value =
  case value of
    RuntimeNode (GrinThunk functionName) fields -> HeapSuspended functionName fields
    _ -> HeapValue value

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
        HeapSuspended functionName fields -> pure (RuntimeNode (GrinThunk functionName) fields)
        HeapValue result -> pure result
        HeapRaised exception -> throwE (EvalRaised exception)
        HeapBlackhole -> throwInterpret (InterpretBlackhole location)
    other -> throwInterpret (InterpretExpectedLocation other)

updateValue :: RuntimeValue -> RuntimeValue -> EvalM RuntimeValue
updateValue pointer value =
  if isLiftedRuntimeValue value
    then case pointer of
      RuntimeLocation location -> writeCell location (HeapValue value) >> pure value
      other -> throwInterpret (InterpretExpectedLocation other)
    else throwInterpret (InterpretInvalidUpdateValue value)

forceValue :: RuntimeValue -> EvalM RuntimeValue
forceValue value =
  case value of
    RuntimeLocation location -> forceLocation location
    _ -> pure value

forceLocation :: Int -> EvalM RuntimeValue
forceLocation location = do
  cell <- readCell location
  case cell of
    HeapSuspended functionName fields -> do
      function <- lookupFunction functionName
      let resultRep = grinFunctionResultRep function
      if isLiftedRuntimeRep resultRep
        then pure ()
        else throwInterpret (InterpretInvalidThunkResultRep functionName resultRep)
      writeCell location HeapBlackhole
      result <- (Right <$> callFunction functionName fields) `catchE` (pure . Left)
      case result of
        Right [value] -> do
          writeCell location (HeapValue value)
          forceValue value
        Right values -> do
          writeCell location cell
          throwInterpret (InterpretInvalidThunkResult values)
        Left failure@(EvalRaised exception) -> do
          writeCell location (HeapRaised exception)
          throwE failure
        Left failure@(EvalInterpret _) -> do
          writeCell location cell
          throwE failure
    HeapValue result -> forceValue result
    HeapRaised exception -> throwE (EvalRaised exception)
    HeapBlackhole -> throwInterpret (InterpretBlackhole location)

applyValue :: RuntimeValue -> [RuntimeValue] -> EvalM [RuntimeValue]
applyValue function arguments = do
  forcedFunction <- forceValue function
  case forcedFunction of
    RuntimeNode (GrinClosure functionName argumentCount) fields ->
      let runtimeArguments
            | argumentCount == length arguments = arguments
            -- Synthetic evaluator fixtures may leave an ignored State#
            -- lambda binder lifted even though the call site correctly has
            -- no runtime component for it.
            | null arguments,
              argumentCount == 1 =
                [RuntimeStateToken]
            | otherwise = arguments
       in case compare (length runtimeArguments) argumentCount of
            LT ->
              pure
                [ RuntimeNode
                    (GrinClosure functionName (argumentCount - length runtimeArguments))
                    (fields <> runtimeArguments)
                ]
            EQ -> callFunction functionName (fields <> runtimeArguments)
            GT -> throwInterpret (InterpretFunctionArity functionName argumentCount (length arguments))
    RuntimeNode constructor@(GrinConstructor _) fields ->
      pure [RuntimeNode constructor (fields <> arguments)]
    RuntimeNode (GrinPrimitive name arity) fields ->
      applyPrimitive name arity fields arguments
    other -> throwInterpret (InterpretApplyNonFunction other)

callFunction :: FunctionName -> [RuntimeValue] -> EvalM [RuntimeValue]
callFunction functionName arguments = do
  function <- lookupFunction functionName
  let parameters = grinFunctionParameters function
  if length parameters == length arguments
    then evalExpr (Map.fromList (zip parameters arguments)) (grinFunctionBody function)
    else throwInterpret (InterpretFunctionArity functionName (length parameters) (length arguments))

lookupFunction :: FunctionName -> EvalM GrinFunction
lookupFunction functionName = do
  functions <- getsMachine machineFunctions
  case Map.lookup functionName functions of
    Nothing -> throwInterpret (InterpretUnknownFunction functionName)
    Just function -> pure function

isLiftedRuntimeValue :: RuntimeValue -> Bool
isLiftedRuntimeValue value =
  case value of
    RuntimeLit literal -> isLiftedRuntimeRep (grinValueRuntimeRep (GrinLitValue literal))
    RuntimeNode {} -> True
    RuntimeLocation {} -> True
    RuntimeMutVar {} -> False
    RuntimeStateToken -> False

applyPrimitive :: Text -> Int -> [RuntimeValue] -> [RuntimeValue] -> EvalM [RuntimeValue]
applyPrimitive name remaining captured arguments
  | remaining > 1 = pure [RuntimeNode (GrinPrimitive name (remaining - 1)) (captured <> arguments)]
  | remaining == 1 = evalPrimitive name (captured <> arguments)
  | otherwise = throwInterpret (InterpretPrimitiveArity name (length captured))

evalPrimitive :: Text -> [RuntimeValue] -> EvalM [RuntimeValue]
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
  pure [RuntimeLit (GrinLitInt IntRep (fromIntegral (Char.ord charValue)))]
evalPrimitive "intToChar#" [value] = do
  intValue <- forceIntPrimitiveArgument "intToChar#" value
  if intValue >= 0 && intValue <= 0x10ffff
    then pure [RuntimeLit (GrinLitChar WordRep (Char.chr (fromIntegral intValue)))]
    else throwInterpret (InterpretPrimitiveTypeError "intToChar#" (RuntimeLit (GrinLitInt IntRep intValue)))
evalPrimitive "realWorld#" [] = pure []
evalPrimitive "raise#" [exception] =
  forceValue exception >>= throwE . EvalRaised
evalPrimitive "catch#" [action, handler] =
  applyValue action [] `catchE` handleRaised handler []
evalPrimitive "newMutVar#" [initialValue] = do
  mutVar <- GrinMutVar <$> liftEvalIO (newIORef initialValue)
  pure [RuntimeMutVar mutVar]
evalPrimitive "readMutVar#" [mutVar] = do
  GrinMutVar reference <- forceMutVarPrimitiveArgument "readMutVar#" mutVar
  value <- liftEvalIO (readIORef reference)
  pure [value]
evalPrimitive "writeMutVar#" [mutVar, value] = do
  GrinMutVar reference <- forceMutVarPrimitiveArgument "writeMutVar#" mutVar
  liftEvalIO (writeIORef reference value)
  pure []
evalPrimitive name arguments =
  throwInterpret (InterpretPrimitiveArity name (length arguments))

evalIntPrimitive :: Text -> (Integer -> Integer -> Integer) -> RuntimeValue -> RuntimeValue -> EvalM [RuntimeValue]
evalIntPrimitive name operation left right = do
  leftInt <- forceIntPrimitiveArgument name left
  rightInt <- forceIntPrimitiveArgument name right
  pure [RuntimeLit (GrinLitInt IntRep (operation leftInt rightInt))]

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

executeForeignCall :: GrinForeignCall -> [RuntimeValue] -> EvalM [RuntimeValue]
executeForeignCall foreignCall arguments
  | actualArity /= expectedArity = throwInterpret (InterpretForeignArity name expectedArity actualArity)
  | otherwise =
      (: []) <$> callForeign foreignCall arguments
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
  results <- applyValue action []
  case results of
    [ioResult] -> forceValue ioResult
    _ -> throwInterpret (InterpretResultArity 1 (length results))

matchAlternative :: Env -> RuntimeValue -> [GrinAlt] -> EvalM [RuntimeValue]
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

expectSingle :: [RuntimeValue] -> EvalM RuntimeValue
expectSingle values =
  case values of
    [value] -> pure value
    _ -> throwInterpret (InterpretResultArity 1 (length values))

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

buildHeapSnapshot :: IntMap HeapCell -> [RuntimeValue] -> HeapSnapshot
buildHeapSnapshot source values =
  let initial =
        SnapshotBuild
          { snapshotBuildSource = source,
            snapshotBuildLocations = IntMap.empty,
            snapshotBuildSources = IntMap.empty,
            snapshotBuildNextLocation = 0,
            snapshotBuildCells = IntMap.empty
          }
      (returnValues, afterRoots) = runState (mapM snapshotRuntimeValue values) initial
      final = execState (snapshotPendingCells 0) afterRoots
   in HeapSnapshot
        { snapshotReturnValues = returnValues,
          snapshotHeap = snapshotBuildCells final
        }

snapshotPendingCells :: Int -> State SnapshotBuild ()
snapshotPendingCells location = do
  state <- get
  if location >= snapshotBuildNextLocation state
    then pure ()
    else do
      case IntMap.lookup location (snapshotBuildSources state) >>= (`IntMap.lookup` snapshotBuildSource state) of
        Nothing -> pure ()
        Just cell -> do
          snapshotCell <- snapshotHeapCell cell
          modify' $ \current -> current {snapshotBuildCells = IntMap.insert location snapshotCell (snapshotBuildCells current)}
      snapshotPendingCells (location + 1)

snapshotHeapCell :: HeapCell -> State SnapshotBuild SnapshotCell
snapshotHeapCell cell =
  case cell of
    HeapSuspended functionName fields -> SnapshotSuspended functionName <$> mapM snapshotRuntimeValue fields
    HeapValue value -> SnapshotValue <$> snapshotRuntimeValue value
    HeapRaised exception -> SnapshotRaised <$> snapshotRuntimeValue exception
    HeapBlackhole -> pure SnapshotBlackhole

snapshotRuntimeValue :: RuntimeValue -> State SnapshotBuild SnapshotValue
snapshotRuntimeValue value =
  case value of
    RuntimeLit literal -> pure (SnapshotLiteral literal)
    RuntimeNode tag fields -> SnapshotNode tag <$> mapM snapshotRuntimeValue fields
    RuntimeLocation sourceLocation -> SnapshotLocation <$> snapshotLocation sourceLocation
    RuntimeMutVar {} -> pure SnapshotMutVar
    RuntimeStateToken -> pure SnapshotStateToken

snapshotLocation :: Int -> State SnapshotBuild Int
snapshotLocation sourceLocation = do
  state <- get
  case IntMap.lookup sourceLocation (snapshotBuildLocations state) of
    Just location -> pure location
    Nothing -> do
      let location = snapshotBuildNextLocation state
      modify' $ \current ->
        current
          { snapshotBuildLocations = IntMap.insert sourceLocation location (snapshotBuildLocations current),
            snapshotBuildSources = IntMap.insert location sourceLocation (snapshotBuildSources current),
            snapshotBuildNextLocation = location + 1
          }
      pure location
