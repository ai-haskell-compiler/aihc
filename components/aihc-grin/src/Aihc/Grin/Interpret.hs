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
import Aihc.Tc.Types (Levity (..), RuntimeRep (..))
import Control.Exception (SomeException, displayException, try)
import Control.Monad (zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, catchE, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (State, StateT, execState, get, gets, modify', runState, runStateT)
import Data.ByteString qualified as BS
import Data.Char qualified as Char
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq, ViewL (..), (|>))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Foreign.C.Types (CInt (..))
import Foreign.LibFFI (Arg, argCInt, argPtr, argWord64, callFFI, retCInt, retPtr, retVoid, retWord64)
import Foreign.Marshal.Array (newArray0, peekArray, withArray0)
import Foreign.Ptr (FunPtr, Ptr, castPtr)
import System.IO (Handle, hFlush, stdin, stdout)
import System.Posix.DynamicLinker (DL (Default), dlsym)

data InterpretError
  = InterpretUnboundVariable !GrinVar
  | InterpretMissingBinding !Text
  | InterpretUnknownFunction !FunctionName
  | InterpretFunctionArity !FunctionName !Int !Int
  | InterpretConstructorArity !Text !Int !Int
  | InterpretApplyNonFunction !RuntimeValue
  | InterpretNoMatchingAlternative !RuntimeValue
  | InterpretPrimitiveArity !Text !Int
  | InterpretPrimitiveTypeError !Text !RuntimeValue
  | InterpretForeignArity !Text !Int !Int
  | InterpretForeignTypeError !Text !RuntimeValue
  | InterpretForeignLookupError !Text !Text
  | InterpretInvalidIOBufferRange !Text !Integer !Integer !Int
  | InterpretResultArity !Int !Int
  | InterpretInvalidThunkResult ![RuntimeValue]
  | InterpretInvalidThunkResultRep !FunctionName !RuntimeRep
  | InterpretInvalidUpdateValue !RuntimeValue
  | InterpretExpectedLocation !RuntimeValue
  | InterpretInvalidLocation !Int
  | InterpretBlackhole !Int
  | InterpretNoRunnableThreads
  | InterpretCpsExpression !GrinExpr
  | InterpretRaisedException !Text
  deriving (Eq, Show)

data RuntimeValue
  = RuntimeLit !GrinLiteral
  | RuntimeAddress !(Ptr ())
  | RuntimeIOHandle !GrinIOHandle
  | RuntimeIOBuffer !GrinIOBuffer
  | RuntimeIORequest !GrinIORequest
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

data GrinIOHandle = GrinIOHandle !Int !Handle

instance Eq GrinIOHandle where
  GrinIOHandle left _ == GrinIOHandle right _ = left == right

instance Show GrinIOHandle where
  show _ = "<io-handle>"

newtype GrinIOBuffer = GrinIOBuffer (IORef BS.ByteString)

instance Eq GrinIOBuffer where
  GrinIOBuffer left == GrinIOBuffer right = left == right

instance Show GrinIOBuffer where
  show _ = "<io-buffer>"

data GrinIOOperation
  = GrinRead !GrinIOHandle !GrinIOBuffer !Int !Int
  | GrinWrite !GrinIOHandle !GrinIOBuffer !Int !Int
  deriving (Eq, Show)

data GrinIOState
  = GrinIOSubmitted !GrinIOOperation
  | GrinIOCompleted !Integer
  | GrinIOConsumed
  deriving (Eq, Show)

newtype GrinIORequest = GrinIORequest (IORef GrinIOState)

instance Eq GrinIORequest where
  GrinIORequest left == GrinIORequest right = left == right

instance Show GrinIORequest where
  show _ = "<io-request>"

type Env = Map GrinVar RuntimeValue

data HeapCell
  = HeapSuspended !FunctionName ![RuntimeValue]
  | HeapValue !RuntimeValue
  | HeapRaised !RuntimeValue
  | HeapBlackhole
  | HeapThread

data Machine = Machine
  { machineProgram :: !GrinProgram,
    machineFunctions :: !(Map FunctionName GrinFunction),
    machineGlobals :: !(Map Text RuntimeValue),
    machineHeap :: !(IntMap HeapCell),
    machineNextLocation :: !Int,
    machineRunQueue :: !(Seq ThreadAction)
  }

data EvalFailure
  = EvalInterpret !InterpretError
  | EvalRaised !RuntimeValue

type EvalM = ExceptT EvalFailure (StateT Machine IO)

-- | A suspended direct-style continuation. Keeping the continuation as an
-- interpreter action lets yield# switch threads without relying on the host
-- call stack to represent the resumed computation.
newtype ThreadAction = ThreadAction (EvalM [RuntimeValue])

type ScheduledContinuation = [RuntimeValue] -> EvalM [RuntimeValue]

data SnapshotBuild = SnapshotBuild
  { snapshotBuildSource :: !(IntMap HeapCell),
    snapshotBuildValueSources :: ![(RuntimeValue, Int)],
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
          | ((_, node), location) <- zip globalNodes [0 ..]
          ],
      machineNextLocation = length globalNodes,
      machineRunQueue = Seq.empty
    }
  where
    globalNodes = Map.toAscList globalNodeMap
    globalNodeMap =
      Map.unions
        [ Map.fromList [(grinVarName var, node) | (var, node) <- grinCafs program],
          Map.fromList [(grinVarName var, node) | (var, node) <- grinWhnfGlobals program],
          Map.fromList
            [ (constructor, GrinNode (GrinConstructor constructor 0) [])
            | (constructor, layouts) <- builtinConstructors <> grinConstructors program,
              null layouts
            ]
        ]
    globals =
      Map.fromList
        [ (name, RuntimeLocation location)
        | ((name, _), location) <- zip globalNodes [0 ..]
        ]
    staticNode (GrinNode tag fields) = RuntimeNode tag (map staticValue fields)
    staticValue value =
      case value of
        GrinVarValue var ->
          case Map.lookup (grinVarName var) globals of
            Just runtimeValue -> runtimeValue
            Nothing -> error ("GRIN interpreter found an unbound static global " <> T.unpack (grinVarName var))
        GrinLitValue literal -> RuntimeLit literal

evalScheduledExpr :: Env -> GrinExpr -> ScheduledContinuation -> EvalM [RuntimeValue]
evalScheduledExpr env expr continue =
  case expr of
    GrinConstant values -> continue =<< mapM (materializeValue env) values
    GrinBind vars valueExpr body ->
      evalScheduledExpr env valueExpr $ \values ->
        if length vars == length values
          then evalScheduledExpr (Map.fromList (zip vars values) `Map.union` env) body continue
          else throwInterpret (InterpretResultArity (length vars) (length values))
    GrinStore node -> do
      value <- materializeNode env node >>= allocateCell . storedCell
      continue [value]
    GrinEnsureHeap _ roots -> continue =<< mapM (materializeValue env) roots
    GrinStoreUnchecked node -> do
      value <- materializeNode env node >>= allocateCell . storedCell
      continue [value]
    GrinStoreRec bindings body -> do
      locations <- mapM (const (allocateLocation HeapBlackhole)) bindings
      let recursiveBindings = zip (map fst bindings) (map RuntimeLocation locations)
          recursiveEnv = Map.fromList recursiveBindings `Map.union` env
      runtimeNodes <- mapM (materializeNode recursiveEnv . snd) bindings
      mapM_ (uncurry writeCell) (zip locations (map storedCell runtimeNodes))
      evalScheduledExpr recursiveEnv body continue
    GrinStoreRecUnchecked bindings body -> do
      locations <- mapM (const (allocateLocation HeapBlackhole)) bindings
      let recursiveBindings = zip (map fst bindings) (map RuntimeLocation locations)
          recursiveEnv = Map.fromList recursiveBindings `Map.union` env
      runtimeNodes <- mapM (materializeNode recursiveEnv . snd) bindings
      mapM_ (uncurry writeCell) (zip locations (map storedCell runtimeNodes))
      evalScheduledExpr recursiveEnv body continue
    GrinFetch _ pointer -> do
      value <- materializeValue env pointer >>= fetchValue
      continue [value]
    GrinUpdate pointer value -> do
      pointerValue <- materializeValue env pointer
      updatedValue <- materializeValue env value
      result <- updateValue pointerValue updatedValue
      continue [result]
    GrinUpdateBlackhole {} -> rejectCpsExpression
    GrinEval _ value -> do
      runtimeValue <- materializeValue env value
      forceScheduledValue runtimeValue (continue . (: []))
    GrinCpsEval {} -> rejectCpsExpression
    GrinCall _ functionName arguments -> do
      argumentValues <- mapM (materializeValue env) arguments
      callScheduledFunction functionName argumentValues continue
    GrinPrimitiveCall _ name arguments -> do
      argumentValues <- mapM (materializeValue env) arguments
      evalScheduledPrimitive name argumentValues continue
    GrinCpsPrimitiveCall {} -> rejectCpsExpression
    GrinApply _ function arguments -> do
      functionValue <- materializeValue env function
      argumentValues <- mapM (materializeValue env) arguments
      applyScheduledValue functionValue argumentValues continue
    GrinCpsApply {} -> rejectCpsExpression
    GrinContinue {} -> rejectCpsExpression
    GrinHalt {} -> rejectCpsExpression
    GrinCase scrutinee binder alternatives -> do
      value <- materializeValue env scrutinee
      matchScheduledAlternative (Map.insert binder value env) value alternatives continue
    GrinThrow exception -> do
      exceptionValue <- materializeValue env exception
      throwE (EvalRaised exceptionValue)
    GrinCatch runtimeRep action handler state -> do
      actionValue <- materializeValue env action
      handlerValue <- materializeValue env handler
      stateValues <- mapM (materializeValue env) state
      let receive results = do
            let expectedCount = length (runtimeRepComponents runtimeRep)
            case length results - expectedCount of
              0 -> continue results
              1 -> continue (drop 1 results)
              _ -> throwInterpret (InterpretResultArity expectedCount (length results))
      applyScheduledValue actionValue stateValues receive
        `catchE` handleScheduledRaised handlerValue stateValues receive
    GrinForeignCallExpr foreignCall arguments -> do
      argumentValues <- mapM (materializeValue env) arguments
      continue =<< executeForeignCall foreignCall argumentValues
  where
    rejectCpsExpression = throwInterpret (InterpretCpsExpression expr)

evalScheduledPrimitive :: Text -> [RuntimeValue] -> ScheduledContinuation -> EvalM [RuntimeValue]
evalScheduledPrimitive "fork#" [action] continue = do
  threadId <- allocateCell HeapThread
  enqueueThread
    ( applyScheduledValue action [] (const scheduleNextThread)
        `catchE` finishChild
    )
  continue [threadId]
evalScheduledPrimitive "yield#" [] continue = do
  enqueueThread (continue [])
  scheduleNextThread
evalScheduledPrimitive "awaitIO#" [RuntimeIORequest request] continue = do
  completeIORequest request
  continue []
evalScheduledPrimitive name arguments continue =
  continue =<< evalPrimitive name arguments

finishChild :: EvalFailure -> EvalM [RuntimeValue]
finishChild failure =
  case failure of
    -- An uncaught Haskell exception terminates only the forked thread.
    EvalRaised _ -> scheduleNextThread
    EvalInterpret _ -> throwE failure

enqueueThread :: EvalM [RuntimeValue] -> EvalM ()
enqueueThread action =
  modifyMachine $ \machine ->
    machine {machineRunQueue = machineRunQueue machine |> ThreadAction action}

scheduleNextThread :: EvalM [RuntimeValue]
scheduleNextThread = do
  queue <- getsMachine machineRunQueue
  case Seq.viewl queue of
    EmptyL -> throwInterpret InterpretNoRunnableThreads
    ThreadAction action :< remaining -> do
      modifyMachine $ \machine -> machine {machineRunQueue = remaining}
      action

callScheduledFunction :: FunctionName -> [RuntimeValue] -> ScheduledContinuation -> EvalM [RuntimeValue]
callScheduledFunction functionName arguments continue = do
  function <- lookupFunction functionName
  let parameters = grinFunctionParameters function
  if length parameters == length arguments
    then evalScheduledExpr (Map.fromList (zip parameters arguments)) (grinFunctionBody function) continue
    else throwInterpret (InterpretFunctionArity functionName (length parameters) (length arguments))

applyScheduledValue :: RuntimeValue -> [RuntimeValue] -> ScheduledContinuation -> EvalM [RuntimeValue]
applyScheduledValue function arguments continue = do
  (tag, fields) <- appliedNode function
  case tag of
    GrinClosure functionName remainingLayouts ->
      case remainingLayouts of
        [] -> throwInterpret (InterpretFunctionArity functionName 0 1)
        layout : rest ->
          let normalizedArguments
                | layout == [BoxedRep Lifted], null arguments = [RuntimeStateToken]
                | otherwise = arguments
              appliedFields = fields <> normalizedArguments
           in case rest of
                [] -> callScheduledFunction functionName appliedFields continue
                _ -> do
                  applied <- allocateCell (HeapValue (RuntimeNode (GrinClosure functionName rest) appliedFields))
                  continue [applied]
    GrinConstructor name remaining ->
      case compare remaining 1 of
        GT -> do
          applied <- allocateCell (HeapValue (RuntimeNode (GrinConstructor name (remaining - 1)) (fields <> arguments)))
          continue [applied]
        EQ -> do
          applied <- allocateCell (HeapValue (RuntimeNode (GrinConstructor name 0) (fields <> arguments)))
          continue [applied]
        LT -> throwInterpret (InterpretConstructorArity name 0 1)
    GrinThunk _ -> throwInterpret (InterpretApplyNonFunction function)

forceScheduledValue :: RuntimeValue -> (RuntimeValue -> EvalM [RuntimeValue]) -> EvalM [RuntimeValue]
forceScheduledValue value continue =
  case value of
    RuntimeLocation location -> forceScheduledLocation location continue
    _ -> continue value

forceScheduledLocation :: Int -> (RuntimeValue -> EvalM [RuntimeValue]) -> EvalM [RuntimeValue]
forceScheduledLocation location continue = do
  cell <- readCell location
  case cell of
    HeapSuspended functionName fields -> do
      function <- lookupFunction functionName
      let resultRep = grinFunctionResultRep function
      if isLiftedRuntimeRep resultRep
        then pure ()
        else throwInterpret (InterpretInvalidThunkResultRep functionName resultRep)
      writeCell location HeapBlackhole
      callScheduledFunction functionName fields (updateThunk cell)
        `catchE` \failure -> writeCell location cell >> throwE failure
    HeapValue (RuntimeLocation target) -> forceScheduledLocation target continue
    HeapValue _ -> continue (RuntimeLocation location)
    HeapRaised exception -> throwE (EvalRaised exception)
    HeapBlackhole -> throwInterpret (InterpretBlackhole location)
    HeapThread -> continue (RuntimeLocation location)
  where
    updateThunk original values =
      case values of
        [value] -> do
          writeCell location (HeapValue value)
          forceScheduledLocation location continue
        _ -> do
          writeCell location original
          throwInterpret (InterpretInvalidThunkResult values)

matchScheduledAlternative :: Env -> RuntimeValue -> [GrinAlt] -> ScheduledContinuation -> EvalM [RuntimeValue]
matchScheduledAlternative env value alternatives continue = do
  inspected <- inspectCaseValue value
  go inspected alternatives
  where
    go _ [] = throwInterpret (InterpretNoMatchingAlternative value)
    go inspected (alt : rest) =
      case matchAlt value inspected alt of
        Just bindings -> evalScheduledExpr (bindings `Map.union` env) (grinAltRhs alt) continue
        Nothing -> go inspected rest

handleScheduledRaised :: RuntimeValue -> [RuntimeValue] -> ScheduledContinuation -> EvalFailure -> EvalM [RuntimeValue]
handleScheduledRaised handler state continue failure =
  case failure of
    EvalRaised exception ->
      applyScheduledValue handler [exception] $ \values -> do
        handlerWithException <- expectSingle values
        applyScheduledValue handlerWithException state continue
    EvalInterpret err -> throwE (EvalInterpret err)

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
        HeapThread -> pure (RuntimeLocation location)
    other -> throwInterpret (InterpretExpectedLocation other)

updateValue :: RuntimeValue -> RuntimeValue -> EvalM RuntimeValue
updateValue pointer value =
  if isLiftedRuntimeValue value
    then case pointer of
      RuntimeLocation location -> writeCell location (HeapValue value) >> pure value
      other -> throwInterpret (InterpretExpectedLocation other)
    else throwInterpret (InterpretInvalidUpdateValue value)

forceValue :: RuntimeValue -> EvalM RuntimeValue
forceValue value = expectSingle =<< forceScheduledValue value (pure . (: []))

applyValue :: RuntimeValue -> [RuntimeValue] -> EvalM [RuntimeValue]
applyValue function arguments = applyScheduledValue function arguments pure

appliedNode :: RuntimeValue -> EvalM (GrinNodeTag, [RuntimeValue])
appliedNode function =
  case function of
    RuntimeLocation location -> do
      cell <- readCell location
      case cell of
        HeapValue (RuntimeNode tag fields) -> pure (tag, fields)
        _ -> throwInterpret (InterpretApplyNonFunction function)
    _ -> throwInterpret (InterpretApplyNonFunction function)

callFunction :: FunctionName -> [RuntimeValue] -> EvalM [RuntimeValue]
callFunction functionName arguments = callScheduledFunction functionName arguments pure

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
    RuntimeAddress {} -> False
    RuntimeIOHandle {} -> False
    RuntimeIOBuffer {} -> False
    RuntimeIORequest {} -> False
    RuntimeNode {} -> True
    RuntimeLocation {} -> True
    RuntimeMutVar {} -> False
    RuntimeStateToken -> False

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
  charValue <- expectCharPrimitiveArgument "charToInt#" value
  pure [RuntimeLit (GrinLitInt IntRep (fromIntegral (Char.ord charValue)))]
evalPrimitive "intToChar#" [value] = do
  intValue <- expectIntPrimitiveArgument "intToChar#" value
  if intValue >= 0 && intValue <= 0x10ffff
    then pure [RuntimeLit (GrinLitChar WordRep (Char.chr (fromIntegral intValue)))]
    else throwInterpret (InterpretPrimitiveTypeError "intToChar#" (RuntimeLit (GrinLitInt IntRep intValue)))
evalPrimitive "realWorld#" [] = pure []
evalPrimitive "raise#" [exception] =
  throwE (EvalRaised exception)
evalPrimitive "catch#" [action, handler] =
  applyValue action [] `catchE` handleRaised handler []
evalPrimitive "newMutVar#" [initialValue] = do
  mutVar <- GrinMutVar <$> liftEvalIO (newIORef initialValue)
  pure [RuntimeMutVar mutVar]
evalPrimitive "readMutVar#" [mutVar] = do
  GrinMutVar reference <- expectMutVarPrimitiveArgument "readMutVar#" mutVar
  value <- liftEvalIO (readIORef reference)
  pure [value]
evalPrimitive "writeMutVar#" [mutVar, value] = do
  GrinMutVar reference <- expectMutVarPrimitiveArgument "writeMutVar#" mutVar
  liftEvalIO (writeIORef reference value)
  pure []
evalPrimitive name arguments =
  throwInterpret (InterpretPrimitiveArity name (length arguments))

evalIntPrimitive :: Text -> (Integer -> Integer -> Integer) -> RuntimeValue -> RuntimeValue -> EvalM [RuntimeValue]
evalIntPrimitive name operation left right = do
  leftInt <- expectIntPrimitiveArgument name left
  rightInt <- expectIntPrimitiveArgument name right
  pure [RuntimeLit (GrinLitInt IntRep (operation leftInt rightInt))]

expectIntPrimitiveArgument :: Text -> RuntimeValue -> EvalM Integer
expectIntPrimitiveArgument name value =
  case value of
    RuntimeLit (GrinLitInt _ intValue) -> pure intValue
    other -> throwInterpret (InterpretPrimitiveTypeError name other)

expectCharPrimitiveArgument :: Text -> RuntimeValue -> EvalM Char
expectCharPrimitiveArgument name value =
  case value of
    RuntimeLit (GrinLitChar _ charValue) -> pure charValue
    other -> throwInterpret (InterpretPrimitiveTypeError name other)

expectMutVarPrimitiveArgument :: Text -> RuntimeValue -> EvalM GrinMutVar
expectMutVarPrimitiveArgument name value =
  case value of
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
callForeign foreignCall arguments
  | symbol == "aihc_io_stdin",
    [] <- arguments =
      pure (RuntimeIOHandle (GrinIOHandle 0 stdin))
  | symbol == "aihc_io_stdout",
    [] <- arguments =
      pure (RuntimeIOHandle (GrinIOHandle 1 stdout))
  | symbol == "aihc_io_buffer_new",
    [capacityValue] <- arguments = do
      capacity <- expectInt32 symbol capacityValue
      if capacity < 0
        then throwInterpret (InterpretInvalidIOBufferRange symbol 0 capacity 0)
        else RuntimeIOBuffer . GrinIOBuffer <$> liftEvalIO (newIORef (BS.replicate (fromInteger capacity) 0))
  | symbol == "aihc_io_buffer_get",
    [bufferValue, indexValue] <- arguments = do
      buffer@(GrinIOBuffer reference) <- expectIOBuffer symbol bufferValue
      index <- expectInt32 symbol indexValue
      (checkedIndex, _) <- checkedIOBufferRange symbol buffer index 1
      bytes <- liftEvalIO (readIORef reference)
      pure (RuntimeLit (GrinLitInt Int32Rep (toInteger (BS.index bytes checkedIndex))))
  | symbol == "aihc_io_buffer_set",
    [bufferValue, indexValue, byteValue] <- arguments = do
      buffer@(GrinIOBuffer reference) <- expectIOBuffer symbol bufferValue
      index <- expectInt32 symbol indexValue
      byte <- expectInt32 symbol byteValue
      (checkedIndex, _) <- checkedIOBufferRange symbol buffer index 1
      liftEvalIO $ modifyIORef' reference $ \bytes -> BS.take checkedIndex bytes <> BS.singleton (fromInteger byte) <> BS.drop (checkedIndex + 1) bytes
      pure (RuntimeLit (GrinLitInt Int32Rep 0))
  | symbol == "aihc_io_buffer_copy_from_addr",
    [sourceValue, bufferValue, offsetValue, lengthValue] <- arguments = do
      buffer@(GrinIOBuffer reference) <- expectIOBuffer symbol bufferValue
      offset <- expectInt32 symbol offsetValue
      byteCount <- expectInt32 symbol lengthValue
      (checkedOffset, checkedLength) <- checkedIOBufferRange symbol buffer offset byteCount
      sourceBytes <- readAddressBytes symbol checkedLength sourceValue
      liftEvalIO $ modifyIORef' reference $ \bytes -> BS.take checkedOffset bytes <> sourceBytes <> BS.drop (checkedOffset + checkedLength) bytes
      pure (RuntimeLit (GrinLitInt Int32Rep 0))
  | symbol == "aihc_io_submit_read",
    [handleValue, bufferValue, offsetValue, lengthValue] <- arguments = do
      handle <- expectIOHandle symbol handleValue
      buffer <- expectIOBuffer symbol bufferValue
      offset <- expectInt32 symbol offsetValue
      byteCount <- expectInt32 symbol lengthValue
      (checkedOffset, checkedLength) <- checkedIOBufferRange symbol buffer offset byteCount
      RuntimeIORequest . GrinIORequest <$> liftEvalIO (newIORef (GrinIOSubmitted (GrinRead handle buffer checkedOffset checkedLength)))
  | symbol == "aihc_io_submit_write",
    [handleValue, bufferValue, offsetValue, lengthValue] <- arguments = do
      handle <- expectIOHandle symbol handleValue
      buffer <- expectIOBuffer symbol bufferValue
      offset <- expectInt32 symbol offsetValue
      byteCount <- expectInt32 symbol lengthValue
      (checkedOffset, checkedLength) <- checkedIOBufferRange symbol buffer offset byteCount
      RuntimeIORequest . GrinIORequest <$> liftEvalIO (newIORef (GrinIOSubmitted (GrinWrite handle buffer checkedOffset checkedLength)))
  | symbol == "aihc_io_take_result",
    [request] <- arguments =
      takeIOResult symbol request
  | otherwise = do
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
        GrinForeignAddr ->
          RuntimeAddress <$> liftEvalIO (callFFI functionPointer (retPtr retVoid) marshalledArguments)
  where
    symbol = grinForeignCallSymbol foreignCall

expectIOHandle :: Text -> RuntimeValue -> EvalM GrinIOHandle
expectIOHandle symbol value =
  case value of
    RuntimeIOHandle handle -> pure handle
    _ -> throwInterpret (InterpretForeignTypeError symbol value)

expectIOBuffer :: Text -> RuntimeValue -> EvalM GrinIOBuffer
expectIOBuffer symbol value =
  case value of
    RuntimeIOBuffer buffer -> pure buffer
    _ -> throwInterpret (InterpretForeignTypeError symbol value)

checkedIOBufferRange :: Text -> GrinIOBuffer -> Integer -> Integer -> EvalM (Int, Int)
checkedIOBufferRange symbol (GrinIOBuffer reference) offset byteCount = do
  capacity <- BS.length <$> liftEvalIO (readIORef reference)
  if offset < 0 || byteCount < 0 || offset > toInteger capacity || byteCount > toInteger capacity - offset
    then throwInterpret (InterpretInvalidIOBufferRange symbol offset byteCount capacity)
    else pure (fromInteger offset, fromInteger byteCount)

readAddressBytes :: Text -> Int -> RuntimeValue -> EvalM BS.ByteString
readAddressBytes symbol byteCount value =
  case value of
    RuntimeLit (GrinLitAddr bytes) ->
      liftEvalIO (withArray0 0 (BS.unpack bytes) (fmap BS.pack . peekArray byteCount))
    RuntimeAddress pointer -> liftEvalIO (BS.pack <$> peekArray byteCount (castPtr pointer))
    other -> throwInterpret (InterpretForeignTypeError symbol other)

completeIORequest :: GrinIORequest -> EvalM ()
completeIORequest (GrinIORequest reference) = do
  state <- liftEvalIO (readIORef reference)
  case state of
    GrinIOSubmitted operation -> do
      result <- performIOOperation operation
      liftEvalIO (writeIORef reference (GrinIOCompleted result))
    GrinIOCompleted {} -> pure ()
    GrinIOConsumed -> throwInterpret (InterpretPrimitiveTypeError "awaitIO#" (RuntimeIORequest (GrinIORequest reference)))

performIOOperation :: GrinIOOperation -> EvalM Integer
performIOOperation operation =
  case operation of
    GrinRead (GrinIOHandle _ handle) (GrinIOBuffer reference) offset byteCount -> do
      input <- liftEvalIO (BS.hGet handle byteCount)
      liftEvalIO $ modifyIORef' reference $ \buffer -> BS.take offset buffer <> input <> BS.drop (offset + BS.length input) buffer
      pure (toInteger (BS.length input))
    GrinWrite (GrinIOHandle _ handle) (GrinIOBuffer reference) offset byteCount -> do
      buffer <- liftEvalIO (readIORef reference)
      liftEvalIO (BS.hPut handle (BS.take byteCount (BS.drop offset buffer)) >> hFlush handle)
      pure (toInteger byteCount)

takeIOResult :: Text -> RuntimeValue -> EvalM RuntimeValue
takeIOResult symbol value =
  case value of
    RuntimeIORequest (GrinIORequest reference) -> do
      state <- liftEvalIO (readIORef reference)
      case state of
        GrinIOCompleted result -> do
          liftEvalIO (writeIORef reference GrinIOConsumed)
          pure (RuntimeLit (GrinLitInt Int32Rep result))
        _ -> throwInterpret (InterpretForeignTypeError symbol value)
    _ -> throwInterpret (InterpretForeignTypeError symbol value)

marshalForeignArgument :: Text -> GrinForeignType -> RuntimeValue -> EvalM Arg
marshalForeignArgument symbol GrinForeignInt32 argument = do
  argumentValue <- expectInt32 symbol argument
  pure (argCInt (CInt (fromInteger argumentValue)))
marshalForeignArgument symbol GrinForeignWord64 argument = do
  argumentValue <- expectWord64 symbol argument
  pure (argWord64 (fromInteger argumentValue :: Word64))
marshalForeignArgument symbol GrinForeignAddr argument =
  case argument of
    RuntimeLit (GrinLitAddr value) -> do
      pointer <- liftEvalIO (newArray0 0 (BS.unpack value))
      pure (argPtr pointer)
    RuntimeAddress pointer -> pure (argPtr pointer)
    other -> throwInterpret (InterpretForeignTypeError symbol other)

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

expectInt32 :: Text -> RuntimeValue -> EvalM Integer
expectInt32 symbol value =
  case value of
    RuntimeLit (GrinLitInt Int32Rep intValue) -> pure intValue
    other -> throwInterpret (InterpretForeignTypeError symbol other)

expectWord64 :: Text -> RuntimeValue -> EvalM Integer
expectWord64 symbol value =
  case value of
    RuntimeLit (GrinLitInt Word64Rep intValue) -> pure intValue
    other -> throwInterpret (InterpretForeignTypeError symbol other)

runIOValue :: RuntimeValue -> EvalM RuntimeValue
runIOValue action = do
  results <- applyValue action []
  case results of
    [ioResult] -> pure ioResult
    _ -> throwInterpret (InterpretResultArity 1 (length results))

inspectCaseValue :: RuntimeValue -> EvalM RuntimeValue
inspectCaseValue value =
  case value of
    RuntimeLocation location -> do
      cell <- readCell location
      case cell of
        HeapValue node@RuntimeNode {} -> pure node
        _ -> throwInterpret (InterpretNoMatchingAlternative value)
    _ -> pure value

matchAlt :: RuntimeValue -> RuntimeValue -> GrinAlt -> Maybe Env
matchAlt original inspected alt =
  case (grinAltCon alt, inspected) of
    (GrinDefaultAlt, _) ->
      Just (Map.fromList [(var, original) | var <- grinAltBinders alt])
    (GrinLitAlt expected, RuntimeLit actual)
      | expected == actual -> Just Map.empty
    (GrinDataAlt expected, RuntimeNode (GrinConstructor actual 0) fields)
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
  exposed <- exposeWhnfValue value
  case exposed of
    RuntimeLit literal -> pure (renderLiteral literal)
    RuntimeAddress address -> pure (T.pack (show address))
    RuntimeIOHandle {} -> pure "<io-handle>"
    RuntimeIOBuffer {} -> pure "<io-buffer>"
    RuntimeIORequest {} -> pure "<io-request>"
    RuntimeNode (GrinConstructor "C#" 0) [char] -> renderBoxedChar char
    RuntimeNode (GrinConstructor name 0) [] -> pure name
    RuntimeNode (GrinConstructor name 0) arguments
      | isTupleConstructor name (length arguments) -> do
          renderedArguments <- mapM renderRawArgument arguments
          pure ("(" <> T.intercalate "," renderedArguments <> ")")
    RuntimeNode (GrinConstructor name 0) arguments -> do
      renderedArguments <- mapM renderRawArgument arguments
      pure (T.unwords (name : renderedArguments))
    RuntimeNode GrinConstructor {} _ -> pure "<function>"
    RuntimeNode GrinClosure {} _ -> pure "<function>"
    RuntimeNode GrinThunk {} _ -> pure "<thunk>"
    RuntimeLocation location -> throwInterpret (InterpretInvalidLocation location)
    RuntimeMutVar {} -> pure "<mutvar>"
    RuntimeStateToken -> pure "<state>"

renderRawArgument :: RuntimeValue -> EvalM Text
renderRawArgument value = do
  exposed <- exposeWhnfValue value
  rendered <- renderRawValueM exposed
  pure $
    case exposed of
      RuntimeNode (GrinConstructor name 0) arguments
        | isTupleConstructor name (length arguments) -> rendered
      RuntimeNode (GrinConstructor "C#" 0) [_] -> rendered
      RuntimeNode (GrinConstructor _ 0) (_ : _) -> "(" <> rendered <> ")"
      _ -> rendered

exposeWhnfValue :: RuntimeValue -> EvalM RuntimeValue
exposeWhnfValue value = do
  forced <- forceValue value
  case forced of
    RuntimeLocation _ -> fetchValue forced
    _ -> pure forced

renderLiteral :: GrinLiteral -> Text
renderLiteral literal =
  case literal of
    GrinLitInt _ value -> T.pack (show value)
    GrinLitChar _ value -> T.pack (show value) <> "#"
    GrinLitString value -> T.pack (show (T.unpack value))
    GrinLitAddr value -> T.pack (show (map (Char.chr . fromIntegral) (BS.unpack value))) <> "#"

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
  let indirectionTargets =
        [ target
        | HeapValue (RuntimeLocation target) <- IntMap.elems source
        ]
      initial =
        SnapshotBuild
          { snapshotBuildSource = source,
            snapshotBuildValueSources =
              [ (value, location)
              | (location, HeapValue value@RuntimeNode {}) <- IntMap.toAscList source,
                location `elem` indirectionTargets
              ],
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
    HeapValue (RuntimeLocation sourceLocation) -> SnapshotIndirection <$> snapshotLocation sourceLocation
    HeapValue value -> SnapshotValue <$> snapshotStoredValue value
    HeapRaised exception -> SnapshotRaised <$> snapshotRuntimeValue exception
    HeapBlackhole -> pure SnapshotBlackhole
    HeapThread -> pure SnapshotThreadId

snapshotRuntimeValue :: RuntimeValue -> State SnapshotBuild SnapshotValue
snapshotRuntimeValue value =
  case value of
    RuntimeLit literal -> pure (SnapshotLiteral literal)
    RuntimeAddress {} -> pure SnapshotAddress
    RuntimeIOHandle {} -> pure SnapshotAddress
    RuntimeIOBuffer {} -> pure SnapshotAddress
    RuntimeIORequest {} -> pure SnapshotAddress
    RuntimeNode {} -> do
      valueSources <- gets snapshotBuildValueSources
      case lookup value valueSources of
        Just sourceLocation -> SnapshotLocation <$> snapshotLocation sourceLocation
        Nothing -> snapshotStoredValue value
    RuntimeLocation sourceLocation -> SnapshotLocation <$> snapshotLocation sourceLocation
    RuntimeMutVar {} -> pure SnapshotMutVar
    RuntimeStateToken -> pure SnapshotStateToken

-- Heap-cell payloads define locations and therefore render their node inline.
-- The same node encountered elsewhere is rendered as a pointer back to this
-- owning value cell.
snapshotStoredValue :: RuntimeValue -> State SnapshotBuild SnapshotValue
snapshotStoredValue value =
  case value of
    RuntimeNode tag fields -> SnapshotNode tag <$> mapM snapshotRuntimeValue fields
    _ -> snapshotRuntimeValue value

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
