{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reference interpreter for strict GRIN programs.
module Aihc.Grin.Interpret
  ( InterpretError (..),
    RuntimeValue (..),
    interpretProgramBinding,
  )
where

import Aihc.Grin.Cps
import Aihc.Grin.Syntax
import Aihc.Tc.Prim (PrimOp, primOpArity, primOpName)
import Aihc.Tc.Types (RuntimeRep (..))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, gets, modify', runStateT)
import Data.Char qualified as Char
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.List (insertBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Foreign.C.Types (CInt (..))
import Foreign.LibFFI (Arg, argCInt, argWord64, callFFI, retCInt, retWord64)
import Foreign.Ptr (FunPtr)
import GHC.Clock (getMonotonicTimeNSec)
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
  | InterpretDeadlock
  | InterpretRaisedException !Text
  deriving (Eq, Show)

data RuntimeValue
  = RuntimeLit !GrinLiteral
  | RuntimeNode !GrinNodeTag ![RuntimeValue]
  | RuntimeLocation !Int
  | RuntimeMutVar !GrinMutVar
  | RuntimeMVar !Int
  | RuntimeThreadId !Int
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
    machineNextLocation :: !Int,
    machineRunnable :: !(Seq (SchedulerM ())),
    machineMVars :: !(IntMap MVarState),
    machineNextMVar :: !Int,
    machineNextThreadId :: !Int,
    machineTimers :: ![Timer],
    machineNextTimerSequence :: !Word64,
    machineResult :: !(Maybe (Either EvalFailure Text))
  }

data EvalFailure
  = EvalInterpret !InterpretError
  | EvalRaised !RuntimeValue

type SchedulerM = StateT Machine IO

newtype EvalM a = EvalM
  { runEvalM :: (Either EvalFailure a -> SchedulerM ()) -> SchedulerM ()
  }

instance Functor EvalM where
  fmap function action =
    EvalM $ \continuation ->
      runEvalM action $ \case
        Left failure -> continuation (Left failure)
        Right value -> continuation (Right (function value))

instance Applicative EvalM where
  pure value = EvalM (\continuation -> continuation (Right value))
  function <*> argument = do
    applied <- function
    applied <$> argument

instance Monad EvalM where
  action >>= next =
    EvalM $ \continuation ->
      runEvalM action $ \case
        Left failure -> continuation (Left failure)
        Right value -> runEvalM (next value) continuation

instance MonadFail EvalM where
  fail message = error ("GRIN interpreter invariant failed: " <> message)

data MVarState = MVarState
  { mvarValue :: !(Maybe RuntimeValue),
    mvarTakers :: !(Seq Taker),
    mvarPutters :: !(Seq Putter)
  }

data Taker
  = Taker
      !RuntimeValue
      !(Either EvalFailure RuntimeValue -> SchedulerM ())

data Putter
  = Putter
      !RuntimeValue
      !RuntimeValue
      !(Either EvalFailure RuntimeValue -> SchedulerM ())

data Timer = Timer
  { timerDeadline :: !Word64,
    timerSequence :: !Word64,
    timerAction :: !(SchedulerM ())
  }

-- | Interpret and render a named top-level binding using the raw constructor
-- representation shared by the compiler pipeline evaluation fixtures.
interpretProgramBinding :: Text -> GrinProgram -> IO (Either InterpretError Text)
interpretProgramBinding name program = do
  let machine = initialMachine program
  (_, finalMachine) <-
    runStateT
      (enqueueAction (runEvalM action finishMain) >> runScheduler)
      machine
  case machineResult finalMachine of
    Just (Right rendered) -> pure (Right rendered)
    Just (Left (EvalInterpret err)) -> pure (Left err)
    Just (Left (EvalRaised exception)) ->
      pure (Left (InterpretRaisedException (T.pack (show exception))))
    Nothing -> pure (Left InterpretDeadlock)
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
    finishMain result = modify' (\machine -> machine {machineResult = Just result})

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
      machineNextLocation = length cafs,
      machineRunnable = Seq.empty,
      machineMVars = IntMap.empty,
      machineNextMVar = 0,
      machineNextThreadId = 1,
      machineTimers = [],
      machineNextTimerSequence = 0,
      machineResult = Nothing
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
            [ (grinVarName var, RuntimeNode (GrinPrimitive primOp) [])
            | (var, primOp) <- grinPrimitives program
            ],
          Map.fromList
            [ (constructor, RuntimeNode (GrinConstructor constructor) [])
            | constructor <- map fst builtinConstructors <> map fst (grinConstructors program)
            ]
        ]

enqueueAction :: SchedulerM () -> SchedulerM ()
enqueueAction action =
  modify' (\machine -> machine {machineRunnable = machineRunnable machine |> action})

runScheduler :: SchedulerM ()
runScheduler = do
  completed <- gets machineResult
  case completed of
    Just _ -> pure ()
    Nothing -> do
      now <- lift getMonotonicTimeNSec
      drainDueTimers now
      runnable <- gets machineRunnable
      case Seq.viewl runnable of
        action Seq.:< rest -> do
          modify' (\machine -> machine {machineRunnable = rest})
          action
          runScheduler
        Seq.EmptyL -> do
          timers <- gets machineTimers
          case timers of
            [] ->
              modify'
                ( \machine ->
                    machine
                      { machineResult = Just (Left (EvalInterpret InterpretDeadlock))
                      }
                )
            Timer deadline _ _ : _ -> do
              waitForDeadline now deadline
              runScheduler

drainDueTimers :: Word64 -> SchedulerM ()
drainDueTimers now = do
  timers <- gets machineTimers
  let (due, pending) = span ((<= now) . timerDeadline) timers
  modify'
    ( \machine ->
        machine
          { machineTimers = pending,
            machineRunnable = foldl (|>) (machineRunnable machine) (map timerAction due)
          }
    )

waitForDeadline :: Word64 -> Word64 -> SchedulerM ()
waitForDeadline now deadline
  | deadline <= now = pure ()
  | otherwise =
      lift (threadDelay (fromIntegral (min (fromIntegral (maxBound :: Int)) microseconds)))
  where
    nanoseconds = deadline - now
    microseconds = max 1 ((nanoseconds + 999) `div` 1000)

insertTimer :: Timer -> [Timer] -> [Timer]
insertTimer = insertBy compareTimer
  where
    compareTimer left right =
      compare
        (timerDeadline left, timerSequence left)
        (timerDeadline right, timerSequence right)

evalCpsExpr :: Env -> CpsExpr -> EvalM RuntimeValue
evalCpsExpr env expression =
  case expression of
    CpsContinue continuation value ->
      evalValue env value >>= continueCps env continuation
    CpsOperation operation continuation ->
      evalCpsOperation env operation >>= continueCps env continuation
    CpsStoreRec bindings body -> do
      locations <- mapM (const (allocateLocation HeapBlackhole)) bindings
      let recursiveBindings = zip (map fst bindings) (map RuntimeLocation locations)
          recursiveEnv = Map.fromList recursiveBindings `Map.union` env
      mapM_
        (\((_, node), location) -> writeCell location (HeapSuspended recursiveEnv node))
        (zip bindings locations)
      evalCpsExpr recursiveEnv body
    CpsCase scrutinee binder alternatives -> do
      value <- evalValue env scrutinee >>= forceValue
      matchCpsAlternative (Map.insert binder value env) value alternatives

continueCps :: Env -> CpsContinuation -> RuntimeValue -> EvalM RuntimeValue
continueCps env continuation value =
  case continuation of
    CpsReturn -> pure value
    CpsBind binder body -> evalCpsExpr (Map.insert binder value env) body

evalCpsOperation :: Env -> CpsOperation -> EvalM RuntimeValue
evalCpsOperation env operation =
  case operation of
    CpsStore node -> allocateCell (HeapSuspended env node)
    CpsFetch _ pointer -> evalValue env pointer >>= fetchValue
    CpsUpdate pointer value -> do
      pointerValue <- evalValue env pointer
      updatedValue <- evalValue env value
      updateValue pointerValue updatedValue
    CpsEval _ value -> evalValue env value >>= forceValue
    CpsApply _ function argument -> do
      functionValue <- evalValue env function
      argumentValue <- evalValue env argument
      applyValue functionValue argumentValue
    CpsDictSelect _ dictionary index -> do
      dictionaryValue <- evalValue env dictionary >>= forceValue
      case dictionaryValue of
        RuntimeNode GrinDictionary fields
          | index >= 0,
            index < length fields ->
              forceValue (fields !! index)
          | otherwise -> throwInterpret (InterpretInvalidDictSelect dictionaryValue index)
        other -> throwInterpret (InterpretInvalidDictSelect other index)
    CpsThrow exception -> do
      exceptionValue <- evalValue env exception >>= forceValue
      throwE (EvalRaised exceptionValue)
    CpsCatch _ action handler state -> do
      actionValue <- evalValue env action
      handlerValue <- evalValue env handler
      stateValue <- evalValue env state
      applyValue actionValue stateValue `catchE` handleRaised handlerValue stateValue
    CpsScheduler _ schedulerOp arguments -> do
      argumentValues <- mapM (evalValue env) arguments
      evalScheduler schedulerOp argumentValues
    CpsForeignCall foreignCall arguments -> do
      argumentValues <- mapM (evalValue env) arguments
      executeForeignCall foreignCall argumentValues

handleRaised :: RuntimeValue -> RuntimeValue -> EvalFailure -> EvalM RuntimeValue
handleRaised handler state failure =
  case failure of
    EvalRaised exception -> do
      handlerWithException <- applyValue handler exception
      applyValue handlerWithException state
    EvalInterpret err -> throwE (EvalInterpret err)

evalScheduler :: SchedulerPrimOp -> [RuntimeValue] -> EvalM RuntimeValue
evalScheduler SchedulerFork [action, state] =
  EvalM $ \parentContinuation -> do
    threadId <- gets machineNextThreadId
    modify' (\machine -> machine {machineNextThreadId = threadId + 1})
    enqueueAction
      ( runEvalM
          (applyValue action state)
          (const (pure ()))
      )
    parentContinuation
      ( Right
          ( RuntimeNode
              (GrinConstructor "(#,#)")
              [state, RuntimeThreadId threadId]
          )
      )
evalScheduler SchedulerYield [state] =
  EvalM $ \continuation -> enqueueAction (continuation (Right state))
evalScheduler SchedulerNewMVar [state] =
  EvalM $ \continuation -> do
    identifier <- gets machineNextMVar
    modify'
      ( \machine ->
          machine
            { machineMVars =
                IntMap.insert identifier emptyMVarState (machineMVars machine),
              machineNextMVar = identifier + 1
            }
      )
    continuation
      ( Right
          ( RuntimeNode
              (GrinConstructor "(#,#)")
              [state, RuntimeMVar identifier]
          )
      )
evalScheduler SchedulerTakeMVar [RuntimeMVar identifier, state] =
  EvalM $ \continuation -> do
    mvar <- lookupMVar identifier
    case mvarValue mvar of
      Nothing ->
        updateMVar
          identifier
          mvar {mvarTakers = mvarTakers mvar |> Taker state continuation}
      Just value ->
        case Seq.viewl (mvarPutters mvar) of
          Putter nextValue putterState resumePutter Seq.:< remainingPutters -> do
            updateMVar
              identifier
              mvar
                { mvarValue = Just nextValue,
                  mvarPutters = remainingPutters
                }
            enqueueAction (resumePutter (Right putterState))
            continuation (Right (stateValueTuple state value))
          Seq.EmptyL -> do
            updateMVar identifier mvar {mvarValue = Nothing}
            continuation (Right (stateValueTuple state value))
evalScheduler SchedulerPutMVar [RuntimeMVar identifier, value, state] =
  EvalM $ \continuation -> do
    mvar <- lookupMVar identifier
    case Seq.viewl (mvarTakers mvar) of
      Taker takerStateValue resumeTaker Seq.:< remainingTakers -> do
        updateMVar identifier mvar {mvarTakers = remainingTakers}
        enqueueAction (resumeTaker (Right (stateValueTuple takerStateValue value)))
        continuation (Right state)
      Seq.EmptyL ->
        case mvarValue mvar of
          Nothing -> do
            updateMVar identifier mvar {mvarValue = Just value}
            continuation (Right state)
          Just _ ->
            updateMVar
              identifier
              mvar {mvarPutters = mvarPutters mvar |> Putter value state continuation}
evalScheduler SchedulerDelay [RuntimeLit (GrinLitInt _ microseconds), state]
  | microseconds <= 0 = pure state
  | otherwise =
      EvalM $ \continuation -> do
        now <- lift getMonotonicTimeNSec
        sequenceNumber <- gets machineNextTimerSequence
        let duration = saturatingMicrosecondsToNanoseconds microseconds
            deadline = saturatingAdd now duration
            timer = Timer deadline sequenceNumber (continuation (Right state))
        modify'
          ( \machine ->
              machine
                { machineTimers = insertTimer timer (machineTimers machine),
                  machineNextTimerSequence = sequenceNumber + 1
                }
          )
evalScheduler schedulerOp arguments =
  throwInterpret
    ( InterpretPrimitiveArity
        (T.pack (show schedulerOp))
        (length arguments)
    )

emptyMVarState :: MVarState
emptyMVarState = MVarState Nothing Seq.empty Seq.empty

lookupMVar :: Int -> SchedulerM MVarState
lookupMVar identifier = do
  mvars <- gets machineMVars
  case IntMap.lookup identifier mvars of
    Just mvar -> pure mvar
    Nothing -> error ("GRIN scheduler referenced unknown MVar " <> show identifier)

updateMVar :: Int -> MVarState -> SchedulerM ()
updateMVar identifier mvar =
  modify'
    ( \machine ->
        machine {machineMVars = IntMap.insert identifier mvar (machineMVars machine)}
    )

stateValueTuple :: RuntimeValue -> RuntimeValue -> RuntimeValue
stateValueTuple state value =
  RuntimeNode (GrinConstructor "(#,#)") [state, value]

saturatingMicrosecondsToNanoseconds :: Integer -> Word64
saturatingMicrosecondsToNanoseconds microseconds =
  fromInteger (min (toInteger (maxBound :: Word64)) (microseconds * 1000))

saturatingAdd :: Word64 -> Word64 -> Word64
saturatingAdd left right
  | maxBound - left < right = maxBound
  | otherwise = left + right

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
    RuntimeNode (GrinPrimitive primOp) []
      | primOpArity primOp == 0 -> evalPrimitive (primOpName primOp) []
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
    RuntimeNode (GrinPrimitive primOp) fields ->
      applyPrimitive primOp (fields <> [argument])
    other -> throwInterpret (InterpretApplyNonFunction other)

callFunction :: FunctionName -> [RuntimeValue] -> EvalM RuntimeValue
callFunction functionName arguments = do
  functions <- getsMachine machineFunctions
  case Map.lookup functionName functions of
    Nothing -> throwInterpret (InterpretUnknownFunction functionName)
    Just function ->
      let parameters = grinFunctionParameters function
       in if length parameters == length arguments
            then evalCpsExpr (Map.fromList (zip parameters arguments)) (cpsExpr (grinFunctionBody function))
            else throwInterpret (InterpretFunctionArity functionName (length parameters) (length arguments))

applyPrimitive :: PrimOp -> [RuntimeValue] -> EvalM RuntimeValue
applyPrimitive primOp arguments
  | length arguments < arity = pure (RuntimeNode (GrinPrimitive primOp) arguments)
  | length arguments == arity = evalPrimitive name arguments
  | otherwise = throwInterpret (InterpretPrimitiveArity name (length arguments))
  where
    name = primOpName primOp
    arity = primOpArity primOp

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

matchCpsAlternative :: Env -> RuntimeValue -> [CpsAlt] -> EvalM RuntimeValue
matchCpsAlternative env value alternatives =
  case alternatives of
    [] -> throwInterpret (InterpretNoMatchingAlternative value)
    alt : rest ->
      case matchCpsAlt value alt of
        Just bindings -> evalCpsExpr (bindings `Map.union` env) (cpsAltRhs alt)
        Nothing -> matchCpsAlternative env value rest

matchCpsAlt :: RuntimeValue -> CpsAlt -> Maybe Env
matchCpsAlt value alt =
  case (cpsAltCon alt, value) of
    (GrinDefaultAlt, _) ->
      Just (Map.fromList [(var, value) | var <- cpsAltBinders alt])
    (GrinLitAlt expected, RuntimeLit actual)
      | expected == actual -> Just Map.empty
    (GrinDataAlt expected, RuntimeNode (GrinConstructor actual) fields)
      | expected == actual,
        length fields == length (cpsAltBinders alt) ->
          Just (Map.fromList (zip (cpsAltBinders alt) fields))
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
    RuntimeNode (GrinPrimitive _) _ -> pure "<function>"
    RuntimeNode GrinThunk {} _ -> pure "<thunk>"
    RuntimeLocation _ -> renderRawValueM forced
    RuntimeMutVar {} -> pure "<mutvar>"
    RuntimeMVar {} -> pure "<mvar>"
    RuntimeThreadId identifier -> pure ("<thread " <> T.pack (show identifier) <> ">")
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

throwE :: EvalFailure -> EvalM value
throwE failure = EvalM (\continuation -> continuation (Left failure))

catchE :: EvalM value -> (EvalFailure -> EvalM value) -> EvalM value
catchE action handler =
  EvalM $ \continuation ->
    runEvalM action $ \case
      Left failure -> runEvalM (handler failure) continuation
      Right value -> continuation (Right value)

liftEvalIO :: IO value -> EvalM value
liftEvalIO action =
  EvalM $ \continuation -> lift action >>= continuation . Right

getsMachine :: (Machine -> value) -> EvalM value
getsMachine project =
  EvalM $ \continuation -> gets project >>= continuation . Right

modifyMachine :: (Machine -> Machine) -> EvalM ()
modifyMachine update =
  EvalM $ \continuation -> modify' update >> continuation (Right ())
