{-# LANGUAGE OverloadedStrings #-}

-- | Convert direct-style GRIN into explicit continuation-passing style.
--
-- Computation entries receive an ordinary heap closure as their hidden final
-- parameter. Generated continuation entries consume one logical result and
-- never return. Consequently every potentially transferring operation is in
-- tail position and the runtime never needs a continuation stack.
module Aihc.Grin.Cps
  ( CpsGrinProgram,
    CpsGrinError (..),
    cpsContinuationFunctions,
    cpsFunctionContinuations,
    cpsGrinProgram,
    cpsUpdateFunction,
    toCpsGrin,
  )
where

import Aihc.Grin.Syntax
import Aihc.Tc.Types (RuntimeRep (..), liftedRuntimeRep)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify', put, runStateT)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

-- | A GRIN program whose computation entries and control transfers obey the
-- CPS calling convention. The metadata distinguishes computation entries from
-- continuation entries without polluting direct GRIN syntax.
data CpsGrinProgram = CpsGrinProgram
  { cpsGrinProgram :: !GrinProgram,
    cpsContinuationFunctions :: !(Set FunctionName),
    cpsFunctionContinuations :: !(Map FunctionName GrinVar),
    cpsUpdateFunction :: !FunctionName
  }
  deriving (Eq, Show, Read)

data CpsGrinError
  = CpsGrinUnexpectedThrow !FunctionName
  | CpsGrinUnexpectedCatch !FunctionName
  | CpsGrinAlreadyTransformed !FunctionName
  deriving (Eq, Show)

data CpsState = CpsState
  { cpsNextVarUnique :: !Int,
    cpsNextContinuationUnique :: !Int,
    cpsUsedFunctionNames :: !(Set FunctionName),
    cpsGeneratedFunctionsRev :: ![GrinFunction],
    cpsContinuationNames :: !(Set FunctionName),
    cpsComputationContinuations :: !(Map FunctionName GrinVar)
  }

type CpsM = StateT CpsState (Either CpsGrinError)

toCpsGrin :: GrinProgram -> Either CpsGrinError CpsGrinProgram
toCpsGrin program = do
  ((functions, updateFunction), finalState) <- runStateT transform initialState
  pure
    CpsGrinProgram
      { cpsGrinProgram =
          program
            { grinExternalFunctions = map addExternalContinuation (grinExternalFunctions program),
              grinFunctions =
                functions
                  <> reverse (cpsGeneratedFunctionsRev finalState)
                  <> [updateFunction]
            },
        cpsContinuationFunctions =
          Set.insert (grinFunctionName updateFunction) (cpsContinuationNames finalState),
        cpsFunctionContinuations = cpsComputationContinuations finalState,
        cpsUpdateFunction = grinFunctionName updateFunction
      }
  where
    sourceFunctions = grinFunctions program
    initialState =
      CpsState
        { cpsNextVarUnique = 1 + maximumProgramVarUnique program,
          cpsNextContinuationUnique = 0,
          cpsUsedFunctionNames = Set.fromList (map grinFunctionName sourceFunctions),
          cpsGeneratedFunctionsRev = [],
          cpsContinuationNames = Set.empty,
          cpsComputationContinuations = Map.empty
        }
    transform = do
      updateName <- freshFunctionName "$cps$update"
      functions <- mapM (transformFunction updateName) sourceFunctions
      updateFunction <- makeUpdateFunction updateName
      pure (functions, updateFunction)

    addExternalContinuation info =
      info
        { grinCodeParameterLayouts =
            grinCodeParameterLayouts info <> [[liftedRuntimeRep]]
        }

transformFunction :: FunctionName -> GrinFunction -> CpsM GrinFunction
transformFunction updateName function = do
  continuation <- freshVar "$cps_return" liftedRuntimeRep
  let parameters = grinFunctionParameters function
      bound = Set.fromList (continuation : parameters)
  body <-
    transformTail
      updateName
      (grinFunctionName function)
      bound
      (grinFunctionResultRep function)
      (GrinVarValue continuation)
      (grinFunctionBody function)
  modify' $ \state ->
    state
      { cpsComputationContinuations =
          Map.insert (grinFunctionName function) continuation (cpsComputationContinuations state)
      }
  pure
    function
      { grinFunctionParameters = parameters <> [continuation],
        grinFunctionBody = body
      }

transformTail :: FunctionName -> FunctionName -> Set GrinVar -> RuntimeRep -> GrinValue -> GrinExpr -> CpsM GrinExpr
transformTail updateName parent bound resultRep continuation expression =
  case expression of
    GrinConstant values -> pure (GrinContinue continuation values)
    GrinBind resultVars (GrinCase scrutinee binder alternatives) body ->
      GrinCase scrutinee binder <$> mapM transformBoundAlternative alternatives
      where
        transformBoundAlternative alternative = do
          let alternativeBound = bound <> Set.fromList (binder : grinAltBinders alternative)
          rhs <-
            transformTail
              updateName
              parent
              alternativeBound
              resultRep
              continuation
              (GrinBind resultVars (grinAltRhs alternative) body)
          pure alternative {grinAltRhs = rhs}
    GrinBind resultVars valueExpression body
      | isDirectExpression valueExpression -> do
          transformedBody <-
            transformTail
              updateName
              parent
              (bound <> Set.fromList resultVars)
              resultRep
              continuation
              body
          pure (GrinBind resultVars valueExpression transformedBody)
      | otherwise -> do
          (nextVar, nextNode) <-
            reifyContinuation
              updateName
              parent
              bound
              resultRep
              continuation
              resultVars
              body
          transformedValue <-
            transformTail
              updateName
              parent
              (Set.insert nextVar bound)
              (varsRuntimeRep resultVars)
              (GrinVarValue nextVar)
              valueExpression
          pure (GrinBind [nextVar] (GrinStore nextNode) transformedValue)
    GrinStore node -> continueDirect resultRep continuation (GrinStore node)
    GrinEnsureHeap {} -> alreadyTransformed
    GrinStoreUnchecked {} -> alreadyTransformed
    GrinStoreRec bindings body -> do
      let recursiveVars = Set.fromList (map fst bindings)
      GrinStoreRec bindings
        <$> transformTail updateName parent (bound <> recursiveVars) resultRep continuation body
    GrinStoreRecUnchecked {} -> alreadyTransformed
    GrinFetch runtimeRep value ->
      continueDirect runtimeRep continuation (GrinFetch runtimeRep value)
    GrinUpdate pointer value ->
      continueDirect (grinValueRuntimeRep value) continuation (GrinUpdate pointer value)
    GrinUpdateBlackhole pointer value ->
      continueDirect (grinValueRuntimeRep value) continuation (GrinUpdateBlackhole pointer value)
    GrinEval runtimeRep value -> do
      (updateVar, updateNode) <- makeUpdateContinuation updateName value continuation
      pure
        ( GrinBind
            [updateVar]
            (GrinStore updateNode)
            (GrinCpsEval runtimeRep value continuation (GrinVarValue updateVar))
        )
    GrinCpsEval {} -> alreadyTransformed
    GrinCall runtimeRep functionName arguments ->
      pure (GrinCall runtimeRep functionName (arguments <> [continuation]))
    GrinPrimitiveCall runtimeRep name arguments
      | isControlPrimitive name ->
          pure (GrinCpsPrimitiveCall runtimeRep name arguments continuation)
      | otherwise ->
          continueDirect runtimeRep continuation (GrinPrimitiveCall runtimeRep name arguments)
    GrinCpsPrimitiveCall {} -> alreadyTransformed
    GrinApply runtimeRep function arguments ->
      pure (GrinCpsApply runtimeRep function arguments continuation)
    GrinCpsApply {} -> alreadyTransformed
    GrinContinue {} -> alreadyTransformed
    GrinHalt {} -> alreadyTransformed
    GrinCase scrutinee binder alternatives ->
      GrinCase scrutinee binder <$> mapM transformAlternative alternatives
      where
        transformAlternative alternative = do
          let alternativeBound = bound <> Set.fromList (binder : grinAltBinders alternative)
          rhs <-
            transformTail
              updateName
              parent
              alternativeBound
              resultRep
              continuation
              (grinAltRhs alternative)
          pure alternative {grinAltRhs = rhs}
    GrinThrow {} -> lift (Left (CpsGrinUnexpectedThrow parent))
    GrinCatch {} -> lift (Left (CpsGrinUnexpectedCatch parent))
    GrinForeignCallExpr foreignCall arguments ->
      continueDirect resultRep continuation (GrinForeignCallExpr foreignCall arguments)
  where
    alreadyTransformed = lift (Left (CpsGrinAlreadyTransformed parent))

reifyContinuation :: FunctionName -> FunctionName -> Set GrinVar -> RuntimeRep -> GrinValue -> [GrinVar] -> GrinExpr -> CpsM (GrinVar, GrinNode)
reifyContinuation updateName parent bound resultRep outerContinuation resultVars body = do
  transformedBody <-
    transformTail
      updateName
      parent
      (bound <> Set.fromList resultVars)
      resultRep
      outerContinuation
      body
  continuationName <- freshContinuationName parent
  pointer <- freshVar "$cps_continuation" liftedRuntimeRep
  let captures = Set.toAscList (freeExprVars transformedBody `Set.intersection` bound)
      continuationFunction =
        GrinFunction
          { grinFunctionName = continuationName,
            grinFunctionLinkName = Nothing,
            grinFunctionParameters = captures <> resultVars,
            grinFunctionResultRep = resultRep,
            grinFunctionBody = transformedBody
          }
      continuationNode =
        GrinNode
          (GrinClosure continuationName [map grinVarRuntimeRep resultVars])
          (map GrinVarValue captures)
  addContinuationFunction continuationFunction
  pure (pointer, continuationNode)

continueDirect :: RuntimeRep -> GrinValue -> GrinExpr -> CpsM GrinExpr
continueDirect runtimeRep continuation directExpression = do
  resultVars <- mapM (freshVar "$cps_result") (runtimeRepComponents runtimeRep)
  pure
    ( GrinBind
        resultVars
        directExpression
        (GrinContinue continuation (map GrinVarValue resultVars))
    )

makeUpdateContinuation :: FunctionName -> GrinValue -> GrinValue -> CpsM (GrinVar, GrinNode)
makeUpdateContinuation updateName blackhole continuation = do
  pointer <- freshVar "$cps_update" liftedRuntimeRep
  pure
    ( pointer,
      GrinNode (GrinClosure updateName [[liftedRuntimeRep]]) [blackhole, continuation]
    )

makeUpdateFunction :: FunctionName -> CpsM GrinFunction
makeUpdateFunction updateName = do
  blackhole <- freshVar "$cps_blackhole" liftedRuntimeRep
  outerContinuation <- freshVar "$cps_outer" liftedRuntimeRep
  result <- freshVar "$cps_thunk_result" liftedRuntimeRep
  updated <- freshVar "$cps_updated" liftedRuntimeRep
  nextUpdate <- freshVar "$cps_next_update" liftedRuntimeRep
  let nextUpdateNode =
        GrinNode
          (GrinClosure updateName [[liftedRuntimeRep]])
          [GrinVarValue result, GrinVarValue outerContinuation]
  pure
    GrinFunction
      { grinFunctionName = updateName,
        grinFunctionLinkName = Nothing,
        grinFunctionParameters = [blackhole, outerContinuation, result],
        grinFunctionResultRep = liftedRuntimeRep,
        grinFunctionBody =
          GrinBind
            [updated]
            (GrinUpdateBlackhole (GrinVarValue blackhole) (GrinVarValue result))
            ( GrinBind
                [nextUpdate]
                (GrinStore nextUpdateNode)
                ( GrinCpsEval
                    liftedRuntimeRep
                    (GrinVarValue result)
                    (GrinVarValue outerContinuation)
                    (GrinVarValue nextUpdate)
                )
            )
      }

isDirectExpression :: GrinExpr -> Bool
isDirectExpression expression =
  case expression of
    GrinConstant {} -> True
    GrinStore {} -> True
    GrinFetch {} -> True
    GrinUpdate {} -> True
    GrinUpdateBlackhole {} -> True
    GrinPrimitiveCall _ name _ -> not (isControlPrimitive name)
    GrinCpsPrimitiveCall {} -> False
    GrinForeignCallExpr {} -> True
    _ -> False

freshContinuationName :: FunctionName -> CpsM FunctionName
freshContinuationName parent =
  freshFunctionName ("$cps$" <> unFunctionName parent <> "$")

freshFunctionName :: T.Text -> CpsM FunctionName
freshFunctionName prefix = do
  state <- get
  let unique = cpsNextContinuationUnique state
      candidate = FunctionName (prefix <> T.pack (show unique))
  put state {cpsNextContinuationUnique = unique + 1}
  if candidate `Set.member` cpsUsedFunctionNames state
    then freshFunctionName prefix
    else do
      modify' $ \current ->
        current {cpsUsedFunctionNames = Set.insert candidate (cpsUsedFunctionNames current)}
      pure candidate

freshVar :: T.Text -> RuntimeRep -> CpsM GrinVar
freshVar name runtimeRep = do
  state <- get
  let unique = cpsNextVarUnique state
  put state {cpsNextVarUnique = unique + 1}
  pure (GrinVar name unique runtimeRep)

addContinuationFunction :: GrinFunction -> CpsM ()
addContinuationFunction function = do
  addGeneratedFunction function
  modify' $ \state ->
    state
      { cpsContinuationNames =
          Set.insert (grinFunctionName function) (cpsContinuationNames state)
      }

addGeneratedFunction :: GrinFunction -> CpsM ()
addGeneratedFunction function =
  modify' $ \state ->
    state
      { cpsGeneratedFunctionsRev = function : cpsGeneratedFunctionsRev state
      }

varsRuntimeRep :: [GrinVar] -> RuntimeRep
varsRuntimeRep vars =
  case map grinVarRuntimeRep vars of
    [runtimeRep] -> runtimeRep
    runtimeReps -> TupleRep runtimeReps

freeExprVars :: GrinExpr -> Set GrinVar
freeExprVars expression =
  case expression of
    GrinConstant values -> foldMap freeValueVars values
    GrinBind vars valueExpression body ->
      freeExprVars valueExpression
        <> (freeExprVars body `Set.difference` Set.fromList vars)
    GrinStore node -> freeNodeVars node
    GrinEnsureHeap _ roots -> foldMap freeValueVars roots
    GrinStoreUnchecked node -> freeNodeVars node
    GrinStoreRec bindings body ->
      (foldMap (freeNodeVars . snd) bindings <> freeExprVars body)
        `Set.difference` Set.fromList (map fst bindings)
    GrinStoreRecUnchecked bindings body ->
      (foldMap (freeNodeVars . snd) bindings <> freeExprVars body)
        `Set.difference` Set.fromList (map fst bindings)
    GrinFetch _ pointer -> freeValueVars pointer
    GrinUpdate pointer value -> freeValueVars pointer <> freeValueVars value
    GrinUpdateBlackhole pointer value -> freeValueVars pointer <> freeValueVars value
    GrinEval _ value -> freeValueVars value
    GrinCpsEval _ value continuation updateContinuation ->
      freeValueVars value <> freeValueVars continuation <> freeValueVars updateContinuation
    GrinCall _ _ arguments -> foldMap freeValueVars arguments
    GrinPrimitiveCall _ _ arguments -> foldMap freeValueVars arguments
    GrinCpsPrimitiveCall _ _ arguments continuation ->
      foldMap freeValueVars arguments <> freeValueVars continuation
    GrinApply _ function arguments -> freeValueVars function <> foldMap freeValueVars arguments
    GrinCpsApply _ function arguments continuation ->
      freeValueVars function <> foldMap freeValueVars arguments <> freeValueVars continuation
    GrinContinue continuation values -> freeValueVars continuation <> foldMap freeValueVars values
    GrinHalt values -> foldMap freeValueVars values
    GrinCase scrutinee binder alternatives ->
      freeValueVars scrutinee
        <> foldMap (freeAlternativeVars binder) alternatives
    GrinThrow exception -> freeValueVars exception
    GrinCatch _ action handler state ->
      freeValueVars action <> freeValueVars handler <> foldMap freeValueVars state
    GrinForeignCallExpr _ arguments -> foldMap freeValueVars arguments

freeAlternativeVars :: GrinVar -> GrinAlt -> Set GrinVar
freeAlternativeVars binder alternative =
  freeExprVars (grinAltRhs alternative)
    `Set.difference` Set.fromList (binder : grinAltBinders alternative)

freeValueVars :: GrinValue -> Set GrinVar
freeValueVars value =
  case value of
    GrinVarValue var -> Set.singleton var
    GrinLitValue {} -> Set.empty

freeNodeVars :: GrinNode -> Set GrinVar
freeNodeVars = foldMap freeValueVars . grinNodeFields

maximumProgramVarUnique :: GrinProgram -> Int
maximumProgramVarUnique program =
  maximum
    ( 0
        : map (grinVarUnique . fst) (grinPrimitives program)
          <> concatMap bindingUniques (grinWhnfGlobals program)
          <> concatMap bindingUniques (grinCafs program)
          <> concatMap functionUniques (grinFunctions program)
    )
  where
    bindingUniques (var, node) = grinVarUnique var : nodeUniques node
    functionUniques function =
      map grinVarUnique (grinFunctionParameters function)
        <> exprUniques (grinFunctionBody function)

exprUniques :: GrinExpr -> [Int]
exprUniques expression =
  case expression of
    GrinConstant values -> concatMap valueUniques values
    GrinBind vars valueExpression body ->
      map grinVarUnique vars <> exprUniques valueExpression <> exprUniques body
    GrinStore node -> nodeUniques node
    GrinEnsureHeap _ roots -> concatMap valueUniques roots
    GrinStoreUnchecked node -> nodeUniques node
    GrinStoreRec bindings body ->
      concatMap (\(var, node) -> grinVarUnique var : nodeUniques node) bindings
        <> exprUniques body
    GrinStoreRecUnchecked bindings body ->
      concatMap (\(var, node) -> grinVarUnique var : nodeUniques node) bindings
        <> exprUniques body
    GrinFetch _ pointer -> valueUniques pointer
    GrinUpdate pointer value -> valueUniques pointer <> valueUniques value
    GrinUpdateBlackhole pointer value -> valueUniques pointer <> valueUniques value
    GrinEval _ value -> valueUniques value
    GrinCpsEval _ value continuation updateContinuation ->
      valueUniques value <> valueUniques continuation <> valueUniques updateContinuation
    GrinCall _ _ arguments -> concatMap valueUniques arguments
    GrinPrimitiveCall _ _ arguments -> concatMap valueUniques arguments
    GrinCpsPrimitiveCall _ _ arguments continuation ->
      concatMap valueUniques arguments <> valueUniques continuation
    GrinApply _ function arguments -> valueUniques function <> concatMap valueUniques arguments
    GrinCpsApply _ function arguments continuation ->
      valueUniques function <> concatMap valueUniques arguments <> valueUniques continuation
    GrinContinue continuation values -> valueUniques continuation <> concatMap valueUniques values
    GrinHalt values -> concatMap valueUniques values
    GrinCase scrutinee binder alternatives ->
      valueUniques scrutinee
        <> (grinVarUnique binder : concatMap alternativeUniques alternatives)
    GrinThrow exception -> valueUniques exception
    GrinCatch _ action handler state ->
      valueUniques action <> valueUniques handler <> concatMap valueUniques state
    GrinForeignCallExpr _ arguments -> concatMap valueUniques arguments

isControlPrimitive :: T.Text -> Bool
isControlPrimitive name = name == "fork#" || name == "yield#"

alternativeUniques :: GrinAlt -> [Int]
alternativeUniques alternative =
  map grinVarUnique (grinAltBinders alternative)
    <> exprUniques (grinAltRhs alternative)

valueUniques :: GrinValue -> [Int]
valueUniques value =
  case value of
    GrinVarValue var -> [grinVarUnique var]
    GrinLitValue {} -> []

nodeUniques :: GrinNode -> [Int]
nodeUniques = concatMap valueUniques . grinNodeFields
