{-# LANGUAGE OverloadedStrings #-}

-- | Reify direct-style GRIN continuations as ordinary GRIN closures.
--
-- This pass deliberately keeps the GRIN syntax. Each source 'GrinBind' is
-- rewritten so its body lives in a generated function. A closure for that
-- function is allocated with 'GrinStore' and invoked with 'GrinApply' after
-- the bound expression produces its values. The introduced administrative
-- binds retain the current runtime-operation protocol; future suspension
-- lowering can transfer ownership of the explicit closure instead of
-- returning through them.
module Aihc.Grin.Cps
  ( CpsGrinProgram,
    CpsGrinError (..),
    cpsGrinProgram,
    toCpsGrin,
  )
where

import Aihc.Grin.Syntax
import Aihc.Tc.Types (RuntimeRep (..), liftedRuntimeRep)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify', put, runStateT)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

-- | A GRIN program known to have passed through continuation reification.
-- Keeping the constructor private prevents accidentally applying the pass
-- twice or sending direct-style GRIN to a backend that expects CPS-GRIN.
newtype CpsGrinProgram = CpsGrinProgram
  { cpsGrinProgram :: GrinProgram
  }
  deriving (Eq, Show, Read)

-- | A violated precondition at the CPS boundary. Exception control must have
-- been lowered to ordinary GRIN control flow before continuations are reified.
data CpsGrinError
  = CpsGrinUnexpectedThrow !FunctionName
  | CpsGrinUnexpectedCatch !FunctionName
  deriving (Eq, Show)

data CpsState = CpsState
  { cpsNextVarUnique :: !Int,
    cpsNextContinuationUnique :: !Int,
    cpsUsedFunctionNames :: !(Set FunctionName),
    cpsGeneratedFunctionsRev :: ![GrinFunction]
  }

type CpsM = StateT CpsState (Either CpsGrinError)

toCpsGrin :: GrinProgram -> Either CpsGrinError CpsGrinProgram
toCpsGrin program = do
  (functions, finalState) <- runStateT (mapM transformFunction sourceFunctions) initialState
  pure . CpsGrinProgram $
    program
      { grinFunctions = functions <> reverse (cpsGeneratedFunctionsRev finalState)
      }
  where
    sourceFunctions = grinFunctions program
    initialState =
      CpsState
        { cpsNextVarUnique = 1 + maximumProgramVarUnique program,
          cpsNextContinuationUnique = 0,
          cpsUsedFunctionNames = Set.fromList (map grinFunctionName sourceFunctions),
          cpsGeneratedFunctionsRev = []
        }

transformFunction :: GrinFunction -> CpsM GrinFunction
transformFunction function = do
  body <-
    transformExpr
      (grinFunctionName function)
      (Set.fromList (grinFunctionParameters function))
      (grinFunctionResultRep function)
      (grinFunctionBody function)
  pure function {grinFunctionBody = body}

transformExpr :: FunctionName -> Set GrinVar -> RuntimeRep -> GrinExpr -> CpsM GrinExpr
transformExpr parent bound resultRep expression =
  case expression of
    GrinConstant {} -> pure expression
    GrinBind resultVars valueExpression body -> do
      transformedValue <- transformExpr parent bound (varsRuntimeRep resultVars) valueExpression
      transformedBody <- transformExpr parent (bound <> Set.fromList resultVars) resultRep body
      continuationName <- freshContinuationName parent
      continuationVar <- freshContinuationVar
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
          invokeContinuation =
            GrinApply
              resultRep
              (GrinVarValue continuationVar)
              (map GrinVarValue resultVars)
      addGeneratedFunction continuationFunction
      pure
        ( GrinBind
            [continuationVar]
            (GrinStore continuationNode)
            (GrinBind resultVars transformedValue invokeContinuation)
        )
    GrinStore {} -> pure expression
    GrinStoreRec bindings body -> do
      let recursiveVars = Set.fromList (map fst bindings)
      GrinStoreRec bindings <$> transformExpr parent (bound <> recursiveVars) resultRep body
    GrinFetch {} -> pure expression
    GrinUpdate {} -> pure expression
    GrinEval {} -> pure expression
    GrinCall {} -> pure expression
    GrinPrimitiveCall {} -> pure expression
    GrinApply {} -> pure expression
    GrinCase scrutinee binder alternatives ->
      GrinCase scrutinee binder <$> mapM transformAlternative alternatives
      where
        transformAlternative alternative = do
          let alternativeBound = bound <> Set.fromList (binder : grinAltBinders alternative)
          rhs <- transformExpr parent alternativeBound resultRep (grinAltRhs alternative)
          pure alternative {grinAltRhs = rhs}
    GrinThrow {} -> lift (Left (CpsGrinUnexpectedThrow parent))
    GrinCatch {} -> lift (Left (CpsGrinUnexpectedCatch parent))
    GrinForeignCallExpr {} -> pure expression

freshContinuationName :: FunctionName -> CpsM FunctionName
freshContinuationName parent = do
  state <- get
  let unique = cpsNextContinuationUnique state
      candidate =
        FunctionName
          ( "$cps$"
              <> unFunctionName parent
              <> "$"
              <> T.pack (show unique)
          )
  put state {cpsNextContinuationUnique = unique + 1}
  if candidate `Set.member` cpsUsedFunctionNames state
    then freshContinuationName parent
    else do
      modify' $ \current -> current {cpsUsedFunctionNames = Set.insert candidate (cpsUsedFunctionNames current)}
      pure candidate

freshContinuationVar :: CpsM GrinVar
freshContinuationVar = do
  state <- get
  let unique = cpsNextVarUnique state
  put state {cpsNextVarUnique = unique + 1}
  pure (GrinVar "$cps_continuation" unique liftedRuntimeRep)

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
    GrinStoreRec bindings body ->
      (foldMap (freeNodeVars . snd) bindings <> freeExprVars body)
        `Set.difference` Set.fromList (map fst bindings)
    GrinFetch _ pointer -> freeValueVars pointer
    GrinUpdate pointer value -> freeValueVars pointer <> freeValueVars value
    GrinEval _ value -> freeValueVars value
    GrinCall _ _ arguments -> foldMap freeValueVars arguments
    GrinPrimitiveCall _ _ arguments -> foldMap freeValueVars arguments
    GrinApply _ function arguments -> freeValueVars function <> foldMap freeValueVars arguments
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
    GrinStoreRec bindings body ->
      concatMap (\(var, node) -> grinVarUnique var : nodeUniques node) bindings
        <> exprUniques body
    GrinFetch _ pointer -> valueUniques pointer
    GrinUpdate pointer value -> valueUniques pointer <> valueUniques value
    GrinEval _ value -> valueUniques value
    GrinCall _ _ arguments -> concatMap valueUniques arguments
    GrinPrimitiveCall _ _ arguments -> concatMap valueUniques arguments
    GrinApply _ function arguments -> valueUniques function <> concatMap valueUniques arguments
    GrinCase scrutinee binder alternatives ->
      valueUniques scrutinee
        <> (grinVarUnique binder : concatMap alternativeUniques alternatives)
    GrinThrow exception -> valueUniques exception
    GrinCatch _ action handler state ->
      valueUniques action <> valueUniques handler <> concatMap valueUniques state
    GrinForeignCallExpr _ arguments -> concatMap valueUniques arguments

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
