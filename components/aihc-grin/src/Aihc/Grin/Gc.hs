{-# LANGUAGE OverloadedStrings #-}

-- | Make post-CPS allocation safepoints and relocated roots explicit.
module Aihc.Grin.Gc
  ( GcGrinProgram,
    gcContinuationFunctions,
    gcFunctionContinuations,
    gcGrinProgram,
    gcUpdateFunction,
    lowerGc,
  )
where

import Aihc.Grin.Analysis (freeExprVars, freeNodeVars)
import Aihc.Grin.Cps (CpsGrinProgram (..))
import Aihc.Grin.Syntax
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | A CPS-GRIN program whose managed allocations have explicit safepoints.
-- Keeping this phase distinct prevents native backends from accidentally
-- consuming CPS-GRIN before roots have been made relocatable.
data GcGrinProgram = GcGrinProgram
  { gcGrinProgram :: !GrinProgram,
    gcContinuationFunctions :: !(Set FunctionName),
    gcFunctionContinuations :: !(Map FunctionName GrinVar),
    gcUpdateFunction :: !FunctionName
  }
  deriving (Eq, Show, Read)

-- | Replace every managed store with an explicit reservation followed by an
-- unchecked allocation. The reservation is an identity operation on its fast
-- path and returns fresh SSA names for roots relocated by its slow path.
lowerGc :: CpsGrinProgram -> GcGrinProgram
lowerGc cps =
  GcGrinProgram
    { gcGrinProgram =
        program
          { grinFunctions = evalState (mapM lowerFunction (grinFunctions program)) nextUnique
          },
      gcContinuationFunctions = cpsContinuationFunctions cps,
      gcFunctionContinuations = cpsFunctionContinuations cps,
      gcUpdateFunction = cpsUpdateFunction cps
    }
  where
    program = cpsGrinProgram cps
    nextUnique = 1 + maximumProgramVarUnique program

lowerFunction :: GrinFunction -> State Int GrinFunction
lowerFunction function = do
  body <- lowerExpr (Set.fromList (grinFunctionParameters function)) (grinFunctionBody function)
  pure function {grinFunctionBody = body}

lowerExpr :: Set GrinVar -> GrinExpr -> State Int GrinExpr
lowerExpr bound expression =
  case expression of
    GrinBind resultVars (GrinStore node) body ->
      lowerStore bound resultVars node body
    GrinBind resultVars valueExpression body -> do
      valueExpression' <- lowerExpr bound valueExpression
      body' <- lowerExpr (bound <> Set.fromList resultVars) body
      pure (GrinBind resultVars valueExpression' body')
    GrinStore node -> lowerTailStore bound node
    GrinStoreRec bindings body -> lowerStoreRec bound bindings body
    GrinCase scrutinee binder alternatives ->
      GrinCase scrutinee binder <$> mapM (lowerAlternative (Set.insert binder bound)) alternatives
    GrinConstant {} -> pure expression
    GrinEnsureHeap {} -> pure expression
    GrinStoreUnchecked {} -> pure expression
    GrinStoreRecUnchecked {} -> pure expression
    GrinFetch {} -> pure expression
    GrinUpdate {} -> pure expression
    GrinUpdateBlackhole {} -> pure expression
    GrinEval {} -> pure expression
    GrinCpsEval {} -> pure expression
    GrinCall {} -> pure expression
    GrinPrimitiveCall {} -> pure expression
    GrinCpsPrimitiveCall {} -> pure expression
    GrinApply {} -> pure expression
    GrinCpsApply {} -> pure expression
    GrinContinue {} -> pure expression
    GrinHalt {} -> pure expression
    GrinThrow {} -> pure expression
    GrinCatch {} -> pure expression
    GrinForeignCallExpr {} -> pure expression

lowerStore :: Set GrinVar -> [GrinVar] -> GrinNode -> GrinExpr -> State Int GrinExpr
lowerStore bound resultVars node body = do
  let roots = livePointerRoots bound (freeNodeVars node <> freeExprVars body)
  relocated <- mapM freshRelocated roots
  let substitutions = Map.fromList (zip roots relocated)
      node' = substituteNode substitutions node
      bodyWithRelocatedRoots = substituteExpr substitutions body
  body' <- lowerExpr (bound <> Set.fromList relocated <> Set.fromList resultVars) bodyWithRelocatedRoots
  pure
    ( GrinBind
        relocated
        (GrinEnsureHeap (nodeWords node) (map GrinVarValue roots))
        (GrinBind resultVars (GrinStoreUnchecked node') body')
    )

lowerTailStore :: Set GrinVar -> GrinNode -> State Int GrinExpr
lowerTailStore bound node = do
  let roots = livePointerRoots bound (freeNodeVars node)
  relocated <- mapM freshRelocated roots
  let substitutions = Map.fromList (zip roots relocated)
  pure
    ( GrinBind
        relocated
        (GrinEnsureHeap (nodeWords node) (map GrinVarValue roots))
        (GrinStoreUnchecked (substituteNode substitutions node))
    )

lowerStoreRec :: Set GrinVar -> [(GrinVar, GrinNode)] -> GrinExpr -> State Int GrinExpr
lowerStoreRec bound bindings body = do
  let recursiveVars = Set.fromList (map fst bindings)
      uses = foldMap (freeNodeVars . snd) bindings <> freeExprVars body
      roots = livePointerRoots bound (uses `Set.difference` recursiveVars)
  relocated <- mapM freshRelocated roots
  let substitutions = Map.fromList (zip roots relocated)
      bindings' = [(var, substituteNode substitutions node) | (var, node) <- bindings]
      bodyWithRelocatedRoots = substituteExpr substitutions body
  body' <- lowerExpr (bound <> Set.fromList relocated <> recursiveVars) bodyWithRelocatedRoots
  pure
    ( GrinBind
        relocated
        (GrinEnsureHeap (sum (map (nodeWords . snd) bindings)) (map GrinVarValue roots))
        (GrinStoreRecUnchecked bindings' body')
    )

lowerAlternative :: Set GrinVar -> GrinAlt -> State Int GrinAlt
lowerAlternative bound alternative = do
  rhs <- lowerExpr (bound <> Set.fromList (grinAltBinders alternative)) (grinAltRhs alternative)
  pure alternative {grinAltRhs = rhs}

livePointerRoots :: Set GrinVar -> Set GrinVar -> [GrinVar]
livePointerRoots bound uses =
  Set.toAscList (Set.filter (isPointerRuntimeRep . grinVarRuntimeRep) (bound `Set.intersection` uses))

freshRelocated :: GrinVar -> State Int GrinVar
freshRelocated var = do
  unique <- get
  put (unique + 1)
  pure var {grinVarName = grinVarName var <> "$gc", grinVarUnique = unique}

-- One tagged info-table pointer plus the statically known payload. A
-- zero-field thunk reserves one payload word so it can become an indirection
-- in place.
nodeWords :: GrinNode -> Int
nodeWords node =
  1
    + case grinNodeTag node of
      GrinThunk {} -> max 1 fieldCount
      _ -> fieldCount
  where
    fieldCount = length (grinNodeFields node)

substituteExpr :: Map GrinVar GrinVar -> GrinExpr -> GrinExpr
substituteExpr substitutions expression =
  case expression of
    GrinConstant values -> GrinConstant (map (substituteValue substitutions) values)
    GrinBind vars valueExpression body ->
      GrinBind vars (substituteExpr substitutions valueExpression) (substituteExpr (without vars substitutions) body)
    GrinStore node -> GrinStore (substituteNode substitutions node)
    GrinEnsureHeap requiredWords roots -> GrinEnsureHeap requiredWords (map (substituteValue substitutions) roots)
    GrinStoreUnchecked node -> GrinStoreUnchecked (substituteNode substitutions node)
    GrinStoreRec bindings body -> substituteStoreRec GrinStoreRec substitutions bindings body
    GrinStoreRecUnchecked bindings body -> substituteStoreRec GrinStoreRecUnchecked substitutions bindings body
    GrinFetch runtimeRep pointer -> GrinFetch runtimeRep (substituteValue substitutions pointer)
    GrinUpdate pointer value -> GrinUpdate (substituteValue substitutions pointer) (substituteValue substitutions value)
    GrinUpdateBlackhole pointer value -> GrinUpdateBlackhole (substituteValue substitutions pointer) (substituteValue substitutions value)
    GrinEval runtimeRep value -> GrinEval runtimeRep (substituteValue substitutions value)
    GrinCpsEval runtimeRep value continuation updateContinuation ->
      GrinCpsEval runtimeRep (substituteValue substitutions value) (substituteValue substitutions continuation) (substituteValue substitutions updateContinuation)
    GrinCall runtimeRep name arguments -> GrinCall runtimeRep name (map (substituteValue substitutions) arguments)
    GrinPrimitiveCall runtimeRep name arguments -> GrinPrimitiveCall runtimeRep name (map (substituteValue substitutions) arguments)
    GrinCpsPrimitiveCall runtimeRep name arguments continuation ->
      GrinCpsPrimitiveCall runtimeRep name (map (substituteValue substitutions) arguments) (substituteValue substitutions continuation)
    GrinApply runtimeRep function arguments -> GrinApply runtimeRep (substituteValue substitutions function) (map (substituteValue substitutions) arguments)
    GrinCpsApply runtimeRep function arguments continuation ->
      GrinCpsApply runtimeRep (substituteValue substitutions function) (map (substituteValue substitutions) arguments) (substituteValue substitutions continuation)
    GrinContinue continuation values -> GrinContinue (substituteValue substitutions continuation) (map (substituteValue substitutions) values)
    GrinHalt values -> GrinHalt (map (substituteValue substitutions) values)
    GrinCase scrutinee binder alternatives ->
      GrinCase
        (substituteValue substitutions scrutinee)
        binder
        [ alternative
            { grinAltRhs = substituteExpr (without (binder : grinAltBinders alternative) substitutions) (grinAltRhs alternative)
            }
        | alternative <- alternatives
        ]
    GrinThrow exception -> GrinThrow (substituteValue substitutions exception)
    GrinCatch runtimeRep action handler state ->
      GrinCatch runtimeRep (substituteValue substitutions action) (substituteValue substitutions handler) (map (substituteValue substitutions) state)
    GrinForeignCallExpr foreignCall arguments -> GrinForeignCallExpr foreignCall (map (substituteValue substitutions) arguments)

substituteStoreRec :: ([(GrinVar, GrinNode)] -> GrinExpr -> GrinExpr) -> Map GrinVar GrinVar -> [(GrinVar, GrinNode)] -> GrinExpr -> GrinExpr
substituteStoreRec constructor substitutions bindings body =
  constructor
    [(var, substituteNode substitutions' node) | (var, node) <- bindings]
    (substituteExpr substitutions' body)
  where
    substitutions' = without (map fst bindings) substitutions

substituteNode :: Map GrinVar GrinVar -> GrinNode -> GrinNode
substituteNode substitutions node =
  node {grinNodeFields = map (substituteValue substitutions) (grinNodeFields node)}

substituteValue :: Map GrinVar GrinVar -> GrinValue -> GrinValue
substituteValue substitutions value =
  case value of
    GrinVarValue var -> GrinVarValue (Map.findWithDefault var var substitutions)
    GrinLitValue {} -> value

without :: [GrinVar] -> Map GrinVar GrinVar -> Map GrinVar GrinVar
without vars substitutions = foldr Map.delete substitutions vars

maximumProgramVarUnique :: GrinProgram -> Int
maximumProgramVarUnique program =
  maximum
    ( 0
        : map (grinVarUnique . fst) (grinPrimitives program)
          <> concatMap staticUniques (grinWhnfGlobals program)
          <> concatMap staticUniques (grinCafs program)
          <> concatMap functionUniques (grinFunctions program)
    )
  where
    staticUniques (var, node) = grinVarUnique var : concatMap valueUnique (grinNodeFields node)
    functionUniques function = map grinVarUnique (grinFunctionParameters function) <> exprUniques (grinFunctionBody function)
    valueUnique value =
      case value of
        GrinVarValue var -> [grinVarUnique var]
        GrinLitValue {} -> []
    nodeUniques = concatMap valueUnique . grinNodeFields
    altUniques alternative = map grinVarUnique (grinAltBinders alternative) <> exprUniques (grinAltRhs alternative)
    exprUniques expr =
      case expr of
        GrinConstant values -> concatMap valueUnique values
        GrinBind vars valueExpression body -> map grinVarUnique vars <> exprUniques valueExpression <> exprUniques body
        GrinStore node -> nodeUniques node
        GrinEnsureHeap _ roots -> concatMap valueUnique roots
        GrinStoreUnchecked node -> nodeUniques node
        GrinStoreRec bindings body -> storeRecUniques bindings body
        GrinStoreRecUnchecked bindings body -> storeRecUniques bindings body
        GrinFetch _ value -> valueUnique value
        GrinUpdate pointer value -> concatMap valueUnique [pointer, value]
        GrinUpdateBlackhole pointer value -> concatMap valueUnique [pointer, value]
        GrinEval _ value -> valueUnique value
        GrinCpsEval _ value continuation updateContinuation -> concatMap valueUnique [value, continuation, updateContinuation]
        GrinCall _ _ arguments -> concatMap valueUnique arguments
        GrinPrimitiveCall _ _ arguments -> concatMap valueUnique arguments
        GrinCpsPrimitiveCall _ _ arguments continuation -> concatMap valueUnique (continuation : arguments)
        GrinApply _ function arguments -> concatMap valueUnique (function : arguments)
        GrinCpsApply _ function arguments continuation -> concatMap valueUnique (function : continuation : arguments)
        GrinContinue continuation values -> concatMap valueUnique (continuation : values)
        GrinHalt values -> concatMap valueUnique values
        GrinCase scrutinee binder alternatives ->
          valueUnique scrutinee
            <> (grinVarUnique binder : concatMap altUniques alternatives)
        GrinThrow exception -> valueUnique exception
        GrinCatch _ action handler state -> concatMap valueUnique (action : handler : state)
        GrinForeignCallExpr _ arguments -> concatMap valueUnique arguments
    storeRecUniques bindings body = concatMap (\(var, node) -> grinVarUnique var : nodeUniques node) bindings <> exprUniques body
