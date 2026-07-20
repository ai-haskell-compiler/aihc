-- | Shared structural analyses over strict GRIN.
module Aihc.Grin.Analysis
  ( freeExprVars,
    freeNodeVars,
  )
where

import Aihc.Grin.Syntax
import Data.Set (Set)
import Data.Set qualified as Set

freeExprVars :: GrinExpr -> Set GrinVar
freeExprVars expression =
  case expression of
    GrinConstant values -> foldMap freeValueVars values
    GrinBind vars valueExpression body ->
      freeExprVars valueExpression <> (freeExprVars body `Set.difference` Set.fromList vars)
    GrinStore node -> freeNodeVars node
    GrinEnsureHeap _ roots -> foldMap freeValueVars roots
    GrinStoreUnchecked node -> freeNodeVars node
    GrinStoreRec bindings body -> freeStoreRecVars bindings body
    GrinStoreRecUnchecked bindings body -> freeStoreRecVars bindings body
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
      freeValueVars scrutinee <> foldMap (freeAlternativeVars binder) alternatives
    GrinThrow exception -> freeValueVars exception
    GrinCatch _ action handler state ->
      freeValueVars action <> freeValueVars handler <> foldMap freeValueVars state
    GrinForeignCallExpr _ arguments -> foldMap freeValueVars arguments

freeStoreRecVars :: [(GrinVar, GrinNode)] -> GrinExpr -> Set GrinVar
freeStoreRecVars bindings body =
  (foldMap (freeNodeVars . snd) bindings <> freeExprVars body)
    `Set.difference` Set.fromList (map fst bindings)

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
