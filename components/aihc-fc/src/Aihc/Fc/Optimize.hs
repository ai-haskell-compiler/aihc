-- | Optional, monotone System FC simplifications.
--
-- Every optimization in this module must strictly simplify the Core program:
-- rules may remove structure, but must not trade one form for another form of
-- equal complexity. This makes it safe to run the complete rule set to a
-- fixpoint and keeps interactions between independent rules predictable.
-- Compiler correctness must never depend on running this module.
module Aihc.Fc.Optimize
  ( optimizeProgram,
  )
where

import Aihc.Fc.Subst (OccurrenceCount (..), countExprVar)
import Aihc.Fc.Syntax
import Aihc.Tc.Types (isLiftedType)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

type CoreOptimization = FcProgram -> FcProgram

-- | Optionally apply every Core simplification until a complete pass makes no
-- change.
optimizeProgram :: FcProgram -> FcProgram
optimizeProgram = untilStable runOptimizations
  where
    runOptimizations program = List.foldl' (flip ($)) program coreOptimizations

-- Keep this list ordered from local canonicalizations to broader rewrites so
-- later rules see the simplest output available from earlier rules.
coreOptimizations :: [CoreOptimization]
coreOptimizations = [copyPropagateProgram, eliminateDeadLetsProgram]

untilStable :: (Eq value) => (value -> value) -> value -> value
untilStable transform input =
  let output = transform input
   in if output == input then input else untilStable transform output

-- | Eliminate aliases of the form @let x = y in body@. Source lets currently
-- arrive as singleton recursive groups, so that representation is simplified
-- when it is not genuinely recursive. Computed right-hand sides are
-- intentionally retained.
copyPropagateProgram :: FcProgram -> FcProgram
copyPropagateProgram (FcProgram topBinds) =
  FcProgram (map copyTopBind topBinds)
  where
    copyTopBind topBind =
      case topBind of
        FcTopBind bind -> FcTopBind (copyBind Map.empty bind)
        _ -> topBind

copyBind :: Map Var Var -> FcBind -> FcBind
copyBind aliases bind =
  case bind of
    FcNonRec binder rhs ->
      FcNonRec binder (copyExpr aliases rhs)
    FcRec bindings ->
      let innerAliases = removeAliases (map fst bindings) aliases
       in FcRec [(binder, copyExpr innerAliases rhs) | (binder, rhs) <- bindings]

copyExpr :: Map Var Var -> FcExpr -> FcExpr
copyExpr aliases expression =
  case expression of
    FcVar var -> FcVar (resolveAlias aliases var)
    FcLit {} -> expression
    FcApp function argument -> FcApp (copyExpr aliases function) (copyExpr aliases argument)
    FcTyApp function ty -> FcTyApp (copyExpr aliases function) ty
    FcLam binder body -> FcLam binder (copyExpr (Map.delete binder aliases) body)
    FcTyLam tyVar body -> FcTyLam tyVar (copyExpr aliases body)
    FcLet (FcNonRec binder (FcVar source)) body ->
      copyExpr (Map.insert binder (resolveAlias aliases source) aliases) body
    FcLet (FcRec [(binder, FcVar source)]) body
      | binder /= source ->
          copyExpr (Map.insert binder (resolveAlias aliases source) aliases) body
    FcLet bind@(FcNonRec binder _) body ->
      FcLet
        (copyBind aliases bind)
        (copyExpr (Map.delete binder aliases) body)
    FcLet bind@(FcRec bindings) body ->
      let binders = map fst bindings
          innerAliases = removeAliases binders aliases
       in FcLet (copyBind innerAliases bind) (copyExpr innerAliases body)
    FcCase scrutinee binder alternatives ->
      FcCase
        (copyExpr aliases scrutinee)
        binder
        (map (copyAlt (Map.delete binder aliases)) alternatives)
    FcCast inner coercion -> FcCast (copyExpr aliases inner) coercion
    FcCallForeign foreignCall arguments ->
      FcCallForeign foreignCall (map (copyExpr aliases) arguments)

copyAlt :: Map Var Var -> FcAlt -> FcAlt
copyAlt aliases alternative =
  alternative
    { altRhs =
        copyExpr
          (removeAliases (altBinders alternative) aliases)
          (altRhs alternative)
    }

removeAliases :: [Var] -> Map Var Var -> Map Var Var
removeAliases binders aliases = foldr Map.delete aliases binders

resolveAlias :: Map Var Var -> Var -> Var
resolveAlias aliases var =
  case Map.lookup var aliases of
    Just target -> resolveAlias aliases target
    Nothing -> var

-- | Remove lazy non-recursive bindings whose values are never demanded. An
-- unlifted binding is strict, so its right-hand side must be retained even
-- when the binder is absent from the body.
eliminateDeadLetsProgram :: FcProgram -> FcProgram
eliminateDeadLetsProgram (FcProgram topBinds) = FcProgram (map eliminateTopBind topBinds)
  where
    eliminateTopBind topBind =
      case topBind of
        FcTopBind bind -> FcTopBind (eliminateBind bind)
        _ -> topBind

eliminateBind :: FcBind -> FcBind
eliminateBind bind =
  case bind of
    FcNonRec binder rhs -> FcNonRec binder (eliminateExpr rhs)
    FcRec bindings -> FcRec [(binder, eliminateExpr rhs) | (binder, rhs) <- bindings]

eliminateExpr :: FcExpr -> FcExpr
eliminateExpr expression =
  case expression of
    FcVar {} -> expression
    FcLit {} -> expression
    FcApp function argument -> FcApp (eliminateExpr function) (eliminateExpr argument)
    FcTyApp function ty -> FcTyApp (eliminateExpr function) ty
    FcLam binder body -> FcLam binder (eliminateExpr body)
    FcTyLam tyVar body -> FcTyLam tyVar (eliminateExpr body)
    FcLet (FcNonRec binder rhs) body
      | Dead <- countExprVar binder body,
        isLiftedType (varType binder) ->
          eliminateExpr body
      | otherwise -> FcLet (FcNonRec binder (eliminateExpr rhs)) (eliminateExpr body)
    FcLet bind body -> FcLet (eliminateBind bind) (eliminateExpr body)
    FcCase scrutinee binder alternatives ->
      FcCase
        (eliminateExpr scrutinee)
        binder
        [alternative {altRhs = eliminateExpr (altRhs alternative)} | alternative <- alternatives]
    FcCast inner coercion -> FcCast (eliminateExpr inner) coercion
    FcCallForeign foreignCall arguments -> FcCallForeign foreignCall (map eliminateExpr arguments)
