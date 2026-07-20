-- | Small, monotone System FC simplifications.
--
-- Every optimization in this module must strictly simplify the Core program:
-- rules may remove structure, but must not trade one form for another form of
-- equal complexity. This makes it safe to run the complete rule set to a
-- fixpoint and keeps interactions between independent rules predictable.
module Aihc.Fc.Optimize
  ( optimizeProgram,
  )
where

import Aihc.Fc.Syntax
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

type CoreOptimization = FcProgram -> FcProgram

-- | Apply every Core simplification until a complete pass makes no change.
optimizeProgram :: FcProgram -> FcProgram
optimizeProgram = untilStable runOptimizations
  where
    runOptimizations program = List.foldl' (flip ($)) program coreOptimizations

-- Keep this list ordered from local canonicalizations to broader rewrites so
-- later rules see the simplest output available from earlier rules.
coreOptimizations :: [CoreOptimization]
coreOptimizations = [copyPropagateProgram]

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
