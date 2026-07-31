-- | System FC simplification and equality-saturation optimization.
--
-- Local canonicalizations run to a fixpoint. Binding-aware inlining separately
-- generates equivalent alternatives and uses an e-graph cost model to decide
-- whether the exposed optimization opportunities justify the added code.
module Aihc.Fc.Optimize
  ( optimizeProgram,
  )
where

import Aihc.Fc.Optimize.Inline (inlineCandidatesProgram)
import Aihc.Fc.Syntax
import Aihc.Tc.Types (Unique)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- Linked compilation units currently have independently allocated uniques.
-- Include the printed name so an alias in one unit cannot capture an unrelated
-- constructor or global imported from another unit.
type VarKey = (Unique, Text)

-- | Canonicalize existing aliases, then explore sharing-safe inlining
-- alternatives. Generated non-trivial lets remain explicit sharing points.
optimizeProgram :: FcProgram -> FcProgram
optimizeProgram = inlineCandidatesProgram . simplifyAliases
  where
    simplifyAliases = untilStable copyPropagateProgram

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

copyBind :: Map VarKey Var -> FcBind -> FcBind
copyBind aliases bind =
  case bind of
    FcNonRec binder rhs ->
      FcNonRec binder (copyExpr aliases rhs)
    FcRec bindings ->
      let innerAliases = removeAliases (map fst bindings) aliases
       in FcRec [(binder, copyExpr innerAliases rhs) | (binder, rhs) <- bindings]

copyExpr :: Map VarKey Var -> FcExpr -> FcExpr
copyExpr aliases expression =
  case expression of
    FcVar var -> FcVar (resolveAlias aliases var)
    FcLit {} -> expression
    FcApp function argument -> FcApp (copyExpr aliases function) (copyExpr aliases argument)
    FcTyApp function ty -> FcTyApp (copyExpr aliases function) ty
    FcLam binder body -> FcLam binder (copyExpr (Map.delete (varKey binder) aliases) body)
    FcTyLam tyVar body -> FcTyLam tyVar (copyExpr aliases body)
    FcLet (FcNonRec binder (FcVar source)) body ->
      copyExpr (Map.insert (varKey binder) (resolveAlias aliases source) aliases) body
    FcLet (FcRec [(binder, FcVar source)]) body
      | varKey binder /= varKey source ->
          copyExpr (Map.insert (varKey binder) (resolveAlias aliases source) aliases) body
    FcLet bind@(FcNonRec binder _) body ->
      FcLet
        (copyBind aliases bind)
        (copyExpr (Map.delete (varKey binder) aliases) body)
    FcLet bind@(FcRec bindings) body ->
      let binders = map fst bindings
          innerAliases = removeAliases binders aliases
       in FcLet (copyBind innerAliases bind) (copyExpr innerAliases body)
    FcCase scrutinee binder alternatives ->
      FcCase
        (copyExpr aliases scrutinee)
        binder
        (map (copyAlt (Map.delete (varKey binder) aliases)) alternatives)
    FcCast inner coercion -> FcCast (copyExpr aliases inner) coercion
    FcCallForeign foreignCall arguments ->
      FcCallForeign foreignCall (map (copyExpr aliases) arguments)

copyAlt :: Map VarKey Var -> FcAlt -> FcAlt
copyAlt aliases alternative =
  alternative
    { altRhs =
        copyExpr
          (removeAliases (altBinders alternative) aliases)
          (altRhs alternative)
    }

removeAliases :: [Var] -> Map VarKey Var -> Map VarKey Var
removeAliases binders aliases = foldr (Map.delete . varKey) aliases binders

resolveAlias :: Map VarKey Var -> Var -> Var
resolveAlias aliases var =
  case Map.lookup (varKey var) aliases of
    Just target -> resolveAlias aliases target
    Nothing -> var

varKey :: Var -> VarKey
varKey var = (varUnique var, varName var)
