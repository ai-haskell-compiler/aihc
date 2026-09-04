-- | Administrative-normal-form normalization for GRIN.
--
-- Two rules run as one pass over each function body.
--
-- Sequencing becomes one flat spine. GRIN binds may syntactically contain
-- another bind as their value expression, so
--
-- @
-- x <- (y <- value; inner)
-- body
-- @
--
-- is reassociated into:
--
-- @
-- y <- value
-- x <- inner
-- body
-- @
--
-- A bind that only names a value it already has is then dropped. Lowering
-- names every intermediate result, so an operand that is already a value
-- still gets a bind of its own:
--
-- @
-- argument :: IntRep <- constant (rightInt :: IntRep)
-- store (CIS (argument :: IntRep))
-- @
--
-- Such a bind computes nothing, so it disappears and its values are
-- substituted for the names it bound:
--
-- @
-- store (CIS (rightInt :: IntRep))
-- @
--
-- The first rule feeds the second: the constant an expression ends in is
-- usually buried under the binds that compute its operands, and flattening is
-- what brings it into the tail position where the bind above it can see it. A
-- 'GrinConstant' that ends a body is that body's result rather than a bind,
-- and always stays.
--
-- 'GrinVar' uniques make the harmless scope extension of @y@ unambiguous.
-- Substitution does not depend on them: a binder that shadows a substituted
-- name, or a name a substitution stands for, drops the entries it would
-- otherwise capture.
module Aihc.Grin.Anf
  ( normalizeGrinProgram,
    normalizeGrinExpr,
  )
where

import Aihc.Grin.Syntax
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

-- | The values that stand for the names dropped copy binds used to bind.
type Substitution = Map GrinVar GrinValue

-- | Normalize every function body in a GRIN program.
normalizeGrinProgram :: GrinProgram -> GrinProgram
normalizeGrinProgram program =
  program
    { grinFunctions = map normalizeFunction (grinFunctions program)
    }
  where
    normalizeFunction function =
      function {grinFunctionBody = normalizeGrinExpr (grinFunctionBody function)}

-- | Flatten the bind spine of an expression and drop its copy binds.
--
-- Case alternatives and recursive-allocation bodies form their own spines and
-- are normalized recursively. Cases are deliberately not distributed through
-- binds: doing so would duplicate the bind body.
normalizeGrinExpr :: GrinExpr -> GrinExpr
normalizeGrinExpr expression = normalizeInto Map.empty expression (\_ tailExpression -> tailExpression)
  where
    -- Passing the enclosing spine as a function reassociates every bind in
    -- linear time. Repeatedly peeling an already-normalized value expression
    -- would make deeply nested constructor applications quadratic.
    --
    -- The continuation also receives the substitution in force after the
    -- value expression, because a copy bind dropped inside it binds names the
    -- rest of the spine still mentions.
    normalizeInto substitution current continue =
      case current of
        GrinBind resultVars valueExpression body ->
          normalizeInto substitution valueExpression $ \valueSubstitution normalizedValue ->
            case normalizedValue of
              GrinConstant values
                | Just extended <- extend valueSubstitution resultVars values ->
                    normalizeInto extended body continue
              _ ->
                GrinBind
                  resultVars
                  normalizedValue
                  (normalizeInto (shadow resultVars valueSubstitution) body continue)
        GrinStoreRec bindings body -> storeRec GrinStoreRec bindings body
        GrinStoreRecUnchecked bindings body -> storeRec GrinStoreRecUnchecked bindings body
        GrinCase scrutinee binder alternatives ->
          continue
            substitution
            ( GrinCase
                (useValue substitution scrutinee)
                binder
                (map (normalizeAlternative (shadow [binder] substitution)) alternatives)
            )
        _ -> continue substitution (mapExprValues (useValue substitution) current)
      where
        -- A recursive group's names are in scope for its own nodes as well as
        -- for its body.
        storeRec rebuild bindings body =
          let inner = shadow (map fst bindings) substitution
              nodes = [(var, useNode inner node) | (var, node) <- bindings]
           in continue substitution (rebuild nodes (normalizeExpr inner body))

    normalizeAlternative substitution alternative =
      alternative
        { grinAltRhs =
            normalizeExpr (shadow (grinAltBinders alternative) substitution) (grinAltRhs alternative)
        }

    normalizeExpr substitution expression' =
      normalizeInto substitution expression' (\_ tailExpression -> tailExpression)

-- | Record a copy bind, or refuse it when the bind is not a plain renaming.
--
-- A count mismatch or a representation change means the bind carries meaning
-- beyond naming a value, so it stays where it is.
extend :: Substitution -> [GrinVar] -> [GrinValue] -> Maybe Substitution
extend substitution vars values
  | length vars /= length values = Nothing
  | not (and (zipWith sameRep vars substituted)) = Nothing
  | otherwise = Just (foldl' record (shadow vars substitution) (zip vars substituted))
  where
    substituted = map (useValue substitution) values
    sameRep var value = grinVarRuntimeRep var == grinValueRuntimeRep value
    record current (var, value) = Map.insert var value current

-- | Forget every entry a group of binders would capture.
--
-- A binder that rebinds a substituted name ends that substitution, and one
-- that rebinds a name some substitution stands for would silently change which
-- variable that substitution means.
shadow :: [GrinVar] -> Substitution -> Substitution
shadow vars substitution
  | null vars || Map.null substitution = substitution
  | otherwise = Map.filterWithKey keep substitution
  where
    shadowed = Set.fromList vars
    keep var value = not (Set.member var shadowed) && not (captures value)
    captures value =
      case value of
        GrinVarValue var -> Set.member var shadowed
        GrinGlobalValue {} -> False
        GrinLitValue {} -> False

useNode :: Substitution -> GrinNode -> GrinNode
useNode substitution node =
  node {grinNodeFields = map (useValue substitution) (grinNodeFields node)}

useValue :: Substitution -> GrinValue -> GrinValue
useValue substitution value =
  case value of
    GrinVarValue var -> Map.findWithDefault value var substitution
    GrinGlobalValue {} -> value
    GrinLitValue {} -> value

-- | Rewrite the values an expression mentions directly, leaving the
-- expressions nested inside it alone.
mapExprValues :: (GrinValue -> GrinValue) -> GrinExpr -> GrinExpr
mapExprValues f expression =
  case expression of
    GrinConstant values -> GrinConstant (map f values)
    GrinBind vars valueExpression body -> GrinBind vars valueExpression body
    GrinStore node -> GrinStore (node' node)
    GrinEnsureHeap requiredWords roots -> GrinEnsureHeap (f requiredWords) (map f roots)
    GrinStoreUnchecked node -> GrinStoreUnchecked (node' node)
    GrinStoreRec bindings body -> GrinStoreRec (nodes bindings) body
    GrinStoreRecUnchecked bindings body -> GrinStoreRecUnchecked (nodes bindings) body
    GrinUpdate pointer value -> GrinUpdate (f pointer) (f value)
    GrinEval runtimeRep value -> GrinEval runtimeRep (f value)
    GrinCpsEval runtimeRep value continuation updateContinuation ->
      GrinCpsEval runtimeRep (f value) (f continuation) (f updateContinuation)
    GrinCall runtimeRep functionName arguments -> GrinCall runtimeRep functionName (map f arguments)
    GrinPrimitiveCall runtimeRep name arguments -> GrinPrimitiveCall runtimeRep name (map f arguments)
    GrinCpsPrimitiveCall runtimeRep name arguments continuation ->
      GrinCpsPrimitiveCall runtimeRep name (map f arguments) (f continuation)
    GrinApply runtimeRep function arguments -> GrinApply runtimeRep (f function) (map f arguments)
    GrinCpsApply runtimeRep function arguments continuation ->
      GrinCpsApply runtimeRep (f function) (map f arguments) (f continuation)
    GrinContinue continuation arguments -> GrinContinue (f continuation) (map f arguments)
    GrinCpsRaise exception continuation -> GrinCpsRaise (f exception) (f continuation)
    GrinUpdateBlackhole pointer value -> GrinUpdateBlackhole (f pointer) (f value)
    GrinHalt values -> GrinHalt (map f values)
    GrinExit status -> GrinExit (f status)
    GrinCase scrutinee binder alternatives -> GrinCase (f scrutinee) binder alternatives
    GrinThrow exception -> GrinThrow (f exception)
    GrinCatch runtimeRep action handler state ->
      GrinCatch runtimeRep (f action) (f handler) (map f state)
    GrinForeignCallExpr foreignCall arguments -> GrinForeignCallExpr foreignCall (map f arguments)
  where
    node' node = node {grinNodeFields = map f (grinNodeFields node)}
    nodes bindings = [(var, node' node) | (var, node) <- bindings]
