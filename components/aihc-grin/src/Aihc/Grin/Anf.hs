-- | Administrative-normal-form normalization for GRIN.
--
-- GRIN binds may syntactically contain another bind as their value expression.
-- Reassociate such binds so that sequencing is represented by one flat spine:
--
-- @
-- x <- (y <- value; inner)
-- body
-- @
--
-- becomes:
--
-- @
-- y <- value
-- x <- inner
-- body
-- @
--
-- 'GrinVar' uniques make the harmless scope extension of @y@ unambiguous.
module Aihc.Grin.Anf
  ( normalizeGrinProgram,
    normalizeGrinExpr,
  )
where

import Aihc.Grin.Syntax

-- | Normalize every function body in a GRIN program.
normalizeGrinProgram :: GrinProgram -> GrinProgram
normalizeGrinProgram program =
  program
    { grinFunctions = map normalizeFunction (grinFunctions program)
    }
  where
    normalizeFunction function =
      function {grinFunctionBody = normalizeGrinExpr (grinFunctionBody function)}

-- | Reassociate nested binds into a flat sequencing spine.
--
-- Case alternatives and recursive-allocation bodies form their own spines and
-- are normalized recursively. Cases are deliberately not distributed through
-- binds: doing so would duplicate the bind body.
normalizeGrinExpr :: GrinExpr -> GrinExpr
normalizeGrinExpr expression = normalizeInto expression id
  where
    -- Passing the enclosing spine as a function reassociates every bind in
    -- linear time. Repeatedly peeling an already-normalized value expression
    -- would make deeply nested constructor applications quadratic.
    normalizeInto current continue =
      case current of
        GrinBind resultVars valueExpression body ->
          normalizeInto valueExpression $ \normalizedValue ->
            GrinBind resultVars normalizedValue (normalizeInto body continue)
        GrinStoreRec bindings body ->
          continue (GrinStoreRec bindings (normalizeInto body id))
        GrinStoreRecUnchecked bindings body ->
          continue (GrinStoreRecUnchecked bindings (normalizeInto body id))
        GrinCase scrutinee binder alternatives ->
          continue (GrinCase scrutinee binder (map normalizeAlternative alternatives))
        _ -> continue current

    normalizeAlternative alternative =
      alternative {grinAltRhs = normalizeGrinExpr (grinAltRhs alternative)}
