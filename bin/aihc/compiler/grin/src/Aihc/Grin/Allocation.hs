-- | Place heap allocations inside the case alternatives that use them.
-- The code of a thunk or closure stays in its separate function.
module Aihc.Grin.Allocation
  ( placeGrinAllocations,
  )
where

import Aihc.Grin.Analysis (freeExprVars, freeNodeVars, freeValueVars)
import Aihc.Grin.Syntax
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set

-- | Move plain stores before CPS conversion and GC safepoint insertion.
-- Each selected path retains one shared object. Recursive groups stay intact.
placeGrinAllocations :: GrinProgram -> GrinProgram
placeGrinAllocations program =
  program {grinFunctions = map placeFunction (grinFunctions program)}
  where
    placeFunction function = function {grinFunctionBody = place (grinFunctionBody function)}
    place expression =
      case expression of
        GrinBind [pointer] (GrinStore node) body ->
          let body' = place body
           in fromMaybe (GrinBind [pointer] (GrinStore node) body') (move pointer node body')
        GrinBind vars value body -> GrinBind vars (place value) (place body)
        GrinStoreRec nodes body -> GrinStoreRec nodes (place body)
        GrinCase value binder alternatives ->
          GrinCase value binder [alternative {grinAltRhs = place (grinAltRhs alternative)} | alternative <- alternatives]
        _ -> expression

-- | Cross a bind only when it neither uses the pointer nor captures an operand.
-- Stop at every other use, including aliases, calls, updates, and captures.
-- A successful move must omit the allocation from at least one path. A case
-- whose alternatives all use the pointer still takes a copy in each of
-- them when a case inside one of them has a path without a use. The copies
-- are in separate alternatives, so each path still allocates once at most.
--
-- The hot path of a scanning loop often evaluates a thunk only through a
-- direct call of its code, after 'Aihc.Grin.Simplify' replaced a
-- single-entry @eval@ of the known thunk. The stored object is then dead
-- on that path, and only a rarer path, such as the inside of a string
-- literal, passes it on.
move :: GrinVar -> GrinNode -> GrinExpr -> Maybe GrinExpr
move pointer node = go
  where
    protected = Set.insert pointer (freeNodeVars node)
    safe vars = Set.disjoint protected (Set.fromList vars)
    uses = Set.member pointer . freeExprVars
    wrap body
      | body == GrinConstant [GrinVarValue pointer] = GrinStore node
      | otherwise = GrinBind [pointer] (GrinStore node) body
    placeBranch body
      | not (uses body) = body
      | otherwise = fromMaybe (wrap body) (go body)
    go expression =
      case expression of
        GrinBind vars value body
          | safe vars,
            not (uses value) ->
              GrinBind vars value <$> go body
          | not (uses body) -> (\value' -> GrinBind vars value' body) <$> go value
        GrinCase value binder alternatives
          | movable expression ->
              Just (GrinCase value binder [alternative {grinAltRhs = placeBranch (grinAltRhs alternative)} | alternative <- alternatives])
        _ -> Nothing
    -- Whether the store can enter a case, and a path through it does not
    -- use the pointer.
    movable expression =
      case expression of
        GrinCase value binder alternatives ->
          Set.notMember pointer (freeValueVars value)
            && safe (binder : concatMap grinAltBinders alternatives)
            && any (avoids . grinAltRhs) alternatives
        _ -> False
    -- Whether a path through the expression does not use the pointer, past
    -- the binds and into the cases that the move can cross and enter.
    avoids expression = not (uses expression) || avoidsUsed expression
    avoidsUsed expression =
      case expression of
        GrinBind vars value body
          | safe vars,
            not (uses value) ->
              avoidsUsed body
          | not (uses body) -> avoidsUsed value
        _ -> movable expression
