{-# LANGUAGE DeriveTraversable #-}

-- | The e-graph boundary for System FC optimization.
--
-- Binding-aware rewrites are produced outside the e-graph, where fresh names
-- and call-by-need substitution can be handled explicitly. For now each whole
-- expression is an opaque e-node. This is deliberate: recursively interning a
-- named, binder-rich syntax tree would let extraction combine subexpressions
-- from different scopes. A future structural representation must use a
-- scope-safe encoding such as De Bruijn indices.
module Aihc.Fc.Optimize.EGraph
  ( selectSmallest,
  )
where

import Aihc.Fc.Syntax
import Control.Monad (forM_, void)
import Data.Equality.Extraction (extractBest)
import Data.Equality.Graph.Classes (ClassId)
import Data.Equality.Graph.Monad qualified as EGraph
import Data.Equality.Utils (Fix (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- | An opaque, scope-preserving candidate and its complete lowered-size cost.
-- There are no recursive children yet, so an e-class can select alternatives
-- without accidentally moving a bound variable across its scope.
data FcAlternativeF child
  = FcAlternativeF !(Stable FcExpr) !Int
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-- Core's identity instances intentionally compare only uniques. That is useful
-- inside one freshly generated unit, but linked units are not guaranteed to
-- have collision-free uniques. E-graph node ordering must be stronger: if two
-- payloads compare equal, congruence will treat them as the same syntax. The
-- complete readable representation includes names, uniques, kinds, and types
-- and is therefore a safe proof-of-concept key.
newtype Stable value = Stable {stableValue :: value}
  deriving (Show)

instance (Show value) => Eq (Stable value) where
  left == right = show (stableValue left) == show (stableValue right)

instance (Show value) => Ord (Stable value) where
  compare left right = compare (show (stableValue left)) (show (stableValue right))

-- | Put all proven-equivalent candidates in one e-class and select the term
-- whose cost best predicts the size of the strict GRIN produced downstream.
selectSmallest :: Map Text Int -> FcExpr -> [FcExpr] -> FcExpr
selectSmallest callCosts original candidates =
  fromFix (extractBest graph alternativeCost root)
  where
    (root, graph) = EGraph.egraph (buildAlternatives callCosts original candidates)

buildAlternatives :: Map Text Int -> FcExpr -> [FcExpr] -> EGraph.EGraphM () FcAlternativeF ClassId
buildAlternatives callCosts original candidates = do
  originalClass <- EGraph.represent (toFix callCosts original)
  forM_ candidates $ \candidate -> do
    candidateClass <- EGraph.represent (toFix callCosts candidate)
    void (EGraph.merge originalClass candidateClass)
  EGraph.rebuild
  pure originalClass

alternativeCost :: FcAlternativeF Int -> Int
alternativeCost (FcAlternativeF _ cost) = cost

-- | Types and casts disappear before runtime code generation, while calls,
-- closures, lets, and cases all survive in some form. This deliberately
-- optimizes for generated-code size rather than raw System FC node count.
loweredSizeCost :: Map Text Int -> FcExpr -> Int
loweredSizeCost callCosts expression =
  case expression of
    FcVar var -> Map.findWithDefault 1 (varName var) callCosts
    FcLit {} -> 1
    FcApp function argument -> loweredSizeCost callCosts function + loweredSizeCost callCosts argument + 2
    FcTyApp function _ -> loweredSizeCost callCosts function
    FcLam _ body -> loweredSizeCost callCosts body + 2
    FcTyLam _ body -> loweredSizeCost callCosts body
    FcLet (FcNonRec _ rhs) body -> loweredSizeCost callCosts rhs + loweredSizeCost callCosts body + 1
    FcLet (FcRec bindings) body ->
      sum [loweredSizeCost callCosts rhs | (_, rhs) <- bindings]
        + loweredSizeCost callCosts body
        + 3
    FcCase scrutinee _ alternatives ->
      loweredSizeCost callCosts scrutinee
        + sum [loweredSizeCost callCosts (altRhs alternative) | alternative <- alternatives]
        + 2
    FcCast inner _ -> loweredSizeCost callCosts inner
    FcCallForeign _ arguments -> sum (map (loweredSizeCost callCosts) arguments) + 2

toFix :: Map Text Int -> FcExpr -> Fix FcAlternativeF
toFix callCosts expression =
  Fix (FcAlternativeF (Stable expression) (loweredSizeCost callCosts expression))

fromFix :: Fix FcAlternativeF -> FcExpr
fromFix (Fix (FcAlternativeF expression _)) = stableValue expression
