-- | Constraint canonicalization.
--
-- Turns messy constraints into normalized forms suitable for the solver.
-- For the MVP, this mainly orients equalities (meta on left) and
-- classifies constraints.
module Aihc.Tc.Solve.Canonicalize
  ( canonicalize,
    classifyCt,
    CanonResult (..),
  )
where

import Aihc.Tc.Constraint
import Aihc.Tc.Types

-- | Result of canonicalization.
data CanonResult
  = -- | Produce canonical equality constraints.
    CanonEqs ![Ct]
  | -- | Produce a canonical dictionary constraint.
    CanonDict !Ct
  | -- | Constraint is already solved (trivially true).
    CanonSolved
  deriving (Show)

-- | Canonicalize a constraint.
canonicalize :: Ct -> CanonResult
canonicalize ct = case ctPred ct of
  EqPred t1 t2 -> canonEq ct t1 t2
  ClassPred {} -> CanonDict ct

-- | Canonicalize an equality constraint.
canonEq :: Ct -> TcType -> TcType -> CanonResult
canonEq ct t1 t2 = case (t1, t2) of
  -- Reflexive: solved.
  _ | t1 == t2 -> CanonSolved
  -- Orient: meta on left.
  (TcMetaTv _, _) -> CanonEqs [ct]
  (_, TcMetaTv _) ->
    CanonEqs [ct {ctPred = EqPred t2 t1}]
  -- Decompose type constructor application.
  (TcTyCon tc1 args1, TcTyCon tc2 args2)
    | tc1 == tc2,
      length args1 == length args2 ->
        CanonEqs
          [ ct {ctPred = EqPred a1 a2}
          | (a1, a2) <- zip args1 args2
          ]
  -- Decompose function type.
  (TcFunTy a1 b1, TcFunTy a2 b2) ->
    CanonEqs
      [ ct {ctPred = EqPred a1 a2},
        ct {ctPred = EqPred b1 b2}
      ]
  -- Decompose type application.
  (TcAppTy f1 a1, TcAppTy f2 a2) ->
    CanonEqs
      [ ct {ctPred = EqPred f1 f2},
        ct {ctPred = EqPred a1 a2}
      ]
  -- Cannot decompose further: leave as-is.
  _ -> CanonEqs [ct]

-- | Classify a constraint as equality or dictionary.
classifyCt :: Ct -> Either Ct Ct
classifyCt ct = case ctPred ct of
  EqPred {} -> Left ct
  ClassPred {} -> Right ct
