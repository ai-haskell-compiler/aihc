-- | Constraint representation for the OutsideIn(X) solver.
--
-- The internal constraint language is richer than just 'Pred' because
-- OutsideIn(X) needs implications.
module Aihc.Tc.Constraint
  ( -- * Constraint flavors
    CtFlavor (..),

    -- * Constraint origins
    CtOrigin (..),

    -- * Constraints
    Ct (..),
    mkWantedCt,

    -- * Implications
    Implication (..),
  )
where

import Aihc.Parser.Syntax (SourceSpan)
import Aihc.Tc.Evidence (EvVar)
import Aihc.Tc.Types
import Data.Text (Text)

-- | The flavor of a constraint.
data CtFlavor
  = -- | Given: from GADT match, user annotation, or superclass.
    Given
  | -- | Wanted: must be solved to typecheck the program.
    Wanted
  | -- | Derived: inferred, no evidence needed.
    Derived
  deriving (Eq, Show)

-- | The origin of a constraint, for error reporting.
--
-- Every constraint carries origin data so that error messages can explain
-- where the constraint came from.
data CtOrigin
  = OccurrenceOf !Text
  | AppOrigin !SourceSpan
  | LambdaOrigin !SourceSpan
  | LetOrigin !SourceSpan
  | LitOrigin !SourceSpan
  | SigOrigin !SourceSpan
  | CaseBranchOrigin !SourceSpan
  | InstOrigin !Text
  | UnifyOrigin !SourceSpan
  deriving (Eq, Show)

-- | A constraint to be solved.
data Ct = Ct
  { ctPred :: !Pred,
    ctFlavor :: !CtFlavor,
    ctEvVar :: !EvVar,
    ctOrigin :: !CtOrigin,
    ctLoc :: !SourceSpan
  }
  deriving (Show)

-- | Create a wanted constraint.
mkWantedCt :: Pred -> EvVar -> CtOrigin -> SourceSpan -> Ct
mkWantedCt p ev orig loc =
  Ct
    { ctPred = p,
      ctFlavor = Wanted,
      ctEvVar = ev,
      ctOrigin = orig,
      ctLoc = loc
    }

-- | An implication constraint.
--
-- This is one of the most important data types in the whole design.
-- It represents branch-local reasoning scoped inside implications,
-- which is the heart of OutsideIn(X).
data Implication = Implication
  { -- | Skolem type variables introduced by this implication.
    implSkols :: ![TyVarId],
    -- | Evidence variables for given constraints.
    implGivenEvs :: ![EvVar],
    -- | Given constraints (from GADT match, etc.).
    implGivenCts :: ![Ct],
    -- | Wanted constraints to solve under this implication.
    implWantedCts :: ![Ct],
    -- | Nesting level.
    implTcLevel :: !TcLevel,
    -- | What caused this implication.
    implInfo :: !CtOrigin
  }
  deriving (Show)
