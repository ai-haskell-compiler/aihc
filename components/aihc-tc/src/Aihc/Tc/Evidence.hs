-- | Evidence terms and coercions.
--
-- Evidence should be a first-class IR concept from day one.
-- Class constraints elaborate to dictionaries, equality constraints
-- elaborate to coercions.
module Aihc.Tc.Evidence
  ( -- * Evidence variables
    EvVar (..),

    -- * Evidence terms
    EvTerm (..),
    EvBinding (..),

    -- * Coercions
    Coercion (..),
  )
where

import Aihc.Tc.Types
import Data.Text (Text)

-- | An evidence variable, uniquely identified.
newtype EvVar = EvVar {evVarUnique :: Unique}
  deriving (Eq, Ord, Show)

-- | Evidence terms.
--
-- Every wanted constraint gets an evidence variable; the solver fills it.
-- The evidence is then used during elaboration to produce dictionary-passing
-- and coercion-carrying core.
data EvTerm
  = -- | Reference to an evidence variable (given or previously solved).
    EvVarTerm !EvVar
  | -- | Dictionary construction: class name, type args, sub-evidence.
    EvDict !Text ![TcType] ![EvTerm]
  | -- | Coercion evidence (for equality constraints).
    EvCoercion !Coercion
  | -- | Superclass selection from a dictionary.
    EvSuperClass !EvTerm !Int
  | -- | Cast evidence through a coercion.
    EvCast !EvTerm !Coercion
  deriving (Eq, Show)

-- | A binding of an evidence variable to its term.
data EvBinding = EvBinding
  { evBindVar :: !EvVar,
    evBindTerm :: !EvTerm
  }
  deriving (Eq, Show)

-- | Coercions for type equality evidence.
--
-- The full coercion language will grow to support GADTs and type families.
-- For the MVP, only 'Refl' and 'CoVar' are actively produced by the solver.
data Coercion
  = -- | Coercion variable.
    CoVar !EvVar
  | -- | Reflexivity: @t ~ t@.
    Refl !TcType
  | -- | Symmetry: if @co : t1 ~ t2@ then @Sym co : t2 ~ t1@.
    Sym !Coercion
  | -- | Transitivity: @Trans co1 co2 : t1 ~ t3@.
    Trans !Coercion !Coercion
  | -- | Lift through type constructor: @T co1 ... con@.
    TyConAppCo !TyCon ![Coercion]
  | -- | Type family / newtype axiom instantiation (future).
    AxiomInstCo !Text ![TcType]
  deriving (Eq, Show)
