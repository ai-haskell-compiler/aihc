-- | Error types for the type checker.
module Aihc.Tc.Error
  ( TcDiagnostic (..),
    TcErrorKind (..),
    TcSeverity (..),
  )
where

import Aihc.Parser.Syntax (SourceSpan)
import Aihc.Tc.Constraint (CtOrigin, EqProvenance)
import Aihc.Tc.Types
import Data.Text (Text)

-- | A diagnostic produced by the type checker.
--
-- Source locations are display metadata, not the identity of the diagnostic.
-- A diagnostic can be attached to an AST even when the input did not preserve
-- source spans.
data TcDiagnostic = TcDiagnostic
  { diagLoc :: !(Maybe SourceSpan),
    diagSeverity :: !TcSeverity,
    diagKind :: !TcErrorKind
  }
  deriving (Show)

-- | Severity of a diagnostic.
data TcSeverity
  = TcError
  | TcWarning
  deriving (Eq, Show)

-- | Kinds of type checking errors.
data TcErrorKind
  = -- | Could not unify two types.
    UnificationError !TcType !TcType !CtOrigin !(Maybe EqProvenance)
  | -- | Occurs check failure (infinite type).
    OccursCheckError !Unique !TcType
  | -- | Unbound variable.
    UnboundVariable !String
  | -- | Type-level expression has a different kind than expected.
    KindMismatch !Kind !Kind
  | -- | Unsolved wanted constraint.
    UnsolvedWanted !Pred !CtOrigin
  | -- | A source top-level value has an unlifted runtime representation.
    TopLevelUnliftedBinding !Text !TcType
  | -- | Other error with a message.
    OtherError !String
  deriving (Show)
