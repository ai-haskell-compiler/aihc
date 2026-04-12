-- | Error types for the type checker.
module Aihc.Tc.Error
  ( TcDiagnostic (..),
    TcErrorKind (..),
    TcSeverity (..),
  )
where

import Aihc.Parser.Syntax (SourceSpan)
import Aihc.Tc.Constraint (CtOrigin)
import Aihc.Tc.Types

-- | A diagnostic produced by the type checker.
data TcDiagnostic = TcDiagnostic
  { diagLoc :: !SourceSpan,
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
    UnificationError !TcType !TcType !CtOrigin
  | -- | Occurs check failure (infinite type).
    OccursCheckError !Unique !TcType
  | -- | Unbound variable.
    UnboundVariable !String
  | -- | Unsolved wanted constraint.
    UnsolvedWanted !Pred !CtOrigin
  | -- | Other error with a message.
    OtherError !String
  deriving (Show)
