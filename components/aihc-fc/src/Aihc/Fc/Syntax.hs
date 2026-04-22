-- | System FC core language.
--
-- System FC extends System F with explicit coercions (proofs of type
-- equality). In this core language:
--
-- * All type variables are explicit (type abstraction and application).
-- * Type classes are explicit as dictionary arguments.
-- * All syntactic sugar is removed.
-- * Coercions witness type equalities and enable safe casts.
--
-- Verifying the types in System FC is simple structural type checking,
-- used for internal consistency checks.
module Aihc.Fc.Syntax
  ( -- * Core expressions
    FcExpr (..),

    -- * Variables
    Var (..),

    -- * Bindings
    FcBind (..),
    FcTopBind (..),
    FcProgram (..),

    -- * Case alternatives
    FcAlt (..),
    FcAltCon (..),

    -- * Literals
    Literal (..),
  )
where

import Aihc.Tc.Evidence (Coercion)
import Aihc.Tc.Types (TcType, TyVarId, Unique)
import Data.Text (Text)

-- | A System FC program: a collection of top-level bindings.
newtype FcProgram = FcProgram
  { -- | Top-level bindings in dependency order.
    fcTopBinds :: [FcTopBind]
  }
  deriving (Eq, Show)

-- | A top-level binding.
data FcTopBind
  = -- | Data type declaration: type name, type variable parameters,
    -- list of (constructor name, field types).
    FcData !Text ![TyVarId] ![(Text, [TcType])]
  | -- | Value binding.
    FcTopBind !FcBind
  deriving (Eq, Show)

-- | A typed variable.
data Var = Var
  { varName :: !Text,
    varUnique :: !Unique,
    varType :: !TcType
  }
  deriving (Show)

-- Eq/Ord on Unique only, mirroring TyVarId.
instance Eq Var where
  a == b = varUnique a == varUnique b

instance Ord Var where
  compare a b = compare (varUnique a) (varUnique b)

-- | System FC core expression.
--
-- Every binding is explicit. No syntactic sugar. No implicit arguments.
data FcExpr
  = -- | Term variable reference.
    FcVar !Var
  | -- | Literal value.
    FcLit !Literal
  | -- | Term application.
    FcApp !FcExpr !FcExpr
  | -- | Type application (@e \@\tau@).
    FcTyApp !FcExpr !TcType
  | -- | Term lambda (@\lambda x : \tau . e@).
    FcLam !Var !FcExpr
  | -- | Type lambda (@\Lambda a . e@).
    FcTyLam !TyVarId !FcExpr
  | -- | Let binding.
    FcLet !FcBind !FcExpr
  | -- | Case expression: scrutinee, case binder, result type, alternatives.
    FcCase !FcExpr !Var !TcType ![FcAlt]
  | -- | Cast: @e \triangleright \gamma@.
    FcCast !FcExpr !Coercion
  deriving (Eq, Show)

-- | Binding group.
data FcBind
  = -- | Non-recursive binding.
    FcNonRec !Var !FcExpr
  | -- | Recursive binding group.
    FcRec ![(Var, FcExpr)]
  deriving (Eq, Show)

-- | Case alternative.
data FcAlt = FcAlt
  { -- | The constructor or literal being matched.
    altCon :: !FcAltCon,
    -- | Bound variables (constructor fields).
    altBinders :: ![Var],
    -- | Right-hand side.
    altRhs :: !FcExpr
  }
  deriving (Eq, Show)

-- | Case alternative constructor.
data FcAltCon
  = -- | Data constructor with type variable binders.
    DataAlt !Text
  | -- | Literal match.
    LitAlt !Literal
  | -- | Default/wildcard.
    DefaultAlt
  deriving (Eq, Show)

-- | Literal values.
data Literal
  = LitInt !Integer
  | LitChar !Char
  | LitString !Text
  deriving (Eq, Show)
