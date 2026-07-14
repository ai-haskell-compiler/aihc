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
    FcForeignCall (..),
    FcForeignSignature (..),
    FcForeignResult (..),
    FcForeignType (..),

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
  deriving (Eq, Show, Read)

-- | A top-level binding.
data FcTopBind
  = -- | Data type declaration: type name, type variable parameters,
    -- list of (constructor name, field types).
    FcData !Text ![TyVarId] ![(Text, [TcType])]
  | -- | Newtype declaration: type name, type variable parameters, and its
    -- single constructor.
    FcNewtype !Text ![TyVarId] !Text !TcType
  | -- | A primitive imported by @foreign import prim@.
    FcPrimitive !Var !Int
  | -- | A C function imported by @foreign import ccall@.
    FcForeignImport !FcForeignCall
  | -- | Value binding.
    FcTopBind !FcBind
  deriving (Eq, Show, Read)

-- | A statically named C function resolved by the evaluator or a code generator.
data FcForeignCall = FcForeignCall
  { fcForeignCallVar :: !Var,
    fcForeignCallSymbol :: !Text,
    fcForeignCallSignature :: !FcForeignSignature
  }
  deriving (Eq, Show, Read)

-- | The ABI-relevant part of a foreign import's type.
--
-- Arguments are represented independently, so adding a new marshalled type
-- does not require a constructor for every arity and result combination.
data FcForeignSignature = FcForeignSignature
  { fcForeignArgumentTypes :: ![FcForeignType],
    fcForeignResult :: !FcForeignResult
  }
  deriving (Eq, Show, Read)

-- | Whether a foreign call is pure or produces an 'IO' action.
data FcForeignResult
  = FcForeignPure !FcForeignType
  | FcForeignIO !FcForeignType
  deriving (Eq, Show, Read)

-- | A value type with explicit host ABI marshalling support.
data FcForeignType
  = FcForeignCInt
  deriving (Eq, Show, Read)

-- | A typed variable.
data Var = Var
  { varName :: !Text,
    varUnique :: !Unique,
    varType :: !TcType
  }
  deriving (Show, Read)

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
  | -- | Dictionary application (@e dict@), kept separate from ordinary
    -- term application so the dictionary-passing phase is explicit.
    FcDictApp !FcExpr !FcExpr
  | -- | Type application (@e \@\tau@).
    FcTyApp !FcExpr !TcType
  | -- | Term lambda (@\lambda x : \tau . e@).
    FcLam !Var !FcExpr
  | -- | Type lambda (@\Lambda a . e@).
    FcTyLam !TyVarId !FcExpr
  | -- | Dictionary lambda.
    FcDictLam !Var !FcExpr
  | -- | Dictionary value, represented as method fields.
    FcDict ![FcExpr]
  | -- | Dictionary field selection.
    FcDictSelect !FcExpr !Int
  | -- | Let binding.
    FcLet !FcBind !FcExpr
  | -- | Case expression: scrutinee, case binder, alternatives.
    FcCase !FcExpr !Var ![FcAlt]
  | -- | Cast: @e \triangleright \gamma@.
    FcCast !FcExpr !Coercion
  deriving (Eq, Show, Read)

-- | Binding group.
data FcBind
  = -- | Non-recursive binding.
    FcNonRec !Var !FcExpr
  | -- | Recursive binding group.
    FcRec ![(Var, FcExpr)]
  deriving (Eq, Show, Read)

-- | Case alternative.
data FcAlt = FcAlt
  { -- | The constructor or literal being matched.
    altCon :: !FcAltCon,
    -- | Bound variables (constructor fields).
    altBinders :: ![Var],
    -- | Right-hand side.
    altRhs :: !FcExpr
  }
  deriving (Eq, Show, Read)

-- | Case alternative constructor.
data FcAltCon
  = -- | Data constructor with type variable binders.
    DataAlt !Text
  | -- | Literal match.
    LitAlt !Literal
  | -- | Default/wildcard.
    DefaultAlt
  deriving (Eq, Show, Read)

-- | Literal values.
data Literal
  = LitInt !Integer
  | -- | An unboxed character literal, such as @'x'#@.
    LitChar !Char
  | LitString !Text
  deriving (Eq, Show, Read)
