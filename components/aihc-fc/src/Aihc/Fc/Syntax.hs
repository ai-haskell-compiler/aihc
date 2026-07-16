{-# LANGUAGE OverloadedStrings #-}

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
    FcNewtypeDecl (..),
    FcProgram (..),
    FcForeignCall (..),
    FcForeignSignature (..),
    FcForeignEffect (..),
    FcForeignType (..),
    fcForeignOperandTypes,
    fcForeignCallResultType,
    fcForeignCallType,

    -- * Case alternatives
    FcAlt (..),
    FcAltCon (..),

    -- * Literals
    Literal (..),
    literalRuntimeRep,
    literalType,
  )
where

import Aihc.Tc.Evidence (Coercion)
import Aihc.Tc.Types
  ( RuntimeRep (..),
    TcType (..),
    TyCon (..),
    TyVarId,
    Unique,
    liftedRuntimeRep,
  )
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
  | -- | A nominal type with a representational equality axiom.
    FcNewtype !FcNewtypeDecl
  | -- | A primitive imported by @foreign import prim@.
    FcPrimitive !Var !Int
  | -- | A C symbol available to saturated 'FcCallForeign' expressions.  It
    -- does not introduce a term variable.
    FcForeignImport !FcForeignCall
  | -- | Value binding.
    FcTopBind !FcBind
  deriving (Eq, Show, Read)

-- | The type-level information retained for a @newtype@ after its constructor
-- and pattern matches have been lowered to representational casts.
--
-- This declaration is proof metadata, not a runtime constructor declaration.
data FcNewtypeDecl = FcNewtypeDecl
  { fcNewtypeName :: !Text,
    fcNewtypeTyVars :: ![TyVarId],
    fcNewtypeConstructor :: !Text,
    fcNewtypeRepresentation :: !TcType,
    fcNewtypeResult :: !TcType
  }
  deriving (Eq, Show, Read)

-- | A statically named C function resolved by the evaluator or a code generator.
data FcForeignCall = FcForeignCall
  { fcForeignCallName :: !Text,
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
    fcForeignResultType :: !FcForeignType,
    fcForeignEffect :: !FcForeignEffect
  }
  deriving (Eq, Show, Read)

data FcForeignEffect
  = FcForeignPure
  | FcForeignRealWorld
  deriving (Eq, Show, Read)

-- | A value type with explicit host ABI marshalling support.
data FcForeignType
  = FcForeignInt32
  | FcForeignWord64
  deriving (Eq, Show, Read)

fcForeignOperandTypes :: FcForeignSignature -> [TcType]
fcForeignOperandTypes signature =
  map foreignPrimitiveType (fcForeignArgumentTypes signature)
    <> case fcForeignEffect signature of
      FcForeignPure -> []
      FcForeignRealWorld -> [statePrimRealWorldType]

fcForeignCallResultType :: FcForeignSignature -> TcType
fcForeignCallResultType signature =
  case fcForeignEffect signature of
    FcForeignPure -> foreignPrimitiveType (fcForeignResultType signature)
    FcForeignRealWorld ->
      TcTyCon
        (TyCon "(#,#)" 2)
        [statePrimRealWorldType, foreignPrimitiveType (fcForeignResultType signature)]

fcForeignCallType :: FcForeignSignature -> TcType
fcForeignCallType signature =
  foldr TcFunTy (fcForeignCallResultType signature) (fcForeignOperandTypes signature)

foreignPrimitiveType :: FcForeignType -> TcType
foreignPrimitiveType foreignType =
  case foreignType of
    FcForeignInt32 -> TcTyCon (TyCon "Int32#" 0) []
    FcForeignWord64 -> TcTyCon (TyCon "Word64#" 0) []

statePrimRealWorldType :: TcType
statePrimRealWorldType =
  TcTyCon (TyCon "State#" 1) [TcTyCon (TyCon "RealWorld" 0) []]

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
  | -- | A fully saturated foreign call.  Unlike a term application, this
    -- node cannot represent a foreign function value or partial application.
    FcCallForeign !FcForeignCall ![FcExpr]
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
  = LitInt !RuntimeRep !Integer
  | -- | An unboxed character literal, such as @'x'#@.
    LitChar !RuntimeRep !Char
  | LitString !Text
  deriving (Eq, Show, Read)

-- | The runtime representation carried by a Core literal. This is recorded
-- during desugaring from type-checker information and must not be reconstructed
-- by a downstream phase.
literalRuntimeRep :: Literal -> RuntimeRep
literalRuntimeRep literal =
  case literal of
    LitInt runtimeRep _ -> runtimeRep
    LitChar runtimeRep _ -> runtimeRep
    LitString {} -> liftedRuntimeRep

-- | The primitive type denoted by a literal. Unsupported combinations are
-- deliberately absent so Core Lint can reject them.
literalType :: Literal -> Maybe TcType
literalType literal =
  case literal of
    LitInt runtimeRep _ -> scalarType runtimeRep
    LitChar WordRep _ -> Just (primitiveType "Char#")
    LitChar _ _ -> Nothing
    LitString {} -> Just (TcTyCon (TyCon "[]" 1) [TcTyCon (TyCon "Char" 0) []])
  where
    scalarType runtimeRep =
      primitiveType
        <$> lookup
          runtimeRep
          [ (IntRep, "Int#"),
            (Int8Rep, "Int8#"),
            (Int16Rep, "Int16#"),
            (Int32Rep, "Int32#"),
            (Int64Rep, "Int64#"),
            (WordRep, "Word#"),
            (Word8Rep, "Word8#"),
            (Word16Rep, "Word16#"),
            (Word32Rep, "Word32#"),
            (Word64Rep, "Word64#")
          ]
    primitiveType name = TcTyCon (TyCon name 0) []
