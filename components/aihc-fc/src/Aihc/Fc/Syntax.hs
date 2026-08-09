{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

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
    Var (Var, varName, varUnique, varType, varResolvedName),
    FcSymbolOrigin (..),
    fcSymbolOriginText,
    fcExternalVar,

    -- * Bindings
    FcBind (..),
    FcTopBind (..),
    FcAxiomDecl (..),
    FcAxiomRole (..),
    FcNewtypeDecl (..),
    FcModuleId (..),
    FcProgram (..),
    FcForeignCall (..),
    FcForeignSignature (..),
    FcForeignEffect (..),
    FcForeignType (..),
    fcForeignOperandTypes,
    fcForeignCallResultType,
    fcForeignCallType,
    fcDictionaryConstructorName,

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
    Unique (..),
    liftedRuntimeRep,
  )
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Text (Text)
import Data.Text qualified as T

-- | The required identity of one System FC module container.
data FcModuleId = FcModuleId
  { fcModulePackage :: !Text,
    fcModuleName :: !Text
  }
  deriving (Eq, Ord, Show, Read)

-- | A System FC program with one module identity.
data FcProgram = FcProgram
  { fcProgramModule :: !FcModuleId,
    -- | Declarative top-level bindings.
    fcTopBinds :: ![FcTopBind]
  }
  deriving (Eq, Show, Read)

-- | A top-level binding.
data FcTopBind
  = -- | A term symbol defined by another compilation unit. Its type is
    -- declared once here and omitted from every occurrence.
    FcExternal !FcSymbolOrigin !TcType
  | -- | Data type declaration: type name, type variable parameters,
    -- list of (constructor name, field types).
    FcData !Text ![TyVarId] ![(Text, [TcType])]
  | -- | A type equality axiom. Axioms are proof metadata and have no
    -- runtime representation.
    FcAxiom !FcAxiomDecl
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

-- | The role at which an axiom proves equality.
data FcAxiomRole
  = FcNominal
  | FcRepresentational
  deriving (Eq, Show, Read)

-- | A named, parameterized type equality retained in System FC.
data FcAxiomDecl = FcAxiomDecl
  { fcAxiomName :: !Text,
    fcAxiomTyVars :: ![TyVarId],
    fcAxiomRole :: !FcAxiomRole,
    fcAxiomLeft :: !TcType,
    fcAxiomRight :: !TcType
  }
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
  = FcForeignInt
  | FcForeignInt32
  | FcForeignWord64
  | FcForeignAddr
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
    FcForeignInt -> TcTyCon (TyCon "Int#" 0) []
    FcForeignInt32 -> TcTyCon (TyCon "Int32#" 0) []
    FcForeignWord64 -> TcTyCon (TyCon "Word64#" 0) []
    FcForeignAddr -> TcTyCon (TyCon "Addr#" 0) []

statePrimRealWorldType :: TcType
statePrimRealWorldType =
  TcTyCon (TyCon "State#" 1) [TcTyCon (TyCon "RealWorld" 0) []]

fcDictionaryConstructorName :: Text -> Text
fcDictionaryConstructorName className = "$Dict$" <> className

-- | A typed variable.
data Var = ResolvedVar
  { varName :: !Text,
    varUnique :: !Unique,
    varType :: !TcType,
    -- | Resolver identity for an imported occurrence. Kept separate from the
    -- display name so whole-program FC evaluation remains source-readable.
    varResolvedName :: !(Maybe FcSymbolOrigin)
  }
  deriving (Show, Read)

-- | Stable source identity for a non-local symbol. Unlike the display name,
-- this includes the package selected by name resolution.
data FcSymbolOrigin
  = FcTopLevelOrigin
      { fcOriginPackage :: !Text,
        fcOriginModule :: !Text,
        fcOriginName :: !Text
      }
  | FcBuiltinOrigin
      { fcOriginName :: !Text
      }
  deriving (Eq, Ord, Show, Read)

fcSymbolOriginText :: FcSymbolOrigin -> Text
fcSymbolOriginText origin =
  case origin of
    FcTopLevelOrigin packageName moduleName symbolName ->
      (if packageName == "" then "" else packageName <> ":")
        <> moduleName
        <> "."
        <> symbolName
    FcBuiltinOrigin symbolName -> "builtin:" <> symbolName

-- | Rebuild the alpha-renamed variable introduced by an external declaration.
-- The complete origin participates so equal names from different packages are
-- distinct variables after parsing.
fcExternalVar :: FcSymbolOrigin -> TcType -> Var
fcExternalVar origin ty =
  ResolvedVar
    { varName = fcOriginName origin,
      varUnique = Unique (-2000000000 - abs (hash `rem` 1000000000)),
      varType = ty,
      varResolvedName = Just origin
    }
  where
    key = fcSymbolOriginText origin <> T.pack (show ty)
    hash = T.foldl' (\value character -> value * 33 + ord character) 5381 key

-- | Construct a variable without a separate imported identity.
pattern Var :: Text -> Unique -> TcType -> Var
pattern Var name unique ty <- ResolvedVar name unique ty _
  where
    Var name unique ty = ResolvedVar name unique ty Nothing

{-# COMPLETE Var #-}

-- Eq/Ord on Unique only, mirroring TyVarId. Exact syntax-tree comparisons
-- must inspect every field because imported occurrences can carry additional
-- source identity in 'varResolvedName'.
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
  | -- | Latin-1 bytes with an implicit trailing NUL, such as @"hello"#@.
    LitAddr !ByteString
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
    LitAddr {} -> AddrRep

-- | The primitive type denoted by a literal. Unsupported combinations are
-- deliberately absent so Core Lint can reject them.
literalType :: Literal -> Maybe TcType
literalType literal =
  case literal of
    LitInt runtimeRep _ -> scalarType runtimeRep
    LitChar WordRep _ -> Just (primitiveType "Char#")
    LitChar _ _ -> Nothing
    LitString {} -> Just (TcTyCon (TyCon "[]" 1) [TcTyCon (TyCon "Char" 0) []])
    LitAddr {} -> Just (primitiveType "Addr#")
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
