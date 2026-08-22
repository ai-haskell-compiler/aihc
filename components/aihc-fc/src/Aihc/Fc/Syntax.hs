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
    FcDataDecl (..),
    fcDataKindTyVars,
    fcDataResultType,
    fcDataTyCon,
    FcDataConDecl (..),
    FcConstructorId (..),
    fcConstructorIdFromSymbol,
    fcConstructorSymbolOrigin,
    FcAxiomDecl (..),
    FcAxiomRole (..),
    FcNewtypeDecl (..),
    FcModuleId (..),
    fcModulePackageText,
    FcProgram (..),
    FcForeignCall (..),
    FcForeignSignature (..),
    FcForeignEffect (..),
    FcForeignType (..),
    fcForeignOperandTypes,
    fcForeignCallResultType,
    fcForeignCallType,
    fcDictionaryConstructorName,
    legacyTyCon,
    legacyTyConWithKind,

    -- * Case alternatives
    FcAlt (..),
    FcAltCon (..),

    -- * Literals
    Literal (..),
    literalRuntimeRep,
  )
where

import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Evidence (Coercion)
import Aihc.Tc.Types
  ( Pred (..),
    TcKindEnv,
    TcType (..),
    TyCon (..),
    TyVarId (..),
    Unique (..),
    liftedRuntimeRep,
    mkTyConWithOrigin,
    setTyVarKind,
    tvKind,
    unboxedTupleTyConName,
    pattern AddrRep,
    pattern KFun,
    pattern KRuntimeRep,
    pattern KType,
  )
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T

-- | Make a type constructor for the FC1 compatibility path.
legacyTyCon :: Text -> Int -> TyCon
legacyTyCon name arity =
  legacyTyConWithKind name arity (foldr KFun KType (replicate arity KType))

-- | Make a type constructor with a kind for the FC1 compatibility path.
legacyTyConWithKind :: Text -> Int -> TcType -> TyCon
legacyTyConWithKind name arity _ =
  mkTyConWithOrigin (PackageId "aihc-internal") "Aihc.Internal" name arity

-- | The required identity of one System FC module container.
data FcModuleId = FcModuleId
  { fcModulePackage :: !PackageId,
    fcModuleName :: !Text
  }
  deriving (Eq, Show, Read)

fcModulePackageText :: FcModuleId -> Text
fcModulePackageText = packageIdText . fcModulePackage

-- | A System FC program with one module identity.
data FcProgram = FcProgram
  { fcProgramModule :: !FcModuleId,
    fcProgramKindEnv :: !TcKindEnv,
    fcTopBinds :: ![FcTopBind]
  }
  deriving (Eq, Show, Read)

{-# COMPLETE FcProgram #-}

-- | A top-level binding.
data FcTopBind
  = -- | A term symbol defined by another compilation unit. Its type is
    -- declared once here and omitted from every occurrence.
    FcExternal !FcSymbolOrigin !TcType
  | -- | A data type declaration.
    FcData !FcDataDecl
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

-- | A data type with its stable source identity.
data FcDataDecl = FcDataDecl
  { fcDataOrigin :: !FcSymbolOrigin,
    fcDataName :: !Text,
    fcDataTyVars :: ![TyVarId],
    fcDataResultKind :: !TcType,
    fcDataConstructors :: ![FcDataConDecl]
  }
  deriving (Eq, Show, Read)

fcDataTyCon :: FcDataDecl -> TyCon
fcDataTyCon declaration =
  case fcDataOrigin declaration of
    FcTopLevelOrigin packageId moduleName _ ->
      mkTyConWithOrigin
        (PackageId packageId)
        moduleName
        (fcDataName declaration)
        (length (fcDataTyVars declaration))
    FcBuiltinOrigin {} ->
      legacyTyConWithKind
        (fcDataName declaration)
        (length (fcDataTyVars declaration))
        (foldr (KFun . tvKind) (fcDataResultKind declaration) (fcDataTyVars declaration))

fcDataKindTyVars :: FcDataDecl -> [TyVarId]
fcDataKindTyVars declaration =
  [ setTyVarKind KRuntimeRep (TyVarId ("r" <> T.pack (show unique)) (Unique unique))
  | variable@(TyVarId _ (Unique unique)) <- nub (concatMap (typeVariables . tvKind) (fcDataTyVars declaration) <> typeVariables (fcDataResultKind declaration)),
    tvKind variable == KRuntimeRep
  ]
  where
    typeVariables ty =
      case ty of
        TcTyVar variable -> [variable]
        TcMetaTv {} -> []
        TcTyCon _ arguments -> concatMap typeVariables arguments
        TcFunTy argument result -> typeVariables argument <> typeVariables result
        TcForAllTy variable body -> filter ((/= tvUnique variable) . tvUnique) (typeVariables body)
        TcQualTy predicates body -> concatMap predicateVariables predicates <> typeVariables body
        TcAppTy function argument -> typeVariables function <> typeVariables argument
    predicateVariables predicate =
      case predicate of
        ClassPred _ arguments -> concatMap typeVariables arguments
        EqPred left right -> typeVariables left <> typeVariables right

fcDataResultType :: FcDataDecl -> TcType
fcDataResultType declaration =
  TcTyCon (fcDataTyCon declaration) (map TcTyVar (fcDataTyVars declaration))

-- | A data constructor with its stable source identity.
data FcDataConDecl = FcDataConDecl
  { fcDataConOrigin :: !FcConstructorId,
    fcDataConName :: !Text,
    fcDataConFields :: ![TcType]
  }
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
  { fcNewtypeOrigin :: !FcSymbolOrigin,
    fcNewtypeName :: !Text,
    fcNewtypeTyVars :: ![TyVarId],
    fcNewtypeConstructorOrigin :: !FcConstructorId,
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
      let fields = [statePrimRealWorldType, foreignPrimitiveType (fcForeignResultType signature)]
       in TcTyCon (legacyTyCon (unboxedTupleTyConName 2) 2) fields

fcForeignCallType :: FcForeignSignature -> TcType
fcForeignCallType signature =
  foldr TcFunTy (fcForeignCallResultType signature) (fcForeignOperandTypes signature)

foreignPrimitiveType :: FcForeignType -> TcType
foreignPrimitiveType foreignType =
  case foreignType of
    FcForeignInt -> TcTyCon (legacyTyCon "Int#" 0) []
    FcForeignInt32 -> TcTyCon (legacyTyCon "Int32#" 0) []
    FcForeignWord64 -> TcTyCon (legacyTyCon "Word64#" 0) []
    FcForeignAddr -> TcTyCon (legacyTyCon "Addr#" 0) []

statePrimRealWorldType :: TcType
statePrimRealWorldType =
  TcTyCon (legacyTyCon "State#" 1) [TcTyCon (legacyTyCon "RealWorld" 0) []]

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
  deriving (Eq, Ord, Show, Read)

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

-- | The complete identity of a data constructor.
data FcConstructorId = FcConstructorId
  { fcConstructorPackage :: !PackageId,
    fcConstructorModule :: !Text,
    fcConstructorName :: !Text
  }
  deriving (Eq, Ord, Show, Read)

fcConstructorSymbolOrigin :: FcConstructorId -> FcSymbolOrigin
fcConstructorSymbolOrigin constructor =
  FcTopLevelOrigin
    (packageIdText (fcConstructorPackage constructor))
    (fcConstructorModule constructor)
    (fcConstructorName constructor)

fcConstructorIdFromSymbol :: FcSymbolOrigin -> FcConstructorId
fcConstructorIdFromSymbol origin =
  case origin of
    FcTopLevelOrigin packageName moduleName constructorName ->
      FcConstructorId (PackageId packageName) moduleName constructorName
    FcBuiltinOrigin constructorName ->
      error ("constructor does not have a complete identity: " <> T.unpack constructorName)

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

-- | System FC core expression.
--
-- Every binding is explicit. No syntactic sugar. No implicit arguments.
data FcExpr
  = -- | Term variable reference.
    FcVar !Var
  | -- | Literal value.
    FcLit !Literal !TcType
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
  = -- | Data constructor with its full identity.
    DataAlt !FcConstructorId
  | -- | Literal match.
    LitAlt !Literal !TcType
  | -- | Default/wildcard.
    DefaultAlt
  deriving (Eq, Show, Read)

-- | Literal values.
data Literal
  = LitInt !TcType !Integer
  | -- | An unboxed character literal, such as @'x'#@.
    LitChar !TcType !Char
  | LitString !Text
  | -- | Latin-1 bytes with an implicit trailing NUL, such as @"hello"#@.
    LitAddr !ByteString
  deriving (Eq, Show, Read)

-- | The runtime representation carried by a Core literal. This is recorded
-- during desugaring from type-checker information and must not be reconstructed
-- by a downstream phase.
literalRuntimeRep :: Literal -> TcType
literalRuntimeRep literal =
  case literal of
    LitInt runtimeRep _ -> runtimeRep
    LitChar runtimeRep _ -> runtimeRep
    LitString {} -> liftedRuntimeRep
    LitAddr {} -> AddrRep
