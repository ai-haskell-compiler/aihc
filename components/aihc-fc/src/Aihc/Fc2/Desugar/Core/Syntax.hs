{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Private terms for FC2 value desugaring.
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
module Aihc.Fc2.Desugar.Core.Syntax
  ( -- * Core expressions
    CoreExpr (..),

    -- * Variables
    Var (Var, varName, varUnique, varType, varResolvedName),
    CoreSymbolOrigin (..),
    coreSymbolOriginText,
    coreExternalVar,

    -- * Bindings
    CoreBind (..),
    CoreTopBind (..),
    CoreDataDecl (..),
    coreDataKindTyVars,
    coreDataResultType,
    coreDataTyCon,
    CoreDataConDecl (..),
    CoreConstructorId (..),
    coreConstructorIdFromSymbol,
    coreConstructorSymbolOrigin,
    CoreAxiomDecl (..),
    CoreAxiomRole (..),
    CoreNewtypeDecl (..),
    CoreModuleId (..),
    coreModulePackageText,
    CoreProgram (..),
    CoreForeignCall (..),
    CoreForeignSignature (..),
    CoreForeignEffect (..),
    CoreForeignType (..),
    coreForeignOperandTypes,
    coreForeignCallResultType,
    coreForeignCallType,
    coreDictionaryConstructorName,

    -- * Case alternatives
    CoreAlt (..),
    CoreAltCon (..),

    -- * Literals
    Literal (..),
    literalRuntimeRep,
  )
where

import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Evidence (Coercion)
import Aihc.Tc.Types
  ( Kind (..),
    RuntimeRep (..),
    TcType (..),
    TyCon (..),
    TyVarId (..),
    Unique (..),
    liftedRuntimeRep,
    mkTyCon,
    mkTyConWithOrigin,
    runtimeRepOfType,
    setTyVarKind,
    tvKind,
    typeKind,
    unboxedTupleTyConName,
  )
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Either (fromRight)
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T

-- | The required identity of one System FC module container.
data CoreModuleId = CoreModuleId
  { coreModulePackage :: !PackageId,
    coreModuleName :: !Text
  }
  deriving (Eq, Show, Read)

coreModulePackageText :: CoreModuleId -> Text
coreModulePackageText = packageIdText . coreModulePackage

-- | A System FC program with one module identity.
data CoreProgram = CoreProgram
  { coreProgramModule :: !CoreModuleId,
    -- | Declarative top-level bindings.
    coreTopBinds :: ![CoreTopBind]
  }
  deriving (Eq, Show, Read)

-- | A top-level binding.
data CoreTopBind
  = -- | A term symbol defined by another compilation unit. Its type is
    -- declared once here and omitted from every occurrence.
    CoreExternal !CoreSymbolOrigin !TcType
  | -- | A data type declaration.
    CoreData !CoreDataDecl
  | -- | A type equality axiom. Axioms are proof metadata and have no
    -- runtime representation.
    CoreAxiom !CoreAxiomDecl
  | -- | A nominal type with a representational equality axiom.
    CoreNewtype !CoreNewtypeDecl
  | -- | A primitive imported by @foreign import prim@.
    CorePrimitive !Var !Int
  | -- | A C symbol available to saturated 'CoreCallForeign' expressions.  It
    -- does not introduce a term variable.
    CoreForeignImport !CoreForeignCall
  | -- | Value binding.
    CoreTopBind !CoreBind
  deriving (Eq, Show, Read)

-- | The role at which an axiom proves equality.
data CoreAxiomRole
  = CoreNominal
  | CoreRepresentational
  deriving (Eq, Show, Read)

-- | A data type with its stable source identity.
data CoreDataDecl = CoreDataDecl
  { coreDataOrigin :: !CoreSymbolOrigin,
    coreDataName :: !Text,
    coreDataTyVars :: ![TyVarId],
    coreDataResultKind :: !Kind,
    coreDataConstructors :: ![CoreDataConDecl]
  }
  deriving (Eq, Show, Read)

coreDataTyCon :: CoreDataDecl -> TyCon
coreDataTyCon declaration =
  case coreDataOrigin declaration of
    CoreTopLevelOrigin packageId moduleName _ ->
      mkTyConWithOrigin
        (PackageId packageId)
        moduleName
        (coreDataName declaration)
        (length (coreDataTyVars declaration))
        kind
    CoreBuiltinOrigin {} ->
      mkTyCon
        (coreDataName declaration)
        (length (coreDataTyVars declaration))
        kind
  where
    kind = foldr (KFun . tvKind) (coreDataResultKind declaration) (coreDataTyVars declaration)

coreDataKindTyVars :: CoreDataDecl -> [TyVarId]
coreDataKindTyVars declaration =
  [ setTyVarKind KRuntimeRep (TyVarId ("r" <> T.pack (show unique)) (Unique unique))
  | Unique unique <- nub (concatMap (kindRuntimeRepVariables . tvKind) (coreDataTyVars declaration) <> kindRuntimeRepVariables (coreDataResultKind declaration))
  ]
  where
    kindRuntimeRepVariables kind =
      case kind of
        KTYPE runtimeRep -> runtimeRepVariables runtimeRep
        KFun argument result -> kindRuntimeRepVariables argument <> kindRuntimeRepVariables result
        _ -> []
    runtimeRepVariables runtimeRep =
      case runtimeRep of
        RuntimeRepVar unique -> [unique]
        TupleRep fields -> concatMap runtimeRepVariables fields
        SumRep fields -> concatMap runtimeRepVariables fields
        _ -> []

coreDataResultType :: CoreDataDecl -> TcType
coreDataResultType declaration =
  TcTyCon (coreDataTyCon declaration) (map TcTyVar (coreDataTyVars declaration))

-- | A data constructor with its stable source identity.
data CoreDataConDecl = CoreDataConDecl
  { coreDataConOrigin :: !CoreConstructorId,
    coreDataConName :: !Text,
    coreDataConFields :: ![TcType]
  }
  deriving (Eq, Show, Read)

-- | A named, parameterized type equality retained in System FC.
data CoreAxiomDecl = CoreAxiomDecl
  { coreAxiomName :: !Text,
    coreAxiomTyVars :: ![TyVarId],
    coreAxiomRole :: !CoreAxiomRole,
    coreAxiomLeft :: !TcType,
    coreAxiomRight :: !TcType
  }
  deriving (Eq, Show, Read)

-- | The type-level information retained for a @newtype@ after its constructor
-- and pattern matches have been lowered to representational casts.
--
-- This declaration is proof metadata, not a runtime constructor declaration.
data CoreNewtypeDecl = CoreNewtypeDecl
  { coreNewtypeOrigin :: !CoreSymbolOrigin,
    coreNewtypeName :: !Text,
    coreNewtypeTyVars :: ![TyVarId],
    coreNewtypeConstructorOrigin :: !CoreConstructorId,
    coreNewtypeConstructor :: !Text,
    coreNewtypeRepresentation :: !TcType,
    coreNewtypeResult :: !TcType
  }
  deriving (Eq, Show, Read)

-- | A statically named C function resolved by the evaluator or a code generator.
data CoreForeignCall = CoreForeignCall
  { coreForeignCallName :: !Text,
    coreForeignCallSymbol :: !Text,
    coreForeignCallSignature :: !CoreForeignSignature
  }
  deriving (Eq, Show, Read)

-- | The ABI-relevant part of a foreign import's type.
--
-- Arguments are represented independently, so adding a new marshalled type
-- does not require a constructor for every arity and result combination.
data CoreForeignSignature = CoreForeignSignature
  { coreForeignArgumentTypes :: ![CoreForeignType],
    coreForeignResultType :: !CoreForeignType,
    coreForeignEffect :: !CoreForeignEffect
  }
  deriving (Eq, Show, Read)

data CoreForeignEffect
  = CoreForeignPure
  | CoreForeignRealWorld
  deriving (Eq, Show, Read)

-- | A value type with explicit host ABI marshalling support.
data CoreForeignType
  = CoreForeignInt
  | CoreForeignInt32
  | CoreForeignWord64
  | CoreForeignAddr
  deriving (Eq, Show, Read)

coreForeignOperandTypes :: CoreForeignSignature -> [TcType]
coreForeignOperandTypes signature =
  map foreignPrimitiveType (coreForeignArgumentTypes signature)
    <> case coreForeignEffect signature of
      CoreForeignPure -> []
      CoreForeignRealWorld -> [statePrimRealWorldType]

coreForeignCallResultType :: CoreForeignSignature -> TcType
coreForeignCallResultType signature =
  case coreForeignEffect signature of
    CoreForeignPure -> foreignPrimitiveType (coreForeignResultType signature)
    CoreForeignRealWorld ->
      let fields = [statePrimRealWorldType, foreignPrimitiveType (coreForeignResultType signature)]
          fieldRep field = fromRight liftedRuntimeRep (runtimeRepOfType field)
          resultKind = KTYPE (TupleRep (map fieldRep fields))
          tupleKind = foldr (KFun . typeKind) resultKind fields
       in TcTyCon (mkTyCon (unboxedTupleTyConName 2) 2 tupleKind) fields

coreForeignCallType :: CoreForeignSignature -> TcType
coreForeignCallType signature =
  foldr TcFunTy (coreForeignCallResultType signature) (coreForeignOperandTypes signature)

foreignPrimitiveType :: CoreForeignType -> TcType
foreignPrimitiveType foreignType =
  case foreignType of
    CoreForeignInt -> TcTyCon (TyCon "Int#" 0) []
    CoreForeignInt32 -> TcTyCon (TyCon "Int32#" 0) []
    CoreForeignWord64 -> TcTyCon (TyCon "Word64#" 0) []
    CoreForeignAddr -> TcTyCon (TyCon "Addr#" 0) []

statePrimRealWorldType :: TcType
statePrimRealWorldType =
  TcTyCon (TyCon "State#" 1) [TcTyCon (TyCon "RealWorld" 0) []]

coreDictionaryConstructorName :: Text -> Text
coreDictionaryConstructorName className = "$Dict$" <> className

-- | A typed variable.
data Var = ResolvedVar
  { varName :: !Text,
    varUnique :: !Unique,
    varType :: !TcType,
    -- | Resolver identity for an imported occurrence. Kept separate from the
    -- display name so whole-program FC evaluation remains source-readable.
    varResolvedName :: !(Maybe CoreSymbolOrigin)
  }
  deriving (Eq, Ord, Show, Read)

-- | Stable source identity for a non-local symbol. Unlike the display name,
-- this includes the package selected by name resolution.
data CoreSymbolOrigin
  = CoreTopLevelOrigin
      { coreOriginPackage :: !Text,
        coreOriginModule :: !Text,
        coreOriginName :: !Text
      }
  | CoreBuiltinOrigin
      { coreOriginName :: !Text
      }
  deriving (Eq, Ord, Show, Read)

-- | The complete identity of a data constructor.
data CoreConstructorId = CoreConstructorId
  { coreConstructorPackage :: !PackageId,
    coreConstructorModule :: !Text,
    coreConstructorName :: !Text
  }
  deriving (Eq, Ord, Show, Read)

coreConstructorSymbolOrigin :: CoreConstructorId -> CoreSymbolOrigin
coreConstructorSymbolOrigin constructor =
  CoreTopLevelOrigin
    (packageIdText (coreConstructorPackage constructor))
    (coreConstructorModule constructor)
    (coreConstructorName constructor)

coreConstructorIdFromSymbol :: CoreSymbolOrigin -> CoreConstructorId
coreConstructorIdFromSymbol origin =
  case origin of
    CoreTopLevelOrigin packageName moduleName constructorName ->
      CoreConstructorId (PackageId packageName) moduleName constructorName
    CoreBuiltinOrigin constructorName ->
      error ("constructor does not have a complete identity: " <> T.unpack constructorName)

coreSymbolOriginText :: CoreSymbolOrigin -> Text
coreSymbolOriginText origin =
  case origin of
    CoreTopLevelOrigin packageName moduleName symbolName ->
      (if packageName == "" then "" else packageName <> ":")
        <> moduleName
        <> "."
        <> symbolName
    CoreBuiltinOrigin symbolName -> "builtin:" <> symbolName

-- | Rebuild the alpha-renamed variable introduced by an external declaration.
-- The complete origin participates so equal names from different packages are
-- distinct variables after parsing.
coreExternalVar :: CoreSymbolOrigin -> TcType -> Var
coreExternalVar origin ty =
  ResolvedVar
    { varName = coreOriginName origin,
      varUnique = Unique (-2000000000 - abs (hash `rem` 1000000000)),
      varType = ty,
      varResolvedName = Just origin
    }
  where
    key = coreSymbolOriginText origin <> T.pack (show ty)
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
data CoreExpr
  = -- | Term variable reference.
    CoreVar !Var
  | -- | Literal value.
    CoreLit !Literal !TcType
  | -- | Term application.
    CoreApp !CoreExpr !CoreExpr
  | -- | Type application (@e \@\tau@).
    CoreTyApp !CoreExpr !TcType
  | -- | Term lambda (@\lambda x : \tau . e@).
    CoreLam !Var !CoreExpr
  | -- | Type lambda (@\Lambda a . e@).
    CoreTyLam !TyVarId !CoreExpr
  | -- | Let binding.
    CoreLet !CoreBind !CoreExpr
  | -- | Case expression: scrutinee, case binder, alternatives.
    CoreCase !CoreExpr !Var ![CoreAlt]
  | -- | Cast: @e \triangleright \gamma@.
    CoreCast !CoreExpr !Coercion
  | -- | A fully saturated foreign call.  Unlike a term application, this
    -- node cannot represent a foreign function value or partial application.
    CoreCallForeign !CoreForeignCall ![CoreExpr]
  deriving (Eq, Show, Read)

-- | Binding group.
data CoreBind
  = -- | Non-recursive binding.
    CoreNonRec !Var !CoreExpr
  | -- | Recursive binding group.
    CoreRec ![(Var, CoreExpr)]
  deriving (Eq, Show, Read)

-- | Case alternative.
data CoreAlt = CoreAlt
  { -- | The constructor or literal being matched.
    altCon :: !CoreAltCon,
    -- | Bound variables (constructor fields).
    altBinders :: ![Var],
    -- | Right-hand side.
    altRhs :: !CoreExpr
  }
  deriving (Eq, Show, Read)

-- | Case alternative constructor.
data CoreAltCon
  = -- | Data constructor with its full identity.
    DataAlt !CoreConstructorId
  | -- | Literal match.
    LitAlt !Literal !TcType
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
