{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Runtime-only input for GRIN lowering.
module Aihc.Grin.Lower.Syntax
  ( FcExpr (..),
    Var (Var, varName, varUnique, varType, varResolvedName),
    FcSymbolOrigin (..),
    fcSymbolOriginText,
    fcExternalVar,
    FcBind (..),
    FcTopBind (..),
    FcDataDecl (..),
    FcDataConDecl (..),
    FcConstructorId (..),
    fcConstructorSymbolOrigin,
    FcModuleId (..),
    fcModulePackageText,
    FcProgram (..),
    FcForeignCall (..),
    FcForeignSignature (..),
    FcForeignEffect (..),
    FcForeignType (..),
    fcForeignOperandTypes,
    fcForeignCallResultType,
    FcAlt (..),
    FcAltCon (..),
    Literal (..),
    literalRuntimeRep,
    Kind (..),
    RuntimeRep,
    Levity,
    VecCount,
    VecElem,
    TcType (..),
    TyCon,
    TyVarId (..),
    Unique (..),
    liftedRuntimeRep,
    legacyTyConWithKind,
    runtimeRepOfType,
    setTyVarKind,
    substType,
  )
where

import Aihc.Grin.Syntax
  ( GrinLevity (..),
    GrinRep (..),
    GrinVecCount,
    GrinVecElem,
    liftedGrinRep,
  )
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Types (Unique (..))
import Data.ByteString (ByteString)
import Data.Char (ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

type RuntimeRep = GrinRep

type Levity = GrinLevity

type VecCount = GrinVecCount

type VecElem = GrinVecElem

liftedRuntimeRep :: RuntimeRep
liftedRuntimeRep = liftedGrinRep

data Kind
  = KTYPE !RuntimeRep
  | KTYPEVar !Unique
  | KConstraint
  | KRuntimeRep
  | KLevity
  | KVecCount
  | KVecElem
  | KFun !Kind !Kind
  deriving (Eq, Ord, Show, Read)

data TyVarId = TyVarId
  { tvName :: !Text,
    tvUnique :: !Unique,
    tvKind :: !Kind
  }
  deriving (Eq, Ord, Show, Read)

setTyVarKind :: Kind -> TyVarId -> TyVarId
setTyVarKind kind tyVar = tyVar {tvKind = kind}

data TyCon = TyCon !Text !Int !Kind
  deriving (Eq, Ord, Show, Read)

legacyTyConWithKind :: Text -> Int -> Kind -> TyCon
legacyTyConWithKind = TyCon

data TcType
  = TcTyVar !TyVarId
  | TcTyCon !TyCon ![TcType]
  | TcBuiltinTyCon !Text !Int ![TcType]
  | TcFunTy !TcType !TcType
  | TcForAllTy !TyVarId !TcType
  | TcQualTy ![TcType] !TcType
  deriving (Eq, Ord, Show, Read)

runtimeRepOfType :: TcType -> Either String RuntimeRep
runtimeRepOfType ty =
  case ty of
    TcTyVar tyVar -> runtimeRepOfKind (tvKind tyVar)
    TcTyCon (TyCon _ _ kind) _ -> runtimeRepOfKind kind
    TcBuiltinTyCon {} -> pure liftedRuntimeRep
    TcFunTy {} -> pure liftedRuntimeRep
    TcForAllTy _ body -> runtimeRepOfType body
    TcQualTy _ body -> runtimeRepOfType body

runtimeRepOfKind :: Kind -> Either String RuntimeRep
runtimeRepOfKind kind =
  case kind of
    KTYPE representation -> pure representation
    KTYPEVar {} -> pure liftedRuntimeRep
    _ -> Left ("type has a non-runtime kind: " <> show kind)

substType :: Map TyVarId TcType -> TcType -> TcType
substType = go
  where
    go active ty =
      case ty of
        TcTyVar tyVar -> Map.findWithDefault (TcTyVar (substTyVar active tyVar)) tyVar active
        TcTyCon tyCon arguments -> TcTyCon (substTyCon active tyCon) (map (go active) arguments)
        TcBuiltinTyCon name arity arguments -> TcBuiltinTyCon name arity (map (go active) arguments)
        TcFunTy argument result -> TcFunTy (go active argument) (go active result)
        TcForAllTy tyVar body ->
          let active' = Map.delete tyVar active
           in TcForAllTy (substTyVar active' tyVar) (go active' body)
        TcQualTy predicates body -> TcQualTy (map (go active) predicates) (go active body)

    substTyCon active (TyCon name arity kind) = TyCon name arity (substKind active kind)

    substTyVar active tyVar = tyVar {tvKind = substKind active (tvKind tyVar)}

    substKind active kind =
      case kind of
        KTYPEVar unique ->
          maybe kind KTYPE (lookupRuntimeRep active unique)
        KFun argument result -> KFun (substKind active argument) (substKind active result)
        _ -> kind

    lookupRuntimeRep active unique =
      foldr
        ( \(tyVar, replacement) found ->
            if tvUnique tyVar == unique
              then runtimeRepTypeValue replacement
              else found
        )
        Nothing
        (Map.toList active)

runtimeRepTypeValue :: TcType -> Maybe RuntimeRep
runtimeRepTypeValue ty =
  case ty of
    TcTyCon (TyCon name _ KRuntimeRep) _ -> namedRuntimeRep name
    _ -> Nothing

namedRuntimeRep :: Text -> Maybe RuntimeRep
namedRuntimeRep rawName =
  case T.dropWhile (== '\'') rawName of
    "LiftedRep" -> Just liftedRuntimeRep
    "UnliftedRep" -> Just (BoxedRep Unlifted)
    "IntRep" -> Just IntRep
    "Int8Rep" -> Just Int8Rep
    "Int16Rep" -> Just Int16Rep
    "Int32Rep" -> Just Int32Rep
    "Int64Rep" -> Just Int64Rep
    "WordRep" -> Just WordRep
    "Word8Rep" -> Just Word8Rep
    "Word16Rep" -> Just Word16Rep
    "Word32Rep" -> Just Word32Rep
    "Word64Rep" -> Just Word64Rep
    "AddrRep" -> Just AddrRep
    "FloatRep" -> Just FloatRep
    "DoubleRep" -> Just DoubleRep
    _ -> Nothing

data FcModuleId = FcModuleId
  { fcModulePackage :: !PackageId,
    fcModuleName :: !Text
  }
  deriving (Eq, Show, Read)

fcModulePackageText :: FcModuleId -> Text
fcModulePackageText = packageIdText . fcModulePackage

data FcProgram = FcProgram
  { fcProgramModule :: !FcModuleId,
    fcTopBinds :: ![FcTopBind]
  }
  deriving (Eq, Show, Read)

data FcTopBind
  = FcExternal
  | FcData !FcDataDecl
  | FcAxiom
  | FcNewtype
  | FcPrimitive !Var !Int
  | FcForeignImport !FcForeignCall
  | FcTopBind !FcBind
  deriving (Eq, Show, Read)

data FcDataDecl = FcDataDecl
  { fcDataOrigin :: !FcSymbolOrigin,
    fcDataName :: !Text,
    fcDataTyVars :: ![TyVarId],
    fcDataResultKind :: !Kind,
    fcDataConstructors :: ![FcDataConDecl]
  }
  deriving (Eq, Show, Read)

data FcDataConDecl = FcDataConDecl
  { fcDataConOrigin :: !FcConstructorId,
    fcDataConName :: !Text,
    fcDataConFields :: ![TcType]
  }
  deriving (Eq, Show, Read)

data FcForeignCall = FcForeignCall
  { fcForeignCallName :: !Text,
    fcForeignCallSymbol :: !Text,
    fcForeignCallSignature :: !FcForeignSignature
  }
  deriving (Eq, Show, Read)

data FcForeignSignature = FcForeignSignature
  { fcForeignArgumentTypes :: ![FcForeignType],
    fcForeignResultType :: !FcForeignType,
    fcForeignEffect :: !FcForeignEffect
  }
  deriving (Eq, Show, Read)

data FcForeignEffect = FcForeignPure | FcForeignRealWorld
  deriving (Eq, Show, Read)

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
      FcForeignRealWorld -> [runtimeType (TupleRep [])]

fcForeignCallResultType :: FcForeignSignature -> TcType
fcForeignCallResultType signature =
  case fcForeignEffect signature of
    FcForeignPure -> foreignPrimitiveType (fcForeignResultType signature)
    FcForeignRealWorld ->
      runtimeType
        ( TupleRep
            [ TupleRep [],
              foreignTypeRuntimeRep (fcForeignResultType signature)
            ]
        )

foreignPrimitiveType :: FcForeignType -> TcType
foreignPrimitiveType = runtimeType . foreignTypeRuntimeRep

foreignTypeRuntimeRep :: FcForeignType -> RuntimeRep
foreignTypeRuntimeRep foreignType =
  case foreignType of
    FcForeignInt -> IntRep
    FcForeignInt32 -> Int32Rep
    FcForeignWord64 -> Word64Rep
    FcForeignAddr -> AddrRep

runtimeType :: RuntimeRep -> TcType
runtimeType representation = TcTyCon (TyCon "$runtime" 0 (KTYPE representation)) []

data Var = ResolvedVar
  { varName :: !Text,
    varUnique :: !Unique,
    varType :: !TcType,
    varResolvedName :: !(Maybe FcSymbolOrigin)
  }
  deriving (Eq, Ord, Show, Read)

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

fcSymbolOriginText :: FcSymbolOrigin -> Text
fcSymbolOriginText origin =
  case origin of
    FcTopLevelOrigin packageName moduleName symbolName ->
      (if packageName == "" then "" else packageName <> ":")
        <> moduleName
        <> "."
        <> symbolName
    FcBuiltinOrigin symbolName -> "builtin:" <> symbolName

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

pattern Var :: Text -> Unique -> TcType -> Var
pattern Var name unique ty <- ResolvedVar name unique ty _
  where
    Var name unique ty = ResolvedVar name unique ty Nothing

{-# COMPLETE Var #-}

data FcExpr
  = FcVar !Var
  | FcLit !Literal !TcType
  | FcApp !FcExpr !FcExpr
  | FcTyApp !FcExpr !TcType
  | FcLam !Var !FcExpr
  | FcTyLam !TyVarId !FcExpr
  | FcLet !FcBind !FcExpr
  | FcCase !FcExpr !Var ![FcAlt]
  | FcCast !FcExpr !()
  | FcCallForeign !FcForeignCall ![FcExpr]
  deriving (Eq, Show, Read)

data FcBind
  = FcNonRec !Var !FcExpr
  | FcRec ![(Var, FcExpr)]
  deriving (Eq, Show, Read)

data FcAlt = FcAlt
  { altCon :: !FcAltCon,
    altBinders :: ![Var],
    altRhs :: !FcExpr
  }
  deriving (Eq, Show, Read)

data FcAltCon
  = DataAlt !FcConstructorId
  | LitAlt !Literal !TcType
  | DefaultAlt
  deriving (Eq, Show, Read)

data Literal
  = LitInt !RuntimeRep !Integer
  | LitChar !RuntimeRep !Char
  | LitString !Text
  | LitAddr !ByteString
  deriving (Eq, Show, Read)

literalRuntimeRep :: Literal -> RuntimeRep
literalRuntimeRep literal =
  case literal of
    LitInt runtimeRep _ -> runtimeRep
    LitChar runtimeRep _ -> runtimeRep
    LitString {} -> liftedRuntimeRep
    LitAddr {} -> AddrRep
