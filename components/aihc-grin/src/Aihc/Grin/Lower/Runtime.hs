{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Runtime-only input for GRIN lowering.
module Aihc.Grin.Lower.Runtime
  ( RuntimeExpr (..),
    RuntimeVar (RuntimeVar, varName, varUnique, varType, varResolvedName),
    RuntimeSymbolOrigin (..),
    runtimeSymbolOriginText,
    runtimeExternalVar,
    RuntimeBind (..),
    RuntimeTopBind (..),
    RuntimeDataDecl (..),
    RuntimeDataConDecl (..),
    RuntimeConstructorId (..),
    runtimeConstructorSymbolOrigin,
    RuntimeModuleId (..),
    runtimeModulePackageText,
    RuntimeProgram (..),
    RuntimeForeignCall (..),
    RuntimeForeignSignature (..),
    RuntimeForeignEffect (..),
    RuntimeForeignType (..),
    runtimeForeignOperandTypes,
    runtimeForeignCallResultType,
    RuntimeAlt (..),
    RuntimeAltCon (..),
    Literal (..),
    literalRuntimeRep,
    RuntimeRep,
    Levity,
    VecCount,
    VecElem,
    RuntimeLayout (..),
    RuntimeType (..),
    Unique (..),
    liftedRuntimeRep,
    runtimeType,
    runtimeRepArgument,
    runtimeRepVariable,
    runtimeRepOfRuntimeType,
    substRuntimeType,
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
import Data.Text (Text)
import Data.Text qualified as T

type RuntimeRep = GrinRep

type Levity = GrinLevity

type VecCount = GrinVecCount

type VecElem = GrinVecElem

liftedRuntimeRep :: RuntimeRep
liftedRuntimeRep = liftedGrinRep

-- | Runtime layout information that remains after FC2 type erasure.
data RuntimeLayout
  = ConcreteLayout !RuntimeRep
  | VariableLayout !Unique
  deriving (Eq, Ord, Show, Read)

-- | Calling information for one FC2 expression.
-- This data contains no Haskell type or kind information.
data RuntimeType
  = RuntimeValue !RuntimeLayout
  | RuntimeRepArgument !RuntimeLayout
  | RuntimeFunction !RuntimeType !RuntimeType
  | RuntimeForAll !(Maybe Unique) !RuntimeType
  | RuntimeErased
  deriving (Eq, Ord, Show, Read)

runtimeType :: RuntimeRep -> RuntimeType
runtimeType = RuntimeValue . ConcreteLayout

runtimeRepArgument :: RuntimeRep -> RuntimeType
runtimeRepArgument = RuntimeRepArgument . ConcreteLayout

runtimeRepVariable :: Unique -> RuntimeType
runtimeRepVariable = RuntimeRepArgument . VariableLayout

runtimeRepOfRuntimeType :: RuntimeType -> Either String RuntimeRep
runtimeRepOfRuntimeType runtimeTypeInfo =
  case runtimeTypeInfo of
    RuntimeValue (ConcreteLayout representation) -> pure representation
    RuntimeValue (VariableLayout _) -> pure liftedRuntimeRep
    RuntimeFunction {} -> pure liftedRuntimeRep
    RuntimeForAll _ body -> runtimeRepOfRuntimeType body
    RuntimeRepArgument {} -> Left "runtime representation argument is not a value"
    RuntimeErased -> Left "erased type has no runtime layout"

substRuntimeType :: Maybe Unique -> RuntimeType -> RuntimeType -> RuntimeType
substRuntimeType binder argument =
  case (binder, argument) of
    (Just unique, RuntimeRepArgument layout) -> replaceLayout unique layout
    _ -> id

replaceLayout :: Unique -> RuntimeLayout -> RuntimeType -> RuntimeType
replaceLayout unique replacement runtimeTypeInfo =
  case runtimeTypeInfo of
    RuntimeValue (VariableLayout variable)
      | variable == unique -> RuntimeValue replacement
    RuntimeFunction argument result ->
      RuntimeFunction (replaceLayout unique replacement argument) (replaceLayout unique replacement result)
    RuntimeForAll binder body
      | binder == Just unique -> runtimeTypeInfo
      | otherwise -> RuntimeForAll binder (replaceLayout unique replacement body)
    _ -> runtimeTypeInfo

data RuntimeModuleId = RuntimeModuleId
  { runtimeModulePackage :: !PackageId,
    runtimeModuleName :: !Text
  }
  deriving (Eq, Show, Read)

runtimeModulePackageText :: RuntimeModuleId -> Text
runtimeModulePackageText = packageIdText . runtimeModulePackage

data RuntimeProgram = RuntimeProgram
  { runtimeProgramModule :: !RuntimeModuleId,
    runtimeTopBinds :: ![RuntimeTopBind]
  }
  deriving (Eq, Show, Read)

data RuntimeTopBind
  = RuntimeData !RuntimeDataDecl
  | RuntimePrimitive !RuntimeVar !Int
  | RuntimeForeignImport !RuntimeForeignCall
  | RuntimeTopValue !RuntimeBind
  deriving (Eq, Show, Read)

data RuntimeDataDecl = RuntimeDataDecl
  { runtimeDataIsUnboxedTuple :: !Bool,
    runtimeDataConstructors :: ![RuntimeDataConDecl]
  }
  deriving (Eq, Show, Read)

data RuntimeDataConDecl = RuntimeDataConDecl
  { runtimeDataConOrigin :: !RuntimeConstructorId,
    runtimeDataConName :: !Text,
    runtimeDataConFields :: ![RuntimeType]
  }
  deriving (Eq, Show, Read)

data RuntimeForeignCall = RuntimeForeignCall
  { runtimeForeignCallName :: !Text,
    runtimeForeignCallSymbol :: !Text,
    runtimeForeignCallSignature :: !RuntimeForeignSignature
  }
  deriving (Eq, Show, Read)

data RuntimeForeignSignature = RuntimeForeignSignature
  { runtimeForeignArgumentTypes :: ![RuntimeForeignType],
    runtimeForeignResultType :: !RuntimeForeignType,
    runtimeForeignEffect :: !RuntimeForeignEffect
  }
  deriving (Eq, Show, Read)

data RuntimeForeignEffect = RuntimeForeignPure | RuntimeForeignRealWorld
  deriving (Eq, Show, Read)

data RuntimeForeignType
  = RuntimeForeignInt
  | RuntimeForeignInt32
  | RuntimeForeignWord64
  | RuntimeForeignAddr
  deriving (Eq, Show, Read)

runtimeForeignOperandTypes :: RuntimeForeignSignature -> [RuntimeType]
runtimeForeignOperandTypes signature =
  map foreignPrimitiveType (runtimeForeignArgumentTypes signature)
    <> case runtimeForeignEffect signature of
      RuntimeForeignPure -> []
      RuntimeForeignRealWorld -> [runtimeType (TupleRep [])]

runtimeForeignCallResultType :: RuntimeForeignSignature -> RuntimeType
runtimeForeignCallResultType signature =
  case runtimeForeignEffect signature of
    RuntimeForeignPure -> foreignPrimitiveType (runtimeForeignResultType signature)
    RuntimeForeignRealWorld ->
      runtimeType
        ( TupleRep
            [ TupleRep [],
              foreignTypeRuntimeRep (runtimeForeignResultType signature)
            ]
        )

foreignPrimitiveType :: RuntimeForeignType -> RuntimeType
foreignPrimitiveType = runtimeType . foreignTypeRuntimeRep

foreignTypeRuntimeRep :: RuntimeForeignType -> RuntimeRep
foreignTypeRuntimeRep foreignType =
  case foreignType of
    RuntimeForeignInt -> IntRep
    RuntimeForeignInt32 -> Int32Rep
    RuntimeForeignWord64 -> Word64Rep
    RuntimeForeignAddr -> AddrRep

data RuntimeVar = ResolvedRuntimeVar
  { varName :: !Text,
    varUnique :: !Unique,
    varType :: !RuntimeType,
    varResolvedName :: !(Maybe RuntimeSymbolOrigin)
  }
  deriving (Eq, Ord, Show, Read)

data RuntimeSymbolOrigin
  = RuntimeTopLevelOrigin
  { runtimeOriginPackage :: !Text,
    runtimeOriginModule :: !Text,
    runtimeOriginName :: !Text
  }
  deriving (Eq, Ord, Show, Read)

data RuntimeConstructorId = RuntimeConstructorId
  { runtimeConstructorPackage :: !PackageId,
    runtimeConstructorModule :: !Text,
    runtimeConstructorSourceName :: !Text
  }
  deriving (Eq, Ord, Show, Read)

runtimeConstructorSymbolOrigin :: RuntimeConstructorId -> RuntimeSymbolOrigin
runtimeConstructorSymbolOrigin constructor =
  RuntimeTopLevelOrigin
    (packageIdText (runtimeConstructorPackage constructor))
    (runtimeConstructorModule constructor)
    (runtimeConstructorSourceName constructor)

runtimeSymbolOriginText :: RuntimeSymbolOrigin -> Text
runtimeSymbolOriginText (RuntimeTopLevelOrigin packageName moduleName symbolName) =
  (if packageName == "" then "" else packageName <> ":")
    <> moduleName
    <> "."
    <> symbolName

runtimeExternalVar :: RuntimeSymbolOrigin -> RuntimeType -> RuntimeVar
runtimeExternalVar origin ty =
  ResolvedRuntimeVar
    { varName = runtimeOriginName origin,
      varUnique = Unique (-2000000000 - abs (hash `rem` 1000000000)),
      varType = ty,
      varResolvedName = Just origin
    }
  where
    key = runtimeSymbolOriginText origin
    hash = T.foldl' (\value character -> value * 33 + ord character) 5381 key

pattern RuntimeVar :: Text -> Unique -> RuntimeType -> RuntimeVar
pattern RuntimeVar name unique ty <- ResolvedRuntimeVar name unique ty _
  where
    RuntimeVar name unique ty = ResolvedRuntimeVar name unique ty Nothing

{-# COMPLETE RuntimeVar #-}

data RuntimeExpr
  = RuntimeVarExpr !RuntimeVar
  | RuntimeLit !Literal !RuntimeType
  | RuntimeApp !RuntimeExpr !RuntimeExpr
  | RuntimeTyApp !RuntimeExpr !RuntimeType
  | RuntimeLam !RuntimeVar !RuntimeExpr
  | RuntimeTyLam !(Maybe Unique) !RuntimeExpr
  | RuntimeLet !RuntimeBind !RuntimeExpr
  | RuntimeCase !RuntimeExpr !RuntimeVar ![RuntimeAlt]
  | RuntimeCallForeign !RuntimeForeignCall ![RuntimeExpr]
  deriving (Eq, Show, Read)

data RuntimeBind
  = RuntimeNonRec !RuntimeVar !RuntimeExpr
  | RuntimeRec ![(RuntimeVar, RuntimeExpr)]
  deriving (Eq, Show, Read)

data RuntimeAlt = RuntimeAlt
  { altCon :: !RuntimeAltCon,
    altBinders :: ![RuntimeVar],
    altRhs :: !RuntimeExpr
  }
  deriving (Eq, Show, Read)

data RuntimeAltCon
  = DataAlt !RuntimeConstructorId
  | LitAlt !Literal !RuntimeType
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
