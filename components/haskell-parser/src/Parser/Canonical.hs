{-# LANGUAGE OverloadedStrings #-}

module Parser.Canonical
  ( CanonicalCallConv (..),
    CanonicalDecl (..),
    CanonicalExpr (..),
    CanonicalForeignDirection (..),
    CanonicalForeignSafety (..),
    CanonicalModule (..),
    normalizeDecl,
    normalizeExpr,
    normalizeModule,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Parser.Ast

data CanonicalModule = CanonicalModule
  { canonicalModuleName :: Maybe Text,
    canonicalDecls :: [CanonicalDecl]
  }
  deriving (Eq, Show)

data CanonicalDecl
  = CanonicalValueDecl
      { canonicalDeclName :: Text,
        canonicalDeclExpr :: CanonicalExpr
      }
  | CanonicalTypeSigDecl
      { canonicalTypeSigName :: Text
      }
  | CanonicalFunctionDecl
      { canonicalFunctionName :: Text
      }
  | CanonicalDataDecl
      { canonicalTypeName :: Text,
        canonicalConstructors :: [Text]
      }
  | CanonicalForeignDecl
      { canonicalForeignDirection :: CanonicalForeignDirection,
        canonicalForeignCallConv :: CanonicalCallConv,
        canonicalForeignSafety :: Maybe CanonicalForeignSafety,
        canonicalForeignEntity :: Maybe Text,
        canonicalForeignName :: Text
      }
  deriving (Eq, Show)

data CanonicalForeignDirection
  = CanonicalForeignImport
  | CanonicalForeignExport
  deriving (Eq, Show)

data CanonicalCallConv
  = CanonicalCCall
  | CanonicalStdCall
  deriving (Eq, Show)

data CanonicalForeignSafety
  = CanonicalSafe
  | CanonicalUnsafe
  deriving (Eq, Show)

data CanonicalExpr
  = CVar Text
  | CInt Integer
  | CApp CanonicalExpr CanonicalExpr
  deriving (Eq, Show)

normalizeModule :: Module -> CanonicalModule
normalizeModule m =
  CanonicalModule
    { canonicalModuleName = moduleName m,
      canonicalDecls = fmap normalizeDecl (moduleDecls m)
    }

normalizeDecl :: Decl -> CanonicalDecl
normalizeDecl d =
  case d of
    Decl {declName = name, declExpr = expr} ->
      CanonicalValueDecl
        { canonicalDeclName = name,
          canonicalDeclExpr = normalizeExpr expr
        }
    TypeSigDecl {typeSigName = name} ->
      CanonicalTypeSigDecl
        { canonicalTypeSigName = name
        }
    FunctionDecl {functionName = name} ->
      CanonicalFunctionDecl
        { canonicalFunctionName = name
        }
    DataDecl {dataTypeName = typeName, dataConstructors = ctors} ->
      CanonicalDataDecl
        { canonicalTypeName = typeName,
          canonicalConstructors = ctors
        }
    ForeignDecl
      { foreignDirection = direction,
        foreignCallConv = callConv,
        foreignSafety = safety,
        foreignEntity = entity,
        foreignName = name
      } ->
        CanonicalForeignDecl
          { canonicalForeignDirection = normalizeDirection direction,
            canonicalForeignCallConv = normalizeCallConv callConv,
            canonicalForeignSafety = fmap normalizeSafety safety,
            canonicalForeignEntity = fmap classifyForeignEntity entity,
            canonicalForeignName = name
          }

normalizeExpr :: Expr -> CanonicalExpr
normalizeExpr expr =
  case expr of
    EVar name -> CVar name
    EInt value -> CInt value
    EApp fn arg -> CApp (normalizeExpr fn) (normalizeExpr arg)

normalizeDirection :: ForeignDirection -> CanonicalForeignDirection
normalizeDirection direction =
  case direction of
    ForeignImport -> CanonicalForeignImport
    ForeignExport -> CanonicalForeignExport

normalizeCallConv :: CallConv -> CanonicalCallConv
normalizeCallConv callConv =
  case callConv of
    CCall -> CanonicalCCall
    StdCall -> CanonicalStdCall

normalizeSafety :: ForeignSafety -> CanonicalForeignSafety
normalizeSafety safety =
  case safety of
    Safe -> CanonicalSafe
    Unsafe -> CanonicalUnsafe

classifyForeignEntity :: Text -> Text
classifyForeignEntity entity
  | entity == "dynamic" = "dynamic"
  | entity == "wrapper" = "wrapper"
  | "static " `T.isPrefixOf` entity = "static"
  | "&" `T.isPrefixOf` entity = "address"
  | otherwise = "named"
