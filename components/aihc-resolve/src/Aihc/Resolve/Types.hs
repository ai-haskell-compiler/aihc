{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Aihc.Resolve.Types
  ( pattern DeclResolution,
    pattern EResolution,
    pattern ImportResolution,
    pattern PResolution,
    pattern TResolution,
    ResolutionNamespace (..),
    PackageId (..),
    parsePackageId,
    renderPackageId,
    ResolvedName (..),
    ResolutionAnnotation (..),
    ResolveError (..),
    ResolveResult (..),
  )
where

import Aihc.Parser.Syntax
  ( Decl (..),
    Expr (..),
    ImportDecl (..),
    Module (..),
    Name (..),
    Pattern (..),
    SourceSpan (..),
    Type (..),
    UnqualifiedName (..),
    fromAnnotation,
  )
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | An installed package identity, split at the unambiguous version and hash
-- suffix so package names may themselves contain dashes.
data PackageId = PackageId
  { packageIdName :: !Text,
    packageIdVersion :: !Text,
    packageIdHash :: !Text
  }
  deriving (Eq, Ord, Show)

parsePackageId :: Text -> Maybe PackageId
parsePackageId raw =
  case reverse (T.splitOn "-" raw) of
    packageHash : version : reversedName
      | not (T.null packageHash),
        validVersion version,
        not (null reversedName),
        let packageName = T.intercalate "-" (reverse reversedName),
        not (T.null packageName) ->
          Just (PackageId packageName version packageHash)
    _ -> Nothing
  where
    validVersion version =
      all (\component -> not (T.null component) && T.all (`elem` ['0' .. '9']) component) (T.splitOn "." version)

renderPackageId :: PackageId -> Text
renderPackageId packageId =
  T.intercalate "-" [packageIdName packageId, packageIdVersion packageId, packageIdHash packageId]

data ResolvedName
  = ResolvedTopLevel Name
  | ResolvedPackageTopLevel PackageId Name
  | ResolvedLocal Int UnqualifiedName
  | ResolvedBuiltin Text
  | ResolvedError String
  deriving (Eq, Show)

data ResolutionNamespace
  = ResolutionNamespaceTerm
  | ResolutionNamespaceType
  | ResolutionNamespaceModule
  deriving (Eq, Show)

data ResolutionAnnotation = ResolutionAnnotation
  { resolutionSpan :: !SourceSpan,
    resolutionName :: !Text,
    resolutionNamespace :: !ResolutionNamespace,
    resolutionTarget :: !ResolvedName
  }
  deriving (Eq, Show)

data ResolveError
  = ResolveResolutionError
      { resolveErrorSpan :: !SourceSpan,
        resolveErrorName :: !Text,
        resolveErrorNamespace :: !ResolutionNamespace,
        resolveErrorMessage :: !String
      }
  | ResolveNotImplemented String
  deriving (Eq, Show)

data ResolveResult = ResolveResult
  { resolvedModules :: [Module],
    resolveErrors :: [ResolveError]
  }
  deriving (Show)

pattern DeclResolution :: ResolutionAnnotation -> Decl
pattern DeclResolution resolution <- DeclAnn (fromAnnotation -> Just resolution) _

pattern PResolution :: ResolutionAnnotation -> Pattern
pattern PResolution resolution <- PAnn (fromAnnotation -> Just resolution) _

pattern TResolution :: ResolutionAnnotation -> Type
pattern TResolution resolution <- TAnn (fromAnnotation -> Just resolution) _

pattern EResolution :: ResolutionAnnotation -> Expr
pattern EResolution resolution <- EAnn (fromAnnotation -> Just resolution) _

pattern ImportResolution :: ResolutionAnnotation -> ImportDecl
pattern ImportResolution resolution <- (importResolutionAnnotation -> Just resolution)

importResolutionAnnotation :: ImportDecl -> Maybe ResolutionAnnotation
importResolutionAnnotation = listToMaybe . importResolutionAnnotations

importResolutionAnnotations :: ImportDecl -> [ResolutionAnnotation]
importResolutionAnnotations = mapMaybe fromAnnotation . importDeclAnns
