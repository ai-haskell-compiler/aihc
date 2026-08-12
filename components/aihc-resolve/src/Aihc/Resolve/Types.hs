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
    ResolutionForm (..),
    PackageId (..),
    Package (..),
    ModuleOrigin (..),
    unnamedPackage,
    modulesInPackage,
    ResolvedName (..),
    ResolutionAnnotation (..),
    ResolveError (..),
    ResolveResult (..),
    resolvedModuleAsts,
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

-- | An opaque identity for one installed package instance.
newtype PackageId = PackageId {packageIdText :: Text}
  deriving (Eq, Ord, Show)

-- | The user-visible package name used in imports and its opaque identity.
data Package = Package
  { packageName :: !Text,
    packageId :: !PackageId
  }
  deriving (Eq, Ord, Show)

-- | The complete source identity of one resolved module.
data ModuleOrigin = ModuleOrigin
  { moduleOriginPackageId :: !PackageId,
    moduleOriginName :: !Text
  }
  deriving (Eq, Ord, Show)

unnamedPackage :: Package
unnamedPackage = Package "" (PackageId "")

modulesInPackage :: Package -> [Module] -> [(Package, Module)]
modulesInPackage package = map pairWithPackage
  where
    pairWithPackage modu = (package, modu)

data ResolvedName
  = ResolvedTopLevel PackageId Name
  | ResolvedLocal Int UnqualifiedName
  | ResolvedBuiltin Text
  | ResolvedError String
  deriving (Eq, Show)

data ResolutionNamespace
  = ResolutionNamespaceTerm
  | ResolutionNamespaceType
  | ResolutionNamespaceModule
  deriving (Eq, Show)

-- | The syntax form that caused one resolution request.
data ResolutionForm
  = ResolutionNamed
  | ResolutionTuple
  deriving (Eq, Show)

data ResolutionAnnotation = ResolutionAnnotation
  { resolutionSpan :: !SourceSpan,
    resolutionName :: !Text,
    resolutionNamespace :: !ResolutionNamespace,
    resolutionTarget :: !ResolvedName,
    resolutionForm :: !ResolutionForm
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
  { resolvedModules :: [(Package, Module)],
    resolveErrors :: [ResolveError]
  }
  deriving (Show)

resolvedModuleAsts :: ResolveResult -> [Module]
resolvedModuleAsts = map snd . resolvedModules

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
