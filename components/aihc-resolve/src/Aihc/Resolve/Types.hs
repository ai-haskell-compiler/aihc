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
    Identifier (..),
    displayIdentifier,
    PackageId (..),
    Package (..),
    unnamedPackage,
    ModuleUnit (..),
    modulesInPackage,
    ResolvedName (..),
    ResolutionAnnotation (..),
    VisibleTermIdentities (..),
    ResolveError (..),
    ResolveResult (..),
    resolvedModuleAsts,
  )
where

import Aihc.Parser.Syntax
  ( Decl (..),
    Expr (..),
    Extension,
    ImportDecl (..),
    Module (..),
    Name (..),
    Pattern (..),
    SourceSpan (..),
    TupleFlavor (..),
    Type (..),
    UnqualifiedName (..),
    fromAnnotation,
  )
import Control.DeepSeq (NFData)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | An opaque identity for one installed package instance.
newtype PackageId = PackageId {packageIdText :: Text}
  deriving (Eq, Ord, Show, Read, Generic)

instance IsString PackageId where
  fromString = PackageId . T.pack

-- | The user-visible package name used in imports and its opaque identity.
data Package = Package
  { packageName :: !Text,
    packageId :: !PackageId
  }
  deriving (Eq, Ord, Show, Generic)

unnamedPackage :: Package
unnamedPackage = Package "" (PackageId "main")

-- | One module as every phase after parsing sees it: the package it belongs
-- to, the language extensions in force for it, and its syntax tree.
--
-- Language pragmas are a source-level notion. Whoever reads the source folds
-- the language edition, the package's default extensions and the module's own
-- @LANGUAGE@ pragmas into one extension set and hands that set on. No later
-- phase reads 'moduleLanguagePragmas' again.
data ModuleUnit = ModuleUnit
  { moduleUnitPackage :: !Package,
    moduleUnitExtensions :: ![Extension],
    moduleUnitAst :: !Module
  }
  deriving (Show)

-- | Attach one package to modules that already know their extensions.
modulesInPackage :: Package -> [(Module, [Extension])] -> [ModuleUnit]
modulesInPackage package = map unitInPackage
  where
    unitInPackage (modu, extensions) = ModuleUnit package extensions modu

-- | Global term identities visible in one module, including qualified imports.
newtype VisibleTermIdentities = VisibleTermIdentities [(PackageId, Text, Text)]
  deriving (Eq, Show)

data ResolvedName
  = -- | A top-level entity, by the package and the module that define it
    -- and its name there. Every top-level entity has a defining module, so
    -- the module is a field of its own rather than the optional qualifier
    -- of the source name, which is only the spelling of an occurrence.
    ResolvedTopLevel PackageId Text Name
  | ResolvedLocal Int UnqualifiedName
  | ResolvedSyntax
  | ResolvedError String
  deriving (Eq, Show, Generic)

-- | The source identifier that caused one resolution request.
data Identifier
  = IdentifierTuple !TupleFlavor !Int
  | IdentifierList
  | IdentifierNamed !Text
  deriving (Eq, Show, Generic)

-- | Render an identifier for diagnostics and other user output.
displayIdentifier :: Identifier -> Text
displayIdentifier identifier =
  case identifier of
    IdentifierTuple flavor arity ->
      case flavor of
        Boxed -> "(" <> T.replicate (max 0 (arity - 1)) "," <> ")"
        Unboxed -> "(#" <> T.replicate (max 0 (arity - 1)) "," <> "#)"
    IdentifierList -> "[]"
    IdentifierNamed name -> name

data ResolutionNamespace
  = ResolutionNamespaceTerm
  | ResolutionNamespaceType
  | ResolutionNamespaceModule
  deriving (Eq, Ord, Show, Read, Generic)

instance NFData PackageId

instance NFData Package

instance NFData ResolvedName

instance NFData Identifier

instance NFData ResolutionNamespace

data ResolutionAnnotation = ResolutionAnnotation
  { resolutionSpan :: !SourceSpan,
    resolutionIdentifier :: !Identifier,
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
  { resolvedModules :: [ModuleUnit],
    resolveErrors :: [ResolveError]
  }
  deriving (Show)

resolvedModuleAsts :: ResolveResult -> [Module]
resolvedModuleAsts = map moduleUnitAst . resolvedModules

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
