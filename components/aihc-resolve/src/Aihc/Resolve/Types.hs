{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Aihc.Resolve.Types
  ( pattern DeclResolution,
    pattern EResolution,
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
    GlobalName (..),
    LocalId (..),
    Entity (.., GlobalTerm),
    globalTerm,
    Resolution (..),
    ResolutionAnnotation (..),
    annotationResolution,
    resolutionOf,
    nameResolution,
    termResolution,
    typeResolution,
    binderResolution,
    binderEntity,
    nameEntity,
    nameOrigin,
    ResolveError (..),
    resolveErrorAt,
    ResolveFailure (..),
    ResolvedModule (..),
    ResolvedUnit (..),
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    Decl (..),
    Expr (..),
    Extension,
    Module (..),
    Name (..),
    Pattern (..),
    SourceSpan,
    TupleFlavor (..),
    Type (..),
    UnqualifiedName (..),
    fromAnnotation,
  )
import Control.DeepSeq (NFData)
import Data.List (find)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Set (Set)
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

-- | The identity of one top-level entity: the package and the module that
-- define it, its name there, and the namespace it lives in. Every phase
-- after name resolution names a top-level entity by this record.
--
-- The derived 'Ord' compares the fields in the order they are declared, and
-- the name comes first deliberately: it is what discriminates, where a
-- package id is a long 'Text' that a whole package shares.
data GlobalName = GlobalName
  { globalNameText :: !Text,
    globalNamePackage :: !PackageId,
    globalNameModule :: !Text,
    globalNameNamespace :: !ResolutionNamespace
  }
  deriving (Eq, Ord, Show, Read, Generic)

-- | The identity of one local binder, unique within a compilation unit.
newtype LocalId = LocalId {localIdUnique :: Int}
  deriving (Eq, Ord, Show, Read, Generic)

-- | What a name stands for once it is resolved.
data Entity
  = -- | A top-level entity of some module.
    EntityGlobal !GlobalName
  | -- | A local binder.
    EntityLocal !LocalId
  | -- | Built-in syntax with no declaration, such as a tuple constructor.
    -- The identifier of the annotation says which syntax.
    EntitySyntax
  deriving (Eq, Ord, Show, Read, Generic)

-- | A top-level term, by package, module, and name.
pattern GlobalTerm :: PackageId -> Text -> Text -> Entity
pattern GlobalTerm package modu name = EntityGlobal (GlobalName name package modu ResolutionNamespaceTerm)

-- | The entity of a top-level term, by package, module, and name.
globalTerm :: PackageId -> Text -> Text -> Entity
globalTerm = GlobalTerm

-- | The outcome of one lookup inside the resolver: the entity, or the
-- reason there is none. Only 'Resolved' leaves the resolver as a
-- 'ResolutionAnnotation'. 'Unresolved' leaves it as a 'ResolveError'.
data Resolution
  = Resolved !Entity
  | Unresolved String
  deriving (Eq, Show)

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

instance NFData GlobalName

instance NFData LocalId

instance NFData Entity

instance NFData Identifier

instance NFData ResolutionNamespace

-- | A resolution that succeeded, attached to the syntax it resolves.
data ResolutionAnnotation = ResolutionAnnotation
  { -- | Where the identifier is in the source, or 'Nothing' for syntax the
    -- compiler synthesized.
    resolutionSpan :: !(Maybe SourceSpan),
    resolutionIdentifier :: !Identifier,
    resolutionNamespace :: !ResolutionNamespace,
    resolutionTarget :: !Entity
  }
  deriving (Eq, Show)

-- | The resolution that one annotation carries, if it is one.
annotationResolution :: Annotation -> Maybe ResolutionAnnotation
annotationResolution = fromAnnotation

-- | The first resolution of the given namespace among some annotations.
resolutionOf :: (ResolutionNamespace -> Bool) -> [Annotation] -> Maybe ResolutionAnnotation
resolutionOf wanted = find (wanted . resolutionNamespace) . mapMaybe annotationResolution

-- | The first resolution of a name occurrence, in any namespace.
nameResolution :: Name -> Maybe ResolutionAnnotation
nameResolution = listToMaybe . mapMaybe annotationResolution . nameAnns

-- | The term resolution of a name occurrence.
termResolution :: Name -> Maybe ResolutionAnnotation
termResolution = resolutionOf (== ResolutionNamespaceTerm) . nameAnns

-- | The type resolution of a name occurrence. A promoted data constructor
-- at the type level resolves in the term namespace, so this is any
-- resolution that is not of a module.
typeResolution :: Name -> Maybe ResolutionAnnotation
typeResolution = resolutionOf (/= ResolutionNamespaceModule) . nameAnns

-- | The resolution of a binder, whatever namespace it binds in.
binderResolution :: UnqualifiedName -> Maybe ResolutionAnnotation
binderResolution = listToMaybe . mapMaybe annotationResolution . unqualifiedNameAnns

-- | The entity that a binder defines.
binderEntity :: UnqualifiedName -> Maybe Entity
binderEntity = fmap resolutionTarget . binderResolution

-- | The entity that a name occurrence stands for, in any namespace.
nameEntity :: Name -> Maybe Entity
nameEntity = fmap resolutionTarget . nameResolution

-- | The package and the module that define the top-level entity a name
-- occurrence stands for. A local binder or built-in syntax gives 'Nothing'.
nameOrigin :: Name -> Maybe (PackageId, Text)
nameOrigin name =
  case nameEntity name of
    Just (EntityGlobal global) -> Just (globalNamePackage global, globalNameModule global)
    _ -> Nothing

-- | One failed resolution. The resolver records it in the unit's
-- 'ResolveFailure' and attaches it to the syntax in place of a
-- 'ResolutionAnnotation'.
data ResolveError = ResolveError
  { resolveErrorSpan :: !(Maybe SourceSpan),
    resolveErrorName :: !Text,
    resolveErrorNamespace :: !ResolutionNamespace,
    resolveErrorMessage :: !String
  }
  deriving (Eq, Show)

-- | The error of one failed resolution.
resolveErrorAt :: Maybe SourceSpan -> Identifier -> ResolutionNamespace -> String -> ResolveError
resolveErrorAt span' identifier namespace message =
  ResolveError
    { resolveErrorSpan = span',
      resolveErrorName = displayIdentifier identifier,
      resolveErrorNamespace = namespace,
      resolveErrorMessage = message
    }

-- | One module after name resolution: its syntax with every name resolved,
-- and the top-level terms it can see, including through qualified imports.
data ResolvedModule = ResolvedModule
  { resolvedModuleUnit :: !ModuleUnit,
    resolvedVisibleTerms :: ![GlobalName],
    -- | The term and type names that this module defines and exports.
    resolvedExportedNames :: !(Set (ResolutionNamespace, Text))
  }
  deriving (Show)

-- | A compilation unit after name resolution. Every name of every module
-- resolved: a unit with a failed resolution is a 'ResolveFailure' instead.
newtype ResolvedUnit = ResolvedUnit
  { resolvedModules :: [ResolvedModule]
  }
  deriving (Show)

-- | A compilation unit that did not resolve. The modules carry the
-- resolutions that succeeded and, as a 'ResolveError' annotation, each one
-- that failed. Tools render them. The compiler does not go on from here.
data ResolveFailure = ResolveFailure
  { failureErrors :: [ResolveError],
    failureModules :: [ResolvedModule]
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
