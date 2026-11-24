{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Aihc.Resolve.Types
  ( ResolutionNamespace (..),
    Identifier (..),
    displayIdentifier,
    PackageId (..),
    Package (..),
    unnamedPackage,
    ModuleUnit (..),
    modulesInPackage,
    GlobalName (..),
    ExactTerm (..),
    exactTerm,
    ExactType (..),
    exactType,
    LocalId (..),
    Entity (.., GlobalTerm),
    globalTerm,
    Resolution (..),
    Resolved (..),
    RecKnotVariables (..),
    ResolutionResult (..),
    resolvedOf,
    ResolveError (..),
    resolveErrorAt,
  )
where

import Aihc.Parser.Syntax
  ( Extension,
    Module (..),
    Name (..),
    SourceSpan,
    TupleFlavor (..),
    fromAnnotation,
  )
import Control.DeepSeq (NFData)
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set
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

-- | A mark on a name occurrence that gives its top-level term. The resolver
-- gives the occurrence that term and does not look in the scope. The
-- result of a Template Haskell splice has such names: a quote binds a name
-- to its definition, and the scope at the splice can be different.
newtype ExactTerm = ExactTerm GlobalName
  deriving (Eq, Show)

-- | The term that a mark on the name gives, if the name has a mark.
exactTerm :: Name -> Maybe GlobalName
exactTerm name = listToMaybe [global | Just (ExactTerm global) <- map fromAnnotation (nameAnns name)]

-- | A mark on a type constructor occurrence that gives its top-level type
-- or class, as 'ExactTerm' does for a term.
newtype ExactType = ExactType GlobalName
  deriving (Eq, Show)

-- | The type that a mark on the name gives, if the name has a mark.
exactType :: Name -> Maybe GlobalName
exactType name = listToMaybe [global | Just (ExactType global) <- map fromAnnotation (nameAnns name)]

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
-- reason there is none. Only 'Found' leaves the resolver as a
-- 'Resolved'. 'NotFound' leaves it as a 'ResolveError'.
data Resolution
  = Found !Entity
  | NotFound String
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
data Resolved = Resolved
  { -- | Where the identifier is in the source, or 'Nothing' for syntax the
    -- compiler synthesized.
    resolutionSpan :: !(Maybe SourceSpan),
    resolutionIdentifier :: !Identifier,
    resolutionNamespace :: !ResolutionNamespace,
    resolutionTarget :: !Entity
  }
  deriving (Eq, Show, Generic)

instance NFData Resolved

-- | The outcome of one resolution in the syntax tree that the resolver
-- gives: the resolution, or the error that the resolver found.
data ResolutionResult
  = ResolutionOk !Resolved
  | ResolutionError !ResolveError
  deriving (Eq, Show, Generic)

instance NFData ResolutionResult

-- | The successful resolution of a result, if it is one.
resolvedOf :: ResolutionResult -> Maybe Resolved
resolvedOf result =
  case result of
    ResolutionOk annotation -> Just annotation
    ResolutionError _ -> Nothing

-- | The variables that the knot of a @rec@ statement carries. The resolver
-- gives this annotation to each segment of an @mdo@ block. A variable of
-- the segment is in the knot when a statement uses it before the segment
-- binds it, or when a statement after the segment uses it. This is the rule
-- of GHC. A @rec@ statement without this annotation carries all of its
-- variables in the knot, as an explicit @rec@ statement does in GHC.
newtype RecKnotVariables = RecKnotVariables (Set.Set Entity)
  deriving (Eq, Show, Generic)

instance NFData RecKnotVariables

-- | One failed resolution. The resolver records it in the unit's
-- 'ResolveFailure' and attaches it to the syntax in place of a
-- 'Resolved'.
data ResolveError = ResolveError
  { resolveErrorSpan :: !(Maybe SourceSpan),
    resolveErrorName :: !Text,
    resolveErrorNamespace :: !ResolutionNamespace,
    resolveErrorMessage :: !String
  }
  deriving (Eq, Show, Generic)

instance NFData ResolveError

-- | The error of one failed resolution.
resolveErrorAt :: Maybe SourceSpan -> Identifier -> ResolutionNamespace -> String -> ResolveError
resolveErrorAt span' identifier namespace message =
  ResolveError
    { resolveErrorSpan = span',
      resolveErrorName = displayIdentifier identifier,
      resolveErrorNamespace = namespace,
      resolveErrorMessage = message
    }
