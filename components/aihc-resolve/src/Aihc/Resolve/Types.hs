{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Aihc.Resolve.Types
  ( pattern DeclResolution,
    pattern EResolution,
    pattern ImportResolution,
    pattern PResolution,
    pattern TResolution,
    ResolutionNamespace (..),
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

data ResolvedName
  = ResolvedTopLevel Name
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
    resolvedAnnotations :: [(Text, [ResolutionAnnotation])],
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
