{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Aihc.Resolve.Types
  ( pattern DeclResolution,
    pattern EResolution,
    pattern PResolution,
    pattern TResolution,
    ResolutionNamespace (..),
    ResolvedName (..),
    ResolutionAnnotation (..),
    ResolveError (..),
    ResolveResult (..),
    renderResolveResult,
    renderResolvedName,
  )
where

import Aihc.Parser.Syntax
  ( Decl (..),
    Expr (..),
    Module (..),
    Name,
    Pattern (..),
    SourceSpan (..),
    Type (..),
    UnqualifiedName,
    fromAnnotation,
    moduleName,
    renderName,
    renderUnqualifiedName,
  )
import Data.Data (Data, cast, gmapQ)
import Data.List (intercalate, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)

data ResolvedName
  = ResolvedTopLevel Name
  | ResolvedLocal Int UnqualifiedName
  | ResolvedError String
  deriving (Eq, Show, Typeable)

data ResolutionNamespace
  = ResolutionNamespaceTerm
  | ResolutionNamespaceType
  deriving (Eq, Show, Typeable)

data ResolutionAnnotation = ResolutionAnnotation
  { resolutionSpan :: !SourceSpan,
    resolutionName :: !Text,
    resolutionNamespace :: !ResolutionNamespace,
    resolutionTarget :: !ResolvedName
  }
  deriving (Eq, Show, Typeable)

newtype ResolveError
  = ResolveNotImplemented String
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

renderResolveResult :: ResolveResult -> String
renderResolveResult result =
  intercalate "\n" (map renderModuleAnnotations (sortOn fst (mergeModules (collectModules (resolvedModules result) <> resolvedAnnotations result))))

renderResolvedName :: ResolvedName -> String
renderResolvedName resolvedName =
  case resolvedName of
    ResolvedTopLevel name -> T.unpack (renderName name)
    ResolvedLocal uniqueId localName -> "Local " <> show uniqueId <> " " <> T.unpack (renderUnqualifiedName localName)
    ResolvedError msg -> "Error " <> msg

renderResolutionAnnotation :: ResolutionAnnotation -> String
renderResolutionAnnotation ann =
  renderSourceSpan (resolutionSpan ann)
    <> " "
    <> T.unpack (resolutionName ann)
    <> " => "
    <> renderResolutionNamespace (resolutionNamespace ann)
    <> " "
    <> renderResolvedName (resolutionTarget ann)

renderResolutionNamespace :: ResolutionNamespace -> String
renderResolutionNamespace namespace =
  case namespace of
    ResolutionNamespaceTerm -> "(value)"
    ResolutionNamespaceType -> "(type)"

annotationKey :: ResolutionAnnotation -> (Int, Int, Int, Int, Text)
annotationKey ann =
  case resolutionSpan ann of
    SourceSpan _ startLine startCol endLine endCol _ _ ->
      (startLine, startCol, endLine, endCol, resolutionName ann)
    NoSourceSpan ->
      (maxBound, maxBound, maxBound, maxBound, resolutionName ann)

renderSourceSpan :: SourceSpan -> String
renderSourceSpan span' =
  case span' of
    SourceSpan _ startLine startCol endLine endCol _ _ ->
      show startLine
        <> ":"
        <> show startCol
        <> "-"
        <> show endLine
        <> ":"
        <> show endCol
    NoSourceSpan -> "<no-source>"

collectModules :: [Module] -> [(Text, [ResolutionAnnotation])]
collectModules = map collectModuleAnnotations

mergeModules :: [(Text, [ResolutionAnnotation])] -> [(Text, [ResolutionAnnotation])]
mergeModules modules =
  Map.toList (Map.fromListWith (<>) modules)

collectModuleAnnotations :: Module -> (Text, [ResolutionAnnotation])
collectModuleAnnotations modu =
  (moduleDisplayName modu, collectAnnotations modu)

collectAnnotations :: (Data a) => a -> [ResolutionAnnotation]
collectAnnotations node =
  ownAnnotation node <> concat (gmapQ collectAnnotations node)

ownAnnotation :: (Data a) => a -> [ResolutionAnnotation]
ownAnnotation node =
  declResolution (cast node)
    <> patternResolution (cast node)
    <> typeResolution (cast node)
    <> exprResolution (cast node)

declResolution :: Maybe Decl -> [ResolutionAnnotation]
declResolution maybeDecl =
  case maybeDecl of
    Just (DeclResolution resolution) -> [resolution]
    _ -> []

patternResolution :: Maybe Pattern -> [ResolutionAnnotation]
patternResolution maybePattern =
  case maybePattern of
    Just (PResolution resolution) -> [resolution]
    _ -> []

typeResolution :: Maybe Type -> [ResolutionAnnotation]
typeResolution maybeType =
  case maybeType of
    Just (TResolution resolution) -> [resolution]
    _ -> []

exprResolution :: Maybe Expr -> [ResolutionAnnotation]
exprResolution maybeExpr =
  case maybeExpr of
    Just (EResolution resolution) -> [resolution]
    _ -> []

renderModuleAnnotations :: (Text, [ResolutionAnnotation]) -> String
renderModuleAnnotations (moduleNameText, annotations) =
  T.unpack moduleNameText
    <> ":\n"
    <> intercalate "\n" (map (("  " <>) . renderResolutionAnnotation) (sortOn annotationKey annotations))

moduleDisplayName :: Module -> Text
moduleDisplayName modu = fromMaybe (T.pack "<unnamed>") (moduleName modu)
