{-# LANGUAGE DeriveDataTypeable #-}

module Aihc.Resolve.Types
  ( ResolvedName (..),
    ResolutionAnnotation (..),
    ResolveError (..),
    ResolveResult (..),
    renderResolveResult,
    renderResolvedName,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    Decl (..),
    Expr (..),
    Module (..),
    Name,
    Pattern (..),
    SourceSpan (..),
    UnqualifiedName,
    fromAnnotation,
    renderName,
    renderUnqualifiedName,
  )
import Data.Data (Data, cast, gmapQ)
import Data.List (intercalate, sortOn)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (Typeable)

data ResolvedName
  = ResolvedTopLevel Name
  | ResolvedLocal Int UnqualifiedName
  | ResolvedError String
  deriving (Eq, Show, Typeable)

data ResolutionAnnotation = ResolutionAnnotation
  { resolutionSpan :: !SourceSpan,
    resolutionName :: !Text,
    resolutionTarget :: !ResolvedName
  }
  deriving (Eq, Show, Typeable)

newtype ResolveError
  = ResolveNotImplemented String
  deriving (Eq, Show)

data ResolveResult = ResolveResult
  { resolvedModules :: [Module],
    resolveErrors :: [ResolveError]
  }
  deriving (Show)

renderResolveResult :: ResolveResult -> String
renderResolveResult result =
  intercalate "\n" (map renderResolutionAnnotation (sortOn annotationKey (collectModules (resolvedModules result))))

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
    <> renderResolvedName (resolutionTarget ann)

annotationKey :: ResolutionAnnotation -> (FilePath, Int, Int, Int, Int, Text)
annotationKey ann =
  case resolutionSpan ann of
    SourceSpan path startLine startCol endLine endCol _ _ ->
      (path, startLine, startCol, endLine, endCol, resolutionName ann)
    NoSourceSpan ->
      ("", maxBound, maxBound, maxBound, maxBound, resolutionName ann)

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

collectModules :: [Module] -> [ResolutionAnnotation]
collectModules = concatMap (concatMap collectAnnotations . moduleDecls)

collectAnnotations :: (Data a) => a -> [ResolutionAnnotation]
collectAnnotations node =
  ownAnnotation node <> concat (gmapQ collectAnnotations node)

ownAnnotation :: (Data a) => a -> [ResolutionAnnotation]
ownAnnotation node =
  case cast node of
    Just decl ->
      case decl of
        DeclAnn ann _ -> maybeAnnotation ann
        _ -> []
    Nothing ->
      case cast node of
        Just pat ->
          case pat of
            PAnn ann _ -> maybeAnnotation ann
            _ -> []
        Nothing ->
          case cast node of
            Just expr ->
              case expr of
                EAnn ann _ -> maybeAnnotation ann
                _ -> []
            Nothing -> []

maybeAnnotation :: Annotation -> [ResolutionAnnotation]
maybeAnnotation ann =
  case fromAnnotation ann :: Maybe ResolutionAnnotation of
    Just resolution -> [resolution]
    Nothing -> []
