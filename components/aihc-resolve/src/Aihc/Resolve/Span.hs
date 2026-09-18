{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}

module Aihc.Resolve.Span
  ( pushSpanFromAnn,
    sourceSpanFromAnns,
    peelDeclSpan,
    peelPatternSpan,
    peelGuardQualifierSpan,
    peelImportItemSpan,
    rhsSpan,
    unhandledSyntaxName,
    spanStartNameSpan,
    annotateDecl,
    annotateExpr,
    annotatePattern,
    annotateType,
    annotateImport,
    importModuleNameSpan,
    importMemberNameSpan,
    declKeywordNameSpan,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    ClassDeclItem (..),
    Decl (..),
    Expr (..),
    GuardQualifier (..),
    ImportDecl (..),
    ImportItem (..),
    ImportLevel (..),
    InstanceDeclItem (..),
    Pattern (..),
    Rhs (..),
    SourceSpan,
    Type (..),
    fromAnnotation,
    mkAnnotation,
    pattern SourceSpan,
  )
import Aihc.Resolve.Types
import Control.Applicative ((<|>))
import Data.Data (Data, showConstr, toConstr)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | The innermost span of a chain of annotations: a 'SourceSpan' on the
-- annotation if it carries one, and otherwise the span already in hand.
--
-- Syntax the compiler synthesized carries no 'SourceSpan' annotation at all,
-- so a span the resolver looks for is a 'Maybe'. A span it has found is a
-- concrete span; @aihc-parser@ has no empty one.
pushSpanFromAnn :: Maybe SourceSpan -> Annotation -> Maybe SourceSpan
pushSpanFromAnn cur ann = fromAnnotation @SourceSpan ann <|> cur

-- | The first source span embedded in a list of annotations.
sourceSpanFromAnns :: [Annotation] -> Maybe SourceSpan
sourceSpanFromAnns = listToMaybe . mapMaybe (fromAnnotation @SourceSpan)

-- | Resolver-owned span tracking for nodes that now store source spans only in
-- annotations.
peelDeclSpan :: Decl -> (Maybe SourceSpan, Decl)
peelDeclSpan = go Nothing
  where
    go ambient (DeclAnn ann inner) = go (pushSpanFromAnn ambient ann) inner
    go ambient decl = (ambient, decl)

peelPatternSpan :: Pattern -> Maybe SourceSpan
peelPatternSpan = go Nothing
  where
    go ambient (PAnn ann inner) = go (pushSpanFromAnn ambient ann) inner
    go ambient _ = ambient

peelGuardQualifierSpan :: GuardQualifier -> Maybe SourceSpan
peelGuardQualifierSpan = go Nothing
  where
    go ambient (GuardAnn ann inner) = go (pushSpanFromAnn ambient ann) inner
    go ambient _ = ambient

peelImportItemSpan :: ImportItem -> Maybe SourceSpan
peelImportItemSpan = go Nothing
  where
    go ambient (ImportAnn ann inner) = go (pushSpanFromAnn ambient ann) inner
    go ambient _ = ambient

rhsSpan :: Rhs body -> Maybe SourceSpan
rhsSpan rhs =
  case rhs of
    UnguardedRhs anns _ _ -> sourceSpanFromAnns anns
    GuardedRhss anns _ _ -> sourceSpanFromAnns anns

-- | The name a diagnostic gives to syntax the resolver has no case for:
-- the constructor of the form it met.
unhandledSyntaxName :: (Data a) => a -> Text
unhandledSyntaxName node = T.pack (showConstr (toConstr node))

-- | Narrow a span to the name that starts at it. There is nothing to narrow
-- when the syntax had no span to begin with.
spanStartNameSpan :: Maybe SourceSpan -> Text -> Maybe SourceSpan
spanStartNameSpan span' name = flip spanStartNameSpanAt name <$> span'

spanStartNameSpanAt :: SourceSpan -> Text -> SourceSpan
spanStartNameSpanAt (SourceSpan sourceName startLine startCol _ _ startOffset endOffset) name =
  let width = T.length name
   in SourceSpan
        sourceName
        startLine
        startCol
        startLine
        (startCol + width)
        startOffset
        (min endOffset (startOffset + width))

annotateDecl :: ResolutionAnnotation -> Decl -> Decl
annotateDecl annotation = DeclAnn (mkAnnotation annotation)

annotateExpr :: ResolutionAnnotation -> Expr -> Expr
annotateExpr annotation = EAnn (mkAnnotation annotation)

annotatePattern :: ResolutionAnnotation -> Pattern -> Pattern
annotatePattern annotation = PAnn (mkAnnotation annotation)

annotateType :: ResolutionAnnotation -> Type -> Type
annotateType annotation = TAnn (mkAnnotation annotation)

annotateImport :: ResolutionAnnotation -> ImportDecl -> ImportDecl
annotateImport annotation importDecl =
  importDecl {importDeclAnns = mkAnnotation annotation : importDeclAnns importDecl}

importModuleNameSpan :: ImportDecl -> Maybe SourceSpan
importModuleNameSpan importDecl =
  (\sp -> shiftSpanStartNameSpanAt sp prefixWidth (importDeclModule importDecl))
    <$> sourceSpanFromAnns (importDeclAnns importDecl)
  where
    prefixWidth =
      T.length "import "
        + safeWidth
        + sourcePragmaWidth
        + preQualifiedWidth
        + levelWidth
        + packageWidth
    safeWidth
      | importDeclSafe importDecl = T.length "safe "
      | otherwise = 0
    sourcePragmaWidth =
      case importDeclSourcePragma importDecl of
        Just _ -> T.length "{-# SOURCE #-} "
        Nothing -> 0
    preQualifiedWidth
      | importDeclQualified importDecl && not (importDeclQualifiedPost importDecl) = T.length "qualified "
      | otherwise = 0
    levelWidth =
      case importDeclLevel importDecl of
        Just ImportLevelQuote -> T.length "quote "
        Just ImportLevelSplice -> T.length "splice "
        Nothing -> 0
    packageWidth =
      case importDeclPackage importDecl of
        Just packageName -> T.length packageName + T.length "\"\" "
        Nothing -> 0

importMemberNameSpan :: Maybe SourceSpan -> Text -> Maybe SourceSpan
importMemberNameSpan span' name = flip importMemberNameSpanAt name <$> span'

importMemberNameSpanAt :: SourceSpan -> Text -> SourceSpan
importMemberNameSpanAt (SourceSpan sourceName startLine startCol endLine endCol startOffset endOffset) memberName =
  let width = T.length memberName
      (memberStartCol, memberStartOffset)
        | startLine == endLine =
            let col = max startCol (endCol - width - 1)
             in (col, startOffset + (col - startCol))
        | otherwise = (startCol, startOffset)
   in SourceSpan
        sourceName
        startLine
        memberStartCol
        endLine
        endCol
        memberStartOffset
        endOffset

shiftSpanStartNameSpanAt :: SourceSpan -> Int -> Text -> SourceSpan
shiftSpanStartNameSpanAt (SourceSpan sourceName startLine startCol _ _ startOffset endOffset) offset name =
  let shifted =
        SourceSpan
          sourceName
          startLine
          (startCol + offset)
          startLine
          (startCol + offset)
          (startOffset + offset)
          endOffset
   in spanStartNameSpanAt shifted name

-- | Narrow a declaration's span to the name that follows its keyword.
declKeywordNameSpan :: Text -> Maybe SourceSpan -> Text -> Maybe SourceSpan
declKeywordNameSpan keyword span' name =
  (\sp -> shiftSpanStartNameSpanAt sp (T.length keyword) name) <$> span'
