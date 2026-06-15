{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Aihc.Resolve.Span
  ( pushSpanFromAnn,
    effectiveResolutionSpan,
    sourceSpanFromAnns,
    peelDeclSpan,
    peelPatternSpan,
    peelGuardQualifierSpan,
    peelDataConSpan,
    peelImportItemSpan,
    rhsSpan,
    annotateUnhandledDecl,
    annotateUnhandledClassDeclItem,
    annotateUnhandledInstanceDeclItem,
    annotateUnhandledExpr,
    annotateUnhandledPattern,
    annotateUnhandledType,
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
    DataConDecl (..),
    Decl (..),
    Expr (..),
    GuardQualifier (..),
    ImportDecl (..),
    ImportItem (..),
    ImportLevel (..),
    InstanceDeclItem (..),
    Pattern (..),
    Rhs (..),
    SourceSpan (..),
    Type (..),
    fromAnnotation,
    mkAnnotation,
  )
import Aihc.Resolve.Types
import Data.Data (Data, showConstr, toConstr)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | Use a 'SourceSpan' stored in a dynamic annotation as the innermost ambient span.
pushSpanFromAnn :: SourceSpan -> Annotation -> SourceSpan
pushSpanFromAnn cur ann = fromMaybe cur (fromAnnotation @SourceSpan ann)

-- | Prefer a concrete span on a node; fall back to the ambient span from annotations.
effectiveResolutionSpan :: SourceSpan -> SourceSpan -> SourceSpan
effectiveResolutionSpan ambient localSpan =
  case localSpan of
    NoSourceSpan -> ambient
    _ -> localSpan

-- | Merge concrete source spans embedded in a list of annotations.
sourceSpanFromAnns :: [Annotation] -> SourceSpan
sourceSpanFromAnns anns =
  case mapMaybe (fromAnnotation @SourceSpan) anns of
    [] -> NoSourceSpan
    s : _ -> s

-- | Resolver-owned span tracking for nodes that now store source spans only in
-- annotations.
peelDeclSpan :: SourceSpan -> Decl -> (SourceSpan, Decl)
peelDeclSpan ambient (DeclAnn ann inner) = peelDeclSpan (pushSpanFromAnn ambient ann) inner
peelDeclSpan ambient decl = (ambient, decl)

peelPatternSpan :: SourceSpan -> Pattern -> SourceSpan
peelPatternSpan ambient (PAnn ann inner) = peelPatternSpan (pushSpanFromAnn ambient ann) inner
peelPatternSpan ambient _ = ambient

peelGuardQualifierSpan :: SourceSpan -> GuardQualifier -> SourceSpan
peelGuardQualifierSpan ambient (GuardAnn ann inner) = peelGuardQualifierSpan (pushSpanFromAnn ambient ann) inner
peelGuardQualifierSpan ambient _ = ambient

peelDataConSpan :: SourceSpan -> DataConDecl -> SourceSpan
peelDataConSpan ambient (DataConAnn ann inner) = peelDataConSpan (pushSpanFromAnn ambient ann) inner
peelDataConSpan ambient _ = ambient

peelImportItemSpan :: SourceSpan -> ImportItem -> SourceSpan
peelImportItemSpan ambient (ImportAnn ann inner) = peelImportItemSpan (pushSpanFromAnn ambient ann) inner
peelImportItemSpan ambient _ = ambient

rhsSpan :: Rhs body -> SourceSpan
rhsSpan rhs =
  case rhs of
    UnguardedRhs anns _ _ -> sourceSpanFromAnns anns
    GuardedRhss anns _ _ -> sourceSpanFromAnns anns

unhandledSyntaxAnnotation :: (Data a) => ResolutionNamespace -> SourceSpan -> a -> ResolutionAnnotation
unhandledSyntaxAnnotation namespace span' node =
  ResolutionAnnotation
    span'
    (T.pack (showConstr (toConstr node)))
    namespace
    (ResolvedError "unhandled syntax")

annotateUnhandledDecl :: SourceSpan -> Decl -> Decl
annotateUnhandledDecl span' decl =
  annotateDecl (unhandledSyntaxAnnotation ResolutionNamespaceTerm span' decl) decl

annotateUnhandledClassDeclItem :: SourceSpan -> ClassDeclItem -> ClassDeclItem
annotateUnhandledClassDeclItem span' item =
  ClassItemAnn (mkAnnotation (unhandledSyntaxAnnotation ResolutionNamespaceTerm span' item)) item

annotateUnhandledInstanceDeclItem :: SourceSpan -> InstanceDeclItem -> InstanceDeclItem
annotateUnhandledInstanceDeclItem span' item =
  InstanceItemAnn (mkAnnotation (unhandledSyntaxAnnotation ResolutionNamespaceTerm span' item)) item

annotateUnhandledExpr :: SourceSpan -> Expr -> Expr
annotateUnhandledExpr span' expr =
  annotateExpr (unhandledSyntaxAnnotation ResolutionNamespaceTerm span' expr) expr

annotateUnhandledPattern :: SourceSpan -> Pattern -> Pattern
annotateUnhandledPattern span' pat =
  annotatePattern (unhandledSyntaxAnnotation ResolutionNamespaceTerm span' pat) pat

annotateUnhandledType :: SourceSpan -> Type -> Type
annotateUnhandledType span' ty =
  annotateType (unhandledSyntaxAnnotation ResolutionNamespaceType span' ty) ty

spanStartNameSpan :: SourceSpan -> Text -> SourceSpan
spanStartNameSpan span' name =
  case span' of
    SourceSpan sourceName startLine startCol _ _ startOffset endOffset ->
      let width = T.length name
       in SourceSpan
            sourceName
            startLine
            startCol
            startLine
            (startCol + width)
            startOffset
            (min endOffset (startOffset + width))
    NoSourceSpan -> NoSourceSpan

annotateDecl :: ResolutionAnnotation -> Decl -> Decl
annotateDecl annotation =
  DeclAnn (mkAnnotation annotation) . DeclAnn (mkAnnotation (resolutionSpan annotation))

annotateExpr :: ResolutionAnnotation -> Expr -> Expr
annotateExpr annotation =
  EAnn (mkAnnotation annotation) . EAnn (mkAnnotation (resolutionSpan annotation))

annotatePattern :: ResolutionAnnotation -> Pattern -> Pattern
annotatePattern annotation =
  PAnn (mkAnnotation annotation) . PAnn (mkAnnotation (resolutionSpan annotation))

annotateType :: ResolutionAnnotation -> Type -> Type
annotateType annotation =
  TAnn (mkAnnotation annotation) . TAnn (mkAnnotation (resolutionSpan annotation))

annotateImport :: ResolutionAnnotation -> ImportDecl -> ImportDecl
annotateImport annotation importDecl =
  importDecl {importDeclAnns = mkAnnotation annotation : importDeclAnns importDecl}

importModuleNameSpan :: ImportDecl -> SourceSpan
importModuleNameSpan importDecl =
  shiftSpanStartNameSpan (sourceSpanFromAnns (importDeclAnns importDecl)) prefixWidth (importDeclModule importDecl)
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

importMemberNameSpan :: SourceSpan -> Text -> SourceSpan
importMemberNameSpan itemSpan memberName =
  case itemSpan of
    SourceSpan sourceName startLine startCol endLine endCol startOffset endOffset ->
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
    NoSourceSpan -> NoSourceSpan

shiftSpanStartNameSpan :: SourceSpan -> Int -> Text -> SourceSpan
shiftSpanStartNameSpan span' offset name =
  case span' of
    SourceSpan sourceName startLine startCol _ _ startOffset endOffset ->
      let shiftedStartOffset = startOffset + offset
          shifted =
            SourceSpan
              sourceName
              startLine
              (startCol + offset)
              startLine
              (startCol + offset)
              shiftedStartOffset
              endOffset
       in spanStartNameSpan shifted name
    NoSourceSpan -> NoSourceSpan

declKeywordNameSpan :: Text -> SourceSpan -> Text -> SourceSpan
declKeywordNameSpan keyword span' name =
  case span' of
    SourceSpan sourceName startLine startCol _ _ startOffset endOffset ->
      let keywordWidth = T.length keyword
          shifted =
            SourceSpan
              sourceName
              startLine
              (startCol + keywordWidth)
              startLine
              (startCol + keywordWidth)
              (startOffset + keywordWidth)
              endOffset
       in spanStartNameSpan shifted name
    NoSourceSpan -> NoSourceSpan
