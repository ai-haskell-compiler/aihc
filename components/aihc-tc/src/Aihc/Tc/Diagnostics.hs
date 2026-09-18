{-# LANGUAGE ScopedTypeVariables #-}

-- | Attach type diagnostics to their source nodes.
module Aihc.Tc.Diagnostics
  ( annotateModuleDiagnostics,
    attachSccDiagnostics,
    collectTcDiagnostics,
    internalAbortDiagnostic,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    ArithSeq (..),
    ClassDeclItem (..),
    Cmd (..),
    CompStmt (..),
    DataConDecl (..),
    Decl (..),
    DoStmt (..),
    ExportSpec (..),
    Expr (..),
    GuardQualifier (..),
    ImportItem (..),
    InstanceDeclItem (..),
    Literal (..),
    Module (..),
    Pattern (..),
    SourceSpan,
    Type (..),
    fromAnnotation,
    mkAnnotation,
    sourceSpanSourceName,
  )
import Aihc.Resolve.Generic (everywhereM)
import Aihc.Resolve.Traverse (collectAnnotations)
import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..), TcSeverity (..))
import Control.Applicative ((<|>))
import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Data.Data (Data)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Typeable (cast)

attachSccDiagnostics :: [TcDiagnostic] -> [Module] -> [Module]
attachSccDiagnostics diagnostics modules = foldl attachOne modules diagnostics
  where
    attachOne [] _ = []
    attachOne current@(first : rest) diagnostic =
      case diagLoc diagnostic of
        Nothing -> annotateModuleDiagnostics [diagnostic] first : rest
        Just span' ->
          let sourceName = sourceSpanSourceName span'
              matches m = sourceName `elem` moduleSourceNames m
           in if any matches current
                then map (\m -> if matches m then annotateModuleDiagnostics [diagnostic] m else m) current
                else annotateModuleDiagnostics [internalAbortDiagnostic "SCC diagnostic source did not match a module"] first : rest

moduleSourceNames :: Module -> [Text]
moduleSourceNames modu =
  map sourceSpanSourceName (maybeToList (spanFromAnnotations (moduleAnns modu)))

annotateModuleDiagnostics :: [TcDiagnostic] -> Module -> Module
annotateModuleDiagnostics diagnostics m =
  let (located, unlocated) = partitionDiagnostics diagnostics
      moduleWithLocated = foldl attachLocatedDiagnostic m located
   in moduleWithLocated {moduleAnns = moduleAnns moduleWithLocated <> map mkAnnotation unlocated}

partitionDiagnostics :: [TcDiagnostic] -> ([(SourceSpan, TcDiagnostic)], [TcDiagnostic])
partitionDiagnostics =
  foldr partitionOne ([], [])
  where
    partitionOne diagnostic (located, unlocated) =
      case diagLoc diagnostic of
        Just sp -> ((sp, diagnostic) : located, unlocated)
        Nothing -> (located, diagnostic : unlocated)

attachLocatedDiagnostic :: Module -> (SourceSpan, TcDiagnostic) -> Module
attachLocatedDiagnostic m (sp, diagnostic) =
  case runState (attachDiagnosticAt sp diagnostic m) False of
    (m', True) -> m'
    (_, False) ->
      error ("type checker diagnostic has no matching syntax node for source span: " <> show sp)

-- Attach bottom-up so an exact child span wins over an exact parent span.
-- Located diagnostics must never guess: if no exact syntax span exists, abort.
attachDiagnosticAt :: (Data a) => SourceSpan -> TcDiagnostic -> a -> State Bool a
attachDiagnosticAt sp diagnostic =
  everywhereM attachHere
  where
    attachHere :: forall node. (Data node) => node -> State Bool node
    attachHere value = do
      alreadyAttached <- get
      if alreadyAttached
        then pure value
        else case attachDiagnosticHere sp diagnostic value of
          Just value' -> do
            put True
            pure value'
          Nothing ->
            pure value

attachDiagnosticHere :: forall a. (Data a) => SourceSpan -> TcDiagnostic -> a -> Maybe a
attachDiagnosticHere sp diagnostic value =
  attachAnnotationList
    <|> attachExpr
    <|> attachPattern
    <|> attachType
    <|> attachDecl
    <|> attachDataConDecl
    <|> attachLiteral
    <|> attachGuardQualifier
    <|> attachDoStmtExpr
    <|> attachDoStmtCmd
    <|> attachCompStmt
    <|> attachArithSeq
    <|> attachClassDeclItem
    <|> attachInstanceDeclItem
    <|> attachCmd
    <|> attachExportSpec
    <|> attachImportItem
  where
    diagnosticAnn = mkAnnotation diagnostic
    atExactSpan span' wrap =
      if span' == Just sp
        then cast wrap
        else Nothing
    attachTyped :: forall node. (Data node) => (node -> Maybe node) -> Maybe a
    attachTyped f = do
      node <- cast value
      node' <- f node
      cast node'
    attachAnnotationList =
      attachTyped $ \(anns :: [Annotation]) ->
        atExactSpan (spanFromAnnotations anns) (anns <> [diagnosticAnn])
    attachExpr =
      attachTyped $ \(expr :: Expr) ->
        atExactSpan (wrappedSpan peelExprAnnOnce expr) (EAnn diagnosticAnn expr)
    attachPattern =
      attachTyped $ \(pat :: Pattern) ->
        atExactSpan (wrappedSpan peelPatternAnnOnce pat) (PAnn diagnosticAnn pat)
    attachType =
      attachTyped $ \(ty :: Type) ->
        atExactSpan (wrappedSpan peelTypeAnnOnce ty) (TAnn diagnosticAnn ty)
    attachDecl =
      attachTyped $ \(decl :: Decl) ->
        atExactSpan (wrappedSpan peelDeclAnnOnce decl) (DeclAnn diagnosticAnn decl)
    attachDataConDecl =
      attachTyped $ \(decl :: DataConDecl) ->
        atExactSpan (wrappedSpan peelDataConAnnOnce decl) (DataConAnn diagnosticAnn decl)
    attachLiteral =
      attachTyped $ \(lit :: Literal) ->
        atExactSpan (wrappedSpan peelLiteralAnnOnce lit) (LitAnn diagnosticAnn lit)
    attachGuardQualifier =
      attachTyped $ \(qualifier :: GuardQualifier) ->
        atExactSpan (wrappedSpan peelGuardAnnOnce qualifier) (GuardAnn diagnosticAnn qualifier)
    attachDoStmtExpr =
      attachTyped $ \(stmt :: DoStmt Expr) ->
        atExactSpan (wrappedSpan peelDoAnnOnce stmt) (DoAnn diagnosticAnn stmt)
    attachDoStmtCmd =
      attachTyped $ \(stmt :: DoStmt Cmd) ->
        atExactSpan (wrappedSpan peelDoAnnOnce stmt) (DoAnn diagnosticAnn stmt)
    attachCompStmt =
      attachTyped $ \(stmt :: CompStmt) ->
        atExactSpan (wrappedSpan peelCompAnnOnce stmt) (CompAnn diagnosticAnn stmt)
    attachArithSeq =
      attachTyped $ \(seq' :: ArithSeq) ->
        atExactSpan (wrappedSpan peelArithSeqAnnOnce seq') (ArithSeqAnn diagnosticAnn seq')
    attachClassDeclItem =
      attachTyped $ \(item :: ClassDeclItem) ->
        atExactSpan (wrappedSpan peelClassItemAnnOnce item) (ClassItemAnn diagnosticAnn item)
    attachInstanceDeclItem =
      attachTyped $ \(item :: InstanceDeclItem) ->
        atExactSpan (wrappedSpan peelInstanceItemAnnOnce item) (InstanceItemAnn diagnosticAnn item)
    attachCmd =
      attachTyped $ \(cmd :: Cmd) ->
        atExactSpan (wrappedSpan peelCmdAnnOnce cmd) (CmdAnn diagnosticAnn cmd)
    attachExportSpec =
      attachTyped $ \(spec :: ExportSpec) ->
        atExactSpan (wrappedSpan peelExportAnnOnce spec) (ExportAnn diagnosticAnn spec)
    attachImportItem =
      attachTyped $ \(item :: ImportItem) ->
        atExactSpan (wrappedSpan peelImportAnnOnce item) (ImportAnn diagnosticAnn item)

wrappedSpan :: (node -> Maybe (Annotation, node)) -> node -> Maybe SourceSpan
wrappedSpan peel =
  spanFromAnnotations . fst . peelLeading peel

peelLeading :: (node -> Maybe (Annotation, node)) -> node -> ([Annotation], node)
peelLeading peel =
  go []
  where
    go anns node =
      case peel node of
        Just (ann, inner) -> go (ann : anns) inner
        Nothing -> (reverse anns, node)

peelExprAnnOnce :: Expr -> Maybe (Annotation, Expr)
peelExprAnnOnce (EAnn ann inner) = Just (ann, inner)
peelExprAnnOnce _ = Nothing

peelPatternAnnOnce :: Pattern -> Maybe (Annotation, Pattern)
peelPatternAnnOnce (PAnn ann inner) = Just (ann, inner)
peelPatternAnnOnce _ = Nothing

peelTypeAnnOnce :: Type -> Maybe (Annotation, Type)
peelTypeAnnOnce (TAnn ann inner) = Just (ann, inner)
peelTypeAnnOnce _ = Nothing

peelDeclAnnOnce :: Decl -> Maybe (Annotation, Decl)
peelDeclAnnOnce (DeclAnn ann inner) = Just (ann, inner)
peelDeclAnnOnce _ = Nothing

peelDataConAnnOnce :: DataConDecl -> Maybe (Annotation, DataConDecl)
peelDataConAnnOnce (DataConAnn ann inner) = Just (ann, inner)
peelDataConAnnOnce _ = Nothing

peelLiteralAnnOnce :: Literal -> Maybe (Annotation, Literal)
peelLiteralAnnOnce (LitAnn ann inner) = Just (ann, inner)
peelLiteralAnnOnce _ = Nothing

peelGuardAnnOnce :: GuardQualifier -> Maybe (Annotation, GuardQualifier)
peelGuardAnnOnce (GuardAnn ann inner) = Just (ann, inner)
peelGuardAnnOnce _ = Nothing

peelDoAnnOnce :: DoStmt body -> Maybe (Annotation, DoStmt body)
peelDoAnnOnce (DoAnn ann inner) = Just (ann, inner)
peelDoAnnOnce _ = Nothing

peelCompAnnOnce :: CompStmt -> Maybe (Annotation, CompStmt)
peelCompAnnOnce (CompAnn ann inner) = Just (ann, inner)
peelCompAnnOnce _ = Nothing

peelArithSeqAnnOnce :: ArithSeq -> Maybe (Annotation, ArithSeq)
peelArithSeqAnnOnce (ArithSeqAnn ann inner) = Just (ann, inner)
peelArithSeqAnnOnce _ = Nothing

peelClassItemAnnOnce :: ClassDeclItem -> Maybe (Annotation, ClassDeclItem)
peelClassItemAnnOnce (ClassItemAnn ann inner) = Just (ann, inner)
peelClassItemAnnOnce _ = Nothing

peelInstanceItemAnnOnce :: InstanceDeclItem -> Maybe (Annotation, InstanceDeclItem)
peelInstanceItemAnnOnce (InstanceItemAnn ann inner) = Just (ann, inner)
peelInstanceItemAnnOnce _ = Nothing

peelCmdAnnOnce :: Cmd -> Maybe (Annotation, Cmd)
peelCmdAnnOnce (CmdAnn ann inner) = Just (ann, inner)
peelCmdAnnOnce _ = Nothing

peelExportAnnOnce :: ExportSpec -> Maybe (Annotation, ExportSpec)
peelExportAnnOnce (ExportAnn ann inner) = Just (ann, inner)
peelExportAnnOnce _ = Nothing

peelImportAnnOnce :: ImportItem -> Maybe (Annotation, ImportItem)
peelImportAnnOnce (ImportAnn ann inner) = Just (ann, inner)
peelImportAnnOnce _ = Nothing

spanFromAnnotations :: [Annotation] -> Maybe SourceSpan
spanFromAnnotations =
  foldr ((<|>) . spanFromAnnotation) Nothing

spanFromAnnotation :: Annotation -> Maybe SourceSpan
spanFromAnnotation = fromAnnotation

collectTcDiagnostics :: Module -> [TcDiagnostic]
collectTcDiagnostics = collectAnnotations fromAnnotation

internalAbortDiagnostic :: String -> TcDiagnostic
internalAbortDiagnostic msg =
  TcDiagnostic
    { diagLoc = Nothing,
      diagSeverity = TcError,
      diagKind = OtherError ("internal type checker abort: " <> msg)
    }
