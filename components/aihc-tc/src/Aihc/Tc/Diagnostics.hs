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
import Aihc.Resolve.Traverse (Walk (..), collectAnnotations, idWalk, walk)
import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..), TcSeverity (..))
import Control.Applicative ((<|>))
import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, maybeToList)
import Data.Text (Text)

-- | Attach the diagnostics of a type-checked SCC to its modules.
--
-- Each module gets its diagnostics in their original order. A run of
-- located diagnostics is attached in one walk of the module, because a
-- walk for each diagnostic makes a large module with many diagnostics
-- quadratic.
attachSccDiagnostics :: [TcDiagnostic] -> [Module] -> [Module]
attachSccDiagnostics diagnostics modules =
  zipWith annotateInOrder [0 :: Int ..] modules
  where
    moduleNames = map moduleSourceNames modules
    routed = concatMap route diagnostics
    route diagnostic =
      case diagLoc diagnostic of
        Nothing -> [(0, diagnostic)]
        Just span' ->
          let sourceName = sourceSpanSourceName span'
           in case [index | (index, names) <- zip [0 ..] moduleNames, sourceName `elem` names] of
                [] -> [(0, internalAbortDiagnostic "SCC diagnostic source did not match a module")]
                indices -> [(index, diagnostic) | index <- indices]
    annotateInOrder index m =
      foldl (flip annotateModuleDiagnostics) m (diagnosticRuns [diagnostic | (target, diagnostic) <- routed, target == index])

-- | Split diagnostics into runs that 'annotateModuleDiagnostics' can attach
-- together without a change of order. It attaches located diagnostics before
-- unlocated ones, so each unlocated diagnostic is a run of its own.
diagnosticRuns :: [TcDiagnostic] -> [[TcDiagnostic]]
diagnosticRuns =
  List.groupBy (\left right -> isJust (diagLoc left) && isJust (diagLoc right))

moduleSourceNames :: Module -> [Text]
moduleSourceNames modu =
  map sourceSpanSourceName (maybeToList (spanFromAnnotations (moduleAnns modu)))

annotateModuleDiagnostics :: [TcDiagnostic] -> Module -> Module
annotateModuleDiagnostics diagnostics m =
  let (located, unlocated) = partitionDiagnostics diagnostics
      moduleWithLocated = attachLocatedDiagnostics m located
   in moduleWithLocated {moduleAnns = moduleAnns moduleWithLocated <> map mkAnnotation unlocated}

partitionDiagnostics :: [TcDiagnostic] -> ([(SourceSpan, TcDiagnostic)], [TcDiagnostic])
partitionDiagnostics =
  foldr partitionOne ([], [])
  where
    partitionOne diagnostic (located, unlocated) =
      case diagLoc diagnostic of
        Just sp -> ((sp, diagnostic) : located, unlocated)
        Nothing -> (located, diagnostic : unlocated)

-- | Attach located diagnostics in one walk of the module. The diagnostics
-- of one span go to the same node, in their original order.
attachLocatedDiagnostics :: Module -> [(SourceSpan, TcDiagnostic)] -> Module
attachLocatedDiagnostics m [] = m
attachLocatedDiagnostics m located =
  case runState (attachDiagnosticsAt m) pending of
    (m', remaining) ->
      case List.find (`Map.member` remaining) (map fst located) of
        Nothing -> m'
        Just sp ->
          error ("type checker diagnostic has no matching syntax node for source span: " <> show sp)
  where
    pending = Map.fromListWith (flip (<>)) [(sp, [diagnostic]) | (sp, diagnostic) <- located]

-- Attach bottom-up so an exact child span wins over an exact parent span.
-- Located diagnostics must never guess: if no exact syntax span exists, abort.
attachDiagnosticsAt :: Module -> State (Map SourceSpan [TcDiagnostic]) Module
attachDiagnosticsAt =
  walk
    idWalk
      { walkAnnotationList = attachAt spanFromAnnotations (<>),
        walkExpr = attachWrapped peelExprAnnOnce EAnn,
        walkPattern = attachWrapped peelPatternAnnOnce PAnn,
        walkType = attachWrapped peelTypeAnnOnce TAnn,
        walkDecl = attachWrapped peelDeclAnnOnce DeclAnn,
        walkDataConDecl = attachWrapped peelDataConAnnOnce DataConAnn,
        walkLiteral = attachWrapped peelLiteralAnnOnce LitAnn,
        walkGuardQualifier = attachWrapped peelGuardAnnOnce GuardAnn,
        walkDoStmt = attachWrapped peelDoAnnOnce DoAnn,
        walkCompStmt = attachWrapped peelCompAnnOnce CompAnn,
        walkArithSeq = attachWrapped peelArithSeqAnnOnce ArithSeqAnn,
        walkClassDeclItem = attachWrapped peelClassItemAnnOnce ClassItemAnn,
        walkInstanceDeclItem = attachWrapped peelInstanceItemAnnOnce InstanceItemAnn,
        walkCmd = attachWrapped peelCmdAnnOnce CmdAnn,
        walkExportSpec = attachWrapped peelExportAnnOnce ExportAnn,
        walkImportItem = attachWrapped peelImportAnnOnce ImportAnn
      }
  where
    -- A node that wraps its annotations one by one. The first diagnostic
    -- becomes the outermost wrapper, as when each diagnostic is attached
    -- in a walk of its own.
    attachWrapped :: (node -> Maybe (Annotation, node)) -> (Annotation -> node -> node) -> (node -> State (Map SourceSpan [TcDiagnostic]) node) -> node -> State (Map SourceSpan [TcDiagnostic]) node
    attachWrapped peel wrap = attachAt (wrappedSpan peel) (flip (foldr wrap))

    -- Walk the children first, then attach the diagnostics of the exact
    -- span of the node, if any are pending.
    attachAt :: (node -> Maybe SourceSpan) -> ([Annotation] -> node -> node) -> (node -> State (Map SourceSpan [TcDiagnostic]) node) -> node -> State (Map SourceSpan [TcDiagnostic]) node
    attachAt spanOf attach children node = do
      node' <- children node
      pending <- get
      if Map.null pending
        then pure node'
        else case spanOf node' >>= \sp -> (,) sp <$> Map.lookup sp pending of
          Nothing -> pure node'
          Just (sp, diagnostics) -> do
            put (Map.delete sp pending)
            pure (attach (map mkAnnotation diagnostics) node')

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
