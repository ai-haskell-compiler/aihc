{-# LANGUAGE ScopedTypeVariables #-}

-- | Give type diagnostics to the modules of the final tree, and attach
-- them to their source nodes in the bridge to the parser tree.
module Aihc.Tc.Diagnostics
  ( annotateModuleDiagnostics,
    attachSccDiagnostics,
    checkedParserModule,
    internalAbortDiagnostic,
  )
where

import Aihc.Parser.Syntax qualified as P
import Aihc.Resolve.Syntax
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
import Aihc.Resolve.Syntax.ToParser (toParserModule)
import Aihc.Resolve.Syntax.Traverse (Walk (..), idWalk, walk)
import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..), TcSeverity (..))
import Aihc.Tc.Syntax qualified as Final
import Aihc.Tc.Syntax.ToResolved (ToResolved (..))
import Aihc.Tc.Syntax.Traverse (collectAnnotations)
import Control.Applicative ((<|>))
import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)

-- | Attach the diagnostics of a type-checked SCC to its modules.
--
-- Each module gets its diagnostics in their original order. A run of
-- located diagnostics is attached in one walk of the module, because a
-- walk for each diagnostic makes a large module with many diagnostics
-- quadratic.
attachSccDiagnostics :: [TcDiagnostic] -> [Final.Module] -> [Final.Module]
attachSccDiagnostics diagnostics modules =
  zipWith annotateInOrder [0 :: Int ..] modules
  where
    moduleNames = map moduleSourceNames modules
    -- A module can contain syntax from other files, for example from a CPP
    -- @#include@. The spans of that syntax have the source name of the
    -- included file. Thus a diagnostic that matches no module source name
    -- goes to the module that contains its exact span. The span sets are
    -- lazy, so they are made only for such a diagnostic.
    moduleSpans = map (Set.fromList . collectAnnotations (fromAnnotation @SourceSpan)) modules
    routed = concatMap route diagnostics
    route diagnostic =
      case diagLoc diagnostic of
        Nothing -> [(0, diagnostic)]
        Just span' ->
          let sourceName = sourceSpanSourceName span'
           in case [index | (index, names) <- zip [0 ..] moduleNames, sourceName `elem` names] of
                [] ->
                  case [index | (index, spans) <- zip [0 ..] moduleSpans, span' `Set.member` spans] of
                    index : _ -> [(index, diagnostic)]
                    [] -> [(0, internalAbortDiagnostic "SCC diagnostic source did not match a module")]
                indices -> [(index, diagnostic) | index <- indices]
    annotateInOrder index m =
      foldl (flip annotateModuleDiagnostics) m (diagnosticRuns [diagnostic | (target, diagnostic) <- routed, target == index])

-- | Split diagnostics into runs that 'attachModuleDiagnostics' can attach
-- together without a change of order. It attaches located diagnostics before
-- unlocated ones, so each unlocated diagnostic is a run of its own.
diagnosticRuns :: [TcDiagnostic] -> [[TcDiagnostic]]
diagnosticRuns =
  List.groupBy (\left right -> isJust (diagLoc left) && isJust (diagLoc right))

moduleSourceNames :: Final.Module -> [Text]
moduleSourceNames modu =
  map sourceSpanSourceName (maybeToList (spanFromAnnotations (Final.moduleAnns modu)))

-- | Give a module one group of diagnostics. The bridge to the parser tree
-- attaches the group in one walk.
annotateModuleDiagnostics :: [TcDiagnostic] -> Final.Module -> Final.Module
annotateModuleDiagnostics diagnostics m =
  m {Final.moduleTcDiagnostics = Final.moduleTcDiagnostics m <> [diagnostics]}

-- | The parser tree of a checked module. Each diagnostic of the module is
-- an annotation on the node of its exact span, or on the module when it
-- has no span.
checkedParserModule :: Final.Module -> P.Module
checkedParserModule m =
  toParserModule (foldl (flip attachModuleDiagnostics) (toResolved m) (Final.moduleTcDiagnostics m))

attachModuleDiagnostics :: [TcDiagnostic] -> Module -> Module
attachModuleDiagnostics diagnostics m =
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
    attachWrapped :: (node -> Maybe ([Annotation], node)) -> (Annotation -> node -> node) -> (node -> State (Map SourceSpan [TcDiagnostic]) node) -> node -> State (Map SourceSpan [TcDiagnostic]) node
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

-- | The span of the leading wrappers of a node. A resolution wrapper of
-- the resolved syntax tree has no annotation, and the peel goes through it.
wrappedSpan :: (node -> Maybe ([Annotation], node)) -> node -> Maybe SourceSpan
wrappedSpan peel =
  spanFromAnnotations . fst . peelLeading peel

peelLeading :: (node -> Maybe ([Annotation], node)) -> node -> ([Annotation], node)
peelLeading peel =
  go []
  where
    go anns node =
      case peel node of
        Just (nodeAnns, inner) -> go (reverse nodeAnns <> anns) inner
        Nothing -> (reverse anns, node)

peelExprAnnOnce :: Expr -> Maybe ([Annotation], Expr)
peelExprAnnOnce (EAnn ann inner) = Just ([ann], inner)
peelExprAnnOnce (EResolved _ inner) = Just ([], inner)
peelExprAnnOnce _ = Nothing

peelPatternAnnOnce :: Pattern -> Maybe ([Annotation], Pattern)
peelPatternAnnOnce (PAnn ann inner) = Just ([ann], inner)
peelPatternAnnOnce (PResolved _ inner) = Just ([], inner)
peelPatternAnnOnce _ = Nothing

peelTypeAnnOnce :: Type -> Maybe ([Annotation], Type)
peelTypeAnnOnce (TAnn ann inner) = Just ([ann], inner)
peelTypeAnnOnce (TResolved _ inner) = Just ([], inner)
peelTypeAnnOnce _ = Nothing

peelDeclAnnOnce :: Decl -> Maybe ([Annotation], Decl)
peelDeclAnnOnce (DeclAnn ann inner) = Just ([ann], inner)
peelDeclAnnOnce (DeclError _ inner) = Just ([], inner)
peelDeclAnnOnce _ = Nothing

peelDataConAnnOnce :: DataConDecl -> Maybe ([Annotation], DataConDecl)
peelDataConAnnOnce (DataConAnn ann inner) = Just ([ann], inner)
peelDataConAnnOnce (DataConResolved _ inner) = Just ([], inner)
peelDataConAnnOnce _ = Nothing

peelLiteralAnnOnce :: Literal -> Maybe ([Annotation], Literal)
peelLiteralAnnOnce (LitAnn ann inner) = Just ([ann], inner)
peelLiteralAnnOnce _ = Nothing

peelGuardAnnOnce :: GuardQualifier -> Maybe ([Annotation], GuardQualifier)
peelGuardAnnOnce (GuardAnn ann inner) = Just ([ann], inner)
peelGuardAnnOnce _ = Nothing

peelDoAnnOnce :: DoStmt body -> Maybe ([Annotation], DoStmt body)
peelDoAnnOnce (DoAnn ann inner) = Just ([ann], inner)
peelDoAnnOnce (DoResolved _ inner) = Just ([], inner)
peelDoAnnOnce (DoRecKnot _ inner) = Just ([], inner)
peelDoAnnOnce _ = Nothing

peelCompAnnOnce :: CompStmt -> Maybe ([Annotation], CompStmt)
peelCompAnnOnce (CompAnn ann inner) = Just ([ann], inner)
peelCompAnnOnce _ = Nothing

peelArithSeqAnnOnce :: ArithSeq -> Maybe ([Annotation], ArithSeq)
peelArithSeqAnnOnce (ArithSeqAnn ann inner) = Just ([ann], inner)
peelArithSeqAnnOnce (ArithSeqResolved _ inner) = Just ([], inner)
peelArithSeqAnnOnce _ = Nothing

peelClassItemAnnOnce :: ClassDeclItem -> Maybe ([Annotation], ClassDeclItem)
peelClassItemAnnOnce (ClassItemAnn ann inner) = Just ([ann], inner)
peelClassItemAnnOnce (ClassItemError _ inner) = Just ([], inner)
peelClassItemAnnOnce _ = Nothing

peelInstanceItemAnnOnce :: InstanceDeclItem -> Maybe ([Annotation], InstanceDeclItem)
peelInstanceItemAnnOnce (InstanceItemAnn ann inner) = Just ([ann], inner)
peelInstanceItemAnnOnce (InstanceItemError _ inner) = Just ([], inner)
peelInstanceItemAnnOnce _ = Nothing

peelCmdAnnOnce :: Cmd -> Maybe ([Annotation], Cmd)
peelCmdAnnOnce (CmdAnn ann inner) = Just ([ann], inner)
peelCmdAnnOnce _ = Nothing

peelExportAnnOnce :: ExportSpec -> Maybe ([Annotation], ExportSpec)
peelExportAnnOnce (ExportAnn ann inner) = Just ([ann], inner)
peelExportAnnOnce _ = Nothing

peelImportAnnOnce :: ImportItem -> Maybe ([Annotation], ImportItem)
peelImportAnnOnce (ImportAnn ann inner) = Just ([ann], inner)
peelImportAnnOnce (ImportItemError _ inner) = Just ([], inner)
peelImportAnnOnce _ = Nothing

spanFromAnnotations :: [Annotation] -> Maybe SourceSpan
spanFromAnnotations =
  foldr ((<|>) . spanFromAnnotation) Nothing

spanFromAnnotation :: Annotation -> Maybe SourceSpan
spanFromAnnotation = fromAnnotation

internalAbortDiagnostic :: String -> TcDiagnostic
internalAbortDiagnostic msg =
  TcDiagnostic
    { diagLoc = Nothing,
      diagSeverity = TcError,
      diagKind = OtherError ("internal type checker abort: " <> msg)
    }
