{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Give type diagnostics to the modules of the final tree, and put them
-- in the order of the syntax nodes that they belong to.
module Aihc.Tc.Diagnostics
  ( annotateModuleDiagnostics,
    attachSccDiagnostics,
    internalAbortDiagnostic,
    moduleDiagnostics,
  )
where

import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..), TcSeverity (..))
import Aihc.Tc.Syntax
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
    annotationSpan,
    sourceSpanSourceName,
  )
import Aihc.Tc.Syntax.Pending qualified as Pending
import Aihc.Tc.Syntax.Pending.Traverse qualified as PendingWalk
import Aihc.Tc.Syntax.Traverse (Walk (..), collectAnnotations, idWalk, walk)
import Control.Applicative ((<|>))
import Control.Monad.Trans.State.Strict (State, execState, get, modify', put)
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
attachSccDiagnostics :: [TcDiagnostic] -> [Module] -> [Module]
attachSccDiagnostics diagnostics modules =
  zipWith annotateInOrder [0 :: Int ..] modules
  where
    moduleNames = map moduleSourceNames modules
    -- A module can contain syntax from other files, for example from a CPP
    -- @#include@. The spans of that syntax have the source name of the
    -- included file. Thus a diagnostic that matches no module source name
    -- goes to the module that contains its exact span. The span sets are
    -- lazy, so they are made only for such a diagnostic.
    moduleSpans = map (Set.fromList . collectAnnotations annotationSpan) modules
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

moduleSourceNames :: Module -> [Text]
moduleSourceNames modu =
  map sourceSpanSourceName (maybeToList (spanFromAnnotations (moduleAnns modu)))

-- | Give a module one group of diagnostics.
annotateModuleDiagnostics :: [TcDiagnostic] -> Module -> Module
annotateModuleDiagnostics diagnostics m =
  m {moduleTcDiagnostics = moduleTcDiagnostics m <> [diagnostics]}

-- | The diagnostics of a module, in the order of the syntax nodes that
-- they belong to.
--
-- A located diagnostic belongs to the innermost node that has its exact
-- span. When a child and its parent have the same span, the child thus
-- wins. A located diagnostic must never guess: if no node has its exact
-- span, the type checker stops. An unlocated diagnostic belongs to the
-- annotation list of the module, after the located diagnostics of that
-- list.
--
-- The nodes come in source order, a parent before its children. The
-- diagnostics of one node come in the order of their groups, and in their
-- original order in each group.
moduleDiagnostics :: Module -> [TcDiagnostic]
moduleDiagnostics m =
  map snd (List.sortOn fst (concatMap groupEntries (moduleTcDiagnostics m)))
  where
    groupEntries diagnostics =
      let (located, unlocated) = partitionDiagnostics diagnostics
       in locatedEntries m located <> map (moduleAnnotationsNode,) unlocated

-- | The node number of the annotation list of a module. The walk visits
-- that list first.
moduleAnnotationsNode :: Int
moduleAnnotationsNode = 0

partitionDiagnostics :: [TcDiagnostic] -> ([(SourceSpan, TcDiagnostic)], [TcDiagnostic])
partitionDiagnostics =
  foldr partitionOne ([], [])
  where
    partitionOne diagnostic (located, unlocated) =
      case diagLoc diagnostic of
        Just sp -> ((sp, diagnostic) : located, unlocated)
        Nothing -> (located, diagnostic : unlocated)

-- | The state of the walk that finds the node of each located diagnostic.
data Placement = Placement
  { -- | The number of the next node, in source order.
    placementNext :: !Int,
    -- | The diagnostics that have no node yet, by their span.
    placementPending :: !(Map SourceSpan [TcDiagnostic]),
    -- | The node number of each placed diagnostic, last placement first.
    placementPlaced :: ![(Int, TcDiagnostic)]
  }

-- | Find the node of each located diagnostic in one walk of the module.
-- The diagnostics of one span go to the same node, in their original
-- order.
locatedEntries :: Module -> [(SourceSpan, TcDiagnostic)] -> [(Int, TcDiagnostic)]
locatedEntries _ [] = []
locatedEntries m located =
  case execState (walk placementWalk m) (Placement 0 pending []) of
    placement ->
      case List.find (`Map.member` placementPending placement) (map fst located) of
        Nothing -> reverse (placementPlaced placement)
        Just sp ->
          error ("type checker diagnostic has no matching syntax node for source span: " <> show sp)
  where
    pending = Map.fromListWith (flip (<>)) [(sp, [diagnostic]) | (sp, diagnostic) <- located]

-- Give each node its number before the walk of its children. Place the
-- diagnostics after the walk of the children, so an exact child span wins
-- over an exact parent span.
placementWalk :: Walk (State Placement)
placementWalk =
  idWalk
    { walkAnnotationList = placeAt spanFromAnnotations,
      walkExpr = placeAt (wrappedSpan peelExprOnce),
      walkPattern = placeAt (wrappedSpan peelPatternOnce),
      walkType = placeAt (wrappedSpan peelTypeOnce),
      walkDecl = placeAt (wrappedSpan peelDeclOnce),
      walkDataConDecl = placeAt (wrappedSpan peelDataConOnce),
      walkLiteral = placeAt (wrappedSpan peelLiteralOnce),
      walkGuardQualifier = placeAt (wrappedSpan peelGuardOnce),
      walkDoStmt = placeAt (wrappedSpan peelDoOnce),
      walkCompStmt = placeAt (wrappedSpan peelCompOnce),
      walkArithSeq = placeAt (wrappedSpan peelArithSeqOnce),
      walkClassDeclItem = placeAt (wrappedSpan peelClassItemOnce),
      walkInstanceDeclItem = placeAt (wrappedSpan peelInstanceItemOnce),
      walkCmd = placeAt (wrappedSpan peelCmdOnce),
      walkExportSpec = placeAt (wrappedSpan peelExportOnce),
      walkImportItem = placeAt (wrappedSpan peelImportOnce),
      walkPending = pendingPlacementWalk
    }

-- | The placement walk of a part that the type checker did not check.
pendingPlacementWalk :: PendingWalk.Walk (State Placement)
pendingPlacementWalk =
  PendingWalk.idWalk
    { PendingWalk.walkExpr = placeAt (wrappedSpan peelPendingExprOnce),
      PendingWalk.walkPattern = placeAt (wrappedSpan peelPendingPatternOnce),
      PendingWalk.walkType = placeAt (wrappedSpan peelPendingTypeOnce),
      PendingWalk.walkDecl = placeAt (wrappedSpan peelPendingDeclOnce),
      PendingWalk.walkDataConDecl = placeAt (wrappedSpan peelPendingDataConOnce),
      PendingWalk.walkLiteral = placeAt (wrappedSpan peelPendingLiteralOnce),
      PendingWalk.walkGuardQualifier = placeAt (wrappedSpan peelPendingGuardOnce),
      PendingWalk.walkDoStmt = placeAt (wrappedSpan peelPendingDoOnce),
      PendingWalk.walkCompStmt = placeAt (wrappedSpan peelPendingCompOnce),
      PendingWalk.walkArithSeq = placeAt (wrappedSpan peelPendingArithSeqOnce),
      PendingWalk.walkClassDeclItem = placeAt (wrappedSpan peelPendingClassItemOnce),
      PendingWalk.walkInstanceDeclItem = placeAt (wrappedSpan peelPendingInstanceItemOnce),
      PendingWalk.walkCmd = placeAt (wrappedSpan peelPendingCmdOnce)
    }

-- | Give a node its number before the walk of its children. Place the
-- diagnostics of its span after the walk of the children.
placeAt :: (node -> Maybe SourceSpan) -> (node -> State Placement node) -> node -> State Placement node
placeAt spanOf children node = do
  placement <- get
  let index = placementNext placement
  put placement {placementNext = index + 1}
  node' <- children node
  modify' (place index (spanOf node))
  pure node'

-- | Place the diagnostics of a span at a node.
place :: Int -> Maybe SourceSpan -> Placement -> Placement
place index span' placement
  | Map.null (placementPending placement) = placement
  | otherwise =
      case span' >>= \sp -> (,) sp <$> Map.lookup sp (placementPending placement) of
        Nothing -> placement
        Just (sp, diagnostics) ->
          placement
            { placementPending = Map.delete sp (placementPending placement),
              placementPlaced = reverse (map (index,) diagnostics) <> placementPlaced placement
            }

-- | The span of the leading wrappers of a node. A wrapper that holds a
-- resolution, an error, or type checker information has no span, and the
-- peel goes through it.
wrappedSpan :: (node -> Maybe ([Annotation], node)) -> node -> Maybe SourceSpan
wrappedSpan peel =
  go
  where
    go node =
      case peel node of
        Just (anns, inner) -> spanFromAnnotations anns <|> go inner
        Nothing -> Nothing

peelExprOnce :: Expr -> Maybe ([Annotation], Expr)
peelExprOnce (EAnn ann inner) = Just ([ann], inner)
peelExprOnce _ = Nothing

peelPatternOnce :: Pattern -> Maybe ([Annotation], Pattern)
peelPatternOnce (PAnn ann inner) = Just ([ann], inner)
peelPatternOnce _ = Nothing

peelTypeOnce :: Type -> Maybe ([Annotation], Type)
peelTypeOnce (TAnn ann inner) = Just ([ann], inner)
peelTypeOnce _ = Nothing

peelDeclOnce :: Decl -> Maybe ([Annotation], Decl)
peelDeclOnce (DeclAnn ann inner) = Just ([ann], inner)
peelDeclOnce (DeclTc _ inner) = Just ([], inner)
peelDeclOnce (DeclError _ inner) = Just ([], inner)
peelDeclOnce _ = Nothing

peelDataConOnce :: DataConDecl -> Maybe ([Annotation], DataConDecl)
peelDataConOnce (DataConAnn ann inner) = Just ([ann], inner)
peelDataConOnce _ = Nothing

peelLiteralOnce :: Literal -> Maybe ([Annotation], Literal)
peelLiteralOnce (LitAnn ann inner) = Just ([ann], inner)
peelLiteralOnce (LitTc _ inner) = Just ([], inner)
peelLiteralOnce _ = Nothing

peelGuardOnce :: GuardQualifier -> Maybe ([Annotation], GuardQualifier)
peelGuardOnce (GuardAnn ann inner) = Just ([ann], inner)
peelGuardOnce _ = Nothing

peelDoOnce :: DoStmt body -> Maybe ([Annotation], DoStmt body)
peelDoOnce (DoAnn ann inner) = Just ([ann], inner)
peelDoOnce (DoRecKnot _ inner) = Just ([], inner)
peelDoOnce _ = Nothing

peelCompOnce :: CompStmt -> Maybe ([Annotation], CompStmt)
peelCompOnce (CompAnn ann inner) = Just ([ann], inner)
peelCompOnce _ = Nothing

peelArithSeqOnce :: ArithSeq -> Maybe ([Annotation], ArithSeq)
peelArithSeqOnce (ArithSeqAnn ann inner) = Just ([ann], inner)
peelArithSeqOnce _ = Nothing

peelClassItemOnce :: ClassDeclItem -> Maybe ([Annotation], ClassDeclItem)
peelClassItemOnce (ClassItemAnn ann inner) = Just ([ann], inner)
peelClassItemOnce (ClassItemError _ inner) = Just ([], inner)
peelClassItemOnce _ = Nothing

peelInstanceItemOnce :: InstanceDeclItem -> Maybe ([Annotation], InstanceDeclItem)
peelInstanceItemOnce (InstanceItemAnn ann inner) = Just ([ann], inner)
peelInstanceItemOnce (InstanceItemError _ inner) = Just ([], inner)
peelInstanceItemOnce _ = Nothing

peelCmdOnce :: Cmd -> Maybe ([Annotation], Cmd)
peelCmdOnce (CmdAnn ann inner) = Just ([ann], inner)
peelCmdOnce _ = Nothing

peelExportOnce :: ExportSpec -> Maybe ([Annotation], ExportSpec)
peelExportOnce (ExportAnn ann inner) = Just ([ann], inner)
peelExportOnce _ = Nothing

peelImportOnce :: ImportItem -> Maybe ([Annotation], ImportItem)
peelImportOnce (ImportAnn ann inner) = Just ([ann], inner)
peelImportOnce (ImportItemError _ inner) = Just ([], inner)
peelImportOnce _ = Nothing

peelPendingExprOnce :: Pending.Expr -> Maybe ([Annotation], Pending.Expr)
peelPendingExprOnce (Pending.EAnn ann inner) = Just ([ann], inner)
peelPendingExprOnce _ = Nothing

peelPendingPatternOnce :: Pending.Pattern -> Maybe ([Annotation], Pending.Pattern)
peelPendingPatternOnce (Pending.PAnn ann inner) = Just ([ann], inner)
peelPendingPatternOnce _ = Nothing

peelPendingTypeOnce :: Pending.Type -> Maybe ([Annotation], Pending.Type)
peelPendingTypeOnce (Pending.TAnn ann inner) = Just ([ann], inner)
peelPendingTypeOnce _ = Nothing

peelPendingDeclOnce :: Pending.Decl -> Maybe ([Annotation], Pending.Decl)
peelPendingDeclOnce (Pending.DeclAnn ann inner) = Just ([ann], inner)
peelPendingDeclOnce (Pending.DeclTc _ inner) = Just ([], inner)
peelPendingDeclOnce (Pending.DeclError _ inner) = Just ([], inner)
peelPendingDeclOnce _ = Nothing

peelPendingDataConOnce :: Pending.DataConDecl -> Maybe ([Annotation], Pending.DataConDecl)
peelPendingDataConOnce (Pending.DataConAnn ann inner) = Just ([ann], inner)
peelPendingDataConOnce _ = Nothing

peelPendingLiteralOnce :: Pending.Literal -> Maybe ([Annotation], Pending.Literal)
peelPendingLiteralOnce (Pending.LitAnn ann inner) = Just ([ann], inner)
peelPendingLiteralOnce (Pending.LitTc _ inner) = Just ([], inner)
peelPendingLiteralOnce _ = Nothing

peelPendingGuardOnce :: Pending.GuardQualifier -> Maybe ([Annotation], Pending.GuardQualifier)
peelPendingGuardOnce (Pending.GuardAnn ann inner) = Just ([ann], inner)
peelPendingGuardOnce _ = Nothing

peelPendingDoOnce :: Pending.DoStmt body -> Maybe ([Annotation], Pending.DoStmt body)
peelPendingDoOnce (Pending.DoAnn ann inner) = Just ([ann], inner)
peelPendingDoOnce (Pending.DoRecKnot _ inner) = Just ([], inner)
peelPendingDoOnce _ = Nothing

peelPendingCompOnce :: Pending.CompStmt -> Maybe ([Annotation], Pending.CompStmt)
peelPendingCompOnce (Pending.CompAnn ann inner) = Just ([ann], inner)
peelPendingCompOnce _ = Nothing

peelPendingArithSeqOnce :: Pending.ArithSeq -> Maybe ([Annotation], Pending.ArithSeq)
peelPendingArithSeqOnce (Pending.ArithSeqAnn ann inner) = Just ([ann], inner)
peelPendingArithSeqOnce _ = Nothing

peelPendingClassItemOnce :: Pending.ClassDeclItem -> Maybe ([Annotation], Pending.ClassDeclItem)
peelPendingClassItemOnce (Pending.ClassItemAnn ann inner) = Just ([ann], inner)
peelPendingClassItemOnce (Pending.ClassItemError _ inner) = Just ([], inner)
peelPendingClassItemOnce _ = Nothing

peelPendingInstanceItemOnce :: Pending.InstanceDeclItem -> Maybe ([Annotation], Pending.InstanceDeclItem)
peelPendingInstanceItemOnce (Pending.InstanceItemAnn ann inner) = Just ([ann], inner)
peelPendingInstanceItemOnce (Pending.InstanceItemError _ inner) = Just ([], inner)
peelPendingInstanceItemOnce _ = Nothing

peelPendingCmdOnce :: Pending.Cmd -> Maybe ([Annotation], Pending.Cmd)
peelPendingCmdOnce (Pending.CmdAnn ann inner) = Just ([ann], inner)
peelPendingCmdOnce _ = Nothing

spanFromAnnotations :: [Annotation] -> Maybe SourceSpan
spanFromAnnotations =
  foldr ((<|>) . annotationSpan) Nothing

internalAbortDiagnostic :: String -> TcDiagnostic
internalAbortDiagnostic msg =
  TcDiagnostic
    { diagLoc = Nothing,
      diagSeverity = TcError,
      diagKind = OtherError ("internal type checker abort: " <> msg)
    }
