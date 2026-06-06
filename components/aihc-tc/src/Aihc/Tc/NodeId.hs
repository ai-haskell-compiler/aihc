{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Temporary type-checker annotation targets.
--
-- This is deliberately internal bookkeeping. It is an ugly bridge between
-- constraint generation, constraint solving, and source-overlay payloads;
-- it must never escape in the module returned by the type checker.
module Aihc.Tc.NodeId
  ( TcNodeId (..),
    TcNodeAnnotation (..),
    assignTcNodeIds,
    attachTargetedAnnotations,
    stripTcNodeIds,
    tcNodeIdFromAnnotations,
    tcNodeIdFromExpr,
    tcNodeIdFromPattern,
    tcNodeIdFromRhs,
    tcNodeIdFromExprRhs,
    tcNodeIdFromType,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    ClassDeclItem (..),
    CompStmt (..),
    DataConDecl (..),
    Decl (..),
    Expr (..),
    InstanceDeclItem (..),
    Module (..),
    Pattern (..),
    Rhs (..),
    Type (..),
    fromAnnotation,
    mkAnnotation,
  )
import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Control.Monad.Trans.State.Strict (State, evalState, get, modify', put, runState)
import Data.Data (Data, Typeable, cast, gmapM)
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)

newtype TcNodeId = TcNodeId Int
  deriving (Eq, Ord, Show)

newtype TcNodeAnnotation = TcNodeAnnotation TcNodeId
  deriving (Show)

assignTcNodeIds :: Module -> Module
assignTcNodeIds modu =
  evalState (assignNodeIds modu) 0

stripTcNodeIds :: Module -> Module
stripTcNodeIds =
  stripNodeIds

attachTargetedAnnotations :: (Typeable ann) => [(Maybe TcNodeId, ann)] -> Module -> Module
attachTargetedAnnotations annotations modu =
  stripTcNodeIds (foldl attachAnnotation modu annotations)

attachAnnotation :: (Typeable ann) => Module -> (Maybe TcNodeId, ann) -> Module
attachAnnotation modu (target, payload) =
  case target of
    Just nodeId ->
      let (modu', attached) = runState (attachAnnotationAt nodeId payload modu) False
       in if attached then modu' else appendModuleAnnotation payload modu
    Nothing ->
      appendModuleAnnotation payload modu

appendModuleAnnotation :: (Typeable ann) => ann -> Module -> Module
appendModuleAnnotation payload modu =
  modu {moduleAnns = moduleAnns modu <> [mkAnnotation payload]}

tcNodeIdFromAnnotations :: [Annotation] -> Maybe TcNodeId
tcNodeIdFromAnnotations =
  listToMaybe . mapMaybe annotationNodeId

tcNodeIdFromExpr :: Expr -> Maybe TcNodeId
tcNodeIdFromExpr (EAnn ann _) = annotationNodeId ann
tcNodeIdFromExpr _ = Nothing

tcNodeIdFromPattern :: Pattern -> Maybe TcNodeId
tcNodeIdFromPattern (PAnn ann _) = annotationNodeId ann
tcNodeIdFromPattern _ = Nothing

tcNodeIdFromType :: Type -> Maybe TcNodeId
tcNodeIdFromType (TAnn ann _) = annotationNodeId ann
tcNodeIdFromType _ = Nothing

tcNodeIdFromRhs :: Rhs body -> Maybe TcNodeId
tcNodeIdFromRhs rhs =
  case rhs of
    UnguardedRhs anns _ _ -> tcNodeIdFromAnnotations anns
    GuardedRhss anns _ _ -> tcNodeIdFromAnnotations anns

tcNodeIdFromExprRhs :: Rhs Expr -> Maybe TcNodeId
tcNodeIdFromExprRhs rhs =
  case rhs of
    UnguardedRhs anns expr _ -> tcNodeIdFromExpr expr <|> tcNodeIdFromAnnotations anns
    GuardedRhss anns _ _ -> tcNodeIdFromAnnotations anns

annotationNodeId :: Annotation -> Maybe TcNodeId
annotationNodeId ann = do
  TcNodeAnnotation nodeId <- fromAnnotation ann
  pure nodeId

assignNodeIds :: (Data a) => a -> State Int a
assignNodeIds node =
  case cast node of
    Just (anns :: [Annotation]) -> do
      anns' <- tagAnnotations anns
      pure (fromMaybe node (cast anns'))
    Nothing -> do
      node' <- gmapM assignNodeIds node
      assignNodeIdHere node'

assignNodeIdHere :: (Data a) => a -> State Int a
assignNodeIdHere =
  tryCastM tagAnnotations
    >=> tryCastM tagExpr
    >=> tryCastM tagDecl
    >=> tryCastM tagPattern
    >=> tryCastM tagType
    >=> tryCastM tagDataConDecl
    >=> tryCastM tagClassDeclItem
    >=> tryCastM tagInstanceDeclItem
    >=> tryCastM tagCompStmt

nextNodeAnnotation :: State Int Annotation
nextNodeAnnotation = do
  next <- get
  put (next + 1)
  pure (mkAnnotation (TcNodeAnnotation (TcNodeId next)))

tagAnnotations :: [Annotation] -> State Int [Annotation]
tagAnnotations anns = do
  ann <- nextNodeAnnotation
  pure (anns <> [ann])

tagExpr :: Expr -> State Int Expr
tagExpr expr = do
  ann <- nextNodeAnnotation
  pure (EAnn ann expr)

tagDecl :: Decl -> State Int Decl
tagDecl decl = do
  ann <- nextNodeAnnotation
  pure (DeclAnn ann decl)

tagPattern :: Pattern -> State Int Pattern
tagPattern pattern' = do
  ann <- nextNodeAnnotation
  pure (PAnn ann pattern')

tagType :: Type -> State Int Type
tagType ty = do
  ann <- nextNodeAnnotation
  pure (TAnn ann ty)

tagDataConDecl :: DataConDecl -> State Int DataConDecl
tagDataConDecl dataCon = do
  ann <- nextNodeAnnotation
  pure (DataConAnn ann dataCon)

tagClassDeclItem :: ClassDeclItem -> State Int ClassDeclItem
tagClassDeclItem item = do
  ann <- nextNodeAnnotation
  pure (ClassItemAnn ann item)

tagInstanceDeclItem :: InstanceDeclItem -> State Int InstanceDeclItem
tagInstanceDeclItem item = do
  ann <- nextNodeAnnotation
  pure (InstanceItemAnn ann item)

tagCompStmt :: CompStmt -> State Int CompStmt
tagCompStmt stmt = do
  ann <- nextNodeAnnotation
  pure (CompAnn ann stmt)

attachAnnotationAt :: (Data a, Typeable ann) => TcNodeId -> ann -> a -> State Bool a
attachAnnotationAt target payload node =
  case cast node of
    Just (anns :: [Annotation]) -> do
      anns' <- attachAnnotationList target payload anns
      pure (fromMaybe node (cast anns'))
    Nothing -> do
      node' <- gmapM (attachAnnotationAt target payload) node
      alreadyAttached <- get
      if alreadyAttached
        then pure node'
        else attachAnnotationHere target payload node'

attachAnnotationHere :: (Data a, Typeable ann) => TcNodeId -> ann -> a -> State Bool a
attachAnnotationHere target payload =
  tryCastBoolM (attachAnnotationList target payload)
    >=> tryCastBoolM (attachExprAnn target payload)
    >=> tryCastBoolM (attachDeclAnn target payload)
    >=> tryCastBoolM (attachPatternAnn target payload)
    >=> tryCastBoolM (attachTypeAnn target payload)
    >=> tryCastBoolM (attachDataConAnn target payload)
    >=> tryCastBoolM (attachClassItemAnn target payload)
    >=> tryCastBoolM (attachInstanceItemAnn target payload)
    >=> tryCastBoolM (attachCompStmtAnn target payload)

attachAnnotationList :: (Typeable ann) => TcNodeId -> ann -> [Annotation] -> State Bool [Annotation]
attachAnnotationList target payload anns
  | target `elem` mapMaybe annotationNodeId anns = do
      modify' (const True)
      pure (anns <> [mkAnnotation payload])
  | otherwise =
      pure anns

attachExprAnn :: (Typeable ann) => TcNodeId -> ann -> Expr -> State Bool Expr
attachExprAnn target payload expr@(EAnn ann _)
  | annotationNodeId ann == Just target = do
      modify' (const True)
      pure (EAnn (mkAnnotation payload) expr)
attachExprAnn _ _ expr =
  pure expr

attachDeclAnn :: (Typeable ann) => TcNodeId -> ann -> Decl -> State Bool Decl
attachDeclAnn target payload decl@(DeclAnn ann _)
  | annotationNodeId ann == Just target = do
      modify' (const True)
      pure (DeclAnn (mkAnnotation payload) decl)
attachDeclAnn _ _ decl =
  pure decl

attachPatternAnn :: (Typeable ann) => TcNodeId -> ann -> Pattern -> State Bool Pattern
attachPatternAnn target payload pattern'@(PAnn ann _)
  | annotationNodeId ann == Just target = do
      modify' (const True)
      pure (PAnn (mkAnnotation payload) pattern')
attachPatternAnn _ _ pattern' =
  pure pattern'

attachTypeAnn :: (Typeable ann) => TcNodeId -> ann -> Type -> State Bool Type
attachTypeAnn target payload ty@(TAnn ann _)
  | annotationNodeId ann == Just target = do
      modify' (const True)
      pure (TAnn (mkAnnotation payload) ty)
attachTypeAnn _ _ ty =
  pure ty

attachDataConAnn :: (Typeable ann) => TcNodeId -> ann -> DataConDecl -> State Bool DataConDecl
attachDataConAnn target payload dataCon@(DataConAnn ann _)
  | annotationNodeId ann == Just target = do
      modify' (const True)
      pure (DataConAnn (mkAnnotation payload) dataCon)
attachDataConAnn _ _ dataCon =
  pure dataCon

attachClassItemAnn :: (Typeable ann) => TcNodeId -> ann -> ClassDeclItem -> State Bool ClassDeclItem
attachClassItemAnn target payload item@(ClassItemAnn ann _)
  | annotationNodeId ann == Just target = do
      modify' (const True)
      pure (ClassItemAnn (mkAnnotation payload) item)
attachClassItemAnn _ _ item =
  pure item

attachInstanceItemAnn :: (Typeable ann) => TcNodeId -> ann -> InstanceDeclItem -> State Bool InstanceDeclItem
attachInstanceItemAnn target payload item@(InstanceItemAnn ann _)
  | annotationNodeId ann == Just target = do
      modify' (const True)
      pure (InstanceItemAnn (mkAnnotation payload) item)
attachInstanceItemAnn _ _ item =
  pure item

attachCompStmtAnn :: (Typeable ann) => TcNodeId -> ann -> CompStmt -> State Bool CompStmt
attachCompStmtAnn target payload stmt@(CompAnn ann _)
  | annotationNodeId ann == Just target = do
      modify' (const True)
      pure (CompAnn (mkAnnotation payload) stmt)
attachCompStmtAnn _ _ stmt =
  pure stmt

stripNodeIds :: (Data a) => a -> a
stripNodeIds node =
  case cast node of
    Just (anns :: [Annotation]) ->
      fromMaybe node (cast (stripAnnotations anns))
    Nothing ->
      stripNodeIdHere (runIdentity (gmapM (Identity . stripNodeIds) node))

stripNodeIdHere :: (Data a) => a -> a
stripNodeIdHere =
  tryCast stripAnnotations
    . tryCast stripExprAnn
    . tryCast stripDeclAnn
    . tryCast stripPatternAnn
    . tryCast stripTypeAnn
    . tryCast stripDataConAnn
    . tryCast stripClassItemAnn
    . tryCast stripInstanceItemAnn
    . tryCast stripCompStmtAnn

stripAnnotations :: [Annotation] -> [Annotation]
stripAnnotations =
  filter (not . isNodeAnnotation)

isNodeAnnotation :: Annotation -> Bool
isNodeAnnotation ann =
  case fromAnnotation @TcNodeAnnotation ann of
    Just _ -> True
    Nothing -> False

stripExprAnn :: Expr -> Expr
stripExprAnn (EAnn ann inner)
  | isNodeAnnotation ann = inner
stripExprAnn expr = expr

stripDeclAnn :: Decl -> Decl
stripDeclAnn (DeclAnn ann inner)
  | isNodeAnnotation ann = inner
stripDeclAnn decl = decl

stripPatternAnn :: Pattern -> Pattern
stripPatternAnn (PAnn ann inner)
  | isNodeAnnotation ann = inner
stripPatternAnn pattern' = pattern'

stripTypeAnn :: Type -> Type
stripTypeAnn (TAnn ann inner)
  | isNodeAnnotation ann = inner
stripTypeAnn ty = ty

stripDataConAnn :: DataConDecl -> DataConDecl
stripDataConAnn (DataConAnn ann inner)
  | isNodeAnnotation ann = inner
stripDataConAnn dataCon = dataCon

stripClassItemAnn :: ClassDeclItem -> ClassDeclItem
stripClassItemAnn (ClassItemAnn ann inner)
  | isNodeAnnotation ann = inner
stripClassItemAnn item = item

stripInstanceItemAnn :: InstanceDeclItem -> InstanceDeclItem
stripInstanceItemAnn (InstanceItemAnn ann inner)
  | isNodeAnnotation ann = inner
stripInstanceItemAnn item = item

stripCompStmtAnn :: CompStmt -> CompStmt
stripCompStmtAnn (CompAnn ann inner)
  | isNodeAnnotation ann = inner
stripCompStmtAnn stmt = stmt

tryCastM :: forall a b s. (Data a, Typeable b) => (b -> State s b) -> a -> State s a
tryCastM f node =
  case cast node of
    Just typedNode -> do
      typedNode' <- f typedNode
      pure (fromMaybe node (cast typedNode'))
    Nothing ->
      pure node

tryCastBoolM :: forall a b. (Data a, Typeable b) => (b -> State Bool b) -> a -> State Bool a
tryCastBoolM = tryCastM

tryCast :: forall a b. (Typeable a, Typeable b) => (b -> b) -> a -> a
tryCast f node =
  case cast node of
    Just typedNode ->
      fromMaybe node (cast (f typedNode))
    Nothing ->
      node
