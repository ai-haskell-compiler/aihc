{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Finalize type-checker annotations after constraint solving.
module Aihc.Tc.Finalize
  ( finalizeModuleTc,
  )
where

import Aihc.Parser.Syntax (Annotation, Module, fromAnnotation, mkAnnotation)
import Aihc.Tc.Annotations (PendingTcAnnotation (..), PendingTcBinderAnnotation (..), TcAnnotation (..), TcBinderAnnotation (..))
import Aihc.Tc.Evidence (EvTerm, EvVar)
import Aihc.Tc.Monad
import Aihc.Tc.Zonk (zonkType)
import Data.Data (Data, gmapM)
import Data.Typeable (cast)

-- | Convert every pending type-checker annotation in a module to a final
-- annotation. This is intentionally generic over the parser AST so adding new
-- syntax constructors cannot make pending annotations escape.
finalizeModuleTc :: Module -> TcM Module
finalizeModuleTc =
  everywhereM finalizeAnnotationNode

everywhereM :: (Monad m, Data a) => (forall b. (Data b) => b -> m b) -> a -> m a
everywhereM f value =
  gmapM (everywhereM f) value >>= f

finalizeAnnotationNode :: forall a. (Data a) => a -> TcM a
finalizeAnnotationNode value =
  case cast value of
    Just (ann :: Annotation) -> do
      ann' <- finalizeAnnotationTc ann
      case cast ann' of
        Just value' -> pure value'
        Nothing ->
          abortTc "internal type annotation error: annotation cast failed during finalization"
    Nothing ->
      pure value

finalizeAnnotationTc :: Annotation -> TcM Annotation
finalizeAnnotationTc ann =
  case fromAnnotation @PendingTcAnnotation ann of
    Just pending -> mkAnnotation <$> annotationForPendingTc pending
    Nothing ->
      case fromAnnotation @PendingTcBinderAnnotation ann of
        Just pending -> mkAnnotation <$> annotationForPendingBinder pending
        Nothing -> pure ann

annotationForPendingTc :: PendingTcAnnotation -> TcM TcAnnotation
annotationForPendingTc pending = do
  ty <- zonkType (pendingTcAnnType pending)
  typeArgs <- mapM zonkType (pendingTcAnnTypeArgs pending)
  evidenceTerms <- mapM evidenceForEvVar (pendingTcAnnEvidenceVars pending)
  termArgTypes <- mapM zonkType (pendingTcAnnTermArgTypes pending)
  pure (TcAnnotation ty typeArgs evidenceTerms termArgTypes)

annotationForPendingBinder :: PendingTcBinderAnnotation -> TcM TcBinderAnnotation
annotationForPendingBinder pending = do
  ty <- zonkType (pendingTcBinderType pending)
  pure (TcBinderAnnotation (pendingTcBinderName pending) ty)

evidenceForEvVar :: EvVar -> TcM EvTerm
evidenceForEvVar ev = do
  maybeEvidence <- lookupEvidence ev
  case maybeEvidence of
    Just evidence -> pure evidence
    Nothing ->
      abortTc ("internal type annotation error: missing evidence for " <> show ev)
