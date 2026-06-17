{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Finalize type-checker annotations after constraint solving.
module Aihc.Tc.Finalize
  ( finalizeModuleTc,
  )
where

import Aihc.Parser.Syntax (Annotation, Module, fromAnnotation, mkAnnotation)
import Aihc.Tc.Annotations
  ( PendingTcAnnotation (..),
    TcAnnotation (..),
    TcClassAnnotation (..),
    TcClassMethodAnnotation (..),
    TcDictBinderAnnotation (..),
    TcInstanceAnnotation (..),
    TcInstanceMethodAnnotation (..),
  )
import Aihc.Tc.Evidence (Coercion (..), EvTerm (..), EvVar)
import Aihc.Tc.Monad
import Aihc.Tc.Types (Pred (..), TcType (..), Unique (..))
import Aihc.Tc.Zonk (zonkType)
import Control.Applicative ((<|>))
import Control.Monad ((>=>))
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
    Nothing -> do
      rejectMetaFinalAnnotation ann
      pure ann

annotationForPendingTc :: PendingTcAnnotation -> TcM TcAnnotation
annotationForPendingTc pending = do
  ty <- zonkType (pendingTcAnnType pending)
  typeArgs <- mapM zonkType (pendingTcAnnTypeArgs pending)
  evidenceTerms <- mapM (evidenceForEvVar >=> zonkEvTerm) (pendingTcAnnEvidenceVars pending)
  termArgTypes <- mapM zonkType (pendingTcAnnTermArgTypes pending)
  let ann = TcAnnotation ty typeArgs evidenceTerms termArgTypes
  rejectMetaTcAnnotation ann
  pure ann

evidenceForEvVar :: EvVar -> TcM EvTerm
evidenceForEvVar ev = do
  maybeEvidence <- lookupEvidence ev
  case maybeEvidence of
    Just evidence -> pure evidence
    Nothing ->
      abortTc ("internal type annotation error: missing evidence for " <> show ev)

zonkEvTerm :: EvTerm -> TcM EvTerm
zonkEvTerm evTerm =
  case evTerm of
    EvVarTerm ev ->
      pure (EvVarTerm ev)
    EvGiven pred' ->
      EvGiven <$> zonkPred pred'
    EvDict name typeArgs evidence ->
      EvDict name <$> mapM zonkType typeArgs <*> mapM zonkEvTerm evidence
    EvCoercion coercion ->
      EvCoercion <$> zonkCoercion coercion
    EvSuperClass evidence index ->
      EvSuperClass <$> zonkEvTerm evidence <*> pure index
    EvCast evidence coercion ->
      EvCast <$> zonkEvTerm evidence <*> zonkCoercion coercion

zonkCoercion :: Coercion -> TcM Coercion
zonkCoercion coercion =
  case coercion of
    CoVar ev ->
      pure (CoVar ev)
    Refl ty ->
      Refl <$> zonkType ty
    Sym inner ->
      Sym <$> zonkCoercion inner
    Trans left right ->
      Trans <$> zonkCoercion left <*> zonkCoercion right
    TyConAppCo tyCon coercions ->
      TyConAppCo tyCon <$> mapM zonkCoercion coercions
    AxiomInstCo name typeArgs ->
      AxiomInstCo name <$> mapM zonkType typeArgs

zonkPred :: Pred -> TcM Pred
zonkPred pred' =
  case pred' of
    ClassPred className args ->
      ClassPred className <$> mapM zonkType args
    EqPred left right ->
      EqPred <$> zonkType left <*> zonkType right

rejectMetaTcAnnotation :: TcAnnotation -> TcM ()
rejectMetaTcAnnotation ann =
  rejectMeta "finalized annotation" (firstMetaTcAnnotation ann)

rejectMetaFinalAnnotation :: Annotation -> TcM ()
rejectMetaFinalAnnotation ann = do
  traverseReject (firstMetaTcAnnotation <$> fromAnnotation @TcAnnotation ann)
  traverseReject (firstMetaClassAnnotation <$> fromAnnotation @TcClassAnnotation ann)
  traverseReject (firstMetaInstanceAnnotation <$> fromAnnotation @TcInstanceAnnotation ann)
  traverseReject (firstMetaInstanceMethodAnnotation <$> fromAnnotation @TcInstanceMethodAnnotation ann)
  where
    traverseReject Nothing = pure ()
    traverseReject (Just maybeMeta) = rejectMeta "finalized annotation" maybeMeta

rejectMeta :: String -> Maybe Unique -> TcM ()
rejectMeta context maybeMeta =
  case maybeMeta of
    Nothing ->
      pure ()
    Just (Unique meta) ->
      abortTc ("internal type annotation error: unzonked meta-variable ?" <> show meta <> " in " <> context)

firstMetaTcAnnotation :: TcAnnotation -> Maybe Unique
firstMetaTcAnnotation ann =
  firstJusts
    ( firstMetaType (tcAnnType ann)
        : map firstMetaType (tcAnnTypeArgs ann)
        ++ map firstMetaEvTerm (tcAnnEvidenceTerms ann)
        ++ map firstMetaType (tcAnnTermArgTypes ann)
    )

firstMetaClassAnnotation :: TcClassAnnotation -> Maybe Unique
firstMetaClassAnnotation (TcClassAnnotation methods) =
  firstJusts (map firstMetaClassMethodAnnotation methods)

firstMetaClassMethodAnnotation :: TcClassMethodAnnotation -> Maybe Unique
firstMetaClassMethodAnnotation method =
  firstMetaType (tcClassMethodType method) <|> firstMetaType (tcClassMethodDictType method)

firstMetaInstanceAnnotation :: TcInstanceAnnotation -> Maybe Unique
firstMetaInstanceAnnotation ann =
  firstJusts
    ( firstMetaType (tcInstanceDictType ann)
        : map firstMetaType (tcInstanceHeadTypes ann)
        ++ map firstMetaDictBinderAnnotation (tcInstanceContextDicts ann)
    )

firstMetaDictBinderAnnotation :: TcDictBinderAnnotation -> Maybe Unique
firstMetaDictBinderAnnotation ann =
  firstJusts (map firstMetaType (tcDictBinderArgs ann)) <|> firstMetaType (tcDictBinderType ann)

firstMetaInstanceMethodAnnotation :: TcInstanceMethodAnnotation -> Maybe Unique
firstMetaInstanceMethodAnnotation ann =
  firstMetaType (tcInstanceMethodType ann)

firstMetaEvTerm :: EvTerm -> Maybe Unique
firstMetaEvTerm evTerm =
  case evTerm of
    EvVarTerm {} ->
      Nothing
    EvGiven pred' ->
      firstMetaPred pred'
    EvDict _ typeArgs evidence ->
      firstJusts (map firstMetaType typeArgs ++ map firstMetaEvTerm evidence)
    EvCoercion coercion ->
      firstMetaCoercion coercion
    EvSuperClass evidence _ ->
      firstMetaEvTerm evidence
    EvCast evidence coercion ->
      firstMetaEvTerm evidence <|> firstMetaCoercion coercion

firstMetaCoercion :: Coercion -> Maybe Unique
firstMetaCoercion coercion =
  case coercion of
    CoVar {} ->
      Nothing
    Refl ty ->
      firstMetaType ty
    Sym inner ->
      firstMetaCoercion inner
    Trans left right ->
      firstMetaCoercion left <|> firstMetaCoercion right
    TyConAppCo _ coercions ->
      firstJusts (map firstMetaCoercion coercions)
    AxiomInstCo _ typeArgs ->
      firstJusts (map firstMetaType typeArgs)

firstMetaPred :: Pred -> Maybe Unique
firstMetaPred pred' =
  case pred' of
    ClassPred _ args ->
      firstJusts (map firstMetaType args)
    EqPred left right ->
      firstMetaType left <|> firstMetaType right

firstMetaType :: TcType -> Maybe Unique
firstMetaType ty =
  case ty of
    TcMetaTv meta ->
      Just meta
    TcTyVar {} ->
      Nothing
    TcTyCon _ args ->
      firstJusts (map firstMetaType args)
    TcFunTy left right ->
      firstMetaType left <|> firstMetaType right
    TcForAllTy _ body ->
      firstMetaType body
    TcQualTy preds body ->
      firstJusts (map firstMetaPred preds) <|> firstMetaType body
    TcAppTy fun arg ->
      firstMetaType fun <|> firstMetaType arg

firstJusts :: [Maybe a] -> Maybe a
firstJusts [] = Nothing
firstJusts (Nothing : rest) = firstJusts rest
firstJusts (Just value : _) = Just value
