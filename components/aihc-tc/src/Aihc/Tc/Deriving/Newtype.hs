{-# LANGUAGE OverloadedStrings #-}

-- | Check the method casts of a derived instance that coerces the methods
-- of another type's instance, before FC conversion.
module Aihc.Tc.Deriving.Newtype (checkNewtypeInstance) where

import Aihc.Tc.Annotations
import Aihc.Tc.Deriving.Coerce (coercionBetween)
import Aihc.Tc.Deriving.Context (newtypeRepresentation)
import Aihc.Tc.Env
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Evidence
import Aihc.Tc.Kind (tcTypeKind)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Dict (matchTypes)
import Aihc.Tc.Types
import Control.Monad (zipWithM)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T

checkNewtypeInstance :: (Text, Text) -> (Text -> [Pred] -> Pred -> TcM EvTerm) -> (ClassInfo -> [TcType] -> Text -> TcM TypeScheme) -> TcDerivingPlan -> ClassInfo -> [Pred] -> TcInstanceAnnotation -> TcM TcInstanceAnnotation
checkNewtypeInstance origin solve methodScheme original info context annotation = do
  let substitution = Map.fromList [(tvUnique old, TcTyVar new) | old <- tcDerivingTyVars original, new <- tcInstanceTyVars annotation, tvName old == tvName new]
      plan = original {tcDerivingHeadTypes = tcInstanceHeadTypes annotation}
  case coercedSource plan of
    Left message -> reject message >> pure annotation
    Right rawSource -> do
      let sourceType = applySubst substitution rawSource
          headTypes = init (tcInstanceHeadTypes annotation) <> [sourceType]
      sourceSchemes <- mapM (methodScheme info headTypes . fst) (ciMethods info)
      headKinds <- mapM tcTypeKind headTypes
      let kindSubstitution = fromMaybe Map.empty (matchTypes (map tvKind (ciTyVars info)) headKinds)
          classSubstitution = Map.fromList (zip (map tvUnique (ciTyVars info)) headTypes) <> kindSubstitution
          superclassFields = map (applySubst classSubstitution) (ciSuperClassTypes info)
          fieldTypes = superclassFields <> map fieldType sourceSchemes
      methods <- zipWithM (checkMethod headTypes) [length superclassFields ..] (map fst (ciMethods info))
      evidence <- if null methods then pure Nothing else Just <$> solve (ciName info) context (ClassPred (ciTyCon info) headTypes)
      case evidence of
        Just term | mentionsSelf term -> reject (mechanism <> " requires evidence that does not stand on the instance being derived")
        _ -> pure ()
      dictionaryCast <- case (tcDerivingDataType plan, tcInstanceSuperClasses annotation, evidence) of
        (Just _, [], Just _) | null (ciKindTyVars info) -> do
          proof <- coercionBetween (tcInstanceAssociatedTypes annotation) sourceType (last (tcInstanceHeadTypes annotation))
          pure (TyConAppCo (ciTyCon info) headTypes . (map Refl (init headTypes) <>) . (: []) <$> proof)
        _ -> pure Nothing
      pure annotation {tcInstanceNewtype = Just (TcNewtypeInstance headTypes evidence fieldTypes dictionaryCast (catMaybes methods))}
  where
    reject = emitError (tcDerivingSourceSpan original) . OtherError
    mechanism =
      case tcDerivingStrategy original of
        TcDerivingVia {} -> "deriving via"
        _ -> "newtype deriving"
    fieldType (ForAll variables predicates body) =
      foldr TcForAllTy (if null predicates then body else TcQualTy predicates body) variables
    checkMethod headTypes index name = do
      ForAll variables sourcePredicates source <- methodScheme info headTypes name
      ForAll _ targetPredicates target <- methodScheme info (tcInstanceHeadTypes annotation) name
      -- The coercion search finds the newtypes it needs itself, so a plan
      -- needs no datatype metadata of its own to prove a method cast.
      proof <-
        if sourcePredicates == targetPredicates
          then coercionBetween (tcInstanceAssociatedTypes annotation) source target
          else pure Nothing
      case proof of
        Nothing -> reject (mechanism <> " cannot prove a safe coercion for method " <> T.unpack name) >> pure Nothing
        Just coercion -> pure (Just (TcNewtypeMethod name index variables targetPredicates coercion))
    mentionsSelf term = case term of
      EvDict dictionaryOrigin name _ arguments -> (dictionaryOrigin == origin && name == tcInstanceDictName annotation) || any mentionsSelf arguments
      EvSuperClass inner _ _ _ _ -> mentionsSelf inner
      EvCast inner _ -> mentionsSelf inner
      EvTypeLam _ inner -> mentionsSelf inner
      EvDictLam _ _ inner -> mentionsSelf inner
      EvTypeApp inner _ -> mentionsSelf inner
      EvDictApp function argument -> mentionsSelf function || mentionsSelf argument
      _ -> False

-- | The type whose instance the plan reuses, and whose methods the generated
-- instance coerces: the representation for a newtype, the via type for a via.
coercedSource :: TcDerivingPlan -> Either String TcType
coercedSource plan =
  case tcDerivingStrategy plan of
    TcDerivingVia viaType -> Right viaType
    _ -> newtypeRepresentation plan
