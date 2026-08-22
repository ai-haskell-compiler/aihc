{-# LANGUAGE OverloadedStrings #-}

-- | Dictionary (class constraint) solver.
--
-- For the MVP, this is a stub. The full implementation will match
-- wanted class constraints against given dictionaries and instance
-- declarations.
module Aihc.Tc.Solve.Dict
  ( solveDict,
    solveDictWithGivens,
    DictResult (..),
    constraintTypeToPred,
    matchTypes,
    substPred,
  )
where

import Aihc.Tc.Constraint
import Aihc.Tc.Env (ClassInfo (..), InstanceInfo (..))
import Aihc.Tc.Evidence (EvTerm (..))
import Aihc.Tc.Instantiate (applySubst)
import Aihc.Tc.Monad (TcM, bindEvidence, freshEvVar, getInstances, lookupClass, lookupEvidence)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkPred, zonkType)
import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | Result of attempting to solve a dictionary constraint.
data DictResult
  = -- | Solved by given or instance.
    DictSolved
  | -- | Cannot solve yet; leave in inert set.
    DictStuck !Ct
  deriving (Show)

-- | Attempt to solve a dictionary (class) constraint.
--
-- This covers the Haskell 2010 instance path used by the current Prelude:
-- match a wanted class predicate against an in-scope instance head, solve the
-- instance context recursively, and bind the wanted evidence to a dictionary
-- term. The plain entry point has no local givens; annotation generation uses
-- 'solveDictWithGivens' when elaborating inside a qualified binding.
solveDict :: Ct -> TcM DictResult
solveDict = solveDictWithGivens []

solveDictWithGivens :: [Pred] -> Ct -> TcM DictResult
solveDictWithGivens givens ct =
  case ctPred ct of
    ClassPred className args -> do
      args' <- mapM zonkType args
      givens' <- mapM zonkPred givens
      givenEvidence <- givenDict givens' className args'
      case givenEvidence of
        Just evidence -> do
          bindEvidence (ctEvVar ct) evidence
          pure DictSolved
        Nothing ->
          case (tyConName className, args') of
            ("Typeable", [ty]) -> tryTypeable className ty
            _ -> do
              instances <- getInstances
              tryInstances (tyConName className) args' instances
    _ ->
      pure (DictStuck ct)
  where
    givenDict zonkedGivens className args =
      firstGivenOrSuperclass (ClassPred className args) zonkedGivens

    firstGivenOrSuperclass _ [] = pure Nothing
    firstGivenOrSuperclass target (given : rest)
      | target == given = pure (Just (EvGiven given))
      | otherwise = do
          projected <- superclassEvidence [] target (EvGiven given) given
          case projected of
            Just evidence -> pure (Just evidence)
            Nothing -> firstGivenOrSuperclass target rest

    superclassEvidence visited target sourceEvidence sourcePredicate =
      case sourcePredicate of
        ClassPred sourceClass sourceArgs
          | sourceClass `elem` visited -> pure Nothing
          | otherwise -> do
              classInfo <- lookupClass (tyConName sourceClass)
              case classInfo of
                Nothing -> pure Nothing
                Just info -> do
                  let substitution = Map.fromList [(tvUnique tyVar, argument) | (tyVar, argument) <- zip (ciTyVars info) sourceArgs]
                      fieldTypes = classFieldTypes info substitution
                  case traverse (constraintTypeToPred . applySubst substitution) (ciSuperClassTypes info) of
                    Just superClasses -> searchSuperClasses (sourceClass : visited) sourceEvidence (ciOrigin info) sourcePredicate fieldTypes target 0 superClasses
                    Nothing -> pure Nothing
        _ -> pure Nothing

    searchSuperClasses _ _ _ _ _ _ _ [] = pure Nothing
    searchSuperClasses visited sourceEvidence sourceOrigin sourcePredicate fieldTypes target index (superClass : rest)
      | superClass == target =
          pure (Just (EvSuperClass sourceEvidence sourceOrigin sourcePredicate fieldTypes index))
      | otherwise = do
          let projection = EvSuperClass sourceEvidence sourceOrigin sourcePredicate fieldTypes index
          nested <- superclassEvidence visited target projection superClass
          case nested of
            Just evidence -> pure (Just evidence)
            Nothing -> searchSuperClasses visited sourceEvidence sourceOrigin sourcePredicate fieldTypes target (index + 1) rest

    tryInstances _ _ [] = pure (DictStuck ct)
    tryInstances className args (instanceInfo : rest)
      | iiClassName instanceInfo /= className =
          tryInstances className args rest
      | otherwise =
          case matchTypes (iiHead instanceInfo) args of
            Nothing -> tryInstances className args rest
            Just subst -> do
              let context = map (substPred subst) (iiContext instanceInfo)
                  typeArgs = map (applySubst subst . TcTyVar) (iiTyVars instanceInfo)
              contextEvidence <- mapM solveSubPred context
              case sequence contextEvidence of
                Just evidence -> do
                  bindEvidence (ctEvVar ct) (EvDict (iiDictOrigin instanceInfo) (iiDictName instanceInfo) typeArgs evidence)
                  pure DictSolved
                Nothing -> tryInstances className args rest

    solveSubPred pred' = do
      ev <- freshEvVar
      result <- solveDictWithGivens givens (ct {ctPred = pred', ctEvVar = ev})
      case result of
        DictSolved -> lookupEvidence ev
        DictStuck _ -> pure Nothing

    tryTypeable typeableTyCon ty =
      case typeableArguments ty of
        Nothing -> pure (DictStuck ct)
        Just arguments -> do
          classOrigin <- maybe Nothing ciOrigin <$> lookupClass "Typeable"
          argumentEvidence <- mapM (solveSubPred . ClassPred typeableTyCon . (: [])) arguments
          case sequence argumentEvidence of
            Just evidence -> do
              bindEvidence (ctEvVar ct) (EvTypeable classOrigin ty evidence)
              pure DictSolved
            Nothing -> pure (DictStuck ct)

typeableArguments :: TcType -> Maybe [TcType]
typeableArguments ty =
  case ty of
    TcTyCon _ arguments -> Just arguments
    TcFunTy argument result -> Just [argument, result]
    TcTyVar {} -> Nothing
    TcMetaTv {} -> Nothing
    TcForAllTy {} -> Nothing
    TcQualTy {} -> Nothing
    TcAppTy {} -> Nothing

classFieldTypes :: ClassInfo -> Map Unique TcType -> [TcType]
classFieldTypes classInfo substitution =
  map (applySubst substitution) (ciSuperClassTypes classInfo)
    <> map (methodFieldType classInfo substitution . snd) (ciMethods classInfo)

methodFieldType :: ClassInfo -> Map Unique TcType -> TypeScheme -> TcType
methodFieldType classInfo substitution (ForAll typeVariables predicates body) =
  applySubst substitution $
    foldr TcForAllTy qualifiedBody extraTypeVariables
  where
    classVariables = ciTyVars classInfo
    extraTypeVariables = filter (`notElem` classVariables) typeVariables
    remainingPredicates = filter (not . isClassPredicate) predicates
    qualifiedBody
      | null remainingPredicates = body
      | otherwise = TcQualTy remainingPredicates body
    isClassPredicate predicate =
      case predicate of
        ClassPred className _ -> tyConName className == ciName classInfo
        EqPred {} -> False

constraintTypeToPred :: TcType -> Maybe Pred
constraintTypeToPred ty =
  case collectTypeApplications ty of
    (TcTyCon (TyCon "~" 2) [], [left, right]) -> Just (EqPred left right)
    (TcTyCon tyCon headArgs, arguments) ->
      Just (ClassPred tyCon (headArgs <> arguments))
    _ -> Nothing

collectTypeApplications :: TcType -> (TcType, [TcType])
collectTypeApplications ty =
  case ty of
    TcAppTy function argument ->
      let (headType, arguments) = collectTypeApplications function
       in (headType, arguments <> [argument])
    _ -> (ty, [])

matchTypes :: [TcType] -> [TcType] -> Maybe (Map Unique TcType)
matchTypes patterns targets
  | length patterns /= length targets = Nothing
  | otherwise = foldM matchOne Map.empty (zip patterns targets)

matchOne :: Map Unique TcType -> (TcType, TcType) -> Maybe (Map Unique TcType)
matchOne subst (TcTyVar tv, target) =
  case Map.lookup (tvUnique tv) subst of
    Nothing -> Just (Map.insert (tvUnique tv) target subst)
    Just existing
      | existing == target -> Just subst
      | otherwise -> Nothing
matchOne subst (TcTyCon tc args, TcTyCon targetTc targetArgs)
  | tc == targetTc,
    length args == length targetArgs =
      foldM matchOne subst (zip args targetArgs)
matchOne subst (TcFunTy a b, TcFunTy targetA targetB) =
  matchOne subst (a, targetA) >>= \subst' -> matchOne subst' (b, targetB)
matchOne subst (TcAppTy f a, TcAppTy targetF targetA) =
  matchOne subst (f, targetF) >>= \subst' -> matchOne subst' (a, targetA)
matchOne subst (patternTy, targetTy)
  | patternTy == targetTy = Just subst
  | otherwise = Nothing

substPred :: Map Unique TcType -> Pred -> Pred
substPred subst (ClassPred className args) = ClassPred className (map (applySubst subst) args)
substPred subst (EqPred left right) = EqPred (applySubst subst left) (applySubst subst right)
