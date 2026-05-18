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
  )
where

import Aihc.Tc.Constraint
import Aihc.Tc.Env (InstanceInfo (..))
import Aihc.Tc.Evidence (EvTerm (..))
import Aihc.Tc.Instantiate (applySubst)
import Aihc.Tc.Monad (TcM, bindEvidence, freshEvVar, getInstances, lookupEvidence)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Monad (foldM)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

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
      case givenDict className args' of
        Just evidence -> do
          bindEvidence (ctEvVar ct) evidence
          pure DictSolved
        Nothing -> do
          instances <- getInstances
          tryInstances className args' instances
    _ ->
      pure (DictStuck ct)
  where
    givenDict className args =
      EvGiven <$> find (sameClassPred className args) givens

    tryInstances _ _ [] = pure (DictStuck ct)
    tryInstances className args (instanceInfo : rest)
      | iiClassName instanceInfo /= className =
          tryInstances className args rest
      | otherwise =
          case matchTypes (iiHead instanceInfo) args of
            Nothing -> tryInstances className args rest
            Just subst -> do
              let context = map (substPred subst) (iiContext instanceInfo)
                  dictName = instanceDictName className (iiHead instanceInfo)
                  typeArgs = map (applySubst subst . TcTyVar) (iiTyVars instanceInfo)
              contextEvidence <- mapM solveSubPred context
              case sequence contextEvidence of
                Just evidence -> do
                  bindEvidence (ctEvVar ct) (EvDict dictName typeArgs evidence)
                  pure DictSolved
                Nothing -> tryInstances className args rest

    solveSubPred pred' = do
      ev <- freshEvVar
      result <- solveDictWithGivens givens (ct {ctPred = pred', ctEvVar = ev})
      case result of
        DictSolved -> lookupEvidence ev
        DictStuck _ -> pure Nothing

sameClassPred :: Text -> [TcType] -> Pred -> Bool
sameClassPred className args pred' =
  case pred' of
    ClassPred givenClass givenArgs ->
      givenClass == className && givenArgs == args
    EqPred {} ->
      False

instanceDictName :: Text -> [TcType] -> Text
instanceDictName className tys = "$f" <> className <> T.concat (map typeSuffix tys)

typeSuffix :: TcType -> Text
typeSuffix ty =
  case ty of
    TcTyVar tv -> tvName tv
    TcTyCon tc [] -> tyConName tc
    TcTyCon (TyCon "[]" _) [_] -> "List"
    TcTyCon tc args -> tyConName tc <> T.concat (map typeSuffix args)
    _ -> "T"

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
