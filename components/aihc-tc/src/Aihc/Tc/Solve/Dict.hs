-- | Dictionary (class constraint) solver.
--
-- For the MVP, this is a stub. The full implementation will match
-- wanted class constraints against given dictionaries and instance
-- declarations.
module Aihc.Tc.Solve.Dict
  ( solveDict,
    DictResult (..),
  )
where

import Aihc.Tc.Constraint
import Aihc.Tc.Env (InstanceInfo (..))
import Aihc.Tc.Evidence (EvTerm (..))
import Aihc.Tc.Instantiate (applySubst)
import Aihc.Tc.Monad (TcM, bindEvidence, getInstances)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
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
-- term. Given-dictionary selection and overlap handling are deliberately left
-- to the fuller OutsideIn implementation.
solveDict :: Ct -> TcM DictResult
solveDict ct =
  case ctPred ct of
    ClassPred className args -> do
      args' <- mapM zonkType args
      instances <- getInstances
      tryInstances className args' instances
    _ ->
      pure (DictStuck ct)
  where
    tryInstances _ _ [] = pure (DictStuck ct)
    tryInstances className args (instanceInfo : rest)
      | iiClassName instanceInfo /= className =
          tryInstances className args rest
      | otherwise =
          case matchTypes (iiHead instanceInfo) args of
            Nothing -> tryInstances className args rest
            Just subst -> do
              let context = map (substPred subst) (iiContext instanceInfo)
              solvedContext <- allM solveSubPred context
              if solvedContext
                then do
                  bindEvidence (ctEvVar ct) (EvDict className args [])
                  pure DictSolved
                else tryInstances className args rest

    solveSubPred pred' = do
      let ev = ctEvVar ct
      result <- solveDict (ct {ctPred = pred', ctEvVar = ev})
      pure $
        case result of
          DictSolved -> True
          DictStuck _ -> False

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

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM _ [] = pure True
allM f (x : xs) = do
  ok <- f x
  if ok then allM f xs else pure False

foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ acc [] = pure acc
foldM f acc (x : xs) = f acc x >>= \acc' -> foldM f acc' xs
