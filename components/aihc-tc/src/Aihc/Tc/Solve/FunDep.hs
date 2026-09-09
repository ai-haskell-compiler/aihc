-- | Improvement from functional dependencies.
--
-- A dependency @U -> V@ of a class says that the parameters at the
-- positions @U@ determine the ones at @V@. When two class constraints agree
-- on the determining parameters, the solver may therefore equate their
-- determined parameters, which solves meta variables that no single
-- constraint could solve on its own. This is improvement: it produces
-- equalities, never evidence, and the dictionaries themselves are still
-- solved by an instance or a given afterwards.
--
-- Three constraints can improve a wanted: another wanted, a given, and the
-- head of an instance whose determining parameters the wanted matches.
module Aihc.Tc.Solve.FunDep
  ( improveFunDeps,
  )
where

import Aihc.Tc.Constraint (Ct (..))
import Aihc.Tc.Env (ClassInfo (..), FunDep (..), InstanceInfo (..))
import Aihc.Tc.FunDep (atPositions)
import Aihc.Tc.Monad (TcM, getClassInstances, lookupClass)
import Aihc.Tc.Solve.Family (matchTypes)
import Aihc.Tc.Types
import Aihc.Tc.Unify (unifyTypes)
import Aihc.Tc.Zonk (zonkPred)
import Control.Monad (forM_, void, zipWithM_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)

-- | Improve the given dictionary constraints against each other, the givens
-- in scope, and the instances of their classes.
--
-- The result says whether improvement solved a meta variable, which means
-- the constraints are worth another attempt.
improveFunDeps :: [Pred] -> [Ct] -> TcM Bool
improveFunDeps givens constraints = do
  improvable <- catMaybes <$> mapM constraintFunDeps constraints
  if null improvable
    then pure False
    else do
      let siblings = map fst improvable
      before <- mapM (zonkPred . ctPred) siblings
      forM_ improvable (uncurry (improveConstraint givens siblings))
      after <- mapM (zonkPred . ctPred) siblings
      pure (before /= after)

-- | The class of a constraint, when it is a class constraint whose class
-- declares a functional dependency. Every other constraint is left alone.
constraintFunDeps :: Ct -> TcM (Maybe (Ct, ClassInfo))
constraintFunDeps constraint =
  case ctPred constraint of
    ClassPred className _ -> do
      classInfo <- lookupClass className
      pure $ case classInfo of
        Just info | not (null (ciFunDeps info)) -> Just (constraint, info)
        _ -> Nothing
    _ -> pure Nothing

-- | Improve one wanted constraint from every source in turn. A constraint
-- improves against itself trivially, so the sibling list may hold it.
improveConstraint :: [Pred] -> [Ct] -> Ct -> ClassInfo -> TcM ()
improveConstraint givens siblings constraint info = do
  siblingPredicates <- mapM (zonkPred . ctPred) siblings
  forM_ (ciFunDeps info) $ \dependency -> do
    forM_ (givens <> siblingPredicates) $ \other ->
      improveFromPredicate (ciTyCon info) dependency constraint other
    improveFromInstances (ciTyCon info) dependency constraint

-- | Improve a wanted from another class constraint of the same class.
improveFromPredicate :: TyCon -> FunDep -> Ct -> Pred -> TcM ()
improveFromPredicate className dependency constraint other =
  case other of
    ClassPred otherClass otherArguments
      | tyConKey otherClass == tyConKey className -> do
          predicate <- zonkPred (ctPred constraint)
          case predicate of
            ClassPred _ arguments ->
              case agreeTypes Map.empty (determiners dependency arguments) (determiners dependency otherArguments) of
                Just substitution ->
                  improveEqualities
                    (map (substituteMetas substitution) (determined dependency arguments))
                    (map (substituteMetas substitution) (determined dependency otherArguments))
                Nothing -> pure ()
            _ -> pure ()
    _ -> pure ()

-- | Improve a wanted from the head of every instance whose determining
-- parameters it matches.
--
-- The match is one-way: the instance variables stand for the types the
-- wanted names, and a meta variable of the wanted is rigid. Matching it
-- against a concrete instance parameter instead would commit the wanted to
-- an instance that a later solution could contradict.
improveFromInstances :: TyCon -> FunDep -> Ct -> TcM ()
improveFromInstances className dependency constraint = do
  instances <- getClassInstances className
  forM_ instances $ \instanceInfo -> do
    predicate <- zonkPred (ctPred constraint)
    case predicate of
      ClassPred _ arguments
        | Just substitution <-
            matchTypes
              (determiners dependency (iiHead instanceInfo))
              (determiners dependency arguments) ->
            improveEqualities
              (map (applySubst substitution) (determined dependency (iiHead instanceInfo)))
              (determined dependency arguments)
      _ -> pure ()

determiners :: FunDep -> [TcType] -> [TcType]
determiners dependency = atPositions (fdDeterminers dependency)

determined :: FunDep -> [TcType] -> [TcType]
determined dependency = atPositions (fdDetermined dependency)

-- | Equate the determined parameters. Improvement carries no evidence, so
-- the equality is solved by unification alone. Types that do not unify are
-- left to the constraint itself to report as unsolved.
improveEqualities :: [TcType] -> [TcType] -> TcM ()
improveEqualities left right
  | length left /= length right = pure ()
  | otherwise = zipWithM_ improveOne left right
  where
    improveOne leftType rightType
      | leftType == rightType = pure ()
      | otherwise = void (unifyTypes leftType rightType)

-- | Whether two types agree once some meta variables stand for the same
-- type, and the substitution that makes them agree.
--
-- A type variable is rigid here: it is a skolem of the wanted or of a
-- given, so nothing may choose what it stands for. A meta variable is not
-- solved by this match; the substitution only records what agreement would
-- require, so that the determined parameters can be read in its light.
agreeTypes :: Map Unique TcType -> [TcType] -> [TcType] -> Maybe (Map Unique TcType)
agreeTypes substitution left right
  | length left /= length right = Nothing
  | otherwise = foldl step (Just substitution) (zip left right)
  where
    step accumulator (leftType, rightType) =
      accumulator >>= \current -> agreeType current leftType rightType

agreeType :: Map Unique TcType -> TcType -> TcType -> Maybe (Map Unique TcType)
agreeType substitution left right =
  case (substituteMetas substitution left, substituteMetas substitution right) of
    (TcMetaTv unique, other) -> bindMeta substitution unique other
    (other, TcMetaTv unique) -> bindMeta substitution unique other
    (TcTyCon leftTyCon leftArguments, TcTyCon rightTyCon rightArguments)
      | tyConKey leftTyCon == tyConKey rightTyCon ->
          agreeTypes substitution leftArguments rightArguments
    (TcFunTy leftArgument leftResult, TcFunTy rightArgument rightResult) ->
      agreeTypes substitution [leftArgument, leftResult] [rightArgument, rightResult]
    (TcAppTy leftFunction leftArgument, TcAppTy rightFunction rightArgument) ->
      agreeTypes substitution [leftFunction, leftArgument] [rightFunction, rightArgument]
    (leftType, rightType)
      | leftType == rightType -> Just substitution
      | otherwise -> Nothing

bindMeta :: Map Unique TcType -> Unique -> TcType -> Maybe (Map Unique TcType)
bindMeta substitution unique ty
  | TcMetaTv other <- ty, other == unique = Just substitution
  | unique `elem` typeMetas ty = Nothing
  | otherwise = Just (Map.insert unique ty substitution)

-- | Replace the meta variables that a substitution records.
substituteMetas :: Map Unique TcType -> TcType -> TcType
substituteMetas substitution ty
  | Map.null substitution = ty
  | otherwise = go ty
  where
    go current =
      case current of
        TcMetaTv unique -> maybe current go (Map.lookup unique substitution)
        TcTyCon tyCon arguments -> TcTyCon tyCon (map go arguments)
        TcFunTy argument result -> TcFunTy (go argument) (go result)
        TcAppTy function argument -> mkAppTy (go function) (go argument)
        _ -> current

typeMetas :: TcType -> [Unique]
typeMetas ty =
  case ty of
    TcMetaTv unique -> [unique]
    TcTyCon _ arguments -> concatMap typeMetas arguments
    TcFunTy argument result -> typeMetas argument <> typeMetas result
    TcAppTy function argument -> typeMetas function <> typeMetas argument
    TcForAllTy _ body -> typeMetas body
    _ -> []
