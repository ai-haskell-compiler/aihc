{-# LANGUAGE OverloadedStrings #-}

-- | Ambiguity resolution and defaulting.
--
-- Haskell 2010 section 4.3.4 makes an ambiguous type variable concrete when
-- its constraints permit only one sensible choice. A type variable @v@ is a
-- candidate when every unsolved constraint that mentions @v@ has the form
-- @C v@, at least one @C@ is a numeric class, and every @C@ is a standard
-- class. The solver then takes the first type of the default list that is an
-- instance of every @C@ in the group.
--
-- Haskell 2010 section 4.5.3 reduces a context to head normal form before
-- defaulting runs. The dictionary solver leaves a wanted such as @Eq [v]@
-- unsolved when the context @Eq v@ of the matching instance is stuck on the
-- meta-variable. This module reduces such a wanted through the instance head
-- to @Eq v@ for the group test only. Once the variable is concrete, the
-- caller solves the original wanted again.
--
-- The default list comes from the module @default@ declaration. A module
-- without one uses @(Integer, Double)@.
module Aihc.Tc.Solve.Defaulting
  ( defaultAmbiguousMetas,
    isNumericClassName,
    isStandardClassName,
    standardDefaultTypeNames,
  )
where

import Aihc.Tc.Constraint (Ct (..), CtOrigin (..), mkWantedCt)
import Aihc.Tc.Env (InstanceInfo (..), TyConInfo (..))
import Aihc.Tc.Generalize (predMetaVars)
import Aihc.Tc.Match (matchTypes)
import Aihc.Tc.Monad (TcM, freshEvVar, getClassInstances, getDefaultTypes, lookupTyCon, tcSpeculate, writeMetaTv)
import Aihc.Tc.Solve.Dict (DictResult (..), mostSpecificInstances, solveDict)
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), Unique, applySubstPred)
import Aihc.Tc.Zonk (zonkPred)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Text (Text)

-- | Apply Haskell 2010 defaulting to the ambiguous meta-variables of a set of
-- unsolved constraints.
--
-- @keep@ holds the meta-variables that the enclosing binding still
-- generalizes over, plus those that the environment mentions. Defaulting
-- never touches them, so an inferred @Num a => a -> a@ stays polymorphic.
--
-- The result reports whether any meta-variable got a solution. A caller that
-- gets 'True' must solve its constraints again, because the new solutions can
-- discharge them.
defaultAmbiguousMetas :: [Unique] -> [Ct] -> TcM Bool
defaultAmbiguousMetas keep constraints = do
  zonked <- mapM (zonkPred . ctPred) constraints
  reduced <- concat <$> mapM (reduceToHeadNormalForm reductionDepthLimit) zonked
  candidates <- defaultCandidateTypes
  if null candidates
    then pure False
    else do
      let ambiguous = filter (`notElem` keep) (nub (concatMap predMetaVars reduced))
      results <- mapM (defaultOneMeta candidates reduced) ambiguous
      pure (or results)

-- | Reduce a class constraint to head normal form through the instance that
-- matches it, as Haskell 2010 section 4.5.3 does before defaulting.
--
-- A constraint on a bare meta-variable is already in head normal form. A
-- constraint that exactly one instance matches becomes the instance
-- context under the match substitution, reduced again. Any other
-- constraint stays as it is, so it blocks defaulting as before.
--
-- The reduction is only a view for the group test. It binds no evidence and
-- no kind meta-variable. The depth limit stops an instance chain that does
-- not terminate.
reduceToHeadNormalForm :: Int -> Pred -> TcM [Pred]
reduceToHeadNormalForm depth predicate =
  case predicate of
    ClassPred _ [TcMetaTv _] -> pure [predicate]
    ClassPred className args
      | depth > 0 -> do
          instances <- getClassInstances className
          case mostSpecificInstances args instances of
            [instanceInfo]
              | Just substitution <- matchTypes (iiHead instanceInfo) args ->
                  concat <$> mapM (reduceToHeadNormalForm (depth - 1) . applySubstPred substitution) (iiContext instanceInfo)
            _ -> pure [predicate]
    _ -> pure [predicate]

-- | The number of instance steps that 'reduceToHeadNormalForm' takes at
-- most.
reductionDepthLimit :: Int
reductionDepthLimit = 32

-- | Default one meta-variable, if its constraint group permits it.
defaultOneMeta :: [TcType] -> [Pred] -> Unique -> TcM Bool
defaultOneMeta candidates constraints unique =
  case defaultableGroup unique constraints of
    Nothing -> pure False
    Just classes -> do
      solution <- firstSatisfying candidates classes
      case solution of
        Nothing -> pure False
        Just ty -> do
          writeMetaTv unique ty
          pure True

-- | The classes constraining one meta-variable, when the Haskell 2010 rule
-- allows defaulting it.
--
-- Every constraint that mentions the variable must be a single-parameter
-- class constraint applied to the bare variable. The constraints are in
-- head normal form, so @C [v]@ reaches here only when no instance reduces
-- it. Such a constraint, @C v w@, or an unsolved equality blocks
-- defaulting, as does a non-standard class or a group without a numeric
-- class.
defaultableGroup :: Unique -> [Pred] -> Maybe [TyCon]
defaultableGroup unique constraints = do
  classes <- traverse classOfConstraint mentioning
  let names = map tyConName classes
  if any isNumericClassName names && all isStandardClassName names
    then Just (nub classes)
    else Nothing
  where
    mentioning = [predicate | predicate <- constraints, unique `elem` predMetaVars predicate]

    classOfConstraint predicate =
      case predicate of
        ClassPred className [TcMetaTv argument]
          | argument == unique -> Just className
        _ -> Nothing

-- | The first candidate type that is an instance of every class in the group.
firstSatisfying :: [TcType] -> [TyCon] -> TcM (Maybe TcType)
firstSatisfying [] _ = pure Nothing
firstSatisfying (candidate : rest) classes = do
  ok <- allM (hasInstance candidate) classes
  if ok
    then pure (Just candidate)
    else firstSatisfying rest classes

-- | Whether a type is an instance of a class.
--
-- The trial runs the real dictionary solver so that superclasses and
-- instance contexts count, then discards everything it did.
hasInstance :: TcType -> TyCon -> TcM Bool
hasInstance ty className = tcSpeculate $ do
  evidence <- freshEvVar
  let constraint = mkWantedCt (ClassPred className [ty]) evidence (InstOrigin (tyConName className)) Nothing
  result <- solveDict constraint
  case result of
    DictSolved -> pure True
    DictStuck _ -> pure False

-- | The candidate types that defaulting may choose from.
defaultCandidateTypes :: TcM [TcType]
defaultCandidateTypes = do
  declared <- getDefaultTypes
  case declared of
    Just types -> pure types
    Nothing -> mapMaybe (fmap standardType) <$> mapM lookupTyCon standardDefaultTypeNames
  where
    standardType info = TcTyCon (tciTyCon info) []

-- | The Haskell 2010 default list for a module without a @default@
-- declaration.
standardDefaultTypeNames :: [Text]
standardDefaultTypeNames = ["Integer", "Double"]

-- | The numeric classes of the Haskell 2010 report. A defaultable group must
-- contain at least one of them.
isNumericClassName :: Text -> Bool
isNumericClassName name =
  name `elem` ["Num", "Real", "Integral", "Fractional", "Floating", "RealFrac", "RealFloat"]

-- | The standard classes. Defaulting refuses a group that mentions a class
-- outside this set, so a user class never gets a defaulted argument.
isStandardClassName :: Text -> Bool
isStandardClassName name =
  isNumericClassName name
    || name
      `elem` [ "Eq",
               "Ord",
               "Show",
               "Read",
               "Enum",
               "Bounded",
               "Ix",
               "Functor",
               "Applicative",
               "Alternative",
               "Monad",
               "MonadPlus",
               "MonadFail",
               "Foldable",
               "Traversable",
               "Semigroup",
               "Monoid",
               "IsString"
             ]

allM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
allM _ [] = pure True
allM predicate (x : xs) = do
  ok <- predicate x
  if ok then allM predicate xs else pure False
