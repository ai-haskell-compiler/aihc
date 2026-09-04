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
-- The default list comes from the module @default@ declaration. A module
-- without one uses @(Integer, Double)@.
module Aihc.Tc.Solve.Defaulting
  ( defaultAmbiguousMetas,
    isNumericClassName,
    isStandardClassName,
    standardDefaultTypeNames,
  )
where

import Aihc.Parser.Syntax (SourceSpan (..))
import Aihc.Tc.Constraint (Ct (..), CtOrigin (..), mkWantedCt)
import Aihc.Tc.Env (TyConInfo (..))
import Aihc.Tc.Generalize (predMetaVars)
import Aihc.Tc.Monad (TcM, freshEvVar, getDefaultTypes, lookupTyCon, tcSpeculate, writeMetaTv)
import Aihc.Tc.Solve.Dict (DictResult (..), solveDict)
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), Unique)
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
  zonked <- mapM zonkCtPred constraints
  candidates <- defaultCandidateTypes
  if null candidates
    then pure False
    else do
      let ambiguous = filter (`notElem` keep) (nub (concatMap (predMetaVars . ctPred) zonked))
      results <- mapM (defaultOneMeta candidates zonked) ambiguous
      pure (or results)
  where
    zonkCtPred ct = do
      predicate <- zonkPred (ctPred ct)
      pure (ct {ctPred = predicate})

-- | Default one meta-variable, if its constraint group permits it.
defaultOneMeta :: [TcType] -> [Ct] -> Unique -> TcM Bool
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
-- class constraint applied to the bare variable. A constraint such as
-- @C [v]@, @C v w@, or an unsolved equality blocks defaulting, as does a
-- non-standard class or a group without a numeric class.
defaultableGroup :: Unique -> [Ct] -> Maybe [TyCon]
defaultableGroup unique constraints = do
  classes <- traverse classOfConstraint mentioning
  let names = map tyConName classes
  if any isNumericClassName names && all isStandardClassName names
    then Just (nub classes)
    else Nothing
  where
    mentioning = [ctPred ct | ct <- constraints, unique `elem` predMetaVars (ctPred ct)]

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
  let constraint = mkWantedCt (ClassPred className [ty]) evidence (InstOrigin (tyConName className)) NoSourceSpan
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
