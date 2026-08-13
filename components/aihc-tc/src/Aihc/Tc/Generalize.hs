-- | Let-generalization.
--
-- For top-level bindings, we generalize over free meta-variables that
-- are not in the environment, and abstract over residual class constraints
-- as dictionary parameters.
module Aihc.Tc.Generalize
  ( generalize,
    generalizeIgnoring,
    generalizeAndCommit,
    generalizeAndCommitIgnoring,
    predMetaVars,
  )
where

import Aihc.Tc.Monad (TcBinder (..), TcM, TcTermKey, freshSkolemTv, getTermEnv, readMetaTvKind, writeMetaTv)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Monad (foldM, forM_)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T

-- | Generalize a monotype into a type scheme.
--
-- Collects free meta-variables in the type (but not in the environment),
-- promotes them to universally quantified type variables, and wraps
-- any residual predicates.
generalize :: TcType -> [Pred] -> TcM TypeScheme
generalize = generalizeIgnoring Set.empty

-- | Generalize a monotype and commit generalized meta-variables.
--
-- Pending annotations may still point at the original meta-variables. Once
-- those metas become quantified type variables, the meta store must know that
-- replacement so later zonking cannot expose raw type-checking metavariables.
generalizeAndCommit :: TcType -> [Pred] -> TcM TypeScheme
generalizeAndCommit = generalizeAndCommitIgnoring Set.empty

-- | Generalize a monotype while ignoring the selected environment binders.
--
-- This is used for recursive local binding groups: the group's placeholder
-- binders are in scope while the group is checked, but they are not part of
-- the outer environment that should block generalization.
generalizeIgnoring :: Set.Set TcTermKey -> TcType -> [Pred] -> TcM TypeScheme
generalizeIgnoring ignoredKeys ty preds =
  fst <$> generalizeIgnoringWithSubst ignoredKeys ty preds

-- | Generalize while ignoring selected binders, then write the generalized
-- substitutions back to the meta store.
generalizeAndCommitIgnoring :: Set.Set TcTermKey -> TcType -> [Pred] -> TcM TypeScheme
generalizeAndCommitIgnoring ignoredKeys ty preds = do
  (scheme, subst) <- generalizeIgnoringWithSubst ignoredKeys ty preds
  forM_ subst (uncurry writeMetaTv)
  pure scheme

generalizeIgnoringWithSubst :: Set.Set TcTermKey -> TcType -> [Pred] -> TcM (TypeScheme, [(Unique, TcType)])
generalizeIgnoringWithSubst ignoredKeys ty preds = do
  env <- getTermEnv
  directEnvMetaVars <-
    concat
      <$> mapM
        binderMetaVars
        [binder | (key, binder) <- Map.toList env, key `Set.notMember` ignoredKeys]
  ty' <- zonkType ty
  preds' <- mapM zonkPred preds
  envMetaVars <- expandMetaKindDependencies (nubOrd directEnvMetaVars)
  let freeMetaVars = collectMetaVars ty' ++ concatMap predMetaVars preds'
  expandedMetaVars <- expandMetaKindDependencies (nubOrd freeMetaVars)
  orderedMetaVars <- representationMetasFirst expandedMetaVars
  let uniqueMetaVars = filter (`notElem` envMetaVars) orderedMetaVars
  -- Create a type variable for each free meta-variable, naming them
  -- sequentially starting from 'a'.
  tvs <- metaVarsToTyVars uniqueMetaVars
  let subst = zip uniqueMetaVars (map TcTyVar tvs)
  let ty'' = substMetas subst ty'
  let preds'' = map (substMetasPred subst) preds'
  pure (ForAll tvs preds'' ty'', subst)

-- | Collect free meta-variable uniques from a type.
collectMetaVars :: TcType -> [Unique]
collectMetaVars (TcMetaTv u) = [u]
collectMetaVars (TcTyVar _) = []
collectMetaVars (TcTyCon _ args) = concatMap collectMetaVars args
collectMetaVars (TcFunTy a b) = collectMetaVars a ++ collectMetaVars b
collectMetaVars (TcForAllTy _ body) = collectMetaVars body
collectMetaVars (TcQualTy ps body) = concatMap predMetaVars ps ++ collectMetaVars body
collectMetaVars (TcAppTy f a) = collectMetaVars f ++ collectMetaVars a
collectMetaVars (TcBuiltinTyCon _ _ arguments) = concatMap collectMetaVars arguments

-- | Collect free meta-variable uniques from a predicate.
predMetaVars :: Pred -> [Unique]
predMetaVars (ClassPred _ args) = concatMap collectMetaVars args
predMetaVars (EqPred a b) = collectMetaVars a ++ collectMetaVars b

-- | Create a type variable from a meta-variable unique, using a
-- sequential index for naming (so the first generalized variable is
-- 'a', the second 'b', etc.).
metaVarsToTyVars :: [Unique] -> TcM [TyVarId]
metaVarsToTyVars uniques = reverse . fst <$> foldM addTyVar ([], Map.empty) (zip [0 ..] uniques)
  where
    addTyVar (reversedTyVars, replacements) (index, unique) = do
      kind <- substituteMetaRuntimeReps replacements <$> readMetaTvKind unique
      rawTyVar <- freshSkolemTv (mkName index)
      let tyVar = setTyVarKind kind rawTyVar
      pure (tyVar : reversedTyVars, Map.insert unique tyVar replacements)

    mkName i =
      let c = toEnum (fromEnum 'a' + i `mod` 26)
       in if i < 26
            then T.singleton c
            else T.pack [c] <> T.pack (show (i `div` 26))

expandMetaKindDependencies :: [Unique] -> TcM [Unique]
expandMetaKindDependencies = foldM addWithDependencies []
  where
    addWithDependencies accumulated unique
      | unique `elem` accumulated = pure accumulated
      | otherwise = do
          kind <- readMetaTvKind unique
          withDependencies <- foldM addWithDependencies accumulated (runtimeRepMetasInKind kind)
          pure (withDependencies <> [unique])

representationMetasFirst :: [Unique] -> TcM [Unique]
representationMetasFirst uniques = do
  classified <- mapM classify uniques
  let representationMetas = [unique | (unique, True) <- classified]
      otherMetas = [unique | (unique, False) <- classified]
  pure (representationMetas <> otherMetas)
  where
    classify unique = do
      kind <- readMetaTvKind unique
      pure (unique, kind == KRuntimeRep)

runtimeRepMetasInKind :: Kind -> [Unique]
runtimeRepMetasInKind kind =
  case kind of
    KTYPE runtimeRep -> runtimeRepMetas runtimeRep
    KFun argument result -> runtimeRepMetasInKind argument <> runtimeRepMetasInKind result
    _ -> []
  where
    runtimeRepMetas runtimeRepresentation =
      case runtimeRepresentation of
        RuntimeRepMeta unique -> [unique]
        TupleRep reps -> concatMap runtimeRepMetas reps
        SumRep reps -> concatMap runtimeRepMetas reps
        _ -> []

substituteMetaRuntimeReps :: Map.Map Unique TyVarId -> Kind -> Kind
substituteMetaRuntimeReps replacements kind =
  case kind of
    KTYPE runtimeRep -> KTYPE (go runtimeRep)
    KFun argument result ->
      KFun
        (substituteMetaRuntimeReps replacements argument)
        (substituteMetaRuntimeReps replacements result)
    _ -> kind
  where
    go runtimeRep =
      case runtimeRep of
        RuntimeRepMeta unique ->
          maybe runtimeRep (RuntimeRepVar . tvUnique) (Map.lookup unique replacements)
        TupleRep reps -> TupleRep (map go reps)
        SumRep reps -> SumRep (map go reps)
        _ -> runtimeRep

-- | Substitute meta-variables with their corresponding type variables.
substMetas :: [(Unique, TcType)] -> TcType -> TcType
substMetas subst = go
  where
    go (TcMetaTv u) = case lookup u subst of
      Just ty -> ty
      Nothing -> TcMetaTv u
    go (TcTyVar tv) = TcTyVar tv
    go (TcTyCon tc args) = TcTyCon tc (map go args)
    go (TcFunTy a b) = TcFunTy (go a) (go b)
    go (TcForAllTy tv body) = TcForAllTy tv (go body)
    go (TcQualTy ps body) = TcQualTy (map (substMetasPred subst) ps) (go body)
    go (TcAppTy f a) = TcAppTy (go f) (go a)
    go (TcBuiltinTyCon name arity arguments) = TcBuiltinTyCon name arity (map go arguments)

-- | Substitute meta-variables in a predicate.
substMetasPred :: [(Unique, TcType)] -> Pred -> Pred
substMetasPred subst (ClassPred cls args) = ClassPred cls (map (substMetas subst) args)
substMetasPred subst (EqPred a b) = EqPred (substMetas subst a) (substMetas subst b)

-- | Zonk a predicate (local copy to avoid circular imports).
zonkPred :: Pred -> TcM Pred
zonkPred (ClassPred cls args) = ClassPred cls <$> mapM zonkType args
zonkPred (EqPred a b) = EqPred <$> zonkType a <*> zonkType b

binderMetaVars :: TcBinder -> TcM [Unique]
binderMetaVars (TcIdBinder (ForAll _ preds ty) _) =
  do
    ty' <- zonkType ty
    preds' <- mapM zonkPred preds
    pure (collectMetaVars ty' ++ concatMap predMetaVars preds')
binderMetaVars (TcMonoIdBinder ty) =
  collectMetaVars <$> zonkType ty

-- | Remove duplicates from an ordered list.
nubOrd :: (Ord a) => [a] -> [a]
nubOrd = go []
  where
    go _ [] = []
    go seen (x : xs)
      | x `elem` seen = go seen xs
      | otherwise = x : go (x : seen) xs
