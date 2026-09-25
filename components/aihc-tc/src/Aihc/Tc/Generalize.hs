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
    generalizeAndCommitWithInterior,
    generalizeGroupAndCommitIgnoring,
    environmentMetaVars,
    collectMetaVars,
    predMetaVars,
  )
where

import Aihc.Tc.Kind (defaultKindMetas, deferKindMetas)
import Aihc.Tc.Monad (TcBinder (..), TcM, deferKindMeta, freshSkolemTv, getKinds, getMetaTermEnv, getPolyKinds, readMetaTv, readMetaTvKind, writeMetaTv)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Monad (forM_, void, when)
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
  fst <$> generalizeIgnoringWithSubst ignoredKeys [] ty preds

-- | Generalize while ignoring selected binders, then write the generalized
-- substitutions back to the meta store.
generalizeAndCommitIgnoring :: Set.Set TcTermKey -> TcType -> [Pred] -> TcM TypeScheme
generalizeAndCommitIgnoring ignoredKeys ty preds = do
  (scheme, subst) <- generalizeIgnoringWithSubst ignoredKeys [] ty preds
  forM_ subst (uncurry writeMetaTv)
  pure scheme

-- | Generalize a monotype, and also quantify the meta-variables of some
-- interior types that the monotype does not mention. Then write the
-- substitution back to the meta store.
--
-- A rewrite rule needs this. An occurrence inside a side of a rule can
-- have a type variable that the type of the rule does not mention, for
-- example the intermediate type of @re (re s)@. The rule must match at
-- each such type, so the type variable becomes a type binder of the rule.
-- The type variables of the monotype come first in the scheme.
generalizeAndCommitWithInterior :: [TcType] -> TcType -> [Pred] -> TcM TypeScheme
generalizeAndCommitWithInterior interior ty preds = do
  (scheme, subst) <- generalizeIgnoringWithSubst Set.empty interior ty preds
  forM_ subst (uncurry writeMetaTv)
  pure scheme

-- | Generalize the bindings of one recursive group over one shared set of
-- type variables, then write the substitution back to the meta store.
--
-- The bindings of a group share meta-variables. A separate generalization
-- of each binding would turn a shared meta-variable into a type variable
-- of the first binding, and the later bindings would then mention a type
-- variable that they do not quantify. Each scheme quantifies the shared
-- type variables that its own type or predicates mention.
generalizeGroupAndCommitIgnoring :: Set.Set TcTermKey -> [Unique] -> [(TcType, [Pred])] -> TcM [TypeScheme]
generalizeGroupAndCommitIgnoring ignoredKeys monoMetaVars bindings = do
  envMetaVars <- (++ monoMetaVars) <$> environmentMetaVars ignoredKeys
  zonked <- mapM zonkBinding bindings
  forM_ zonked (uncurry (defaultRuntimeRepMetas envMetaVars))
  zonked' <- mapM zonkBinding zonked
  let bindingMetaVars = [nubOrd (collectMetaVars ty ++ concatMap predMetaVars preds) | (ty, preds) <- zonked']
      uniqueMetaVars = filter (`notElem` envMetaVars) (nubOrd (concat bindingMetaVars))
  settleKinds <- settleKindsNow
  when settleKinds (mapM_ defaultMetaKind uniqueMetaVars)
  tvs <- metaVarsToTyVars settleKinds uniqueMetaVars
  let subst = zip uniqueMetaVars (map TcTyVar tvs)
  forM_ subst (uncurry writeMetaTv)
  pure
    [ Scheme [tv | (unique, tv) <- zip uniqueMetaVars tvs, unique `elem` metaVars] [] (map (substMetasPred subst) preds) (substMetas subst ty)
    | ((ty, preds), metaVars) <- zip zonked' bindingMetaVars
    ]
  where
    zonkBinding (ty, preds) = (,) <$> zonkType ty <*> mapM zonkPred preds

generalizeIgnoringWithSubst :: Set.Set TcTermKey -> [TcType] -> TcType -> [Pred] -> TcM (TypeScheme, [(Unique, TcType)])
generalizeIgnoringWithSubst ignoredKeys interior ty preds = do
  envMetaVars <- environmentMetaVars ignoredKeys
  ty' <- zonkType ty
  preds' <- mapM zonkPred preds
  interior' <- mapM zonkType interior
  defaultRuntimeRepMetas envMetaVars (foldr TcFunTy ty' interior') preds'
  ty'' <- zonkType ty'
  preds'' <- mapM zonkPred preds'
  interior'' <- mapM zonkType interior'
  let freeMetaVars = collectMetaVars ty'' ++ concatMap predMetaVars preds'' ++ concatMap collectMetaVars interior''
      uniqueFreeMetaVars = nubOrd freeMetaVars
      uniqueMetaVars = filter (`notElem` envMetaVars) uniqueFreeMetaVars
  -- Only a quantified meta-variable needs a fixed kind now. A meta-variable
  -- of the environment, such as the type of a lambda-bound variable, keeps
  -- its open kind until a use fixes it; an unlifted use is still possible.
  settleKinds <- settleKindsNow
  when settleKinds (mapM_ defaultMetaKind uniqueMetaVars)
  -- Create a type variable for each free meta-variable, naming them
  -- sequentially starting from 'a'.
  tvs <- metaVarsToTyVars settleKinds uniqueMetaVars
  let subst = zip uniqueMetaVars (map TcTyVar tvs)
  let quantifiedTy = substMetas subst ty''
  let quantifiedPreds = map (substMetasPred subst) preds''
  pure (Scheme tvs [] quantifiedPreds quantifiedTy, subst)

-- | Solve every RuntimeRep meta-variable that quantification would capture.
--
-- GHC never generalizes over a RuntimeRep variable, so @seqAlias = seq@ gets
-- the type @a -> b -> b@ instead of a representation-polymorphic scheme. The
-- representation variables of a binding hide in the /kinds/ of the type's
-- meta-variables, so we walk those kinds transitively and default each
-- unsolved RuntimeRep meta-variable to LiftedRep. Meta-variables the
-- environment already mentions are left alone: they belong to an enclosing
-- binding that decides their fate.
defaultRuntimeRepMetas :: [Unique] -> TcType -> [Pred] -> TcM ()
defaultRuntimeRepMetas envMetaVars ty preds = do
  reachable <- reachableMetaVars (collectMetaVars ty ++ concatMap predMetaVars preds)
  mapM_ defaultOne (filter (`notElem` envMetaVars) reachable)
  where
    defaultOne unique = do
      solution <- readMetaTv unique
      case solution of
        Just {} -> pure ()
        Nothing -> do
          kinds <- getKinds
          kind <- zonkType =<< readMetaTvKind unique
          case kind of
            KRuntimeRep -> writeMetaTv unique (liftedRep kinds)
            _ -> pure ()

-- | The meta-variables reachable from a set of roots, following the kind of
-- each meta-variable. A type such as @a -> b -> b@ does not mention the
-- RuntimeRep variable of @b@ anywhere except in @b@'s kind.
reachableMetaVars :: [Unique] -> TcM [Unique]
reachableMetaVars = go []
  where
    go seen [] = pure (reverse seen)
    go seen (unique : rest)
      | unique `elem` seen = go seen rest
      | otherwise = do
          kind <- zonkType =<< readMetaTvKind unique
          go (unique : seen) (collectMetaVars kind ++ rest)

-- | Meta-variables that the environment mentions. Generalization does not
-- quantify over them. The ignored binders are not part of the environment.
environmentMetaVars :: Set.Set TcTermKey -> TcM [Unique]
environmentMetaVars ignoredKeys = do
  env <- getMetaTermEnv
  nubOrd . concat
    <$> mapM
      binderMetaVars
      [binder | (key, binder) <- Map.toList env, key `Set.notMember` ignoredKeys]

-- | Collect free meta-variable uniques from a type.
collectMetaVars :: TcType -> [Unique]
collectMetaVars (TcMetaTv u) = [u]
collectMetaVars TcArrowTy = []
collectMetaVars (TcTyLit _) = []
collectMetaVars (TcTyVar _) = []
collectMetaVars (TcTyCon _ args) = concatMap collectMetaVars args
-- The kind arguments are kinds. A kind meta is not quantified: it is
-- settled with the other kinds when the module is finalized.
collectMetaVars (TcKindedTyCon _ _) = []
collectMetaVars (TcFunTy a b) = collectMetaVars a ++ collectMetaVars b
collectMetaVars (TcForAllTy _ body) = collectMetaVars body
collectMetaVars (TcQualTy ps body) = concatMap predMetaVars ps ++ collectMetaVars body
collectMetaVars (TcAppTy f a) = collectMetaVars f ++ collectMetaVars a

-- | Collect free meta-variable uniques from a predicate.
predMetaVars :: Pred -> [Unique]
predMetaVars (ClassPred _ args) = concatMap collectMetaVars args
predMetaVars (EqPred a b) = collectMetaVars a ++ collectMetaVars b
predMetaVars (IParamPred _ payload) = collectMetaVars payload
predMetaVars (IrredPred constraint) = collectMetaVars constraint
predMetaVars (QuantifiedPred variables antecedents consequent) =
  concatMap (collectMetaVars . tvKind) variables
    ++ concatMap predMetaVars antecedents
    ++ predMetaVars consequent

-- | Create a type variable from a meta-variable unique, using a
-- sequential index for naming (so the first generalized variable is
-- 'a', the second 'b', etc.).
metaVarsToTyVars :: Bool -> [Unique] -> TcM [TyVarId]
metaVarsToTyVars settleKinds uniques = mapM makeTyVar (zip [0 ..] uniques)
  where
    makeTyVar (index, unique) = do
      kind <- readMetaTvKind unique >>= (if settleKinds then defaultKindMetas else deferKindMetas deferKindMeta)
      rawTyVar <- freshSkolemTv (mkName index)
      pure (setTyVarKind kind rawTyVar)

    mkName i =
      let c = toEnum (fromEnum 'a' + i `mod` 26)
       in if i < 26
            then T.singleton c
            else T.pack [c] <> T.pack (show (i `div` 26))

defaultMetaKind :: Unique -> TcM ()
defaultMetaKind unique = do
  kind <- readMetaTvKind unique
  -- defaultKindMetas writes each solution to the meta-variable store.
  void (defaultKindMetas kind)

-- | Whether the kinds of the meta-variables a binding quantifies over
-- must be settled before they are quantified.
--
-- Without PolyKinds an open kind defaults to 'Type' here. With it the
-- kind is left open instead. A binding local to a kind-polymorphic
-- instance is used at that instance's own kind, which need not be 'Type',
-- and fixing the kind here would make the binding unusable in the very
-- body it belongs to; FC has no way to bind a kind variable of a local
-- scheme, so quantifying over it is not an option either. The kind is
-- settled once the component is checked, by
-- 'Aihc.Tc.Monad.takeDeferredKindMetas'.
settleKindsNow :: TcM Bool
settleKindsNow = not <$> getPolyKinds

-- | Substitute meta-variables with their corresponding type variables.
substMetas :: [(Unique, TcType)] -> TcType -> TcType
substMetas subst = go
  where
    go (TcMetaTv u) = case lookup u subst of
      Just ty -> ty
      Nothing -> TcMetaTv u
    go TcArrowTy = TcArrowTy
    go ty@(TcTyLit _) = ty
    go (TcTyVar tv) = TcTyVar tv
    go (TcTyCon tc args) = TcTyCon tc (map go args)
    go (TcKindedTyCon tc kindArgs) = TcKindedTyCon tc (map go kindArgs)
    go (TcFunTy a b) = TcFunTy (go a) (go b)
    go (TcForAllTy tv body) = TcForAllTy tv (go body)
    go (TcQualTy ps body) = TcQualTy (map (substMetasPred subst) ps) (go body)
    go (TcAppTy f a) = mkAppTy (go f) (go a)

-- | Substitute meta-variables in a predicate.
substMetasPred :: [(Unique, TcType)] -> Pred -> Pred
substMetasPred subst (ClassPred cls args) = ClassPred cls (map (substMetas subst) args)
substMetasPred subst (EqPred a b) = EqPred (substMetas subst a) (substMetas subst b)
substMetasPred subst (IParamPred name payload) = IParamPred name (substMetas subst payload)
substMetasPred subst (IrredPred constraint) = IrredPred (substMetas subst constraint)
substMetasPred subst (QuantifiedPred variables antecedents consequent) =
  QuantifiedPred
    (map (\variable -> setTyVarKind (substMetas subst (tvKind variable)) variable) variables)
    (map (substMetasPred subst) antecedents)
    (substMetasPred subst consequent)

-- | Zonk a predicate (local copy to avoid circular imports).
zonkPred :: Pred -> TcM Pred
zonkPred (ClassPred cls args) = ClassPred cls <$> mapM zonkType args
zonkPred (EqPred a b) = EqPred <$> zonkType a <*> zonkType b
zonkPred (IParamPred name payload) = IParamPred name <$> zonkType payload
zonkPred (IrredPred constraint) = IrredPred <$> zonkType constraint
zonkPred (QuantifiedPred variables antecedents consequent) =
  QuantifiedPred
    <$> mapM zonkVariable variables
    <*> mapM zonkPred antecedents
    <*> zonkPred consequent
  where
    zonkVariable variable = setTyVarKind <$> zonkType (tvKind variable) <*> pure variable

-- | The meta-variables of one binder after zonking. Zonking only replaces
-- meta-variables, so a binder without any needs no zonk. Most binders in the
-- environment are closed top-level schemes.
binderMetaVars :: TcBinder -> TcM [Unique]
binderMetaVars (TcIdBinder (ForAll _ preds ty) _)
  | not (typeMentionsMeta ty) && not (any predicateMentionsMeta preds) = pure []
  | otherwise = do
      ty' <- zonkType ty
      preds' <- mapM zonkPred preds
      pure (collectMetaVars ty' ++ concatMap predMetaVars preds')
binderMetaVars (TcMonoIdBinder ty)
  | not (typeMentionsMeta ty) = pure []
  | otherwise = collectMetaVars <$> zonkType ty

-- | Remove duplicates from an ordered list.
nubOrd :: (Ord a) => [a] -> [a]
nubOrd = go []
  where
    go _ [] = []
    go seen (x : xs)
      | x `elem` seen = go seen xs
      | otherwise = x : go (x : seen) xs
