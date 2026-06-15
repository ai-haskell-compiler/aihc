-- | Let-generalization.
--
-- For top-level bindings, we generalize over free meta-variables that
-- are not in the environment, and abstract over residual class constraints
-- as dictionary parameters.
module Aihc.Tc.Generalize
  ( generalize,
    generalizeIgnoring,
  )
where

import Aihc.Tc.Monad (TcBinder (..), TcM, freshSkolemTv, getTermEnv)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

-- | Generalize a monotype into a type scheme.
--
-- Collects free meta-variables in the type (but not in the environment),
-- promotes them to universally quantified type variables, and wraps
-- any residual predicates.
generalize :: TcType -> [Pred] -> TcM TypeScheme
generalize = generalizeIgnoring []

-- | Generalize a monotype while ignoring the named environment binders.
--
-- This is used for recursive local binding groups: the group's placeholder
-- binders are in scope while the group is checked, but they are not part of
-- the outer environment that should block generalization.
generalizeIgnoring :: [T.Text] -> TcType -> [Pred] -> TcM TypeScheme
generalizeIgnoring ignoredNames ty preds = do
  env <- getTermEnv
  envMetaVars <-
    concat
      <$> mapM
        binderMetaVars
        (filter (not . ignoredBinder) (Map.elems env))
  ty' <- zonkType ty
  preds' <- mapM zonkPred preds
  let freeMetaVars = collectMetaVars ty' ++ concatMap predMetaVars preds'
      uniqueMetaVars = filter (`notElem` envMetaVars) (nubOrd freeMetaVars)
  -- Create a type variable for each free meta-variable, naming them
  -- sequentially starting from 'a'.
  tvs <- sequence [metaToTyVar i u | (i, u) <- zip [0 ..] uniqueMetaVars]
  let subst = zip uniqueMetaVars (map TcTyVar tvs)
  let ty'' = substMetas subst ty'
  let preds'' = map (substMetasPred subst) preds'
  pure (ForAll tvs preds'' ty'')
  where
    ignoredBinder binder =
      binderName binder `elem` ignoredNames

binderName :: TcBinder -> T.Text
binderName (TcIdBinder name _ _) = name
binderName (TcMonoIdBinder name _) = name

-- | Collect free meta-variable uniques from a type.
collectMetaVars :: TcType -> [Unique]
collectMetaVars (TcMetaTv u) = [u]
collectMetaVars (TcTyVar _) = []
collectMetaVars (TcTyCon _ args) = concatMap collectMetaVars args
collectMetaVars (TcFunTy a b) = collectMetaVars a ++ collectMetaVars b
collectMetaVars (TcForAllTy _ body) = collectMetaVars body
collectMetaVars (TcQualTy ps body) = concatMap predMetaVars ps ++ collectMetaVars body
collectMetaVars (TcAppTy f a) = collectMetaVars f ++ collectMetaVars a

-- | Collect free meta-variable uniques from a predicate.
predMetaVars :: Pred -> [Unique]
predMetaVars (ClassPred _ args) = concatMap collectMetaVars args
predMetaVars (EqPred a b) = collectMetaVars a ++ collectMetaVars b

-- | Create a type variable from a meta-variable unique, using a
-- sequential index for naming (so the first generalized variable is
-- 'a', the second 'b', etc.).
metaToTyVar :: Int -> Unique -> TcM TyVarId
metaToTyVar idx _u = freshSkolemTv (mkName idx)
  where
    mkName i =
      let c = toEnum (fromEnum 'a' + i `mod` 26)
       in if i < 26
            then T.singleton c
            else T.pack [c] <> T.pack (show (i `div` 26))

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

-- | Substitute meta-variables in a predicate.
substMetasPred :: [(Unique, TcType)] -> Pred -> Pred
substMetasPred subst (ClassPred cls args) = ClassPred cls (map (substMetas subst) args)
substMetasPred subst (EqPred a b) = EqPred (substMetas subst a) (substMetas subst b)

-- | Zonk a predicate (local copy to avoid circular imports).
zonkPred :: Pred -> TcM Pred
zonkPred (ClassPred cls args) = ClassPred cls <$> mapM zonkType args
zonkPred (EqPred a b) = EqPred <$> zonkType a <*> zonkType b

binderMetaVars :: TcBinder -> TcM [Unique]
binderMetaVars (TcIdBinder _ (ForAll _ preds ty) _) =
  do
    ty' <- zonkType ty
    preds' <- mapM zonkPred preds
    pure (collectMetaVars ty' ++ concatMap predMetaVars preds')
binderMetaVars (TcMonoIdBinder _ ty) =
  collectMetaVars <$> zonkType ty

-- | Remove duplicates from an ordered list.
nubOrd :: (Ord a) => [a] -> [a]
nubOrd = go []
  where
    go _ [] = []
    go seen (x : xs)
      | x `elem` seen = go seen xs
      | otherwise = x : go (x : seen) xs
