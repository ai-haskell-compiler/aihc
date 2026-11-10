-- | Matching of the kinds of instance and class variables.
module Aihc.Tc.Solve.KindMatch
  ( matchInstanceKinds,
    matchKinds,
  )
where

import Aihc.Tc.Kind (bindKindMeta, tcTypeKind, zonkKind)
import Aihc.Tc.Monad (TcM)
import Aihc.Tc.Types
import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | Include implicit kind arguments in instance evidence.
--
-- Matching also fixes the kind metas that the instance determines. A wanted
-- can reach the solver before its kinds are settled: a type argument
-- instantiated from a poly-kinded signature carries a kind meta until
-- something forces it, and choosing the instance is what forces it.
matchInstanceKinds :: [TyVarId] -> Map Unique TcType -> TcM (Maybe (Map Unique TcType))
matchInstanceKinds variables substitution = do
  matched <- foldM extend (Just (substitution, [])) variables
  case matched of
    Nothing -> pure Nothing
    Just (final, kindMetas) -> do
      mapM_ (uncurry bindKindMeta) kindMetas
      pure (Just final)
  where
    extend Nothing _ = pure Nothing
    extend (Just (current, kindMetas)) variable = case Map.lookup (tvUnique variable) current of
      Nothing -> pure (Just (current, kindMetas))
      Just target -> do
        targetKind <- tcTypeKind target >>= zonkKind
        patternKind <- zonkKind (tvKind variable)
        pure $ do
          (inferred, metas) <- matchKinds patternKind targetKind
          merged <- foldM merge current (Map.toList inferred)
          pure (merged, kindMetas <> metas)
    merge current (key, ty) = case Map.lookup key current of
      Just existing | existing /= ty -> Nothing
      _ -> Just (Map.insert key ty current)

-- | Match an instance variable's kind against the kind of the type the
-- instance head matched it with.
--
-- A variable in either kind stands for an implicit kind argument of the
-- instance, and an unsolved meta stands for a kind that the wanted has not
-- fixed yet: the match returns the binding that settles it rather than
-- failing. Both kinds must already be zonked.
matchKinds :: TcType -> TcType -> Maybe (Map Unique TcType, [(Unique, TcType)])
matchKinds = go (Map.empty, [])
  where
    go (substitution, metas) patternKind targetKind =
      case (patternKind, targetKind) of
        (TcTyVar variable, _) ->
          case Map.lookup (tvUnique variable) substitution of
            Nothing -> Just (Map.insert (tvUnique variable) targetKind substitution, metas)
            Just existing
              | existing == targetKind -> Just (substitution, metas)
              | otherwise -> Nothing
        (_, TcMetaTv unique) -> Just (substitution, metas <> [(unique, patternKind)])
        (TcMetaTv unique, _) -> Just (substitution, metas <> [(unique, targetKind)])
        (TcTyCon tyCon arguments, TcTyCon targetTyCon targetArguments)
          | tyCon == targetTyCon,
            length arguments == length targetArguments ->
              foldM (uncurry . go) (substitution, metas) (zip arguments targetArguments)
        (TcFunTy argument result, TcFunTy targetArgument targetResult) ->
          go (substitution, metas) argument targetArgument
            >>= \next -> go next result targetResult
        (TcAppTy function argument, TcAppTy targetFunction targetArgument) ->
          go (substitution, metas) function targetFunction
            >>= \next -> go next argument targetArgument
        _
          | patternKind == targetKind -> Just (substitution, metas)
          | otherwise -> Nothing
