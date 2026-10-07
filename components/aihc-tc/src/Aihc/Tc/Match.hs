-- | Pure structural operations on checked types.
--
-- Only pattern variables can receive substitutions.
-- This module does not solve constraints or reduce type families.
module Aihc.Tc.Match (matchTypes) where

import Aihc.Tc.Types
import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | Match pattern types against target types. The type variables of the
-- patterns are the pattern variables.
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
matchOne subst (TcKindedTyCon tc kindArgs, TcKindedTyCon targetTc targetKindArgs)
  | tc == targetTc,
    length kindArgs == length targetKindArgs =
      foldM matchOne subst (zip kindArgs targetKindArgs)
-- A bare constructor that no use site kinded agrees with every kinding.
matchOne subst (TcKindedTyCon tc _, TcTyCon targetTc [])
  | tc == targetTc = Just subst
matchOne subst (TcTyCon tc [], TcKindedTyCon targetTc _)
  | tc == targetTc = Just subst
matchOne subst (TcFunTy a b, TcFunTy targetA targetB) =
  matchOne subst (a, targetA) >>= \subst' -> matchOne subst' (b, targetB)
matchOne subst (TcAppTy f a, TcAppTy targetF targetA) =
  matchOne subst (f, targetF) >>= \subst' -> matchOne subst' (a, targetA)
-- A pattern application @t m@ matches a saturated constructor application
-- @CmdM m@: the head takes all arguments but the last one.
matchOne subst (TcAppTy f a, TcTyCon targetTc targetArgs@(_ : _)) =
  matchOne subst (f, TcTyCon targetTc (init targetArgs)) >>= \subst' -> matchOne subst' (a, last targetArgs)
-- A constructor pattern @Rec l r@ matches the same application written as
-- a spine, as a use site that kinds the constructor writes it: the last
-- argument against the last application.
matchOne subst (TcTyCon tc args@(_ : _), TcAppTy targetF targetA) =
  matchOne subst (TcTyCon tc (init args), targetF) >>= \subst' -> matchOne subst' (last args, targetA)
-- Two quantified types match when their bodies do, with the pattern's
-- bound variable standing for the target's. A default signature and its
-- method share @forall a. f a -> e (g a)@ with different binders. The bound
-- variable must not escape into the binding of a pattern variable.
matchOne subst (TcForAllTy tv body, TcForAllTy targetTv targetBody) = do
  inner <- matchOne (Map.insert (tvUnique tv) (TcTyVar targetTv) subst) (body, targetBody)
  let result = maybe (Map.delete (tvUnique tv) inner) (\previous -> Map.insert (tvUnique tv) previous inner) (Map.lookup (tvUnique tv) subst)
      escapes = or [typeMentionsTyVar targetTv bound | (unique, bound) <- Map.toList result, Map.lookup unique subst /= Just bound]
  if escapes then Nothing else Just result
matchOne subst (patternTy, targetTy)
  | patternTy == targetTy = Just subst
  | otherwise = Nothing
