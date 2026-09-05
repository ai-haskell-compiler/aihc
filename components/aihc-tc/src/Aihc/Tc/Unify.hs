{-# LANGUAGE OverloadedStrings #-}

-- | Unification of types.
--
-- Handles meta-variable solving with occurs check.
module Aihc.Tc.Unify
  ( unify,
    unifyTypes,
  )
where

import Aihc.Parser.Syntax (SourceSpan (..))
import Aihc.Tc.Constraint (CtOrigin (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Kind (tcTypeKind, unifyKindsAt)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Family (reduceTypeFamilies)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Monad (zipWithM)

-- | Unify two types, recording the solution and emitting an error if
-- they are incompatible.
unify :: SourceSpan -> CtOrigin -> TcType -> TcType -> TcM ()
unify loc origin t1 t2 = do
  t1' <- zonkType t1 >>= reduceTypeFamilies
  t2' <- zonkType t2 >>= reduceTypeFamilies
  result <- unifyTypesAt loc t1' t2'
  case result of
    Right () -> pure ()
    Left (UnificationError left right _ provenance) ->
      emitError loc (UnificationError left right origin provenance)
    Left err -> emitError loc err

-- | Attempt to unify two types, returning an error kind on failure.
unifyTypes :: TcType -> TcType -> TcM (Either TcErrorKind ())
unifyTypes = unifyTypesAt NoSourceSpan

-- | Attempt to unify two types. A kind mismatch is reported at the span.
unifyTypesAt :: SourceSpan -> TcType -> TcType -> TcM (Either TcErrorKind ())
unifyTypesAt _ (TcMetaTv u1) (TcMetaTv u2)
  | u1 == u2 = pure (Right ())
unifyTypesAt loc (TcMetaTv u) ty = unifyMetaTv loc u ty
unifyTypesAt loc ty (TcMetaTv u) = unifyMetaTv loc u ty
unifyTypesAt _ (TcTyVar v1) (TcTyVar v2)
  | v1 == v2 = pure (Right ())
unifyTypesAt loc (TcTyCon tc1 args1) (TcTyCon tc2 args2)
  | tc1 == tc2,
    length args1 == length args2 = do
      results <- zipWithM (unifyTypesAt loc) args1 args2
      pure $ sequence_ results
unifyTypesAt loc (TcFunTy a1 b1) (TcFunTy a2 b2) = do
  r1 <- unifyTypesAt loc a1 a2
  r2 <- unifyTypesAt loc b1 b2
  pure $ r1 >> r2
unifyTypesAt loc (TcAppTy f a) (TcTyCon tc args)
  | not (null args) = do
      r1 <- unifyTypesAt loc f (TcTyCon tc (init args))
      r2 <- unifyTypesAt loc a (last args)
      pure $ r1 >> r2
unifyTypesAt loc (TcTyCon tc args) (TcAppTy f a)
  | not (null args) = do
      r1 <- unifyTypesAt loc (TcTyCon tc (init args)) f
      r2 <- unifyTypesAt loc (last args) a
      pure $ r1 >> r2
unifyTypesAt loc (TcAppTy f1 a1) (TcAppTy f2 a2) = do
  r1 <- unifyTypesAt loc f1 f2
  r2 <- unifyTypesAt loc a1 a2
  pure $ r1 >> r2
-- The function type is the saturated arrow constructor.
unifyTypesAt loc (TcAppTy f a) (TcFunTy argument result) = do
  arrow <- mkKnownTyCon "GHC.Types" "(->)" 2 (KFun KType (KFun KType KType))
  r1 <- unifyTypesAt loc f (TcTyCon arrow [argument])
  r2 <- unifyTypesAt loc a result
  pure $ r1 >> r2
unifyTypesAt loc (TcFunTy argument result) (TcAppTy f a) = do
  arrow <- mkKnownTyCon "GHC.Types" "(->)" 2 (KFun KType (KFun KType KType))
  r1 <- unifyTypesAt loc (TcTyCon arrow [argument]) f
  r2 <- unifyTypesAt loc result a
  pure $ r1 >> r2
unifyTypesAt _ t1 t2 =
  pure $ Left $ UnificationError t1 t2 (UnifyOrigin NoSourceSpan) Nothing

-- | Unify a meta-variable with a type, performing the occurs check.
unifyMetaTv :: SourceSpan -> Unique -> TcType -> TcM (Either TcErrorKind ())
unifyMetaTv loc u ty = do
  ty' <- zonkType ty
  case ty' of
    TcMetaTv u' | u == u' -> pure (Right ())
    _ ->
      if occursIn u ty'
        then pure $ Left $ OccursCheckError (TcMetaTv u) ty'
        else do
          declaredKind <- readMetaTvKind u
          solvedKind <- tcTypeKind ty'
          unifyKindsAt loc declaredKind solvedKind
          writeMetaTv u ty'
          pure (Right ())

-- | Check whether a meta-variable occurs in a type (occurs check).
occursIn :: Unique -> TcType -> Bool
occursIn u = go
  where
    go (TcMetaTv u') = u == u'
    go (TcTyVar _) = False
    go (TcTyCon _ args) = any go args
    go (TcFunTy a b) = go a || go b
    go (TcForAllTy _ body) = go body
    go (TcQualTy preds body) = any goPred preds || go body
    go (TcAppTy f a) = go f || go a

    goPred (ClassPred _ args) = any go args
    goPred (EqPred a b) = go a || go b
    goPred (IParamPred _ payload) = go payload
    goPred (QuantifiedPred variables antecedents consequent) =
      any (go . tvKind) variables || any goPred antecedents || goPred consequent
