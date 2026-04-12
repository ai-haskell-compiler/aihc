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
import Aihc.Tc.Monad
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)

-- | Unify two types, recording the solution and emitting an error if
-- they are incompatible.
unify :: SourceSpan -> CtOrigin -> TcType -> TcType -> TcM ()
unify loc origin t1 t2 = do
  t1' <- zonkType t1
  t2' <- zonkType t2
  result <- unifyTypes t1' t2'
  case result of
    Right () -> pure ()
    Left err -> emitError loc err
  where
    _ = origin -- carried for future error-reporting enhancement

-- | Attempt to unify two types, returning an error kind on failure.
unifyTypes :: TcType -> TcType -> TcM (Either TcErrorKind ())
unifyTypes (TcMetaTv u1) (TcMetaTv u2)
  | u1 == u2 = pure (Right ())
unifyTypes (TcMetaTv u) ty = unifyMetaTv u ty
unifyTypes ty (TcMetaTv u) = unifyMetaTv u ty
unifyTypes (TcTyVar v1) (TcTyVar v2)
  | v1 == v2 = pure (Right ())
unifyTypes (TcTyCon tc1 args1) (TcTyCon tc2 args2)
  | tc1 == tc2,
    length args1 == length args2 = do
      results <- mapM (uncurry unifyTypes) (zip args1 args2)
      pure $ sequence_ results
unifyTypes (TcFunTy a1 b1) (TcFunTy a2 b2) = do
  r1 <- unifyTypes a1 a2
  r2 <- unifyTypes b1 b2
  pure $ r1 >> r2
unifyTypes (TcAppTy f1 a1) (TcAppTy f2 a2) = do
  r1 <- unifyTypes f1 f2
  r2 <- unifyTypes a1 a2
  pure $ r1 >> r2
unifyTypes t1 t2 =
  pure $ Left $ UnificationError t1 t2 (UnifyOrigin NoSourceSpan)
  where

-- Placeholder SourceSpan; the caller passes the real one via `unify`.

-- | Unify a meta-variable with a type, performing the occurs check.
unifyMetaTv :: Unique -> TcType -> TcM (Either TcErrorKind ())
unifyMetaTv u ty = do
  ty' <- zonkType ty
  case ty' of
    TcMetaTv u' | u == u' -> pure (Right ())
    _ ->
      if occursIn u ty'
        then pure $ Left $ OccursCheckError u ty'
        else do
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
