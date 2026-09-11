-- | Unification of types.
--
-- Handles meta-variable solving with occurs check.
module Aihc.Tc.Unify
  ( unify,
    unifyDeferring,
    unifyTypes,
  )
where

import Aihc.Parser.Syntax (SourceSpan (..))
import Aihc.Tc.Constraint (CtOrigin (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Kind (refineGivenTyVarKinds, tcTypeKind, unifyKindsAt)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Decompose (decomposeNominalEquality)
import Aihc.Tc.Solve.Family (isTypeFamilyApplication, reduceTypeFamilies, unsaturateFamilyApplication)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)

-- | Unify two types, recording the solution and emitting an error if
-- they are incompatible.
unify :: SourceSpan -> CtOrigin -> TcType -> TcType -> TcM ()
unify loc origin t1 t2 = do
  stuck <- unifyDeferring loc origin t1 t2
  mapM_ reportStuck stuck
  where
    reportStuck (left, right) =
      emitError loc (UnificationError left right origin Nothing)

-- | Unify two types, returning the equalities that a saturated type
-- family application no equation reduces yet leaves undecided.
--
-- Such an equality must not be decomposed or rejected: the application
-- may still reduce once a meta variable of its arguments is solved,
-- which can happen long after this unification. The caller turns the
-- result into wanted constraints for the solver.
unifyDeferring :: SourceSpan -> CtOrigin -> TcType -> TcType -> TcM [(TcType, TcType)]
unifyDeferring loc origin t1 t2 = do
  t1' <- zonkType t1 >>= reduceTypeFamilies
  t2' <- zonkType t2 >>= reduceTypeFamilies
  result <- unifyCollecting loc t1' t2'
  case result of
    Left err -> report err >> pure []
    -- The rest of the unification may have solved the meta variables
    -- that kept an application from reducing, so retry before deferring.
    Right deferred -> concat <$> mapM retry deferred
  where
    report (UnificationError left right _ provenance) =
      emitError loc (UnificationError left right origin provenance)
    report err = emitError loc err

    retry (left, right) = do
      left' <- zonkType left >>= reduceTypeFamilies
      right' <- zonkType right >>= reduceTypeFamilies
      if (left', right') == (left, right)
        then pure [(left, right)]
        else do
          result <- unifyTypesAt loc left' right'
          case result of
            Right () -> pure []
            Left err -> report err >> pure []

-- | Attempt to unify two types, returning an error kind on failure.
unifyTypes :: TcType -> TcType -> TcM (Either TcErrorKind ())
unifyTypes = unifyTypesAt NoSourceSpan

-- | Attempt to unify two types. A kind mismatch is reported at the span.
unifyTypesAt :: SourceSpan -> TcType -> TcType -> TcM (Either TcErrorKind ())
unifyTypesAt loc t1 t2 = do
  result <- unifyCollecting loc t1 t2
  case result of
    Left err -> pure (Left err)
    Right deferred -> retryDeferred loc deferred

-- | Unify two types, collecting the pairs that a type family application
-- no equation reduces yet has held back.
unifyCollecting :: SourceSpan -> TcType -> TcType -> TcM (Either TcErrorKind [(TcType, TcType)])
unifyCollecting _ (TcMetaTv u1) (TcMetaTv u2)
  | u1 == u2 = pure (Right [])
unifyCollecting loc (TcMetaTv u) ty = fmap (const []) <$> unifyMetaTv loc u ty
unifyCollecting loc ty (TcMetaTv u) = fmap (const []) <$> unifyMetaTv loc u ty
unifyCollecting _ (TcTyVar v1) (TcTyVar v2)
  | v1 == v2 = pure (Right [])
unifyCollecting loc t1 t2
  | t1 == t2 = pure (Right [])
  | otherwise = do
      stuck <- isStuckFamilyEquality t1 t2
      if stuck
        then pure (Right [(t1, t2)])
        else do
          children <- decomposeNominalEquality t1 t2
          case children of
            Just pairs -> fmap concat . sequence <$> mapM (uncurry (unifyCollecting loc)) pairs
            Nothing -> pure (Left (UnificationError t1 t2 (UnifyOrigin NoSourceSpan) Nothing))

-- | Whether either side is a saturated type family application that no
-- equation reduces. Decomposing such an equality is unsound: the
-- application may still reduce once its arguments are known.
isStuckFamilyEquality :: TcType -> TcType -> TcM Bool
isStuckFamilyEquality t1 t2 = do
  left <- unsaturateFamilyApplication t1 >>= isTypeFamilyApplication
  right <- unsaturateFamilyApplication t2 >>= isTypeFamilyApplication
  pure (left || right)

-- | Retry the equalities that a stuck type family application held back.
-- Unifying the other pairs may have solved the meta variables that kept
-- the application from reducing.
retryDeferred :: SourceSpan -> [(TcType, TcType)] -> TcM (Either TcErrorKind ())
retryDeferred loc pairs = sequence_ <$> mapM retryOne pairs
  where
    retryOne (t1, t2) = do
      t1' <- zonkType t1 >>= reduceTypeFamilies
      t2' <- zonkType t2 >>= reduceTypeFamilies
      if (t1', t2') == (t1, t2)
        then pure (Left (UnificationError t1 t2 (UnifyOrigin NoSourceSpan) Nothing))
        else unifyTypesAt loc t1' t2'

-- | Unify a meta-variable with a type, performing the occurs check.
unifyMetaTv :: SourceSpan -> Unique -> TcType -> TcM (Either TcErrorKind ())
unifyMetaTv loc u ty = do
  ty' <- zonkType ty >>= refineGivenTyVarKinds
  case ty' of
    TcMetaTv u' | u == u' -> pure (Right ())
    _
      | occursIn u ty' -> pure $ Left $ OccursCheckError (TcMetaTv u) ty'
      -- A meta-variable stands for a monotype. Binding it to a polytype
      -- would let inference guess an impredicative instantiation.
      | isPolyType ty' -> pure $ Left $ UnificationError (TcMetaTv u) ty' (UnifyOrigin NoSourceSpan) Nothing
      | otherwise -> do
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
    go TcArrowTy = False
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
