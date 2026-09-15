-- | Improvement from type family injectivity.
--
-- An injectivity annotation @type family F a b = r | r -> a@ promises that
-- the result of the family determines the argument @a@. A wanted equality
-- whose one side is a saturated application of such a family may therefore
-- determine that argument, which solves meta variables that the equality
-- itself cannot solve: a family application is opaque, so the solver never
-- decomposes it.
--
-- Two rules produce the improvement:
--
--   * two applications of the same family are equal only if they agree on
--     the injective arguments, so @F t1 t2 ~ F s1 s2@ equates @t1@ and
--     @s1@;
--   * an equation @F p1 p2 = rhs@ is the only way the family can reach
--     @rhs@, so @F t1 t2 ~ rhs@ equates @t1@ with the pattern @p1@ under
--     the substitution that matches @rhs@.
--
-- Like functional dependency improvement this produces equalities, never
-- evidence: the equality constraint itself is still solved afterwards, by
-- reduction or by a given.
--
-- The annotation is taken on trust. GHC checks that the equations of a
-- family really are injective in the arguments the annotation names; aihc
-- does not, so a wrong annotation makes the solver derive a wrong equality
-- rather than report the annotation.
module Aihc.Tc.Solve.Injective
  ( improveInjectivity,
  )
where

import Aihc.Tc.Env (TyConFlavor (..), TyConInfo (..), TypeFamilyInstanceInfo (..))
import Aihc.Tc.FunDep (atPositions)
import Aihc.Tc.Monad (TcM, lookupTyConByIdentity)
import Aihc.Tc.Solve.Decompose (decomposeNominalEquality)
import Aihc.Tc.Solve.Family (familyEquations, isTypeFamilyApplication, matchTypes, reduceTypeFamilies, unsaturateFamilyApplication)
import Aihc.Tc.Types
import Aihc.Tc.Unify (unifyTypes)
import Aihc.Tc.Zonk (zonkPred, zonkType)
import Control.Monad (forM_, unless, void, zipWithM_)

-- | Improve the wanted predicates from the injectivity annotations of the
-- families they mention, reading them in the light of the givens.
--
-- The result says whether improvement solved a meta variable, which means
-- the wanteds are worth another attempt.
improveInjectivity :: [Pred] -> [Pred] -> TcM Bool
improveInjectivity givens wanteds = do
  before <- mapM zonkPred wanteds
  equalities <- givenEqualities givens
  forM_ wanteds (improveConstraint equalities)
  after <- mapM zonkPred wanteds
  pure (before /= after)

-- | The equalities that the givens state, in both orientations, zonked and
-- reduced so that they can be compared with the sides of a wanted.
givenEqualities :: [Pred] -> TcM [(TcType, TcType)]
givenEqualities givens = concat <$> mapM equality givens
  where
    equality predicate = case predicate of
      EqPred rawLeft rawRight -> do
        left <- normalize rawLeft
        right <- normalize rawRight
        pure [(left, right), (right, left)]
      _ -> pure []

normalize :: TcType -> TcM TcType
normalize ty = zonkType ty >>= reduceTypeFamilies >>= unsaturateFamilyApplication

-- | Improve one wanted. Only an equality names two types that injectivity
-- can relate.
improveConstraint :: [(TcType, TcType)] -> Pred -> TcM ()
improveConstraint equalities predicate =
  case predicate of
    EqPred left right -> improveTypes equalities left right
    _ -> pure ()

-- | Improve an equality between two types, and between the corresponding
-- parts of their outer structure: a family application is usually a field
-- of the type the wanted names rather than the whole of it.
--
-- A given that equates one side with something else offers a second
-- reading of that side, and a family application can appear in either
-- reading.
improveTypes :: [(TcType, TcType)] -> TcType -> TcType -> TcM ()
improveTypes equalities rawLeft rawRight = do
  left <- normalize rawLeft
  right <- normalize rawRight
  sequence_
    [ improvePair leftReading rightReading
    | leftReading <- readings left,
      rightReading <- readings right
    ]
  children <- decomposeNominalEquality left right
  forM_ (concat children) (uncurry (improveTypes equalities))
  where
    readings ty = ty : [other | (one, other) <- equalities, sameType one ty]

-- | A saturated application of a type family that declares an injectivity
-- annotation: its constructor, the argument positions the result
-- determines, and its arguments.
data InjectiveApplication = InjectiveApplication !TyCon ![Int] ![TcType]

injectiveApplication :: TcType -> TcM (Maybe InjectiveApplication)
injectiveApplication ty =
  case ty of
    TcTyCon tyCon arguments -> do
      maybeInfo <- lookupTyConByIdentity tyCon
      pure $ case maybeInfo of
        Just info
          | tciFlavor info == TypeFamilyTyCon,
            length arguments == tciArity info,
            Just positions <- tciInjectivity info,
            not (null positions) ->
              Just (InjectiveApplication tyCon positions arguments)
        _ -> Nothing
    _ -> pure Nothing

improvePair :: TcType -> TcType -> TcM ()
improvePair left right = do
  leftFamily <- injectiveApplication left
  rightFamily <- injectiveApplication right
  case (leftFamily, rightFamily) of
    (Just (InjectiveApplication leftTyCon positions leftArguments), Just (InjectiveApplication rightTyCon _ rightArguments))
      | leftTyCon == rightTyCon ->
          improveEqualities (atPositions positions leftArguments) (atPositions positions rightArguments)
    (Just family, Nothing) -> improveFromEquations family right
    (Nothing, Just family) -> improveFromEquations family left
    _ -> pure ()

-- | Improve a family application against a type that is not one.
--
-- Only an equation whose right-hand side matches the type can have
-- produced it, so its left-hand side names the arguments. An equation that
-- the match leaves incomplete -- one whose determined patterns still
-- mention a variable the right-hand side does not fix -- says nothing, and
-- neither does a type that more than one equation can produce, which the
-- annotation would not permit but nothing here has checked.
improveFromEquations :: InjectiveApplication -> TcType -> TcM ()
improveFromEquations (InjectiveApplication tyCon positions arguments) target = do
  targetIsFamily <- isTypeFamilyApplication target
  unless (targetIsFamily || isMetaTv target) $ do
    equations <- familyEquations tyCon
    case concatMap (determinedByEquation positions target) equations of
      [determined] -> improveEqualities determined (atPositions positions arguments)
      _ -> pure ()

isMetaTv :: TcType -> Bool
isMetaTv ty = case ty of
  TcMetaTv _ -> True
  TcArrowTy -> True
  _ -> False

-- | The arguments that one equation determines for a target type, when it
-- produces that type at all and the match fixes every variable it uses.
determinedByEquation :: [Int] -> TcType -> TypeFamilyInstanceInfo -> [[TcType]]
determinedByEquation positions target equation =
  case (tfiiLeft equation, matchTypes [tfiiRight equation] [target]) of
    (TcTyCon _ patterns, Just substitution)
      | length patterns > maximum positions,
        determined <- map (applySubst substitution) (atPositions positions patterns),
        not (any (\tyVar -> any (typeMentionsTyVar tyVar) determined) (tfiiTyVars equation)) ->
          [determined]
    _ -> []

-- | Equate the determined arguments. Improvement carries no evidence, so
-- the equality is solved by unification alone. Arguments that do not unify
-- are left to the constraint itself to report as unsolved.
improveEqualities :: [TcType] -> [TcType] -> TcM ()
improveEqualities left right
  | length left /= length right = pure ()
  | otherwise = zipWithM_ improveOne left right
  where
    improveOne leftType rightType
      | sameType leftType rightType = pure ()
      | otherwise = void (unifyTypes leftType rightType)
