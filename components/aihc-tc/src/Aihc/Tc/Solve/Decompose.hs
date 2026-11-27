-- | Structural rules for nominal equality.
module Aihc.Tc.Solve.Decompose
  ( decomposeNominalEquality,
    predicateArgumentPairs,
  )
where

import Aihc.Tc.Monad
import Aihc.Tc.Solve.Family (isTypeFamilyTyCon, unsaturateFamilyApplication)
import Aihc.Tc.Types
import Data.Map.Strict qualified as Map

-- | Return the equalities that the outer type structure permits.
-- A saturated family application is opaque. Extra arguments can decompose.
-- The caller must preserve evidence and apply these rules to each child.
decomposeNominalEquality :: TcType -> TcType -> TcM (Maybe [(TcType, TcType)])
decomposeNominalEquality rawLeft rawRight = do
  left <- unsaturateFamilyApplication rawLeft
  right <- unsaturateFamilyApplication rawRight
  isFamily <- isTypeFamilyTyCon
  let familyHead (TcTyCon tyCon _) = isFamily tyCon
      familyHead _ = False
  if familyHead left || familyHead right
    then pure Nothing
    else decompose left right
  where
    decompose (TcTyConApp leftCon leftKinds leftArgs) (TcTyConApp rightCon rightKinds rightArgs)
      | leftCon == rightCon,
        length leftArgs == length rightArgs =
          pure (Just (pairedKindArguments leftKinds rightKinds <> zip leftArgs rightArgs))
    decompose (TcFunTy leftArg leftResult) (TcFunTy rightArg rightResult) =
      pure (Just [(leftArg, rightArg), (leftResult, rightResult)])
    decompose (TcAppTy function argument) (TcFunTy domain range) = do
      arrow <- arrowType
      pure (Just [(function, TcAppTy arrow domain), (argument, range)])
    decompose (TcFunTy domain range) (TcAppTy function argument) = do
      arrow <- arrowType
      pure (Just [(TcAppTy arrow domain, function), (range, argument)])
    decompose (TcAppTy function argument) (TcTyConApp tyCon kindArguments arguments)
      | not (null arguments) =
          pure (Just [(function, TcTyConApp tyCon kindArguments (init arguments)), (argument, last arguments)])
    decompose (TcTyConApp tyCon kindArguments arguments) (TcAppTy function argument)
      | not (null arguments) =
          pure (Just [(TcTyConApp tyCon kindArguments (init arguments), function), (last arguments, argument)])
    decompose (TcAppTy leftFunction leftArgument) (TcAppTy rightFunction rightArgument) =
      pure (Just [(leftFunction, rightFunction), (leftArgument, rightArgument)])
    -- Two polymorphic types are equal up to the names of their bound
    -- variables, and two qualified types when their predicates are equal
    -- one by one.
    decompose (TcForAllTy leftVariable leftBody) (TcForAllTy rightVariable rightBody) =
      pure (Just [(leftBody, applySubst (Map.singleton (tvUnique rightVariable) (TcTyVar leftVariable)) rightBody)])
    decompose (TcQualTy leftPredicates leftBody) (TcQualTy rightPredicates rightBody)
      | length leftPredicates == length rightPredicates =
          pure ((<> [(leftBody, rightBody)]) . concat <$> sequence (zipWith predicateArgumentPairs leftPredicates rightPredicates))
    decompose _ _ = pure Nothing

-- | The type equalities that make two predicates equal. A quantified
-- predicate binds variables, so it must be the same predicate already.
predicateArgumentPairs :: Pred -> Pred -> Maybe [(TcType, TcType)]
predicateArgumentPairs left right =
  case (left, right) of
    (ClassPred leftClass leftArguments, ClassPred rightClass rightArguments)
      | leftClass == rightClass,
        length leftArguments == length rightArguments ->
          Just (zip leftArguments rightArguments)
    (EqPred leftLeft leftRight, EqPred rightLeft rightRight) -> Just [(leftLeft, rightLeft), (leftRight, rightRight)]
    (IParamPred leftName leftPayload, IParamPred rightName rightPayload)
      | leftName == rightName -> Just [(leftPayload, rightPayload)]
    (IrredPred leftConstraint, IrredPred rightConstraint) -> Just [(leftConstraint, rightConstraint)]
    _
      | samePred left right -> Just []
      | otherwise -> Nothing

