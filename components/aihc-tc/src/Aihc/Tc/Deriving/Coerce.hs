{-# LANGUAGE OverloadedStrings #-}

-- | Synthesize a representational coercion between two types.
--
-- Newtype deriving only ever has to unwrap the one newtype it derives for,
-- so a proof runs in a single direction. Deriving via coerces between two
-- types that are each free to be a newtype of some shared representation,
-- so the search unwraps on either side and joins the two halves with
-- transitivity.
module Aihc.Tc.Deriving.Coerce (coercionBetween) where

import Aihc.Tc.Deriving.Context (typeTyVars)
import Aihc.Tc.Env
import Aihc.Tc.Evidence
import Aihc.Tc.Kind (tcTypeKind)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Coercible (isRepresentationParameter)
import Aihc.Tc.Solve.Dict (matchTypes)
import Aihc.Tc.Types
import Control.Monad (zipWithM)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)

-- | A proof of @source ~ target@ at representational role, or 'Nothing'
-- when the two types do not share a representation. Coercions lift through
-- representation parameters, as roles permit.
coercionBetween :: [TypeFamilyInstanceInfo] -> TcType -> TcType -> TcM (Maybe Coercion)
coercionBetween equations rawSource rawTarget = go [] (normalize rawSource) (normalize rawTarget)
  where
    -- Unwrapping both sides can cycle through a recursive newtype, which a
    -- one-sided search never could, so the walk remembers the pairs it is
    -- already working on.
    go visited source target
      | source == target = pure (Just (Refl source))
      | (source, target) `elem` visited || length visited >= coercionFuel = pure Nothing
      | otherwise =
          firstJustM
            [ congruence seen source target,
              unwrapTarget seen source target,
              unwrapSource seen source target,
              pure (familyProof source target)
            ]
      where
        seen = (source, target) : visited

    congruence visited source target =
      case (source, target) of
        (TcFunTy sourceArgument sourceResult, TcFunTy targetArgument targetResult) -> do
          argument <- go visited sourceArgument targetArgument
          result <- go visited sourceResult targetResult
          pure (FunCo <$> argument <*> result)
        (TcTyCon sourceConstructor sourceArguments, TcTyCon targetConstructor targetArguments)
          | sourceConstructor == targetConstructor,
            length sourceArguments == length targetArguments -> do
              proofs <- sequence <$> zipWithM (argumentProof visited sourceConstructor) [0 ..] (zip sourceArguments targetArguments)
              pure (TyConAppCo sourceConstructor sourceArguments <$> proofs)
        _ -> pure Nothing

    -- An axiom proves @target ~ representation@, so a proof of
    -- @source ~ target@ reaches the representation and turns around.
    unwrapTarget visited source target =
      withUnwrapping target $ \representation axiom -> do
        inner <- go visited source representation
        pure (flip mkTrans (Sym axiom) <$> inner)

    unwrapSource visited source target =
      withUnwrapping source $ \representation axiom -> do
        inner <- go visited representation target
        pure (mkTrans axiom <$> inner)

    withUnwrapping ty use = do
      unwrapping <- newtypeUnwrapping ty
      case unwrapping of
        Nothing -> pure Nothing
        Just (representation, axiom) -> use representation axiom

    familyProof source target =
      case [ Sym (AxiomInstCo (typeFamilyAxiomKey equation) arguments)
           | equation <- equations,
             Just substitution <- [matchTypes [tfiiLeft equation] [target]],
             normalize (applySubst substitution (tfiiRight equation)) == source,
             let arguments = map (applySubst substitution . TcTyVar) (tfiiTyVars equation)
           ] of
        proof : _ -> Just proof
        [] -> Nothing

    -- A representation parameter carries an argument coercion. A nominal one
    -- admits only the argument it already has.
    argumentProof visited constructor index (sourceArgument, targetArgument) = do
      representational <- isRepresentationParameter constructor index
      if representational
        then go visited sourceArgument targetArgument
        else pure (if sourceArgument == targetArgument then Just (Refl sourceArgument) else Nothing)

coercionFuel :: Int
coercionFuel = 100

-- | The representation that a saturated newtype application unwraps to,
-- with the axiom @ty ~ representation@ that proves it. A data type with a
-- single field is not a newtype and never unwraps: only a newtype shares
-- its runtime representation with its field.
newtypeUnwrapping :: TcType -> TcM (Maybe (TcType, Coercion))
newtypeUnwrapping ty =
  case ty of
    TcTyCon constructor arguments -> do
      maybeDataType <- lookupDataType constructor
      case maybeDataType of
        Just dataType
          | dtiFlavor dataType == NewtypeTyCon,
            length arguments == length (dtiTyVars dataType),
            [con] <- dtiConstructors dataType,
            [field] <- dciFields con -> do
              let substitution = Map.fromList (zip (map tvUnique (dtiTyVars dataType)) arguments)
              axiom <- newtypeAxiom constructor dataType arguments
              pure (Just (normalize (applySubst substitution (dcfiType field)), axiom))
        _ -> pure Nothing
    _ -> pure Nothing

-- | Instantiate the axiom of a newtype. The kind arguments the declaration
-- binds implicitly come before the type arguments.
newtypeAxiom :: TyCon -> DataTypeInfo -> [TcType] -> TcM Coercion
newtypeAxiom constructor dataType arguments = do
  argumentKinds <- mapM tcTypeKind arguments
  let kindSubstitution = fromMaybe Map.empty (matchTypes (map tvKind (dtiTyVars dataType)) argumentKinds)
      kindVariables = filter (`notElem` dtiTyVars dataType) (nub (concatMap (typeTyVars . tvKind) (dtiTyVars dataType)))
      kindArguments = map (applySubst kindSubstitution . TcTyVar) kindVariables
      key = TcAxiomKey (tyConPackageId constructor) (tyConModuleName constructor) ("$ax$" <> dtiName dataType)
  pure (AxiomInstCo key (kindArguments <> arguments))

-- | Reflexivity carries no information, so a step that proves nothing does
-- not reach the generated code. Keeping the proof of a single unwrapping
-- bare also keeps it the shape that newtype deriving produced before the
-- search could run in two directions.
mkTrans :: Coercion -> Coercion -> Coercion
mkTrans (Refl _) coercion = coercion
mkTrans coercion (Refl _) = coercion
mkTrans left right = Trans left right

firstJustM :: (Monad m) => [m (Maybe a)] -> m (Maybe a)
firstJustM [] = pure Nothing
firstJustM (action : rest) = do
  result <- action
  case result of
    Just value -> pure (Just value)
    Nothing -> firstJustM rest

-- | Flatten applications onto the type constructor spine, so that a type
-- written as an application matches one written as a saturated constructor.
normalize :: TcType -> TcType
normalize ty = case ty of
  TcAppTy function argument -> case normalize function of
    TcTyCon constructor arguments -> TcTyCon constructor (arguments <> [normalize argument])
    other -> TcAppTy other (normalize argument)
  TcTyCon constructor arguments -> TcTyCon constructor (map normalize arguments)
  TcFunTy argument result -> TcFunTy (normalize argument) (normalize result)
  _ -> ty
