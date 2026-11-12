{-# LANGUAGE OverloadedStrings #-}

-- | Synthesize a representational coercion between two types.
--
-- Newtype deriving only ever has to unwrap the one newtype it derives for,
-- so a proof runs in a single direction. Deriving via coerces between two
-- types that are each free to be a newtype of some shared representation,
-- so the search unwraps on either side and joins the two halves with
-- transitivity.
module Aihc.Tc.Deriving.Coerce (coercionBetween, nominalCoercionBetween) where

import Aihc.Tc.Deriving.Context (typeTyVars)
import Aihc.Tc.Env
import Aihc.Tc.Evidence
import Aihc.Tc.Kind (tcTypeKind)
import Aihc.Tc.Match (matchTypes)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Coercible (isRepresentationParameter)
import Aihc.Tc.Types
import Control.Monad (zipWithM)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T

-- | A proof of @source ~ target@ at representational role, or 'Nothing'
-- when the two types do not share a representation. Coercions lift through
-- representation parameters, as roles permit.
coercionBetween :: [TypeFamilyInstanceInfo] -> TcType -> TcType -> TcM (Maybe Coercion)
coercionBetween = searchCoercion True

-- | A proof of @source ~ target@ at nominal role, or 'Nothing'. The proof
-- uses only congruence and type family equations, and no newtype axiom.
-- A dictionary argument of a derived method needs this proof, because a
-- class parameter has the nominal role.
nominalCoercionBetween :: [TypeFamilyInstanceInfo] -> TcType -> TcType -> TcM (Maybe Coercion)
nominalCoercionBetween = searchCoercion False

-- | The search. When @unwrap@ is false, the search does not unwrap a
-- newtype, and each argument of a type constructor gets a nominal proof.
searchCoercion :: Bool -> [TypeFamilyInstanceInfo] -> TcType -> TcType -> TcM (Maybe Coercion)
searchCoercion unwrap equations rawSource rawTarget = go [] (normalize rawSource) (normalize rawTarget)
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
              if unwrap then unwrapTarget seen source target else pure Nothing,
              if unwrap then unwrapSource seen source target else pure Nothing,
              familyTarget seen source target,
              familySource seen source target
            ]
      where
        seen = (source, target) : visited

    congruence visited source target =
      case (source, target) of
        (TcFunTy sourceArgument sourceResult, TcFunTy targetArgument targetResult) -> do
          argument <- go visited sourceArgument targetArgument
          result <- go visited sourceResult targetResult
          pure (FunCo <$> argument <*> result)
        -- A method type can quantify inside an argument, as the argument
        -- of @mask@ does. The two bodies are compared under one variable.
        (TcForAllTy sourceVariable sourceBody, TcForAllTy targetVariable targetBody)
          | tvKind sourceVariable == tvKind targetVariable -> do
              let renamedTarget = normalize (applySubst (Map.singleton (tvUnique targetVariable) (TcTyVar sourceVariable)) targetBody)
              body <- go visited sourceBody renamedTarget
              pure (ForAllCo sourceVariable <$> body)
        (TcTyCon sourceConstructor sourceArguments, TcTyCon targetConstructor targetArguments)
          | sourceConstructor == targetConstructor,
            length sourceArguments == length targetArguments -> do
              proofs <- sequence <$> zipWithM (argumentProof visited sourceConstructor) [0 ..] (zip sourceArguments targetArguments)
              pure ((\argumentProofs -> TyConAppCo sourceConstructor sourceArguments argumentProofs []) <$> proofs)
        -- An application of a type variable, as the @b (StM m a)@ of
        -- @RunInBase m b@. The argument of an unknown function has the
        -- nominal role, thus it gets a proof without newtype axioms.
        (TcAppTy sourceFunction sourceArgument, TcAppTy targetFunction targetArgument) -> do
          function <- go visited sourceFunction targetFunction
          argument <-
            if unwrap
              then nominalCoercionBetween equations sourceArgument targetArgument
              else go visited sourceArgument targetArgument
          pure (AppCo <$> function <*> argument)
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

    -- An axiom proves @target ~ reduct@. The search continues from the
    -- reduct, because the other side can need a reduction too, as in
    -- @Element [Output] ~ Element Outputs@, where both sides reduce to
    -- @Output@.
    familyTarget visited source target = do
      reductions <- familyReductions target
      firstJustM [fmap (\inner -> mkTrans inner (mkSym axiom)) <$> go visited source reduct | (reduct, axiom) <- reductions]

    familySource visited source target = do
      reductions <- familyReductions source
      firstJustM [fmap (mkTrans axiom) <$> go visited reduct target | (reduct, axiom) <- reductions]

    -- The equations are the associated types of the instance and the open
    -- equations of the family, such as a top-level
    -- @type instance Element (NonNull mono) = Element mono@. A closed
    -- equation is not used, because its match also needs an apartness check.
    -- Each reduction comes with the axiom that proves @ty ~ reduct@.
    familyReductions ty = do
      openEquations <- case ty of
        TcTyCon family _ -> filter (not . tfiiClosed) . Map.elems <$> getTypeFamilyEquations family
        _ -> pure []
      pure
        [ (normalize (applySubst substitution (tfiiRight equation)), AxiomInstCo (typeFamilyAxiomKey equation) arguments)
        | equation <- equations <> openEquations,
          Just substitution <- [matchTypes [tfiiLeft equation] [ty]],
          let arguments = map (applySubst substitution . TcTyVar) (tfiiTyVars equation)
        ]

    -- A representation parameter carries an argument coercion. A nominal one
    -- admits only the argument it already has.
    argumentProof visited constructor index (sourceArgument, targetArgument) = do
      representational <- if unwrap then isRepresentationParameter constructor index else pure True
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
              -- The kind variables of the declaration get the kinds of the
              -- arguments, as in the axiom. A partial application in the
              -- field, as the @P f@ of @newtype W f a = W (P (P f) a)@,
              -- keeps its kind arguments.
              argumentKinds <- mapM tcTypeKind arguments
              let kindSubstitution = fromMaybe Map.empty (matchTypes (map tvKind (dtiTyVars dataType)) argumentKinds)
                  substitution = Map.fromList (zip (map tvUnique (dtiTyVars dataType)) arguments) `Map.union` kindSubstitution
              axiom <- newtypeAxiom constructor dataType arguments
              pure (Just (normalize (applySubst substitution (dcfiType field)), axiom))
        _ -> familyNewtypeUnwrapping (TcTyCon constructor arguments)
    _ -> pure Nothing

-- | The representation of a data family application that a newtype
-- instance covers. The family axiom proves @F pats ~ R:F vars@, and the
-- representation axiom proves @R:F vars ~ field@. A deriving clause on the
-- newtype instance coerces through both.
familyNewtypeUnwrapping :: TcType -> TcM (Maybe (TcType, Coercion))
familyNewtypeUnwrapping target = do
  instances <- getDataFamilyInstances
  pure $ case [ (normalize (applySubst substitution (dcfiType field)), mkTrans (AxiomInstCo (dataFamilyAxiomKey familyInstance) arguments) (AxiomInstCo (representationAxiomKey familyInstance) arguments))
              | familyInstance <- instances,
                dfiiIsNewtype familyInstance,
                [con] <- [dfiiConstructors familyInstance],
                null (dciExTyVars con),
                null (dciTheta con),
                [field] <- [dciFields con],
                Just substitution <- [matchTypes [normalize (dfiiFamilyType familyInstance)] [target]],
                let arguments = map (applySubst substitution . TcTyVar) (dfiiTyVars familyInstance)
              ] of
    [unwrapping] -> Just unwrapping
    _ -> Nothing
  where
    representationAxiomKey familyInstance =
      let tyCon = dfiiRepresentationTyCon familyInstance
       in TcAxiomKey (tyConPackageId tyCon) (tyConModuleName tyCon) ("$ax$" <> T.drop 1 (tyConName tyCon))

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

-- | Symmetry that removes a double symmetry.
mkSym :: Coercion -> Coercion
mkSym (Sym coercion) = coercion
mkSym coercion = Sym coercion

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
