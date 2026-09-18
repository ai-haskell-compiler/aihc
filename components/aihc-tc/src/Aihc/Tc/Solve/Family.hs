-- | Type family reduction.
--
-- A saturated application of a type family rewrites to the right-hand
-- side of the first equation whose left-hand side matches. The solver
-- reduces both sides of an equality and the arguments of a class
-- constraint before it compares them, so @Elem Bag@ and @Nat@ are the same
-- type when an equation says so.
module Aihc.Tc.Solve.Family
  ( reduceTypeFamilies,
    reducePredFamilies,
    normalizeFamilyPred,
    isTypeFamilyTyCon,
    isTypeFamilyApplication,
    unsaturateFamilyApplication,
    familyEquations,
  )
where

import Aihc.Tc.Env (TyConFlavor (..), TyConInfo (..), TypeFamilyInstanceInfo (..))
import Aihc.Tc.Match (matchTypes)
import Aihc.Tc.Monad (TcM, TcState (tcsGlobalTyCons), getKinds, getTypeFamilyInstances, getWiring, lookupTyConByIdentity)
import Aihc.Tc.TypeLitFamily (TypeLitValue (..), evaluateTypeLitFamily)
import Aihc.Tc.Types
import Aihc.Tc.Wiring (TcWiring (..))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (gets)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Text.Read (readMaybe)

-- | Rewrite every type family application in a type that an equation
-- matches. The type must be zonked.
reduceTypeFamilies :: TcType -> TcM TcType
reduceTypeFamilies ty =
  case ty of
    TcTyCon tyCon arguments -> do
      arguments' <- mapM reduceTypeFamilies arguments
      reduceHead (TcTyCon tyCon arguments')
    TcFunTy argument result -> TcFunTy <$> reduceTypeFamilies argument <*> reduceTypeFamilies result
    TcAppTy function argument -> do
      function' <- reduceTypeFamilies function
      argument' <- reduceTypeFamilies argument
      reduceHead (mkAppTy function' argument')
    TcForAllTy tyVar body -> TcForAllTy tyVar <$> reduceTypeFamilies body
    TcQualTy predicates body -> TcQualTy <$> mapM reducePredFamilies predicates <*> reduceTypeFamilies body
    _ -> pure ty

-- | Rewrite the type family applications in a predicate.
reducePredFamilies :: Pred -> TcM Pred
reducePredFamilies predicate =
  case predicate of
    ClassPred className arguments -> ClassPred className <$> mapM reduceTypeFamilies arguments
    EqPred left right -> EqPred <$> reduceTypeFamilies left <*> reduceTypeFamilies right
    IParamPred name payload -> IParamPred name <$> reduceTypeFamilies payload
    IrredPred constraint -> IrredPred <$> reduceTypeFamilies constraint
    QuantifiedPred variables antecedents consequent ->
      QuantifiedPred variables <$> mapM reducePredFamilies antecedents <*> reducePredFamilies consequent

-- | Reclassify a predicate whose head is a type family as irreducible.
--
-- A constraint headed by a family reaches the checker in two shapes: as an
-- 'IrredPred' when the source wrote it, and as a 'ClassPred' when it was
-- rebuilt from a type -- an expanded constraint synonym, or a stored
-- superclass -- without access to the flavour of the head. Both have to
-- become the same predicate: a wanted and a given of one constraint must
-- compare equal, and the desugarer must not mint a dictionary type for a
-- family, which has no declaration.
normalizeFamilyPred :: Pred -> TcM Pred
normalizeFamilyPred predicate = do
  family <- isTypeFamilyTyCon
  let normalize predicate' =
        case predicate' of
          ClassPred tyCon arguments | family tyCon -> IrredPred (TcTyCon tyCon arguments)
          QuantifiedPred variables antecedents consequent ->
            QuantifiedPred variables (map normalize antecedents) (normalize consequent)
          _ -> predicate'
  pure (normalize predicate)

-- | Whether a type constructor is a type family.
isTypeFamilyTyCon :: TcM (TyCon -> Bool)
isTypeFamilyTyCon = do
  tyCons <- lift $ gets tcsGlobalTyCons
  pure (\tyCon -> maybe False ((== TypeFamilyTyCon) . tciFlavor) (Map.lookup (tyConKey tyCon) tyCons))

-- | Whether a type is a saturated application of a type family that no
-- equation reduces yet. The equality solver waits for such a type.
isTypeFamilyApplication :: TcType -> TcM Bool
isTypeFamilyApplication ty =
  case ty of
    TcTyCon tyCon arguments -> do
      maybeInfo <- lookupTyConByIdentity tyCon
      pure $ case maybeInfo of
        Just info -> tciFlavor info == TypeFamilyTyCon && length arguments == tciArity info
        Nothing -> False
    _ -> pure False

-- | Split a type family application with more arguments than the family
-- arity into an application spine over the saturated family application.
-- The extra arguments of a family application decompose like the arguments
-- of a type constructor; only the family arguments do not.
unsaturateFamilyApplication :: TcType -> TcM TcType
unsaturateFamilyApplication ty =
  case ty of
    TcTyCon tyCon arguments -> do
      maybeInfo <- lookupTyConByIdentity tyCon
      pure $ case maybeInfo of
        Just info
          | tciFlavor info == TypeFamilyTyCon,
            length arguments > tciArity info ->
              let (familyArguments, extraArguments) = splitAt (tciArity info) arguments
               in foldl TcAppTy (TcTyCon tyCon familyArguments) extraArguments
        _ -> ty
    _ -> pure ty

-- | Rewrite the head of a type whose arguments are reduced.
reduceHead :: TcType -> TcM TcType
reduceHead ty =
  case ty of
    TcTyCon tyCon arguments -> do
      maybeInfo <- lookupTyConByIdentity tyCon
      case maybeInfo of
        Just info
          | tciFlavor info == TypeFamilyTyCon,
            length arguments >= tciArity info -> do
              let (familyArguments, extraArguments) = splitAt (tciArity info) arguments
              kinds <- getKinds
              wiring <- getWiring
              -- A built-in family has no equations to consult: the solver
              -- computes it, and only when every argument is a literal.
              case builtinTypeLitFamily wiring kinds tyCon familyArguments of
                Just reduced -> reduceTypeFamilies (foldl mkAppTy reduced extraArguments)
                Nothing -> do
                  equations <- familyEquations tyCon
                  family <- isTypeFamilyTyCon
                  case firstEquation family equations familyArguments of
                    Just reduced -> reduceTypeFamilies (foldl mkAppTy reduced extraArguments)
                    Nothing -> pure ty
        _ -> pure ty
    _ -> pure ty

-- | The value of a built-in type-literal family, when the family is one
-- and every argument is a literal.
--
-- The operations are the compiler's own -- what @CmpNat@ or @+@ mean is
-- not something a library can say -- but where they are declared is, so
-- the module comes from the wiring and a family of the same name declared
-- anywhere else stays an ordinary one.
builtinTypeLitFamily :: TcWiring -> TcKinds -> TyCon -> [TcType] -> Maybe TcType
builtinTypeLitFamily wiring kinds tyCon arguments
  | tyConModuleName tyCon `notElem` tcWiringTypeLitFamilyModules wiring = Nothing
  | otherwise = do
      literals <- traverse literal arguments
      value <- evaluateTypeLitFamily (tyConName tyCon) literals
      pure $ case value of
        TypeLitNatural natural -> TcTyLit (TyLitNat natural)
        TypeLitOrdering ordering -> TcTyCon (kindsDataCon kinds (T.pack (show ordering)) 0) []
  where
    literal ty =
      case ty of
        TcTyLit value -> Just value
        _ -> Nothing

-- | The equations of a type family, in declaration order.
familyEquations :: TyCon -> TcM [TypeFamilyInstanceInfo]
familyEquations tyCon =
  sortOn axiomIndex . filter isEquationOf <$> getTypeFamilyInstances
  where
    isEquationOf info =
      case tfiiLeft info of
        TcTyCon familyTyCon _ -> familyTyCon == tyCon
        _ -> False

-- | The index of an equation in its family. The axiom name ends with it.
axiomIndex :: TypeFamilyInstanceInfo -> Int
axiomIndex info =
  fromMaybe 0 (readMaybe (T.unpack (T.takeWhileEnd (/= '$') (tfiiAxiomName info))))

-- | The right-hand side of the first equation that matches. In a closed
-- family, an earlier equation that could still match after the meta
-- variables are solved blocks the later equations.
-- | The first equation that matches the arguments. A closed family stops
-- at an earlier equation that does not match but is not apart from the
-- arguments either: it may still match once a stuck family application
-- or a type variable in them is known, and the equations after it are
-- only reached when it cannot.
firstEquation :: (TyCon -> Bool) -> [TypeFamilyInstanceInfo] -> [TcType] -> Maybe TcType
firstEquation family equations arguments =
  case equations of
    [] -> Nothing
    equation : rest ->
      case equationArguments equation of
        Just patterns
          | Just substitution <- matchTypes patterns arguments ->
              Just (applySubst substitution (tfiiRight equation))
          | tfiiClosed equation,
            and (zipWith (couldUnify family) patterns arguments) ->
              Nothing
        _ -> firstEquation family rest arguments

equationArguments :: TypeFamilyInstanceInfo -> Maybe [TcType]
equationArguments info =
  case tfiiLeft info of
    TcTyCon _ patterns -> Just patterns
    _ -> Nothing

-- | Whether a pattern could match a type once more is known about it.
--
-- As in GHC's apartness check, every type variable of the target counts
-- as unifiable -- a skolem here is instantiated elsewhere -- and so does a
-- stuck type family application, which may reduce to anything.
couldUnify :: (TyCon -> Bool) -> TcType -> TcType -> Bool
couldUnify family = go
  where
    go patternType target =
      case (patternType, target) of
        (TcTyVar _, _) -> True
        (_, TcTyVar _) -> True
        (_, TcMetaTv _) -> True
        (_, TcTyCon targetTyCon _) | family targetTyCon -> True
        (TcTyCon tyCon arguments, TcTyCon targetTyCon targetArguments) ->
          tyCon == targetTyCon
            && length arguments == length targetArguments
            && and (zipWith go arguments targetArguments)
        (TcFunTy argument result, TcFunTy targetArgument targetResult) ->
          go argument targetArgument && go result targetResult
        (TcAppTy function argument, TcAppTy targetFunction targetArgument) ->
          go function targetFunction && go argument targetArgument
        _ -> patternType == target
