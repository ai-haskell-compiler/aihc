{-# LANGUAGE MagicHash #-}

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
    irreduciblePred,
    reclassifyIrreduciblePred,
    isTypeFamilyTyCon,
    isTypeFamilyApplication,
    unsaturateFamilyApplication,
    familyEquations,
    occursOutsideFamilies,
  )
where

import Aihc.Tc.Env (TyConFlavor (..), TyConInfo (..), TypeFamilyInstanceInfo (..))
import Aihc.Tc.Match (matchTypes)
import Aihc.Tc.Monad (TcM, TcState (tcsGlobalTyCons), getKinds, getTypeFamilyEquations, getWiring, lookupTyConByIdentity)
import Aihc.Tc.TypeLitFamily (TypeLitValue (..), evaluateTypeLitFamily, simplifyTypeLitFamily)
import Aihc.Tc.Types
import Aihc.Tc.Wiring (TcWiring (..))
import Control.Monad (zipWithM, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (gets)
import Data.List (sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import Text.Read (readMaybe)

-- | Rewrite every type family application in a type that an equation
-- matches. The type must be zonked.
reduceTypeFamilies :: TcType -> TcM TcType
reduceTypeFamilies ty =
  -- A part that no equation rewrites stays the same object, so the result
  -- shares every such part of the input.
  case ty of
    TcTyCon tyCon arguments -> do
      arguments' <- mapMSame reduceTypeFamilies arguments
      reduceHead (if sameObject arguments arguments' then ty else TcTyCon tyCon arguments')
    TcFunTy argument result -> do
      argument' <- reduceTypeFamilies argument
      result' <- reduceTypeFamilies result
      pure (if sameObject argument argument' && sameObject result result' then ty else TcFunTy argument' result')
    TcAppTy function argument -> do
      function' <- reduceTypeFamilies function
      argument' <- reduceTypeFamilies argument
      if sameObject function function' && sameObject argument argument' && plainApplication function
        then pure ty
        else reduceHead (mkAppTy function' argument')
    TcForAllTy tyVar body -> do
      body' <- reduceTypeFamilies body
      pure (if sameObject body body' then ty else TcForAllTy tyVar body')
    TcQualTy predicates body -> do
      predicates' <- mapMSame (reducePredFamilies >=> reclassifyIrreduciblePred) predicates
      body' <- reduceTypeFamilies body
      pure (if sameObject predicates predicates' && sameObject body body' then ty else TcQualTy predicates' body')
    _ -> pure ty

-- | Whether 'mkAppTy' of the function and an argument is the plain
-- application, so that a 'TcAppTy' of them stays as it is.
plainApplication :: TcType -> Bool
plainApplication function =
  case collectTypeApplications function of
    (TcKindedTyCon tyCon _, arguments)
      | not (isAnyTyCon tyCon),
        length arguments + 1 >= tyConArity tyCon ->
          False
    _ ->
      case function of
        TcTyCon {} -> False
        TcAppTy TcArrowTy _ -> False
        _ -> True

-- | Map an action over a list, and keep the list when the action gives
-- each element back as it is.
mapMSame :: (Monad m) => (a -> m a) -> [a] -> m [a]
mapMSame f list =
  case list of
    [] -> pure list
    x : rest -> do
      x' <- f x
      rest' <- mapMSame f rest
      pure (if sameObject x x' && sameObject rest rest' then list else x' : rest')

-- | Whether two values are the same heap object. Two different objects
-- can be equal, so a 'False' only means that a walk makes a new object.
sameObject :: a -> a -> Bool
sameObject left right = isTrue# (reallyUnsafePtrEquality# left right)
{-# INLINE sameObject #-}

-- | Rewrite the type family applications in a predicate.
reducePredFamilies :: Pred -> TcM Pred
reducePredFamilies predicate =
  case predicate of
    ClassPred className arguments -> do
      arguments' <- mapMSame reduceTypeFamilies arguments
      pure (if sameObject arguments arguments' then predicate else ClassPred className arguments')
    EqPred left right -> do
      left' <- reduceTypeFamilies left
      right' <- reduceTypeFamilies right
      pure (if sameObject left left' && sameObject right right' then predicate else EqPred left' right')
    IParamPred name payload -> do
      payload' <- reduceTypeFamilies payload
      pure (if sameObject payload payload' then predicate else IParamPred name payload')
    IrredPred constraint -> do
      constraint' <- reduceTypeFamilies constraint
      pure (if sameObject constraint constraint' then predicate else IrredPred constraint')
    QuantifiedPred variables antecedents consequent -> do
      antecedents' <- mapMSame reducePredFamilies antecedents
      consequent' <- reducePredFamilies consequent
      pure (if sameObject antecedents antecedents' && sameObject consequent consequent' then predicate else QuantifiedPred variables antecedents' consequent')

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

-- | The ordinary predicate that a stuck constraint type has become, if
-- any. A saturated family application is still stuck. A family
-- application with more arguments than the family arity is also still
-- stuck: @MinCtxt v a@ applies the stuck @MinCtxt v@ to @a@. A class
-- application is a class predicate: the head was a type family that
-- reduced, or a class variable that an instantiation solved.
irreduciblePred :: TcType -> TcM (Maybe Pred)
irreduciblePred ty = do
  spine <- unsaturateFamilyApplication ty
  stillStuck <- isTypeFamilyApplication (applicationHead spine)
  if stillStuck
    then pure Nothing
    else do
      kinds <- getKinds
      pure $ case constraintTypeToPred kinds ty of
        Just IrredPred {} -> Nothing
        predicate -> predicate

-- | Reclassify an irreducible predicate whose constraint type is now an
-- ordinary predicate. The solver compares a wanted and a given
-- structurally, and the desugarer keys a given dictionary by its
-- predicate, so @q b@ with @q@ solved to @Eq@ must be the same predicate
-- as @Eq b@ on both sides.
reclassifyIrreduciblePred :: Pred -> TcM Pred
reclassifyIrreduciblePred predicate =
  case predicate of
    IrredPred constraint -> do
      reclassified <- irreduciblePred constraint
      pure $ case reclassified of
        -- A constraint variable keeps lifted dictionary evidence after specialization.
        Just EqPred {} -> predicate
        _ -> fromMaybe predicate reclassified
    _ -> pure predicate

-- | The head of an application spine.
applicationHead :: TcType -> TcType
applicationHead ty =
  case ty of
    TcAppTy function _ -> applicationHead function
    _ -> ty

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

-- | Whether a meta variable occurs in a type outside every saturated type
-- family application.
--
-- The occurs check fails only for such an occurrence. An occurrence in a
-- family argument does not make the equality insoluble: the application
-- can reduce and remove it. For example, @m ~ ST (PrimState m)@ holds
-- when @m@ is @ST s@, because @PrimState (ST s)@ reduces to @s@.
occursOutsideFamilies :: TcM (Unique -> TcType -> Bool)
occursOutsideFamilies = do
  tyCons <- lift $ gets tcsGlobalTyCons
  let familyArity tyCon =
        case Map.lookup (tyConKey tyCon) tyCons of
          Just info | tciFlavor info == TypeFamilyTyCon -> Just (tciArity info)
          _ -> Nothing
      occurs u = go
        where
          go ty =
            case ty of
              TcMetaTv u' -> u == u'
              TcTyCon tyCon arguments
                | Just arity <- familyArity tyCon,
                  length arguments >= arity ->
                    any go (drop arity arguments)
                | otherwise -> any go arguments
              TcFunTy argument result -> go argument || go result
              TcForAllTy _ body -> go body
              TcQualTy predicates body -> any goPred predicates || go body
              TcAppTy function argument -> go function || go argument
              _ -> False
          goPred predicate =
            case predicate of
              ClassPred _ arguments -> any go arguments
              EqPred left right -> go left || go right
              IParamPred _ payload -> go payload
              IrredPred constraint -> go constraint
              QuantifiedPred variables antecedents consequent ->
                any (go . tvKind) variables || any goPred antecedents || goPred consequent
  pure occurs

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
                  kindEnv <- lift $ gets (Map.map tciKindScheme . tcsGlobalTyCons)
                  let kindOf = either (const Nothing) Just . typeKindInEnv kinds kindEnv
                      argumentKinds = either (const Nothing) (Just . take (length familyArguments) . tcVisibleArgumentKinds) (typeApplicationKinds kinds kindEnv tyCon familyArguments Nothing)
                  case argumentKinds >>= \actualKinds -> firstEquation family kindOf equations familyArguments actualKinds extraArguments of
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
  | Just simplified <- simplifyTypeLitFamily (tyConName tyCon) literal TcTyLit arguments = Just simplified
  | otherwise = do
      literals <- traverse literal arguments
      value <- evaluateTypeLitFamily (tyConName tyCon) literals
      pure $ case value of
        TypeLitSymbol symbol -> TcTyLit (TyLitSymbol symbol)
        TypeLitChar char -> TcTyLit (TyLitChar char)
        TypeLitNatural natural -> TcTyLit (TyLitNat natural)
        TypeLitOrdering ordering -> TcTyCon (kindsDataCon kinds (T.pack (show ordering)) 0) []
  where
    literal ty =
      case ty of
        TcTyLit value -> Just value
        _ -> Nothing

-- | The equations of a type family, in declaration order.
familyEquations :: TyCon -> TcM [TypeFamilyInstanceInfo]
familyEquations tyCon = sortOn axiomIndex . Map.elems <$> getTypeFamilyEquations tyCon

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
firstEquation :: (TyCon -> Bool) -> (TcType -> Maybe TcType) -> [TypeFamilyInstanceInfo] -> [TcType] -> [TcType] -> [TcType] -> Maybe TcType
firstEquation family kindOf equations arguments argumentKinds extraArguments =
  case equations of
    [] -> Nothing
    equation : rest ->
      case equationArguments equation of
        Just patterns
          | Just patternKinds <- traverse kindOf patterns,
            Just kindSubstitution <- matchTypes patternKinds argumentKinds,
            Just substitution <- matchTypes patterns arguments,
            let variableSubstitution = variableKinds equation substitution,
            Just resultSubstitution <- matchResultKinds equation (substitution <> kindSubstitution <> variableSubstitution) ->
              Just (applySubst (substitution <> kindSubstitution <> variableSubstitution <> resultSubstitution) (tfiiRight equation))
          | tfiiClosed equation,
            couldUnifyAll family patterns arguments,
            maybe True (and . zipWith (couldUnify family) argumentKinds) (traverse kindOf patterns) ->
              Nothing
        _ -> firstEquation family kindOf rest arguments argumentKinds extraArguments
  where
    -- A kind variable that only the kind of an equation variable names:
    -- in @Rep (C b a) = ... (b (Const a))@ with @b :: (k -> Type) -> Type@,
    -- the right-hand side applies @Const@ at @k@, but no argument kind
    -- mentions @k@. The kind of the type that @b@ matched gives it.
    variableKinds equation substitution =
      let pairs =
            [ (tvKind variable, actualKind)
            | variable <- tfiiTyVars equation,
              Just actual <- [Map.lookup (tvUnique variable) substitution],
              Just actualKind <- [kindOf actual]
            ]
       in fromMaybe Map.empty (matchTypes (map fst pairs) (map snd pairs))
    matchResultKinds _ _ | null extraArguments = Just Map.empty
    matchResultKinds equation substitution = do
      resultKind <- applySubst substitution <$> kindOf (tfiiLeft equation)
      extraKinds <- traverse kindOf extraArguments
      matchTypes (take (length extraKinds) (argumentKindsOf resultKind)) extraKinds
    argumentKindsOf (TcFunTy argument result) = argument : argumentKindsOf result
    argumentKindsOf _ = []

equationArguments :: TypeFamilyInstanceInfo -> Maybe [TcType]
equationArguments info =
  case tfiiLeft info of
    TcTyCon _ patterns -> Just patterns
    TcKindedTyCon {} -> Just []
    _ -> Nothing

-- | Whether patterns could match types once more is known about them.
--
-- As in GHC's apartness check, every type variable of the target counts
-- as unifiable -- a skolem here is instantiated elsewhere -- and so does a
-- stuck type family application, which may reduce to anything. A pattern
-- variable that occurs more than once, as @d@ in @d / d@, binds types
-- that must unify with each other. Thus @'Dim 'Pos1 / 'Dim 'Zero@ is apart
-- from @d / d@.
couldUnifyAll :: (TyCon -> Bool) -> [TcType] -> [TcType] -> Bool
couldUnifyAll family patterns targets =
  case concat <$> zipWithM (patternBindings family) patterns targets of
    Nothing -> False
    Just bindings ->
      all
        consistent
        (Map.elems (Map.fromListWith (<>) [(tvUnique variable, [target]) | (variable, target) <- bindings]))
  where
    consistent bound = and [typesCouldUnify family left right | (left, rest) <- picks bound, right <- rest]
    picks values = case values of
      [] -> []
      value : rest -> (value, rest) : picks rest

couldUnify :: (TyCon -> Bool) -> TcType -> TcType -> Bool
couldUnify family patternType target = couldUnifyAll family [patternType] [target]

-- | The types that the pattern variables bind when a pattern could match a
-- type, or 'Nothing' when the two are apart.
patternBindings :: (TyCon -> Bool) -> TcType -> TcType -> Maybe [(TyVarId, TcType)]
patternBindings family = go
  where
    go patternType target =
      case (patternType, target) of
        (TcTyVar variable, _) -> Just [(variable, target)]
        (_, TcTyVar _) -> Just []
        (_, TcMetaTv _) -> Just []
        (_, TcTyCon targetTyCon _) | family targetTyCon -> Just []
        (TcTyCon tyCon arguments, TcTyCon targetTyCon targetArguments)
          | tyCon == targetTyCon && length arguments == length targetArguments ->
              concat <$> zipWithM go arguments targetArguments
          | otherwise -> Nothing
        -- The kind arguments are kinds, and a kind can still change.
        (TcKindedTyCon tyCon _, TcKindedTyCon targetTyCon _) -> sameTyCon tyCon targetTyCon
        (TcKindedTyCon tyCon _, TcTyCon targetTyCon []) -> sameTyCon tyCon targetTyCon
        (TcTyCon tyCon [], TcKindedTyCon targetTyCon _) -> sameTyCon tyCon targetTyCon
        (TcFunTy argument result, TcFunTy targetArgument targetResult) ->
          (<>) <$> go argument targetArgument <*> go result targetResult
        (TcAppTy function argument, TcAppTy targetFunction targetArgument) ->
          (<>) <$> go function targetFunction <*> go argument targetArgument
        _
          | patternType == target -> Just []
          | otherwise -> Nothing
    sameTyCon tyCon targetTyCon = if tyCon == targetTyCon then Just [] else Nothing

-- | Whether two types that one pattern variable binds could become equal.
-- A type variable, a meta variable, or a stuck family application on
-- either side can become anything.
typesCouldUnify :: (TyCon -> Bool) -> TcType -> TcType -> Bool
typesCouldUnify family = go
  where
    open ty = case ty of
      TcTyVar _ -> True
      TcMetaTv _ -> True
      TcTyCon tyCon _ -> family tyCon
      _ -> False
    go left right
      | open left || open right = True
      | otherwise =
          case (left, right) of
            (TcTyCon tyCon arguments, TcTyCon tyCon' arguments') ->
              tyCon == tyCon' && length arguments == length arguments' && and (zipWith go arguments arguments')
            (TcKindedTyCon tyCon _, TcKindedTyCon tyCon' _) -> tyCon == tyCon'
            (TcKindedTyCon tyCon _, TcTyCon tyCon' []) -> tyCon == tyCon'
            (TcTyCon tyCon [], TcKindedTyCon tyCon' _) -> tyCon == tyCon'
            (TcFunTy argument result, TcFunTy argument' result') -> go argument argument' && go result result'
            (TcAppTy function argument, TcAppTy function' argument') -> go function function' && go argument argument'
            _ -> left == right
