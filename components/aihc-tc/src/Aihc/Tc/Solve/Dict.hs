{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Dictionary (class constraint) solver.
--
-- For the MVP, this is a stub. The full implementation will match
-- wanted class constraints against given dictionaries and instance
-- declarations.
module Aihc.Tc.Solve.Dict
  ( solveDict,
    solveDictWithGivens,
    DictResult (..),
    callStackOrigin,
    isDefaultedImplicitParam,
    reportUnsolvedDict,
    splitTypeable,
    classFieldTypes,
    mostSpecificInstances,
    matchInstanceKinds,
  )
where

import Aihc.Parser.Syntax (pattern SourceSpan)
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Annotations (renderTcType)
import Aihc.Tc.Constraint
import Aihc.Tc.Env (ClassInfo (..), InstanceInfo (..), TyConInfo (..), classFieldTypes, superClassPred)
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Evidence (CallSite (..), Coercion (..), EvTerm (..), TypeableKind (..), TypeableTyCon (..))
import Aihc.Tc.Instantiate (Instantiation (..), instantiateWithArgs)
import Aihc.Tc.Kind (bindKindMeta, tcTypeKind, unifyKinds, zonkKind)
import Aihc.Tc.Match (matchTypes)
import Aihc.Tc.Monad (TcM, abortTc, bindEvidence, emitError, freshEvVar, freshMetaTvOfKind, freshSkolemTv, getClassInstances, getGivenPredicates, getKinds, getRecursiveDictionaries, getWiring, implicitParamType, lookupClass, lookupClassByName, lookupEvidence, lookupTyConByIdentity, wiredTyCon, wiredTyConIdentity, withErrorTracking, withGivenPredicates, withRecursiveDictionary)
import Aihc.Tc.Solve.Coercible (isCoercibleClass, solveCoercible, solveCoercibleFromGivens)
import Aihc.Tc.Solve.Congruence (givenEqualities)
import Aihc.Tc.Solve.Decompose (decomposeNominalEquality)
import Aihc.Tc.Solve.Equality (EqResult (..), solveEquality)
import Aihc.Tc.Solve.Family (irreduciblePred, isTypeFamilyApplication, normalizeFamilyPred, reclassifyIrreduciblePred, reducePredFamilies, reduceTypeFamilies)
import Aihc.Tc.Solve.FunDep (improveFunDeps, superClassClosure)
import Aihc.Tc.Solve.KindMatch (matchInstanceKinds, matchKinds)
import Aihc.Tc.Types
import Aihc.Tc.Unify (unifyDeferring)
import Aihc.Tc.Wiring (TcWiring (..))
import Aihc.Tc.Zonk (zonkPred, zonkType)
import Control.Applicative ((<|>))
import Control.Monad (foldM, foldM_, guard, when, zipWithM_, (<=<), (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.State.Strict (get, put)
import Data.List (elemIndex, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T

-- | Result of attempting to solve a dictionary constraint.
data DictResult
  = -- | Solved by given or instance.
    DictSolved
  | -- | Cannot solve yet; leave in inert set.
    DictStuck !Ct
  deriving (Show)

-- | Attempt to solve a dictionary (class) constraint.
--
-- This covers the Haskell 2010 instance path used by the current Prelude:
-- match a wanted class predicate against an in-scope instance head, solve the
-- instance context recursively, and bind the wanted evidence to a dictionary
-- term. The plain entry point has no local givens; annotation generation uses
-- 'solveDictWithGivens' when elaborating inside a qualified binding.
solveDict :: Ct -> TcM DictResult
solveDict = solveDictWithGivens []

-- | A wanted that left a pattern branch keeps the givens of the branch,
-- so they join the givens of the caller.
solveDictWithGivens :: [Pred] -> Ct -> TcM DictResult
solveDictWithGivens givens ct = do
  outerGivens <- getGivenPredicates
  solveDictWithGivensVisited [] (givens <> map ctPred (ctBranchGivens ct) <> outerGivens) ct

solveDictWithGivensVisited :: [Pred] -> [Pred] -> Ct -> TcM DictResult
solveDictWithGivensVisited visited givens ct0 = do
  -- A given that came from a stored superclass type may still be headed by
  -- a family; see 'normalizeFamilyPred'.
  givens' <- mapM normalizeFamilyPred givens
  predicate <- normalizeFamilyPred (ctPred ct0)
  solveNormalizedDict visited givens' ct0 {ctPred = predicate}

solveNormalizedDict :: [Pred] -> [Pred] -> Ct -> TcM DictResult
solveNormalizedDict visited givens ct
  | ctPred ct `elem` visited = do
      -- A wanted that comes back to a dictionary that an instance is
      -- building names that dictionary. The loop goes through the
      -- instance, so the recursive dictionary is productive. Any other
      -- loop cannot be solved.
      recursive <- getRecursiveDictionaries
      if ctPred ct `elem` recursive
        then do
          bindEvidence (ctEvVar ct) (EvGiven (ctPred ct))
          pure DictSolved
        else pure (DictStuck ct)
  | otherwise =
      case ctPred ct of
        ClassPred className args -> do
          args' <- mapM (reduceTypeFamilies <=< zonkType) args
          coercibleClass <- isCoercibleClass className
          withDictClass <- (className ==) <$> wiredTyConIdentity tcWiringWithDictTyCon
          givens' <- mapM zonkGivenPred givens
          givenEvidence <- givenDict (ctPred ct : visited) givens' className args'
          case givenEvidence of
            Just evidence -> do
              bindEvidence (ctEvVar ct) evidence
              pure DictSolved
            Nothing ->
              case (tyConName className, args') of
                (_, [constraintType, methodType]) | withDictClass -> tryWithDict className constraintType methodType
                (_, [left, right]) | coercibleClass -> do
                  info <- lookupClass className
                  solved <- case info of
                    Just classInfo
                      | null (ciMethods classInfo),
                        null (ciSuperClassTypes classInfo) -> do
                          -- A Coercible superclass of a given is a given
                          -- too: @class Coercible a (Old a) => Newtype a@
                          -- gives @Coercible (Old a) a@ by symmetry.
                          closure <- superClassClosure givens'
                          direct <- solveCoercible className closure (nestedCoercible className givens') left right
                          if direct
                            then pure True
                            else solveCoercibleFromGivens className closure left right
                    _ -> pure False
                  if solved
                    then do
                      bindEvidence (ctEvVar ct) (EvCoercible className left right)
                      pure DictSolved
                    else pure (DictStuck ct)
                ("Typeable", [ty]) -> do
                  result <- solveThroughGivenEqualities (ctPred ct : visited) givens' className args'
                  case result of
                    DictSolved -> pure DictSolved
                    DictStuck _ -> tryTypeable className ty
                ("KnownNat", [ty]) -> tryTypeLitOrGivens (ctPred ct : visited) givens' className args' "KnownNat" isNatLiteral ty
                ("KnownSymbol", [ty]) -> tryTypeLitOrGivens (ctPred ct : visited) givens' className args' "KnownSymbol" isSymbolLiteral ty
                ("KnownChar", [ty]) -> tryTypeLitOrGivens (ctPred ct : visited) givens' className args' "KnownChar" isCharLiteral ty
                _ -> do
                  instances <- getClassInstances className
                  result <- tryInstances (ctPred ct : visited) className args' (mostSpecificInstances args' instances)
                  case result of
                    DictSolved -> pure DictSolved
                    DictStuck _ -> solveThroughGivenEqualities (ctPred ct : visited) givens' className args'
        IrredPred constraint -> do
          -- A stuck constraint says nothing until its families reduce. Once
          -- they do it is an ordinary constraint -- a class, an equality, or
          -- the empty constraint tuple -- and the ordinary machinery solves
          -- it and builds its dictionary.
          reduced <- reduceTypeFamilies =<< zonkType constraint
          reclassified <- irreduciblePred reduced
          case reclassified of
            Just equality@(EqPred left right) -> do
              -- A constraint family can reduce to an equality.
              -- Its dictionary contains the checked coercion as a field.
              proof <- freshEvVar
              result <- withGivenPredicates givens (solveEquality ct {ctPred = equality, ctEvVar = proof})
              case result of
                EqSolved -> do
                  kinds <- getKinds
                  let constructor = case collectTypeApplications reduced of
                        (TcTyCon tyCon _, _) | isEqualityTyCon kinds tyCon -> tyCon
                        _ -> kindsEqualityTyCon kinds
                  checkedProof <- lookupEvidence proof
                  case checkedProof of
                    Just (EvCoercion coercion) -> bindEvidence (ctEvVar ct) (EvEqualityDict constructor left right coercion)
                    _ -> abortTc "equality dictionary has no checked coercion"
                  pure DictSolved
                _ -> pure (DictStuck ct {ctPred = IrredPred reduced})
            Just solvable ->
              solveDictWithGivensVisited (ctPred ct : visited) givens ct {ctPred = solvable}
            Nothing -> do
              -- A stuck constraint is discharged by a given of the same
              -- shape, or by a superclass of one: @1 <= SeedSize g@ is a
              -- superclass of @SeedGen g@, and an instance for @SeedGen
              -- (StateGen g)@ owes it for @SeedSize (StateGen g)@, which
              -- reduces to the given's.
              givens' <- mapM zonkGivenPred givens
              evidence <- firstGivenOrSuperclass (ctPred ct : visited) (IrredPred reduced) givens'
              case evidence of
                Just given -> do
                  bindEvidence (ctEvVar ct) given
                  pure DictSolved
                Nothing -> do
                  rules <- givenRewriteRuleSets givens'
                  -- The rules can rewrite a stuck given to the wanted: with
                  -- the given @p ~ Wrapped arr@, the given @Constraints k p@
                  -- is the wanted @Constraints k (Wrapped arr)@.
                  fromGivens <- firstRuleSet (\ruleSet -> rewrittenIrreducibleGiven (ctPred ct : visited) ruleSet (IrredPred reduced) givens') rules
                  case fromGivens of
                    Just given -> do
                      bindEvidence (ctEvVar ct) given
                      pure DictSolved
                    Nothing -> tryIrreducibleRules (ctPred ct : visited) givens' reduced rules
        quantified@QuantifiedPred {} -> solveQuantifiedWanted visited givens quantified
        EqPred {} -> pure (DictStuck ct)
        IParamPred name payload -> do
          payload' <- zonkType payload
          givens' <- mapM zonkGivenPred givens
          -- The innermost binding of the name wins. Givens are outermost first.
          case [given | given@(IParamPred givenName _) <- reverse givens', givenName == name] of
            given@(IParamPred _ givenPayload) : _ -> do
              -- The name determines the type of an implicit parameter.
              -- A type family application in the payload can stay stuck
              -- until a later improvement solves a meta variable, as
              -- @Token s@ waits for a functional dependency to find @s@.
              -- Then the wanted stays and the solver tries it again.
              deferred <- unifyDeferring (ctLoc ct) (ctOrigin ct) payload' givenPayload
              if null deferred
                then do
                  bindEvidence (ctEvVar ct) (implicitParamEvidence ct name givenPayload (EvGiven given))
                  pure DictSolved
                else pure (DictStuck ct)
            _ -> pure (DictStuck ct)
  where
    -- A @WithDict cls meth@ wanted is solved when @cls@ is a class with one
    -- method and no superclasses, and @meth@ equals the type of that method.
    tryWithDict adapter constraintType methodType = do
      target <- reclassifyIrreduciblePred (IrredPred constraintType)
      evidence <- runMaybeT $ do
        ClassPred targetClass targetArguments <- pure target
        adapterInfo <- MaybeT (lookupClass adapter)
        targetInfo <- MaybeT (lookupClass targetClass)
        guard (null (ciSuperClassTypes targetInfo) && length (ciMethods targetInfo) == 1)
        guard (length targetArguments == length (ciTyVars targetInfo))
        substitution <- MaybeT (matchInstanceKinds (ciTyVars targetInfo) (classArguments targetInfo targetArguments))
        [adapterField] <- pure (classFieldTypes adapterInfo (classArguments adapterInfo [constraintType, methodType]))
        [targetMethod] <- pure (classFieldTypes targetInfo substitution)
        Just (EvCoercion proof) <- lift (solveSubPred visited (EqPred methodType targetMethod))
        pure (EvWithDict adapter [constraintType, methodType] adapterField targetClass targetArguments proof)
      case evidence of
        Just solved -> do
          bindEvidence (ctEvVar ct) solved
          pure DictSolved
        Nothing -> pure (DictStuck ct)

    classArguments info arguments = Map.fromList (zip (map tvUnique (ciTyVars info)) arguments)

    givenDict visited' zonkedGivens className args =
      firstGivenOrSuperclass visited' (ClassPred className args) zonkedGivens

    -- A given equality with a type family application on one side rewrites
    -- that application in the wanted: @Token s ~ Word8@ turns @Num (Token
    -- s)@ into @Num Word8@, which an instance solves, and @a ~ Tokens s@
    -- turns @IsString (Tokens s)@ into @IsString a@, which is a given. A
    -- given equality between two rigid variables, or between a rigid
    -- variable and another type, rewrites the variable in the same way:
    -- @a ~ b@ turns @HasSetter t a@ into @HasSetter t b@, which the
    -- superclass of a given @HasUpdate t a b@ solves. Such an equality has
    -- no preferred side, so each direction is tried as an alternative. The
    -- evidence for the rewritten wanted is cast back to the original
    -- predicate along the congruence of the given coercions.
    tryIrreducibleRules _ _ reduced [] = pure (DictStuck ct {ctPred = IrredPred reduced})
    tryIrreducibleRules visited' localGivens reduced (rules : rest) = do
      let (rewritten, proof) = rewriteWithRules rules reduced
      if sameType rewritten reduced
        then tryIrreducibleRules visited' localGivens reduced rest
        else do
          saved <- lift get
          variable <- freshEvVar
          result <- solveDictWithGivensVisited visited' localGivens ct {ctPred = IrredPred rewritten, ctEvVar = variable}
          case result of
            DictStuck _ -> do
              lift (put saved)
              tryIrreducibleRules visited' localGivens reduced rest
            DictSolved -> do
              evidence <- lookupEvidence variable
              case evidence of
                Just term -> do
                  bindEvidence (ctEvVar ct) (EvCast term (Sym proof))
                  pure DictSolved
                Nothing -> pure (DictStuck ct)

    solveThroughGivenEqualities visited' zonkedGivens className args = do
      outerGivens <- mapM zonkGivenPred =<< getGivenPredicates
      let allGivens = zonkedGivens <> filter (`notElem` zonkedGivens) outerGivens
      ruleSets <- givenRewriteRuleSets allGivens
      tryRuleSets visited' zonkedGivens allGivens className args ruleSets

    tryRuleSets _ _ _ _ _ [] = pure (DictStuck ct)
    tryRuleSets visited' zonkedGivens allGivens className args (rules : rest) = do
      let (rewrittenArgs, coercions) = unzip (map (rewriteWithRules rules) args)
      -- The rules rewrite the givens as well as the wanted: with the
      -- given @d ~ Maybe x@ a wanted @C (Maybe x)@ names no @d@, but the
      -- given @C d@ rewrites to @C (Maybe x)@ and solves it. The given
      -- dictionary is cast along the congruence of the rules.
      case rewrittenGivenMatches rules allGivens className args of
        (given, givenArgs, givenCoercions) : _ -> do
          bindEvidence (ctEvVar ct) (EvCast (EvGiven given) (TyConAppCo className givenArgs givenCoercions []))
          pure DictSolved
        [] -> do
          irreducible <- rewrittenIrreducibleGiven visited' rules (ClassPred className args) allGivens
          case irreducible of
            Just evidence -> do
              bindEvidence (ctEvVar ct) evidence
              pure DictSolved
            Nothing ->
              if null rules || and (zipWith sameType rewrittenArgs args)
                then tryRuleSets visited' zonkedGivens allGivens className args rest
                else tryRewrittenWanted visited' zonkedGivens allGivens className args rest rewrittenArgs coercions

    -- The rules can also make a stuck given reduce: with the given @k ~
    -- 'Iso@, the given @Constraints k p@ rewrites to @Constraints 'Iso p@,
    -- which reduces to @Pro p@. That given, or one of its superclasses,
    -- then solves the wanted. The given dictionary is cast along the
    -- rewrite.
    firstRuleSet _ [] = pure Nothing
    firstRuleSet attempt (ruleSet : rest) = do
      found <- attempt ruleSet
      maybe (firstRuleSet attempt rest) (pure . Just) found

    rewrittenIrreducibleGiven _ _ _ [] = pure Nothing
    rewrittenIrreducibleGiven visited' rules target (given : rest) =
      case given of
        IrredPred constraint
          | let (rewritten, proof) = rewriteWithRules rules constraint,
            not (sameType rewritten constraint) -> do
              normalized <- normalizeGiven (IrredPred rewritten)
              let evidence = EvCast (EvGiven given) proof
              found <-
                if samePredicate normalized target
                  then pure (Just evidence)
                  else superclassEvidence [] visited' target evidence normalized
              maybe (rewrittenIrreducibleGiven visited' rules target rest) (pure . Just) found
        -- A class given rewrites in the same way, and a superclass of the
        -- rewritten given can solve the wanted: with the given @a ~ ()@,
        -- the given @Gen (m a)@ rewrites to @Gen (m ())@, and its
        -- superclass is the wanted @Str (m ())@.
        ClassPred givenClass givenArgs
          | let (rewrittenArgs, coercions) = unzip (map (rewriteWithRules rules) givenArgs),
            not (and (zipWith sameType rewrittenArgs givenArgs)) -> do
              let evidence = EvCast (EvGiven given) (TyConAppCo givenClass givenArgs coercions [])
              found <- superclassEvidence [] visited' target evidence (ClassPred givenClass rewrittenArgs)
              maybe (rewrittenIrreducibleGiven visited' rules target rest) (pure . Just) found
        _ -> rewrittenIrreducibleGiven visited' rules target rest

    rewrittenGivenMatches rules allGivens className args =
      [ (given, givenArgs, givenCoercions)
      | given@(ClassPred givenClass givenArgs) <- allGivens,
        givenClass == className,
        not (and (zipWith sameType givenArgs args)),
        let (rewrittenGivenArgs, givenCoercions) = unzip (map (rewriteWithRules rules) givenArgs),
        and (zipWith sameType rewrittenGivenArgs args)
      ]

    tryRewrittenWanted visited' zonkedGivens allGivens className args rest rewrittenArgs coercions = do
      saved <- lift get
      rewrittenEvidence <- freshEvVar
      let rewritten = ClassPred className rewrittenArgs
      result <- solveDictWithGivensVisited visited' zonkedGivens (ct {ctPred = rewritten, ctEvVar = rewrittenEvidence})
      case result of
        DictStuck _ -> do
          -- A failed alternative must not change another alternative's types or evidence.
          lift (put saved)
          tryRuleSets visited' zonkedGivens allGivens className args rest
        DictSolved -> do
          inner <- lookupEvidence rewrittenEvidence
          case inner of
            Nothing -> pure (DictStuck ct)
            Just evidence -> do
              bindEvidence (ctEvVar ct) (EvCast evidence (Sym (TyConAppCo className args coercions [])))
              pure DictSolved

    -- A given that does not match as written is compared in the normal
    -- form the wanted has: its families reduced. The given @NatWithinBound
    -- Word64 n@ reduces to the same stuck @If@ application as a wanted at
    -- the same rigid @n@, and only then are the two equal. The evidence
    -- names the given as written, because that is the dictionary the
    -- desugarer binds.
    firstGivenOrSuperclass visited' target givens' = do
      asWritten <- firstGivenOrSuperclassAsWritten visited' target givens'
      case asWritten of
        Just evidence -> pure (Just evidence)
        Nothing -> do
          normalized <- mapM normalizeGiven givens'
          normalizedEvidence (zip givens' normalized)
      where
        normalizedEvidence [] = pure Nothing
        normalizedEvidence ((given, normalized) : rest)
          | samePredicate normalized target = pure (Just (EvGiven given))
          | otherwise = do
              quantified <- useQuantifiedEvidence visited' target (EvGiven given) normalized
              projected <- superclassEvidence [] visited' target (EvGiven given) normalized
              case quantified <|> projected of
                Just evidence -> pure (Just evidence)
                Nothing -> normalizedEvidence rest

    normalizeGiven given = do
      familyPredicate <- normalizeFamilyPred given
      reduced <- reducePredFamilies familyPredicate
      normalized <- normalizeFamilyPred reduced
      case normalized of
        IrredPred constraint -> do
          reclassified <- irreduciblePred constraint
          pure (fromMaybe normalized reclassified)
        _ -> pure normalized

    firstGivenOrSuperclassAsWritten _ _ [] = pure Nothing
    firstGivenOrSuperclassAsWritten visited' target (given : rest)
      | target == given = pure (Just (EvGiven given))
      | otherwise = do
          quantified <- useQuantifiedEvidence visited' target (EvGiven given) given
          projected <- superclassEvidence [] visited' target (EvGiven given) given
          case quantified <|> projected of
            Just evidence -> pure (Just evidence)
            Nothing -> firstGivenOrSuperclassAsWritten visited' target rest

    superclassEvidence classVisited solveVisited target sourceEvidence sourcePredicate =
      case sourcePredicate of
        ClassPred sourceClass sourceArgs
          | sourceClass `elem` classVisited -> pure Nothing
          | otherwise -> do
              classInfo <- lookupClass sourceClass
              case classInfo of
                Nothing -> pure Nothing
                Just info -> do
                  kinds <- getKinds
                  let visibleSubstitution = Map.fromList [(tvUnique tyVar, argument) | (tyVar, argument) <- zip (ciTyVars info) sourceArgs]
                  substitution <- fromMaybe visibleSubstitution <$> matchInstanceKinds (ciTyVars info) visibleSubstitution
                  let fieldTypes = classFieldTypes info substitution
                  case traverse (superClassPred kinds substitution) (ciSuperClassTypes info) of
                    Just superClasses -> do
                      -- A superclass is compared in the same normal form as
                      -- the wanted: families reduced, and a family-headed
                      -- one irreducible rather than a class predicate.
                      normalized <- mapM normalizeGiven superClasses
                      searchSuperClasses (sourceClass : classVisited) solveVisited sourceEvidence (ciOrigin info) sourcePredicate fieldTypes target 0 normalized
                    Nothing -> pure Nothing
        _ -> pure Nothing

    searchSuperClasses _ _ _ _ _ _ _ _ [] = pure Nothing
    searchSuperClasses classVisited solveVisited sourceEvidence sourceOrigin sourcePredicate fieldTypes target index (superClass : rest)
      | samePredicate superClass target =
          pure (Just (EvSuperClass sourceEvidence sourceOrigin sourcePredicate fieldTypes index))
      | otherwise = do
          let projection = EvSuperClass sourceEvidence sourceOrigin sourcePredicate fieldTypes index
          quantified <- useQuantifiedEvidence solveVisited target projection superClass
          nested <- superclassEvidence classVisited solveVisited target projection superClass
          case quantified <|> nested of
            Just evidence -> pure (Just evidence)
            Nothing -> searchSuperClasses classVisited solveVisited sourceEvidence sourceOrigin sourcePredicate fieldTypes target (index + 1) rest

    tryInstances _ _ _ [] = pure (DictStuck ct)
    tryInstances visited' className args (instanceInfo : rest) = do
      saved <- lift get
      matched <- case matchTypes (iiHead instanceInfo) args of
        Nothing -> pure Nothing
        Just substitution -> matchInstanceKinds (iiTyVars instanceInfo) substitution
      case matched of
        Nothing -> tryInstances visited' className args rest
        Just headSubst -> do
          -- A variable that the head does not bind takes its type from the
          -- context, through the functional dependencies of the class that
          -- constrains it, as @all@ does in @(AllNullary a l, AllNullary b
          -- r, And l r all) => AllNullary (a :+: b) all@. It is a meta
          -- variable until the context determines it.
          unboundMetas <-
            mapM
              (\tyVar -> (,) (tvUnique tyVar) <$> freshMetaTvOfKind (applySubst headSubst (tvKind tyVar)))
              (filter (\tyVar -> not (Map.member (tvUnique tyVar) headSubst)) (iiTyVars instanceInfo))
          let subst = headSubst <> Map.fromList unboundMetas
          settleHeadKinds subst [unique | (_, TcMetaTv unique) <- unboundMetas] (iiHead instanceInfo)
          let context = map (applySubstPred subst) (iiContext instanceInfo)
              typeArgs = map (applySubst subst . TcTyVar) (iiTyVars instanceInfo)
          (contextEvidence, failed) <- withErrorTracking (withRecursiveDictionary (ctPred ct) (improveContext context >> solveContext visited' context))
          case contextEvidence of
            Just evidence | not failed -> do
              let dictionary = EvDict (iiDictOrigin instanceInfo) (iiDictName instanceInfo) typeArgs evidence
              if any (mentionsGiven (ctPred ct)) evidence
                then do
                  dictionaryType <- predicateType (ctPred ct)
                  bindEvidence (ctEvVar ct) (EvRecursive (ctPred ct) dictionaryType dictionary)
                else bindEvidence (ctEvVar ct) dictionary
              pure DictSolved
            _ -> do
              -- A failed candidate must not change another candidate's types or evidence.
              lift (put saved)
              tryInstances visited' className args rest

    -- A kind variable of an instance can occur only in the kind arguments
    -- of its head: @k2@ in @instance C ((:~~:) \@k1 \@k2 a)@. A partial
    -- application such as @(:~~:) Int@ in the wanted does not keep that
    -- kind argument, so the head match leaves @k2@ open. The wanted
    -- argument as it was written can still be a meta variable whose kind
    -- tells the kind, so match the kind of the head against it. Only the
    -- open variables of the instance get a binding here.
    --
    -- The solver can zonk the wanted before it gets here, so use the
    -- predicate as it was made. A constraint that the solver made from
    -- another one keeps the written predicate of the other one, so use
    -- it only when it zonks to the wanted.
    settleHeadKinds subst openMetas heads
      | null openMetas = pure ()
      | otherwise = do
          written <- zonkPred (ctWrittenPred ct)
          current <- zonkPred (ctPred ct)
          case ctWrittenPred ct of
            ClassPred _ writtenArgs
              | written == current,
                length writtenArgs == length heads ->
                  zipWithM_ settleHeadKind heads writtenArgs
            _ -> pure ()
      where
        settleHeadKind headType writtenArg = do
          headKind <- tcTypeKind (applySubst subst headType) >>= zonkKind
          writtenKind <- tcTypeKind writtenArg >>= zonkKind
          case matchKinds headKind writtenKind of
            Just (_, metas) -> mapM_ (uncurry bindKindMeta) [binding | binding@(unique, _) <- metas, unique `elem` openMetas]
            Nothing -> pure ()

    -- Let the constraints of an instance context improve each other before
    -- any of them is solved. One constraint can fix a meta variable that
    -- another needs to select its instance. This has no effect on a
    -- context without metas or without functional dependencies.
    improveContext predicates = do
      let dictionaries = [predicate | predicate@ClassPred {} <- predicates]
      cts <- mapM (\predicate -> (\ev -> ct {ctPred = predicate, ctEvVar = ev}) <$> freshEvVar) dictionaries
      improveRounds (8 :: Int) cts
      where
        improveRounds rounds cts
          | rounds <= 0 || null cts = pure ()
          | otherwise = do
              changed <- improveFunDeps givens cts
              when changed (improveRounds (rounds - 1) cts)

    -- Solve equalities first, but retain the dictionary field order.
    solveContext visited' predicates = do
      results <- mapM solveOne (sortOn (isDictionary . snd) (zip [0 :: Int ..] predicates))
      pure (traverse snd (sortOn fst results))
      where
        solveOne (index, predicate) = do
          evidence <- solveSubPred visited' predicate
          pure (index, evidence)
        isDictionary EqPred {} = False
        isDictionary _ = True

    solveSubPred visited' pred' = do
      ev <- freshEvVar
      case pred' of
        EqPred {} -> do
          result <- withGivenPredicates givens (solveEquality (ct {ctPred = pred', ctEvVar = ev}))
          case result of
            EqSolved -> lookupEvidence ev
            _ -> pure Nothing
        _ -> do
          result <- solveDictWithGivensVisited visited' givens (ct {ctPred = pred', ctEvVar = ev})
          case result of
            DictSolved -> lookupEvidence ev
            DictStuck _ -> pure Nothing

    -- A literal argument builds the dictionary. Otherwise a given
    -- equality can still rewrite the argument to one a given names.
    tryTypeLitOrGivens visited' zonkedGivens className args classNameText matchesSort ty = do
      result <- tryTypeLit classNameText matchesSort ty
      case result of
        DictSolved -> pure DictSolved
        DictStuck _ -> solveThroughGivenEqualities visited' zonkedGivens className args

    -- A @KnownNat@, @KnownSymbol@, or @KnownChar@ constraint is solved when its argument
    -- is a literal of the matching sort. The dictionary carries the
    -- literal's value, which the desugarer builds; nothing here needs the
    -- class declaration beyond where it comes from.
    tryTypeLit classNameText matchesSort ty = do
      zonked <- zonkType ty
      case zonked of
        TcTyLit literal
          | matchesSort literal -> do
              classOrigin <- maybe Nothing ciOrigin <$> lookupClassByName classNameText
              bindEvidence (ctEvVar ct) (EvTypeLit classOrigin zonked literal)
              pure DictSolved
        _ -> pure (DictStuck ct)

    tryTypeable typeableTyCon ty = do
      kinds <- getKinds
      kind <- tcTypeKind ty >>= zonkKind
      let literalClasses =
            [ (kindsNaturalTyCon kinds, "KnownNat", "typeRepNat"),
              (kindsSymbolTyCon kinds, "KnownSymbol", "typeRepSymbol"),
              (kindsCharTyCon kinds, "KnownChar", "typeRepChar")
            ]
      case [(className, helper) | (constructor, className, helper) <- literalClasses, sameType kind (TcTyCon constructor [])] of
        (className, helper) : _ -> do
          info <- lookupClassByName className
          evidence <- case info of
            Just classInfo -> solveSubPred [ctPred ct] (ClassPred (ciTyCon classInfo) [ty])
            Nothing -> pure Nothing
          case evidence of
            Just singleton -> do
              origin <- maybe Nothing ciOrigin <$> lookupClassByName "Typeable"
              bindEvidence (ctEvVar ct) (EvTypeableLiteral origin ty helper singleton)
              pure DictSolved
            Nothing -> pure (DictStuck ct)
        [] -> tryStructuralTypeable typeableTyCon ty

    tryStructuralTypeable typeableTyCon ty =
      case typeableArguments ty of
        -- An application whose head is not a type constructor, as @f a@
        -- with the givens @Typeable f@ and @Typeable a@, is the application
        -- of the two representations.
        Nothing
          | TcAppTy function argument <- ty -> do
              classOrigin <- maybe Nothing ciOrigin <$> lookupClassByName "Typeable"
              functionEvidence <- solveSubPred [ctPred ct] (ClassPred typeableTyCon [function])
              argumentEvidence <- solveSubPred [ctPred ct] (ClassPred typeableTyCon [argument])
              case (functionEvidence, argumentEvidence) of
                (Just functionTerm, Just argumentTerm) -> do
                  bindEvidence (ctEvVar ct) (EvTypeableApp classOrigin function argument functionTerm argumentTerm)
                  pure DictSolved
                _ -> pure (DictStuck ct)
        Nothing -> pure (DictStuck ct)
        Just arguments -> do
          classOrigin <- maybe Nothing ciOrigin <$> lookupClassByName "Typeable"
          argumentEvidence <- mapM (solveSubPred [ctPred ct] . ClassPred typeableTyCon . (: [])) arguments
          case sequence argumentEvidence of
            Just evidence -> do
              (constructor, kindArguments) <- typeableConstructor ty
              kindEvidence <- mapM (solveSubPred [ctPred ct] . ClassPred typeableTyCon . (: [])) kindArguments
              case sequence kindEvidence of
                Just solvedKinds -> do
                  bindEvidence (ctEvVar ct) (EvTypeable classOrigin ty constructor (zip kindArguments solvedKinds) evidence)
                  pure DictSolved
                Nothing -> pure (DictStuck ct)
            Nothing -> pure (DictStuck ct)

    -- An inner pair of a @Coercible@ wanted can follow from a quantified
    -- given, such as the superclass @forall a b. Coercible a b =>
    -- Coercible (m a) (m b)@ of a class. Solve the pair as a new wanted
    -- with all the given machinery. A failed attempt keeps no unifications.
    nestedCoercible className givens' left right = do
      saved <- lift get
      proof <- freshEvVar
      let predicate = ClassPred className [left, right]
      result <- solveDictWithGivensVisited (ctPred ct : visited) givens' ct {ctPred = predicate, ctEvVar = proof}
      case result of
        DictSolved -> pure True
        DictStuck _ -> False <$ lift (put saved)

    solveQuantifiedWanted visited' localGivens (QuantifiedPred variables antecedents consequent) = do
      (freshVariables, substitution) <- freshQuantifiedVariables variables
      let instantiatedAntecedents = map (applySubstPred substitution) antecedents
          instantiatedConsequent = applySubstPred substitution consequent
      consequenceVariable <- freshEvVar
      result <-
        solveDictWithGivensVisited
          (ctPred ct : visited')
          (localGivens <> instantiatedAntecedents)
          (ct {ctPred = instantiatedConsequent, ctEvVar = consequenceVariable})
      case result of
        DictStuck _ -> pure (DictStuck ct)
        DictSolved -> do
          maybeBody <- lookupEvidence consequenceVariable
          case maybeBody of
            Nothing -> pure (DictStuck ct)
            Just body -> do
              antecedentTypes <- mapM predicateType instantiatedAntecedents
              let dictionaryBody = foldr (uncurry EvDictLam) body (zip instantiatedAntecedents antecedentTypes)
                  quantifiedBody = foldr EvTypeLam dictionaryBody freshVariables
              bindEvidence (ctEvVar ct) quantifiedBody
              pure DictSolved
    solveQuantifiedWanted _ _ _ = pure (DictStuck ct)

    useQuantifiedEvidence visited' target source (QuantifiedPred variables antecedents consequent) =
      searchQuantifiedChain
        visited'
        target
        variables
        (applyQuantifiedEvidence visited' source variables antecedents)
        consequent
        []
    useQuantifiedEvidence _ _ _ _ = pure Nothing

    applyQuantifiedEvidence visited' source variables antecedents substitution = do
      let typeArguments = map (\variable -> Map.findWithDefault (TcTyVar variable) (tvUnique variable) substitution) variables
          instantiatedAntecedents = map (applySubstPred substitution) antecedents
      antecedentEvidence <- mapM (solveSubPred visited') instantiatedAntecedents
      pure $ do
        evidence <- sequence antecedentEvidence
        pure (foldl EvDictApp (foldl EvTypeApp source typeArguments) evidence)

    searchQuantifiedChain visited' target variables build sourcePredicate classVisited =
      case matchQuantifiedPredicate variables sourcePredicate target of
        Just substitution -> build substitution
        Nothing ->
          case sourcePredicate of
            ClassPred sourceClass sourceArguments
              | sourceClass `elem` classVisited -> pure Nothing
              | otherwise -> do
                  classInfo <- lookupClass sourceClass
                  case classInfo of
                    Nothing -> pure Nothing
                    Just info -> do
                      kinds <- getKinds
                      let visibleSubstitution =
                            Map.fromList
                              [ (tvUnique variable, argument)
                              | (variable, argument) <- zip (ciTyVars info) sourceArguments
                              ]
                      classSubstitution <- fromMaybe visibleSubstitution <$> matchInstanceKinds (ciTyVars info) visibleSubstitution
                      let fieldTypes = classFieldTypes info classSubstitution
                      case traverse (superClassPred kinds classSubstitution) (ciSuperClassTypes info) of
                        Nothing -> pure Nothing
                        Just superClasses -> do
                          normalized <- mapM normalizeGiven superClasses
                          searchQuantifiedSuperClasses
                            visited'
                            target
                            variables
                            build
                            sourcePredicate
                            (sourceClass : classVisited)
                            (ciOrigin info)
                            fieldTypes
                            0
                            normalized
            _ -> pure Nothing

    searchQuantifiedSuperClasses _ _ _ _ _ _ _ _ _ [] = pure Nothing
    searchQuantifiedSuperClasses visited' target variables build sourcePredicate classVisited sourceOrigin fieldTypes index (superClass : rest) = do
      let project substitution = do
            source <- build substitution
            pure $ do
              sourceEvidence <- source
              pure
                ( EvSuperClass
                    sourceEvidence
                    sourceOrigin
                    (applySubstPred substitution sourcePredicate)
                    (map (applySubst substitution) fieldTypes)
                    index
                )
      result <-
        case superClass of
          QuantifiedPred newVariables antecedents consequent ->
            searchQuantifiedChain
              visited'
              target
              (variables <> newVariables)
              ( \substitution -> do
                  projected <- project substitution
                  case projected of
                    Nothing -> pure Nothing
                    Just evidence -> applyQuantifiedEvidence visited' evidence newVariables antecedents substitution
              )
              consequent
              classVisited
          _ ->
            searchQuantifiedChain visited' target variables project superClass classVisited
      case result of
        Just evidence -> pure (Just evidence)
        Nothing ->
          searchQuantifiedSuperClasses visited' target variables build sourcePredicate classVisited sourceOrigin fieldTypes (index + 1) rest

    freshQuantifiedVariables = foldM freshOne ([], Map.empty)
      where
        freshOne (variables, substitution) variable = do
          fresh <- freshSkolemTv (tvName variable)
          let kind = applySubst substitution (tvKind variable)
              freshVariable = setTyVarKind kind fresh
          pure (variables <> [freshVariable], Map.insert (tvUnique variable) (TcTyVar freshVariable) substitution)

    predicateType predicate =
      case predicate of
        ClassPred classTyCon arguments -> pure (TcTyCon classTyCon arguments)
        EqPred left right -> do
          kinds <- getKinds
          equalityTyCon <- wiredTyCon tcWiringEqualityTyCon (KFun (typeKind kinds) (KFun (typeKind kinds) (constraintKind kinds)))
          pure (TcTyCon equalityTyCon [left, right])
        IParamPred name payload -> implicitParamType name payload
        IrredPred constraint -> pure constraint
        QuantifiedPred variables antecedents consequent -> do
          consequentType <- predicateType consequent
          let qualified = if null antecedents then consequentType else TcQualTy antecedents consequentType
          pure (foldr TcForAllTy qualified variables)

-- | Whether evidence names a given predicate. A recursive dictionary names
-- itself in this way.
mentionsGiven :: Pred -> EvTerm -> Bool
mentionsGiven predicate evidence =
  case evidence of
    EvGiven given -> given == predicate
    EvDict _ _ _ arguments -> any (mentionsGiven predicate) arguments
    EvSuperClass inner _ _ _ _ -> mentionsGiven predicate inner
    EvCast inner _ -> mentionsGiven predicate inner
    EvTypeable _ _ _ kindArguments arguments -> any (mentionsGiven predicate . snd) kindArguments || any (mentionsGiven predicate) arguments
    EvTypeableLiteral _ _ _ inner -> mentionsGiven predicate inner
    EvTypeableApp _ _ _ functionEvidence argumentEvidence -> mentionsGiven predicate functionEvidence || mentionsGiven predicate argumentEvidence
    EvTypeLam _ body -> mentionsGiven predicate body
    EvDictLam _ _ body -> mentionsGiven predicate body
    EvTypeApp function _ -> mentionsGiven predicate function
    EvDictApp function argument -> mentionsGiven predicate function || mentionsGiven predicate argument
    EvCallStackPush _ _ _ parent -> mentionsGiven predicate parent
    EvRecursive inner _ body -> inner /= predicate && mentionsGiven predicate body
    EvVarTerm {} -> False
    EvEqualityDict {} -> False
    EvCoercible {} -> False
    EvCoercion {} -> False
    EvTypeLit {} -> False
    EvWithDict {} -> False
    EvCallStackEmpty {} -> False
    EvExceptionContextEmpty {} -> False

-- | The instances whose heads match the arguments, without an instance that
-- a more specific match overlaps. For @MonadState s (StateT s m)@ the
-- instance for @StateT s m@ wins over an overlappable instance for @t m@,
-- although the context of the overlappable instance can also be solvable.
mostSpecificInstances :: [TcType] -> [InstanceInfo] -> [InstanceInfo]
mostSpecificInstances args instances =
  [candidate | candidate <- candidates, not (any (`moreSpecificThan` candidate) candidates)]
  where
    candidates = [instanceInfo | instanceInfo <- instances, isJust (matchTypes (iiHead instanceInfo) args)]
    moreSpecificThan specific general =
      isJust (matchTypes (iiHead general) (iiHead specific))
        && isNothing (matchTypes (iiHead specific) (iiHead general))

typeableArguments :: TcType -> Maybe [TcType]
typeableArguments ty =
  case ty of
    TcTyCon _ arguments -> Just arguments
    TcKindedTyCon {} -> Just []
    TcFunTy argument result -> Just [argument, result]
    TcTyVar {} -> Nothing
    TcMetaTv {} -> Nothing
    -- The bare function arrow, as in @Typeable (->)@, is a type constructor.
    TcArrowTy -> Just []
    TcTyLit {} -> Nothing
    TcForAllTy {} -> Nothing
    TcQualTy {} -> Nothing
    TcAppTy {} ->
      case collectTypeApplications ty of
        (TcKindedTyCon {}, arguments) -> Just arguments
        (TcArrowTy, arguments) -> Just arguments
        _ -> Nothing

-- | The rewrite rule sets that the given equalities permit, most
-- specific first. Each rule's coercion proves @from ~ to@.
--
-- The first set holds every family rule: a family application rewrites to
-- the other side of its given. A given whose two sides are both family
-- applications rewrites nothing. Each later set adds one rigid variable
-- rule to the family rules. A rigid variable equal to another type
-- rewrites to that type. Two rigid variables give one set for each
-- direction, because the wanted can name either one while the given names
-- the other.
givenRewriteRuleSets :: [Pred] -> TcM [[(TcType, TcType, Coercion)]]
givenRewriteRuleSets givens = do
  written <- concat <$> traverse (\predicate -> givenEqualities [] (predicate, EvGiven predicate)) givens
  decomposed <- concat <$> mapM (decomposeGiven (8 :: Int)) written
  -- A rule is compared with a wanted whose families are reduced, so its
  -- sides are reduced too. The proof still names the given as written.
  reduced <- traverse (\(left, right, proof) -> (,,proof) <$> reduceTypeFamilies left <*> reduceTypeFamilies right) decomposed
  let equalities = decomposed <> [equality | equality@(left, right, _) <- reduced, (left, right) `notElem` [(l, r) | (l, r, _) <- decomposed]]
  oriented <- mapM orient equalities
  let familyRules = concat [rules | (rules, _) <- oriented]
      variableRules = concat [rules | (_, rules) <- oriented]
  pure (familyRules : [familyRules <> [rule] | rule <- variableRules])
  where
    -- A given equality between two applications of one constructor also
    -- gives the equalities of their arguments: @M1 f ~ M1 g@ gives @f ~
    -- g@, which can rewrite a wanted @C f@ to the given @C g@. Each part
    -- keeps the given and its projection, and the projection numbers the
    -- arguments from the last one, as 'NthCo' does.
    --
    -- A split of an application @f a ~ g b@ gives the function and the
    -- argument, not the constructor arguments. 'NthCo' numbers the
    -- arguments of the outer constructor, so these parts use 'LeftCo' and
    -- 'RightCo'. The function part is sound only when the two arguments
    -- have one kind.
    decomposeGiven depth equality@(left, right, proof)
      | depth <= 0 || not (decomposable left && decomposable right) = pure [equality]
      | otherwise = do
          children <- decomposeNominalEquality left right
          projected <- case children of
            Nothing -> pure Nothing
            Just [(leftFunction, rightFunction), (leftArgument, rightArgument)]
              | isApplication left || isApplication right -> do
                  leftKind <- tcTypeKind leftArgument >>= zonkType
                  rightKind <- tcTypeKind rightArgument >>= zonkType
                  pure . Just $
                    [(leftFunction, rightFunction, LeftCo proof) | sameType leftKind rightKind]
                      <> [(leftArgument, rightArgument, RightCo proof)]
            Just pairs ->
              pure (Just [(childLeft, childRight, NthCo index proof) | (index, (childLeft, childRight)) <- zip [0 ..] (reverse pairs)])
          case projected of
            Nothing -> pure [equality]
            Just parts -> do
              decomposed <-
                mapM
                  (decomposeGiven (depth - 1))
                  [part | part@(childLeft, childRight, _) <- parts, not (sameType childLeft childRight)]
              pure (equality : concat decomposed)
    isApplication ty =
      case ty of
        TcAppTy {} -> True
        _ -> False
    decomposable ty =
      case ty of
        TcTyCon {} -> True
        TcAppTy {} -> True
        TcFunTy {} -> True
        _ -> False
    orient (left, right, proof) = do
      leftIsFamily <- isTypeFamilyApplication left
      rightIsFamily <- isTypeFamilyApplication right
      pure $ case (leftIsFamily, rightIsFamily) of
        (True, False) -> ([(left, right, proof)], skolemRule right left (Sym proof))
        (False, True) -> ([(right, left, Sym proof)], skolemRule left right proof)
        (False, False) -> ([], variableRule left right proof)
        -- Two family applications have no preferred side. Each direction
        -- is an alternative: the given @AllB c b ~ GAll 0 c (Rep b)@
        -- rewrites a wanted @GAll 0 c (Rep b)@ to the given @AllB c b@.
        -- A direction whose result contains its own left side is not a
        -- rule: the given @F (F a) ~ F a@ would rewrite @F a@ to @F (F
        -- a)@, and each rewritten wanted to a larger one, without end.
        (True, True)
          | sameType left right -> ([], [])
          | otherwise ->
              ( [],
                [ rule
                | rule@(from, to, _) <- [(left, right, proof), (right, left, Sym proof)],
                  not (containsType from to)
                ]
              )
    containsType from ty =
      sameType from ty
        || case ty of
          TcTyCon _ arguments -> any (containsType from) arguments
          TcAppTy function argument -> containsType from function || containsType from argument
          TcFunTy domain range -> containsType from domain || containsType from range
          TcForAllTy _ body -> containsType from body
          _ -> False
    -- A rigid variable equal to a family application also rewrites to
    -- that application, as an alternative. The given @bool ~ BooleanOf
    -- a@ turns the wanted @Boolean bool@ into @Boolean (BooleanOf a)@,
    -- which the superclass of a given @EqB a@ solves.
    skolemRule variable application proof =
      case variable of
        TcTyVar _
          | not (containsType variable application) -> [(variable, application, proof)]
        _ -> []
    variableRule left right proof =
      case (left, right) of
        (TcTyVar leftVar, TcTyVar rightVar)
          | sameTyVar leftVar rightVar -> []
          | otherwise -> [(left, right, proof), (right, left, Sym proof)]
        (TcTyVar _, _) -> [(left, right, proof)]
        (_, TcTyVar _) -> [(right, left, Sym proof)]
        _ -> []

-- | Whether two predicates are the same up to the spelling of their
-- applications ('canonicalPred'). The evidence keeps each predicate as
-- written, because only that spelling has its kind arguments.
samePredicate :: Pred -> Pred -> Bool
samePredicate left right = left == right || canonicalPred left == canonicalPred right

-- | One spelling of a predicate for the comparison with a given. A
-- poly-kinded constructor that a use site applied to its kinds heads an
-- application spine (@Product \@k f g@), and another use can write the
-- same type as a partial application without its kinds (@Product f g@).
-- Both become the partial application. The kinds follow from the
-- arguments.
canonicalPred :: Pred -> Pred
canonicalPred predicate =
  case predicate of
    ClassPred className arguments -> ClassPred className (map canonicalApplications arguments)
    -- A stuck constraint compares the same way: the given @Constraints k
    -- (Re p a b)@ of a signature and the wanted at the same type.
    IrredPred constraint -> IrredPred (canonicalApplications constraint)
    EqPred left right -> EqPred (canonicalApplications left) (canonicalApplications right)
    _ -> predicate

-- | Rewrite every occurrence of a rule's left side, outermost first, and
-- prove the result equal to the original by congruence. Types under a
-- binder stay as they are.
rewriteWithRules :: [(TcType, TcType, Coercion)] -> TcType -> (TcType, Coercion)
rewriteWithRules rules = go
  where
    go ty =
      case [(to, proof) | (from, to, proof) <- rules, sameType from ty] of
        (to, proof) : _ -> (to, proof)
        [] ->
          case ty of
            TcTyCon tyCon arguments ->
              let (arguments', proofs) = unzip (map go arguments)
               in if and (zipWith sameType arguments' arguments)
                    then (ty, Refl ty)
                    else (TcTyCon tyCon arguments', TyConAppCo tyCon arguments proofs [])
            TcAppTy function argument ->
              let (function', functionProof) = go function
                  (argument', argumentProof) = go argument
               in if sameType function' function && sameType argument' argument
                    then (ty, Refl ty)
                    else (mkAppTy function' argument', AppCo functionProof argumentProof)
            TcFunTy domain range ->
              let (domain', domainProof) = go domain
                  (range', rangeProof) = go range
               in if sameType domain' domain && sameType range' range
                    then (ty, Refl ty)
                    else (TcFunTy domain' range', FunCo domainProof rangeProof)
            TcForAllTy variable body ->
              let (body', proof) = go body
               in if sameType body' body
                    then (ty, Refl ty)
                    else (TcForAllTy variable body', ForAllCo variable proof)
            TcQualTy predicates body
              | Just rewrittenPredicates <- traverse rewritePredicate predicates ->
                  let (body', bodyProof) = go body
                      (predicates', proofs) = unzip rewrittenPredicates
                   in if predicates' == predicates && sameType body' body
                        then (ty, Refl ty)
                        else (TcQualTy predicates' body', foldr FunCo bodyProof proofs)
            _ -> (ty, Refl ty)
    rewritePredicate (ClassPred constructor arguments) =
      let (arguments', proofs) = unzip (map go arguments)
       in Just (ClassPred constructor arguments', TyConAppCo constructor arguments proofs [])
    rewritePredicate _ = Nothing

-- | The evidence for a wanted implicit parameter from the evidence of its binding.
--
-- An occurrence of a function with a @HasCallStack@ constraint pushes its call
-- site onto the parent call stack.
implicitParamEvidence :: Ct -> Text -> TcType -> EvTerm -> EvTerm
implicitParamEvidence ct name payload parent =
  case (callStackOrigin name payload, ctOrigin ct, ctLoc ct) of
    (Just origin, OccurrenceOf function, Just (SourceSpan file startLine startColumn endLine endColumn _ _)) ->
      EvCallStackPush origin function (CallSite file startLine startColumn endLine endColumn) parent
    _ -> parent

-- | The package and module of the @CallStack@ type when the implicit
-- parameter has the type @CallStack@.
--
-- The type alone makes an implicit parameter a call stack. Like GHC, the
-- solver accepts a name other than @?callStack@, such as @?callstack@. A
-- given solves only a wanted with the same name, so a wanted with another
-- name gets the empty call stack.
callStackOrigin :: Text -> TcType -> Maybe (Text, Text)
callStackOrigin _name payload =
  case payload of
    TcTyCon tyCon []
      | tyConName tyCon == "CallStack" ->
          Just (packageIdText (tyConPackageId tyCon), tyConModuleName tyCon)
    _ -> Nothing

-- | The package and module of the @ExceptionContext@ type when the
-- implicit parameter has the type @ExceptionContext@.
--
-- Like GHC, the solver gives the empty exception context to an unsolved
-- parameter of this type. Thus, the construction of a @SomeException@
-- outside a handler does not need a binding of @?exceptionContext@.
exceptionContextOrigin :: TcType -> Maybe (Text, Text)
exceptionContextOrigin payload =
  case payload of
    TcTyCon tyCon []
      | tyConName tyCon == "ExceptionContext" ->
          Just (packageIdText (tyConPackageId tyCon), tyConModuleName tyCon)
    _ -> Nothing

-- | True for an implicit parameter that gets a default value when it is
-- unsolved: a call stack or an exception context. GHC never infers these
-- constraints.
isDefaultedImplicitParam :: Pred -> Bool
isDefaultedImplicitParam predicate =
  case predicate of
    IParamPred name payload -> isJust (callStackOrigin name payload) || isJust (exceptionContextOrigin payload)
    _ -> False

-- | Report an unsolved dictionary constraint.
--
-- An unsolved call-stack parameter is not an error. It gets the empty call
-- stack, as in GHC.
isNatLiteral :: TyLit -> Bool
isNatLiteral literal = case literal of
  TyLitNat {} -> True
  _ -> False

isSymbolLiteral :: TyLit -> Bool
isSymbolLiteral literal = case literal of
  TyLitSymbol {} -> True
  _ -> False

isCharLiteral :: TyLit -> Bool
isCharLiteral literal = case literal of
  TyLitChar {} -> True
  _ -> False

reportUnsolvedDict :: Ct -> TcM ()
reportUnsolvedDict ct = do
  predicate <- zonkPred (ctPred ct)
  wiring <- getWiring
  case predicate of
    IParamPred name payload
      | Just origin <- callStackOrigin name payload ->
          bindEvidence (ctEvVar ct) (implicitParamEvidence ct name payload (EvCallStackEmpty origin))
    IParamPred _ payload
      | Just origin <- exceptionContextOrigin payload ->
          bindEvidence (ctEvVar ct) (EvExceptionContextEmpty origin)
    -- A custom type error is not an unsolved constraint but a message the
    -- library author wrote. It stands where a constraint would, so it
    -- arrives here; what it says is the diagnostic.
    IrredPred constraint
      | Just message <- customTypeErrorMessage wiring constraint ->
          emitError (ctLoc ct) (OtherError message)
    _ -> emitError (ctLoc ct) (UnsolvedWanted predicate (ctOrigin ct))

-- | The rendered message of a @TypeError@ application, if the constraint
-- is one.
customTypeErrorMessage :: TcWiring -> TcType -> Maybe String
customTypeErrorMessage wiring constraint =
  case constraint of
    -- @TypeError@ is polymorphic in its result kind, so an application
    -- carries that kind before the message and the message is last.
    TcTyCon tyCon arguments
      | (tyConModuleName tyCon, tyConName tyCon) == tcWiringTypeErrorFamily wiring,
        message : _ <- reverse arguments ->
          Just (renderErrorMessage wiring message)
    _ -> Nothing

-- | An @ErrorMessage@ as the text it spells. A part that is not one of the
-- four constructors is shown as the type it is, which is what GHC does
-- with a message it cannot reduce any further.
renderErrorMessage :: TcWiring -> TcType -> String
renderErrorMessage wiring = go
  where
    (textCon, showTypeCon, appendCon, aboveCon) = tcWiringErrorMessageCons wiring
    go message =
      case message of
        TcTyCon tyCon [TcTyLit (TyLitSymbol literal)]
          | tyConName tyCon == textCon -> T.unpack literal
        TcTyCon tyCon arguments
          | tyConName tyCon == showTypeCon,
            [shown] <- arguments ->
              renderTcType shown
          -- The constructor is poly-kinded, so the kind comes first.
          | tyConName tyCon == showTypeCon,
            [_, shown] <- arguments ->
              renderTcType shown
          | tyConName tyCon == appendCon,
            [left, right] <- lastTwo arguments ->
              go left <> go right
          | tyConName tyCon == aboveCon,
            [left, right] <- lastTwo arguments ->
              go left <> "\n" <> go right
        _ -> renderTcType message
    lastTwo arguments = drop (length arguments - 2) arguments

matchQuantifiedPredicate :: [TyVarId] -> Pred -> Pred -> Maybe (Map Unique TcType)
matchQuantifiedPredicate variables patternPredicate targetPredicate =
  case (patternPredicate, targetPredicate) of
    (ClassPred patternClass patternArguments, ClassPred targetClass targetArguments)
      | patternClass == targetClass,
        length patternArguments == length targetArguments ->
          foldM matchOneQuantified Map.empty (zip patternArguments targetArguments)
    (IrredPred patternType, IrredPred targetType) ->
      matchOneQuantified Map.empty (patternType, targetType)
    (EqPred patternLeft patternRight, EqPred targetLeft targetRight) ->
      foldM matchOneQuantified Map.empty [(patternLeft, targetLeft), (patternRight, targetRight)]
    _ -> Nothing
  where
    quantified = map tvUnique variables
    matchOneQuantified = matchTypeQuantified quantified

matchTypeQuantified :: [Unique] -> Map Unique TcType -> (TcType, TcType) -> Maybe (Map Unique TcType)
matchTypeQuantified quantified substitution (TcTyVar variable, target)
  | tvUnique variable `elem` quantified =
      case Map.lookup (tvUnique variable) substitution of
        Nothing -> Just (Map.insert (tvUnique variable) target substitution)
        Just existing
          | existing == target -> Just substitution
          | otherwise -> Nothing
matchTypeQuantified quantified substitution (TcTyCon tyCon arguments, TcTyCon targetTyCon targetArguments)
  | tyCon == targetTyCon,
    length arguments == length targetArguments =
      foldM (matchTypeQuantified quantified) substitution (zip arguments targetArguments)
matchTypeQuantified quantified substitution (TcFunTy argument result, TcFunTy targetArgument targetResult) =
  matchTypeQuantified quantified substitution (argument, targetArgument)
    >>= \substitution' -> matchTypeQuantified quantified substitution' (result, targetResult)
matchTypeQuantified quantified substitution (TcAppTy function argument, TcAppTy targetFunction targetArgument) =
  matchTypeQuantified quantified substitution (function, targetFunction)
    >>= \substitution' -> matchTypeQuantified quantified substitution' (argument, targetArgument)
matchTypeQuantified _ substitution (patternType, targetType)
  | patternType == targetType = Just substitution
  | otherwise = Nothing

-- | Split a @Typeable@ wanted for a type constructor application into one
-- @Typeable@ wanted for each kind argument and each type argument. The
-- representation of the application is built from the representations of
-- the parts, so the evidence of the wanted refers to the new wanteds.
--
-- A branch of a GADT match uses this for a wanted that it cannot solve as a
-- whole. In @Typeable (T t0 a)@, the given of the branch can solve
-- @Typeable a@ for the existential @a@, but @Typeable t0@ must wait until
-- the enclosing scope solves the meta variable @t0@.
splitTypeable :: Ct -> TcM (Maybe [Ct])
splitTypeable ct =
  case ctPred ct of
    ClassPred className [ty]
      | tyConName className == "Typeable" -> do
          zonked <- zonkType ty
          case typeableArguments zonked of
            Just arguments@(_ : _) -> do
              (constructor, kindArguments) <- typeableConstructor zonked
              kindWanteds <- mapM part kindArguments
              argumentWanteds <- mapM part arguments
              classOrigin <- maybe Nothing ciOrigin <$> lookupClassByName "Typeable"
              bindEvidence
                (ctEvVar ct)
                ( EvTypeable
                    classOrigin
                    zonked
                    constructor
                    (zip kindArguments (map (EvVarTerm . ctEvVar) kindWanteds))
                    (map (EvVarTerm . ctEvVar) argumentWanteds)
                )
              pure (Just (kindWanteds <> argumentWanteds))
            _ -> pure Nothing
      where
        part argument = do
          evidence <- freshEvVar
          pure ct {ctPred = ClassPred className [argument], ctEvVar = evidence}
    _ -> pure Nothing

-- | Resolve the kind arguments before FC constructs a runtime representation.
typeableConstructor :: TcType -> TcM (TypeableTyCon, [TcType])
typeableConstructor ty = do
  kinds <- getKinds
  case ty of
    TcTyCon constructor arguments -> do
      info <- lookupTyConByIdentity constructor >>= maybe (abortTc "Typeable constructor has no checked kind") pure
      instantiated <- instantiateWithArgs (tciKindScheme info)
      foldM_ applyArgument (instType instantiated) arguments
      kindArguments <- mapM zonkKind (instTypeArgs instantiated)
      metadata <- typeableTyConMetadata constructor
      pure (metadata, kindArguments)
    -- A poly-kinded constructor carries the kind arguments of its use.
    TcKindedTyCon constructor kindArguments -> do
      zonked <- mapM zonkKind kindArguments
      metadata <- typeableTyConMetadata constructor
      pure (metadata, zonked)
    TcAppTy function _ -> typeableConstructor function
    TcFunTy {} -> arrowConstructor kinds
    TcArrowTy -> arrowConstructor kinds
    _ -> abortTc "Typeable evidence has no constructor"
  where
    arrowConstructor kinds' = do
      let lifted = TypeableKindType (liftedRep kinds')
          liftedKind = typeKind kinds'
      constructor <- wiredTyCon tcWiringArrowTyCon (TcFunTy liftedKind (TcFunTy liftedKind liftedKind))
      pure (TypeableTyCon constructor 0 (TypeableKindFun lifted (TypeableKindFun lifted lifted)), [])
    applyArgument kind argument = do
      kind' <- zonkKind kind
      case kind' of
        TcFunTy formal result -> do
          actual <- tcTypeKind argument
          unifyKinds formal actual
          pure result
        _ -> abortTc "Typeable constructor has an invalid application"

typeableTyConMetadata :: TyCon -> TcM TypeableTyCon
typeableTyConMetadata constructor = do
  info <- lookupTyConByIdentity constructor >>= maybe (abortTc "Typeable kind constructor has no checked kind") pure
  let ForAll variables _ body = tciKindScheme info
  TypeableTyCon constructor (length variables) <$> typeableKindMetadata variables body

typeableKindMetadata :: [TyVarId] -> TcType -> TcM TypeableKind
typeableKindMetadata variables kind =
  case kind of
    KTYPE representation -> pure (TypeableKindType representation)
    TcTyVar variable ->
      maybe (abortTc "Typeable kind has an unbound variable") (pure . TypeableKindVar) (elemIndex variable variables)
    TcFunTy argument result -> TypeableKindFun <$> recur argument <*> recur result
    TcTyCon _ arguments -> do
      (constructor, kindArguments) <- typeableConstructor kind
      TypeableKindCon constructor <$> mapM recur (kindArguments <> arguments)
    TcAppTy function argument -> TypeableKindApp <$> recur function <*> recur argument
    _ -> abortTc "Typeable kind has no runtime representation"
  where
    recur = typeableKindMetadata variables

-- | Zonk a given, and reclassify an irreducible one whose head is now a
-- class. A given @q b@ from a rank-2 argument @forall b. q b => f b@ is
-- irreducible when the class variable @q@ is a meta variable; once @q@ is
-- @Eq@, the given has to compare equal to the wanted @Eq b@.
zonkGivenPred :: Pred -> TcM Pred
zonkGivenPred = zonkPred >=> reclassifyIrreduciblePred
