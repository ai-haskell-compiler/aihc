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
    isCallStackPred,
    reportUnsolvedDict,
    classFieldTypes,
  )
where

import Aihc.Parser.Syntax (pattern SourceSpan)
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Annotations (renderTcType)
import Aihc.Tc.Constraint
import Aihc.Tc.Env (ClassInfo (..), InstanceInfo (..), TyConInfo (..), classFieldTypes)
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Evidence (CallSite (..), Coercion (..), EvTerm (..), TypeableKind (..), TypeableTyCon (..))
import Aihc.Tc.Instantiate (Instantiation (..), instantiateWithArgs)
import Aihc.Tc.Kind (bindKindMeta, tcTypeKind, unifyKinds, zonkKind)
import Aihc.Tc.Match (matchTypes)
import Aihc.Tc.Monad (TcM, abortTc, bindEvidence, emitError, freshEvVar, freshSkolemTv, getClassInstances, getGivenPredicates, getKinds, getWiring, implicitParamType, lookupClass, lookupClassByName, lookupEvidence, lookupTyConByIdentity, wiredTyCon, withErrorTracking, withGivenPredicates)
import Aihc.Tc.Solve.Coercible (isCoercibleClass, solveCoercible, solveCoercibleFromGivens)
import Aihc.Tc.Solve.Congruence (givenEqualities)
import Aihc.Tc.Solve.Equality (EqResult (..), solveEquality)
import Aihc.Tc.Solve.Family (isTypeFamilyApplication, normalizeFamilyPred, reducePredFamilies, reduceTypeFamilies)
import Aihc.Tc.Types
import Aihc.Tc.Unify (unify)
import Aihc.Tc.Wiring (TcWiring (..))
import Aihc.Tc.Zonk (zonkPred, zonkType)
import Control.Applicative ((<|>))
import Control.Monad (foldM, foldM_, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (get, put)
import Data.List (elemIndex, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust, isNothing)
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

solveDictWithGivens :: [Pred] -> Ct -> TcM DictResult
solveDictWithGivens = solveDictWithGivensVisited []

solveDictWithGivensVisited :: [Pred] -> [Pred] -> Ct -> TcM DictResult
solveDictWithGivensVisited visited givens ct0 = do
  -- A given that came from a stored superclass type may still be headed by
  -- a family; see 'normalizeFamilyPred'.
  givens' <- mapM normalizeFamilyPred givens
  predicate <- normalizeFamilyPred (ctPred ct0)
  solveNormalizedDict visited givens' ct0 {ctPred = predicate}

solveNormalizedDict :: [Pred] -> [Pred] -> Ct -> TcM DictResult
solveNormalizedDict visited givens ct
  | ctPred ct `elem` visited = pure (DictStuck ct)
  | otherwise =
      case ctPred ct of
        ClassPred className args -> do
          args' <- mapM (reduceTypeFamilies <=< zonkType) args
          coercibleClass <- isCoercibleClass className
          givens' <- mapM zonkPred givens
          givenEvidence <- givenDict (ctPred ct : visited) givens' className args'
          case givenEvidence of
            Just evidence -> do
              bindEvidence (ctEvVar ct) evidence
              pure DictSolved
            Nothing ->
              case (tyConName className, args') of
                (_, [left, right]) | coercibleClass -> do
                  info <- lookupClass className
                  solved <- case info of
                    Just classInfo
                      | null (ciMethods classInfo),
                        null (ciSuperClassTypes classInfo),
                        null (ciKindTyVars classInfo) -> do
                          direct <- solveCoercible left right
                          if direct
                            then pure True
                            else solveCoercibleFromGivens className givens' left right
                    _ -> pure False
                  if solved
                    then do
                      bindEvidence (ctEvVar ct) (EvCoercible className left right)
                      pure DictSolved
                    else pure (DictStuck ct)
                ("Typeable", [ty]) -> tryTypeable className ty
                ("KnownNat", [ty]) -> tryTypeLit "KnownNat" isNatLiteral ty
                ("KnownSymbol", [ty]) -> tryTypeLit "KnownSymbol" isSymbolLiteral ty
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
          kinds <- getKinds
          reclassified <- irreduciblePred kinds reduced
          case reclassified of
            Just solvable ->
              solveDictWithGivensVisited (ctPred ct : visited) givens ct {ctPred = solvable}
            Nothing -> do
              -- A stuck constraint is discharged by a given of the same
              -- shape, or by a superclass of one: @1 <= SeedSize g@ is a
              -- superclass of @SeedGen g@, and an instance for @SeedGen
              -- (StateGen g)@ owes it for @SeedSize (StateGen g)@, which
              -- reduces to the given's.
              givens' <- mapM zonkPred givens
              evidence <- firstGivenOrSuperclass (ctPred ct : visited) (IrredPred reduced) givens'
              case evidence of
                Just given -> do
                  bindEvidence (ctEvVar ct) given
                  pure DictSolved
                Nothing -> pure (DictStuck ct {ctPred = IrredPred reduced})
        quantified@QuantifiedPred {} -> solveQuantifiedWanted visited givens quantified
        EqPred {} -> pure (DictStuck ct)
        IParamPred name payload -> do
          payload' <- zonkType payload
          givens' <- mapM zonkPred givens
          -- The innermost binding of the name wins. Givens are outermost first.
          case [given | given@(IParamPred givenName _) <- reverse givens', givenName == name] of
            given@(IParamPred _ givenPayload) : _ -> do
              -- The name determines the type of an implicit parameter.
              unify (ctLoc ct) (ctOrigin ct) payload' givenPayload
              bindEvidence (ctEvVar ct) (implicitParamEvidence ct name givenPayload (EvGiven given))
              pure DictSolved
            _ -> pure (DictStuck ct)
  where
    givenDict visited' zonkedGivens className args =
      firstGivenOrSuperclass visited' (ClassPred className args) zonkedGivens

    -- A given equality with a type family application on one side rewrites
    -- that application in the wanted: @Token s ~ Word8@ turns @Num (Token
    -- s)@ into @Num Word8@, which an instance solves, and @a ~ Tokens s@
    -- turns @IsString (Tokens s)@ into @IsString a@, which is a given. The
    -- evidence for the rewritten wanted is cast back to the original
    -- predicate along the congruence of the given coercions.
    solveThroughGivenEqualities visited' zonkedGivens className args = do
      outerGivens <- mapM zonkPred =<< getGivenPredicates
      rules <- familyRewriteRules (zonkedGivens <> filter (`notElem` zonkedGivens) outerGivens)
      let (rewrittenArgs, coercions) = unzip (map (rewriteWithRules rules) args)
      if null rules || and (zipWith sameType rewrittenArgs args)
        then pure (DictStuck ct)
        else do
          rewrittenEvidence <- freshEvVar
          let rewritten = ClassPred className rewrittenArgs
          result <- solveDictWithGivensVisited visited' zonkedGivens (ct {ctPred = rewritten, ctEvVar = rewrittenEvidence})
          case result of
            DictStuck _ -> pure (DictStuck ct)
            DictSolved -> do
              inner <- lookupEvidence rewrittenEvidence
              case inner of
                Nothing -> pure (DictStuck ct)
                Just evidence -> do
                  bindEvidence (ctEvVar ct) (EvCast evidence (Sym (TyConAppCo className args coercions)))
                  pure DictSolved

    firstGivenOrSuperclass _ _ [] = pure Nothing
    firstGivenOrSuperclass visited' target (given : rest)
      | target == given = pure (Just (EvGiven given))
      | otherwise = do
          quantified <- useQuantifiedEvidence visited' target (EvGiven given) given
          projected <- superclassEvidence [] visited' target (EvGiven given) given
          case quantified <|> projected of
            Just evidence -> pure (Just evidence)
            Nothing -> firstGivenOrSuperclass visited' target rest

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
                  let substitution = Map.fromList [(tvUnique tyVar, argument) | (tyVar, argument) <- zip (ciTyVars info) sourceArgs]
                      fieldTypes = classFieldTypes info substitution
                  case traverse (constraintTypeToPred kinds . applySubst substitution) (ciSuperClassTypes info) of
                    Just superClasses -> do
                      -- A superclass is compared in the same normal form as
                      -- the wanted: families reduced, and a family-headed
                      -- one irreducible rather than a class predicate.
                      normalized <- mapM (normalizeFamilyPred <=< reducePredFamilies) superClasses
                      searchSuperClasses (sourceClass : classVisited) solveVisited sourceEvidence (ciOrigin info) sourcePredicate fieldTypes target 0 normalized
                    Nothing -> pure Nothing
        _ -> pure Nothing

    searchSuperClasses _ _ _ _ _ _ _ _ [] = pure Nothing
    searchSuperClasses classVisited solveVisited sourceEvidence sourceOrigin sourcePredicate fieldTypes target index (superClass : rest)
      | superClass == target =
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
        Just subst -> do
          let context = map (applySubstPred subst) (iiContext instanceInfo)
              typeArgs = map (applySubst subst . TcTyVar) (iiTyVars instanceInfo)
          (contextEvidence, failed) <- withErrorTracking (solveContext visited' context)
          case contextEvidence of
            Just evidence | not failed -> do
              bindEvidence (ctEvVar ct) (EvDict (iiDictOrigin instanceInfo) (iiDictName instanceInfo) typeArgs evidence)
              pure DictSolved
            _ -> do
              -- A failed candidate must not change another candidate's types or evidence.
              lift (put saved)
              tryInstances visited' className args rest

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

    -- A @KnownNat@ or @KnownSymbol@ constraint is solved when its argument
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

    tryTypeable typeableTyCon ty =
      case typeableArguments ty of
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
                      let classSubstitution =
                            Map.fromList
                              [ (tvUnique variable, argument)
                              | (variable, argument) <- zip (ciTyVars info) sourceArguments
                              ]
                          fieldTypes = classFieldTypes info classSubstitution
                      case traverse (constraintTypeToPred kinds . applySubst classSubstitution) (ciSuperClassTypes info) of
                        Nothing -> pure Nothing
                        Just superClasses ->
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
                            superClasses
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
    TcFunTy argument result -> Just [argument, result]
    TcTyVar {} -> Nothing
    TcMetaTv {} -> Nothing
    TcArrowTy -> Nothing
    TcTyLit {} -> Nothing
    TcForAllTy {} -> Nothing
    TcQualTy {} -> Nothing
    TcAppTy {} -> Nothing

-- | The given equalities that rewrite a type family application, oriented
-- from the family application to the other side. A given whose two sides
-- are both family applications rewrites nothing. The coercion proves
-- @from ~ to@.
familyRewriteRules :: [Pred] -> TcM [(TcType, TcType, Coercion)]
familyRewriteRules givens = do
  equalities <- concat <$> traverse (\predicate -> givenEqualities [] (predicate, EvGiven predicate)) givens
  concat <$> mapM orient equalities
  where
    orient (left, right, proof) = do
      leftIsFamily <- isTypeFamilyApplication left
      rightIsFamily <- isTypeFamilyApplication right
      pure $ case (leftIsFamily, rightIsFamily) of
        (True, False) -> [(left, right, proof)]
        (False, True) -> [(right, left, Sym proof)]
        _ -> []

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
                    else (TcTyCon tyCon arguments', TyConAppCo tyCon arguments proofs)
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
            _ -> (ty, Refl ty)

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
-- parameter is @?callStack :: CallStack@.
callStackOrigin :: Text -> TcType -> Maybe (Text, Text)
callStackOrigin name payload =
  case payload of
    TcTyCon tyCon []
      | name == "?callStack",
        tyConName tyCon == "CallStack" ->
          Just (packageIdText (tyConPackageId tyCon), tyConModuleName tyCon)
    _ -> Nothing

isCallStackPred :: Pred -> Bool
isCallStackPred predicate =
  case predicate of
    IParamPred name payload -> isJust (callStackOrigin name payload)
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

reportUnsolvedDict :: Ct -> TcM ()
reportUnsolvedDict ct = do
  predicate <- zonkPred (ctPred ct)
  wiring <- getWiring
  case predicate of
    IParamPred name payload
      | Just origin <- callStackOrigin name payload ->
          bindEvidence (ctEvVar ct) (implicitParamEvidence ct name payload (EvCallStackEmpty origin))
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
    TcFunTy {} -> do
      let lifted = TypeableKindType (liftedRep kinds)
      constructor <- wiredTyCon tcWiringArrowTyCon (TcFunTy (typeKind kinds) (TcFunTy (typeKind kinds) (typeKind kinds)))
      pure (TypeableTyCon constructor 0 (TypeableKindFun lifted (TypeableKindFun lifted lifted)), [])
    _ -> abortTc "Typeable evidence has no constructor"
  where
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

-- | Include implicit kind arguments in instance evidence.
--
-- Matching also fixes the kind metas that the instance determines. A wanted
-- can reach the solver before its kinds are settled: a type argument
-- instantiated from a poly-kinded signature carries a kind meta until
-- something forces it, and choosing the instance is what forces it.
matchInstanceKinds :: [TyVarId] -> Map Unique TcType -> TcM (Maybe (Map Unique TcType))
matchInstanceKinds variables substitution = do
  matched <- foldM extend (Just (substitution, [])) variables
  case matched of
    Nothing -> pure Nothing
    Just (final, kindMetas) -> do
      mapM_ (uncurry bindKindMeta) kindMetas
      pure (Just final)
  where
    extend Nothing _ = pure Nothing
    extend (Just (current, kindMetas)) variable = case Map.lookup (tvUnique variable) current of
      Nothing -> pure (Just (current, kindMetas))
      Just target -> do
        targetKind <- tcTypeKind target >>= zonkKind
        patternKind <- zonkKind (tvKind variable)
        pure $ do
          (inferred, metas) <- matchKinds patternKind targetKind
          merged <- foldM merge current (Map.toList inferred)
          pure (merged, kindMetas <> metas)
    merge current (key, ty) = case Map.lookup key current of
      Just existing | existing /= ty -> Nothing
      _ -> Just (Map.insert key ty current)

-- | Match an instance variable's kind against the kind of the type the
-- instance head matched it with.
--
-- A variable in either kind stands for an implicit kind argument of the
-- instance, and an unsolved meta stands for a kind that the wanted has not
-- fixed yet: the match returns the binding that settles it rather than
-- failing. Both kinds must already be zonked.
matchKinds :: TcType -> TcType -> Maybe (Map Unique TcType, [(Unique, TcType)])
matchKinds = go (Map.empty, [])
  where
    go (substitution, metas) patternKind targetKind =
      case (patternKind, targetKind) of
        (TcTyVar variable, _) ->
          case Map.lookup (tvUnique variable) substitution of
            Nothing -> Just (Map.insert (tvUnique variable) targetKind substitution, metas)
            Just existing
              | existing == targetKind -> Just (substitution, metas)
              | otherwise -> Nothing
        (_, TcMetaTv unique) -> Just (substitution, metas <> [(unique, patternKind)])
        (TcMetaTv unique, _) -> Just (substitution, metas <> [(unique, targetKind)])
        (TcTyCon tyCon arguments, TcTyCon targetTyCon targetArguments)
          | tyCon == targetTyCon,
            length arguments == length targetArguments ->
              foldM (uncurry . go) (substitution, metas) (zip arguments targetArguments)
        (TcFunTy argument result, TcFunTy targetArgument targetResult) ->
          go (substitution, metas) argument targetArgument
            >>= \next -> go next result targetResult
        (TcAppTy function argument, TcAppTy targetFunction targetArgument) ->
          go (substitution, metas) function targetFunction
            >>= \next -> go next argument targetArgument
        _
          | patternKind == targetKind -> Just (substitution, metas)
          | otherwise -> Nothing

-- | The predicate a reduced constraint denotes, when it is no longer headed
-- by a type family. 'Nothing' keeps it irreducible.
irreduciblePred :: TcKinds -> TcType -> TcM (Maybe Pred)
irreduciblePred kinds ty = do
  stillStuck <- isTypeFamilyApplication ty
  if stillStuck
    then pure Nothing
    else pure (constraintTypeToPred kinds ty)
