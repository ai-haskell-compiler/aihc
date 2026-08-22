{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Core Lint: structural type checker for System FC.
--
-- Lint is dramatically simpler than inference. No unification, no
-- constraint solving, no meta-variables. It is purely structural,
-- top-down type checking. If lint passes, the Core program is type-safe.
--
-- Invariants enforced:
--
-- 1. No meta-variables in types.
-- 2. Every variable reference is in scope.
-- 3. Every type is well-kinded.
-- 4. Every sub-expression's type is consistent with how it is used.
-- 5. Every cast has a valid coercion proof.
module Aihc.Fc.Lint
  ( -- * Lint
    lintProgram,
    lintProgramWithAxiomInterface,
    lintExpr,
    lintType,

    -- * Errors
    LintError (..),

    -- * Environment
    LintEnv (..),
    emptyLintEnv,
  )
where

import Aihc.Fc.Axiom (AxiomInterface, extractAxiomInterface, lookupAxiomDecl)
import Aihc.Fc.Subst (freeRigidTyVarsOf, substType)
import Aihc.Fc.Syntax
import Aihc.Tc.Evidence (Coercion (..))
import Aihc.Tc.Types
  ( Pred (..),
    TcKindEnv,
    TcType (..),
    TyVarId (..),
    TypeScheme (..),
    Unique (..),
    liftedTypeKind,
    runtimeRepOfTypeInEnv,
    tvKind,
    tyConKey,
    tyConName,
    pattern KFun,
    pattern KMeta,
    pattern KTYPE,
    pattern SumRep,
    pattern TupleRep,
  )
import Control.Monad (foldM)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

-- | A lint error.
data LintError
  = -- | Variable not in scope.
    UnboundVar !Text !Unique
  | -- | Type mismatch.
    TypeMismatch !String !TcType !TcType
  | -- | Meta-variable found in Core (should have been zonked).
    MetaVarInCore !Unique
  | -- | Type variable not in scope.
    UnboundTyVar !Text !Unique
  | -- | Kind mismatch.
    KindMismatch !String !TcType !TcType
  | -- | A type application has a non-function kind.
    InvalidKindApplication !String !TcType
  | -- | A term type does not have a runtime representation.
    NonValueKind !String !TcType
  | -- | Case alternatives have inconsistent types.
    InconsistentAlts !TcType !TcType
  | -- | General lint failure.
    LintFailure !String
  | UnknownForeignCall !Text
  | ForeignCallDescriptorMismatch !Text
  deriving (Eq, Show)

-- | Lint environment.
data LintEnv = LintEnv
  { -- | Term variables in scope, mapped to their types.
    leTerms :: !(Map Unique TcType),
    -- | Type variables in scope.
    leTyVars :: !(Set TyVarId),
    -- | Known data constructors by full origin.
    leDataCons :: !(Map FcSymbolOrigin ([TyVarId], [TcType], TcType)),
    -- | Type equality axioms visible to coercion linting.
    leAxioms :: !AxiomInterface,
    -- | Kind schemes for referenced type constructors.
    leKindEnv :: !TcKindEnv,
    leForeignCalls :: !(Map Text FcForeignCall)
  }
  deriving (Show)

-- | An empty lint environment.
emptyLintEnv :: LintEnv
emptyLintEnv =
  LintEnv
    { leTerms = Map.empty,
      leTyVars = Set.empty,
      leDataCons = Map.empty,
      leAxioms = mempty,
      leKindEnv = Map.empty,
      leForeignCalls = Map.empty
    }

-- | Lint an entire program.
lintProgram :: LintEnv -> FcProgram -> [LintError]
lintProgram = lintProgramWithAxiomInterface mempty

-- | Lint a program with equality axioms imported from independently compiled
-- units. Local declarations take precedence over imported declarations.
lintProgramWithAxiomInterface :: AxiomInterface -> LintEnv -> FcProgram -> [LintError]
lintProgramWithAxiomInterface imported env0 prog = go envWithDeclarations (fcTopBinds prog)
  where
    envWithDeclarations =
      foldr
        registerDeclaration
        env0
          { leAxioms = leAxioms env0 <> imported <> extractAxiomInterface prog,
            leKindEnv = fcProgramKindEnv prog <> leKindEnv env0
          }
        (fcTopBinds prog)

    registerDeclaration (FcData declaration) env =
      env
        { leDataCons =
            foldr
              ( \constructor ->
                  let fields = fcDataConFields constructor
                      existentialVariables = filter (`notElem` tyVars) (freeRigidTyVarsOf fields)
                   in Map.insert (fcConstructorSymbolOrigin (fcDataConOrigin constructor)) (kindTyVars <> tyVars <> existentialVariables, fields, resultType)
              )
              (leDataCons env)
              constructors
        }
      where
        tyVars = fcDataTyVars declaration
        kindTyVars = fcDataKindTyVars declaration
        constructors = fcDataConstructors declaration
        resultType = fcDataResultType declaration
    registerDeclaration (FcForeignImport foreignCall) env =
      env {leForeignCalls = Map.insert (fcForeignCallName foreignCall) foreignCall (leForeignCalls env)}
    registerDeclaration _ env = env

    go _ [] = []
    go env (FcExternal origin ty : rest) =
      lintValueTypeErrors "external declaration" env ty
        <> go (extendTermEnv (fcExternalVar origin ty) env) rest
    go env (FcData declaration : rest) =
      lintDataDecl env declaration <> go env rest
    go env (FcAxiom declaration : rest) =
      lintAxiomDecl env declaration <> go env rest
    go env (FcNewtype declaration : rest) =
      lintNewtypeDecl env declaration <> go env rest
    go env (FcPrimitive var _arity : rest) =
      lintValueTypeErrors "primitive declaration" env (varType var)
        <> go (extendTermEnv var env) rest
    go env (FcForeignImport foreignCall : rest) =
      lintValueTypeErrors "foreign declaration" env (fcForeignCallType (fcForeignCallSignature foreignCall))
        <> go env rest
    go env (FcTopBind bind : rest) =
      let (errs, env') = lintBind env bind
       in errs ++ go env' rest

-- | Lint a binding, returning errors and the extended environment.
lintBind :: LintEnv -> FcBind -> ([LintError], LintEnv)
lintBind env (FcNonRec v e) =
  let errs =
        lintValueTypeErrors "non-rec binder" env (varType v)
          <> case lintExpr env e of
            Left err -> [err]
            Right inferredTy ->
              [TypeMismatch "non-rec binding" (varType v) inferredTy | not (typesEqual (varType v) inferredTy)]
      env' = extendTermEnv v env
   in (errs, env')
lintBind env (FcRec binds) =
  let -- All binders are in scope for all RHSs.
      env' = foldr (extendTermEnv . fst) env binds
      errs = concatMap (lintValueTypeErrors "rec binder" env . varType . fst) binds <> concatMap (lintRecBind env') binds
   in (errs, env')
  where
    lintRecBind recEnv (v, e) = case lintExpr recEnv e of
      Left err -> [err]
      Right inferredTy ->
        [TypeMismatch "rec binding" (varType v) inferredTy | not (typesEqual (varType v) inferredTy)]

-- | Lint an expression, returning its type or an error.
lintExpr :: LintEnv -> FcExpr -> Either LintError TcType
lintExpr env (FcVar v) = do
  _ <- lintValueType "variable occurrence" env (varType v)
  case Map.lookup (varUnique v) (leTerms env) of
    Just ty -> Right ty
    Nothing ->
      case lookupDataConstructor v (leDataCons env) of
        Just (tyVars, fields, resultType)
          | typesEqual (varType v) (foldr TcForAllTy (foldr TcFunTy resultType fields) tyVars) -> Right (varType v)
          | otherwise -> Left (TypeMismatch "constructor occurrence" (foldr TcForAllTy (foldr TcFunTy resultType fields) tyVars) (varType v))
        Nothing -> Left (UnboundVar (varName v) (varUnique v))
lintExpr env (FcLit lit ty) = do
  _ <- lintValueType "literal" env ty
  case runtimeRepOfTypeInEnv (leKindEnv env) ty of
    Right runtimeRep
      | runtimeRep == literalRuntimeRep lit -> Right ty
      | otherwise -> Left (LintFailure ("literal runtime representation does not match its checked type: " ++ show lit ++ " :: " ++ show ty))
    Left kind -> Left (LintFailure ("literal checked type does not have a runtime representation: " ++ show lit ++ " :: " ++ show kind))
lintExpr env (FcApp f a) = do
  fTy <- lintExpr env f
  aTy <- lintExpr env a
  case fTy of
    TcFunTy argTy resTy
      | typesEqual argTy aTy -> Right resTy
      | otherwise -> Left (TypeMismatch "application argument" argTy aTy)
    _ -> Left (LintFailure ("application to non-function type: " ++ show fTy))
lintExpr env (FcTyApp e ty) = do
  eTy <- lintExpr env e
  argumentKind <- lintType env ty
  case eTy of
    TcForAllTy tv body
      | tvKind tv == argumentKind ->
          Right (substType (Map.singleton tv ty) body)
      | otherwise -> Left (KindMismatch "type application argument" (tvKind tv) argumentKind)
    _ -> Left (LintFailure ("type application to non-forall type: " ++ show eTy))
lintExpr env (FcLam v body) = do
  _ <- lintValueType "lambda binder" env (varType v)
  let env' = extendTermEnv v env
  bodyTy <- lintExpr env' body
  Right (TcFunTy (varType v) bodyTy)
lintExpr env (FcTyLam tv body) = do
  _ <- lintKind env (tvKind tv)
  let env' = extendTyVarEnv tv env
  bodyTy <- lintExpr env' body
  Right (TcForAllTy tv bodyTy)
lintExpr env (FcLet bind body) = do
  let (errs, env') = lintBind env bind
  case errs of
    [] -> lintExpr env' body
    (e : _) -> Left e
lintExpr env (FcCase scrut binder alts) = do
  scrutTy <- lintExpr env scrut
  _ <- lintValueType "case binder" env (varType binder)
  if typesEqual scrutTy (varType binder)
    then pure ()
    else Left (TypeMismatch "case binder" scrutTy (varType binder))
  let caseEnv = extendTermEnv binder env
  case alts of
    [] -> Left (LintFailure "case expression has no alternatives")
    alt : rest -> do
      resTy <- lintAlt caseEnv scrutTy alt
      mapM_ (lintAltWithExpected caseEnv scrutTy resTy) rest
      Right resTy
lintExpr env (FcCast e co) = do
  eTy <- lintExpr env e
  (coFrom, coTo) <- coercionEndpoints env co
  if typesEqual eTy coFrom
    then Right coTo
    else Left (TypeMismatch "cast source" coFrom eTy)
lintExpr env (FcCallForeign foreignCall arguments) = do
  case Map.lookup (fcForeignCallName foreignCall) (leForeignCalls env) of
    Nothing -> Left (UnknownForeignCall (fcForeignCallName foreignCall))
    Just declared
      | declared /= foreignCall -> Left (ForeignCallDescriptorMismatch (fcForeignCallName foreignCall))
      | otherwise -> Right ()
  argumentTypes <- mapM (lintExpr env) arguments
  let expectedTypes = fcForeignOperandTypes (fcForeignCallSignature foreignCall)
  if length argumentTypes /= length expectedTypes
    then Left (LintFailure ("foreign call arity mismatch for " ++ show (fcForeignCallName foreignCall)))
    else do
      mapM_ checkArgument (zip expectedTypes argumentTypes)
      let resultType = fcForeignCallResultType (fcForeignCallSignature foreignCall)
      _ <- lintValueType "foreign call result" env resultType
      pure resultType
  where
    checkArgument (expected, actual)
      | typesEqual expected actual = Right ()
      | otherwise = Left (TypeMismatch "foreign call argument" expected actual)

lookupDataConstructor :: Var -> Map FcSymbolOrigin value -> Maybe value
lookupDataConstructor var constructors =
  case varResolvedName var >>= (`Map.lookup` constructors) of
    Just value -> Just value
    Nothing ->
      case [value | (origin, value) <- Map.toList constructors, fcOriginName origin == varName var] of
        [value] -> Just value
        _ -> Nothing

-- | Lint a case alternative.
lintAlt :: LintEnv -> TcType -> FcAlt -> Either LintError TcType
lintAlt env scrutineeType (FcAlt con binders rhs) = do
  checkAlternative con binders
  mapM_ (lintValueType "case alternative binder" env . varType) binders
  let env' = foldr extendTermEnv env binders
  lintExpr env' rhs
  where
    checkAlternative alternativeConstructor alternativeBinders =
      case alternativeConstructor of
        DefaultAlt -> requireNoBinders "default alternative" alternativeBinders
        LitAlt _ literalType -> do
          requireNoBinders "literal alternative" alternativeBinders
          if typesEqual scrutineeType literalType
            then pure ()
            else Left (TypeMismatch "literal alternative" scrutineeType literalType)
        DataAlt constructor ->
          case Map.lookup (fcConstructorSymbolOrigin constructor) (leDataCons env) of
            Nothing -> Left (LintFailure ("unknown case alternative constructor: " ++ show constructor))
            Just (_, fields, resultType) -> do
              substitution <- matchConstructorResult resultType scrutineeType
              let expectedFields = map (substType substitution) fields
              if length expectedFields /= length alternativeBinders
                then Left (LintFailure ("case alternative binder count does not match constructor: " ++ show constructor))
                else mapM_ checkField (zip expectedFields alternativeBinders)

    checkField (expected, binder)
      | typesEqual expected (varType binder) = pure ()
      | otherwise = Left (TypeMismatch "case alternative binder" expected (varType binder))

    requireNoBinders _ [] = pure ()
    requireNoBinders context _ = Left (LintFailure (context ++ " has field binders"))

lintAltWithExpected :: LintEnv -> TcType -> TcType -> FcAlt -> Either LintError ()
lintAltWithExpected env scrutineeType resTy alt = do
  rhsTy <- lintAlt env scrutineeType alt
  if typesEqual resTy rhsTy
    then Right ()
    else Left (InconsistentAlts resTy rhsTy)

matchConstructorResult :: TcType -> TcType -> Either LintError (Map TyVarId TcType)
matchConstructorResult = go Map.empty
  where
    go substitution expected actual =
      case expected of
        TcTyVar tyVar ->
          case Map.lookup tyVar substitution of
            Nothing -> pure (Map.insert tyVar actual substitution)
            Just stored
              | typesEqual stored actual -> pure substitution
              | otherwise -> mismatch expected actual
        TcTyCon expectedTyCon expectedArguments ->
          case actual of
            TcTyCon actualTyCon actualArguments
              | expectedTyCon == actualTyCon,
                length expectedArguments == length actualArguments ->
                  foldM (\current (left, right) -> go current left right) substitution (zip expectedArguments actualArguments)
            _ -> mismatch expected actual
        TcAppTy expectedFunction expectedArgument ->
          case actual of
            TcAppTy actualFunction actualArgument -> do
              substitution' <- go substitution expectedFunction actualFunction
              go substitution' expectedArgument actualArgument
            _ -> mismatch expected actual
        _
          | typesEqual expected actual -> pure substitution
          | otherwise -> mismatch expected actual

    mismatch expected actual = Left (TypeMismatch "case alternative constructor result" expected actual)

-- | Extract the endpoints of a coercion.
--
-- For the MVP, this is minimal. A full implementation would recursively
-- compute the proved equality.
coercionEndpoints :: LintEnv -> Coercion -> Either LintError (TcType, TcType)
coercionEndpoints env (Refl ty) = do
  _ <- lintType env ty
  Right (ty, ty)
coercionEndpoints env (Sym co) = do
  (from, to) <- coercionEndpoints env co
  Right (to, from)
coercionEndpoints env (Trans co1 co2) = do
  (from, middleLeft) <- coercionEndpoints env co1
  (middleRight, to) <- coercionEndpoints env co2
  if typesEqual middleLeft middleRight
    then Right (from, to)
    else Left (TypeMismatch "coercion transitivity" middleLeft middleRight)
coercionEndpoints _ (CoVar _) =
  Right (TcMetaTv (Unique (-1)), TcMetaTv (Unique (-1)))
coercionEndpoints env (TyConAppCo tc coercions) = do
  pairs <- mapM (coercionEndpoints env) coercions
  let left = TcTyCon tc (map fst pairs)
      right = TcTyCon tc (map snd pairs)
  _ <- lintType env left
  _ <- lintType env right
  Right (left, right)
coercionEndpoints env (AxiomInstCo name typeArgs) =
  case lookupAxiomDecl name (leAxioms env) of
    Nothing -> Left (LintFailure ("unknown coercion axiom: " ++ show name))
    Just declaration
      | length typeArgs /= length (fcAxiomTyVars declaration) ->
          Left (LintFailure ("coercion axiom arity mismatch: " ++ show name))
      | otherwise -> do
          lintBinderArguments env (fcAxiomTyVars declaration) typeArgs
          let substitution = Map.fromList (zip (fcAxiomTyVars declaration) typeArgs)
          Right
            ( substType substitution (fcAxiomLeft declaration),
              substType substitution (fcAxiomRight declaration)
            )

-- | Infer and check a Core type's kind.
lintType :: LintEnv -> TcType -> Either LintError TcType
lintType env ty =
  case ty of
    TcTyVar tyVar
      | tyVar `Set.member` leTyVars env -> lintKind env (tvKind tyVar) >> pure (tvKind tyVar)
      | otherwise -> Left (UnboundTyVar (tvName tyVar) (tvUnique tyVar))
    TcMetaTv unique -> Left (MetaVarInCore unique)
    TcTyCon tyCon arguments ->
      case Map.lookup (tyConKey tyCon) (leKindEnv env) of
        Nothing -> Left (LintFailure ("missing type-constructor kind scheme: " <> show (tyConName tyCon)))
        Just (ForAll kindTyVars _ initialKind) ->
          let quantified = Set.fromList (map tvUnique kindTyVars) <> kindParameterRuntimeRepVariables initialKind
           in lintTypeArguments env ("type constructor " <> show (tyConName tyCon)) quantified initialKind arguments
    TcFunTy argument result -> do
      _ <- lintValueType "function argument" env argument
      _ <- lintValueType "function result" env result
      pure liftedTypeKind
    TcForAllTy tyVar body -> do
      _ <- lintKind env (tvKind tyVar)
      bodyKind <- lintType (extendTyVarEnv tyVar env) body
      requireValueKind "forall body" bodyKind
    TcQualTy predicates body -> do
      mapM_ (lintPred env) predicates
      bodyKind <- lintType env body
      requireValueKind "qualified type body" bodyKind
    TcAppTy function argument -> do
      functionKind <- lintType env function
      lintTypeArguments env "type application" Set.empty functionKind [argument]

lintPred :: LintEnv -> Pred -> Either LintError ()
lintPred env predicate =
  case predicate of
    ClassPred _ arguments -> mapM_ (lintType env) arguments
    EqPred left right -> do
      leftKind <- lintType env left
      rightKind <- lintType env right
      if leftKind == rightKind
        then pure ()
        else Left (KindMismatch "equality predicate" leftKind rightKind)

lintKind :: LintEnv -> TcType -> Either LintError ()
lintKind env kind = do
  lintKindMetas kind
  actual <- lintType env kind
  if actual == liftedTypeKind
    then pure ()
    else Left (KindMismatch "kind" liftedTypeKind actual)

lintTypeArguments :: LintEnv -> String -> Set Unique -> TcType -> [TcType] -> Either LintError TcType
lintTypeArguments env context quantified = go Map.empty
  where
    go substitution kind [] = do
      let result = substituteKindRuntimeReps substitution kind
      lintKindMetas result
      pure result
    go substitution kind (argument : arguments) = do
      actual <- lintType env argument
      case substituteKindRuntimeReps substitution kind of
        KFun expected result -> do
          substitution' <- matchKinds context quantified substitution expected actual
          go substitution' result arguments
        actualKind -> Left (InvalidKindApplication context actualKind)

lintKindMetas :: TcType -> Either LintError ()
lintKindMetas kind =
  case kind of
    KTYPE runtimeRep -> lintRepresentationType runtimeRep
    KFun argument result -> lintKindMetas argument >> lintKindMetas result
    KMeta unique -> Left (MetaVarInCore unique)
    _ -> pure ()

lintRepresentationType :: TcType -> Either LintError ()
lintRepresentationType runtimeRep =
  case runtimeRep of
    TcMetaTv unique -> Left (MetaVarInCore unique)
    TupleRep fields -> mapM_ lintRepresentationType fields
    SumRep fields -> mapM_ lintRepresentationType fields
    _ -> pure ()

matchKinds :: String -> Set Unique -> Map Unique TcType -> TcType -> TcType -> Either LintError (Map Unique TcType)
matchKinds context quantified substitution expected actual =
  case (expected, actual) of
    (KTYPE expectedRep, KTYPE actualRep) -> matchRuntimeReps expectedRep actualRep
    (KFun expectedArgument expectedResult, KFun actualArgument actualResult) -> do
      substitution' <- matchKinds context quantified substitution expectedArgument actualArgument
      matchKinds context quantified substitution' expectedResult actualResult
    _
      | expected == actual -> pure substitution
      | otherwise -> Left (KindMismatch context expected actual)
  where
    matchRuntimeReps expectedRep actualRep =
      case expectedRep of
        TcTyVar runtimeRepVar
          | tvUnique runtimeRepVar `Set.member` quantified ->
              case Map.lookup (tvUnique runtimeRepVar) substitution of
                Nothing -> pure (Map.insert (tvUnique runtimeRepVar) actualRep substitution)
                Just stored
                  | stored == actualRep -> pure substitution
                  | otherwise -> Left (KindMismatch context (KTYPE stored) actual)
        TupleRep expectedFields
          | TupleRep actualFields <- actualRep,
            length expectedFields == length actualFields ->
              foldM (\current (left, right) -> matchRuntimeRepsWith current left right) substitution (zip expectedFields actualFields)
        SumRep expectedFields
          | SumRep actualFields <- actualRep,
            length expectedFields == length actualFields ->
              foldM (\current (left, right) -> matchRuntimeRepsWith current left right) substitution (zip expectedFields actualFields)
        _
          | expectedRep == actualRep -> pure substitution
          | otherwise -> Left (KindMismatch context expected actual)

    matchRuntimeRepsWith current left right = matchKinds context quantified current (KTYPE left) (KTYPE right)

substituteKindRuntimeReps :: Map Unique TcType -> TcType -> TcType
substituteKindRuntimeReps substitution kind =
  case kind of
    KTYPE runtimeRep -> KTYPE (substituteRuntimeRep runtimeRep)
    KFun argument result -> KFun (substituteKindRuntimeReps substitution argument) (substituteKindRuntimeReps substitution result)
    _ -> kind
  where
    substituteRuntimeRep runtimeRep =
      case runtimeRep of
        TcTyVar runtimeRepVar -> Map.findWithDefault runtimeRep (tvUnique runtimeRepVar) substitution
        TupleRep fields -> TupleRep (map substituteRuntimeRep fields)
        SumRep fields -> SumRep (map substituteRuntimeRep fields)
        _ -> runtimeRep

kindParameterRuntimeRepVariables :: TcType -> Set Unique
kindParameterRuntimeRepVariables kind =
  case kind of
    KFun parameter result -> runtimeRepVariables parameter <> kindParameterRuntimeRepVariables result
    _ -> Set.empty
  where
    runtimeRepVariables currentKind =
      case currentKind of
        KTYPE runtimeRep -> go runtimeRep
        KFun argument result -> runtimeRepVariables argument <> runtimeRepVariables result
        _ -> Set.empty
    go runtimeRep =
      case runtimeRep of
        TcTyVar runtimeRepVar -> Set.singleton (tvUnique runtimeRepVar)
        TupleRep fields -> Set.unions (map go fields)
        SumRep fields -> Set.unions (map go fields)
        _ -> Set.empty

requireValueKind :: String -> TcType -> Either LintError TcType
requireValueKind _ kind@KTYPE {} = Right kind
requireValueKind context kind = Left (NonValueKind context kind)

lintValueType :: String -> LintEnv -> TcType -> Either LintError TcType
lintValueType context env ty = lintType env ty >>= requireValueKind context

lintValueTypeErrors :: String -> LintEnv -> TcType -> [LintError]
lintValueTypeErrors context env ty = either pure (const []) (lintValueType context env ty)

lintDataDecl :: LintEnv -> FcDataDecl -> [LintError]
lintDataDecl env declaration =
  case extendTyVarEnvs env (fcDataKindTyVars declaration <> fcDataTyVars declaration) of
    Left err -> [err]
    Right declarationEnv ->
      either pure (const []) (lintKind declarationEnv (fcDataResultKind declaration) >> requireValueKind "data result" (fcDataResultKind declaration))
        <> concatMap (lintConstructor declarationEnv) (fcDataConstructors declaration)
  where
    lintConstructor declarationEnv constructor =
      let fields = fcDataConFields constructor
          existentialVariables = filter (`Set.notMember` leTyVars declarationEnv) (freeRigidTyVarsOf fields)
       in case extendTyVarEnvs declarationEnv existentialVariables of
            Left err -> [err]
            Right constructorEnv -> concatMap (lintValueTypeErrors "data constructor field" constructorEnv) fields

lintAxiomDecl :: LintEnv -> FcAxiomDecl -> [LintError]
lintAxiomDecl env declaration =
  case extendTyVarEnvs env (fcAxiomTyVars declaration) of
    Left err -> [err]
    Right axiomEnv ->
      case (lintType axiomEnv (fcAxiomLeft declaration), lintType axiomEnv (fcAxiomRight declaration)) of
        (Left err, _) -> [err]
        (_, Left err) -> [err]
        (Right leftKind, Right rightKind)
          | leftKind == rightKind -> []
          | otherwise -> [KindMismatch "axiom sides" leftKind rightKind]

lintNewtypeDecl :: LintEnv -> FcNewtypeDecl -> [LintError]
lintNewtypeDecl env declaration =
  case extendTyVarEnvs env (fcNewtypeTyVars declaration) of
    Left err -> [err]
    Right newtypeEnv ->
      case (lintValueType "newtype representation" newtypeEnv (fcNewtypeRepresentation declaration), lintValueType "newtype result" newtypeEnv (fcNewtypeResult declaration)) of
        (Left err, _) -> [err]
        (_, Left err) -> [err]
        (Right representationKind, Right resultKind)
          | representationKind == resultKind -> []
          | otherwise -> [KindMismatch "newtype sides" representationKind resultKind]

lintBinderArguments :: LintEnv -> [TyVarId] -> [TcType] -> Either LintError ()
lintBinderArguments env = go Map.empty
  where
    go _ [] [] = pure ()
    go substitution (binder : restBinders) (argument : restArguments) = do
      actual <- lintType env argument
      let substitutedBinder =
            case substType substitution (TcTyVar binder) of
              TcTyVar tyVar -> tyVar
              _ -> binder
          expected = tvKind substitutedBinder
      if expected == actual
        then go (Map.insert binder argument substitution) restBinders restArguments
        else Left (KindMismatch "coercion axiom argument" expected actual)
    go _ _ _ = Left (LintFailure "coercion axiom argument count changed during lint")

extendTyVarEnvs :: LintEnv -> [TyVarId] -> Either LintError LintEnv
extendTyVarEnvs = foldM extend
  where
    extend env tyVar = lintKind env (tvKind tyVar) >> pure (extendTyVarEnv tyVar env)

extendTyVarEnv :: TyVarId -> LintEnv -> LintEnv
extendTyVarEnv tyVar env = env {leTyVars = Set.insert tyVar (leTyVars env)}

-- | Extend the term environment with a variable.
extendTermEnv :: Var -> LintEnv -> LintEnv
extendTermEnv v env =
  env {leTerms = Map.insert (varUnique v) (varType v) (leTerms env)}

-- | Structural type equality (no unification).
typesEqual :: TcType -> TcType -> Bool
typesEqual (TcTyVar a) (TcTyVar b) = a == b
typesEqual (TcMetaTv a) (TcMetaTv b) = a == b
typesEqual (TcTyCon tc1 args1) (TcTyCon tc2 args2) =
  tc1 == tc2 && length args1 == length args2 && all (uncurry typesEqual) (zip args1 args2)
typesEqual (TcFunTy a1 b1) (TcFunTy a2 b2) =
  typesEqual a1 a2 && typesEqual b1 b2
typesEqual (TcForAllTy tv1 body1) (TcForAllTy tv2 body2) =
  -- Alpha-equivalence: rename tv2 to tv1 in body2.
  typesEqual body1 (substType (Map.singleton tv2 (TcTyVar tv1)) body2)
typesEqual (TcQualTy p1 b1) (TcQualTy p2 b2) =
  length p1 == length p2 && all (uncurry predsEqual) (zip p1 p2) && typesEqual b1 b2
typesEqual (TcAppTy f1 a1) (TcAppTy f2 a2) =
  typesEqual f1 f2 && typesEqual a1 a2
typesEqual _ _ = False

-- | Predicate equality.
predsEqual :: Pred -> Pred -> Bool
predsEqual (ClassPred c1 a1) (ClassPred c2 a2) =
  c1 == c2 && length a1 == length a2 && all (uncurry typesEqual) (zip a1 a2)
predsEqual (EqPred t1a t1b) (EqPred t2a t2b) =
  typesEqual t1a t2a && typesEqual t1b t2b
predsEqual _ _ = False
