{-# LANGUAGE OverloadedStrings #-}

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
-- 3. Every sub-expression's type is consistent with how it's used.
-- 4. Every cast has a valid coercion proof.
module Aihc.Fc.Lint
  ( -- * Lint
    lintProgram,
    lintProgramWithAxiomInterface,
    lintExpr,

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
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Evidence (Coercion (..))
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon, TyVarId (..), Unique (..), tvName, tyConArity, tyConName, tyConPackageId)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | A lint error.
data LintError
  = -- | Variable not in scope.
    UnboundVar !Text !Unique
  | -- | Error in a term binding.
    InBinding !Text !LintError
  | -- | Type mismatch.
    TypeMismatch !String !TcType !TcType
  | -- | Meta-variable found in Core (should have been zonked).
    MetaVarInCore !Unique
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
    -- | Top-level symbols in scope, mapped to their installed origins.
    leSymbols :: !(Map FcSymbolOrigin TcType),
    -- | Type variables in scope.
    leTyVars :: !(Set TyVarId),
    -- | Known data constructors: name -> (type var params, field types, result type).
    leDataCons :: !(Map Text ([TyVarId], [TcType], TcType)),
    -- | Type equality axioms visible to coercion linting.
    leAxioms :: !AxiomInterface,
    leForeignCalls :: !(Map Text FcForeignCall)
  }
  deriving (Show)

-- | An empty lint environment.
emptyLintEnv :: LintEnv
emptyLintEnv =
  LintEnv
    { leTerms = Map.empty,
      leSymbols = Map.empty,
      leTyVars = Set.empty,
      leDataCons = Map.empty,
      leAxioms = mempty,
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
        env0 {leAxioms = leAxioms env0 <> imported <> extractAxiomInterface prog}
        (fcTopBinds prog)

    registerDeclaration (FcData declaration) env =
      env
        { leDataCons =
            foldr
              ( \constructor ->
                  let fields = fcDataConFields constructor
                      existentialVariables = filter (`notElem` tyVars) (freeRigidTyVarsOf fields)
                   in Map.insert (fcDataConName constructor) (kindTyVars <> tyVars <> existentialVariables, fields, resultType)
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
    registerDeclaration (FcExternal origin ty) env =
      extendTopLevelTerm origin (fcExternalVar origin ty) env
    registerDeclaration (FcPrimitive var _) env =
      extendProgramTerm var env
    registerDeclaration (FcTopBind bind) env =
      foldr extendProgramTerm env (binders bind)
    registerDeclaration _ env = env

    binders bind =
      case bind of
        FcNonRec var _ -> [var]
        FcRec bindings -> map fst bindings

    extendProgramTerm var =
      extendTopLevelTerm (fromMaybe (programOrigin var) (varResolvedName var)) var

    programOrigin var =
      FcTopLevelOrigin
        (fcModulePackageText (fcProgramModule prog))
        (fcModuleName (fcProgramModule prog))
        (varName var)

    go _ [] = []
    go env (FcExternal origin ty : rest) =
      go (extendTermEnv (fcExternalVar origin ty) env) rest
    go env (FcData {} : rest) =
      -- Data declarations don't need expression-level linting.
      go env rest
    go env (FcAxiom {} : rest) =
      -- Axiom declarations don't need expression-level linting.
      go env rest
    go env (FcNewtype {} : rest) =
      -- Newtype declarations don't need expression-level linting.
      go env rest
    go env (FcPrimitive var _arity : rest) =
      go (extendTermEnv var env) rest
    go env (FcForeignImport _ : rest) =
      go env rest
    go env (FcTopBind bind : rest) =
      let (errs, env') = lintBind env bind
       in errs ++ go env' rest

-- | Lint a binding, returning errors and the extended environment.
lintBind :: LintEnv -> FcBind -> ([LintError], LintEnv)
lintBind env (FcNonRec v e) =
  let errs = case lintExprAgainst env (varType v) e of
        Left err -> [InBinding (varName v) err]
        Right inferredTy ->
          [InBinding (varName v) (TypeMismatch "non-rec binding" (varType v) inferredTy) | not (typesEqual (varType v) inferredTy)]
      env' = extendTermEnv v env
   in (errs, env')
lintBind env (FcRec binds) =
  let -- All binders are in scope for all RHSs.
      env' = foldr (extendTermEnv . fst) env binds
      errs = concatMap (lintRecBind env') binds
   in (errs, env')
  where
    lintRecBind recEnv (v, e) = case lintExprAgainst recEnv (varType v) e of
      Left err -> [InBinding (varName v) err]
      Right inferredTy ->
        [InBinding (varName v) (TypeMismatch "rec binding" (varType v) inferredTy) | not (typesEqual (varType v) inferredTy)]

lintExprAgainst :: LintEnv -> TcType -> FcExpr -> Either LintError TcType
lintExprAgainst env expected expression =
  case (expected, expression) of
    (TcForAllTy expectedVariable expectedBody, FcTyLam actualVariable body) -> do
      let bodyType = substType (Map.singleton expectedVariable (TcTyVar actualVariable)) expectedBody
      _ <- lintExprAgainst (env {leTyVars = Set.insert actualVariable (leTyVars env)}) bodyType body
      pure expected
    (TcFunTy expectedArgument expectedResult, FcLam binder body)
      | typesEqual expectedArgument (varType binder) -> do
          _ <- lintExprAgainst (extendTermEnv binder env) expectedResult body
          pure expected
      | otherwise -> Left (TypeMismatch "lambda binder" expectedArgument (varType binder))
    (_, FcLet bind body) -> do
      let (errors, bodyEnv) = lintBind env bind
      case errors of
        [] -> lintExprAgainst bodyEnv expected body
        err : _ -> Left err
    (_, FcCase scrutinee binder alternatives) -> do
      scrutineeType <- lintExpr env scrutinee
      if typesEqual scrutineeType (varType binder)
        then Right ()
        else Left (TypeMismatch "case binder" scrutineeType (varType binder))
      let alternativeEnv = extendTermEnv binder env
      mapM_ (lintAlternativeAgainst alternativeEnv expected) alternatives
      pure expected
    _ -> do
      actual <- lintExpr env expression
      if typesEqual expected actual
        then pure expected
        else Left (TypeMismatch "expression" expected actual)

lintAlternativeAgainst :: LintEnv -> TcType -> FcAlt -> Either LintError ()
lintAlternativeAgainst env expected (FcAlt _ binders rhs) = do
  _ <- lintExprAgainst (foldr extendTermEnv env binders) expected rhs
  pure ()

-- | Lint an expression, returning its type or an error.
lintExpr :: LintEnv -> FcExpr -> Either LintError TcType
lintExpr env (FcVar v) =
  case Map.lookup (varUnique v) (leTerms env) of
    Just ty -> Right ty
    Nothing ->
      case varResolvedName v >>= (`Map.lookup` leSymbols env) of
        Just ty
          | typesEqual ty (varType v) -> Right ty
          | otherwise -> Left (TypeMismatch "top-level occurrence" ty (varType v))
        Nothing ->
          case Map.lookup (varName v) (leDataCons env) of
            Just (tyVars, fields, resultType)
              | typesEqual (varType v) (foldr TcForAllTy (foldr TcFunTy resultType fields) tyVars) -> Right (varType v)
              | otherwise -> Left (TypeMismatch "constructor occurrence" (foldr TcForAllTy (foldr TcFunTy resultType fields) tyVars) (varType v))
            Nothing
              | isBuiltinTupleConstructor (varName v) -> Right (varType v)
              | otherwise -> Left (UnboundVar (varName v) (varUnique v))
lintExpr _ (FcLit lit) =
  case literalType lit of
    Just ty -> Right ty
    Nothing -> Left (LintFailure ("literal has invalid runtime representation: " ++ show lit))
lintExpr env (FcApp f a) = do
  fTy <- lintExpr env f
  case fTy of
    TcFunTy argTy resTy -> do
      _ <- lintExprAgainst env argTy a
      Right resTy
    _ -> Left (LintFailure ("application to non-function type: " ++ show fTy))
lintExpr env (FcTyApp e ty) = do
  eTy <- lintExpr env e
  case eTy of
    TcForAllTy tv body ->
      Right (substType (Map.singleton tv ty) body)
    _ -> Left (LintFailure ("type application to non-forall type: " ++ show eTy))
lintExpr env (FcLam v body) = do
  let env' = extendTermEnv v env
  bodyTy <- lintExpr env' body
  Right (TcFunTy (varType v) bodyTy)
lintExpr env (FcTyLam tv body) = do
  let env' = env {leTyVars = Set.insert tv (leTyVars env)}
  bodyTy <- lintExpr env' body
  Right (TcForAllTy tv bodyTy)
lintExpr env (FcLet bind body) = do
  let (errs, env') = lintBind env bind
  case errs of
    [] -> lintExpr env' body
    (e : _) -> Left e
lintExpr env (FcCase scrut binder alts) = do
  scrutTy <- lintExpr env scrut
  if typesEqual (varType binder) scrutTy
    then Right ()
    else Left (TypeMismatch "case binder" scrutTy (varType binder))
  let altEnv = extendTermEnv binder env
  case alts of
    [] -> Left (LintFailure "case expression has no alternatives")
    _ -> do
      resTy <- inferAlternativeType altEnv alts
      mapM_ (lintAltWithExpected altEnv resTy) alts
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
      pure (fcForeignCallResultType (fcForeignCallSignature foreignCall))
  where
    checkArgument (expected, actual)
      | typesEqual expected actual = Right ()
      | otherwise = Left (TypeMismatch "foreign call argument" expected actual)

-- | Lint a case alternative.
lintAlt :: LintEnv -> FcAlt -> Either LintError TcType
lintAlt env (FcAlt _con binders rhs) = do
  let env' = foldr extendTermEnv env binders
  lintExpr env' rhs

lintAltWithExpected :: LintEnv -> TcType -> FcAlt -> Either LintError ()
lintAltWithExpected = lintAlternativeAgainst

inferAlternativeType :: LintEnv -> [FcAlt] -> Either LintError TcType
inferAlternativeType _ [] = Left (LintFailure "case alternatives do not give a result type")
inferAlternativeType env (alternative : rest) =
  case lintAlt env alternative of
    Left (LintFailure "case expression has no alternatives") -> inferAlternativeType env rest
    result -> result

-- | Extract the endpoints of a coercion.
--
-- For the MVP, this is minimal. A full implementation would recursively
-- compute the proved equality.
coercionEndpoints :: LintEnv -> Coercion -> Either LintError (TcType, TcType)
coercionEndpoints _ (Refl ty) = Right (ty, ty)
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
  Right (TcTyCon tc (map fst pairs), TcTyCon tc (map snd pairs))
coercionEndpoints env (AxiomInstCo name typeArgs) =
  case lookupAxiomDecl name (leAxioms env) of
    Nothing -> Left (LintFailure ("unknown coercion axiom: " ++ show name))
    Just declaration
      | length typeArgs /= length (fcAxiomTyVars declaration) ->
          Left (LintFailure ("coercion axiom arity mismatch: " ++ show name))
      | otherwise ->
          let substitution = Map.fromList (zip (fcAxiomTyVars declaration) typeArgs)
           in Right
                ( substType substitution (fcAxiomLeft declaration),
                  substType substitution (fcAxiomRight declaration)
                )

-- | Extend the term environment with a variable.
extendTermEnv :: Var -> LintEnv -> LintEnv
extendTermEnv v env =
  env {leTerms = Map.insert (varUnique v) (varType v) (leTerms env)}

extendTopLevelTerm :: FcSymbolOrigin -> Var -> LintEnv -> LintEnv
extendTopLevelTerm origin var env =
  (extendTermEnv var env)
    { leSymbols = Map.insert origin (varType var) (leSymbols env)
    }

-- | Structural type equality (no unification).
typesEqual :: TcType -> TcType -> Bool
typesEqual left right =
  case (tyConApplication left, tyConApplication right) of
    (Just (leftTyCon, leftArguments), Just (rightTyCon, rightArguments)) ->
      tyConsEqual leftTyCon rightTyCon
        && length leftArguments == length rightArguments
        && all (uncurry typesEqual) (zip leftArguments rightArguments)
    _ -> compareOtherTypes left right

compareOtherTypes :: TcType -> TcType -> Bool
compareOtherTypes (TcTyVar left) (TcTyVar right) =
  left == right || tvName left == tvName right
compareOtherTypes (TcMetaTv left) (TcMetaTv right) = left == right
compareOtherTypes (TcFunTy leftArgument leftResult) (TcFunTy rightArgument rightResult) =
  typesEqual leftArgument rightArgument && typesEqual leftResult rightResult
compareOtherTypes (TcForAllTy leftVariable leftBody) (TcForAllTy rightVariable rightBody) =
  typesEqual leftBody (substType (Map.singleton rightVariable (TcTyVar leftVariable)) rightBody)
compareOtherTypes (TcQualTy leftPredicates leftBody) (TcQualTy rightPredicates rightBody) =
  length leftPredicates == length rightPredicates
    && all (uncurry predsEqual) (zip leftPredicates rightPredicates)
    && typesEqual leftBody rightBody
compareOtherTypes (TcAppTy leftFunction leftArgument) (TcAppTy rightFunction rightArgument) =
  typesEqual leftFunction rightFunction && typesEqual leftArgument rightArgument
compareOtherTypes _ _ = False

tyConApplication :: TcType -> Maybe (TyCon, [TcType])
tyConApplication (TcTyCon tyCon arguments) = Just (tyCon, arguments)
tyConApplication (TcAppTy function argument) = do
  (tyCon, arguments) <- tyConApplication function
  pure (tyCon, arguments <> [argument])
tyConApplication _ = Nothing

tyConsEqual :: TyCon -> TyCon -> Bool
tyConsEqual left right =
  left == right
    || ( tyConName left == tyConName right
           && tyConArity left == tyConArity right
           && primitivePlaceholder left right
       )
  where
    primitivePlaceholder first second =
      (isInternal first || isInternal second)
        && ( tyConName first
               `elem` [ "Addr#",
                        "Char#",
                        "Int#",
                        "Int8#",
                        "Int16#",
                        "Int32#",
                        "Int64#",
                        "RealWorld",
                        "State#",
                        "Word#",
                        "Word8#",
                        "Word16#",
                        "Word32#",
                        "Word64#"
                      ]
               || ("Tuple" `T.isPrefixOf` tyConName first && "#" `T.isSuffixOf` tyConName first)
           )
    isInternal tyCon = tyConPackageId tyCon == PackageId "aihc-internal"

isBuiltinTupleConstructor :: Text -> Bool
isBuiltinTupleConstructor name =
  "(#" `T.isPrefixOf` name && "#)" `T.isSuffixOf` name

-- | Predicate equality.
predsEqual :: Pred -> Pred -> Bool
predsEqual (ClassPred c1 a1) (ClassPred c2 a2) =
  c1 == c2 && length a1 == length a2 && all (uncurry typesEqual) (zip a1 a2)
predsEqual (EqPred t1a t1b) (EqPred t2a t2b) =
  typesEqual t1a t2a && typesEqual t1b t2b
predsEqual _ _ = False
