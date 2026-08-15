{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Expression desugaring from surface AST to System FC Core.
--
-- Translates each surface expression form into the explicit Core
-- representation. Type lambdas and type applications are inserted
-- where the type checker inferred polymorphism.
module Aihc.Fc.Desugar.Expr
  ( dsMatches,
    dsMatchesWithDicts,
    dsMatchesWithEnclosingDicts,
    dsMatchesWithGivenDicts,
    dsEvidence,
    dsRhs,
    DsM,
    DsState (..),
    ClassDict (..),
    desugarBug,
    freshUnique,
    freshVar,
    lookupType,
    primBoolType,
    withDicts,
  )
where

import Aihc.Fc.Desugar.Match (dsPatternPure, numericRuntimeRep)
import Aihc.Fc.Lower (seqPseudoOpName)
import Aihc.Fc.Subst (OccurrenceCount (..), countExprVar, substExpr, substExprVar, substType)
import Aihc.Fc.Syntax
import Aihc.Parser.Syntax
  ( CaseAlt (..),
    CompStmt (..),
    Decl (..),
    DoStmt (..),
    Expr (..),
    LambdaCaseAlt (..),
    Match (..),
    Name (..),
    NameType (..),
    NumericType (..),
    Pattern (..),
    Rhs (..),
    TupleFlavor (..),
    UnqualifiedName (..),
    ValueDecl (..),
    fromAnnotation,
    mkName,
    peelCompStmtAnn,
    peelDeclAnn,
    peelDoStmtAnn,
    peelLiteralAnn,
    peelPatternAnn,
    qualifyName,
    unqualifiedNameText,
  )
import Aihc.Parser.Syntax qualified as Surface
import Aihc.Resolve (PackageId (..), ResolutionAnnotation (..), ResolutionForm (..), ResolutionNamespace (..), ResolvedName (..))
import Aihc.Tc.Annotations (TcAnnotation (..))
import Aihc.Tc.Env (DataConFieldInfo (..))
import Aihc.Tc.Evidence (EvTerm (..))
import Aihc.Tc.Kind (runtimeRepToTcType)
import Aihc.Tc.Types (Kind (..), Pred (..), RuntimeRep (..), TcType (..), TyCon (..), TyVarId (..), Unique (..), isLiftedType, liftedRuntimeRep, mkTyCon, runtimeRepOfType, setTyVarKind, tvKind, tyConModuleName, tyConPackageId, unboxedTupleTyConName)
import Control.Applicative ((<|>))
import Control.Monad (zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, gets, modify')
import Data.ByteString qualified as BS
import Data.Char (ord)
import Data.Either (fromRight)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | Desugaring monad.
type DsM = StateT DsState (Either String)

-- | Desugaring state.
data DsState = DsState
  { dsNextUnique :: !Int,
    dsPrimPackageId :: !PackageId,
    dsModulePackage :: !PackageId,
    dsModuleName :: !Text,
    -- | Map from surface name to its inferred type (from TC).
    dsTypeEnv :: !(Map Text TcType),
    -- | Map from a complete global identity to its type constructor.
    dsGlobalTyConEnv :: !(Map (PackageId, Text, Text) TyCon),
    -- | Top-level variables, keyed by their complete resolved identity.
    dsGlobalVars :: !(Map FcSymbolOrigin Var),
    -- | Local variable bindings (pattern-bound, lambda-bound).
    dsLocalVars :: !(Map Text Var),
    -- | Local dictionaries, keyed by class predicate.
    dsLocalDicts :: !(Map Text Var),
    -- | Checked constructor fields, including source strictness.
    dsConstructorFields :: !(Map Text [DataConFieldInfo]),
    dsTupleConstructorOrigin :: !(Maybe FcSymbolOrigin)
  }

data ClassDict = ClassDict
  { classDictName :: !Text,
    classDictArgs :: ![TcType],
    classDictVar :: !Var
  }

-- | Generate a fresh unique.
freshUnique :: DsM Unique
freshUnique = do
  st <- get
  let u = dsNextUnique st
  modify' (\s -> s {dsNextUnique = u + 1})
  pure (Unique u)

-- | Make a variable with a fresh unique.
freshVar :: Text -> TcType -> DsM Var
freshVar name ty = do
  u <- freshUnique
  pure (Var name u ty)

freshInternalVar :: Text -> TcType -> DsM Var
freshInternalVar prefix ty = do
  u@(Unique unique) <- freshUnique
  pure (Var (prefix <> T.pack (show unique)) u ty)

desugarBug :: String -> DsM a
desugarBug = lift . Left

-- | Look up a name's type (locals first, then global TC env).
lookupType :: Text -> DsM TcType
lookupType name = do
  st <- get
  case Map.lookup name (dsLocalVars st) of
    Just v -> pure (varType v)
    Nothing -> case Map.lookup name (dsTypeEnv st) of
      Just ty -> pure ty
      Nothing -> desugarBug ("missing type information for name: " <> T.unpack name)

primBoolType :: DsM TcType
primBoolType = (`TcTyCon` []) <$> lookupPrimitiveTyCon "GHC.Types" "Bool"

-- | Look up an aihc-prim type constructor by its complete identity.
lookupPrimitiveTyCon :: Text -> Text -> DsM TyCon
lookupPrimitiveTyCon moduleName name = do
  primPackageId <- gets dsPrimPackageId
  globalTyCons <- gets dsGlobalTyConEnv
  case Map.lookup (primPackageId, moduleName, name) globalTyCons of
    Just tyCon -> pure tyCon
    Nothing -> missingPrimitiveTyCon moduleName name

missingPrimitiveTyCon :: Text -> Text -> DsM a
missingPrimitiveTyCon moduleName name =
  desugarBug
    ( "missing aihc-prim type constructor information for: "
        <> T.unpack moduleName
        <> "."
        <> T.unpack name
    )

resultTyCon :: TcType -> Maybe TyCon
resultTyCon ty =
  case ty of
    TcForAllTy _ body -> resultTyCon body
    TcQualTy _ body -> resultTyCon body
    TcFunTy _ result -> resultTyCon result
    TcTyCon tyCon _ -> Just tyCon
    _ -> Nothing

primitiveDataConOrigin :: Text -> Text -> DsM FcSymbolOrigin
primitiveDataConOrigin moduleName constructorName = do
  PackageId packageName <- gets dsPrimPackageId
  pure (FcTopLevelOrigin packageName moduleName constructorName)

dataConOriginForType :: TcType -> Text -> DsM FcSymbolOrigin
dataConOriginForType ty constructorName =
  case ty of
    TcTyCon tyCon _ ->
      pure (FcTopLevelOrigin (packageIdText (tyConPackageId tyCon)) (tyConModuleName tyCon) constructorName)
    _ -> desugarBug ("data constructor match has no type constructor: " <> show ty)

lookupDataConOrigin :: Text -> DsM FcSymbolOrigin
lookupDataConOrigin constructorName = do
  constructorType <- Map.lookup constructorName . dsTypeEnv <$> get
  case constructorType >>= resultTyCon of
    Just tyCon ->
      pure (FcTopLevelOrigin (packageIdText (tyConPackageId tyCon)) (tyConModuleName tyCon) constructorName)
    Nothing -> primitiveDataConOrigin "GHC.Types" constructorName

-- | Look up a local variable binding.
lookupLocal :: Text -> DsM (Maybe Var)
lookupLocal name =
  Map.lookup name . dsLocalVars <$> get

-- | Run an action with additional local variable bindings.
withLocals :: [(Text, Var)] -> DsM a -> DsM a
withLocals bindings action = do
  st <- get
  let oldLocals = dsLocalVars st
      newLocals = foldr (\(n, v) m -> Map.insert n v m) oldLocals bindings
  modify' (\s -> s {dsLocalVars = newLocals})
  result <- action
  modify' (\s -> s {dsLocalVars = oldLocals})
  pure result

withDicts :: [ClassDict] -> DsM a -> DsM a
withDicts dicts action = do
  st <- get
  let oldDicts = dsLocalDicts st
      newDicts = foldr insertDictionary oldDicts dicts
  modify' (\s -> s {dsLocalDicts = newDicts})
  result <- action
  modify' (\s -> s {dsLocalDicts = oldDicts})
  pure result
  where
    insertDictionary dictionary environment =
      let className = classDictName dictionary
          arguments = classDictArgs dictionary
          variable = classDictVar dictionary
          withFallback = Map.insertWith (\_ existing -> existing) (dictKey className arguments) variable environment
       in Map.insert (exactDictKey className arguments) variable withFallback

-- | Desugar a list of match equations into a Core expression.
--
-- For a function like @not True = False; not False = True@, this
-- produces a lambda + case expression.
--
-- For a polymorphic function like @id x = x@, this wraps with
-- type lambdas and lambdas referencing the same variable.
dsMatches :: TcType -> [Match] -> DsM FcExpr
dsMatches = dsMatchesWithDicts True

dsMatchesWithDicts :: Bool -> TcType -> [Match] -> DsM FcExpr
dsMatchesWithDicts = dsMatchesWithDictSource [] Nothing

-- | Desugar matches that close over dictionaries supplied by an enclosing
-- instance while still abstracting over the method's own constraints.
dsMatchesWithEnclosingDicts :: [ClassDict] -> TcType -> [Match] -> DsM FcExpr
dsMatchesWithEnclosingDicts enclosingDicts = dsMatchesWithDictSource enclosingDicts Nothing True

-- | Desugar matches using dictionary binders supplied by an enclosing scope.
-- The resulting expression refers to those exact variables and does not
-- abstract over a second set of dictionaries.
dsMatchesWithGivenDicts :: [ClassDict] -> TcType -> [Match] -> DsM FcExpr
dsMatchesWithGivenDicts dicts = dsMatchesWithDictSource [] (Just dicts) False

dsMatchesWithDictSource :: [ClassDict] -> Maybe [ClassDict] -> Bool -> TcType -> [Match] -> DsM FcExpr
dsMatchesWithDictSource enclosingDicts givenDicts abstractDicts ty matches = case matches of
  [] -> do
    v <- freshVar "_void" ty
    pure (FcVar v)
  (m0 : _) ->
    let nArgs = length (matchPats m0)
     in if nArgs == 0
          then do
            let (tyLams, afterForAlls) = peelForAlls ty
                dictPreds = fst (peelQuals afterForAlls)
            dicts <- dictionariesFor dictPreds
            body <- withDicts (enclosingDicts <> dicts) (dsRhs (matchRhs m0))
            let dictLamExpr
                  | abstractDicts = foldr (FcLam . classDictVar) body dicts
                  | otherwise = body
            pure (foldr FcTyLam dictLamExpr tyLams)
          else do
            let (tyLams, afterForAlls) = peelForAlls ty
                (dictPreds, innerTy) = peelQuals afterForAlls
                (argTys, resTy) = peelFunTys nArgs innerTy
            dicts <- dictionariesFor dictPreds
            argVars <- mapM (\(i, argTy) -> freshInternalVar (argName i) argTy) (zip [0 :: Int ..] argTys)
            body <- withDicts (enclosingDicts <> dicts) (buildCaseChain argVars resTy matches)
            let lamExpr = foldr FcLam body argVars
                dictLamExpr
                  | abstractDicts = foldr (FcLam . classDictVar) lamExpr dicts
                  | otherwise = lamExpr
            pure (foldr FcTyLam dictLamExpr tyLams)
  where
    dictionariesFor predicates =
      case givenDicts of
        Just dicts -> pure dicts
        Nothing -> mapM mkClassDict (zip [0 :: Int ..] predicates)

mkClassDict :: (Int, Pred) -> DsM ClassDict
mkClassDict (i, pred') =
  case pred' of
    ClassPred className args -> do
      var <- freshVar ("$d" <> T.pack (show i)) (predType pred')
      pure (ClassDict className args var)
    _ -> do
      var <- freshVar ("$d" <> T.pack (show i)) (predType pred')
      pure (ClassDict "<constraint>" [] var)

-- | Generate argument names: x, y, z, x1, y1, ...
argName :: Int -> Text
argName i
  | i < 3 = T.singleton (['x', 'y', 'z'] !! i)
  | otherwise = T.pack ("x" ++ show (i - 2))

-- | Peel forall quantifiers from a type.
peelForAlls :: TcType -> ([TyVarId], TcType)
peelForAlls (TcForAllTy tv rest) =
  let (tvs, inner) = peelForAlls rest
   in (tv : tvs, inner)
peelForAlls ty = ([], ty)

peelQuals :: TcType -> ([Pred], TcType)
peelQuals (TcQualTy preds body) = (preds, body)
peelQuals ty = ([], ty)

predType :: Pred -> TcType
predType (ClassPred className args) = TcTyCon (TyCon className (length args)) args
predType (EqPred left right) = TcTyCon (TyCon "~" 2) [left, right]

-- | Peel a fixed number of function argument types.
peelFunTys :: Int -> TcType -> ([TcType], TcType)
peelFunTys 0 ty = ([], ty)
peelFunTys n (TcFunTy arg rest) =
  let (args, res) = peelFunTys (n - 1) rest
   in (arg : args, res)
peelFunTys _ ty = ([], ty)

-- | Build a chain of case expressions for pattern matching on arguments.
--
-- For constructor patterns, produces a case expression.
-- For variable patterns, binds the pattern variable to the scrutinee
-- and recurses on remaining arguments.
buildCaseChain :: [Var] -> TcType -> [Match] -> DsM FcExpr
buildCaseChain [] _resTy (m : _) = dsRhs (matchRhs m)
buildCaseChain [] resTy [] = do
  v <- freshVar "_error" resTy
  pure (FcVar v)
buildCaseChain scrutVars@(scrutVar : restVars) resTy matches
  | any (any requiresOrderedPatternMatch . matchPats) matches =
      dsOrderedMatches resTy scrutVars matches
  | hasDefaultFallback matches =
      dsOrderedMatches resTy scrutVars matches
  | allVarPatterns matches = do
      -- Variable patterns: bind each pattern variable name to the
      -- scrutinee Var, then recurse.
      let bindings = extractVarBindings scrutVar matches
          innerMatches = map dropFirstPat matches
      withLocals bindings (buildCaseChain restVars resTy innerMatches)
  | otherwise = do
      -- Build one case alternative per first-pattern constructor. Equations
      -- that share that constructor continue together under the next argument.
      packageId <- gets dsPrimPackageId
      alts <- mapM (buildAltGroup scrutVar restVars resTy) (groupFirstPatterns packageId matches)
      caseBinder <- freshVar "_scrut" (varType scrutVar)
      pure (FcCase (FcVar scrutVar) caseBinder alts)

dsOrderedMatches :: TcType -> [Var] -> [Match] -> DsM FcExpr
dsOrderedMatches resultTy scrutVars matches =
  case matches of
    [] -> do
      failureVar <- freshInternalVar "_no_match" resultTy
      pure $
        if isLiftedType resultTy
          then FcLet (FcRec [(failureVar, FcVar failureVar)]) (FcVar failureVar)
          else FcVar failureVar
    match : rest -> do
      failure <- dsOrderedMatches resultTy scrutVars rest
      if isLiftedType resultTy
        then do
          failureVar <- freshInternalVar "_next_match" resultTy
          matched <- dsMatchPatterns scrutVars (matchPats match) (dsRhs (matchRhs match)) (FcVar failureVar)
          pure (shareFailure failureVar failure matched)
        else dsMatchPatterns scrutVars (matchPats match) (dsRhs (matchRhs match)) failure

-- | Materialize a pattern-match failure continuation only when its decision
-- tree contains enough failure leaves to benefit from sharing. Match binders
-- are globally fresh, so substituting the already-built failure expression
-- into its sole use cannot capture any of its free variables.
shareFailure :: Var -> FcExpr -> FcExpr -> FcExpr
shareFailure failureVar failure matched =
  case countExprVar failureVar matched of
    Dead -> matched
    Once -> substExpr failureVar failure matched
    Many -> FcLet (FcNonRec failureVar failure) matched

hasDefaultFallback :: [Match] -> Bool
hasDefaultFallback matches =
  any ((== DefaultAlt) . firstPatternKey) matches
    && any ((/= DefaultAlt) . firstPatternKey) matches

firstPatternKey :: Match -> FcAltCon
firstPatternKey match =
  case matchPats match of
    pat : _ -> patternKey (PackageId "") pat
    [] -> DefaultAlt

dsMatchPatterns :: [Var] -> [Pattern] -> DsM FcExpr -> FcExpr -> DsM FcExpr
dsMatchPatterns [] [] success _failure = success
dsMatchPatterns (scrutVar : scrutVars) (pat : pats) success failure =
  dsMatchPattern scrutVar pat (dsMatchPatterns scrutVars pats success failure) failure
dsMatchPatterns scrutVars pats _success _failure =
  desugarBug ("pattern arity mismatch while desugaring: " <> show (length pats) <> " pattern(s) for " <> show (length scrutVars) <> " scrutinee(s)")

dsMatchPattern :: Var -> Pattern -> DsM FcExpr -> FcExpr -> DsM FcExpr
dsMatchPattern scrutVar pat success failure =
  case peelPatternAnn pat of
    PVar name ->
      withLocals [(unqualifiedNameText name, scrutVar)] success
    PWildcard -> success
    PParen inner -> dsMatchPattern scrutVar inner success failure
    PAs name inner ->
      withLocals [(unqualifiedNameText name, scrutVar)] (dsMatchPattern scrutVar inner success failure)
    PStrict inner -> dsMatchPattern scrutVar inner success failure
    PIrrefutable inner ->
      withLocals (irrefutablePatternBindings scrutVar inner) success
    PTypeSig inner _ -> dsMatchPattern scrutVar inner success failure
    _
      | isOverloadedIntegerPattern pat ->
          dsOverloadedIntegerPatternMatch scrutVar pat success failure
      | Just char <- boxedCharPatternValue pat ->
          dsBoxedCharPatternMatch scrutVar char success failure
      | otherwise ->
          dsOrdinaryPatternMatch scrutVar pat success failure

requiresOrderedPatternMatch :: Pattern -> Bool
requiresOrderedPatternMatch pat =
  isOverloadedIntegerPattern pat
    || requiresPatternWrapperMatch pat
    || case boxedCharPatternValue pat of
      Just _ -> True
      Nothing -> False

requiresPatternWrapperMatch :: Pattern -> Bool
requiresPatternWrapperMatch (PAnn _ inner) = requiresPatternWrapperMatch inner
requiresPatternWrapperMatch (PParen inner) = requiresPatternWrapperMatch inner
requiresPatternWrapperMatch PAs {} = True
requiresPatternWrapperMatch PStrict {} = True
requiresPatternWrapperMatch PIrrefutable {} = True
requiresPatternWrapperMatch PTypeSig {} = True
requiresPatternWrapperMatch _ = False

boxedCharPatternValue :: Pattern -> Maybe Char
boxedCharPatternValue pat =
  case peelPatternAnn pat of
    PLit literal ->
      case peelLiteralAnn literal of
        Surface.LitChar char _ -> Just char
        _ -> Nothing
    PParen inner -> boxedCharPatternValue inner
    _ -> Nothing

dsBoxedCharPatternMatch :: Var -> Char -> DsM FcExpr -> FcExpr -> DsM FcExpr
dsBoxedCharPatternMatch scrutVar char success failure = do
  charVar <- freshInternalVar "_char#" charHashTy
  outerBinder <- freshInternalVar "_boxed_char" charTy
  innerBinder <- freshInternalVar "_unboxed_char" charHashTy
  matched <- success
  charConstructor <- lookupDataConOrigin "C#"
  let innerCase =
        FcCase
          (FcVar charVar)
          innerBinder
          [ FcAlt (LitAlt (LitChar WordRep char)) [] matched,
            FcAlt DefaultAlt [] failure
          ]
  pure
    ( FcCase
        (FcVar scrutVar)
        outerBinder
        [ FcAlt (DataAlt (fcConstructorIdFromSymbol charConstructor)) [charVar] innerCase,
          FcAlt DefaultAlt [] failure
        ]
    )

irrefutablePatternBindings :: Var -> Pattern -> [(Text, Var)]
irrefutablePatternBindings scrutVar pat =
  case peelPatternAnn pat of
    PVar name -> [(unqualifiedNameText name, scrutVar)]
    PParen inner -> irrefutablePatternBindings scrutVar inner
    PAs name inner -> (unqualifiedNameText name, scrutVar) : irrefutablePatternBindings scrutVar inner
    PStrict inner -> irrefutablePatternBindings scrutVar inner
    PIrrefutable inner -> irrefutablePatternBindings scrutVar inner
    PTypeSig inner _ -> irrefutablePatternBindings scrutVar inner
    _ -> []

dsOrdinaryPatternMatch :: Var -> Pattern -> DsM FcExpr -> FcExpr -> DsM FcExpr
dsOrdinaryPatternMatch scrutVar pat success failure = do
  packageId <- gets dsPrimPackageId
  let (con, binderNames) = dsPatternPure packageId pat
  case con of
    DefaultAlt -> success
    _ -> do
      binderTys <- patternBinderTypesM pat (varType scrutVar)
      binders <- zipWithM freshVar binderNames binderTys
      (evidenceBinders, dictionaries) <- patternEvidenceBinders pat
      matched <- withDicts dictionaries (dsMatchPatterns binders (constructorSubpatterns pat) success failure)
      caseBinder <- freshInternalVar "_match" (varType scrutVar)
      pure
        ( FcCase
            (FcVar scrutVar)
            caseBinder
            [ FcAlt con (evidenceBinders <> binders) matched,
              FcAlt DefaultAlt [] failure
            ]
        )

constructorSubpatterns :: Pattern -> [Pattern]
constructorSubpatterns (PAnn _ inner) = constructorSubpatterns inner
constructorSubpatterns (PParen inner) = constructorSubpatterns inner
constructorSubpatterns (PCon _ _ subpatterns) = subpatterns
constructorSubpatterns (PList []) = []
constructorSubpatterns (PList (item : items)) = [item, PList items]
constructorSubpatterns (PInfix left _operator right) = [left, right]
constructorSubpatterns (PTuple _ subpatterns) = subpatterns
constructorSubpatterns _ = []

noPatternMatch :: [Var] -> DsM FcExpr
noPatternMatch (scrutVar : _) = do
  binder <- freshInternalVar "_no_match" (varType scrutVar)
  pure (FcCase (FcVar scrutVar) binder [])
noPatternMatch [] =
  desugarBug "cannot construct a pattern-match failure without a scrutinee"

-- | Extract variable bindings from the first pattern of each match,
-- mapping the pattern variable name to the scrutinee Var.
extractVarBindings :: Var -> [Match] -> [(Text, Var)]
extractVarBindings scrutVar = concatMap go
  where
    go m = case matchPats m of
      (p : _) -> extractName p
      _ -> []
    extractName (PVar uname) = [(unqualifiedNameText uname, scrutVar)]
    extractName (PAnn _ inner) = extractName inner
    extractName (PParen inner) = extractName inner
    extractName _ = []

-- | Check if all first patterns in the matches are variables or wildcards.
allVarPatterns :: [Match] -> Bool
allVarPatterns = all isVarPat
  where
    isVarPat m = case matchPats m of
      (p : _) -> isVarOrWild p
      _ -> False
    isVarOrWild (PVar _) = True
    isVarOrWild PWildcard = True
    isVarOrWild (PAnn _ inner) = isVarOrWild inner
    isVarOrWild (PParen inner) = isVarOrWild inner
    isVarOrWild _ = False

-- | Drop the first pattern from each match.
dropFirstPat :: Match -> Match
dropFirstPat m = m {matchPats = drop 1 (matchPats m)}

data FirstPatternGroup = FirstPatternGroup !Pattern ![Match]

groupFirstPatterns :: PackageId -> [Match] -> [FirstPatternGroup]
groupFirstPatterns packageId =
  List.foldl' insertGroup []
  where
    insertGroup groups match =
      case matchPats match of
        [] -> FirstPatternGroup PWildcard [match] : groups
        pat : _ -> insertByKey pat match groups
    insertByKey pat match [] = [FirstPatternGroup pat [match]]
    insertByKey pat match (FirstPatternGroup groupPat matches : rest)
      | patternKey packageId pat == patternKey packageId groupPat =
          FirstPatternGroup (moreSpecificPattern groupPat pat) (matches <> [match]) : rest
      | otherwise = FirstPatternGroup groupPat matches : insertByKey pat match rest

patternKey :: PackageId -> Pattern -> FcAltCon
patternKey packageId = fst . dsPatternPure packageId

moreSpecificPattern :: Pattern -> Pattern -> Pattern
moreSpecificPattern left right
  | patternSpecificity right > patternSpecificity left = right
  | otherwise = left

patternSpecificity :: Pattern -> Int
patternSpecificity pat =
  length [name | name <- snd (dsPatternPure (PackageId "") pat), name /= "_", name /= "_pat"]

-- | Build a case alternative from all equations with the same first pattern.
buildAltGroup :: Var -> [Var] -> TcType -> FirstPatternGroup -> DsM FcAlt
buildAltGroup scrutVar restVars resTy (FirstPatternGroup pat matches) =
  case matches of
    [] -> do
      body <- buildCaseChain restVars resTy []
      pure (FcAlt DefaultAlt [] body)
    _ -> do
      packageId <- gets dsPrimPackageId
      case dsPatternPure packageId pat of
        (DefaultAlt, _) -> do
          body <- dsOrderedMatches resTy (scrutVar : restVars) matches
          pure (FcAlt DefaultAlt [] body)
        (con, binderNames) -> do
          let innerMatches = map expandFirstConstructorPattern matches
          binderTys <- patternBinderTypesM pat (varType scrutVar)
          binders <- zipWithM freshVar binderNames binderTys
          (evidenceBinders, dictionaries) <- patternEvidenceBinders pat
          body <- withDicts dictionaries (buildCaseChain (binders <> restVars) resTy innerMatches)
          pure (FcAlt con (evidenceBinders <> binders) body)

expandFirstConstructorPattern :: Match -> Match
expandFirstConstructorPattern match =
  case matchPats match of
    [] -> match
    pat : pats -> match {matchPats = constructorSubpatterns pat <> pats}

dsRhs :: Rhs Expr -> DsM FcExpr
dsRhs (UnguardedRhs _sp expr maybeDecls) =
  case maybeDecls of
    Nothing -> dsExpr expr
    Just decls -> dsLetDecls decls (dsExpr expr)
dsRhs GuardedRhss {} =
  desugarBug "unsupported guarded RHS after type checking"

dsExpr :: Expr -> DsM FcExpr
dsExpr (EAnn ann inner)
  | Just resolution <- fromAnnotation ann,
    isTupleResolution resolution =
      withTupleConstructorOrigin (resolvedNameOrigin (resolutionTarget resolution)) (dsExpr inner)
dsExpr (EAnn ann inner)
  | Just tcAnn <- fromAnnotation ann =
      dsAnnotatedExpr tcAnn inner
dsExpr (EVar name) = do
  let resolvedName = resolvedOccurrenceName name
  -- Check local bindings first (pattern/lambda variables).
  mLocal <- lookupLocalName resolvedName
  case mLocal of
    Just v -> pure (FcVar v)
    Nothing -> FcVar <$> lookupGlobalOccurrence name
dsExpr (EInt i numericType _) = pure (FcLit (LitInt (numericRuntimeRep numericType) i))
dsExpr (EChar c _) = do
  constructorOrigin <- lookupDataConOrigin "C#"
  pure (boxCharLiteral constructorOrigin c)
dsExpr (ECharHash c _) = pure (FcLit (LitChar WordRep c))
dsExpr (EString s _) = dsStringLiteral s
dsExpr (EStringHash s _) = FcLit . LitAddr <$> primitiveStringBytes s
dsExpr application@EApp {} = dsApplication application
dsExpr (EInfix lhs op rhs) =
  dsInfix lhs op rhs
dsExpr EList {} =
  desugarBug "missing type-checker annotation for list literal"
dsExpr EListComp {} =
  desugarBug "missing type-checker annotation for list comprehension"
dsExpr ETuple {} =
  desugarBug "missing type-checker annotation for tuple literal"
dsExpr (EParen inner) = dsExpr inner
dsExpr (EAnn _ann inner) = dsExpr inner
dsExpr (ETypeSig inner _ty) = dsExpr inner
-- The type checker records visible type arguments on the polymorphic
-- occurrence, which 'dsAnnotatedVar' lowers to 'FcTyApp'.
dsExpr (ETypeApp fun _ty) = dsExpr fun
dsExpr (EIf cond thenE elseE) =
  dsIf cond thenE elseE
dsExpr (ECase scrut alts) =
  dsCase scrut alts
dsExpr (ELambdaPats pats body) =
  dsLambda pats body
dsExpr (ELambdaCase alts) =
  dsLambdaCase alts
dsExpr (ELambdaCases alts) =
  dsLambdaCases alts
dsExpr (ELetDecls decls body) =
  dsLetDecls decls (dsExpr body)
dsExpr expr =
  desugarBug ("unsupported expression form after type checking: " <> take 80 (show expr))

dsAnnotatedVar :: TcAnnotation -> Name -> Expr -> DsM FcExpr
dsAnnotatedVar tcAnn name _expr = do
  let resolvedName = resolvedOccurrenceName name
  mLocal <- lookupLocalName resolvedName
  variable <-
    case mLocal of
      Just local -> pure local
      Nothing -> lookupGlobalOccurrence name
  let occurrenceVar
        | isGhcPrimSeq name = variable {varName = seqPseudoOpName}
        | otherwise = variable
      typedExpr = List.foldl' FcTyApp (FcVar occurrenceVar) (tcAnnTypeArgs tcAnn)
  dicts <- mapM dsEvidence (tcAnnEvidenceTerms tcAnn)
  pure (List.foldl' FcApp typedExpr dicts)

isGhcPrimSeq :: Name -> Bool
isGhcPrimSeq name =
  any isSeqResolution (mapMaybe fromAnnotation (nameAnns name))
  where
    isSeqResolution resolution =
      resolutionNamespace resolution == ResolutionNamespaceTerm
        && case resolutionTarget resolution of
          ResolvedTopLevel _ target ->
            nameQualifier target == Just "GHC.Prim"
              && nameText target == "seq"
          _ -> False

dsAnnotatedExpr :: TcAnnotation -> Expr -> DsM FcExpr
dsAnnotatedExpr tcAnn inner = do
  body <- case inner of
    EAnn ann (EInt value TInteger _)
      | Just resolution <- fromAnnotation ann,
        isFromIntegerResolution resolution ->
          dsOverloadedIntegerLiteral tcAnn resolution value
    EAnn _ nested -> dsExpr nested
    EVar name -> dsAnnotatedVar tcAnn name inner
    application@EApp {} -> dsApplication application
    ELetDecls decls body -> dsLetDecls decls (dsExpr body)
    EList elems -> dsList tcAnn elems
    EListComp body quals -> dsListComp tcAnn body quals
    EDo stmts Surface.DoPlain -> dsDo stmts
    ETuple flavor elems -> dsTuple flavor tcAnn elems
    ELambdaPats pats body -> dsLambda pats body
    ELambdaCase alts -> dsLambdaCase alts
    ELambdaCases alts -> dsLambdaCases alts
    EIf cond thenE elseE -> dsIf cond thenE elseE
    EInfix lhs op rhs -> dsInfix lhs op rhs
    ECase scrut alts -> dsCase scrut alts
    _ -> desugarBug ("unsupported annotated expression form after type checking: " <> take 80 (show inner))
  pure (foldr FcTyLam body (tcAnnTypeBinders tcAnn))

dsApplication :: Expr -> DsM FcExpr
dsApplication expression = do
  let (headExpression, arguments) = collectApplications expression
  headCore <- dsExpr headExpression
  argumentCores <- mapM dsExpr arguments
  fields <- constructorApplicationFields headExpression
  case fields of
    Just constructorFields
      | length arguments >= length constructorFields ->
          dsStrictConstructorApplication headCore arguments argumentCores constructorFields
    _ -> pure (foldl FcApp headCore argumentCores)

collectApplications :: Expr -> (Expr, [Expr])
collectApplications expression =
  case applicationView expression of
    Just (function, argument) ->
      case collectApplications function of
        (headExpression, arguments) -> (headExpression, arguments <> [argument])
    Nothing -> (expression, [])

applicationView :: Expr -> Maybe (Expr, Expr)
applicationView expression =
  case expression of
    EAnn _ inner -> applicationView inner
    EParen inner -> applicationView inner
    ETypeSig inner _ -> applicationView inner
    EApp function argument -> Just (function, argument)
    _ -> Nothing

constructorApplicationFields :: Expr -> DsM (Maybe [DataConFieldInfo])
constructorApplicationFields expression =
  case peelApplicationHead expression of
    EVar name -> do
      fields <- dsConstructorFields <$> get
      pure (Map.lookup (nameText name) fields)
    _ -> pure Nothing

peelApplicationHead :: Expr -> Expr
peelApplicationHead expression =
  case expression of
    EAnn _ inner -> peelApplicationHead inner
    EParen inner -> peelApplicationHead inner
    ETypeSig inner _ -> peelApplicationHead inner
    ETypeApp inner _ -> peelApplicationHead inner
    _ -> expression

dsStrictConstructorApplication :: FcExpr -> [Expr] -> [FcExpr] -> [DataConFieldInfo] -> DsM FcExpr
dsStrictConstructorApplication headCore argumentExpressions argumentCores fields = do
  strictBinders <- mapM strictFieldBinder (zip3 argumentExpressions argumentCores fields)
  let replacements =
        zipWith replaceStrictArgument strictBinders argumentCores
          <> drop (length strictBinders) argumentCores
      application = foldl FcApp headCore replacements
      strictArguments =
        [ (argumentCore, binder)
        | (argumentCore, Just binder) <- zip argumentCores strictBinders
        ]
  pure (foldr forceStrictArgument application strictArguments)
  where
    strictFieldBinder (argumentExpression, _, field) =
      case dcfiStrict field of
        False -> pure Nothing
        True ->
          Just
            <$> freshInternalVar
              "_strict_field"
              (fromMaybe (dcfiType field) (exprAnnotationType argumentExpression))

    replaceStrictArgument maybeBinder argumentCore =
      maybe argumentCore FcVar maybeBinder

    forceStrictArgument (argumentCore, binder) body =
      FcCase argumentCore binder [FcAlt DefaultAlt [] body]

dsDo :: [DoStmt Expr] -> DsM FcExpr
dsDo stmts =
  case stmts of
    [] -> desugarBug "cannot desugar an empty do block"
    [stmt] ->
      case peelDoStmtAnn stmt of
        DoExpr body -> dsExpr body
        other -> desugarBug ("unsupported final do statement after type checking: " <> take 80 (show other))
    stmt : rest ->
      case peelDoStmtAnn stmt of
        DoLetDecls decls -> dsLetDecls decls (dsDo rest)
        DoBind pat action -> dsDoBind stmt action (dsDoPatternContinuation pat rest)
        DoExpr action -> dsDoBind stmt action (dsDoDiscardContinuation stmt rest)
        other -> desugarBug ("unsupported do statement after type checking: " <> take 80 (show other))

dsDoBind :: DoStmt Expr -> Expr -> DsM FcExpr -> DsM FcExpr
dsDoBind stmt action continuation = do
  (tcAnn, resolution) <- requiredDoBindOccurrence stmt
  let bindName = resolvedAnnotationName resolution
  bind <- dsAnnotatedVar tcAnn bindName (EVar bindName)
  action' <- dsExpr action
  FcApp (FcApp bind action') <$> continuation

dsDoPatternContinuation :: Pattern -> [DoStmt Expr] -> DsM FcExpr
dsDoPatternContinuation pat rest = do
  argTy <- lambdaPatternTypeRequired pat
  arg <- freshInternalVar "_do" argTy
  body <-
    case directPatternBindings pat arg of
      Just bindings -> withLocals bindings (dsDo rest)
      Nothing -> do
        failure <- noPatternMatch [arg]
        dsMatchPattern arg pat (dsDo rest) failure
  pure (FcLam arg body)

dsDoDiscardContinuation :: DoStmt Expr -> [DoStmt Expr] -> DsM FcExpr
dsDoDiscardContinuation stmt rest = do
  (tcAnn, _) <- requiredDoBindOccurrence stmt
  argTy <- doBindArgumentType tcAnn
  arg <- freshInternalVar "_do" argTy
  FcLam arg <$> dsDo rest

doBindArgumentType :: TcAnnotation -> DsM TcType
doBindArgumentType tcAnn =
  case tcAnnType tcAnn of
    TcFunTy _ (TcFunTy (TcFunTy argTy _) _) -> pure argTy
    ty -> desugarBug ("unexpected >>= type while desugaring do notation: " <> show ty)

requiredDoBindOccurrence :: DoStmt Expr -> DsM (TcAnnotation, ResolutionAnnotation)
requiredDoBindOccurrence stmt =
  case doBindOccurrence stmt of
    Just occurrence -> pure occurrence
    Nothing -> desugarBug ("missing >>= occurrence annotation while desugaring do notation: " <> take 80 (show stmt))

doBindOccurrence :: DoStmt Expr -> Maybe (TcAnnotation, ResolutionAnnotation)
doBindOccurrence = go Nothing Nothing
  where
    go maybeTc maybeResolution stmt =
      case stmt of
        DoAnn ann inner ->
          let maybeTc' = (fromAnnotation ann :: Maybe TcAnnotation) <|> maybeTc
              maybeResolution' = (fromAnnotation ann :: Maybe ResolutionAnnotation) <|> maybeResolution
           in go maybeTc' maybeResolution' inner
        _ -> (,) <$> maybeTc <*> maybeResolution

dsOverloadedIntegerLiteral :: TcAnnotation -> ResolutionAnnotation -> Integer -> DsM FcExpr
dsOverloadedIntegerLiteral tcAnn resolution value = do
  fromIntegerExpr <- dsAnnotatedVar tcAnn (resolvedAnnotationName resolution) (EInt value TInteger (T.pack (show value)))
  integerExpr <- dsIntegerLiteral resolution value
  pure (FcApp fromIntegerExpr integerExpr)

dsIntegerLiteral :: ResolutionAnnotation -> Integer -> DsM FcExpr
dsIntegerLiteral resolution value = do
  conTy <- lookupType "IS"
  con <- freshVar "IS" conTy
  constructorOrigin <- lookupDataConOrigin "IS"
  let resolved name var = var {varResolvedName = integerHelperOrigin resolution name}
      small integer = FcApp (FcVar con {varResolvedName = Just constructorOrigin}) (FcLit (LitInt IntRep integer))
  if value >= minIntLiteral && value <= maxIntLiteral
    then pure (small value)
    else do
      let integerTy = TcTyCon (TyCon "Integer" 0) []
          binaryTy = TcFunTy integerTy (TcFunTy integerTy integerTy)
          unaryTy = TcFunTy integerTy integerTy
      add <- freshVar "integerAdd" binaryTy
      multiply <- freshVar "integerMul" binaryTy
      negateInteger <- freshVar "integerNegate" unaryTy
      let resolvedAdd = resolved "integerAdd" add
          resolvedMultiply = resolved "integerMul" multiply
          resolvedNegate = resolved "integerNegate" negateInteger
      let buildPositive integer
            | integer <= maxIntLiteral = small integer
            | otherwise =
                let (quotient, remainder) = integer `quotRem` literalBase
                 in FcApp
                      (FcApp (FcVar resolvedAdd) (FcApp (FcApp (FcVar resolvedMultiply) (buildPositive quotient)) (small literalBase)))
                      (small remainder)
          magnitude = buildPositive (abs value)
      pure
        ( if value < 0
            then FcApp (FcVar resolvedNegate) magnitude
            else magnitude
        )
  where
    literalBase = 1000000000
    minIntLiteral = -9223372036854775808
    maxIntLiteral = 9223372036854775807

integerHelperOrigin :: ResolutionAnnotation -> Text -> Maybe FcSymbolOrigin
integerHelperOrigin resolution symbolName =
  case resolvedNameOrigin (resolutionTarget resolution) of
    Just (FcTopLevelOrigin packageName _ _) -> Just (FcTopLevelOrigin packageName "GHC.Internal.Integer" symbolName)
    _ -> Nothing

resolvedAnnotationName :: ResolutionAnnotation -> Name
resolvedAnnotationName resolution =
  name {nameAnns = Surface.mkAnnotation resolution : nameAnns name}
  where
    name =
      case resolutionTarget resolution of
        ResolvedTopLevel _ target -> mkName (nameQualifier target) (nameType target) (nameText target)
        ResolvedLocal _ target -> qualifyName Nothing target
        ResolvedBuiltin target -> mkName Nothing NameVarId target
        ResolvedError {} -> mkName Nothing NameVarId (resolutionName resolution)

resolvedOccurrenceName :: Name -> Name
resolvedOccurrenceName name =
  maybe
    name
    resolvedAnnotationName
    ( listToMaybe
        [ resolution
        | resolution <- mapMaybe fromAnnotation (nameAnns name),
          resolutionNamespace resolution == ResolutionNamespaceTerm
        ]
    )

resolvedOccurrenceOrigin :: Name -> Maybe FcSymbolOrigin
resolvedOccurrenceOrigin name =
  resolvedNameOrigin . resolutionTarget
    =<< listToMaybe
      [ resolution
      | resolution <- mapMaybe fromAnnotation (nameAnns name),
        resolutionNamespace resolution == ResolutionNamespaceTerm
      ]

lookupGlobalOccurrence :: Name -> DsM Var
lookupGlobalOccurrence name =
  case resolvedOccurrenceOrigin name of
    Nothing -> desugarBug ("missing resolved global identity for variable use: " <> T.unpack (nameToText name))
    Just origin -> do
      globalVariables <- gets dsGlobalVars
      case Map.lookup origin globalVariables of
        Just variable -> pure variable
        Nothing -> desugarBug ("variable use is not in the desugarer environment: " <> T.unpack (fcSymbolOriginText origin))

resolvedNameOrigin :: ResolvedName -> Maybe FcSymbolOrigin
resolvedNameOrigin resolved =
  case resolved of
    ResolvedTopLevel packageId name ->
      Just
        FcTopLevelOrigin
          { fcOriginPackage = packageIdText packageId,
            fcOriginModule = fromMaybe "" (nameQualifier name),
            fcOriginName = nameText name
          }
    ResolvedBuiltin name -> Just (FcBuiltinOrigin name)
    ResolvedLocal {} -> Nothing
    ResolvedError {} -> Nothing

isFromIntegerResolution :: ResolutionAnnotation -> Bool
isFromIntegerResolution resolution =
  resolutionNamespace resolution == ResolutionNamespaceTerm
    && resolutionName resolution == "fromInteger"

dsInfix :: Expr -> Name -> Expr -> DsM FcExpr
dsInfix lhs op rhs = do
  operator <- dsInfixOperator op
  left <- dsExpr lhs
  right <- dsExpr rhs
  fields <- Map.lookup (nameText op) . dsConstructorFields <$> get
  case fields of
    Just constructorFields
      | length constructorFields == 2 ->
          dsStrictConstructorApplication operator [lhs, rhs] [left, right] constructorFields
    _ -> pure (FcApp (FcApp operator left) right)

dsInfixOperator :: Name -> DsM FcExpr
dsInfixOperator op =
  case nameTcAnnotation op of
    Just tcAnn -> dsAnnotatedVar tcAnn op (EVar op)
    Nothing -> dsExpr (EVar op)

dsIf :: Expr -> Expr -> Expr -> DsM FcExpr
dsIf cond thenE elseE = do
  cond' <- dsExpr cond
  then' <- dsExpr thenE
  else' <- dsExpr elseE
  boolTy <- primBoolType
  binder <- freshVar "_if" boolTy
  trueConstructor <- primitiveDataConOrigin "GHC.Types" "True"
  falseConstructor <- primitiveDataConOrigin "GHC.Types" "False"
  pure
    ( FcCase
        cond'
        binder
        [ FcAlt (DataAlt (fcConstructorIdFromSymbol trueConstructor)) [] then',
          FcAlt (DataAlt (fcConstructorIdFromSymbol falseConstructor)) [] else'
        ]
    )

dsCase :: Expr -> [CaseAlt Expr] -> DsM FcExpr
dsCase scrut alts = do
  scrut' <- dsExpr scrut
  scrutTy <-
    case exprAnnotationType scrut of
      Just ty -> pure ty
      Nothing -> fcExprTypeM scrut'
  case scrut' of
    FcVar scrutVar -> dsCaseAlternatives scrutVar alts
    _ -> do
      scrutVar <- freshVar "_case_value" scrutTy
      body <- dsCaseAlternatives scrutVar alts
      bindCaseScrutinee scrutVar scrut' body

-- A source case does not necessarily evaluate its scrutinee: wildcard,
-- variable, and lazy patterns match immediately. Bind a lifted scrutinee with
-- a non-recursive binding and let the compiled patterns introduce strict Core
-- cases only when they actually inspect it. The FC evaluator and backends make
-- such a binding lazy at LiftedRep and strict at every unlifted representation.
bindCaseScrutinee :: Var -> FcExpr -> FcExpr -> DsM FcExpr
bindCaseScrutinee scrutVar scrutinee body =
  pure (FcLet (FcNonRec scrutVar scrutinee) body)

isOverloadedIntegerPattern :: Pattern -> Bool
isOverloadedIntegerPattern pat =
  case peelPatternAnn pat of
    PLit lit -> isOverloadedIntegerLiteral lit
    PNegLit lit -> isOverloadedIntegerLiteral lit
    PParen inner -> isOverloadedIntegerPattern inner
    PStrict inner -> isOverloadedIntegerPattern inner
    PIrrefutable inner -> isOverloadedIntegerPattern inner
    PAs _ inner -> isOverloadedIntegerPattern inner
    PTypeSig inner _ -> isOverloadedIntegerPattern inner
    _ -> False

isOverloadedIntegerLiteral :: Surface.Literal -> Bool
isOverloadedIntegerLiteral lit =
  case peelLiteralAnn lit of
    Surface.LitInt _ TInteger _ -> True
    _ -> False

-- | A compiled source match either cannot fail or accepts the code to run when
-- it does fail. Keeping this distinction explicit is what makes an
-- irrefutable alternative structurally discard every later alternative.
data CaseMatchResult
  = CaseMatchInfallible (DsM FcExpr)
  | CaseMatchFallible (DsM FcExpr -> DsM FcExpr)

dsCaseAlternatives :: Var -> [CaseAlt Expr] -> DsM FcExpr
dsCaseAlternatives scrutVar alternatives =
  extractCaseMatchResult combined noMatch
  where
    combined = foldr (combineCaseMatchResults . dsCaseAlternative scrutVar) alwaysFailCaseMatch alternatives
    noMatch = do
      binder <- freshVar "_case_nomatch" (varType scrutVar)
      pure (FcCase (FcVar scrutVar) binder [])

alwaysFailCaseMatch :: CaseMatchResult
alwaysFailCaseMatch = CaseMatchFallible id

extractCaseMatchResult :: CaseMatchResult -> DsM FcExpr -> DsM FcExpr
extractCaseMatchResult matchResult failure =
  case matchResult of
    CaseMatchInfallible body -> body
    CaseMatchFallible build -> build failure

combineCaseMatchResults :: CaseMatchResult -> CaseMatchResult -> CaseMatchResult
combineCaseMatchResults first second =
  case first of
    CaseMatchInfallible {} -> first
    CaseMatchFallible buildFirst ->
      case second of
        CaseMatchInfallible body -> CaseMatchInfallible (buildFirst body)
        CaseMatchFallible buildSecond -> CaseMatchFallible (buildFirst . buildSecond)

dsCaseAlternative :: Var -> CaseAlt Expr -> CaseMatchResult
dsCaseAlternative scrutVar alternative =
  dsPatternMatchResult scrutVar (caseAltPattern alternative) (dsRhs (caseAltRhs alternative))

dsPatternMatchResult :: Var -> Pattern -> DsM FcExpr -> CaseMatchResult
dsPatternMatchResult scrutVar pattern' success =
  case peelPatternAnn pattern' of
    PVar name ->
      CaseMatchInfallible (withLocals [(unqualifiedNameText name, scrutVar)] success)
    PWildcard -> CaseMatchInfallible success
    PParen inner -> dsPatternMatchResult scrutVar inner success
    PAs name inner ->
      withCaseMatchLocals [(unqualifiedNameText name, scrutVar)] (dsPatternMatchResult scrutVar inner success)
    PStrict inner ->
      adjustCaseMatchResult (forceCaseScrutinee scrutVar) (dsPatternMatchResult scrutVar inner success)
    PIrrefutable inner ->
      CaseMatchInfallible (withLocals (irrefutablePatternBindings scrutVar inner) success)
    PTypeSig inner _ -> dsPatternMatchResult scrutVar inner success
    _ ->
      CaseMatchFallible $ \failureAction -> do
        failure <- failureAction
        dsMatchPattern scrutVar pattern' success failure

withCaseMatchLocals :: [(Text, Var)] -> CaseMatchResult -> CaseMatchResult
withCaseMatchLocals bindings matchResult =
  case matchResult of
    CaseMatchInfallible body -> CaseMatchInfallible (withLocals bindings body)
    CaseMatchFallible build ->
      CaseMatchFallible $ \failureAction -> do
        -- Pattern binders scope over this alternative only. Build the next
        -- alternative before extending the desugaring environment so a failed
        -- as-pattern cannot capture names in its fall-through path.
        failure <- failureAction
        withLocals bindings (build (pure failure))

adjustCaseMatchResult :: (FcExpr -> DsM FcExpr) -> CaseMatchResult -> CaseMatchResult
adjustCaseMatchResult adjust matchResult =
  case matchResult of
    CaseMatchInfallible body -> CaseMatchInfallible (body >>= adjust)
    CaseMatchFallible build ->
      CaseMatchFallible $ \failure -> do
        body <- build failure
        adjust body

forceCaseScrutinee :: Var -> FcExpr -> DsM FcExpr
forceCaseScrutinee scrutVar body = do
  caseBinder <- freshInternalVar "_bang" (varType scrutVar)
  pure
    ( FcCase
        (FcVar scrutVar)
        caseBinder
        [FcAlt DefaultAlt [] (substExprVar scrutVar caseBinder body)]
    )

dsOverloadedIntegerPatternMatch :: Var -> Pattern -> DsM FcExpr -> FcExpr -> DsM FcExpr
dsOverloadedIntegerPatternMatch scrutVar pat success failure = do
  test <- dsOverloadedIntegerPatternTest (FcVar scrutVar) pat
  trueBranch <- success
  testTy <- fcExprTypeM test
  binder <- freshVar "_case_guard" testTy
  trueConstructor <- dataConOriginForType testTy "True"
  falseConstructor <- dataConOriginForType testTy "False"
  pure
    ( FcCase
        test
        binder
        [ FcAlt (DataAlt (fcConstructorIdFromSymbol trueConstructor)) [] trueBranch,
          FcAlt (DataAlt (fcConstructorIdFromSymbol falseConstructor)) [] failure
        ]
    )

dsOverloadedIntegerPatternTest :: FcExpr -> Pattern -> DsM FcExpr
dsOverloadedIntegerPatternTest scrutValue pat =
  case integerPatternValue pat of
    Just (value, isNegative) -> do
      (fromIntegerTc, fromIntegerResolution) <- requiredPatternOccurrence "fromInteger" pat
      (eqTc, eqResolution) <- requiredPatternOccurrence "==" pat
      fromIntegerExpr <- dsAnnotatedVar fromIntegerTc (resolvedAnnotationName fromIntegerResolution) (EInt value TInteger (T.pack (show value)))
      integerExpr <- dsIntegerLiteral fromIntegerResolution value
      eqExpr <- dsAnnotatedVar eqTc (resolvedAnnotationName eqResolution) (EVar (resolvedAnnotationName eqResolution))
      let positiveValue = FcApp fromIntegerExpr integerExpr
      patternValue <-
        if isNegative
          then do
            (negateTc, negateResolution) <- requiredPatternOccurrence "negate" pat
            negateExpr <- dsAnnotatedVar negateTc (resolvedAnnotationName negateResolution) (EVar (resolvedAnnotationName negateResolution))
            pure (FcApp negateExpr positiveValue)
          else pure positiveValue
      pure (FcApp (FcApp eqExpr scrutValue) patternValue)
    Nothing ->
      desugarBug ("expected overloaded integer pattern while desugaring: " <> take 80 (show pat))

integerPatternValue :: Pattern -> Maybe (Integer, Bool)
integerPatternValue pat =
  case peelPatternAnn pat of
    PLit lit -> (,False) <$> overloadedIntegerValue lit
    PNegLit lit -> (,True) <$> overloadedIntegerValue lit
    PParen inner -> integerPatternValue inner
    PStrict inner -> integerPatternValue inner
    PIrrefutable inner -> integerPatternValue inner
    PAs _ inner -> integerPatternValue inner
    PTypeSig inner _ -> integerPatternValue inner
    _ -> Nothing

overloadedIntegerValue :: Surface.Literal -> Maybe Integer
overloadedIntegerValue lit =
  case peelLiteralAnn lit of
    Surface.LitInt value TInteger _ -> Just value
    _ -> Nothing

requiredPatternOccurrence :: Text -> Pattern -> DsM (TcAnnotation, ResolutionAnnotation)
requiredPatternOccurrence name pat =
  case patternOccurrence name pat of
    Just occurrence -> pure occurrence
    Nothing -> desugarBug ("missing " <> T.unpack name <> " annotation for overloaded integer pattern")

patternOccurrence :: Text -> Pattern -> Maybe (TcAnnotation, ResolutionAnnotation)
patternOccurrence target =
  go Nothing
  where
    go currentTc pat =
      case pat of
        PAnn ann inner ->
          case (fromAnnotation ann :: Maybe TcAnnotation, fromAnnotation ann :: Maybe ResolutionAnnotation) of
            (Just tcAnn, _) -> go (Just tcAnn) inner
            (_, Just resolution)
              | resolutionName resolution == target,
                resolutionNamespace resolution == ResolutionNamespaceTerm ->
                  case currentTc of
                    Just tcAnn -> Just (tcAnn, resolution)
                    Nothing -> Nothing
            _ -> go currentTc inner
        PParen inner -> go currentTc inner
        PStrict inner -> go currentTc inner
        PIrrefutable inner -> go currentTc inner
        PAs _ inner -> go currentTc inner
        PTypeSig inner _ -> go currentTc inner
        _ -> Nothing

-- | Desugar local let/where declarations as a recursive Core let.
--
-- Type checking has already validated the binding group. Here we only need
-- stable Core variables so RHSs and the body refer to the same local binders.
dsLetDecls :: [Decl] -> DsM FcExpr -> DsM FcExpr
dsLetDecls decls bodyAction = do
  groups <- groupLocalDecls decls
  let names = map localGroupName groups
      vars = map localGroupBinder groups
  let localBindings = zip names vars
  withLocals localBindings $ do
    rhsBindings <- zipWithM dsLocalGroup vars groups
    body <- bodyAction
    pure $
      if null rhsBindings
        then body
        else FcLet (FcRec rhsBindings) body

data LocalDeclGroup
  = LocalFunction !Text !Var ![Match]
  | LocalPattern !Text !Var !(Rhs Expr)

localGroupName :: LocalDeclGroup -> Text
localGroupName group =
  case group of
    LocalFunction name _ _ -> name
    LocalPattern name _ _ -> name

localGroupBinder :: LocalDeclGroup -> Var
localGroupBinder group =
  case group of
    LocalFunction _ var _ -> var
    LocalPattern _ var _ -> var

groupLocalDecls :: [Decl] -> DsM [LocalDeclGroup]
groupLocalDecls [] = pure []
groupLocalDecls (decl : rest) = do
  maybeFun <- extractLocalFunction decl
  case maybeFun of
    Just (name, var, matches) -> do
      let (sameNameDecls, rest') = span (hasSameLocalFunctionName name) rest
      sameGroups <- mapM extractLocalFunctionRequired sameNameDecls
      let allMatches = matches ++ concatMap (\(_, _, ms) -> ms) sameGroups
      restGroups <- groupLocalDecls rest'
      pure (LocalFunction name var allMatches : restGroups)
    Nothing -> do
      maybePattern <- extractLocalPattern decl
      restGroups <- groupLocalDecls rest
      pure (maybe restGroups (: restGroups) maybePattern)

extractLocalFunction :: Decl -> DsM (Maybe (Text, Var, [Match]))
extractLocalFunction decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind name matches) -> do
      let localName = unqualifiedNameText name
      ty <- localDeclTypeRequired localName decl
      var <- freshVar localName ty
      pure (Just (localName, var, matches))
    _ -> pure Nothing

extractLocalFunctionRequired :: Decl -> DsM (Text, Var, [Match])
extractLocalFunctionRequired decl = do
  maybeFun <- extractLocalFunction decl
  case maybeFun of
    Just fun -> pure fun
    Nothing -> desugarBug ("expected local function declaration: " <> take 80 (show decl))

hasSameLocalFunctionName :: Text -> Decl -> Bool
hasSameLocalFunctionName name decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind declName _) -> unqualifiedNameText declName == name
    _ -> False

extractLocalPattern :: Decl -> DsM (Maybe LocalDeclGroup)
extractLocalPattern decl =
  case peelDeclAnn decl of
    DeclValue (PatternBind _ pat rhs) ->
      case barePatternName pat of
        Just name -> do
          ty <- localDeclTypeRequired name decl
          var <- freshVar name ty
          pure (Just (LocalPattern name var rhs))
        Nothing -> pure Nothing
    _ -> pure Nothing

barePatternName :: Pattern -> Maybe Text
barePatternName pat =
  case pat of
    PVar name -> Just (unqualifiedNameText name)
    PAnn _ inner -> barePatternName inner
    PParen inner -> barePatternName inner
    _ -> Nothing

localDeclTypeRequired :: Text -> Decl -> DsM TcType
localDeclTypeRequired name decl =
  case localDeclType decl of
    Just ty -> pure ty
    Nothing -> desugarBug ("missing type-checker annotation for local declaration " <> T.unpack name)

localDeclType :: Decl -> Maybe TcType
localDeclType decl =
  case decl of
    DeclAnn ann inner ->
      case fromAnnotation ann of
        Just tcAnn -> Just (tcAnnType tcAnn)
        Nothing -> localDeclType inner
    _ -> Nothing

dsLocalGroup :: Var -> LocalDeclGroup -> DsM (Var, FcExpr)
dsLocalGroup var group =
  case group of
    LocalFunction _ _ matches -> do
      rhs <- dsMatches (varType var) matches
      pure (var, rhs)
    LocalPattern _ _ rhs -> do
      rhs' <- dsRhs rhs
      pure (var, rhs')

dsStringLiteral :: Text -> DsM FcExpr
dsStringLiteral text = do
  nil <- nilList charTy
  cons <- consExpr charTy
  constructorOrigin <- lookupDataConOrigin "C#"
  pure (T.foldr (applyConsChar constructorOrigin cons) nil text)

primitiveStringBytes :: Text -> DsM BS.ByteString
primitiveStringBytes text =
  case traverse toLatin1Byte (T.unpack text) of
    Just bytes -> pure (BS.pack bytes)
    Nothing -> desugarBug "primitive string literal escaped parser Latin-1 validation"
  where
    toLatin1Byte character
      | ord character < 256 = Just (fromIntegral (ord character))
      | otherwise = Nothing

dsList :: TcAnnotation -> [Expr] -> DsM FcExpr
dsList tcAnn elems =
  case tcAnnTypeArgs tcAnn of
    [elemTy] ->
      do
        nil <- nilList elemTy
        cons <- consExpr elemTy
        foldr (applyCons cons) nil <$> mapM dsExpr elems
    elemTys ->
      desugarBug ("list annotation arity mismatch: expected 1 type argument, got " <> show (length elemTys))

dsListComp :: TcAnnotation -> Expr -> [CompStmt] -> DsM FcExpr
dsListComp tcAnn body quals = do
  elemTy <- listCompElemTy tcAnn
  nil <- nilList elemTy
  dsCompQuals elemTy body quals nil

listCompElemTy :: TcAnnotation -> DsM TcType
listCompElemTy tcAnn =
  case tcAnnTypeArgs tcAnn of
    [elemTy] -> pure elemTy
    [] -> listElemTyM (tcAnnType tcAnn)
    elemTys ->
      desugarBug ("list comprehension annotation arity mismatch: expected 1 type argument, got " <> show (length elemTys))

dsCompQuals :: TcType -> Expr -> [CompStmt] -> FcExpr -> DsM FcExpr
dsCompQuals elemTy body quals tailExpr =
  case quals of
    [] -> do
      body' <- dsExpr body
      cons <- consExpr elemTy
      pure (applyCons cons body' tailExpr)
    qual : rest ->
      case peelCompStmtAnn qual of
        CompGen pat src -> dsCompGen elemTy body pat src rest tailExpr
        CompGuard guard -> dsCompGuard elemTy body guard rest tailExpr
        CompLetDecls decls -> dsLetDecls decls (dsCompQuals elemTy body rest tailExpr)
        CompThen {} -> unsupportedCompQual qual
        CompThenBy {} -> unsupportedCompQual qual
        CompGroupUsing {} -> unsupportedCompQual qual
        CompGroupByUsing {} -> unsupportedCompQual qual
        CompAnn {} -> desugarBug "unreachable annotated list comprehension qualifier"

unsupportedCompQual :: CompStmt -> DsM a
unsupportedCompQual qual =
  desugarBug ("unsupported list comprehension qualifier after type checking: " <> take 80 (show qual))

dsCompGuard :: TcType -> Expr -> Expr -> [CompStmt] -> FcExpr -> DsM FcExpr
dsCompGuard elemTy body guard rest tailExpr = do
  guard' <- dsExpr guard
  trueBranch <- dsCompQuals elemTy body rest tailExpr
  boolTy <- primBoolType
  binder <- freshInternalVar "_lc_guard" boolTy
  trueConstructor <- primitiveDataConOrigin "GHC.Types" "True"
  falseConstructor <- primitiveDataConOrigin "GHC.Types" "False"
  pure
    ( FcCase
        guard'
        binder
        [ FcAlt (DataAlt (fcConstructorIdFromSymbol trueConstructor)) [] trueBranch,
          FcAlt (DataAlt (fcConstructorIdFromSymbol falseConstructor)) [] tailExpr
        ]
    )

dsCompGen :: TcType -> Expr -> Pattern -> Expr -> [CompStmt] -> FcExpr -> DsM FcExpr
dsCompGen elemTy body pat src rest tailExpr = do
  src' <- dsExpr src
  srcListTy <- fcExprTypeM src'
  srcElemTy <- listElemTyM srcListTy
  worker <- freshInternalVar "$lc" (TcFunTy srcListTy (listType elemTy))
  listVar <- freshInternalVar "_lc_list" srcListTy
  headVar <- freshInternalVar "_lc_head" srcElemTy
  restVar <- freshInternalVar "_lc_tail" srcListTy
  caseBinder <- freshInternalVar "_lc_scrut" srcListTy
  nilConstructor <- lookupDataConOrigin "[]"
  consConstructor <- lookupDataConOrigin ":"
  let recurTail = FcApp (FcVar worker) (FcVar restVar)
  consRhs <- dsCompGenMatch elemTy body pat rest headVar recurTail
  let workerBody =
        FcLam listVar $
          FcCase
            (FcVar listVar)
            caseBinder
            [ FcAlt (DataAlt (fcConstructorIdFromSymbol nilConstructor)) [] tailExpr,
              FcAlt (DataAlt (fcConstructorIdFromSymbol consConstructor)) [headVar, restVar] consRhs
            ]
  pure (FcLet (FcRec [(worker, workerBody)]) (FcApp (FcVar worker) src'))

dsCompGenMatch :: TcType -> Expr -> Pattern -> [CompStmt] -> Var -> FcExpr -> DsM FcExpr
dsCompGenMatch elemTy body pat rest headVar skipExpr =
  case directPatternBindings pat headVar of
    Just bindings ->
      withLocals bindings (dsCompQuals elemTy body rest skipExpr)
    Nothing -> do
      packageId <- gets dsPrimPackageId
      let (con, binderNames) = dsPatternPure packageId pat
      case con of
        DefaultAlt ->
          desugarBug ("unsupported list comprehension generator pattern: " <> take 80 (show pat))
        _ -> do
          binderTys <- patternBinderTypesM pat (varType headVar)
          binders <- zipWithM freshVar binderNames binderTys
          (evidenceBinders, dictionaries) <- patternEvidenceBinders pat
          matched <- withDicts dictionaries (withLocals (zip binderNames binders) (dsCompQuals elemTy body rest skipExpr))
          caseBinder <- freshInternalVar "_lc_match" (varType headVar)
          pure
            ( FcCase
                (FcVar headVar)
                caseBinder
                [ FcAlt con (evidenceBinders <> binders) matched,
                  FcAlt DefaultAlt [] skipExpr
                ]
            )

directPatternBindings :: Pattern -> Var -> Maybe [(Text, Var)]
directPatternBindings pat var =
  case pat of
    PVar name -> Just [(unqualifiedNameText name, var)]
    PWildcard -> Just []
    PAnn _ inner -> directPatternBindings inner var
    PParen inner -> directPatternBindings inner var
    PAs name inner -> ((unqualifiedNameText name, var) :) <$> directPatternBindings inner var
    PStrict inner -> directPatternBindings inner var
    _ -> Nothing

dsLambda :: [Pattern] -> Expr -> DsM FcExpr
dsLambda pats body = do
  argTys <- mapM lambdaPatternTypeRequired pats
  vars <- zipWithM freshInternalVar (map lambdaArgName pats) argTys
  body' <-
    case traverse (uncurry directPatternBindings) (zip pats vars) of
      Nothing -> do
        failure <- noPatternMatch vars
        dsMatchPatterns vars pats (dsExpr body) failure
      Just bindings -> withLocals (concat bindings) (dsExpr body)
  pure (foldr FcLam body' vars)

dsLambdaCase :: [CaseAlt Expr] -> DsM FcExpr
dsLambdaCase alts =
  case alts of
    firstAlt : _ -> do
      argTy <- lambdaPatternTypeRequired (caseAltPattern firstAlt)
      argVar <- freshInternalVar "_lambda_case" argTy
      body <- dsCaseAlternatives argVar alts
      pure (FcLam argVar body)
    [] -> desugarBug "cannot desugar an empty lambda-case"

dsLambdaCases :: [LambdaCaseAlt] -> DsM FcExpr
dsLambdaCases alts =
  case alts of
    firstAlt : _ -> do
      argTys <- mapM lambdaPatternTypeRequired (lambdaCaseAltPats firstAlt)
      argVars <- mapM (\(index, ty) -> freshInternalVar ("_lambda_cases" <> T.pack (show index)) ty) (zip [0 :: Int ..] argTys)
      body <- dsLambdaCaseAlternatives argVars alts
      pure (foldr FcLam body argVars)
    [] -> desugarBug "cannot desugar an empty multi-argument lambda-case"

dsLambdaCaseAlternatives :: [Var] -> [LambdaCaseAlt] -> DsM FcExpr
dsLambdaCaseAlternatives argVars alts =
  case alts of
    [] -> noPatternMatch argVars
    alt : rest -> do
      failure <- dsLambdaCaseAlternatives argVars rest
      dsMatchPatterns argVars (lambdaCaseAltPats alt) (dsRhs (lambdaCaseAltRhs alt)) failure

lambdaArgName :: Pattern -> Text
lambdaArgName pat =
  case pat of
    PVar name -> unqualifiedNameText name
    PAnn _ inner -> lambdaArgName inner
    PParen inner -> lambdaArgName inner
    PAs name _ -> unqualifiedNameText name
    _ -> "_lam"

dsTuple :: TupleFlavor -> TcAnnotation -> [Maybe Expr] -> DsM FcExpr
dsTuple flavor tcAnn elems = do
  let elemTys = tcAnnTypeArgs tcAnn
  if length elemTys == length elems
    then do
      elems' <- zipWithM dsMaybeTupleElem elemTys elems
      constructor <- tupleConExpr flavor elemTys
      pure (List.foldl' FcApp constructor elems')
    else desugarBug ("tuple annotation arity mismatch: expected " <> show (length elems) <> " type argument(s), got " <> show (length elemTys))

dsMaybeTupleElem :: TcType -> Maybe Expr -> DsM FcExpr
dsMaybeTupleElem _ (Just expr) = dsExpr expr
dsMaybeTupleElem ty Nothing = do
  v <- freshVar "_tuple_section" ty
  pure (FcVar v)

tupleConExpr :: TupleFlavor -> [TcType] -> DsM FcExpr
tupleConExpr flavor elemTys = do
  let arity = length elemTys
      name = tupleConName flavor arity
  constructorOrigin <- gets dsTupleConstructorOrigin
  (constructorTy, resolvedOrigin) <-
    case (flavor, constructorOrigin) of
      (_, Just origin@FcTopLevelOrigin {}) -> (,Just origin) <$> lookupType name
      (Unboxed, _) -> do
        origin <- primitiveDataConOrigin "GHC.Types" name
        pure (unboxedTupleConType arity, Just origin)
      (Boxed, _) -> do
        constructorType <- lookupType name
        origin <- primitiveDataConOrigin "GHC.Tuple" name
        pure (constructorType, Just origin)
  constructor <-
    case resolvedOrigin of
      Just FcBuiltinOrigin {} -> pure (builtinVar name (Unique (-20 - arity)) constructorTy)
      _ -> freshVar name constructorTy
  let representationTypes =
        case flavor of
          Boxed -> []
          Unboxed -> map (runtimeRepToTcType . fromRight liftedRuntimeRep . runtimeRepOfType) elemTys
      typeArguments = representationTypes <> elemTys
  pure (List.foldl' FcTyApp (FcVar constructor {varResolvedName = resolvedOrigin}) typeArguments)

unboxedTupleConType :: Int -> TcType
unboxedTupleConType arity =
  foldr TcForAllTy (foldr (TcFunTy . TcTyVar) resultType valueVariables) (representationVariables <> valueVariables)
  where
    representationVariables =
      [ setTyVarKind KRuntimeRep (TyVarId ("r" <> T.pack (show index)) (Unique (-2000000 - arity * 200 - index)))
      | index <- [1 .. arity]
      ]
    valueVariables =
      [ setTyVarKind (KTYPE (RuntimeRepVar (tvUnique representation))) (TyVarId ("a" <> T.pack (show index)) (Unique (-2000000 - arity * 200 - 100 - index)))
      | (index, representation) <- zip [1 ..] representationVariables
      ]
    resultKind = KTYPE (TupleRep [runtimeRep | variable <- valueVariables, KTYPE runtimeRep <- [tvKind variable]])
    tyConKind = foldr (KFun . tvKind) resultKind valueVariables
    resultType = TcTyCon (mkTyCon (unboxedTupleTyConName arity) arity tyConKind) (map TcTyVar valueVariables)

isTupleResolution :: ResolutionAnnotation -> Bool
isTupleResolution resolution =
  case resolutionForm resolution of
    ResolutionTuple -> True
    ResolutionNamed -> False

withTupleConstructorOrigin :: Maybe FcSymbolOrigin -> DsM a -> DsM a
withTupleConstructorOrigin origin action = do
  previous <- gets dsTupleConstructorOrigin
  modify' (\state -> state {dsTupleConstructorOrigin = origin})
  result <- action
  modify' (\state -> state {dsTupleConstructorOrigin = previous})
  pure result

tupleConName :: TupleFlavor -> Int -> Text
tupleConName flavor arity =
  case flavor of
    Boxed -> "(" <> T.replicate (max 0 (arity - 1)) "," <> ")"
    Unboxed -> "(#" <> T.replicate (max 0 (arity - 1)) "," <> "#)"

applyConsChar :: FcSymbolOrigin -> FcExpr -> Char -> FcExpr -> FcExpr
applyConsChar constructorOrigin cons char =
  applyCons cons (boxCharLiteral constructorOrigin char)

boxCharLiteral :: FcSymbolOrigin -> Char -> FcExpr
boxCharLiteral constructorOrigin char =
  FcApp
    ( FcVar
        ( (Var "C#" (Unique (-12)) (TcFunTy charHashTy charTy))
            { varResolvedName = Just constructorOrigin
            }
        )
    )
    (FcLit (LitChar WordRep char))

applyCons :: FcExpr -> FcExpr -> FcExpr -> FcExpr
applyCons cons headExpr =
  FcApp (FcApp cons headExpr)

nilList :: TcType -> DsM FcExpr
nilList elemTy = do
  constructor <- listConstructorVar "[]" (Unique (-10))
  pure (FcTyApp (FcVar constructor) elemTy)

consExpr :: TcType -> DsM FcExpr
consExpr elemTy = do
  constructor <- listConstructorVar ":" (Unique (-11))
  pure (FcTyApp (FcVar constructor) elemTy)

listConstructorVar :: Text -> Unique -> DsM Var
listConstructorVar name unique = do
  constructorType <- lookupType name
  packageId <- gets dsPrimPackageId
  pure
    (Var name unique constructorType)
      { varResolvedName = Just (FcTopLevelOrigin (packageIdText packageId) "GHC.Types" name)
      }

builtinVar :: Text -> Unique -> TcType -> Var
builtinVar name unique ty =
  (Var name unique ty) {varResolvedName = Just (FcBuiltinOrigin name)}

listType :: TcType -> TcType
listType ty =
  TcTyCon (TyCon "[]" 1) [ty]

dsEvidence :: EvTerm -> DsM FcExpr
dsEvidence evidence =
  case evidence of
    EvGiven (ClassPred className args) -> do
      st <- get
      case Map.lookup (exactDictKey className args) (dsLocalDicts st) <|> Map.lookup (dictKey className args) (dsLocalDicts st) of
        Just var -> pure (FcVar var)
        Nothing ->
          desugarBug ("missing local dictionary for " <> T.unpack (dictKey className args))
    EvGiven EqPred {} ->
      unitConstructor
    EvDict dictOrigin dictName typeArgs contextEvidence -> do
      dictTy <- lookupType dictName
      contextDicts <- mapM dsEvidence contextEvidence
      let origin = fmap (\(packageName, moduleName) -> FcTopLevelOrigin packageName moduleName dictName) dictOrigin
          dictExpr = List.foldl' FcTyApp (FcVar (Var dictName (Unique (-199)) dictTy) {varResolvedName = origin}) typeArgs
      pure (List.foldl' FcApp dictExpr contextDicts)
    EvCoercion {} ->
      unitConstructor
    EvSuperClass source sourceOrigin sourcePredicate fieldTypes fieldIndex -> do
      sourceExpression <- dsEvidence source
      sourceBinder <- freshVar "$super_source" (predType sourcePredicate)
      fieldBinders <- zipWithM (\index fieldType -> freshVar ("$super_field" <> T.pack (show index)) fieldType) [0 :: Int ..] fieldTypes
      selected <-
        case drop fieldIndex fieldBinders of
          field : _ -> pure field
          [] -> desugarBug "superclass field index is outside the dictionary layout"
      constructor <-
        case sourcePredicate of
          ClassPred className _ -> pure (fcDictionaryConstructorName className)
          EqPred {} -> desugarBug "cannot select a superclass from equality evidence"
      let constructorOrigin =
            case sourceOrigin of
              Just (packageName, moduleName) -> FcTopLevelOrigin packageName moduleName constructor
              Nothing -> FcBuiltinOrigin constructor
      pure (FcCase sourceExpression sourceBinder [FcAlt (DataAlt (fcConstructorIdFromSymbol constructorOrigin)) fieldBinders (FcVar selected)])
    EvCast dict _co ->
      dsEvidence dict
    EvTypeable origin ty arguments ->
      dsTypeableEvidence origin ty arguments
    EvVarTerm {} ->
      desugarBug "unresolved evidence variable in type-checker annotation"

dsTypeableEvidence :: Maybe (Text, Text) -> TcType -> [EvTerm] -> DsM FcExpr
dsTypeableEvidence typeableOrigin ty argumentEvidence = do
  argumentTypes <-
    case typeableTypeView ty of
      Just (_, arguments) -> pure arguments
      Nothing -> desugarBug ("cannot construct Typeable evidence for " <> show ty)
  if length argumentTypes /= length argumentEvidence
    then desugarBug "Typeable evidence argument arity mismatch"
    else do
      argumentRepresentations <- zipWithM (dsTypeableArgument typeableOrigin) argumentTypes argumentEvidence
      representation <- dsTypeRepresentation typeableOrigin ty argumentRepresentations
      proxy <- freshVar "$typeable_proxy" (TcTyCon (TyCon "Proxy" 1) [ty])
      value <- freshVar "$typeable_value" ty
      dictionaryOrigin <- typeableConstructorOrigin typeableOrigin (fcDictionaryConstructorName "Typeable")
      let proxyMethod = FcLam proxy representation
          valueMethod = FcLam value representation
          dictionaryConstructor =
            ( Var
                (fcDictionaryConstructorName "Typeable")
                (Unique (-2100))
                ( TcForAllTy
                    typeableTyVar
                    ( TcFunTy
                        (TcFunTy (TcTyCon (TyCon "Proxy" 1) [TcTyVar typeableTyVar]) typeRepTy)
                        ( TcFunTy
                            (TcFunTy (TcTyVar typeableTyVar) typeRepTy)
                            (TcTyCon (TyCon "Typeable" 1) [TcTyVar typeableTyVar])
                        )
                    )
                )
            )
              { varResolvedName = Just dictionaryOrigin
              }
      pure (FcApp (FcApp (FcTyApp (FcVar dictionaryConstructor) ty) proxyMethod) valueMethod)

dsTypeableArgument :: Maybe (Text, Text) -> TcType -> EvTerm -> DsM FcExpr
dsTypeableArgument typeableOrigin ty evidence = do
  dictionary <- dsEvidence evidence
  dictionaryBinder <- freshVar "$typeable_dictionary" (TcTyCon (TyCon "Typeable" 1) [ty])
  proxyMethod <- freshVar "$typeable_proxy_method" (TcFunTy (TcTyCon (TyCon "Proxy" 1) [ty]) typeRepTy)
  valueMethod <- freshVar "$typeable_value_method" (TcFunTy ty typeRepTy)
  dictionaryOrigin <- typeableConstructorOrigin typeableOrigin (fcDictionaryConstructorName "Typeable")
  proxyOrigin <- typeableConstructorOrigin typeableOrigin "Proxy"
  let proxyConstructor =
        (Var "Proxy" (Unique (-2101)) (TcForAllTy typeableTyVar (TcTyCon (TyCon "Proxy" 1) [TcTyVar typeableTyVar])))
          { varResolvedName = Just proxyOrigin
          }
      proxy = FcTyApp (FcVar proxyConstructor) ty
  pure
    ( FcCase
        dictionary
        dictionaryBinder
        [ FcAlt
            (DataAlt (fcConstructorIdFromSymbol dictionaryOrigin))
            [proxyMethod, valueMethod]
            (FcApp (FcVar proxyMethod) proxy)
        ]
    )

dsTypeRepresentation :: Maybe (Text, Text) -> TcType -> [FcExpr] -> DsM FcExpr
dsTypeRepresentation typeableOrigin ty arguments =
  case typeableTypeView ty of
    Nothing -> desugarBug ("cannot construct TypeRep for " <> show ty)
    Just (name, _) -> do
      charNil <- nilList charTy
      charCons <- consExpr charTy
      charConstructorOrigin <- lookupDataConOrigin "C#"
      argumentNil <- nilList typeRepTy
      argumentCons <- consExpr typeRepTy
      tyConOrigin <- typeableConstructorOrigin typeableOrigin "TyCon"
      typeRepOrigin <- typeableConstructorOrigin typeableOrigin "TypeRep"
      let tyConConstructor =
            (Var "TyCon" (Unique (-2102)) (TcFunTy stringTy tyConTy))
              { varResolvedName = Just tyConOrigin
              }
          typeRepConstructor =
            ( Var
                "TypeRep"
                (Unique (-2103))
                (TcFunTy tyConTy (TcFunTy (listType typeRepTy) typeRepTy))
            )
              { varResolvedName = Just typeRepOrigin
              }
          tyCon = FcApp (FcVar tyConConstructor) (T.foldr (applyConsChar charConstructorOrigin charCons) charNil name)
          argumentList = foldr (applyCons argumentCons) argumentNil arguments
      pure (FcApp (FcApp (FcVar typeRepConstructor) tyCon) argumentList)

typeableConstructorOrigin :: Maybe (Text, Text) -> Text -> DsM FcSymbolOrigin
typeableConstructorOrigin origin constructorName =
  case origin of
    Just (packageName, _) | constructorName == "Proxy" -> pure (FcTopLevelOrigin packageName "Data.Proxy" constructorName)
    Just (packageName, moduleName) -> pure (FcTopLevelOrigin packageName moduleName constructorName)
    Nothing -> lookupDataConOrigin constructorName

typeableTypeView :: TcType -> Maybe (Text, [TcType])
typeableTypeView ty =
  case ty of
    TcTyCon tyCon arguments -> Just (tyConName tyCon, arguments)
    TcFunTy argument result -> Just ("(->)", [argument, result])
    _ -> Nothing

typeableTyVar :: TyVarId
typeableTyVar = TyVarId "a" (Unique (-2104))

typeRepTy :: TcType
typeRepTy = TcTyCon (TyCon "TypeRep" 0) []

tyConTy :: TcType
tyConTy = TcTyCon (TyCon "TyCon" 0) []

stringTy :: TcType
stringTy = listType charTy

unitConstructor :: DsM FcExpr
unitConstructor = do
  ty <- lookupType "()"
  pure (FcVar (Var "()" (Unique (-13)) ty))

exprAnnotationType :: Expr -> Maybe TcType
exprAnnotationType expr =
  case expr of
    EAnn ann inner ->
      case fromAnnotation ann of
        Just tcAnn -> Just (tcAnnType tcAnn)
        Nothing -> exprAnnotationType inner
    EParen inner -> exprAnnotationType inner
    ETypeSig inner _ -> exprAnnotationType inner
    _ -> Nothing

nameTcAnnotation :: Name -> Maybe TcAnnotation
nameTcAnnotation =
  listToMaybe . mapMaybe fromAnnotation . nameAnns

patternEvidenceBinders :: Pattern -> DsM ([Var], [ClassDict])
patternEvidenceBinders pattern' = do
  let predicates =
        case patternTcAnnotation pattern' of
          Just annotation -> [predicate | EvGiven predicate <- tcAnnEvidenceTerms annotation]
          Nothing -> []
  binders <- mapM (freshVar "$dpattern" . predType) predicates
  pure (binders, zipWith patternDictionary predicates binders)
  where
    patternDictionary predicate binder =
      case predicate of
        ClassPred className arguments -> ClassDict className arguments binder
        EqPred {} -> ClassDict "<constraint>" [] binder

patternTcAnnotation :: Pattern -> Maybe TcAnnotation
patternTcAnnotation pattern' =
  case pattern' of
    PAnn ann inner ->
      case fromAnnotation ann of
        Just annotation -> Just annotation
        Nothing -> patternTcAnnotation inner
    PParen inner -> patternTcAnnotation inner
    PStrict inner -> patternTcAnnotation inner
    PIrrefutable inner -> patternTcAnnotation inner
    PAs _ inner -> patternTcAnnotation inner
    PTypeSig inner _ -> patternTcAnnotation inner
    _ -> Nothing

patternBinderTypesM :: Pattern -> TcType -> DsM [TcType]
patternBinderTypesM pat scrutTy =
  case pat of
    PInfix _lhs op _rhs
      | nameText op == ":" ->
          (\elemTy -> [elemTy, scrutTy]) <$> listElemTyM scrutTy
      | otherwise -> constructorFieldTypesM op 2
    PList (_ : _) ->
      (\elemTy -> [elemTy, scrutTy]) <$> listElemTyM scrutTy
    PCon _ _ [] -> pure []
    PCon name _ subPats -> do
      fallbackTys <- constructorFieldTypesM name (length subPats)
      zipWithM patternFieldTypeM subPats fallbackTys
    PTuple _ [] -> pure []
    PTuple _ subPats -> tupleFieldTypesM (length subPats) scrutTy
    PVar {} -> pure [scrutTy]
    PAnn _ inner -> patternBinderTypesM inner scrutTy
    PParen inner -> patternBinderTypesM inner scrutTy
    _
      | null (snd (dsPatternPure (PackageId "") pat)) -> pure []
      | otherwise -> missingPatternTypes
  where
    missingPatternTypes =
      desugarBug ("missing pattern binder type information while desugaring: " <> take 80 (show pat))

    patternFieldTypeM subPat fallbackTy =
      pure (fromMaybe fallbackTy (patternBinderAnnotationType subPat <|> patternAnnotationType subPat))

constructorFieldTypesM :: Name -> Int -> DsM [TcType]
constructorFieldTypesM name arity = do
  ty <- lookupTypeName name
  takeConstructorFields (nameToText name) arity (dropConstructorContext (dropForAlls ty))

takeConstructorFields :: Text -> Int -> TcType -> DsM [TcType]
takeConstructorFields _ 0 _ = pure []
takeConstructorFields name arity (TcFunTy arg rest) =
  (arg :) <$> takeConstructorFields name (arity - 1) rest
takeConstructorFields name arity ty =
  desugarBug ("missing field type information for constructor pattern " <> T.unpack name <> ": expected " <> show arity <> " more field(s) in " <> show ty)

dropForAlls :: TcType -> TcType
dropForAlls (TcForAllTy _ body) = dropForAlls body
dropForAlls ty = ty

dropConstructorContext :: TcType -> TcType
dropConstructorContext (TcQualTy _ body) = body
dropConstructorContext ty = ty

listElemTyM :: TcType -> DsM TcType
listElemTyM (TcTyCon (TyCon "[]" 1) [elemTy]) = pure elemTy
listElemTyM ty =
  desugarBug ("missing list element type information while desugaring: " <> show ty)

tupleFieldTypesM :: Int -> TcType -> DsM [TcType]
tupleFieldTypesM arity (TcTyCon (TyCon _ arity') fieldTys)
  | arity == arity',
    length fieldTys == arity =
      pure fieldTys
tupleFieldTypesM arity ty =
  desugarBug ("missing tuple field type information while desugaring: expected " <> show arity <> " field(s) in " <> show ty)

lambdaPatternTypeRequired :: Pattern -> DsM TcType
lambdaPatternTypeRequired pat =
  case patternBinderAnnotationType pat <|> patternAnnotationType pat of
    Just ty -> pure ty
    Nothing -> desugarBug ("missing type-checker annotation for lambda pattern: " <> take 80 (show pat))

patternBinderAnnotationType :: Pattern -> Maybe TcType
patternBinderAnnotationType pat =
  case pat of
    PVar name -> unqualifiedNameAnnotationType name
    PAnn _ inner -> patternBinderAnnotationType inner
    PParen inner -> patternBinderAnnotationType inner
    PStrict inner -> patternBinderAnnotationType inner
    PIrrefutable inner -> patternBinderAnnotationType inner
    PAs name _ -> unqualifiedNameAnnotationType name
    PTypeSig inner _ -> patternBinderAnnotationType inner
    _ -> Nothing

unqualifiedNameAnnotationType :: UnqualifiedName -> Maybe TcType
unqualifiedNameAnnotationType =
  fmap tcAnnType . listToMaybe . mapMaybe fromAnnotation . unqualifiedNameAnns

patternAnnotationType :: Pattern -> Maybe TcType
patternAnnotationType pat =
  case pat of
    PAnn ann inner ->
      case fromAnnotation ann of
        Just tcAnn -> Just (tcAnnType tcAnn)
        Nothing -> patternAnnotationType inner
    PParen inner -> patternAnnotationType inner
    PStrict inner -> patternAnnotationType inner
    PIrrefutable inner -> patternAnnotationType inner
    PAs _ inner -> patternAnnotationType inner
    PTypeSig inner _ -> patternAnnotationType inner
    _ -> Nothing

fcExprTypeM :: FcExpr -> DsM TcType
fcExprTypeM expr =
  case expr of
    FcVar var -> pure (varType var)
    FcLit lit ->
      case literalType lit of
        Just ty -> pure ty
        Nothing -> desugarBug ("literal has invalid runtime representation: " <> show lit)
    FcApp fun _arg -> do
      funTy <- fcExprTypeM fun
      case funTy of
        TcQualTy (_pred : preds) body -> pure (if null preds then body else TcQualTy preds body)
        TcFunTy _argTy resTy -> pure resTy
        _ -> desugarBug ("application to non-function type while desugaring: " <> show funTy)
    FcTyApp fun ty -> do
      funTy <- fcExprTypeM fun
      case funTy of
        TcForAllTy tv body -> pure (substType (Map.singleton tv ty) body)
        _ -> desugarBug ("type application to non-forall type while desugaring: " <> show funTy)
    FcLam var body -> TcFunTy (varType var) <$> fcExprTypeM body
    FcTyLam tv body -> TcForAllTy tv <$> fcExprTypeM body
    FcLet _bind body -> fcExprTypeM body
    FcCase _scrut _binder alts ->
      case alts of
        [] -> desugarBug "case expression has no alternatives while desugaring"
        FcAlt _ _ body : _ -> fcExprTypeM body
    FcCast inner _co -> fcExprTypeM inner
    FcCallForeign foreignCall _arguments ->
      pure (fcForeignCallResultType (fcForeignCallSignature foreignCall))

dictKey :: Text -> [TcType] -> Text
dictKey className args = className <> ":" <> T.intercalate "," (map typeKey args)

exactDictKey :: Text -> [TcType] -> Text
exactDictKey className args = className <> ":exact:" <> T.intercalate "," (map exactTypeKey args)

exactTypeKey :: TcType -> Text
exactTypeKey ty =
  case ty of
    TcTyVar tv -> tvName tv <> "#" <> T.pack (show (uniqueInt (tvUnique tv)))
    TcMetaTv (Unique unique) -> "?" <> T.pack (show unique)
    TcTyCon tc [] -> tyConName tc
    TcTyCon (TyCon "[]" _) [elementType] -> "[" <> exactTypeKey elementType <> "]"
    TcTyCon tc arguments -> tyConName tc <> T.concat (map (("_" <>) . exactTypeKey) arguments)
    TcAppTy function argument -> exactTypeKey function <> "_" <> exactTypeKey argument
    TcFunTy argument result -> exactTypeKey argument <> "->" <> exactTypeKey result
    TcForAllTy _ body -> exactTypeKey body
    TcQualTy _ body -> exactTypeKey body
    TcBuiltinTyCon name _ arguments -> name <> T.concat (map (("_" <>) . exactTypeKey) arguments)

uniqueInt :: Unique -> Int
uniqueInt (Unique unique) = unique

typeKey :: TcType -> Text
typeKey ty =
  case ty of
    TcTyVar tv -> tvName tv
    TcMetaTv (Unique u) -> "?" <> T.pack (show u)
    TcTyCon tc [] -> tyConName tc
    TcTyCon (TyCon "[]" _) [elemTy] -> "[" <> typeKey elemTy <> "]"
    TcTyCon tc args -> tyConName tc <> T.concat (map (("_" <>) . typeKey) args)
    TcAppTy f a -> typeKey f <> "_" <> typeKey a
    TcFunTy a b -> typeKey a <> "->" <> typeKey b
    TcForAllTy _ body -> typeKey body
    TcQualTy _ body -> typeKey body
    TcBuiltinTyCon name _ arguments -> name <> T.concat (map (("_" <>) . typeKey) arguments)

charTy :: TcType
charTy = TcTyCon (TyCon "Char" 0) []

charHashTy :: TcType
charHashTy = TcTyCon (TyCon "Char#" 0) []

-- | Convert a Name to Text.
nameToText :: Name -> Text
nameToText n = case nameQualifier n of
  Nothing -> nameText n
  Just q -> q <> "." <> nameText n

lookupLocalName :: Name -> DsM (Maybe Var)
lookupLocalName name = do
  currentModule <- gets dsModuleName
  case nameQualifier name of
    Nothing -> lookupLocal (nameText name)
    Just qualifier
      | qualifier == currentModule -> lookupLocal (nameText name)
      | otherwise -> lookupLocal (nameToText name)

lookupTypeName :: Name -> DsM TcType
lookupTypeName name = do
  maybeTy <- lookupTypeMaybeName name
  case maybeTy of
    Just ty -> pure ty
    Nothing -> desugarBug ("missing type information for name: " <> T.unpack (nameToText name))

lookupTypeMaybeName :: Name -> DsM (Maybe TcType)
lookupTypeMaybeName name = do
  st <- get
  pure (Map.lookup (nameToText name) (dsTypeEnv st) <|> Map.lookup (nameText name) (dsTypeEnv st))
