{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Constraint generation for declarations.
--
-- Processes top-level data declarations and value bindings from a module.
module Aihc.Tc.Generate.Decl
  ( tcModule,
    TcBindingResult (..),
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    BangType (..),
    DataConDecl (..),
    DataDecl (..),
    Decl (..),
    GadtBody (..),
    Match (..),
    Module (..),
    Name (..),
    NameType (..),
    Pattern (..),
    Rhs (..),
    SourceSpan (..),
    Type (..),
    UnqualifiedName (..),
    ValueDecl (..),
    binderHeadName,
    binderHeadParams,
    fromAnnotation,
    gadtBodyResultType,
    getDeclSourceSpan,
    peelDeclAnn,
    peelTypeHead,
    tyVarBinderName,
  )
import Aihc.Tc.Constraint
import Aihc.Tc.Generalize (generalize)
import Aihc.Tc.Generate.Expr (inferExpr)
import Aihc.Tc.Instantiate qualified
import Aihc.Tc.Monad
import Aihc.Tc.Solve (solveConstraints, solveWithImpls)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Monad (zipWithM)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)

-- | Merge concrete source spans embedded in a list of annotations.
sourceSpanFromAnns :: [Annotation] -> SourceSpan
sourceSpanFromAnns anns =
  case mapMaybe (fromAnnotation @SourceSpan) anns of
    [] -> NoSourceSpan
    s : _ -> s

-- | Result of type-checking a single binding.
data TcBindingResult = TcBindingResult
  { tbName :: !Text,
    tbType :: !TcType
  }
  deriving (Show)

-- | Type-check a module, returning the inferred types for each
-- top-level declaration: type constructors (with their kinds),
-- data constructors (with their types), and value bindings.
tcModule :: Module -> TcM [TcBindingResult]
tcModule m = do
  -- Phase 1: collect data declarations, register constructors,
  --          and report their types.
  dataResults <- concat <$> mapM registerDecl (moduleDecls m)
  -- Phase 2: collect type signatures and convert them to schemes.
  let rawSigs = collectRawSigs (moduleDecls m)
  schemes <- traverse sigToScheme rawSigs
  -- Phase 3: group and type-check value bindings using signatures.
  let grouped = groupValueDecls (moduleDecls m)
  valueResults <- concat <$> mapM (tcDeclGroup schemes) grouped
  pure (dataResults ++ valueResults)

-- | Collect type signatures from a list of declarations.
collectRawSigs :: [Decl] -> Map Text Type
collectRawSigs decls = Map.fromList $ concatMap extractSig decls
  where
    extractSig (DeclTypeSig names ty) =
      [(unqualifiedNameText n, ty) | n <- names]
    extractSig (DeclAnn _ inner) = extractSig inner
    extractSig _ = []

-- | Collect free type variable names from a surface type.
freeTypeVars :: Type -> [Text]
freeTypeVars = nub . go
  where
    go (TVar name) = [unqualifiedNameText name]
    go (TApp f a) = go f ++ go a
    go (TFun a b) = go a ++ go b
    go (TParen inner) = go inner
    go (TAnn _ inner) = go inner
    go (TContext _preds inner) = go inner
    go (TForall _ inner) = go inner
    go _ = []

-- | Convert a surface type to a TcType, using a map from variable names to
-- TyVarIds for any type variables in scope.
convertSurfaceType :: Map Text TyVarId -> Type -> TcType
convertSurfaceType tvMap ty = case peelTypeHead ty of
  TVar name ->
    let n = unqualifiedNameText name
     in case Map.lookup n tvMap of
          Just tvId -> TcTyVar tvId
          Nothing -> TcTyCon (TyCon n 0) []
  TCon name _ ->
    TcTyCon (TyCon (nameText name) 0) []
  TApp {} ->
    -- Collect the full application chain to get the arity right.
    let (headTy, args) = collectTApps ty
        convertedArgs = map (convertSurfaceType tvMap) args
        arity = length args
     in case peelTypeHead headTy of
          TCon name _ ->
            TcTyCon (TyCon (nameText name) arity) convertedArgs
          TVar name ->
            let n = unqualifiedNameText name
             in case Map.lookup n tvMap of
                  Just tvId -> foldl TcAppTy (TcTyVar tvId) convertedArgs
                  Nothing -> foldl TcAppTy (TcTyCon (TyCon n 0) []) convertedArgs
          _ -> foldl TcAppTy (convertSurfaceType tvMap headTy) convertedArgs
  TFun a b ->
    TcFunTy (convertSurfaceType tvMap a) (convertSurfaceType tvMap b)
  TTuple _ _ args ->
    let tys = map (convertSurfaceType tvMap) args
        n = length tys
        tc = TyCon ("(" <> mconcat (replicate (n - 1) ",") <> ")") n
     in TcTyCon tc tys
  _ -> TcMetaTv (Unique (-1))

-- | Collect the head and all arguments from a chain of type applications.
collectTApps :: Type -> (Type, [Type])
collectTApps ty = go ty []
  where
    go (TApp f a) acc = go (peelTypeHead f) (a : acc)
    go t acc = (t, acc)

-- | Convert a surface type signature to a TypeScheme.
-- Free type variables become universally quantified type variables.
sigToScheme :: Type -> TcM TypeScheme
sigToScheme ty = do
  let freeVars = freeTypeVars ty
  tvIds <- mapM freshSkolemTv freeVars
  let tvMap = Map.fromList (zip freeVars tvIds)
      tcTy = convertSurfaceType tvMap ty
  pure (ForAll tvIds [] tcTy)

-- | Instantiate a type scheme with fresh skolems for type-checking.
-- Unlike regular instantiation (which uses metas), this produces rigid
-- type variables that cannot be unified during constraint solving.
skolemize :: TypeScheme -> TcM TcType
skolemize (ForAll tvs _preds body) = do
  subst <- Map.fromList <$> mapM mkSubst tvs
  pure (Aihc.Tc.Instantiate.applySubst subst body)
  where
    mkSubst tv = do
      sk <- freshSkolemTv (tvName tv)
      pure (tvUnique tv, TcTyVar sk)

-- | Split a function type into argument types and result type.
splitFunTy :: TcType -> Int -> ([TcType], TcType)
splitFunTy ty 0 = ([], ty)
splitFunTy (TcFunTy a rest) n =
  let (args, res) = splitFunTy rest (n - 1)
   in (a : args, res)
splitFunTy ty _ = ([], ty)

-- | A group of declarations that should be typechecked together.
-- Multiple FunctionBind equations for the same name are merged.
data DeclGroup
  = SingleDecl Decl
  | MergedFunctionBind SourceSpan UnqualifiedName [Match]

-- | Group consecutive FunctionBind declarations with the same name.
groupValueDecls :: [Decl] -> [DeclGroup]
groupValueDecls [] = []
groupValueDecls (d : ds) = case extractFunctionBind d of
  Just (sp, name, matches) ->
    let (sameNameDecls, rest) = span (hasSameName name) ds
        allMatches = matches ++ concatMap (maybe [] (\(_, _, ms) -> ms) . extractFunctionBind) sameNameDecls
     in MergedFunctionBind sp name allMatches : groupValueDecls rest
  Nothing -> SingleDecl d : groupValueDecls ds

-- | Extract function bind info from a declaration.
extractFunctionBind :: Decl -> Maybe (SourceSpan, UnqualifiedName, [Match])
extractFunctionBind decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind name matches) ->
      let sp = getDeclSourceSpan decl
       in Just (sp, name, matches)
    _ -> Nothing

-- | Check if a declaration is a FunctionBind with the given name.
hasSameName :: UnqualifiedName -> Decl -> Bool
hasSameName name d = case extractFunctionBind d of
  Just (_, n, _) -> unqualifiedNameText n == unqualifiedNameText name
  Nothing -> False

-- | Type-check a declaration group.
tcDeclGroup :: Map Text TypeScheme -> DeclGroup -> TcM [TcBindingResult]
tcDeclGroup _ (SingleDecl d) = tcDecl d
tcDeclGroup sigs (MergedFunctionBind _sp binder matches) = do
  let name = unqualifiedNameText binder
      displayName = renderBinderName binder
  case Map.lookup name sigs of
    Just scheme -> do
      -- Use the declared type signature for checking.
      tcFunctionWithSig displayName name scheme matches
    Nothing -> do
      -- No signature: infer the type.
      tcFunctionInfer displayName name matches

-- | Type-check a function with a known type signature.
-- The signature's type variables are opened as rigid skolems so that
-- the body is checked against them. GADT patterns generate implication
-- constraints using the signature's skolems as given equalities.
tcFunctionWithSig :: Text -> Text -> TypeScheme -> [Match] -> TcM [TcBindingResult]
tcFunctionWithSig displayName name scheme matches = do
  -- Open the scheme with skolems (not metas) for checking.
  sigTy <- skolemize scheme
  let nArgs = case matches of
        (m : _) -> length (matchPats m)
        [] -> 0
      (argTys, resTy) = splitFunTy sigTy nArgs
  -- Check each equation against the signature types.
  results <- mapM (tcMatchEquation argTys resTy) matches
  let (ctsList, implsList) = unzip results
      allCts = concat ctsList
      allImpls = concat implsList
  _ <- solveWithImpls allCts allImpls
  -- Report the declared scheme as the binding's type.
  let declaredTy = schemeToType scheme
  zonkedTy <- zonkType declaredTy
  extendTermEnvPermanent name (TcIdBinder name scheme)
  pure [TcBindingResult displayName zonkedTy]

-- | Type-check a function without a type signature (infer).
tcFunctionInfer :: Text -> Text -> [Match] -> TcM [TcBindingResult]
tcFunctionInfer displayName name matches = do
  (ty, cts, impls) <- tcMatches matches
  _ <- solveWithImpls cts impls
  scheme <- generalize ty []
  let schemeTy = schemeToType scheme
  zonkedTy <- zonkType schemeTy
  extendTermEnvPermanent name (TcIdBinder name scheme)
  pure [TcBindingResult displayName zonkedTy]

-- | Register a declaration in the environment (data types, etc.).
-- Returns binding results for the declared names.
registerDecl :: Decl -> TcM [TcBindingResult]
registerDecl (DeclData dd) = registerDataDecl dd
registerDecl (DeclAnn _ inner) = registerDecl inner
registerDecl _ = pure []

-- | Register a data declaration's type constructor and data constructors.
--
-- For @data Bool = True | False@, this produces:
--   - @Bool :: *@
--   - @True :: Bool@
--   - @False :: Bool@
registerDataDecl :: DataDecl -> TcM [TcBindingResult]
registerDataDecl dd = do
  let tyName = unqualifiedNameText (binderHeadName (dataDeclHead dd))
      params = binderHeadParams (dataDeclHead dd)
      arity = length params
      starKind = TcTyCon (TyCon "*" 0) []
      tyConResult = TcBindingResult tyName starKind
  -- Create TyVarIds for the type parameters.
  paramVarIds <- mapM (freshSkolemTv . tyVarBinderName) params
  let paramMap = Map.fromList (zip (map tyVarBinderName params) paramVarIds)
      tc = TyCon tyName arity
  conResults <- mapM (registerDataCon tc paramMap paramVarIds) (dataDeclConstructors dd)
  pure (tyConResult : conResults)

-- | Register a single data constructor as a polymorphic binding.
-- Returns the binding result for the constructor.
registerDataCon :: TyCon -> Map Text TyVarId -> [TyVarId] -> DataConDecl -> TcM TcBindingResult
registerDataCon tc paramMap paramVarIds con = case con of
  DataConAnn _ inner -> registerDataCon tc paramMap paramVarIds inner
  PrefixCon _docs _ctx conName _args ->
    let name = unqualifiedNameText conName
        resTy = TcTyCon tc (map TcTyVar paramVarIds)
        scheme = ForAll paramVarIds [] resTy
     in do
          extendTermEnvPermanent name (TcIdBinder name scheme)
          zonkedTy <- zonkType resTy
          pure (TcBindingResult name zonkedTy)
  InfixCon _docs _ctx _lhs conName _rhs ->
    let name = unqualifiedNameText conName
        resTy = TcTyCon tc (map TcTyVar paramVarIds)
        scheme = ForAll paramVarIds [] resTy
     in do
          extendTermEnvPermanent name (TcIdBinder name scheme)
          zonkedTy <- zonkType resTy
          pure (TcBindingResult name zonkedTy)
  RecordCon _docs _ctx conName _fields ->
    let name = unqualifiedNameText conName
        resTy = TcTyCon tc (map TcTyVar paramVarIds)
        scheme = ForAll paramVarIds [] resTy
     in do
          extendTermEnvPermanent name (TcIdBinder name scheme)
          zonkedTy <- zonkType resTy
          pure (TcBindingResult name zonkedTy)
  GadtCon _forallBinders _ctx names body ->
    -- Parse the GADT constructor's declared result type.
    let resultSurfTy = gadtBodyResultType body
        argSurfTys = gadtBodyArgTypes body
        gadtResTy = convertSurfaceType paramMap resultSurfTy
        gadtArgTys = map (convertSurfaceType paramMap) argSurfTys
        -- The constructor's full type: arg1 -> arg2 -> ... -> resultType
        conTy = foldr TcFunTy gadtResTy gadtArgTys
        -- GADT constructors are universally quantified over no extra vars
        -- (the data type's params are handled via given equalities on match).
        scheme = ForAll [] [] conTy
     in do
          mapM_
            ( \n -> do
                let nm = unqualifiedNameText n
                extendTermEnvPermanent nm (TcIdBinder nm scheme)
                markGadtCon nm
            )
            names
          case names of
            (n : _) -> do
              zonkedTy <- zonkType conTy
              pure (TcBindingResult (unqualifiedNameText n) zonkedTy)
            [] -> pure (TcBindingResult "<gadt>" gadtResTy)
  TupleCon {} ->
    pure (TcBindingResult "<tuple-constructor>" (TcTyCon tc (map TcTyVar paramVarIds)))
  UnboxedSumCon {} ->
    pure (TcBindingResult "<unboxed-sum-constructor>" (TcTyCon tc (map TcTyVar paramVarIds)))
  ListCon {} ->
    pure (TcBindingResult "<list-constructor>" (TcTyCon tc (map TcTyVar paramVarIds)))

-- | Extract argument types from a GadtBody.
gadtBodyArgTypes :: GadtBody -> [Type]
gadtBodyArgTypes (GadtPrefixBody bangTys _) = map bangType bangTys
gadtBodyArgTypes _ = []

-- | Type-check a declaration, returning binding results for value bindings.
tcDecl :: Decl -> TcM [TcBindingResult]
tcDecl (DeclValue vd) = tcValueDecl vd
tcDecl (DeclAnn _ inner) = tcDecl inner
tcDecl _ = pure []

-- | Type-check a value declaration.
tcValueDecl :: ValueDecl -> TcM [TcBindingResult]
tcValueDecl (FunctionBind binder matches) = do
  let name = unqualifiedNameText binder
      displayName = renderBinderName binder
  tcFunctionInfer displayName name matches
tcValueDecl (PatternBind _pat rhs) = do
  ty <- tcRhs rhs
  zonkedTy <- zonkType ty
  pure [TcBindingResult "<pattern>" zonkedTy]

-- | Convert a type scheme to a displayable type.
schemeToType :: TypeScheme -> TcType
schemeToType (ForAll [] [] ty) = ty
schemeToType (ForAll tvs [] ty) = foldr TcForAllTy ty tvs
schemeToType (ForAll [] preds ty) = TcQualTy preds ty
schemeToType (ForAll tvs preds ty) = foldr TcForAllTy (TcQualTy preds ty) tvs

-- | Type-check a list of matches (equations for a function binding).
--
-- All equations must have the same number of patterns and produce
-- a consistent function type. We infer the type from each equation
-- and unify them.
tcMatches :: [Match] -> TcM (TcType, [Ct], [Implication])
tcMatches [] = do
  ty <- freshMetaTv
  pure (ty, [], [])
tcMatches matches@(m0 : _) = do
  let nArgs = length (matchPats m0)
  if nArgs == 0
    then do
      -- No patterns: just infer the RHS of the first match.
      (ty0, cts0) <- inferRhsExpr (matchRhs m0)
      restCts <- concatMapM (unifyMatchRhs ty0) (drop 1 matches)
      pure (ty0, cts0 ++ restCts, [])
    else do
      -- Create fresh meta-variables for the argument types and result type.
      argTys <- mapM (const freshMetaTv) [1 .. nArgs]
      resTy <- freshMetaTv
      -- Process each equation.
      results <- mapM (tcMatchEquation argTys resTy) matches
      let (ctsList, implsList) = unzip results
          allCts = concat ctsList
          allImpls = concat implsList
          funTy = foldr TcFunTy resTy argTys
      pure (funTy, allCts, allImpls)

-- | Type-check a single match equation against expected arg/result types.
-- Returns flat wanted constraints and implication constraints.
tcMatchEquation :: [TcType] -> TcType -> Match -> TcM ([Ct], [Implication])
tcMatchEquation argTys resTy match = do
  let pats = matchPats match
      sp = sourceSpanFromAnns (matchAnns match)
  -- Bind pattern variables with their corresponding arg types.
  let bindings = concatMap extractPatternBindings (zip pats argTys)
  -- Collect pattern constraints, separating GADT givens from regular wanteds.
  (wantedPatCts, givenCts) <-
    unzipPair . concat <$> zipWithM (inferPatConstraints sp) pats argTys
  -- Infer the RHS under the extended environment.
  (rhsTy, rhsCts) <- withPatBindings bindings (inferRhsExpr (matchRhs match))
  -- RHS type must match the expected result type.
  ev <- freshEvVar
  let resCt = mkWantedCt (EqPred rhsTy resTy) ev (AppOrigin sp) sp
  let bodyWanteds = wantedPatCts ++ rhsCts ++ [resCt]
  if null givenCts
    then -- No GADT givens: emit everything as flat wanteds.
      pure (bodyWanteds, [])
    else do
      -- GADT givens: wrap body wanteds in an implication.
      level <- getTcLevel
      let impl =
            Implication
              { implSkols = [],
                implGivenEvs = map ctEvVar givenCts,
                implGivenCts = givenCts,
                implWantedCts = bodyWanteds,
                implTcLevel = level,
                implInfo = AppOrigin sp
              }
      pure ([], [impl])

-- | Unzip a list of pairs of constraint lists.
unzipPair :: [([Ct], [Ct])] -> ([Ct], [Ct])
unzipPair pairs =
  let (as, bs) = unzip pairs
   in (concat as, concat bs)

-- | Infer constraints from a single pattern, separated into regular wanted
-- constraints (for non-GADT constructors) and given constraints (for GADT
-- constructors that refine the scrutinee's type parameters).
--
-- Returns @(wantedCts, givenCts)@.
inferPatConstraints :: SourceSpan -> Pattern -> TcType -> TcM [([Ct], [Ct])]
inferPatConstraints sp pat scrutTy = case pat of
  PCon name _typeArgs _subPats -> do
    let conName = patNameToText name
    mBinder <- lookupTerm conName
    case mBinder of
      Just (TcIdBinder _ scheme) -> do
        (conTy, _preds) <- instantiateSch scheme
        let conResTy = resultType conTy
        ev <- freshEvVar
        gadtCon <- isGadtCon conName
        if gadtCon
          then do
            -- GADT constructor: emit the scrutinee equality as a GIVEN.
            let givenCt =
                  Ct
                    { ctPred = EqPred scrutTy conResTy,
                      ctFlavor = Given,
                      ctEvVar = ev,
                      ctOrigin = AppOrigin sp,
                      ctLoc = sp
                    }
            pure [([], [givenCt])]
          else do
            -- Regular constructor: emit as a WANTED constraint.
            let wantedCt = mkWantedCt (EqPred scrutTy conResTy) ev (AppOrigin sp) sp
            pure [([wantedCt], [])]
      _ -> pure [([], [])]
  PAnn _ann inner -> inferPatConstraints sp inner scrutTy
  PParen inner -> inferPatConstraints sp inner scrutTy
  PStrict inner -> inferPatConstraints sp inner scrutTy
  PIrrefutable inner -> inferPatConstraints sp inner scrutTy
  _ -> pure [([], [])]

-- | Unify an additional match equation's RHS with the expected type.
unifyMatchRhs :: TcType -> Match -> TcM [Ct]
unifyMatchRhs expectedTy match = do
  (rhsTy, rhsCts) <- inferRhsExpr (matchRhs match)
  ev <- freshEvVar
  let sp = sourceSpanFromAnns (matchAnns match)
  let eqCt = mkWantedCt (EqPred rhsTy expectedTy) ev (AppOrigin sp) sp
  pure (rhsCts ++ [eqCt])

-- | Convert a Name to Text for lookup.
patNameToText :: Name -> Text
patNameToText n = case nameQualifier n of
  Nothing -> nameText n
  Just q -> q <> "." <> nameText n

-- | Extract the result type from a (possibly nested) function type.
resultType :: TcType -> TcType
resultType (TcFunTy _ res) = resultType res
resultType ty = ty

-- | Instantiate a type scheme (delegating to the Instantiate module).
instantiateSch :: TypeScheme -> TcM (TcType, [Pred])
instantiateSch = Aihc.Tc.Instantiate.instantiate

-- | Extract variable bindings from a pattern paired with its expected type.
extractPatternBindings :: (Pattern, TcType) -> [(Text, TcType)]
extractPatternBindings (pat, ty) = case pat of
  PVar uname -> [(unqualifiedNameText uname, ty)]
  PAnn _ann inner -> extractPatternBindings (inner, ty)
  PParen inner -> extractPatternBindings (inner, ty)
  PWildcard {} -> []
  PLit {} -> []
  PNegLit {} -> []
  PAs name inner -> (name, ty) : extractPatternBindings (inner, ty)
  PStrict inner -> extractPatternBindings (inner, ty)
  PIrrefutable inner -> extractPatternBindings (inner, ty)
  PCon _name _typeArgs subPats ->
    concatMap (\p -> extractPatternBindings (p, ty)) subPats
  PInfix lhs _name rhs ->
    extractPatternBindings (lhs, ty) ++ extractPatternBindings (rhs, ty)
  _ -> []

-- | Run a computation with pattern bindings in scope.
withPatBindings :: [(Text, TcType)] -> TcM a -> TcM a
withPatBindings [] m = m
withPatBindings ((name, ty) : rest) m =
  extendTermEnv name (TcMonoIdBinder name ty) (withPatBindings rest m)

-- | Infer the type of a right-hand side expression.
inferRhsExpr :: Rhs -> TcM (TcType, [Ct])
inferRhsExpr (UnguardedRhs _sp expr _decls) = inferExpr expr
inferRhsExpr (GuardedRhss _sp _guards _decls) = do
  ty <- freshMetaTv
  pure (ty, [])

-- | Type-check a right-hand side (solving constraints immediately).
tcRhs :: Rhs -> TcM TcType
tcRhs (UnguardedRhs _sp expr _decls) = do
  (ty, cts) <- inferExpr expr
  _ <- solveConstraints cts
  pure ty
tcRhs (GuardedRhss _sp _guards _decls) =
  -- Guarded RHS not handled in MVP.
  freshMetaTv

-- | Render an unqualified name for display.
-- Operators (NameVarSym, NameConSym) are wrapped in parentheses.
renderBinderName :: UnqualifiedName -> Text
renderBinderName uname =
  case unqualifiedNameType uname of
    NameVarSym -> "(" <> unqualifiedNameText uname <> ")"
    NameConSym -> "(" <> unqualifiedNameText uname <> ")"
    _ -> unqualifiedNameText uname

-- | Strict 'concatMap' in a monad.
concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs
