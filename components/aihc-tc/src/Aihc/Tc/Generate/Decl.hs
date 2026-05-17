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
    CallConv (..),
    CaseAlt (..),
    DataConDecl (..),
    DataDecl (..),
    Decl (..),
    Expr (..),
    FieldDecl (..),
    ForeignDecl (..),
    ForeignDirection (..),
    GadtBody (..),
    Match (..),
    MatchHeadForm (..),
    Module (..),
    Name (..),
    NameType (..),
    Pattern (..),
    Rhs (..),
    SourceSpan (..),
    TupleFlavor (..),
    Type (..),
    TypeBuiltinCon (..),
    UnqualifiedName (..),
    ValueDecl (..),
    binderHeadName,
    binderHeadParams,
    forallTelescopeBinders,
    fromAnnotation,
    gadtBodyResultType,
    peelDeclAnn,
    peelTypeHead,
    tyVarBinderKind,
    tyVarBinderName,
  )
import Aihc.Tc.Constraint
import Aihc.Tc.Generalize (generalizeIgnoring)
import Aihc.Tc.Generate.Bind (inferRhsWithLocals)
import Aihc.Tc.Generate.Expr (inferExpr)
import Aihc.Tc.Instantiate qualified
import Aihc.Tc.Monad
import Aihc.Tc.Solve (solveConstraints, solveWithImpls)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Monad (zipWithM)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (nub, (\\))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.String (fromString)
import Data.Text (Text)

-- | Merge concrete source spans embedded in a list of annotations.
sourceSpanFromAnns :: [Annotation] -> SourceSpan
sourceSpanFromAnns anns =
  case mapMaybe (fromAnnotation @SourceSpan) anns of
    [] -> NoSourceSpan
    s : _ -> s

peelDeclSpan :: SourceSpan -> Decl -> SourceSpan
peelDeclSpan ambient (DeclAnn ann inner) =
  peelDeclSpan (fromMaybe ambient (fromAnnotation @SourceSpan ann)) inner
peelDeclSpan ambient _ = ambient

-- | Result of type-checking a single binding.
data TcBindingResult = TcBindingResult
  { -- | Canonical binder identity. Symbolic binders are stored without
    -- prefix-position parentheses, e.g. @++@ rather than @(++)@.
    tbName :: !Text,
    -- | Human-facing rendering for diagnostics and golden output.
    tbDisplayName :: !Text,
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
  let grouped = sortDeclGroups (groupValueDecls (moduleDecls m))
  valueResults <- concat <$> mapM (tcDeclGroup schemes) grouped
  pure (dataResults ++ valueResults)

-- | Collect type signatures from a list of declarations.
collectRawSigs :: [Decl] -> Map Text Type
collectRawSigs decls = Map.fromList $ concatMap extractSig decls
  where
    extractSig (DeclTypeSig names ty) =
      [(unqualifiedNameText n, ty) | n <- names]
    extractSig (DeclForeign foreignDecl)
      | isForeignPrimImport foreignDecl =
          [(unqualifiedNameText (foreignName foreignDecl), foreignType foreignDecl)]
    extractSig (DeclAnn _ inner) = extractSig inner
    extractSig _ = []

-- | Collect free type variable names from a surface type.
freeTypeVars :: Type -> [Text]
freeTypeVars = nub . go
  where
    go (TVar name) = [unqualifiedNameText name]
    go (TApp f a) = go f ++ go a
    go (TTypeApp f a) = go f ++ go a
    go (TInfix lhs _ _ rhs) = go lhs ++ go rhs
    go (TFun _ a b) = go a ++ go b
    go (TTuple _ _ args) = concatMap go args
    go (TUnboxedSum args) = concatMap go args
    go (TList _ args) = concatMap go args
    go (TParen inner) = go inner
    go (TAnn _ inner) = go inner
    go (TKindSig inner kindTy) = go inner ++ go kindTy
    go (TImplicitParam _ inner) = go inner
    go (TContext preds inner) = concatMap go preds ++ go inner
    go (TForall telescope inner) =
      (concatMap binderKindVars (forallTelescopeBinders telescope) ++ go inner)
        \\ map tyVarBinderName (forallTelescopeBinders telescope)
    go _ = []
    binderKindVars binder = maybe [] go (tyVarBinderKind binder)

-- | Convert a surface type to a TcType, using a map from variable names to
-- TyVarIds for any type variables in scope.
convertSurfaceType :: Map Text TyVarId -> Type -> TcType
convertSurfaceType tvMap ty = case peeledTy of
  TVar name ->
    let n = unqualifiedNameText name
     in case Map.lookup n tvMap of
          Just tvId -> TcTyVar tvId
          Nothing -> TcTyCon (TyCon n 0) []
  TCon name _ ->
    namedTypeCon (nameText name)
  TBuiltinCon TBuiltinList ->
    TcTyCon (TyCon "[]" 1) []
  TBuiltinCon TBuiltinCons ->
    TcTyCon (TyCon ":" 2) []
  TBuiltinCon (TBuiltinTuple arity) ->
    TcTyCon (TyCon ("(" <> mconcat (replicate (arity - 1) ",") <> ")") arity) []
  TBuiltinCon TBuiltinArrow ->
    TcTyCon (TyCon "(->)" 2) []
  TApp {} ->
    -- Collect the full application chain to get the arity right.
    let (headTy, args) = collectTApps peeledTy
        convertedArgs = map (convertSurfaceType tvMap) args
        arity = length args
     in case peelTypeHead headTy of
          TCon name _ ->
            TcTyCon (TyCon (canonicalTypeConName arity (nameText name)) arity) convertedArgs
          TVar name ->
            let n = unqualifiedNameText name
             in case Map.lookup n tvMap of
                  Just tvId -> foldl TcAppTy (TcTyVar tvId) convertedArgs
                  Nothing -> foldl TcAppTy (TcTyCon (TyCon n 0) []) convertedArgs
          _ -> foldl TcAppTy (convertSurfaceType tvMap headTy) convertedArgs
  TFun _ a b ->
    TcFunTy (convertSurfaceType tvMap a) (convertSurfaceType tvMap b)
  TTuple _ _ args ->
    let tys = map (convertSurfaceType tvMap) args
        n = length tys
        tc = TyCon ("(" <> mconcat (replicate (n - 1) ",") <> ")") n
     in TcTyCon tc tys
  TList _ args ->
    case args of
      [arg] ->
        listType (convertSurfaceType tvMap arg)
      _ ->
        TcMetaTv (Unique (-1))
  _ -> TcMetaTv (Unique (-1))
  where
    peeledTy = peelTypeHead ty

-- | Collect the head and all arguments from a chain of type applications.
collectTApps :: Type -> (Type, [Type])
collectTApps ty = go ty []
  where
    go (TApp f a) acc = go (peelTypeHead f) (a : acc)
    go t acc = (t, acc)

namedTypeCon :: Text -> TcType
namedTypeCon "String" = listType (TcTyCon (TyCon "Char" 0) [])
namedTypeCon "List" = TcTyCon (TyCon "[]" 1) []
namedTypeCon name = TcTyCon (TyCon name 0) []

canonicalTypeConName :: Int -> Text -> Text
canonicalTypeConName 1 "List" = "[]"
canonicalTypeConName _ name = name

listType :: TcType -> TcType
listType ty = TcTyCon (TyCon "[]" 1) [ty]

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
      let sp = peelDeclSpan NoSourceSpan decl
       in Just (sp, name, matches)
    _ -> Nothing

-- | Check if a declaration is a FunctionBind with the given name.
hasSameName :: UnqualifiedName -> Decl -> Bool
hasSameName name d = case extractFunctionBind d of
  Just (_, n, _) -> unqualifiedNameText n == unqualifiedNameText name
  Nothing -> False

-- | Sort top-level groups so acyclic forward references are checked after
-- their dependencies have been generalized into the global environment.
sortDeclGroups :: [DeclGroup] -> [DeclGroup]
sortDeclGroups groups =
  concatMap flattenScc (stronglyConnComp nodes)
  where
    allBinders = Set.fromList (concatMap declGroupBinders groups)
    nodes =
      [ (group, groupKey ix group, Set.toList (Set.intersection allBinders (Set.fromList (freeVarsGroup group))))
      | (ix, group) <- zip [(0 :: Int) ..] groups
      ]
    flattenScc (AcyclicSCC group) = [group]
    flattenScc (CyclicSCC cyclicGroups) = cyclicGroups

groupKey :: Int -> DeclGroup -> Text
groupKey ix group =
  case declGroupBinders group of
    name : _ -> name
    [] -> "<decl-" <> fromString (show ix) <> ">"

declGroupBinders :: DeclGroup -> [Text]
declGroupBinders group =
  case group of
    MergedFunctionBind _sp binder _matches -> [unqualifiedNameText binder]
    SingleDecl decl ->
      case peelDeclAnn decl of
        DeclValue (FunctionBind binder _) -> [unqualifiedNameText binder]
        DeclValue (PatternBind _ pat _) -> maybe [] ((: []) . snd) (patternBinderName pat)
        _ -> []

freeVarsGroup :: DeclGroup -> [Text]
freeVarsGroup group =
  case group of
    MergedFunctionBind _sp binder matches ->
      concatMap freeVarsMatch matches \\ [unqualifiedNameText binder]
    SingleDecl decl -> freeVarsDecl decl

freeVarsDecl :: Decl -> [Text]
freeVarsDecl decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind binder matches) ->
      concatMap freeVarsMatch matches \\ [unqualifiedNameText binder]
    DeclValue (PatternBind _ pat rhs) ->
      freeVarsRhs rhs \\ patternBinders pat
    _ -> []

freeVarsMatch :: Match -> [Text]
freeVarsMatch match =
  freeVarsRhs (matchRhs match) \\ concatMap patternBinders (matchPats match)

freeVarsRhs :: Rhs Expr -> [Text]
freeVarsRhs rhs =
  case rhs of
    UnguardedRhs _ expr maybeDecls ->
      freeVarsExpr expr ++ maybe [] (concatMap freeVarsDecl) maybeDecls
    GuardedRhss _ _ maybeDecls ->
      maybe [] (concatMap freeVarsDecl) maybeDecls

freeVarsExpr :: Expr -> [Text]
freeVarsExpr expr =
  case expr of
    EVar name -> [patNameToText name]
    EAnn _ inner -> freeVarsExpr inner
    EIf cond trueBranch falseBranch ->
      freeVarsExpr cond ++ freeVarsExpr trueBranch ++ freeVarsExpr falseBranch
    ELambdaPats pats body ->
      freeVarsExpr body \\ concatMap patternBinders pats
    EInfix lhs op rhs ->
      patNameToText op : freeVarsExpr lhs ++ freeVarsExpr rhs
    ENegate inner -> freeVarsExpr inner
    ESectionL inner op -> patNameToText op : freeVarsExpr inner
    ESectionR op inner -> patNameToText op : freeVarsExpr inner
    ELetDecls decls body ->
      let localBinders = concatMap declBinders decls
       in (concatMap freeVarsDecl decls ++ freeVarsExpr body) \\ localBinders
    ECase scrut alts ->
      freeVarsExpr scrut ++ concatMap freeVarsAlt alts
    ETypeSig inner _ -> freeVarsExpr inner
    EParen inner -> freeVarsExpr inner
    EList items -> concatMap freeVarsExpr items
    ETuple _ items -> concatMap (maybe [] freeVarsExpr) items
    EApp fun arg -> freeVarsExpr fun ++ freeVarsExpr arg
    _ -> []

freeVarsAlt :: CaseAlt Expr -> [Text]
freeVarsAlt (CaseAlt _ pat rhs) =
  freeVarsRhs rhs \\ patternBinders pat

declBinders :: Decl -> [Text]
declBinders decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind binder _) -> [unqualifiedNameText binder]
    DeclValue (PatternBind _ pat _) -> patternBinders pat
    DeclTypeSig names _ -> map unqualifiedNameText names
    _ -> []

patternBinders :: Pattern -> [Text]
patternBinders pat =
  case pat of
    PVar name -> [unqualifiedNameText name]
    PAnn _ inner -> patternBinders inner
    PParen inner -> patternBinders inner
    PAs name inner -> unqualifiedNameText name : patternBinders inner
    PStrict inner -> patternBinders inner
    PIrrefutable inner -> patternBinders inner
    PCon _ _ pats -> concatMap patternBinders pats
    PInfix lhs _ rhs -> patternBinders lhs ++ patternBinders rhs
    _ -> []

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
  extendTermEnvPermanent name (TcIdBinder name scheme Closed)
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
  pure [TcBindingResult name displayName zonkedTy]

-- | Type-check a function without a type signature (infer).
tcFunctionInfer :: Text -> Text -> [Match] -> TcM [TcBindingResult]
tcFunctionInfer displayName name matches = do
  placeholderTy <- freshMetaTv
  extendTermEnvPermanent name (TcMonoIdBinder name placeholderTy)
  (ty, cts, impls) <- tcMatches matches
  _ <- solveWithImpls cts impls
  scheme <- generalizeIgnoring [name] ty []
  let schemeTy = schemeToType scheme
  zonkedTy <- zonkType schemeTy
  extendTermEnvPermanent name (TcIdBinder name scheme Closed)
  pure [TcBindingResult name displayName zonkedTy]

-- | Register a declaration in the environment (data types, etc.).
-- Returns binding results for the declared names.
registerDecl :: Decl -> TcM [TcBindingResult]
registerDecl (DeclData dd) = registerDataDecl dd
registerDecl (DeclForeign foreignDecl)
  | isForeignPrimImport foreignDecl =
      registerForeignPrimImport foreignDecl
registerDecl (DeclAnn _ inner) = registerDecl inner
registerDecl _ = pure []

isForeignPrimImport :: ForeignDecl -> Bool
isForeignPrimImport foreignDecl =
  foreignDirection foreignDecl == ForeignImport
    && foreignCallConv foreignDecl == CPrim

registerForeignPrimImport :: ForeignDecl -> TcM [TcBindingResult]
registerForeignPrimImport foreignDecl = do
  scheme <- sigToScheme (foreignType foreignDecl)
  let name = unqualifiedNameText (foreignName foreignDecl)
      displayName = renderBinderName (foreignName foreignDecl)
      declaredTy = schemeToType scheme
  extendTermEnvPermanent name (TcIdBinder name scheme Closed)
  zonkedTy <- zonkType declaredTy
  pure [TcBindingResult name displayName zonkedTy]

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
      tyConResult = TcBindingResult tyName tyName starKind
  -- Create TyVarIds for the type parameters.
  paramVarIds <- mapM (freshSkolemTv . tyVarBinderName) params
  let paramMap = Map.fromList (zip (map tyVarBinderName params) paramVarIds)
      tc = dataDeclTyCon tyName arity
  conResults <- mapM (registerDataCon tc paramMap paramVarIds) (dataDeclConstructors dd)
  pure (tyConResult : conResults)

dataDeclTyCon :: Text -> Int -> TyCon
dataDeclTyCon "List" 1 = TyCon "[]" 1
dataDeclTyCon name arity = TyCon name arity

-- | Register a single data constructor as a polymorphic binding.
-- Returns the binding result for the constructor.
registerDataCon :: TyCon -> Map Text TyVarId -> [TyVarId] -> DataConDecl -> TcM TcBindingResult
registerDataCon tc paramMap paramVarIds con = case con of
  DataConAnn _ inner -> registerDataCon tc paramMap paramVarIds inner
  PrefixCon _docs _ctx conName args ->
    registerNamedDataCon (unqualifiedNameText conName) (map (convertSurfaceType paramMap . bangType) args)
  InfixCon _docs _ctx lhs conName rhs ->
    registerNamedDataCon (unqualifiedNameText conName) (map (convertSurfaceType paramMap . bangType) [lhs, rhs])
  RecordCon _docs _ctx conName fields ->
    registerNamedDataCon (unqualifiedNameText conName) (map (convertSurfaceType paramMap . bangType . fieldType) fields)
  TupleCon _docs _ctx flavor fields ->
    registerNamedDataCon (tupleConText flavor (length fields)) (map (convertSurfaceType paramMap . bangType) fields)
  UnboxedSumCon _docs _ctx pos arity field ->
    registerNamedDataCon (unboxedSumConText pos arity) [convertSurfaceType paramMap (bangType field)]
  ListCon {} ->
    registerNamedDataCon "[]" []
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
        gadtScheme = ForAll [] [] conTy
     in do
          mapM_
            ( \n -> do
                let nm = unqualifiedNameText n
                extendTermEnvPermanent nm (TcIdBinder nm gadtScheme Closed)
                markGadtCon nm
            )
            names
          case names of
            (n : _) -> do
              zonkedTy <- zonkType conTy
              let name = unqualifiedNameText n
               in pure (TcBindingResult name name zonkedTy)
            [] -> pure (TcBindingResult "<gadt>" "<gadt>" gadtResTy)
  where
    resTy = TcTyCon tc (map TcTyVar paramVarIds)
    conScheme argTys = ForAll paramVarIds [] (foldr TcFunTy resTy argTys)

    registerNamedDataCon name argTys = do
      let conTy = foldr TcFunTy resTy argTys
          scheme = conScheme argTys
      extendTermEnvPermanent name (TcIdBinder name scheme Closed)
      zonkedTy <- zonkType conTy
      pure (TcBindingResult name name zonkedTy)

tupleConText :: TupleFlavor -> Int -> Text
tupleConText flavor arity =
  case flavor of
    Boxed -> "(" <> commas arity <> ")"
    Unboxed -> "(#" <> commas arity <> "#)"

unboxedSumConText :: Int -> Int -> Text
unboxedSumConText pos arity = "(#" <> bars (pos - 1) <> "_" <> bars (arity - pos) <> "#)"

commas :: Int -> Text
commas n
  | n <= 1 = ""
  | otherwise = mconcat (replicate (n - 1) ",")

bars :: Int -> Text
bars n
  | n <= 0 = ""
  | otherwise = mconcat (replicate n "|")

-- | Extract argument types from a GadtBody.
gadtBodyArgTypes :: GadtBody -> [Type]
gadtBodyArgTypes (GadtPrefixBody argsWithKinds _) = map (bangType . fst) argsWithKinds
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
tcValueDecl (PatternBind _ pat rhs) = case patternBinderName pat of
  -- Bare variable pattern (e.g. @x = 5@, @(.>.) = (++)@): type-check as a
  -- zero-argument function so that the binding gets generalized and registered
  -- in the environment.
  Just (displayName, name) -> do
    let zeroArgMatch =
          Match
            { matchAnns = [],
              matchHeadForm = MatchHeadPrefix,
              matchPats = [],
              matchRhs = rhs
            }
    tcFunctionInfer displayName name [zeroArgMatch]
  -- Non-trivial pattern binding: infer the RHS type without generalization.
  Nothing -> do
    ty <- tcRhs rhs
    zonkedTy <- zonkType ty
    pure [TcBindingResult "<pattern>" "<pattern>" zonkedTy]

-- | Extract the binder name from a pattern binding's LHS, if it is a bare
-- variable pattern.  Returns @(displayName, envName)@ for simple variable
-- patterns (possibly wrapped in parens or annotations), 'Nothing' for
-- non-trivial patterns like tuples or constructors.
patternBinderName :: Pattern -> Maybe (Text, Text)
patternBinderName (PVar n) = Just (renderBinderName n, unqualifiedNameText n)
patternBinderName (PParen inner) = patternBinderName inner
patternBinderName (PAnn _ inner) = patternBinderName inner
patternBinderName _ = Nothing

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
      Just (TcIdBinder _ scheme _) -> do
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
  PAs name inner -> (unqualifiedNameText name, ty) : extractPatternBindings (inner, ty)
  PStrict inner -> extractPatternBindings (inner, ty)
  PIrrefutable inner -> extractPatternBindings (inner, ty)
  PCon _name _typeArgs subPats ->
    concatMap (\p -> extractPatternBindings (p, ty)) subPats
  PInfix lhs op rhs
    | nameText op == ":" ->
        let elemTy = listElementType ty
         in extractPatternBindings (lhs, elemTy) ++ extractPatternBindings (rhs, ty)
    | otherwise ->
        extractPatternBindings (lhs, ty) ++ extractPatternBindings (rhs, ty)
  _ -> []

listElementType :: TcType -> TcType
listElementType ty =
  case ty of
    TcTyCon (TyCon "[]" 1) [elemTy] -> elemTy
    _ -> ty

-- | Run a computation with pattern bindings in scope.
withPatBindings :: [(Text, TcType)] -> TcM a -> TcM a
withPatBindings [] m = m
withPatBindings ((name, ty) : rest) m =
  extendTermEnv name (TcMonoIdBinder name ty) (withPatBindings rest m)

-- | Infer the type of a right-hand side expression.
inferRhsExpr :: Rhs Expr -> TcM (TcType, [Ct])
inferRhsExpr = inferRhsWithLocals inferExpr

-- | Type-check a right-hand side (solving constraints immediately).
tcRhs :: Rhs Expr -> TcM TcType
tcRhs rhs = do
  (ty, cts) <- inferRhsWithLocals inferExpr rhs
  _ <- solveConstraints cts
  pure ty

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
