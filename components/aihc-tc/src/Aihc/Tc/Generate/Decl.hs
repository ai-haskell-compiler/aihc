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
    DataConDecl (..),
    DataDecl (..),
    Decl (..),
    Match (..),
    Module (..),
    Name (..),
    NameType (..),
    Pattern (..),
    Rhs (..),
    SourceSpan (..),
    UnqualifiedName (..),
    ValueDecl (..),
    fromAnnotation,
    getDeclSourceSpan,
    getPatternSourceSpan,
    peelDeclAnn,
  )
import Aihc.Tc.Constraint
import Aihc.Tc.Generalize (generalize)
import Aihc.Tc.Generate.Expr (inferExpr)
import Aihc.Tc.Instantiate qualified
import Aihc.Tc.Monad
import Aihc.Tc.Solve (solveConstraints)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
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
  -- Phase 2: group and type-check value bindings.
  -- Multiple FunctionBind declarations with the same name are merged
  -- into a single binding with combined match equations.
  let grouped = groupValueDecls (moduleDecls m)
  valueResults <- concat <$> mapM tcDeclGroup grouped
  pure (dataResults ++ valueResults)

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
tcDeclGroup :: DeclGroup -> TcM [TcBindingResult]
tcDeclGroup (SingleDecl d) = tcDecl d
tcDeclGroup (MergedFunctionBind _sp binder matches) = do
  let name = unqualifiedNameText binder
      displayName = renderBinderName binder
  (ty, cts) <- tcMatches matches
  _ <- solveConstraints cts
  -- Generalize over free meta-variables to produce a type scheme.
  scheme <- generalize ty []
  let schemeTy = schemeToType scheme
  zonkedTy <- zonkType schemeTy
  -- Register the binding so later bindings can reference it.
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
  let tyName = unqualifiedNameText (dataDeclName dd)
      resTy = TcTyCon (TyCon tyName 0) []
      starKind = TcTyCon (TyCon "*" 0) []
      tyConResult = TcBindingResult tyName starKind
  conResults <- mapM (registerDataCon resTy) (dataDeclConstructors dd)
  pure (tyConResult : conResults)

-- | Register a single data constructor as a polymorphic binding.
-- Returns the binding result for the constructor.
registerDataCon :: TcType -> DataConDecl -> TcM TcBindingResult
registerDataCon resTy con = case con of
  DataConAnn _ inner -> registerDataCon resTy inner
  PrefixCon _docs _ctx conName args ->
    let name = unqualifiedNameText conName
        -- For each argument type, create a function type.
        -- For MVP, we ignore the actual types and treat nullary
        -- constructors as just returning the result type.
        -- Constructors with args get a function type with fresh args.
        scheme
          | null args = ForAll [] [] resTy
          | otherwise = ForAll [] [] resTy -- TODO: parse arg types
     in do
          extendTermEnvPermanent name (TcIdBinder name scheme)
          pure (TcBindingResult name resTy)
  InfixCon _docs _ctx _lhs conName _rhs ->
    let name = unqualifiedNameText conName
     in do
          extendTermEnvPermanent name (TcIdBinder name (ForAll [] [] resTy))
          pure (TcBindingResult name resTy)
  RecordCon _docs _ctx conName _fields ->
    let name = unqualifiedNameText conName
     in do
          extendTermEnvPermanent name (TcIdBinder name (ForAll [] [] resTy))
          pure (TcBindingResult name resTy)
  GadtCon {} ->
    -- GADTs not handled in MVP.
    pure (TcBindingResult "<gadt>" resTy)

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
  (ty, cts) <- tcMatches matches
  _ <- solveConstraints cts
  -- Generalize over free meta-variables to produce a type scheme.
  scheme <- generalize ty []
  let schemeTy = schemeToType scheme
  zonkedTy <- zonkType schemeTy
  -- Register the binding so later bindings can reference it.
  extendTermEnvPermanent name (TcIdBinder name scheme)
  pure [TcBindingResult displayName zonkedTy]
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
tcMatches :: [Match] -> TcM (TcType, [Ct])
tcMatches [] = do
  ty <- freshMetaTv
  pure (ty, [])
tcMatches matches@(m0 : _) = do
  let nArgs = length (matchPats m0)
  if nArgs == 0
    then do
      -- No patterns: just infer the RHS of the first match.
      -- All match RHSes should agree on a type.
      (ty0, cts0) <- inferRhsExpr (matchRhs m0)
      restCts <- concatMapM (unifyMatchRhs ty0) (drop 1 matches)
      pure (ty0, cts0 ++ restCts)
    else do
      -- Create fresh meta-variables for the argument types and result type.
      argTys <- mapM (const freshMetaTv) [1 .. nArgs]
      resTy <- freshMetaTv
      -- Process each equation.
      allCts <- concatMapM (tcMatchEquation argTys resTy) matches
      let funTy = foldr TcFunTy resTy argTys
      pure (funTy, allCts)

-- | Type-check a single match equation against expected arg/result types.
tcMatchEquation :: [TcType] -> TcType -> Match -> TcM [Ct]
tcMatchEquation argTys resTy match = do
  let pats = matchPats match
  -- Bind pattern variables with their corresponding arg types.
  let bindings = concatMap extractPatternBindings (zip pats argTys)
  -- Also emit constraints from constructor patterns.
  patCts <- concatMapM (uncurry inferPatCts) (zip pats argTys)
  -- Infer the RHS under the extended environment.
  (rhsTy, rhsCts) <- withPatBindings bindings (inferRhsExpr (matchRhs match))
  -- RHS type must match the expected result type.
  ev <- freshEvVar
  let sp = sourceSpanFromAnns (matchAnns match)
  let resCt = mkWantedCt (EqPred rhsTy resTy) ev (AppOrigin sp) sp
  pure (patCts ++ rhsCts ++ [resCt])

-- | Unify an additional match equation's RHS with the expected type.
unifyMatchRhs :: TcType -> Match -> TcM [Ct]
unifyMatchRhs expectedTy match = do
  (rhsTy, rhsCts) <- inferRhsExpr (matchRhs match)
  ev <- freshEvVar
  let sp = sourceSpanFromAnns (matchAnns match)
  let eqCt = mkWantedCt (EqPred rhsTy expectedTy) ev (AppOrigin sp) sp
  pure (rhsCts ++ [eqCt])

-- | Infer constraints from a pattern matching against a scrutinee type.
--
-- For constructor patterns, emit that the scrutinee type matches the
-- constructor's result type. For variable and wildcard patterns, no
-- extra constraints are needed.
inferPatCts :: Pattern -> TcType -> TcM [Ct]
inferPatCts pat scrutTy = case pat of
  PCon name _subPats -> do
    let conName = patNameToText name
    mBinder <- lookupTerm conName
    case mBinder of
      Just (TcIdBinder _ scheme) -> do
        (conTy, _preds) <- instantiateSch scheme
        let conResTy = resultType conTy
        ev <- freshEvVar
        let sp = getPatternSourceSpan pat
        pure [mkWantedCt (EqPred scrutTy conResTy) ev (AppOrigin sp) sp]
      _ -> pure []
  PAnn _ann inner -> inferPatCts inner scrutTy
  PParen inner -> inferPatCts inner scrutTy
  PStrict inner -> inferPatCts inner scrutTy
  PIrrefutable inner -> inferPatCts inner scrutTy
  _ -> pure []

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
  PCon _name subPats ->
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
