{-# LANGUAGE OverloadedStrings #-}

-- | Expression desugaring from surface AST to System FC Core.
--
-- Translates each surface expression form into the explicit Core
-- representation. Type lambdas and type applications are inserted
-- where the type checker inferred polymorphism.
module Aihc.Fc.Desugar.Expr
  ( dsExpr,
    dsMatches,
    DsM,
    DsState (..),
    freshUnique,
    freshVar,
    lookupType,
  )
where

import Aihc.Fc.Desugar.Match (dsPatternPure)
import Aihc.Fc.Syntax
import Aihc.Parser.Syntax
  ( CaseAlt (..),
    Decl (..),
    Expr (..),
    Match (..),
    Name (..),
    Pattern (..),
    Rhs (..),
    TupleFlavor (..),
    UnqualifiedName (..),
    ValueDecl (..),
    peelDeclAnn,
    unqualifiedNameText,
  )
import Aihc.Tc.Types (TcType (..), TyCon (..), TyVarId (..), Unique (..))
import Control.Monad (zipWithM)
import Control.Monad.Trans.State.Strict (State, get, modify')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

-- | Desugaring monad.
type DsM = State DsState

-- | Desugaring state.
data DsState = DsState
  { dsNextUnique :: !Int,
    -- | Map from surface name to its inferred type (from TC).
    dsTypeEnv :: !(Map Text TcType),
    -- | Local variable bindings (pattern-bound, lambda-bound).
    dsLocalVars :: !(Map Text Var)
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

-- | Look up a name's type (locals first, then global TC env).
lookupType :: Text -> DsM TcType
lookupType name = do
  st <- get
  case Map.lookup name (dsLocalVars st) of
    Just v -> pure (varType v)
    Nothing -> case Map.lookup name (dsTypeEnv st) of
      Just ty -> pure ty
      Nothing -> pure (TcTyCon (TyCon name 0) [])

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

-- | Desugar a list of match equations into a Core expression.
--
-- For a function like @not True = False; not False = True@, this
-- produces a lambda + case expression.
--
-- For a polymorphic function like @id x = x@, this wraps with
-- type lambdas and lambdas referencing the same variable.
dsMatches :: TcType -> [Match] -> DsM FcExpr
dsMatches ty matches = case matches of
  [] -> do
    v <- freshVar "_void" ty
    pure (FcVar v)
  (m0 : _) ->
    let nArgs = length (matchPats m0)
     in if nArgs == 0
          then -- No patterns: just desugar the first RHS.
            dsRhs (matchRhs m0)
          else do
            -- Peel off foralls for type lambdas.
            let (tyLams, innerTy) = peelForAlls ty
                (argTys, resTy) = peelFunTys nArgs innerTy
            -- Create variables for each argument.
            argVars <- mapM (\(i, argTy) -> freshVar (argName i) argTy) (zip [0 :: Int ..] argTys)
            -- Build the body: case analysis on arguments.
            body <- buildCaseChain argVars resTy matches
            -- Wrap in lambdas.
            let lamExpr = foldr FcLam body argVars
            -- Wrap in type lambdas.
            pure (foldr FcTyLam lamExpr tyLams)

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
buildCaseChain (scrutVar : restVars) resTy matches = do
  if allVarPatterns matches
    then do
      -- Variable patterns: bind each pattern variable name to the
      -- scrutinee Var, then recurse.
      let bindings = extractVarBindings scrutVar matches
          innerMatches = map dropFirstPat matches
      withLocals bindings (buildCaseChain restVars resTy innerMatches)
    else do
      -- Build case alternatives from the first argument's patterns.
      alts <- mapM (buildAlt scrutVar restVars resTy) matches
      caseBinder <- freshVar "_scrut" (varType scrutVar)
      pure (FcCase (FcVar scrutVar) caseBinder (scrutResultType restVars resTy) alts)

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

-- | Compute the result type of a case expression, accounting for remaining
-- arguments that will be lambdas in each branch.
scrutResultType :: [Var] -> TcType -> TcType
scrutResultType vs resTy = foldr (TcFunTy . varType) resTy vs

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

-- | Build a case alternative from a match equation.
buildAlt :: Var -> [Var] -> TcType -> Match -> DsM FcAlt
buildAlt _scrutVar restVars resTy m = case matchPats m of
  (pat : restPats) -> do
    let m' = m {matchPats = restPats}
        (con, binderNames) = dsPatternPure pat
    -- Create binder variables (with placeholder types for MVP).
    binders <- mapM (\nm -> freshVar nm (TcTyCon (TyCon "?" 0) [])) binderNames
    body <-
      if null restVars && null restPats
        then dsRhs (matchRhs m)
        else buildCaseChain restVars resTy [m']
    pure (FcAlt con binders body)
  [] -> do
    body <- dsRhs (matchRhs m)
    pure (FcAlt DefaultAlt [] body)

-- | Desugar a right-hand side.
dsRhs :: Rhs Expr -> DsM FcExpr
dsRhs (UnguardedRhs _sp expr maybeDecls) =
  case maybeDecls of
    Nothing -> dsExpr expr
    Just decls -> dsLetDecls decls (dsExpr expr)
dsRhs (GuardedRhss _sp _guards _decls) = do
  v <- freshVar "_unimplemented" (TcTyCon (TyCon "?" 0) [])
  pure (FcVar v)

-- | Desugar a surface expression to Core.
dsExpr :: Expr -> DsM FcExpr
dsExpr (EVar name) = do
  let n = nameToText name
  -- Check local bindings first (pattern/lambda variables).
  mLocal <- lookupLocal n
  case mLocal of
    Just v -> pure (FcVar v)
    Nothing -> do
      ty <- lookupType n
      v <- freshVar n ty
      pure (FcVar v)
dsExpr (EInt i _ _) = pure (FcLit (LitInt i))
dsExpr (EChar c _) = pure (FcLit (LitChar c))
dsExpr (EString s _) = dsStringLiteral s
dsExpr (EApp f a) = do
  f' <- dsExpr f
  a' <- dsExpr a
  pure (FcApp f' a')
dsExpr (EInfix lhs op rhs) =
  dsExpr (EApp (EApp (EVar op) lhs) rhs)
dsExpr (EList elems) =
  foldr consList nilList <$> mapM dsExpr elems
dsExpr (ETuple Boxed elems) =
  dsTuple elems
dsExpr (EParen inner) = dsExpr inner
dsExpr (EAnn _ann inner) = dsExpr inner
dsExpr (EIf cond thenE elseE) = do
  cond' <- dsExpr cond
  then' <- dsExpr thenE
  else' <- dsExpr elseE
  let boolTy = TcTyCon (TyCon "Bool" 0) []
  binder <- freshVar "_if" boolTy
  pure
    ( FcCase
        cond'
        binder
        (exprType then')
        [ FcAlt (DataAlt "True") [] then',
          FcAlt (DataAlt "False") [] else'
        ]
    )
dsExpr (ECase scrut alts) = do
  scrut' <- dsExpr scrut
  binder <- freshVar "_case" (exprType scrut')
  alts' <- mapM dsCaseAlt alts
  let resTy = case alts' of
        (a : _) -> exprType (altRhs a)
        [] -> TcTyCon (TyCon "?" 0) []
  pure (FcCase scrut' binder resTy alts')
dsExpr (ELambdaPats pats body) = do
  vars <- mapM (\_ -> freshVar "_lam" (TcTyCon (TyCon "?" 0) [])) pats
  body' <- dsExpr body
  pure (foldr FcLam body' vars)
dsExpr (ELetDecls decls body) =
  dsLetDecls decls (dsExpr body)
dsExpr _ = do
  v <- freshVar "_unsupported" (TcTyCon (TyCon "?" 0) [])
  pure (FcVar v)

-- | Desugar local let/where declarations as a recursive Core let.
--
-- Type checking has already validated the binding group. Here we only need
-- stable Core variables so RHSs and the body refer to the same local binders.
dsLetDecls :: [Decl] -> DsM FcExpr -> DsM FcExpr
dsLetDecls decls bodyAction = do
  let groups = groupLocalDecls decls
      names = map localGroupName groups
  vars <- mapM (`freshVar` unknownTy) names
  let localBindings = zip names vars
  withLocals localBindings $ do
    rhsBindings <- zipWithM dsLocalGroup vars groups
    body <- bodyAction
    pure $
      if null rhsBindings
        then body
        else FcLet (FcRec rhsBindings) body

unknownTy :: TcType
unknownTy = TcTyCon (TyCon "?" 0) []

data LocalDeclGroup
  = LocalFunction !Text ![Match]
  | LocalPattern !Text !(Rhs Expr)

localGroupName :: LocalDeclGroup -> Text
localGroupName group =
  case group of
    LocalFunction name _ -> name
    LocalPattern name _ -> name

groupLocalDecls :: [Decl] -> [LocalDeclGroup]
groupLocalDecls [] = []
groupLocalDecls (decl : rest) =
  case extractLocalFunction decl of
    Just (name, matches) ->
      let (sameNameDecls, rest') = span (hasSameLocalFunctionName name) rest
          allMatches = matches ++ concatMap (maybe [] snd . extractLocalFunction) sameNameDecls
       in LocalFunction name allMatches : groupLocalDecls rest'
    Nothing ->
      case extractLocalPattern decl of
        Just group -> group : groupLocalDecls rest
        Nothing -> groupLocalDecls rest

extractLocalFunction :: Decl -> Maybe (Text, [Match])
extractLocalFunction decl =
  case peelDeclAnn decl of
    DeclValue (FunctionBind name matches) -> Just (unqualifiedNameText name, matches)
    _ -> Nothing

hasSameLocalFunctionName :: Text -> Decl -> Bool
hasSameLocalFunctionName name decl =
  case extractLocalFunction decl of
    Just (declName, _) -> declName == name
    Nothing -> False

extractLocalPattern :: Decl -> Maybe LocalDeclGroup
extractLocalPattern decl =
  case peelDeclAnn decl of
    DeclValue (PatternBind _ pat rhs) ->
      LocalPattern <$> barePatternName pat <*> pure rhs
    _ -> Nothing

barePatternName :: Pattern -> Maybe Text
barePatternName pat =
  case pat of
    PVar name -> Just (unqualifiedNameText name)
    PAnn _ inner -> barePatternName inner
    PParen inner -> barePatternName inner
    _ -> Nothing

dsLocalGroup :: Var -> LocalDeclGroup -> DsM (Var, FcExpr)
dsLocalGroup var group =
  case group of
    LocalFunction _ matches -> do
      rhs <- dsMatches (varType var) matches
      pure (var, rhs)
    LocalPattern _ rhs -> do
      rhs' <- dsRhs rhs
      pure (var, rhs')

dsStringLiteral :: Text -> DsM FcExpr
dsStringLiteral text =
  pure (T.foldr consChar nilList text)

dsTuple :: [Maybe Expr] -> DsM FcExpr
dsTuple elems = do
  elems' <- mapM dsMaybeTupleElem elems
  pure (foldl' FcApp (tupleConExpr (length elems')) elems')

dsMaybeTupleElem :: Maybe Expr -> DsM FcExpr
dsMaybeTupleElem (Just expr) = dsExpr expr
dsMaybeTupleElem Nothing = do
  v <- freshVar "_tuple_section" unknownTy
  pure (FcVar v)

tupleConExpr :: Int -> FcExpr
tupleConExpr arity =
  FcVar (Var (tupleConName arity) (Unique (-20 - arity)) unknownTy)

tupleConName :: Int -> Text
tupleConName arity = "(" <> T.replicate (max 0 (arity - 1)) "," <> ")"

consChar :: Char -> FcExpr -> FcExpr
consChar char =
  consList (FcLit (LitChar char))

consList :: FcExpr -> FcExpr -> FcExpr
consList headExpr =
  FcApp (FcApp consExpr headExpr)

nilList :: FcExpr
nilList =
  FcVar (Var "[]" (Unique (-10)) (listType (TcTyCon (TyCon "?" 0) [])))

consExpr :: FcExpr
consExpr =
  FcVar (Var ":" (Unique (-11)) (TcFunTy unknownTy (TcFunTy (listType unknownTy) (listType unknownTy))))

listType :: TcType -> TcType
listType ty =
  TcTyCon (TyCon "[]" 1) [ty]

-- | Desugar a case alternative.
dsCaseAlt :: CaseAlt Expr -> DsM FcAlt
dsCaseAlt (CaseAlt _anns pat rhs) = do
  let (con, binderNames) = dsPatternPure pat
  binders <- mapM (\nm -> freshVar nm (TcTyCon (TyCon "?" 0) [])) binderNames
  body <- dsRhs rhs
  pure (FcAlt con binders body)

-- | Convert a Name to Text.
nameToText :: Name -> Text
nameToText n = case nameQualifier n of
  Nothing -> nameText n
  Just q -> q <> "." <> nameText n

-- | Extract the type from a Core expression (best effort).
exprType :: FcExpr -> TcType
exprType (FcVar v) = varType v
exprType (FcLit (LitInt _)) = TcTyCon (TyCon "Int" 0) []
exprType (FcLit (LitChar _)) = TcTyCon (TyCon "Char" 0) []
exprType _ = TcTyCon (TyCon "?" 0) []
