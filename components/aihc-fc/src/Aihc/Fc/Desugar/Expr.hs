{-# LANGUAGE OverloadedStrings #-}

-- | Expression desugaring from surface AST to System FC Core.
--
-- Translates each surface expression form into the explicit Core
-- representation. Type lambdas and type applications are inserted
-- where the type checker inferred polymorphism.
module Aihc.Fc.Desugar.Expr
  ( dsExpr,
    dsMatches,
    dsMatchesWithDicts,
    dsRhs,
    DsM,
    DsState (..),
    ClassDict (..),
    freshUnique,
    freshVar,
    lookupType,
    surfaceTypeToTc,
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
    Type (..),
    UnqualifiedName (..),
    ValueDecl (..),
    peelDeclAnn,
    unqualifiedNameText,
  )
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), TyVarId (..), Unique (..))
import Control.Applicative ((<|>))
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
    dsLocalVars :: !(Map Text Var),
    -- | Local dictionaries, keyed by class predicate.
    dsLocalDicts :: !(Map Text Var)
  }

data ClassDict = ClassDict
  { classDictName :: !Text,
    classDictType :: !TcType,
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

withDicts :: [ClassDict] -> DsM a -> DsM a
withDicts dicts action = do
  st <- get
  let oldDicts = dsLocalDicts st
      newDicts = foldr (\dict m -> Map.insert (dictKey (classDictName dict) (classDictType dict)) (classDictVar dict) m) oldDicts dicts
  modify' (\s -> s {dsLocalDicts = newDicts})
  result <- action
  modify' (\s -> s {dsLocalDicts = oldDicts})
  pure result

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
dsMatchesWithDicts abstractDicts ty matches = case matches of
  [] -> do
    v <- freshVar "_void" ty
    pure (FcVar v)
  (m0 : _) ->
    let nArgs = length (matchPats m0)
     in if nArgs == 0
          then -- No patterns: just desugar the first RHS.
            dsRhs (matchRhs m0)
          else do
            let (tyLams, afterForAlls) = peelForAlls ty
                (dictPreds, innerTy) = peelQuals afterForAlls
                (argTys, resTy) = peelFunTys nArgs innerTy
            dicts <- mapM mkClassDict (zip [0 :: Int ..] dictPreds)
            argVars <- mapM (\(i, argTy) -> freshVar (argName i) argTy) (zip [0 :: Int ..] argTys)
            body <- withDicts dicts (buildCaseChain argVars resTy matches)
            let lamExpr = foldr FcLam body argVars
                dictLamExpr
                  | abstractDicts = foldr (FcDictLam . classDictVar) lamExpr dicts
                  | otherwise = lamExpr
            pure (foldr FcTyLam dictLamExpr tyLams)

mkClassDict :: (Int, Pred) -> DsM ClassDict
mkClassDict (i, pred') =
  case pred' of
    ClassPred className [ty] -> do
      var <- freshVar ("$d" <> T.pack (show i)) (predType pred')
      pure (ClassDict className ty var)
    _ -> do
      var <- freshVar ("$d" <> T.pack (show i)) (predType pred')
      pure (ClassDict "<constraint>" unknownTy var)

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
buildCaseChain (scrutVar : restVars) resTy matches = do
  if allVarPatterns matches
    then do
      -- Variable patterns: bind each pattern variable name to the
      -- scrutinee Var, then recurse.
      let bindings = extractVarBindings scrutVar matches
          innerMatches = map dropFirstPat matches
      withLocals bindings (buildCaseChain restVars resTy innerMatches)
    else do
      -- Build one case alternative per first-pattern constructor. Equations
      -- that share that constructor continue together under the next argument.
      alts <- mapM (buildAltGroup scrutVar restVars resTy) (groupFirstPatterns matches)
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

data FirstPatternGroup = FirstPatternGroup !Pattern ![Match]

groupFirstPatterns :: [Match] -> [FirstPatternGroup]
groupFirstPatterns =
  foldl' insertGroup []
  where
    insertGroup groups match =
      case matchPats match of
        [] -> FirstPatternGroup PWildcard [match] : groups
        pat : _ -> insertByKey pat match groups
    insertByKey pat match [] = [FirstPatternGroup pat [match]]
    insertByKey pat match (FirstPatternGroup groupPat matches : rest)
      | patternKey pat == patternKey groupPat =
          FirstPatternGroup (moreSpecificPattern groupPat pat) (matches <> [match]) : rest
      | otherwise = FirstPatternGroup groupPat matches : insertByKey pat match rest

patternKey :: Pattern -> FcAltCon
patternKey = fst . dsPatternPure

moreSpecificPattern :: Pattern -> Pattern -> Pattern
moreSpecificPattern left right
  | patternSpecificity right > patternSpecificity left = right
  | otherwise = left

patternSpecificity :: Pattern -> Int
patternSpecificity pat =
  length [name | name <- snd (dsPatternPure pat), name /= "_", name /= "_pat"]

-- | Build a case alternative from all equations with the same first pattern.
buildAltGroup :: Var -> [Var] -> TcType -> FirstPatternGroup -> DsM FcAlt
buildAltGroup scrutVar restVars resTy (FirstPatternGroup pat matches) =
  case matches of
    [] -> do
      body <- buildCaseChain restVars resTy []
      pure (FcAlt DefaultAlt [] body)
    _ -> do
      let innerMatches = map dropFirstPat matches
          (con, binderNames) = dsPatternPure pat
          binderTys = patternBinderTypes pat (varType scrutVar)
      binders <- zipWithM freshVar binderNames binderTys
      body <- withLocals (zip binderNames binders) (buildCaseChain restVars resTy innerMatches)
      pure (FcAlt con binders body)

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
dsExpr (EApp (EApp fun@(EVar name) arg1) arg2) = do
  maybeDict <- qualifiedCallDict name arg1
  case maybeDict of
    Nothing -> dsRegularApp (EApp fun arg1) arg2
    Just dict -> do
      f' <- dsExpr fun
      arg1' <- dsExpr arg1
      arg2' <- dsExpr arg2
      pure (FcApp (FcApp (FcDictApp f' dict) arg1') arg2')
dsExpr (EApp f a) = do
  dsRegularApp f a
dsExpr (EInfix lhs op rhs)
  | nameText op == "==" || nameText op == "/=" =
      dsEqInfix lhs op rhs
  | otherwise =
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

dsRegularApp :: Expr -> Expr -> DsM FcExpr
dsRegularApp f a = do
  f' <- dsExpr f
  a' <- dsExpr a
  pure (FcApp f' a')

qualifiedCallDict :: Name -> Expr -> DsM (Maybe FcExpr)
qualifiedCallDict name arg = do
  ty <- lookupType (nameToText name)
  qualifiedCallDictFromType arg ty

qualifiedCallDictFromType :: Expr -> TcType -> DsM (Maybe FcExpr)
qualifiedCallDictFromType arg ty =
  case ty of
    TcForAllTy _ body -> qualifiedCallDictFromType arg body
    TcQualTy (ClassPred className [_] : _) _ -> do
      argTy <- exprTcType arg
      traverse (dictForType className) argTy
    _ -> pure Nothing

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

dsEqInfix :: Expr -> Name -> Expr -> DsM FcExpr
dsEqInfix lhs op rhs = do
  lhs' <- dsExpr lhs
  rhs' <- dsExpr rhs
  dict <- dictForEqExpr lhs rhs
  selector <- dsExpr (EVar op)
  pure (FcApp (FcApp (FcDictApp selector dict) lhs') rhs')

dictForEqExpr :: Expr -> Expr -> DsM FcExpr
dictForEqExpr lhs rhs = do
  lhsTy <- exprTcType lhs
  rhsTy <- exprTcType rhs
  case lhsTy <|> rhsTy of
    Just ty -> dictForType "Eq" ty
    Nothing -> dictForType "Eq" unknownTy

dictForType :: Text -> TcType -> DsM FcExpr
dictForType className ty = do
  st <- get
  case Map.lookup (dictKey className ty) (dsLocalDicts st) of
    Just var -> pure (FcVar var)
    Nothing ->
      case ty of
        TcTyCon (TyCon "Bool" 0) [] ->
          pure (FcVar (Var "$fEqBool" (Unique (-101)) (predType (ClassPred "Eq" [boolTy]))))
        TcTyCon (TyCon "[]" 1) [elemTy] -> do
          elemDict <- dictForType className elemTy
          pure (FcDictApp (FcVar (Var "$fEqList" (Unique (-102)) unknownTy)) elemDict)
        _ ->
          pure (FcVar (Var ("$f" <> className <> typeKey ty) (Unique (-199)) (predType (ClassPred className [ty]))))

exprTcType :: Expr -> DsM (Maybe TcType)
exprTcType expr =
  case expr of
    EAnn _ inner -> exprTcType inner
    EParen inner -> exprTcType inner
    EVar name
      | nameText name == "True" || nameText name == "False" ->
          pure (Just boolTy)
      | otherwise -> do
          st <- get
          case Map.lookup (nameToText name) (dsLocalVars st) of
            Just var -> pure (Just (varType var))
            Nothing -> Map.lookup (nameToText name) . dsTypeEnv <$> get
    EList [] -> pure Nothing
    EList (item : _) -> fmap listType <$> exprTcType item
    _ -> pure Nothing

patternBinderTypes :: Pattern -> TcType -> [TcType]
patternBinderTypes pat scrutTy =
  case pat of
    PInfix _lhs op _rhs
      | nameText op == ":" ->
          let elemTy = listElemTy scrutTy
           in [elemTy, scrutTy]
    PCon _ _ subPats -> replicate (length subPats) unknownTy
    PVar {} -> [scrutTy]
    PAnn _ inner -> patternBinderTypes inner scrutTy
    PParen inner -> patternBinderTypes inner scrutTy
    _ -> replicate (length (snd (dsPatternPure pat))) unknownTy

listElemTy :: TcType -> TcType
listElemTy (TcTyCon (TyCon "[]" 1) [elemTy]) = elemTy
listElemTy _ = unknownTy

surfaceTypeToTc :: Map Text TyVarId -> Type -> TcType
surfaceTypeToTc tvMap ty =
  case ty of
    TVar name ->
      case Map.lookup (unqualifiedNameText name) tvMap of
        Just tv -> TcTyVar tv
        Nothing -> TcTyCon (TyCon (unqualifiedNameText name) 0) []
    TCon name _ -> TcTyCon (TyCon (nameText name) 0) []
    TList _ [elemTy] -> listType (surfaceTypeToTc tvMap elemTy)
    TApp f a ->
      case surfaceTypeToTc tvMap f of
        TcTyCon tc args -> TcTyCon (tc {tyConArity = tyConArity tc + 1}) (args <> [surfaceTypeToTc tvMap a])
        fTy -> TcAppTy fTy (surfaceTypeToTc tvMap a)
    TAnn _ inner -> surfaceTypeToTc tvMap inner
    TParen inner -> surfaceTypeToTc tvMap inner
    _ -> unknownTy

dictKey :: Text -> TcType -> Text
dictKey className ty = className <> ":" <> typeKey ty

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

boolTy :: TcType
boolTy = TcTyCon (TyCon "Bool" 0) []

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
