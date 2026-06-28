{-# LANGUAGE OverloadedStrings #-}

-- | Expression desugaring from surface AST to System FC Core.
--
-- Translates each surface expression form into the explicit Core
-- representation. Type lambdas and type applications are inserted
-- where the type checker inferred polymorphism.
module Aihc.Fc.Desugar.Expr
  ( dsMatches,
    dsMatchesWithDicts,
    dsRhs,
    DsM,
    DsState (..),
    ClassDict (..),
    desugarBug,
    freshUnique,
    freshVar,
    lookupType,
  )
where

import Aihc.Fc.Desugar.Match (dsPatternPure)
import Aihc.Fc.Subst (substType)
import Aihc.Fc.Syntax
import Aihc.Parser.Syntax
  ( CaseAlt (..),
    CompStmt (..),
    Decl (..),
    Expr (..),
    Match (..),
    Name (..),
    Pattern (..),
    Rhs (..),
    TupleFlavor (..),
    UnqualifiedName (..),
    ValueDecl (..),
    fromAnnotation,
    peelCompStmtAnn,
    peelDeclAnn,
    unqualifiedNameText,
  )
import Aihc.Tc.Annotations (TcAnnotation (..))
import Aihc.Tc.Evidence (EvTerm (..))
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), TyVarId (..), Unique (..))
import Control.Applicative ((<|>))
import Control.Monad (zipWithM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, get, modify')
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
    -- | Map from surface name to its inferred type (from TC).
    dsTypeEnv :: !(Map Text TcType),
    -- | Local variable bindings (pattern-bound, lambda-bound).
    dsLocalVars :: !(Map Text Var),
    -- | Local dictionaries, keyed by class predicate.
    dsLocalDicts :: !(Map Text Var)
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
      newDicts = foldr (\dict m -> Map.insert (dictKey (classDictName dict) (classDictArgs dict)) (classDictVar dict) m) oldDicts dicts
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
          then do
            let (tyLams, afterForAlls) = peelForAlls ty
                dictPreds = fst (peelQuals afterForAlls)
            dicts <- mapM mkClassDict (zip [0 :: Int ..] dictPreds)
            body <- withDicts dicts (dsRhs (matchRhs m0))
            let dictLamExpr
                  | abstractDicts = foldr (FcDictLam . classDictVar) body dicts
                  | otherwise = body
            pure (foldr FcTyLam dictLamExpr tyLams)
          else do
            let (tyLams, afterForAlls) = peelForAlls ty
                (dictPreds, innerTy) = peelQuals afterForAlls
                (argTys, resTy) = peelFunTys nArgs innerTy
            dicts <- mapM mkClassDict (zip [0 :: Int ..] dictPreds)
            argVars <- mapM (\(i, argTy) -> freshInternalVar (argName i) argTy) (zip [0 :: Int ..] argTys)
            body <- withDicts dicts (buildCaseChain argVars resTy matches)
            let lamExpr = foldr FcLam body argVars
                dictLamExpr
                  | abstractDicts = foldr (FcDictLam . classDictVar) lamExpr dicts
                  | otherwise = lamExpr
            pure (foldr FcTyLam dictLamExpr tyLams)

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

qualifiedBody :: TcType -> TcType
qualifiedBody ty = snd (peelQuals (snd (peelForAlls ty)))

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
      pure (FcCase (FcVar scrutVar) caseBinder alts)

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

groupFirstPatterns :: [Match] -> [FirstPatternGroup]
groupFirstPatterns =
  List.foldl' insertGroup []
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
      binderTys <- patternBinderTypesM pat (varType scrutVar)
      binders <- zipWithM freshVar binderNames binderTys
      body <- withLocals (zip binderNames binders) (buildCaseChain restVars resTy innerMatches)
      pure (FcAlt con binders body)

dsRhs :: Rhs Expr -> DsM FcExpr
dsRhs (UnguardedRhs _sp expr maybeDecls) =
  case maybeDecls of
    Nothing -> dsExpr expr
    Just decls -> dsLetDecls decls (dsExpr expr)
dsRhs GuardedRhss {} =
  desugarBug "unsupported guarded RHS after type checking"

dsExpr :: Expr -> DsM FcExpr
dsExpr (EAnn ann inner)
  | Just tcAnn <- fromAnnotation ann =
      dsAnnotatedExpr tcAnn inner
dsExpr (EVar name) = do
  let n = nameToText name
  -- Check local bindings first (pattern/lambda variables).
  mLocal <- lookupLocalName name
  case mLocal of
    Just v -> pure (FcVar v)
    Nothing -> do
      ty <- lookupTypeName name
      v <- freshVar n ty
      pure (FcVar v)
dsExpr (EInt i _ _) = pure (FcLit (LitInt i))
dsExpr (EChar c _) = pure (FcLit (LitChar c))
dsExpr (EString s _) = dsStringLiteral s
dsExpr (EApp fun arg) =
  FcApp <$> dsExpr fun <*> dsExpr arg
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
dsExpr (EIf cond thenE elseE) =
  dsIf cond thenE elseE
dsExpr (ECase scrut alts) =
  dsCase scrut alts
dsExpr (ELambdaPats pats body) =
  dsLambda pats body
dsExpr (ELetDecls decls body) =
  dsLetDecls decls (dsExpr body)
dsExpr expr =
  desugarBug ("unsupported expression form after type checking: " <> take 80 (show expr))

dsAnnotatedVar :: TcAnnotation -> Name -> Expr -> DsM FcExpr
dsAnnotatedVar tcAnn name _expr = do
  let n = nameToText name
  mLocal <- lookupLocalName name
  case mLocal of
    Just v -> pure (FcVar v)
    Nothing -> do
      ty <- lookupTypeName name
      v <- freshVar n ty
      let typedExpr = List.foldl' FcTyApp (FcVar v) (tcAnnTypeArgs tcAnn)
      dicts <- mapM dsEvidence (tcAnnEvidenceTerms tcAnn)
      pure (List.foldl' FcDictApp typedExpr dicts)

dsAnnotatedExpr :: TcAnnotation -> Expr -> DsM FcExpr
dsAnnotatedExpr tcAnn inner =
  case inner of
    EVar name -> dsAnnotatedVar tcAnn name inner
    EApp fun arg -> FcApp <$> dsExpr fun <*> dsExpr arg
    ELetDecls decls body -> dsLetDecls decls (dsExpr body)
    EList elems -> dsList tcAnn elems
    EListComp body quals -> dsListComp tcAnn body quals
    ETuple Boxed elems -> dsTuple tcAnn elems
    ELambdaPats pats body -> dsLambda pats body
    EIf cond thenE elseE -> dsIf cond thenE elseE
    EInfix lhs op rhs -> dsInfix lhs op rhs
    ECase scrut alts -> dsCase scrut alts
    _ -> desugarBug ("unsupported annotated expression form after type checking: " <> take 80 (show inner))

dsInfix :: Expr -> Name -> Expr -> DsM FcExpr
dsInfix lhs op rhs =
  FcApp <$> (FcApp <$> dsInfixOperator op <*> dsExpr lhs) <*> dsExpr rhs

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
  binder <- freshVar "_if" boolTy
  pure
    ( FcCase
        cond'
        binder
        [ FcAlt (DataAlt "True") [] then',
          FcAlt (DataAlt "False") [] else'
        ]
    )

dsCase :: Expr -> [CaseAlt Expr] -> DsM FcExpr
dsCase scrut alts = do
  scrut' <- dsExpr scrut
  scrutTy <-
    case exprAnnotationType scrut of
      Just ty -> pure ty
      Nothing -> fcExprTypeM scrut'
  binder <- freshVar "_case" scrutTy
  alts' <- mapM (dsCaseAlt scrutTy) alts
  pure (FcCase scrut' binder alts')

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
dsStringLiteral text =
  pure (T.foldr consChar (nilList charTy) text)

dsList :: TcAnnotation -> [Expr] -> DsM FcExpr
dsList tcAnn elems =
  case tcAnnTypeArgs tcAnn of
    [elemTy] ->
      foldr (consList elemTy) (nilList elemTy) <$> mapM dsExpr elems
    elemTys ->
      desugarBug ("list annotation arity mismatch: expected 1 type argument, got " <> show (length elemTys))

dsListComp :: TcAnnotation -> Expr -> [CompStmt] -> DsM FcExpr
dsListComp tcAnn body quals = do
  elemTy <- listCompElemTy tcAnn
  dsCompQuals elemTy body quals (nilList elemTy)

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
      pure (consList elemTy body' tailExpr)
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
  binder <- freshInternalVar "_lc_guard" boolTy
  pure
    ( FcCase
        guard'
        binder
        [ FcAlt (DataAlt "True") [] trueBranch,
          FcAlt (DataAlt "False") [] tailExpr
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
  let recurTail = FcApp (FcVar worker) (FcVar restVar)
  consRhs <- dsCompGenMatch elemTy body pat rest headVar recurTail
  let workerBody =
        FcLam listVar $
          FcCase
            (FcVar listVar)
            caseBinder
            [ FcAlt (DataAlt "[]") [] tailExpr,
              FcAlt (DataAlt ":") [headVar, restVar] consRhs
            ]
  pure (FcLet (FcRec [(worker, workerBody)]) (FcApp (FcVar worker) src'))

dsCompGenMatch :: TcType -> Expr -> Pattern -> [CompStmt] -> Var -> FcExpr -> DsM FcExpr
dsCompGenMatch elemTy body pat rest headVar skipExpr =
  case directPatternBindings pat headVar of
    Just bindings ->
      withLocals bindings (dsCompQuals elemTy body rest skipExpr)
    Nothing -> do
      let (con, binderNames) = dsPatternPure pat
      case con of
        DefaultAlt ->
          desugarBug ("unsupported list comprehension generator pattern: " <> take 80 (show pat))
        _ -> do
          binderTys <- patternBinderTypesM pat (varType headVar)
          binders <- zipWithM freshVar binderNames binderTys
          matched <- withLocals (zip binderNames binders) (dsCompQuals elemTy body rest skipExpr)
          caseBinder <- freshInternalVar "_lc_match" (varType headVar)
          pure
            ( FcCase
                (FcVar headVar)
                caseBinder
                [ FcAlt con binders matched,
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
  body' <- withLocals (concat (zipWith lambdaPatternBindings pats vars)) (dsExpr body)
  pure (foldr FcLam body' vars)

lambdaArgName :: Pattern -> Text
lambdaArgName pat =
  case pat of
    PVar name -> unqualifiedNameText name
    PAnn _ inner -> lambdaArgName inner
    PParen inner -> lambdaArgName inner
    PAs name _ -> unqualifiedNameText name
    _ -> "_lam"

lambdaPatternBindings :: Pattern -> Var -> [(Text, Var)]
lambdaPatternBindings pat var =
  case pat of
    PVar name -> [(unqualifiedNameText name, var)]
    PAnn _ inner -> lambdaPatternBindings inner var
    PParen inner -> lambdaPatternBindings inner var
    PAs name inner -> (unqualifiedNameText name, var) : lambdaPatternBindings inner var
    _ -> []

dsTuple :: TcAnnotation -> [Maybe Expr] -> DsM FcExpr
dsTuple tcAnn elems = do
  let elemTys = tcAnnTypeArgs tcAnn
  if length elemTys == length elems
    then do
      elems' <- zipWithM dsMaybeTupleElem elemTys elems
      pure (List.foldl' FcApp (tupleConExpr elemTys) elems')
    else desugarBug ("tuple annotation arity mismatch: expected " <> show (length elems) <> " type argument(s), got " <> show (length elemTys))

dsMaybeTupleElem :: TcType -> Maybe Expr -> DsM FcExpr
dsMaybeTupleElem _ (Just expr) = dsExpr expr
dsMaybeTupleElem ty Nothing = do
  v <- freshVar "_tuple_section" ty
  pure (FcVar v)

tupleConExpr :: [TcType] -> FcExpr
tupleConExpr elemTys =
  let arity = length elemTys
   in List.foldl' FcTyApp (FcVar (Var (tupleConName arity) (Unique (-20 - arity)) (tupleConType elemTys))) elemTys

tupleConType :: [TcType] -> TcType
tupleConType elemTys =
  foldr TcForAllTy (foldr (TcFunTy . TcTyVar) resultTy tyVars) tyVars
  where
    arity = length elemTys
    tyVars = [TyVarId ("t" <> T.pack (show i)) (Unique (-2100 - i)) | i <- [0 .. arity - 1]]
    resultTy = TcTyCon (TyCon (tupleConName arity) arity) (map TcTyVar tyVars)

tupleConName :: Int -> Text
tupleConName arity = "(" <> T.replicate (max 0 (arity - 1)) "," <> ")"

consChar :: Char -> FcExpr -> FcExpr
consChar char =
  consList charTy (FcLit (LitChar char))

consList :: TcType -> FcExpr -> FcExpr -> FcExpr
consList elemTy headExpr =
  FcApp (FcApp (consExpr elemTy) headExpr)

nilList :: TcType -> FcExpr
nilList =
  FcTyApp (FcVar (Var "[]" (Unique (-10)) nilListType))

consExpr :: TcType -> FcExpr
consExpr =
  FcTyApp (FcVar (Var ":" (Unique (-11)) consListType))

nilListType :: TcType
nilListType =
  TcForAllTy listElemVar (listType (TcTyVar listElemVar))

consListType :: TcType
consListType =
  TcForAllTy listElemVar (TcFunTy elemTy (TcFunTy (listType elemTy) (listType elemTy)))
  where
    elemTy = TcTyVar listElemVar

listElemVar :: TyVarId
listElemVar = TyVarId "a" (Unique (-2000))

listType :: TcType -> TcType
listType ty =
  TcTyCon (TyCon "[]" 1) [ty]

dsEvidence :: EvTerm -> DsM FcExpr
dsEvidence evidence =
  case evidence of
    EvGiven (ClassPred className args) -> do
      st <- get
      case Map.lookup (dictKey className args) (dsLocalDicts st) of
        Just var -> pure (FcVar var)
        Nothing ->
          desugarBug ("missing local dictionary for " <> T.unpack (dictKey className args))
    EvGiven EqPred {} ->
      pure (FcDict [])
    EvDict dictName typeArgs contextEvidence -> do
      dictTy <- lookupType dictName
      contextDicts <- mapM dsEvidence contextEvidence
      let dictExpr = List.foldl' FcTyApp (FcVar (Var dictName (Unique (-199)) dictTy)) typeArgs
      pure (List.foldl' FcDictApp dictExpr contextDicts)
    EvCoercion {} ->
      pure (FcDict [])
    EvSuperClass dict index ->
      (`FcDictSelect` index) <$> dsEvidence dict
    EvCast dict _co ->
      dsEvidence dict
    EvVarTerm {} ->
      desugarBug "unresolved evidence variable in type-checker annotation"

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

patternBinderTypesM :: Pattern -> TcType -> DsM [TcType]
patternBinderTypesM pat scrutTy =
  case pat of
    PInfix _lhs op _rhs
      | nameText op == ":" ->
          (\elemTy -> [elemTy, scrutTy]) <$> listElemTyM scrutTy
    PCon _ _ [] -> pure []
    PCon name _ subPats -> do
      fallbackTys <- constructorFieldTypesM name (length subPats)
      zipWithM patternFieldTypeM subPats fallbackTys
    PVar {} -> pure [scrutTy]
    PAnn _ inner -> patternBinderTypesM inner scrutTy
    PParen inner -> patternBinderTypesM inner scrutTy
    _
      | null (snd (dsPatternPure pat)) -> pure []
      | otherwise -> missingPatternTypes
  where
    missingPatternTypes =
      desugarBug ("missing pattern binder type information while desugaring: " <> take 80 (show pat))

    patternFieldTypeM subPat fallbackTy =
      pure (fromMaybe fallbackTy (patternBinderAnnotationType subPat <|> patternAnnotationType subPat))

constructorFieldTypesM :: Name -> Int -> DsM [TcType]
constructorFieldTypesM name arity = do
  ty <- lookupTypeName name
  takeConstructorFields (nameToText name) arity (dropForAlls ty)

takeConstructorFields :: Text -> Int -> TcType -> DsM [TcType]
takeConstructorFields _ 0 _ = pure []
takeConstructorFields name arity (TcFunTy arg rest) =
  (arg :) <$> takeConstructorFields name (arity - 1) rest
takeConstructorFields name arity ty =
  desugarBug ("missing field type information for constructor pattern " <> T.unpack name <> ": expected " <> show arity <> " more field(s) in " <> show ty)

dropForAlls :: TcType -> TcType
dropForAlls (TcForAllTy _ body) = dropForAlls body
dropForAlls ty = ty

listElemTyM :: TcType -> DsM TcType
listElemTyM (TcTyCon (TyCon "[]" 1) [elemTy]) = pure elemTy
listElemTyM ty =
  desugarBug ("missing list element type information while desugaring: " <> show ty)

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
    FcLit lit -> pure (litType lit)
    FcApp fun _arg -> do
      funTy <- qualifiedBody <$> fcExprTypeM fun
      case funTy of
        TcFunTy _argTy resTy -> pure resTy
        _ -> desugarBug ("application to non-function type while desugaring: " <> show funTy)
    FcDictApp fun _dict -> do
      funTy <- fcExprTypeM fun
      case funTy of
        TcQualTy (_pred : preds) body -> pure (if null preds then body else TcQualTy preds body)
        TcFunTy _argTy resTy -> pure resTy
        _ -> desugarBug ("dictionary application to non-qualified type while desugaring: " <> show funTy)
    FcTyApp fun ty -> do
      funTy <- fcExprTypeM fun
      case funTy of
        TcForAllTy tv body -> pure (substType (Map.singleton tv ty) body)
        _ -> desugarBug ("type application to non-forall type while desugaring: " <> show funTy)
    FcLam var body -> TcFunTy (varType var) <$> fcExprTypeM body
    FcTyLam tv body -> TcForAllTy tv <$> fcExprTypeM body
    FcDictLam var body -> TcFunTy (varType var) <$> fcExprTypeM body
    FcDict _fields -> pure (TcTyCon (TyCon "Dict" 0) [])
    FcDictSelect {} -> desugarBug "dictionary selection lacks field type annotation while desugaring"
    FcLet _bind body -> fcExprTypeM body
    FcCase _scrut _binder alts ->
      case alts of
        [] -> desugarBug "case expression has no alternatives while desugaring"
        FcAlt _ _ body : _ -> fcExprTypeM body
    FcCast inner _co -> fcExprTypeM inner

litType :: Literal -> TcType
litType (LitInt _) = TcTyCon (TyCon "Int" 0) []
litType (LitChar _) = TcTyCon (TyCon "Char" 0) []
litType (LitString _) = TcTyCon (TyCon "[]" 1) [TcTyCon (TyCon "Char" 0) []]

dictKey :: Text -> [TcType] -> Text
dictKey className args = className <> ":" <> T.intercalate "," (map typeKey args)

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

charTy :: TcType
charTy = TcTyCon (TyCon "Char" 0) []

-- | Desugar a case alternative.
dsCaseAlt :: TcType -> CaseAlt Expr -> DsM FcAlt
dsCaseAlt scrutTy (CaseAlt _anns pat rhs) = do
  let (con, binderNames) = dsPatternPure pat
  binderTys <- patternBinderTypesM pat scrutTy
  binders <- zipWithM freshVar binderNames binderTys
  body <- withLocals (zip binderNames binders) (dsRhs rhs)
  pure (FcAlt con binders body)

-- | Convert a Name to Text.
nameToText :: Name -> Text
nameToText n = case nameQualifier n of
  Nothing -> nameText n
  Just q -> q <> "." <> nameText n

lookupLocalName :: Name -> DsM (Maybe Var)
lookupLocalName name = do
  local <- lookupLocal (nameToText name)
  case local of
    Just var -> pure (Just var)
    Nothing -> lookupLocal (nameText name)

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
