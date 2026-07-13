{-# LANGUAGE OverloadedStrings #-}

-- | Desugaring from type-checked surface AST to System FC Core.
--
-- The entry point 'desugarModule' takes a parsed module, runs the type
-- checker, and produces an 'FcProgram'.
module Aihc.Fc.Desugar
  ( -- * Entry point
    desugarModule,
    desugarModuleWithBindings,
    desugarModuleWithTcResult,
    DesugarResult (..),
  )
where

import Aihc.Fc.Desugar.Expr (ClassDict (..), DsM, DsState (..), desugarBug, dsMatches, dsMatchesWithGivenDicts, freshUnique, freshVar, lookupType)
import Aihc.Fc.Desugar.Match (dsDataConPure)
import Aihc.Fc.Subst (substType)
import Aihc.Fc.Syntax
import Aihc.Parser.Syntax
  ( CallConv (..),
    DataConDecl,
    DataDecl (..),
    Decl (..),
    Expr,
    ForeignDecl (..),
    ForeignDirection (..),
    ForeignEntitySpec (..),
    ForeignSafety (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
    Match (..),
    MatchHeadForm (..),
    Module (..),
    NewtypeDecl (..),
    Pattern (..),
    Rhs,
    UnqualifiedName (..),
    ValueDecl (..),
    binderHeadName,
    fromAnnotation,
    peelDeclAnn,
    unqualifiedNameText,
  )
import Aihc.Resolve (ResolveResult (..), resolve)
import Aihc.Tc (TcBindingResult (..), renderTcSignature, tcModuleBindings, tcModuleDiagnostics, tcModuleSuccess, typecheckModule)
import Aihc.Tc.Annotations (TcAnnotation (..), TcClassAnnotation (..), TcClassMethodAnnotation (..), TcDictBinderAnnotation (..), TcInstanceAnnotation (..), TcInstanceMethodAnnotation (..))
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), TyVarId (..), Unique (..))
import Control.Monad (zipWithM)
import Control.Monad.Trans.State.Strict (runStateT)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T

-- | Result of desugaring.
data DesugarResult = DesugarResult
  { dsProgram :: !FcProgram,
    dsSuccess :: !Bool,
    dsErrors :: ![String]
  }
  deriving (Show)

-- | Desugar a module: parse, typecheck, then translate to Core.
desugarModule :: Module -> DesugarResult
desugarModule m =
  case resolve [m] of
    ResolveResult {resolvedModules = [resolved], resolveErrors = []} ->
      desugarModuleWithTcResult (typecheckModule resolved) resolved
    ResolveResult {resolveErrors} ->
      DesugarResult
        { dsProgram = FcProgram [],
          dsSuccess = False,
          dsErrors = ["resolve error: " <> show resolveErrors]
        }

-- | Desugar a module using a type-checking result already computed by
-- the caller. This is useful for clients such as the REPL that preload
-- imported bindings into the type-checker environment.
desugarModuleWithTcResult :: Module -> Module -> DesugarResult
desugarModuleWithTcResult tcResult =
  desugarModuleWithBindings (tcModuleBindings tcResult) tcResult

desugarModuleWithBindings :: [TcBindingResult] -> Module -> Module -> DesugarResult
desugarModuleWithBindings bindings tcResult _m =
  if not (tcModuleSuccess tcResult)
    then
      DesugarResult
        { dsProgram = FcProgram [],
          dsSuccess = False,
          dsErrors = showTcFailure tcResult
        }
    else
      let typeEnv = Map.fromList (builtinTypeEntries <> concatMap bindingTypeEntries bindings)
       in case runStateT (dsModule tcResult) (DsState 1000 typeEnv Map.empty Map.empty) of
            Left err ->
              DesugarResult
                { dsProgram = FcProgram [],
                  dsSuccess = False,
                  dsErrors = [err]
                }
            Right (binds, _) ->
              DesugarResult
                { dsProgram = FcProgram binds,
                  dsSuccess = True,
                  dsErrors = []
                }

-- | Format a binding result for error messages.
showBinding :: TcBindingResult -> String
showBinding b = renderTcSignature (tbDisplayName b) (tbType b)

showTcFailure :: Module -> [String]
showTcFailure tcResult =
  case map show (tcModuleDiagnostics tcResult) of
    [] -> map showBinding (tcModuleBindings tcResult)
    diagnostics -> diagnostics

bindingTypeEntries :: TcBindingResult -> [(Text, TcType)]
bindingTypeEntries b =
  [(tbName b, tbType b)]

builtinTypeEntries :: [(Text, TcType)]
builtinTypeEntries =
  [ (":", TcForAllTy aVar (TcFunTy aTy (TcFunTy listA listA))),
    ("[]", TcForAllTy aVar listA)
  ]
  where
    aVar = TyVarId "a" (Unique (-1000))
    aTy = TcTyVar aVar
    listA = TcTyCon (TyCon "[]" 1) [aTy]

-- | Desugar a module's declarations.
dsModule :: Module -> DsM [FcTopBind]
dsModule m = do
  let decls = moduleDecls m
  -- Phase 1: data declarations and class method selectors.
  dataTops <- concat <$> mapM dsDecl decls
  -- Phase 2: instance dictionaries.
  instanceTops <- concat <$> mapM dsInstanceDecl (moduleInstances decls)
  -- Phase 3: group and desugar value bindings.
  let grouped = groupFunctionBinds decls
  valueTops <- mapM dsGroup grouped
  pure (dataTops ++ instanceTops ++ valueTops)

-- | Desugar a single declaration (data types only; values handled by groups).
dsDecl :: Decl -> DsM [FcTopBind]
dsDecl (DeclData dd) = (: []) <$> dsDataDeclM dd
dsDecl (DeclNewtype nd) = (: []) <$> dsNewtypeDeclM nd
dsDecl (DeclAnn ann (DeclForeign foreignDecl))
  | Just tcAnn <- fromAnnotation ann = (: []) <$> dsForeignImport tcAnn foreignDecl
dsDecl (DeclAnn ann (DeclClass _classDecl))
  | Just classAnn <- fromAnnotation ann = dsClassDeclM classAnn
dsDecl (DeclAnn _ inner) = dsDecl inner
dsDecl DeclClass {} = desugarBug "missing type-checker annotation for class declaration"
dsDecl _ = pure []

-- | Desugar a data declaration.
dsDataDeclM :: DataDecl -> DsM FcTopBind
dsDataDeclM dd = do
  let tyName = unqualifiedNameText (binderHeadName (dataDeclHead dd))
  cons <- mapM dsDataConM (dataDeclConstructors dd)
  pure (FcData tyName [] cons)

-- | Preserve newtype declarations as distinct FC bindings so backends may
-- erase their representation while the evaluator can still model source-level
-- construction and pattern matching.
dsNewtypeDeclM :: NewtypeDecl -> DsM FcTopBind
dsNewtypeDeclM nd = do
  let tyName = unqualifiedNameText (binderHeadName (newtypeDeclHead nd))
  case newtypeDeclConstructor nd of
    Nothing -> desugarBug ("newtype " <> T.unpack tyName <> " has no constructor")
    Just con -> do
      (conName, fields) <- dsDataConM con
      case fields of
        [fieldTy] -> pure (FcNewtype tyName [] conName fieldTy)
        _ -> desugarBug ("newtype constructor " <> T.unpack conName <> " does not have exactly one field")

dsForeignImport :: TcAnnotation -> ForeignDecl -> DsM FcTopBind
dsForeignImport tcAnn foreignDecl
  | foreignDirection foreignDecl /= ForeignImport =
      desugarBug "unsupported foreign export after type checking"
  | otherwise =
      case foreignCallConv foreignDecl of
        CPrim -> dsForeignPrim tcAnn foreignDecl
        CCall -> dsForeignCcall tcAnn foreignDecl
        callConv -> desugarBug ("unsupported foreign calling convention after type checking: " <> show callConv)

dsForeignPrim :: TcAnnotation -> ForeignDecl -> DsM FcTopBind
dsForeignPrim tcAnn foreignDecl = do
  let name = unqualifiedNameText (foreignName foreignDecl)
      ty = tcAnnType tcAnn
  arity <- validatePrimitiveImport name ty
  unique <- freshUnique
  pure (FcPrimitive (Var name unique ty) arity)

dsForeignCcall :: TcAnnotation -> ForeignDecl -> DsM FcTopBind
dsForeignCcall tcAnn foreignDecl = do
  if foreignSafety foreignDecl == Just Unsafe
    then pure ()
    else desugarBug "only unsafe foreign imports are supported"
  symbol <-
    case foreignEntity foreignDecl of
      ForeignEntityNamed name -> pure name
      ForeignEntityStatic (Just name) -> pure name
      ForeignEntityOmitted -> pure (unqualifiedNameText (foreignName foreignDecl))
      _ -> desugarBug "only statically named foreign imports are supported"
  let name = unqualifiedNameText (foreignName foreignDecl)
      ty = tcAnnType tcAnn
  signature <- foreignCallSignature ty
  unique <- freshUnique
  pure
    ( FcForeignImport
        FcForeignCall
          { fcForeignCallVar = Var name unique ty,
            fcForeignCallSymbol = symbol,
            fcForeignCallSignature = signature
          }
    )

foreignCallSignature :: TcType -> DsM FcForeignSignature
foreignCallSignature ty = do
  let (arguments, result) = splitForeignFunctionType ty
  argumentTypes <- traverse marshalForeignType arguments
  resultType <-
    case result of
      TcTyCon (TyCon "IO" 1) [ioResult] -> FcForeignIO <$> marshalForeignType ioResult
      _ -> FcForeignPure <$> marshalForeignType result
  pure
    FcForeignSignature
      { fcForeignArgumentTypes = argumentTypes,
        fcForeignResult = resultType
      }

splitForeignFunctionType :: TcType -> ([TcType], TcType)
splitForeignFunctionType (TcFunTy argument result) =
  let (arguments, finalResult) = splitForeignFunctionType result
   in (argument : arguments, finalResult)
splitForeignFunctionType result = ([], result)

marshalForeignType :: TcType -> DsM FcForeignType
marshalForeignType (TcTyCon (TyCon "CInt" 0) []) = pure FcForeignCInt
marshalForeignType ty =
  desugarBug ("unsupported foreign import value type: " <> show ty)

validatePrimitiveImport :: Text -> TcType -> DsM Int
validatePrimitiveImport name ty =
  case Map.lookup name primitiveImportSpecs of
    Nothing ->
      desugarBug ("unknown foreign import prim: " <> T.unpack name)
    Just spec
      | primitiveSpecAccepts spec ty -> pure (primitiveSpecArity spec)
      | otherwise ->
          desugarBug ("incorrect type for foreign import prim " <> T.unpack name)

data PrimitiveSpec = PrimitiveSpec
  { primitiveSpecArity :: !Int,
    primitiveSpecAccepts :: TcType -> Bool
  }

primitiveImportSpecs :: Map.Map Text PrimitiveSpec
primitiveImportSpecs =
  Map.fromList
    [ ("+#", intBinaryPrim),
      ("-#", intBinaryPrim),
      ("*#", intBinaryPrim),
      ("compareInt#", intBinaryPrim),
      ("<#", intBinaryPrim),
      ("==#", intBinaryPrim),
      ("charToInt#", PrimitiveSpec 1 (typesEqual charToIntPrimTy)),
      ("intToChar#", PrimitiveSpec 1 (typesEqual intToCharPrimTy)),
      ("raise#", PrimitiveSpec 1 isRaisePrimType),
      ("catch#", PrimitiveSpec 3 isCatchPrimType)
    ]

intBinaryPrim :: PrimitiveSpec
intBinaryPrim =
  PrimitiveSpec 2 (typesEqual intHashBinaryTy)

intHashBinaryTy :: TcType
intHashBinaryTy = TcFunTy intHashTy (TcFunTy intHashTy intHashTy)

intHashTy :: TcType
intHashTy = TcTyCon (TyCon "Int#" 0) []

charHashTy :: TcType
charHashTy = TcTyCon (TyCon "Char#" 0) []

charToIntPrimTy :: TcType
charToIntPrimTy = TcFunTy charHashTy intHashTy

intToCharPrimTy :: TcType
intToCharPrimTy = TcFunTy intHashTy charHashTy

isRaisePrimType :: TcType -> Bool
isRaisePrimType ty =
  case collectForAlls ty of
    ([arg, result], TcFunTy (TcTyVar arg') (TcTyVar result')) ->
      arg == arg' && result == result'
    _ -> False

isCatchPrimType :: TcType -> Bool
isCatchPrimType ty =
  case collectForAlls ty of
    ([resultVar, exceptionVar], body) ->
      isCatchPrimBody resultVar exceptionVar body
    _ -> False

isCatchPrimBody :: TyVarId -> TyVarId -> TcType -> Bool
isCatchPrimBody resultVar exceptionVar ty =
  case ty of
    TcFunTy actionTy (TcFunTy handlerTy (TcFunTy stateTy resultTy)) ->
      case (actionTy, handlerTy) of
        (TcFunTy actionState actionResult, TcFunTy exceptionTy (TcFunTy handlerState handlerResult)) ->
          typesEqual statePrimRealWorldTy actionState
            && typesEqual statePrimRealWorldTy stateTy
            && typesEqual statePrimRealWorldTy handlerState
            && typesEqual (TcTyVar exceptionVar) exceptionTy
            && typesEqual resultTupleTy actionResult
            && typesEqual resultTupleTy handlerResult
            && typesEqual resultTupleTy resultTy
        _ -> False
    _ -> False
  where
    resultTupleTy =
      unboxedTupleTy [statePrimRealWorldTy, TcTyVar resultVar]

statePrimRealWorldTy :: TcType
statePrimRealWorldTy = TcTyCon (TyCon "State#" 1) [realWorldTy]

realWorldTy :: TcType
realWorldTy = TcTyCon (TyCon "RealWorld" 0) []

unboxedTupleTy :: [TcType] -> TcType
unboxedTupleTy tys =
  TcTyCon (TyCon ("(#" <> T.replicate (max 0 (length tys - 1)) "," <> "#)") (length tys)) tys

collectForAlls :: TcType -> ([TyVarId], TcType)
collectForAlls (TcForAllTy tv body) =
  let (tvs, inner) = collectForAlls body
   in (tv : tvs, inner)
collectForAlls ty = ([], ty)

typesEqual :: TcType -> TcType -> Bool
typesEqual (TcTyVar a) (TcTyVar b) = a == b
typesEqual (TcMetaTv a) (TcMetaTv b) = a == b
typesEqual (TcTyCon tc1 args1) (TcTyCon tc2 args2) =
  tc1 == tc2 && length args1 == length args2 && all (uncurry typesEqual) (zip args1 args2)
typesEqual (TcFunTy a1 b1) (TcFunTy a2 b2) =
  typesEqual a1 a2 && typesEqual b1 b2
typesEqual (TcForAllTy tv1 body1) (TcForAllTy tv2 body2) =
  typesEqual body1 (substType (Map.singleton tv2 (TcTyVar tv1)) body2)
typesEqual (TcQualTy p1 b1) (TcQualTy p2 b2) =
  length p1 == length p2 && all (uncurry predsEqual) (zip p1 p2) && typesEqual b1 b2
typesEqual (TcAppTy f1 a1) (TcAppTy f2 a2) =
  typesEqual f1 f2 && typesEqual a1 a2
typesEqual _ _ = False

predsEqual :: Pred -> Pred -> Bool
predsEqual (ClassPred c1 a1) (ClassPred c2 a2) =
  c1 == c2 && length a1 == length a2 && all (uncurry typesEqual) (zip a1 a2)
predsEqual (EqPred t1a t1b) (EqPred t2a t2b) =
  typesEqual t1a t2a && typesEqual t1b t2b
predsEqual _ _ = False

dsDataConM :: DataConDecl -> DsM (Text, [TcType])
dsDataConM con = do
  let (name, arity) = dsDataConPure con
  ty <- lookupType name
  fields <- dataConFieldTypes name arity (dropForAlls ty)
  pure (name, fields)

dataConFieldTypes :: Text -> Int -> TcType -> DsM [TcType]
dataConFieldTypes _ 0 _ = pure []
dataConFieldTypes name arity (TcFunTy arg rest) =
  (arg :) <$> dataConFieldTypes name (arity - 1) rest
dataConFieldTypes name arity ty =
  desugarBug ("missing field type information for data constructor " <> T.unpack name <> ": expected " <> show arity <> " more field(s) in " <> show ty)

dsClassDeclM :: TcClassAnnotation -> DsM [FcTopBind]
dsClassDeclM classAnn =
  mapM dsClassSelector (tcClassMethods classAnn)

dsClassSelector :: TcClassMethodAnnotation -> DsM FcTopBind
dsClassSelector methodAnn = do
  methodUnique <- freshUnique
  dictVars <- zipWithM mkSelectorDict [0 :: Int ..] dictPreds
  classDictVar <-
    case dictVars of
      dictVar : _ -> pure dictVar
      [] -> freshVar "$d" (tcClassMethodDictType methodAnn)
  let methodVar = Var (tcClassMethodName methodAnn) methodUnique (tcClassMethodType methodAnn)
      selected = FcDictSelect (FcVar classDictVar) (tcClassMethodIndex methodAnn)
      body = foldr FcTyLam (foldr FcDictLam selected dictVars) (tcClassMethodTyVars methodAnn)
  pure (FcTopBind (FcNonRec methodVar body))
  where
    (_tyVars, afterForAlls) = peelForAlls (tcClassMethodType methodAnn)
    (dictPreds, _bodyTy) = peelQuals afterForAlls
    mkSelectorDict ix pred' =
      freshVar ("$d" <> T.pack (show ix)) (predType pred')

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

dsInstanceDecl :: Decl -> DsM [FcTopBind]
dsInstanceDecl decl =
  case decl of
    DeclAnn ann (DeclInstance instanceDecl)
      | Just instAnn <- fromAnnotation ann -> (: []) <$> dsInstanceDict instAnn instanceDecl
    DeclAnn _ inner -> dsInstanceDecl inner
    DeclInstance {} -> desugarBug "missing type-checker annotation for instance declaration"
    _ -> pure []

dsInstanceDict :: TcInstanceAnnotation -> InstanceDecl -> DsM FcTopBind
dsInstanceDict instAnn instanceDecl = do
  let methods = Map.fromListWith combineMethods (instanceMethodGroups instanceDecl)
  contextDicts <- zipWithM mkContextDict [0 :: Int ..] (tcInstanceContextDicts instAnn)
  fields <- mapM (dsInstanceMethod contextDicts methods) (tcInstanceMethodOrder instAnn)
  dictVar <- freshVar (tcInstanceDictName instAnn) (tcInstanceDictType instAnn)
  let dictBody = foldr FcTyLam (foldr (FcDictLam . classDictVar) (FcDict fields) contextDicts) (tcInstanceTyVars instAnn)
  pure (FcTopBind (FcNonRec dictVar dictBody))
  where
    combineMethods (newTy, newMatches) (_oldTy, oldMatches) = (newTy, oldMatches <> newMatches)

mkContextDict :: Int -> TcDictBinderAnnotation -> DsM ClassDict
mkContextDict ix dictAnn = do
  dictVar <- freshVar ("$d" <> T.pack (show ix)) (tcDictBinderType dictAnn)
  pure (ClassDict (tcDictBinderClassName dictAnn) (tcDictBinderArgs dictAnn) dictVar)

classDictPred :: ClassDict -> Pred
classDictPred dict = ClassPred (classDictName dict) (classDictArgs dict)

dsInstanceMethod :: [ClassDict] -> Map.Map Text (TcType, [Match]) -> Text -> DsM FcExpr
dsInstanceMethod contextDicts methods methodName =
  case Map.lookup methodName methods of
    Just (expected, matches) ->
      dsMatchesWithGivenDicts contextDicts (TcQualTy (map classDictPred contextDicts) expected) matches
    Nothing ->
      desugarBug ("missing method " <> T.unpack methodName <> " in instance dictionary")

moduleInstances :: [Decl] -> [Decl]
moduleInstances = filter isInstance
  where
    isInstance decl =
      case peelDeclAnn decl of
        DeclInstance {} -> True
        _ -> False

instanceMethodGroups :: InstanceDecl -> [(Text, (TcType, [Match]))]
instanceMethodGroups instanceDecl =
  concatMap itemMethods (instanceDeclItems instanceDecl)

itemMethods :: InstanceDeclItem -> [(Text, (TcType, [Match]))]
itemMethods item =
  case item of
    InstanceItemAnn ann inner
      | Just methodAnn <- fromAnnotation ann -> itemMethodWithAnnotation methodAnn inner
      | otherwise -> itemMethods inner
    _ -> []

itemMethodWithAnnotation :: TcInstanceMethodAnnotation -> InstanceDeclItem -> [(Text, (TcType, [Match]))]
itemMethodWithAnnotation methodAnn item =
  case item of
    InstanceItemAnn _ inner -> itemMethodWithAnnotation methodAnn inner
    InstanceItemBind (FunctionBind _ matches) ->
      [(tcInstanceMethodName methodAnn, (tcInstanceMethodType methodAnn, matches))]
    InstanceItemBind (PatternBind _ _ rhs) ->
      [ ( tcInstanceMethodName methodAnn,
          ( tcInstanceMethodType methodAnn,
            [ Match
                { matchAnns = [],
                  matchHeadForm = MatchHeadPrefix,
                  matchPats = [],
                  matchRhs = rhs
                }
            ]
          )
        )
      ]
    _ -> []

dropForAlls :: TcType -> TcType
dropForAlls (TcForAllTy _ body) = dropForAlls body
dropForAlls ty = ty

-- | A group of top-level value declarations.
data DeclGroup
  = DeclFunction !Text ![Match]
  | DeclPattern !Text !(Rhs Expr)

dgName :: DeclGroup -> Text
dgName group =
  case group of
    DeclFunction name _ -> name
    DeclPattern name _ -> name

-- | Group consecutive FunctionBind declarations with the same name and keep
-- simple top-level pattern binds.
groupFunctionBinds :: [Decl] -> [DeclGroup]
groupFunctionBinds [] = []
groupFunctionBinds (d : ds) = case extractFunBind d of
  Just (name, matches) ->
    let (sameNameDecls, rest) = span (hasSameName name) ds
        allMatches = matches ++ concatMap (maybe [] snd . extractFunBind) sameNameDecls
     in DeclFunction name allMatches : groupFunctionBinds rest
  Nothing ->
    case extractPatternBind d of
      Just group -> group : groupFunctionBinds ds
      Nothing -> groupFunctionBinds ds

-- | Extract function bind info from a declaration.
extractFunBind :: Decl -> Maybe (Text, [Match])
extractFunBind decl = case peelDeclAnn decl of
  DeclValue (FunctionBind name matches) ->
    Just (unqualifiedNameText name, matches)
  _ -> Nothing

-- | Check if a declaration is a FunctionBind with the given name.
hasSameName :: Text -> Decl -> Bool
hasSameName name d = case extractFunBind d of
  Just (n, _) -> n == name
  Nothing -> False

extractPatternBind :: Decl -> Maybe DeclGroup
extractPatternBind decl =
  case peelDeclAnn decl of
    DeclValue (PatternBind _ pat rhs) ->
      DeclPattern <$> barePatternName pat <*> pure rhs
    _ -> Nothing

barePatternName :: Pattern -> Maybe Text
barePatternName pat =
  case pat of
    PVar name -> Just (unqualifiedNameText name)
    PAnn _ inner -> barePatternName inner
    PParen inner -> barePatternName inner
    _ -> Nothing

-- | Desugar a function binding group.
dsGroup :: DeclGroup -> DsM FcTopBind
dsGroup grp = do
  ty <- lookupType (dgName grp)
  u <- freshUnique
  let var = Var (dgName grp) u ty
  body <-
    case grp of
      DeclFunction _ matches -> dsMatches ty matches
      DeclPattern _ rhs -> dsMatches ty [rhsAsMatch rhs]
  pure (FcTopBind (FcNonRec var body))

rhsAsMatch :: Rhs Expr -> Match
rhsAsMatch rhs =
  Match
    { matchAnns = [],
      matchHeadForm = MatchHeadPrefix,
      matchPats = [],
      matchRhs = rhs
    }
