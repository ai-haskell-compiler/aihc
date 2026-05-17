{-# LANGUAGE OverloadedStrings #-}

-- | Desugaring from type-checked surface AST to System FC Core.
--
-- The entry point 'desugarModule' takes a parsed module, runs the type
-- checker, and produces an 'FcProgram'.
module Aihc.Fc.Desugar
  ( -- * Entry point
    desugarModule,
    desugarModuleWithTcResult,
    DesugarResult (..),
  )
where

import Aihc.Fc.Desugar.Expr (ClassDict (..), DsM, DsState (..), dsMatches, dsMatchesWithDicts, dsRhs, freshUnique, freshVar, lookupType, surfaceTypeToTc)
import Aihc.Fc.Desugar.Match (dsDataConPure)
import Aihc.Fc.Syntax
import Aihc.Parser.Syntax
  ( ClassDecl (..),
    ClassDeclItem (..),
    DataDecl (..),
    Decl (..),
    Expr,
    InstanceDecl (..),
    InstanceDeclItem (..),
    Match (..),
    Module (..),
    Pattern (..),
    Rhs,
    Type (..),
    UnqualifiedName (..),
    ValueDecl (..),
    binderHeadName,
    instanceHeadName,
    instanceHeadTypes,
    nameText,
    peelClassDeclItemAnn,
    peelDeclAnn,
    peelInstanceDeclItemAnn,
    tyVarBinderName,
    unqualifiedNameText,
  )
import Aihc.Tc (TcBindingResult (..), TcModuleResult (..), renderTcType, typecheckModule)
import Aihc.Tc.Types (Pred (..), TcType (..), TyCon (..), TyVarId (..), Unique (..))
import Control.Monad.Trans.State.Strict (evalState)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
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
desugarModule m = desugarModuleWithTcResult (typecheckModule m) m

-- | Desugar a module using a type-checking result already computed by
-- the caller. This is useful for clients such as the REPL that preload
-- imported bindings into the type-checker environment.
desugarModuleWithTcResult :: TcModuleResult -> Module -> DesugarResult
desugarModuleWithTcResult tcResult m =
  if not (tcmSuccess tcResult)
    then
      DesugarResult
        { dsProgram = FcProgram [],
          dsSuccess = False,
          dsErrors = showTcFailure tcResult
        }
    else
      let typeEnv = Map.fromList (concatMap bindingTypeEntries (tcmBindings tcResult))
          binds = evalState (dsModule m) (DsState 1000 typeEnv Map.empty Map.empty)
       in DesugarResult
            { dsProgram = FcProgram binds,
              dsSuccess = True,
              dsErrors = []
            }

-- | Format a binding result for error messages.
showBinding :: TcBindingResult -> String
showBinding b = T.unpack (tbDisplayName b) ++ " :: " ++ renderTcType (tbType b)

showTcFailure :: TcModuleResult -> [String]
showTcFailure tcResult =
  case map show (tcmDiagnostics tcResult) of
    [] -> map showBinding (tcmBindings tcResult)
    diagnostics -> diagnostics

bindingTypeEntries :: TcBindingResult -> [(Text, TcType)]
bindingTypeEntries b =
  [(tbName b, tbType b)]

-- | Desugar a module's declarations.
dsModule :: Module -> DsM [FcTopBind]
dsModule m = do
  let decls = moduleDecls m
      classMethods = collectClassMethods decls
  -- Phase 1: data declarations and class method selectors.
  dataTops <- concat <$> mapM dsDecl decls
  -- Phase 2: instance dictionaries.
  instanceTops <- concat <$> mapM (dsInstanceDecl classMethods) (moduleInstances decls)
  -- Phase 3: group and desugar value bindings.
  let grouped = groupFunctionBinds decls
  valueTops <- mapM dsGroup grouped
  pure (dataTops ++ instanceTops ++ valueTops)

-- | Desugar a single declaration (data types only; values handled by groups).
dsDecl :: Decl -> DsM [FcTopBind]
dsDecl (DeclData dd) = (: []) <$> dsDataDeclM dd
dsDecl (DeclClass classDecl) = dsClassDeclM classDecl
dsDecl (DeclAnn _ inner) = dsDecl inner
dsDecl _ = pure []

-- | Desugar a data declaration.
dsDataDeclM :: DataDecl -> DsM FcTopBind
dsDataDeclM dd = do
  let tyName = unqualifiedNameText (binderHeadName (dataDeclHead dd))
      cons = map (\c -> let (n, arity) = dsDataConPure c in (n, replicate arity (TcTyCon (TyCon "?" 0) []))) (dataDeclConstructors dd)
  pure (FcData tyName [] cons)

dsClassDeclM :: ClassDecl -> DsM [FcTopBind]
dsClassDeclM classDecl =
  sequence
    [ dsClassSelector methodName index
    | (index, methodName) <- zip [0 ..] (classMethodNames classDecl)
    ]

dsClassSelector :: Text -> Int -> DsM FcTopBind
dsClassSelector methodName index = do
  methodUnique <- freshUnique
  dictVar <- freshVar "$d" unknownTy
  let methodVar = Var methodName methodUnique unknownTy
      body = FcDictLam dictVar (FcDictSelect (FcVar dictVar) index)
  pure (FcTopBind (FcNonRec methodVar body))

dsInstanceDecl :: Map.Map Text [Text] -> Decl -> DsM [FcTopBind]
dsInstanceDecl classMethods decl =
  case peelDeclAnn decl of
    DeclInstance instanceDecl -> (: []) <$> dsInstanceDict classMethods instanceDecl
    _ -> pure []

dsInstanceDict :: Map.Map Text [Text] -> InstanceDecl -> DsM FcTopBind
dsInstanceDict classMethods instanceDecl =
  case instanceHeadName (instanceDeclHead instanceDecl) of
    Nothing -> do
      dictVar <- freshVar "$fInvalid" unknownTy
      pure (FcTopBind (FcNonRec dictVar (FcDict [])))
    Just className -> do
      let tvNames = nub (map tyVarBinderName (instanceDeclForall instanceDecl) <> concatMap freeTypeVars (instanceDeclContext instanceDecl <> instanceHeadTypes (instanceDeclHead instanceDecl)))
          tvMap = Map.fromList [(name, TyVarId name (Unique (-3000 - ix))) | (ix, name) <- zip [0 :: Int ..] tvNames]
          headTys = map (surfaceTypeToTc tvMap) (instanceHeadTypes (instanceDeclHead instanceDecl))
          methods = Map.fromListWith (<>) [(name, matches) | (name, matches) <- instanceMethodGroups instanceDecl]
          orderedMethods = fromMaybe [] (Map.lookup (nameText className) classMethods)
      contextDicts <- mapM (mkContextDict tvMap) (zip [0 :: Int ..] (instanceDeclContext instanceDecl))
      fields <- mapM (dsInstanceMethod (map classDictPred contextDicts) headTys methods) orderedMethods
      dictVar <- freshVar (instanceDictName (nameText className) headTys) unknownTy
      let dictBody = foldr (FcDictLam . classDictVar) (FcDict fields) contextDicts
      pure (FcTopBind (FcNonRec dictVar dictBody))

mkContextDict :: Map.Map Text TyVarId -> (Int, Type) -> DsM ClassDict
mkContextDict tvMap (ix, predTy) = do
  let className = maybe "<constraint>" nameText (instanceHeadName predTy)
      predArgs = map (surfaceTypeToTc tvMap) (instanceHeadTypes predTy)
      predArg = case predArgs of
        [arg] -> arg
        _ -> unknownTy
  dictVar <- freshVar ("$d" <> T.pack (show ix)) (TcTyCon (TyCon className (length predArgs)) predArgs)
  pure (ClassDict className predArg dictVar)

classDictPred :: ClassDict -> Pred
classDictPred dict = ClassPred (classDictName dict) [classDictType dict]

dsInstanceMethod :: [Pred] -> [TcType] -> Map.Map Text [Match] -> Text -> DsM FcExpr
dsInstanceMethod contextPreds headTys methods methodName =
  case Map.lookup methodName methods of
    Just matches ->
      dsMatchesWithDicts False (TcQualTy contextPreds (methodType (matchArity matches) (instanceSelfType headTys))) matches
    Nothing -> do
      missing <- freshVar ("$missing_" <> methodName) unknownTy
      pure (FcVar missing)

methodType :: Int -> TcType -> TcType
methodType arity selfTy = foldr TcFunTy boolTy (replicate arity selfTy)

instanceSelfType :: [TcType] -> TcType
instanceSelfType [ty] = ty
instanceSelfType _ = unknownTy

instanceDictName :: Text -> [TcType] -> Text
instanceDictName "Eq" [TcTyCon (TyCon "Bool" 0) []] = "$fEqBool"
instanceDictName "Eq" [TcTyCon (TyCon "[]" 1) [_]] = "$fEqList"
instanceDictName className tys = "$f" <> className <> T.concat (map typeSuffix tys)

typeSuffix :: TcType -> Text
typeSuffix ty =
  case ty of
    TcTyVar tv -> tvName tv
    TcTyCon tc [] -> tyConName tc
    TcTyCon (TyCon "[]" _) [_] -> "List"
    TcTyCon tc args -> tyConName tc <> T.concat (map typeSuffix args)
    _ -> "T"

classMethodNames :: ClassDecl -> [Text]
classMethodNames classDecl = concatMap itemMethodNames (classDeclItems classDecl)

itemMethodNames :: ClassDeclItem -> [Text]
itemMethodNames item =
  case peelClassDeclItemAnn item of
    ClassItemTypeSig names _ -> map unqualifiedNameText names
    _ -> []

collectClassMethods :: [Decl] -> Map.Map Text [Text]
collectClassMethods = Map.fromList . mapMaybe collect
  where
    collect decl =
      case peelDeclAnn decl of
        DeclClass classDecl ->
          Just (unqualifiedNameText (binderHeadName (classDeclHead classDecl)), classMethodNames classDecl)
        _ -> Nothing

moduleInstances :: [Decl] -> [Decl]
moduleInstances = filter isInstance
  where
    isInstance decl =
      case peelDeclAnn decl of
        DeclInstance {} -> True
        _ -> False

instanceMethodGroups :: InstanceDecl -> [(Text, [Match])]
instanceMethodGroups instanceDecl =
  Map.toList (Map.fromListWith (<>) (concatMap itemMethods (instanceDeclItems instanceDecl)))

itemMethods :: InstanceDeclItem -> [(Text, [Match])]
itemMethods item =
  case peelInstanceDeclItemAnn item of
    InstanceItemBind (FunctionBind name matches) -> [(unqualifiedNameText name, matches)]
    _ -> []

freeTypeVars :: Type -> [Text]
freeTypeVars ty =
  nub $
    case ty of
      TVar name -> [unqualifiedNameText name]
      TList _ args -> concatMap freeTypeVars args
      TApp f a -> freeTypeVars f <> freeTypeVars a
      TAnn _ inner -> freeTypeVars inner
      TParen inner -> freeTypeVars inner
      TContext preds inner -> concatMap freeTypeVars preds <> freeTypeVars inner
      _ -> []

matchArity :: [Match] -> Int
matchArity [] = 0
matchArity (match : _) = length (matchPats match)

unknownTy :: TcType
unknownTy = TcTyCon (TyCon "?" 0) []

boolTy :: TcType
boolTy = TcTyCon (TyCon "Bool" 0) []

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
      DeclPattern _ rhs -> dsRhs rhs
  pure (FcTopBind (FcNonRec var body))
