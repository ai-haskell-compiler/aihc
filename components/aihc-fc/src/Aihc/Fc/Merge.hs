{-# LANGUAGE OverloadedStrings #-}

-- | Merge System FC modules into one module container.
module Aihc.Fc.Merge
  ( FcMergeError (..),
    mergePrograms,
  )
where

import Aihc.Fc.External (declareExternalSymbols)
import Aihc.Fc.Subst (freeRigidTyVarsOf, maximumProgramUnique, programVars)
import Aihc.Fc.Syntax
import Aihc.Tc.TypeScheme (equivalentTypeSchemes, typeSchemeFromType)
import Aihc.Tc.Types (TcType (..), Unique (..))
import Data.Char (isAlphaNum, ord)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)

data FcMergeError
  = FcDuplicateDefinition !FcSymbolOrigin
  | FcImportTypeMismatch !FcSymbolOrigin !TcType !TcType
  deriving (Eq, Show)

-- | Merge one or more programs and resolve each available term reference.
mergePrograms :: FcModuleId -> NonEmpty FcProgram -> Either (NonEmpty FcMergeError) FcProgram
mergePrograms target programs =
  case duplicateErrors <> typeErrors of
    firstError : rest -> Left (firstError NonEmpty.:| rest)
    [] -> Right (declareExternalSymbols merged)
  where
    freshPrograms = freshenPrograms (NonEmpty.toList programs)
    qualifiedPrograms = map qualifyTopLevelBinders freshPrograms
    importedTypes = Map.fromList [(origin, ty) | program <- qualifiedPrograms, FcExternal origin ty <- fcTopBinds program]
    providerEntries = concatMap (programProviders importedTypes) qualifiedPrograms
    providers = Map.fromList providerEntries
    sourceProviders = fallbackSourceProviders providerEntries
    duplicateErrors = map FcDuplicateDefinition (duplicates (map fst providerEntries))
    typeErrors = concatMap (checkExternalTypes providers) qualifiedPrograms
    merged =
      FcProgram
        target
        ( concatMap
            (map (resolveTopBind providers sourceProviders) . filter (keepExternal providers) . fcTopBinds)
            qualifiedPrograms
        )

freshenPrograms :: [FcProgram] -> [FcProgram]
freshenPrograms = snd . foldl freshen (1, [])
  where
    freshen (nextUnique, done) program =
      let offset = nextUnique - minimumProgramUnique program
          shifted = shiftProgramVars offset program
       in (1 + maximumProgramUnique shifted, done <> [shifted])

minimumProgramUnique :: FcProgram -> Int
minimumProgramUnique program =
  minimum (0 : [unique | var <- programVars program, Unique unique <- [varUnique var]])

qualifyTopLevelBinders :: FcProgram -> FcProgram
qualifyTopLevelBinders program =
  program {fcTopBinds = map qualifyTopBind (fcTopBinds program)}
  where
    moduleId = fcProgramModule program
    qualifiedBinders = Map.fromList (concatMap qualifiedEntries (fcTopBinds program))
    qualifyValue var =
      case globalOrigin var of
        origin@FcTopLevelOrigin {} ->
          var
            { varName = globalBinderName origin,
              varResolvedName = Just origin
            }
        origin@FcBuiltinOrigin {} -> var {varResolvedName = Just origin}
    qualifyPrimitive var = var {varResolvedName = Just (globalOrigin var)}
    qualifiedEntries topBind =
      case topBind of
        FcPrimitive var _ -> [(varName var, qualifyPrimitive var)]
        FcTopBind bind -> [(varName var, qualifyValue var) | var <- bindVars bind]
        _ -> []
    globalOrigin var =
      fromMaybe
        ( FcTopLevelOrigin
            { fcOriginPackage = fcModulePackageText moduleId,
              fcOriginModule = fcModuleName moduleId,
              fcOriginName = varName var
            }
        )
        (varResolvedName var)
    qualifyTopBind topBind =
      case topBind of
        FcPrimitive var arity -> FcPrimitive (qualifyPrimitive var) arity
        FcTopBind (FcNonRec var rhs) -> FcTopBind (FcNonRec (qualified var) (resolveLocal Set.empty rhs))
        FcTopBind (FcRec bindings) -> FcTopBind (FcRec [(qualified var, resolveLocal Set.empty rhs) | (var, rhs) <- bindings])
        _ -> topBind
    qualified var = Map.findWithDefault (qualifyValue var) (varName var) qualifiedBinders
    resolveLocal bound expression =
      case expression of
        FcVar var
          | isNothing (varResolvedName var),
            varUnique var `Set.notMember` bound ->
              FcVar (Map.findWithDefault var (varName var) qualifiedBinders)
          | otherwise -> expression
        FcLit {} -> expression
        FcApp function argument -> FcApp (recur bound function) (recur bound argument)
        FcTyApp function ty -> FcTyApp (recur bound function) ty
        FcLam var body -> FcLam var (recur (Set.insert (varUnique var) bound) body)
        FcTyLam tyVar body -> FcTyLam tyVar (recur bound body)
        FcLet bind body ->
          let binders = bindVars bind
              bodyBound = bound <> Set.fromList (map varUnique binders)
           in FcLet (resolveLocalBind bound bind) (recur bodyBound body)
        FcCase scrutinee binder alternatives ->
          FcCase
            (recur bound scrutinee)
            binder
            [ alternative
                { altRhs =
                    recur
                      (bound <> Set.fromList (map varUnique (binder : altBinders alternative)))
                      (altRhs alternative)
                }
            | alternative <- alternatives
            ]
        FcCast body coercion -> FcCast (recur bound body) coercion
        FcCallForeign foreignCall arguments -> FcCallForeign foreignCall (map (recur bound) arguments)
    recur = resolveLocal
    resolveLocalBind bound bind =
      case bind of
        FcNonRec var rhs -> FcNonRec var (recur bound rhs)
        FcRec bindings ->
          let recursiveBound = bound <> Set.fromList (map (varUnique . fst) bindings)
           in FcRec [(var, recur recursiveBound rhs) | (var, rhs) <- bindings]
    bindVars bind =
      case bind of
        FcNonRec var _ -> [var]
        FcRec bindings -> map fst bindings

globalBinderName :: FcSymbolOrigin -> Text
globalBinderName origin =
  case origin of
    FcTopLevelOrigin packageName moduleName symbolName ->
      "$g$" <> encode packageName <> "$" <> encode moduleName <> "$" <> encode symbolName
    FcBuiltinOrigin symbolName -> symbolName
  where
    encode = T.concatMap encodeCharacter
    encodeCharacter character
      | isAlphaNum character = T.singleton character
      | otherwise = "_" <> T.pack (showHex (ord character) "") <> "_"

programProviders :: Map FcSymbolOrigin TcType -> FcProgram -> [(FcSymbolOrigin, Var)]
programProviders importedTypes program =
  [ (origin, var)
  | topBind <- fcTopBinds program,
    var <- topBinders topBind <> declarationConstructorVars importedTypes topBind,
    Just origin <- [varResolvedName var]
  ]

declarationConstructorVars :: Map FcSymbolOrigin TcType -> FcTopBind -> [Var]
declarationConstructorVars importedTypes topBind =
  case topBind of
    FcData declaration ->
      [ fcExternalVar origin (Map.findWithDefault (dataConstructorType declaration constructor) origin importedTypes)
      | constructor <- fcDataConstructors declaration,
        let origin = fcConstructorSymbolOrigin (fcDataConOrigin constructor)
      ]
    FcNewtype declaration ->
      [fcExternalVar origin (Map.findWithDefault (newtypeConstructorType declaration) origin importedTypes)]
      where
        origin = fcConstructorSymbolOrigin (fcNewtypeConstructorOrigin declaration)
    _ -> []

dataConstructorType :: FcDataDecl -> FcDataConDecl -> TcType
dataConstructorType declaration constructor =
  foldr TcForAllTy body (universalTyVars <> existentialTyVars)
  where
    universalTyVars = fcDataKindTyVars declaration <> fcDataTyVars declaration
    fields = fcDataConFields constructor
    existentialTyVars = filter (`notElem` universalTyVars) (freeRigidTyVarsOf fields)
    result = fcDataResultType declaration
    body = foldr TcFunTy result fields

newtypeConstructorType :: FcNewtypeDecl -> TcType
newtypeConstructorType declaration =
  foldr TcForAllTy body (fcNewtypeTyVars declaration)
  where
    body = TcFunTy (fcNewtypeRepresentation declaration) (fcNewtypeResult declaration)

-- The type checker does not yet attach an origin to all compiler-generated
-- evidence references. Keep the prior last-definition fallback for those
-- references. Source references with an origin never use this table.
fallbackSourceProviders :: [(FcSymbolOrigin, Var)] -> Map Text Var
fallbackSourceProviders entries =
  Map.fromList [(fcOriginName origin, var) | (origin, var) <- entries]

topBinders :: FcTopBind -> [Var]
topBinders topBind =
  case topBind of
    FcPrimitive var _ -> [var]
    FcTopBind (FcNonRec var _) -> [var]
    FcTopBind (FcRec bindings) -> map fst bindings
    _ -> []

bindersOf :: FcBind -> [Var]
bindersOf bind =
  case bind of
    FcNonRec var _ -> [var]
    FcRec bindings -> map fst bindings

duplicates :: (Ord value) => [value] -> [value]
duplicates values = Map.keys (Map.filter (> (1 :: Int)) (Map.fromListWith (+) [(value, 1 :: Int) | value <- values]))

checkExternalTypes :: Map FcSymbolOrigin Var -> FcProgram -> [FcMergeError]
checkExternalTypes providers program =
  [ FcImportTypeMismatch origin importedType (varType provider)
  | FcExternal origin importedType <- fcTopBinds program,
    Just provider <- [Map.lookup origin providers],
    not (equivalentTypeSchemes (typeSchemeFromType importedType) (typeSchemeFromType (varType provider)))
  ]

keepExternal :: Map FcSymbolOrigin Var -> FcTopBind -> Bool
keepExternal providers topBind =
  case topBind of
    FcExternal origin _ -> Map.notMember origin providers
    _ -> True

resolveTopBind :: Map FcSymbolOrigin Var -> Map Text Var -> FcTopBind -> FcTopBind
resolveTopBind providers sourceProviders topBind =
  case topBind of
    FcTopBind bind -> FcTopBind (resolveBind providers sourceProviders Set.empty bind)
    _ -> topBind

resolveBind :: Map FcSymbolOrigin Var -> Map Text Var -> Set.Set Unique -> FcBind -> FcBind
resolveBind providers sourceProviders bound bind =
  case bind of
    FcNonRec var rhs -> FcNonRec var (resolveExpr providers sourceProviders bound rhs)
    FcRec bindings ->
      let recursiveBound = bound <> Set.fromList (map (varUnique . fst) bindings)
       in FcRec [(var, resolveExpr providers sourceProviders recursiveBound rhs) | (var, rhs) <- bindings]

resolveExpr :: Map FcSymbolOrigin Var -> Map Text Var -> Set.Set Unique -> FcExpr -> FcExpr
resolveExpr providers sourceProviders bound expression =
  case expression of
    FcVar var ->
      FcVar
        ( case varResolvedName var of
            Just origin -> resolveOccurrence var (Map.lookup origin providers)
            Nothing
              | varUnique var `Set.notMember` bound -> resolveOccurrence var (Map.lookup (varName var) sourceProviders)
              | otherwise -> var
        )
    FcLit {} -> expression
    FcApp function argument -> FcApp (recur function) (recur argument)
    FcTyApp function ty -> FcTyApp (recur function) ty
    FcLam var body -> FcLam var (resolveExpr providers sourceProviders (Set.insert (varUnique var) bound) body)
    FcTyLam tyVar body -> FcTyLam tyVar (recur body)
    FcLet bind body ->
      let bodyBound = bound <> Set.fromList (map varUnique (bindersOf bind))
       in FcLet (resolveBind providers sourceProviders bound bind) (resolveExpr providers sourceProviders bodyBound body)
    FcCase scrutinee binder alternatives ->
      FcCase
        (recur scrutinee)
        binder
        [ alternative
            { altRhs =
                resolveExpr
                  providers
                  sourceProviders
                  (bound <> Set.fromList (map varUnique (binder : altBinders alternative)))
                  (altRhs alternative)
            }
        | alternative <- alternatives
        ]
    FcCast body coercion -> FcCast (recur body) coercion
    FcCallForeign foreignCall arguments -> FcCallForeign foreignCall (map recur arguments)
  where
    recur = resolveExpr providers sourceProviders bound

resolveOccurrence :: Var -> Maybe Var -> Var
resolveOccurrence occurrence provider =
  case provider of
    Just resolved -> resolved {varType = varType occurrence}
    Nothing -> occurrence

shiftProgramVars :: Int -> FcProgram -> FcProgram
shiftProgramVars offset program = program {fcTopBinds = map shiftTopBind (fcTopBinds program)}
  where
    shiftVar var = case varUnique var of Unique unique -> var {varUnique = Unique (unique + offset)}
    shiftTopBind topBind =
      case topBind of
        FcPrimitive var arity -> FcPrimitive (shiftVar var) arity
        FcTopBind bind -> FcTopBind (shiftBind bind)
        _ -> topBind
    shiftBind bind =
      case bind of
        FcNonRec var expression -> FcNonRec (shiftVar var) (shiftExpr expression)
        FcRec bindings -> FcRec [(shiftVar var, shiftExpr expression) | (var, expression) <- bindings]
    shiftExpr expression =
      case expression of
        FcVar var -> FcVar (shiftVar var)
        FcLit {} -> expression
        FcApp function argument -> FcApp (shiftExpr function) (shiftExpr argument)
        FcTyApp inner ty -> FcTyApp (shiftExpr inner) ty
        FcLam var body -> FcLam (shiftVar var) (shiftExpr body)
        FcTyLam tyVar body -> FcTyLam tyVar (shiftExpr body)
        FcLet bind body -> FcLet (shiftBind bind) (shiftExpr body)
        FcCase scrutinee binder alternatives -> FcCase (shiftExpr scrutinee) (shiftVar binder) (map shiftAlt alternatives)
        FcCast inner coercion -> FcCast (shiftExpr inner) coercion
        FcCallForeign foreignCall arguments -> FcCallForeign foreignCall (map shiftExpr arguments)
    shiftAlt alternative =
      alternative
        { altBinders = map shiftVar (altBinders alternative),
          altRhs = shiftExpr (altRhs alternative)
        }
