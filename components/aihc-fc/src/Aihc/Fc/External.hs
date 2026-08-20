-- | Materialize external term declarations in a System FC program.
module Aihc.Fc.External
  ( declareExternalSymbols,
  )
where

import Aihc.Fc.Syntax
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)

declareExternalSymbols :: FcProgram -> FcProgram
declareExternalSymbols program =
  FcProgram (fcProgramModule program) (map (normalizeExternalTopBind externalVars) canonicalBinds)
  where
    canonicalBinds = canonicalProgramBinds program
    externalVars = Map.fromList [(origin, fcExternalVar origin ty) | FcExternal origin ty <- canonicalBinds]

normalizeExternalTopBind :: Map.Map FcSymbolOrigin Var -> FcTopBind -> FcTopBind
normalizeExternalTopBind externalVars topBind =
  case topBind of
    FcTopBind bind -> FcTopBind (normalizeExternalBind externalVars bind)
    _ -> topBind

normalizeExternalBind :: Map.Map FcSymbolOrigin Var -> FcBind -> FcBind
normalizeExternalBind externalVars bind =
  case bind of
    FcNonRec var rhs -> FcNonRec var (normalizeExternalExpr externalVars rhs)
    FcRec bindings -> FcRec [(var, normalizeExternalExpr externalVars rhs) | (var, rhs) <- bindings]

normalizeExternalExpr :: Map.Map FcSymbolOrigin Var -> FcExpr -> FcExpr
normalizeExternalExpr externalVars expression =
  case expression of
    FcVar var ->
      FcVar (fromMaybe var (varResolvedName var >>= (`Map.lookup` externalVars)))
    FcLit {} -> expression
    FcApp function argument -> FcApp (recur function) (recur argument)
    FcTyApp function ty -> FcTyApp (recur function) ty
    FcLam var body -> FcLam var (recur body)
    FcTyLam tyVar body -> FcTyLam tyVar (recur body)
    FcLet bind body -> FcLet (normalizeExternalBind externalVars bind) (recur body)
    FcCase scrutinee binder alternatives -> FcCase (recur scrutinee) binder (map normalizeAlternative alternatives)
    FcCast body coercion -> FcCast (recur body) coercion
    FcCallForeign foreignCall arguments -> FcCallForeign foreignCall (map recur arguments)
  where
    recur = normalizeExternalExpr externalVars
    normalizeAlternative alternative = alternative {altRhs = recur (altRhs alternative)}

canonicalProgramBinds :: FcProgram -> [FcTopBind]
canonicalProgramBinds (FcProgram moduleId topBinds) =
  [FcExternal origin ty | (origin, ty) <- Map.toAscList externalTypes, not (originIsLocal origin)]
    <> definitions
  where
    definitions = [topBind | topBind <- topBinds, not (isHeader topBind)]
    moduleOrigin = Just (fcModulePackageText moduleId, fcModuleName moduleId)
    declaredTypes = Map.fromList [(origin, ty) | FcExternal origin ty <- topBinds]
    referencedTypes = Map.fromList [(origin, varType var) | var <- concatMap topBindOccurrences definitions, Just origin <- [varResolvedName var]]
    externalTypes = Map.union declaredTypes referencedTypes
    definedNames = Set.fromList (concatMap topBindDefinedNames definitions)
    localOrigins =
      Set.fromList [origin | topBind <- definitions, var <- topBindDefinedVars topBind, Just origin <- [varResolvedName var]]
        <> Set.fromList (concatMap topBindDefinedOrigins definitions)
        <> Set.fromList
          [ origin
          | topBind <- definitions,
            var <- topBindOccurrences topBind,
            varName var `Set.member` definedNames,
            Just origin <- [varResolvedName var]
          ]
    localBuiltinOrigins =
      Set.fromList
        [ origin
        | FcPrimitive var _ <- definitions,
          Just origin@FcBuiltinOrigin {} <- [varResolvedName var]
        ]
    originIsLocal origin@(FcTopLevelOrigin packageName moduleName _) = origin `Set.member` localOrigins || Just (packageName, moduleName) == moduleOrigin
    originIsLocal origin@FcBuiltinOrigin {} = origin `Set.member` localBuiltinOrigins
    isHeader FcExternal {} = True
    isHeader _ = False

topBindDefinedNames :: FcTopBind -> [Text]
topBindDefinedNames topBind =
  case topBind of
    FcExternal {} -> []
    FcData declaration -> map fcDataConName (fcDataConstructors declaration)
    FcAxiom {} -> []
    FcNewtype declaration -> [fcNewtypeConstructor declaration]
    FcPrimitive var _ -> [varName var]
    FcForeignImport {} -> []
    FcTopBind bind -> map varName (bindersOf bind)

topBindDefinedOrigins :: FcTopBind -> [FcSymbolOrigin]
topBindDefinedOrigins topBind =
  case topBind of
    FcData declaration -> fcDataOrigin declaration : map (fcConstructorSymbolOrigin . fcDataConOrigin) (fcDataConstructors declaration)
    FcNewtype declaration -> [fcNewtypeOrigin declaration, fcConstructorSymbolOrigin (fcNewtypeConstructorOrigin declaration)]
    _ -> []

topBindDefinedVars :: FcTopBind -> [Var]
topBindDefinedVars topBind =
  case topBind of
    FcPrimitive var _ -> [var]
    FcTopBind bind -> bindersOf bind
    _ -> []

topBindOccurrences :: FcTopBind -> [Var]
topBindOccurrences topBind =
  case topBind of
    FcTopBind bind -> bindOccurrences bind
    _ -> []

bindOccurrences :: FcBind -> [Var]
bindOccurrences bind =
  case bind of
    FcNonRec _ rhs -> expressionOccurrences rhs
    FcRec bindings -> concatMap (expressionOccurrences . snd) bindings

bindersOf :: FcBind -> [Var]
bindersOf bind =
  case bind of
    FcNonRec var _ -> [var]
    FcRec bindings -> map fst bindings

expressionOccurrences :: FcExpr -> [Var]
expressionOccurrences expression =
  case expression of
    FcVar var -> [var]
    FcLit {} -> []
    FcApp function argument -> expressionOccurrences function <> expressionOccurrences argument
    FcTyApp function _ -> expressionOccurrences function
    FcLam _ body -> expressionOccurrences body
    FcTyLam _ body -> expressionOccurrences body
    FcLet bind body -> bindOccurrences bind <> expressionOccurrences body
    FcCase scrutinee _ alternatives -> expressionOccurrences scrutinee <> concatMap (expressionOccurrences . altRhs) alternatives
    FcCast body _ -> expressionOccurrences body
    FcCallForeign _ arguments -> concatMap expressionOccurrences arguments
