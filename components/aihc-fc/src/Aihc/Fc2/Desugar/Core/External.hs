-- | Materialize external term declarations in a System FC program.
module Aihc.Fc2.Desugar.Core.External
  ( declareExternalSymbols,
  )
where

import Aihc.Fc2.Desugar.Core.Syntax
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text (Text)

declareExternalSymbols :: CoreProgram -> CoreProgram
declareExternalSymbols program =
  CoreProgram (coreProgramModule program) (map (normalizeExternalTopBind externalVars) canonicalBinds)
  where
    canonicalBinds = canonicalProgramBinds program
    externalVars = Map.fromList [(origin, coreExternalVar origin ty) | CoreExternal origin ty <- canonicalBinds]

normalizeExternalTopBind :: Map.Map CoreSymbolOrigin Var -> CoreTopBind -> CoreTopBind
normalizeExternalTopBind externalVars topBind =
  case topBind of
    CoreTopBind bind -> CoreTopBind (normalizeExternalBind externalVars bind)
    _ -> topBind

normalizeExternalBind :: Map.Map CoreSymbolOrigin Var -> CoreBind -> CoreBind
normalizeExternalBind externalVars bind =
  case bind of
    CoreNonRec var rhs -> CoreNonRec var (normalizeExternalExpr externalVars rhs)
    CoreRec bindings -> CoreRec [(var, normalizeExternalExpr externalVars rhs) | (var, rhs) <- bindings]

normalizeExternalExpr :: Map.Map CoreSymbolOrigin Var -> CoreExpr -> CoreExpr
normalizeExternalExpr externalVars expression =
  case expression of
    CoreVar var ->
      CoreVar (fromMaybe var (varResolvedName var >>= (`Map.lookup` externalVars)))
    CoreLit {} -> expression
    CoreApp function argument -> CoreApp (recur function) (recur argument)
    CoreTyApp function ty -> CoreTyApp (recur function) ty
    CoreLam var body -> CoreLam var (recur body)
    CoreTyLam tyVar body -> CoreTyLam tyVar (recur body)
    CoreLet bind body -> CoreLet (normalizeExternalBind externalVars bind) (recur body)
    CoreCase scrutinee binder alternatives -> CoreCase (recur scrutinee) binder (map normalizeAlternative alternatives)
    CoreCast body coercion -> CoreCast (recur body) coercion
    CoreCallForeign foreignCall arguments -> CoreCallForeign foreignCall (map recur arguments)
  where
    recur = normalizeExternalExpr externalVars
    normalizeAlternative alternative = alternative {altRhs = recur (altRhs alternative)}

canonicalProgramBinds :: CoreProgram -> [CoreTopBind]
canonicalProgramBinds (CoreProgram moduleId topBinds) =
  [CoreExternal origin ty | (origin, ty) <- Map.toAscList externalTypes, not (originIsLocal origin)]
    <> definitions
  where
    definitions = [topBind | topBind <- topBinds, not (isHeader topBind)]
    moduleOrigin = Just (coreModulePackageText moduleId, coreModuleName moduleId)
    declaredTypes = Map.fromList [(origin, ty) | CoreExternal origin ty <- topBinds]
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
        | CorePrimitive var _ <- definitions,
          Just origin@CoreBuiltinOrigin {} <- [varResolvedName var]
        ]
    originIsLocal origin@(CoreTopLevelOrigin packageName moduleName _) = origin `Set.member` localOrigins || Just (packageName, moduleName) == moduleOrigin
    originIsLocal origin@CoreBuiltinOrigin {} = origin `Set.member` localBuiltinOrigins
    isHeader CoreExternal {} = True
    isHeader _ = False

topBindDefinedNames :: CoreTopBind -> [Text]
topBindDefinedNames topBind =
  case topBind of
    CoreExternal {} -> []
    CoreData declaration -> map coreDataConName (coreDataConstructors declaration)
    CoreAxiom {} -> []
    CoreNewtype declaration -> [coreNewtypeConstructor declaration]
    CorePrimitive var _ -> [varName var]
    CoreForeignImport {} -> []
    CoreTopBind bind -> map varName (bindersOf bind)

topBindDefinedOrigins :: CoreTopBind -> [CoreSymbolOrigin]
topBindDefinedOrigins topBind =
  case topBind of
    CoreData declaration -> coreDataOrigin declaration : map (coreConstructorSymbolOrigin . coreDataConOrigin) (coreDataConstructors declaration)
    CoreNewtype declaration -> [coreNewtypeOrigin declaration, coreConstructorSymbolOrigin (coreNewtypeConstructorOrigin declaration)]
    _ -> []

topBindDefinedVars :: CoreTopBind -> [Var]
topBindDefinedVars topBind =
  case topBind of
    CorePrimitive var _ -> [var]
    CoreTopBind bind -> bindersOf bind
    _ -> []

topBindOccurrences :: CoreTopBind -> [Var]
topBindOccurrences topBind =
  case topBind of
    CoreTopBind bind -> bindOccurrences bind
    _ -> []

bindOccurrences :: CoreBind -> [Var]
bindOccurrences bind =
  case bind of
    CoreNonRec _ rhs -> expressionOccurrences rhs
    CoreRec bindings -> concatMap (expressionOccurrences . snd) bindings

bindersOf :: CoreBind -> [Var]
bindersOf bind =
  case bind of
    CoreNonRec var _ -> [var]
    CoreRec bindings -> map fst bindings

expressionOccurrences :: CoreExpr -> [Var]
expressionOccurrences expression =
  case expression of
    CoreVar var -> [var]
    CoreLit {} -> []
    CoreApp function argument -> expressionOccurrences function <> expressionOccurrences argument
    CoreTyApp function _ -> expressionOccurrences function
    CoreLam _ body -> expressionOccurrences body
    CoreTyLam _ body -> expressionOccurrences body
    CoreLet bind body -> bindOccurrences bind <> expressionOccurrences body
    CoreCase scrutinee _ alternatives -> expressionOccurrences scrutinee <> concatMap (expressionOccurrences . altRhs) alternatives
    CoreCast body _ -> expressionOccurrences body
    CoreCallForeign _ arguments -> concatMap expressionOccurrences arguments
