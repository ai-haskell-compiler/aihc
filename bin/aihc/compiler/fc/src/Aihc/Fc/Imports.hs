-- | Select and check the facts that a System FC program imports.
module Aihc.Fc.Imports
  ( emptyImports,
    importsForProgram,
    importsForProgramPrepared,
    mergePreparedImports,
    prepareImports,
    PreparedImports,
    unusedImports,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

type References = Set Name

data PreparedImports = PreparedImports
  { preparedAvailable :: !TypeEnv,
    preparedAvailableNames :: !(Set Name)
  }

emptyImports :: Imports
emptyImports = Imports Map.empty Map.empty Map.empty Map.empty

-- | Select the transitive import closure from the type-check interface.
importsForProgram :: TypeEnv -> Program -> Imports
importsForProgram available = importsForProgramPrepared (prepareImports available)

prepareImports :: TypeEnv -> PreparedImports
prepareImports available = PreparedImports available (namesInTypeEnv available)

mergePreparedImports :: [PreparedImports] -> PreparedImports
mergePreparedImports prepared =
  PreparedImports
    { preparedAvailable = mergeTypeEnvs (map preparedAvailable prepared),
      preparedAvailableNames = Set.unions (map preparedAvailableNames prepared)
    }

mergeTypeEnvs :: [TypeEnv] -> TypeEnv
mergeTypeEnvs [] = error "cannot merge an empty list of type environments"
mergeTypeEnvs environments@(first : _) =
  TypeEnv
    { tePrimPackage = tePrimPackage first,
      teHeaders = Map.unions (map teHeaders environments),
      teSynonyms = Map.unions (map teSynonyms environments),
      teAxioms = Map.unions (map teAxioms environments),
      teBinders = Map.unions (map teBinders environments)
    }

importsForProgramPrepared :: PreparedImports -> Program -> Imports
importsForProgramPrepared prepared program = mergeImports selectedImports existingImports
  where
    available = preparedAvailable prepared
    existingImports = programImports program
    localProgram = program {programImports = emptyImports}
    localNames = namesInTypeEnv (typeEnvFromProgram (tePrimPackage available) localProgram)
    roots =
      ( foldMap declReferences (programDecls program)
          <> referencesFromImports existingImports
      )
        `Set.difference` localNames
    selectedNames = reachableNames available localNames (roots `Set.intersection` preparedAvailableNames prepared)
    selectedImports = importsForNames available selectedNames

reachableNames :: TypeEnv -> Set Name -> Set Name -> Set Name
reachableNames available localNames = go Set.empty
  where
    go visited pending =
      case Set.minView pending of
        Nothing -> visited
        Just (name, rest) ->
          let visited' = Set.insert name visited
              newNames =
                referencesForName available name
                  `Set.difference` localNames
                  `Set.difference` visited'
           in go visited' (rest <> newNames)

referencesForName :: TypeEnv -> Name -> References
referencesForName available name =
  foldMap typeReferences (Map.lookup name (teHeaders available))
    <> foldMap typeReferences (Map.lookup name (teSynonyms available))
    <> foldMap axiomReferences (Map.lookup name (teAxioms available))
    <> foldMap typeReferences (Map.lookup name (teBinders available))

importsForNames :: TypeEnv -> Set Name -> Imports
importsForNames available names =
  Imports
    { importHeaders = Map.restrictKeys (teHeaders available) names,
      importSynonyms = Map.restrictKeys (teSynonyms available) names,
      importAxioms = Map.restrictKeys (teAxioms available) names,
      importBinders = Map.restrictKeys (teBinders available) names
    }

mergeImports :: Imports -> Imports -> Imports
mergeImports preferred fallback =
  Imports
    { importHeaders = Map.union (importHeaders preferred) (importHeaders fallback),
      importSynonyms = Map.union (importSynonyms preferred) (importSynonyms fallback),
      importAxioms = Map.union (importAxioms preferred) (importAxioms fallback),
      importBinders = Map.union (importBinders preferred) (importBinders fallback)
    }

-- | Return one entry for each import declaration that has no use.
unusedImports :: Program -> [Name]
unusedImports program =
  filter (`Set.notMember` usedNames) importNames
  where
    imports = programImports program
    directReferences = foldMap declReferences (programDecls program)
    importReferences = referencesFromImports imports
    usedNames = directReferences <> importReferences
    importNames =
      Map.keys (importHeaders imports)
        <> Map.keys (importSynonyms imports)
        <> Map.keys (importAxioms imports)
        <> Map.keys (importBinders imports)

namesInTypeEnv :: TypeEnv -> Set Name
namesInTypeEnv env =
  Map.keysSet (teHeaders env)
    <> Map.keysSet (teSynonyms env)
    <> Map.keysSet (teAxioms env)
    <> Map.keysSet (teBinders env)

referencesFromImports :: Imports -> References
referencesFromImports imports =
  foldMap (uncurry entryTypeReferences) (Map.toList (importHeaders imports))
    <> foldMap (uncurry entryTypeReferences) (Map.toList (importSynonyms imports))
    <> foldMap (uncurry entryAxiomReferences) (Map.toList (importAxioms imports))
    <> foldMap (uncurry entryTypeReferences) (Map.toList (importBinders imports))

entryTypeReferences :: Name -> Type -> References
entryTypeReferences name = deleteReference name . typeReferences

entryAxiomReferences :: Name -> AxiomDecl -> References
entryAxiomReferences name = deleteReference name . axiomReferences

deleteReference :: Name -> References -> References
deleteReference = Set.delete

declReferences :: Decl -> References
declReferences decl =
  case decl of
    DeclType declaration ->
      foldMap binderReferences (typeBinders declaration)
        <> typeReferences (typeResult declaration)
        <> foldMap (typeReferences . conType) (typeCons declaration)
    DeclSynonym declaration ->
      foldMap binderReferences (synBinders declaration)
        <> typeReferences (synResult declaration)
        <> typeReferences (synBody declaration)
    DeclAxiom declaration -> axiomReferences declaration
    DeclVal declaration -> typeReferences (valType declaration) <> exprReferences (valBody declaration)
    DeclForeignImport declaration ->
      typeReferences (foreignImportType declaration)
        <> foldMap foreignImportDependencyReferences (foreignImportDependencies declaration)

foreignImportDependencyReferences :: ForeignImportDependency -> References
foreignImportDependencyReferences dependency =
  case dependency of
    ForeignAxiom name -> nameReference name
    ForeignConstructor name -> nameReference name

axiomReferences :: AxiomDecl -> References
axiomReferences declaration =
  foldMap binderReferences (axiomBinders declaration)
    <> typeReferences (axiomLeft declaration)
    <> typeReferences (axiomRight declaration)

binderReferences :: Binder -> References
binderReferences = typeReferences . binderType

typeReferences :: Type -> References
typeReferences ty =
  case ty of
    TyVar name -> nameReference name
    TyCon name -> nameReference name
    TyApp function argument -> typeReferences function <> typeReferences argument
    TyFun r1 r2 argument result -> foldMap typeReferences [r1, r2, argument, result]
    TyForAll binder body -> binderReferences binder <> typeReferences body
    TyEq left right -> typeReferences left <> typeReferences right

exprReferences :: Expr -> References
exprReferences expr =
  case expr of
    ExVar name -> nameReference name
    ExLit literal -> literalReferences literal
    ExApp function argument -> exprReferences function <> exprReferences argument
    ExTyApp function argument -> exprReferences function <> typeReferences argument
    ExLam binder body -> binderReferences binder <> exprReferences body
    ExTyLam binder body -> binderReferences binder <> exprReferences body
    ExLet binding body -> bindReferences binding <> exprReferences body
    ExRec bindings body -> foldMap bindReferences bindings <> exprReferences body
    ExCase scrutinee binder result alts ->
      exprReferences scrutinee
        <> binderReferences binder
        <> typeReferences result
        <> foldMap altReferences alts
    ExCast body coercion -> exprReferences body <> coercionReferences coercion

bindReferences :: Bind -> References
bindReferences binding = binderReferences (bindBinder binding) <> exprReferences (bindRhs binding)

altReferences :: Alt -> References
altReferences alternative =
  altConReferences (altCon alternative)
    <> foldMap binderReferences (altTypeBinders alternative)
    <> foldMap binderReferences (altBinders alternative)
    <> exprReferences (altRhs alternative)

altConReferences :: AltCon -> References
altConReferences altCon =
  case altCon of
    AltData name -> nameReference name
    AltLit literal -> literalReferences literal
    AltDefault -> mempty

literalReferences :: Literal -> References
literalReferences literal =
  case literal of
    LitInt representation _ -> typeReferences representation
    LitChar representation _ -> typeReferences representation
    LitAddr representation _ -> typeReferences representation

coercionReferences :: Coercion -> References
coercionReferences coercion =
  case coercion of
    CoVar name -> nameReference name
    CoRefl ty -> typeReferences ty
    CoSym inner -> coercionReferences inner
    CoTrans left right -> coercionReferences left <> coercionReferences right
    CoTyConApp name arguments -> nameReference name <> foldMap coercionReferences arguments
    CoAxiom name arguments -> nameReference name <> foldMap typeReferences arguments

nameReference :: Name -> References
nameReference = Set.singleton
