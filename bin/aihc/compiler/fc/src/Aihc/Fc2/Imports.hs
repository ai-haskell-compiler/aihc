-- | Select and check the facts that a System FC 2 program imports.
module Aihc.Fc2.Imports
  ( emptyImports,
    importsForProgram,
    unusedImports,
  )
where

import Aihc.Fc2.Name
import Aihc.Fc2.Syntax
import Aihc.Fc2.TypeOf
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

newtype References = References
  { referenceNames :: Set Name
  }

instance Semigroup References where
  left <> right = References (referenceNames left <> referenceNames right)

instance Monoid References where
  mempty = References Set.empty

emptyImports :: Imports
emptyImports = Imports Map.empty Map.empty Map.empty Map.empty

-- | Select the transitive import closure from the type-check interface.
importsForProgram :: TypeEnv -> Program -> Imports
importsForProgram available program = select Set.empty initialNames (programImports program)
  where
    localProgram = program {programImports = emptyImports}
    localNames = namesInTypeEnv (typeEnvFromProgram localProgram)
    directReferences = foldMap declReferences (programDecls program)
    importedReferences = referencesFromImports (programImports program)
    initialNames =
      ( referenceNames directReferences
          <> referenceNames importedReferences
      )
        `Set.difference` localNames

    select visited pending imports
      | Set.null pending = imports
      | otherwise =
          let (name, rest) = Set.deleteFindMin pending
           in if name `Set.member` visited
                then select visited rest imports
                else
                  let (selected, selectedReferences) = selectName available name imports
                      next =
                        (rest <> referenceNames selectedReferences)
                          `Set.difference` localNames
                          `Set.difference` Set.insert name visited
                   in select (Set.insert name visited) next selected

-- | Return one entry for each import declaration that has no use.
unusedImports :: Program -> [Name]
unusedImports program =
  filter (`Set.notMember` usedNames) importNames
  where
    imports = programImports program
    directReferences = foldMap declReferences (programDecls program)
    importReferences = referencesFromImports imports
    usedNames =
      referenceNames directReferences
        <> referenceNames importReferences
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

selectName :: TypeEnv -> Name -> Imports -> (Imports, References)
selectName available name imports =
  ( imports
      { importHeaders = insertFound (teHeaders available) (importHeaders imports),
        importSynonyms = insertFound (teSynonyms available) (importSynonyms imports),
        importAxioms = insertFound (teAxioms available) (importAxioms imports),
        importBinders = insertFound (teBinders available) (importBinders imports)
      },
    foldMap typeReferences (Map.lookup name (teHeaders available))
      <> foldMap typeReferences (Map.lookup name (teSynonyms available))
      <> foldMap axiomReferences (Map.lookup name (teAxioms available))
      <> foldMap typeReferences (Map.lookup name (teBinders available))
  )
  where
    insertFound availableMap selected =
      case Map.lookup name availableMap of
        Nothing -> selected
        Just value -> Map.insert name value selected

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
deleteReference name references =
  references {referenceNames = Set.delete name (referenceNames references)}

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
nameReference name = References (Set.singleton name)
