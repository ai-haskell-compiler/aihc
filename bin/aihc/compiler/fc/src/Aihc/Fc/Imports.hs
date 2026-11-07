-- | Select and check the facts that a System FC program imports.
module Aihc.Fc.Imports
  ( axiomReferences,
    declReferences,
    exprReferences,
    emptyImports,
    importsForProgramLookup,
    pruneImports,
    referencesFromImports,
    restrictProgram,
    restrictProgramWith,
    typeReferences,
    unusedImports,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf
  ( TypeEnv (..),
    emptyTypeEnv,
    typeEnvFromProgram,
    typeHead,
    unionTypeEnv,
  )
import Aihc.Fc.Wired (unboxedTupleArity, unboxedTupleDecl)
import Aihc.Resolve (PackageId)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

type References = Set Name

emptyImports :: Imports
emptyImports = Imports Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty

-- | Convert each used name when the import closure first needs it.
importsForProgramLookup :: PackageId -> (Name -> Either String (Maybe TypeEnv)) -> Program -> Either String Imports
importsForProgramLookup primPackage lookupFact program = do
  let localProgram = program {programImports = emptyImports}
      localNames = namesInTypeEnv (typeEnvFromProgram primPackage localProgram)
      roots =
        ( foldMap declReferences (programDecls program)
            <> referencesFromImports (programImports program)
        )
          `Set.difference` localNames
  (available, selectedNames) <- closeLookup lookupFact localNames primPackage roots
  let selectedImports = importsForNames available selectedNames
  pure (mergeImports selectedImports (programImports program))

-- | Keep the declarations that the predicate accepts, and make an import
-- of every other declared name that they reach.
--
-- A program that keeps part of its declarations must still name the rest.
-- The module that declares them defines them, and the kept declarations
-- call them, so each one becomes an import, together with the types and
-- the axioms that its header reaches. An import that nothing reaches goes
-- away, as 'pruneImports' drops it.
--
-- The optimizer interface of a module is such a program, and so is the
-- program of a module after the values of an interface are removed again.
restrictProgram :: PackageId -> (Decl -> Bool) -> Program -> Program
restrictProgram primPackage keep program =
  restrictProgramWith (typeEnvFromProgram primPackage program) keep program

-- | 'restrictProgram' with the type environment of the program already
-- known. A caller that restricts one program many times builds that
-- environment one time.
restrictProgramWith :: TypeEnv -> (Decl -> Bool) -> Program -> Program
restrictProgramWith available keep program =
  pruneImports program {programImports = imports, programDecls = kept}
  where
    kept = filter keep (programDecls program)
    declared = Set.fromList (concatMap declaredNames kept)
    roots = foldMap declReferences kept `Set.difference` declared
    needed = closeReferences available declared roots
    imports = mergeImports (programImports program) (importsForNames available needed)

-- | The names that the roots reach, without the names the program
-- declares. The type of an import reaches the types and the axioms that
-- it mentions, so those become imports too.
closeReferences :: TypeEnv -> Set Name -> References -> Set Name
closeReferences available declared = go Set.empty
  where
    go done pending =
      case Set.minView pending of
        Nothing -> done
        Just (name, rest)
          | Set.member name done || Set.member name declared -> go done rest
          | otherwise -> go (Set.insert name done) (rest <> (referencesForName available name `Set.difference` done))

-- | The top-level names that a declaration binds. A rule binds none.
declaredNames :: Decl -> [Name]
declaredNames decl =
  case decl of
    DeclType declaration -> typeName declaration : map conName (typeCons declaration)
    DeclSynonym declaration -> [synName declaration]
    DeclAxiom declaration -> [axiomName declaration]
    DeclVal declaration -> [valName declaration]
    DeclRule {} -> []

closeLookup ::
  (Name -> Either String (Maybe TypeEnv)) ->
  Set Name ->
  PackageId ->
  Set Name ->
  Either String (TypeEnv, Set Name)
closeLookup lookupFact localNames primPackage = go Set.empty Set.empty (emptyTypeEnv primPackage)
  where
    go visited selected env pending =
      case Set.minView pending of
        Nothing -> Right (env, selected)
        Just (name, rest)
          | Set.member name visited -> go visited selected env rest
          | Set.member name localNames -> go (Set.insert name visited) selected env rest
          | nameInTypeEnv env name ->
              go
                (Set.insert name visited)
                (Set.insert name selected)
                env
                (rest <> (referencesForName env name `Set.difference` localNames `Set.difference` visited))
          | otherwise ->
              case lookupFact name of
                Left message -> Left message
                Right Nothing -> go (Set.insert name visited) selected env rest
                Right (Just fragment) ->
                  let env' = unionTypeEnv env fragment
                      newNames = referencesForName env' name `Set.difference` localNames `Set.difference` visited
                   in go (Set.insert name visited) (Set.insert name selected) env' (rest <> newNames)

nameInTypeEnv :: TypeEnv -> Name -> Bool
nameInTypeEnv env name =
  Map.member name (teHeaders env)
    || Map.member name (teSynonyms env)
    || Map.member name (teAxioms env)
    || Map.member name (teBinders env)

referencesForName :: TypeEnv -> Name -> References
referencesForName available name =
  foldMap typeReferences (Map.lookup name (teHeaders available))
    <> foldMap typeReferences (Map.lookup name (teSynonyms available))
    <> foldMap axiomReferences (Map.lookup name (teAxioms available))
    <> foldMap typeReferences (Map.lookup name (teBinders available))
    <> familyEquationNames available name

-- | The equations of a type family. A use of the family imports them, so
-- the applications of the family reduce.
familyEquationNames :: TypeEnv -> Name -> References
familyEquationNames available family =
  Set.fromList
    [ axiomName axiom
    | axiom <- Map.elems (teAxioms available),
      axiomRole axiom == Nominal,
      typeHead (axiomLeft axiom) == Just family
    ]

importsForNames :: TypeEnv -> Set Name -> Imports
importsForNames available names =
  Imports
    { importHeaders = Map.restrictKeys (teHeaders available) names,
      importSynonyms = Map.restrictKeys (teSynonyms available) names,
      importAxioms = Map.restrictKeys (teAxioms available) names,
      importBinders = Map.restrictKeys (teBinders available) names,
      importConRepresentations = Map.restrictKeys (teConRepresentations available) names,
      importConStrictFields = Map.restrictKeys (teConStrictFields available) names,
      importDataCons = Map.restrictKeys (teDataCons available) names
    }

mergeImports :: Imports -> Imports -> Imports
mergeImports preferred fallback =
  Imports
    { importHeaders = Map.union (importHeaders preferred) (importHeaders fallback),
      importSynonyms = Map.union (importSynonyms preferred) (importSynonyms fallback),
      importAxioms = Map.union (importAxioms preferred) (importAxioms fallback),
      importBinders = Map.union (importBinders preferred) (importBinders fallback),
      importConRepresentations = Map.union (importConRepresentations preferred) (importConRepresentations fallback),
      importConStrictFields = Map.union (importConStrictFields preferred) (importConStrictFields fallback),
      importDataCons = Map.union (importDataCons preferred) (importDataCons fallback)
    }

-- | Return one entry for each import declaration that has no use.
unusedImports :: Program -> [Name]
unusedImports program =
  filter (`Set.notMember` usedNames) importNames
  where
    imports = programImports program
    directReferences = foldMap declReferences (programDecls program)
    importReferences = referencesFromImports imports
    referencedNames = directReferences <> importReferences
    usedNames = referencedNames <> familyEquations
    -- An equation of a referenced family is in use through the family.
    familyEquations =
      Set.fromList
        [ name
        | (name, axiom) <- Map.toList (importAxioms imports),
          axiomRole axiom == Nominal,
          Just family <- [typeHead (axiomLeft axiom)],
          Set.member family referencedNames
        ]
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
entryTypeReferences name = Set.delete name . typeReferences

entryAxiomReferences :: Name -> AxiomDecl -> References
entryAxiomReferences name = Set.delete name . axiomReferences

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
    DeclRule declaration ->
      foldMap binderReferences (ruleTypeBinders declaration <> ruleBinders declaration)
        <> typeReferences (ruleType declaration)
        <> exprReferences (ruleLhs declaration)
        <> exprReferences (ruleRhs declaration)

axiomReferences :: AxiomDecl -> References
axiomReferences declaration = axiomRefs Set.empty declaration

binderReferences :: Binder -> References
binderReferences = typeReferences . binderType

typeReferences :: Type -> References
typeReferences = typeRefs Set.empty

exprReferences :: Expr -> References
exprReferences = exprRefs Set.empty

-- The functions below add the references to a set. Each name goes into
-- the set one time, without a set for each part of the term.

axiomRefs :: References -> AxiomDecl -> References
axiomRefs refs declaration =
  typeRefs (typeRefs (foldl' binderRefs refs (axiomBinders declaration)) (axiomLeft declaration)) (axiomRight declaration)

binderRefs :: References -> Binder -> References
binderRefs refs = typeRefs refs . binderType

typeRefs :: References -> Type -> References
typeRefs !refs ty =
  case ty of
    TyVar name -> Set.insert name refs
    TyCon name -> Set.insert name refs
    TyApp function argument -> typeRefs (typeRefs refs function) argument
    TyFun r1 r2 argument result -> typeRefs (typeRefs (typeRefs (typeRefs refs r1) r2) argument) result
    TyForAll binder body -> typeRefs (binderRefs refs binder) body
    TyEq left right -> typeRefs (typeRefs refs left) right
    TyLit kindName _ -> Set.insert kindName refs

exprRefs :: References -> Expr -> References
exprRefs !refs expr =
  case expr of
    ExVar name -> Set.insert name refs
    ExLit literal ty -> typeRefs (literalRefs refs literal) ty
    ExApp function argument -> exprRefs (exprRefs refs function) argument
    ExTyApp function argument -> typeRefs (exprRefs refs function) argument
    ExLam binder body -> exprRefs (binderRefs refs binder) body
    ExTyLam binder body -> exprRefs (binderRefs refs binder) body
    ExLet binding body -> exprRefs (bindRefs refs binding) body
    ExRec bindings body -> exprRefs (foldl' bindRefs refs bindings) body
    ExCase scrutinee binder alts ->
      foldl' altRefs (foldl' binderRefs (exprRefs refs scrutinee) binder) alts
    ExCoercion proof -> coercionRefs refs proof
    ExAbsurd scrutinee resultType -> typeRefs (exprRefs refs scrutinee) resultType
    ExCast body coercion -> coercionRefs (exprRefs refs body) coercion
    ExForeignCall call types arguments ->
      foldl' exprRefs (foldl' typeRefs (foreignCallRefs refs call) types) arguments

-- A foreign call carries its own type, so the imported name of the
-- foreign import is not a reference. Its type and its dependencies are.
foreignCallRefs :: References -> ForeignCall -> References
foreignCallRefs refs call =
  foldl' dependencyRefs (typeRefs refs (foreignCallType call)) (foreignCallDependencies call)
  where
    dependencyRefs current dependency =
      case dependency of
        ForeignAxiom name -> Set.insert name current
        ForeignConstructor name -> Set.insert name current

bindRefs :: References -> Bind -> References
bindRefs refs binding = exprRefs (binderRefs refs (bindBinder binding)) (bindRhs binding)

altRefs :: References -> Alt -> References
altRefs refs alternative =
  exprRefs
    ( foldl'
        binderRefs
        (foldl' binderRefs (altConRefs refs (altCon alternative)) (altTypeBinders alternative))
        (altBinders alternative)
    )
    (altRhs alternative)

altConRefs :: References -> AltCon -> References
altConRefs refs altCon =
  case altCon of
    AltData name -> Set.insert name refs
    AltLit literal -> literalRefs refs literal
    AltDefault -> refs

literalRefs :: References -> Literal -> References
literalRefs refs literal =
  case literal of
    LitInt representation _ -> typeRefs refs representation
    LitChar representation _ -> typeRefs refs representation
    LitAddr representation _ -> typeRefs refs representation

coercionRefs :: References -> Coercion -> References
coercionRefs !refs coercion =
  case coercion of
    CoVar name -> Set.insert name refs
    CoRefl ty -> typeRefs refs ty
    CoSym inner -> coercionRefs refs inner
    CoTrans left right -> coercionRefs (coercionRefs refs left) right
    CoApp function argument -> coercionRefs (coercionRefs refs function) argument
    CoNth _ proof -> coercionRefs refs proof
    CoFun domain range -> coercionRefs (coercionRefs refs domain) range
    CoForAll binder body -> coercionRefs (binderRefs refs binder) body
    CoTyConApp name arguments -> foldl' coercionRefs (Set.insert name refs) arguments
    CoAxiom name arguments -> foldl' typeRefs (Set.insert name refs) arguments

-- | Keep only the imports that the declarations reach. Normalization and
-- inlining drop bindings, and with them the last use of an import.
--
-- A pass can also make an unboxed tuple that the program did not use
-- before. Such a tuple gets its imports here, from 'unboxedTupleDecl'.
pruneImports :: Program -> Program
pruneImports program = program {programImports = imports'}
  where
    direct = foldMap declReferences (programDecls program)
    declared = Set.fromList (concatMap declaredNames (programDecls program))
    newTuples =
      Set.fromList
        [ found
        | name <- Set.toList direct,
          Map.notMember name (importHeaders (programImports program)),
          Set.notMember name declared,
          Just found <- [unboxedTupleArity name]
        ]
    imports = foldr (mergeImports . tupleImports) (programImports program) (Set.toList newTuples)
    tupleImports (package, arity) =
      let declaration = unboxedTupleDecl package arity
          env = typeEnvFromProgram package (Program emptyImports [DeclType declaration])
       in importsForNames env (Set.fromList (typeName declaration : map conName (typeCons declaration)))
    used = close Set.empty (Set.toList direct)
    -- Each name comes in one time, and brings the references of its own
    -- imports. A nominal equation comes in with a name of its left side.
    close done pending =
      case pending of
        [] -> done
        name : rest
          | Set.member name done -> close done rest
          | otherwise -> close (Set.insert name done) (Set.toList (referencesOf name) <> rest)
    referencesOf name =
      foldMap typeReferences (Map.lookup name (importHeaders imports))
        <> foldMap typeReferences (Map.lookup name (importSynonyms imports))
        <> foldMap typeReferences (Map.lookup name (importBinders imports))
        <> foldMap axiomReferences (Map.lookup name (importAxioms imports))
        <> foldMap axiomReferences (Map.findWithDefault [] name nominalEquations)
    -- The nominal equations, by each name of their left side.
    nominalEquations =
      Map.fromListWith
        (<>)
        [ (reference, [axiom])
        | axiom <- Map.elems (importAxioms imports),
          axiomRole axiom == Nominal,
          reference <- Set.toList (typeReferences (axiomLeft axiom))
        ]
    -- An equation of a family that the program uses stays with the family.
    keepAxiom current name axiom =
      Set.member name current
        || (axiomRole axiom == Nominal && not (Set.disjoint current (typeReferences (axiomLeft axiom))))
    imports' =
      Imports
        { importHeaders = Map.filterWithKey (\name _ -> Set.member name used) (importHeaders imports),
          importSynonyms = Map.filterWithKey (\name _ -> Set.member name used) (importSynonyms imports),
          importAxioms = Map.filterWithKey (keepAxiom used) (importAxioms imports),
          importBinders = Map.filterWithKey (\name _ -> Set.member name used) (importBinders imports),
          importConRepresentations = Map.restrictKeys (importConRepresentations imports) used,
          importConStrictFields = Map.restrictKeys (importConStrictFields imports) used,
          importDataCons = Map.restrictKeys (importDataCons imports) used
        }
