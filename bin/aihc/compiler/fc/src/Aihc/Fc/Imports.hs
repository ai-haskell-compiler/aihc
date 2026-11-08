-- | Select and check the facts that a System FC program imports.
module Aihc.Fc.Imports
  ( addTupleImports,
    axiomReferences,
    declReferences,
    declTopReferences,
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
    roots = foldl' (declRefs topWalks) Set.empty kept `Set.difference` declared
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
declReferences = declRefs allWalks Set.empty

-- | The top-level names that a declaration names. These are the names
-- that can be imports.
declTopReferences :: Decl -> References
declTopReferences = declRefs topWalks Set.empty

declRefs :: Walks -> References -> Decl -> References
declRefs walks refs decl =
  case decl of
    DeclType declaration ->
      foldl' (walkType walks) (foldl' (walkBinder walks) refs (typeBinders declaration)) (typeResult declaration : map conType (typeCons declaration))
    DeclSynonym declaration ->
      walkType walks (walkType walks (foldl' (walkBinder walks) refs (synBinders declaration)) (synResult declaration)) (synBody declaration)
    DeclAxiom declaration -> walkAxiom walks refs declaration
    DeclVal declaration -> walkExpr walks (walkType walks refs (valType declaration)) (valBody declaration)
    DeclRule declaration ->
      walkExpr
        walks
        (walkExpr walks (walkType walks (foldl' (walkBinder walks) refs (ruleTypeBinders declaration <> ruleBinders declaration)) (ruleType declaration)) (ruleLhs declaration))
        (ruleRhs declaration)

axiomReferences :: AxiomDecl -> References
axiomReferences = walkAxiom allWalks Set.empty

typeReferences :: Type -> References
typeReferences = walkType allWalks Set.empty

exprReferences :: Expr -> References
exprReferences = walkExpr allWalks Set.empty

-- The functions below add the references to a set. Each name goes into
-- the set one time, without a set for each part of the term.

-- | The walks over the parts of a program, with the function that adds
-- one name to the set.
data Walks = Walks
  { walkAxiom :: References -> AxiomDecl -> References,
    walkBinder :: References -> Binder -> References,
    walkType :: References -> Type -> References,
    walkExpr :: References -> Expr -> References
  }

-- Each use of 'walksWith' gets its own copy of the walks, with the
-- function inlined.
{-# INLINE walksWith #-}
walksWith :: (Name -> References -> References) -> Walks
walksWith insert = Walks axiomRefs binderRefs typeRefs exprRefs
  where
    axiomRefs refs declaration =
      typeRefs (typeRefs (foldl' binderRefs refs (axiomBinders declaration)) (axiomLeft declaration)) (axiomRight declaration)

    binderRefs refs = typeRefs refs . binderType

    typeRefs !refs ty =
      case ty of
        TyVar name -> insert name refs
        TyCon name -> insert name refs
        TyApp function argument -> typeRefs (typeRefs refs function) argument
        TyFun r1 r2 argument result -> typeRefs (typeRefs (typeRefs (typeRefs refs r1) r2) argument) result
        TyForAll binder body -> typeRefs (binderRefs refs binder) body
        TyEq left right -> typeRefs (typeRefs refs left) right
        TyLit kindName _ -> insert kindName refs

    exprRefs !refs expr =
      case expr of
        ExVar name -> insert name refs
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
    foreignCallRefs refs call =
      foldl' dependencyRefs (typeRefs refs (foreignCallType call)) (foreignCallDependencies call)
      where
        dependencyRefs current dependency =
          case dependency of
            ForeignAxiom name -> insert name current
            ForeignConstructor name -> insert name current

    bindRefs refs binding = exprRefs (binderRefs refs (bindBinder binding)) (bindRhs binding)

    altRefs refs alternative =
      exprRefs
        ( foldl'
            binderRefs
            (foldl' binderRefs (altConRefs refs (altCon alternative)) (altTypeBinders alternative))
            (altBinders alternative)
        )
        (altRhs alternative)

    altConRefs refs altCon =
      case altCon of
        AltData name -> insert name refs
        AltLit literal -> literalRefs refs literal
        AltDefault -> refs

    literalRefs refs literal =
      case literal of
        LitInt representation _ -> typeRefs refs representation
        LitChar representation _ -> typeRefs refs representation
        LitAddr representation _ -> typeRefs refs representation

    coercionRefs !refs coercion =
      case coercion of
        CoVar name -> insert name refs
        CoRefl ty -> typeRefs refs ty
        CoSym inner -> coercionRefs refs inner
        CoTrans left right -> coercionRefs (coercionRefs refs left) right
        CoApp function argument -> coercionRefs (coercionRefs refs function) argument
        CoNth _ proof -> coercionRefs refs proof
        CoFun domain range -> coercionRefs (coercionRefs refs domain) range
        CoForAll binder body -> coercionRefs (binderRefs refs binder) body
        CoTyConApp name arguments -> foldl' coercionRefs (insert name refs) arguments
        CoAxiom name arguments -> foldl' typeRefs (insert name refs) arguments

-- | Every name: the top-level names and the local ones.
allWalks :: Walks
allWalks = walksWith insertNew

-- | Add a name to a set. A name that the set holds stays: an insert of an
-- equal name would copy the path to it.
insertNew :: Name -> References -> References
insertNew name refs
  | Set.member name refs = refs
  | otherwise = Set.insert name refs

-- | The top-level names only. A local name is never an import, so a walk
-- that looks for imports does not put the local names in its set.
topWalks :: Walks
topWalks = walksWith insertTop
  where
    insertTop name refs =
      case nameOrigin name of
        OriginLocal {} -> refs
        OriginTop {} -> insertNew name refs

-- | Add the imports of each unboxed tuple that the declarations name and
-- that the program neither imports nor declares. A pass can make an
-- unboxed tuple that the program did not use before, and the passes that
-- follow need its type.
--
-- The passes call this, and not 'pruneImports'. An import that no
-- declaration reaches any more does no harm to a pass, and a pass never
-- needs an import that 'pruneImports' would add: it adds the tuples only.
-- Thus the plan prunes one time, after its last pass. This walk puts only
-- the names of unboxed tuples in its set, so it costs much less than the
-- walk of 'pruneImports'.
addTupleImports :: Program -> Program
addTupleImports program
  | null newTuples = program
  | otherwise = program {programImports = foldr (mergeImports . tupleImportsFor) (programImports program) newTuples}
  where
    tuples = foldl' (declRefs tupleWalks) Set.empty (programDecls program)
    imported = importHeaders (programImports program)
    candidates = [name | name <- Set.toList tuples, Map.notMember name imported]
    declared = Set.fromList (concatMap declaredNames (programDecls program))
    newTuples =
      if null candidates
        then []
        else Set.toList (Set.fromList [found | name <- candidates, Set.notMember name declared, Just found <- [unboxedTupleArity name]])
    tupleWalks = walksWith insertTuple
    insertTuple name refs =
      case nameSort name of
        SortTypeConstructor | isTuple name -> Set.insert name refs
        SortDataConstructor | isTuple name -> Set.insert name refs
        _ -> refs
    isTuple name = case unboxedTupleArity name of
      Just _ -> True
      Nothing -> False

-- | The imports of the unboxed tuple of a package and an arity.
tupleImportsFor :: (PackageId, Int) -> Imports
tupleImportsFor (package, arity) =
  let declaration = unboxedTupleDecl package arity
      env = typeEnvFromProgram package (Program emptyImports [DeclType declaration])
   in importsForNames env (Set.fromList (typeName declaration : map conName (typeCons declaration)))

-- | Keep only the imports that the declarations reach. Normalization and
-- inlining drop bindings, and with them the last use of an import.
--
-- A pass can also make an unboxed tuple that the program did not use
-- before. Such a tuple gets its imports here, from 'unboxedTupleDecl'.
pruneImports :: Program -> Program
pruneImports program = program {programImports = imports'}
  where
    -- Only a top-level name can be an import.
    direct = foldl' (declRefs topWalks) Set.empty (programDecls program)
    declared = Set.fromList (concatMap declaredNames (programDecls program))
    newTuples =
      Set.fromList
        [ found
        | name <- Set.toList direct,
          Map.notMember name (importHeaders (programImports program)),
          Set.notMember name declared,
          Just found <- [unboxedTupleArity name]
        ]
    imports = foldr (mergeImports . tupleImportsFor) (programImports program) (Set.toList newTuples)
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
