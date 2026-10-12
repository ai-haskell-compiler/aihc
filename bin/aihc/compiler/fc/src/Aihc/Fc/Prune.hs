-- | Drop the declarations of a System FC program that nothing reaches.
module Aihc.Fc.Prune
  ( pruneProgram,
    dropRules,
    pruneScopes,
  )
where

import Aihc.Fc.Imports (axiomReferences, declReferences, referencesFromImports, typeReferences)
import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf (typeHead)
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | Keep the declarations that one of the roots reaches, and drop the rest.
-- A root that the program does not declare has no effect.
--
-- Reachability is over every declared name, not only the names of values.
-- A type declaration carries one name for the type and one for each of its
-- constructors, and the two are reached apart: a value whose type mentions
-- @Maybe@ needs the header of @Maybe@, because that is where the kind that
-- decides the runtime representation lives, and needs no constructor of it.
-- A constructor is reached only where an expression builds it or a case
-- alternative matches it, so a type whose constructors nothing names keeps
-- its header alone and its constructor info tables go away.
--
-- Two references are not written in the program and hold all the same:
-- a constructor keeps the type it belongs to, and a type family keeps every
-- equation of the family, because 'Aihc.Fc.TypeOf.reduceType' finds an
-- equation by matching the head of its left side and never by name.
--
-- The imports are roots as well. They are not pruned here, and an import
-- whose type mentions a declaration of this program has to keep it.
pruneProgram :: [Name] -> Program -> Program
pruneProgram roots program =
  program {programDecls = concatMap keep (programDecls program)}
  where
    decls = programDecls program
    reachable = close Set.empty (roots <> Set.toList (referencesFromImports (programImports program)))
    keep decl =
      case decl of
        DeclType declaration ->
          let constructors = filter (reaches . conName) (typeCons declaration)
           in [ DeclType declaration {typeCons = constructors}
              | reaches (typeName declaration) || not (null constructors)
              ]
        DeclSynonym declaration -> [decl | reaches (synName declaration)]
        DeclAxiom declaration -> [decl | reaches (axiomName declaration)]
        DeclVal declaration -> [decl | reaches (valName declaration)]
        -- A rule stays while the head of its left-hand side does.
        DeclRule declaration -> [decl | Just headName <- [ruleHead declaration], reaches headName]
    reaches name = Set.member name reachable
    close :: Set Name -> [Name] -> Set Name
    close visited pending =
      case pending of
        [] -> visited
        name : rest
          | Set.member name visited -> close visited rest
          | otherwise ->
              let references =
                    Map.findWithDefault Set.empty name declaredReferences
                      <> Map.findWithDefault Set.empty name familyEquations
               in close (Set.insert name visited) (Set.toList references <> rest)
    -- What each declared name refers to. A name carries its sort, so the
    -- name of a type never collides with the name of a value. The map is
    -- lazy: only the declarations that the walk reaches compute their
    -- references, and a whole program usually reaches a small part of the
    -- core libraries.
    declaredReferences :: Map Name (Set Name)
    declaredReferences = Map.fromListWith (<>) (concatMap declaredEntries decls)
    declaredEntries decl =
      case decl of
        DeclType declaration ->
          ( typeName declaration,
            foldMap binderReferences (typeBinders declaration)
              <> typeReferences (typeResult declaration)
          )
            : [ (conName constructor, Set.insert (typeName declaration) (typeReferences (conType constructor)))
              | constructor <- typeCons declaration
              ]
        DeclSynonym declaration ->
          [ ( synName declaration,
              foldMap binderReferences (synBinders declaration)
                <> typeReferences (synResult declaration)
                <> typeReferences (synBody declaration)
            )
          ]
        DeclAxiom declaration -> [(axiomName declaration, axiomReferences declaration)]
        DeclVal declaration -> [(valName declaration, declReferences decl)]
        -- What a rule refers to is reached through its head, so that a
        -- kept rule keeps its right-hand side alive.
        DeclRule declaration -> [(headName, declReferences decl) | Just headName <- [ruleHead declaration]]
    -- The equations of each type family, under the name of the family.
    familyEquations :: Map Name (Set Name)
    familyEquations =
      Map.fromListWith
        (<>)
        [ (family, Set.singleton (axiomName declaration))
        | DeclAxiom declaration <- decls,
          axiomRole declaration == Nominal,
          Just family <- [typeHead (axiomLeft declaration)]
        ]

-- | Remove every rewrite rule from a program.
--
-- Use this function only on a whole program after its last pass. Then no
-- pass can fire a rule, but each rule that stays keeps the values of its
-- right-hand side, and 'pruneProgram' cannot remove them. A module must
-- keep its rules, because the modules that import it can fire them.
dropRules :: Program -> Program
dropRules program = program {programDecls = filter (not . isRule) (programDecls program)}
  where
    isRule decl =
      case decl of
        DeclRule {} -> True
        _ -> False

-- | Keep the scope entries of the modules that a name of the program
-- comes from, and number them again from 1. A merge keeps the scopes of
-- each module, and the prune drops most of the declarations that use them.
-- The scope table is a part of the program, so this step changes the
-- program and not only its text.
pruneScopes :: Program -> Program
pruneScopes program =
  program {programScopes = foldr insert emptyScopeTable (zip [1 ..] kept)}
  where
    imports = programImports program
    declarations = programDecls program
    names =
      foldMap declReferences declarations
        <> referencesFromImports imports
        <> Set.fromList (concatMap declaredNames declarations)
        <> Set.fromList (concatMap foreignCallNames declarations)
        <> Set.fromList
          ( Map.keys (importHeaders imports)
              <> Map.keys (importSynonyms imports)
              <> Map.keys (importAxioms imports)
              <> Map.keys (importBinders imports)
              <> Map.keys (importConRepresentations imports)
              <> Map.keys (importConStrictFields imports)
              <> Map.keys (importDataCons imports)
              <> concat (Map.elems (importDataCons imports))
          )
    used = Set.fromList [(package, moduleName) | name <- Set.toList names, OriginTop package moduleName <- [nameOrigin name]]
    kept = [(package, moduleName) | (_, package, moduleName) <- scopeEntries (programScopes program), (package, moduleName) `Set.member` used]
    insert (scopeId, (package, moduleName)) = insertScope scopeId package moduleName
    declaredNames decl = case decl of
      DeclType value -> typeName value : map conName (typeCons value)
      DeclSynonym value -> [synName value]
      DeclAxiom value -> [axiomName value]
      DeclVal value -> [valName value]
      DeclRule {} -> []
    foreignCallNames decl = case decl of
      DeclVal value -> expressionCallNames (valBody value)
      DeclRule value -> expressionCallNames (ruleLhs value) <> expressionCallNames (ruleRhs value)
      _ -> []

-- | The names of the foreign imports that the calls of an expression use.
-- 'declReferences' leaves them out, because a call carries its own type.
expressionCallNames :: Expr -> [Name]
expressionCallNames expr = case expr of
  ExApp function argument -> expressionCallNames function <> expressionCallNames argument
  ExTyApp function _ -> expressionCallNames function
  ExLam _ body -> expressionCallNames body
  ExTyLam _ body -> expressionCallNames body
  ExLet binding body -> expressionCallNames (bindRhs binding) <> expressionCallNames body
  ExRec bindings body -> concatMap (expressionCallNames . bindRhs) bindings <> expressionCallNames body
  ExCase scrutinee _ alternatives -> expressionCallNames scrutinee <> concatMap (expressionCallNames . altRhs) alternatives
  ExAbsurd scrutinee _ -> expressionCallNames scrutinee
  ExCast body _ -> expressionCallNames body
  ExForeignCall call _ arguments -> foreignCallName call : concatMap expressionCallNames arguments
  _ -> []

binderReferences :: Binder -> Set Name
binderReferences = typeReferences . binderType

-- | The top-level value at the head of a rule's left-hand side.
ruleHead :: RuleDecl -> Maybe Name
ruleHead declaration = go (ruleLhs declaration)
  where
    go expr =
      case expr of
        ExVar name | OriginTop {} <- nameOrigin name -> Just name
        ExApp function _ -> go function
        ExTyApp function _ -> go function
        ExCast inner _ -> go inner
        _ -> Nothing
