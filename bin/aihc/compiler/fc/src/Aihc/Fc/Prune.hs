-- | Drop the declarations of a System FC program that nothing reaches.
module Aihc.Fc.Prune
  ( pruneProgram,
  )
where

import Aihc.Fc.Imports (axiomReferences, declReferences, referencesFromImports, typeReferences)
import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf (typeHead)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
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
    -- name of a type never collides with the name of a value.
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

binderReferences :: Binder -> Set Name
binderReferences = typeReferences . binderType
