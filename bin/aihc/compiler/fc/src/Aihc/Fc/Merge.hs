-- | Merge the System FC programs of many modules into one program.
module Aihc.Fc.Merge
  ( mergePrograms,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set

-- | One program that holds the declarations of every given program.
--
-- Each program numbers the scopes it names on its own, so the merged
-- program numbers the union of the scopes again. A name carries its full
-- origin, so the declarations do not change. The merged program imports
-- only the facts that none of its declarations supplies.
mergePrograms :: [Program] -> Program
mergePrograms programs =
  Program
    { programScopes = scopes,
      programImports = imports,
      programDecls = decls
    }
  where
    decls = concatMap programDecls programs
    scopes =
      foldl
        (\table (scopeId, (package, moduleName)) -> insertScope scopeId package moduleName table)
        emptyScopeTable
        (zip [1 ..] (Set.toAscList (Set.fromList [(package, moduleName) | program <- programs, (_, package, moduleName) <- scopeEntries (programScopes program)])))
    declared = Set.fromList (concatMap declaredNames decls)
    notDeclared = Map.filterWithKey (\name _ -> name `Set.notMember` declared)
    imports =
      Imports
        { importHeaders = notDeclared (Map.unions (map (importHeaders . programImports) programs)),
          importSynonyms = notDeclared (Map.unions (map (importSynonyms . programImports) programs)),
          importAxioms = notDeclared (Map.unions (map (importAxioms . programImports) programs)),
          importBinders = Map.unions (map (importBinders . programImports) programs)
        }

-- | The top-level names a declaration binds.
declaredNames :: Decl -> [Name]
declaredNames decl =
  case decl of
    DeclType declaration -> typeName declaration : map conName (typeCons declaration)
    DeclSynonym declaration -> [synName declaration]
    DeclAxiom declaration -> [axiomName declaration]
    DeclVal declaration -> [valName declaration]
