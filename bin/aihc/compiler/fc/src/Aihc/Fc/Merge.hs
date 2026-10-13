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
-- A name carries its full origin, so the declarations do not change. The
-- merged program imports
-- only the facts that none of its declarations supplies.
mergePrograms :: [Program] -> Program
mergePrograms programs =
  Program
    { programImports = imports,
      programDecls = decls
    }
  where
    decls = concatMap programDecls programs
    declared = Set.fromList (concatMap declaredNames decls)
    notDeclared = Map.filterWithKey (\name _ -> name `Set.notMember` declared)
    imports =
      Imports
        { importConRepresentations = notDeclared (Map.unions (map (importConRepresentations . programImports) programs)),
          importConStrictFields = notDeclared (Map.unions (map (importConStrictFields . programImports) programs)),
          importDataCons = notDeclared (Map.unions (map (importDataCons . programImports) programs)),
          importHeaders = notDeclared (Map.unions (map (importHeaders . programImports) programs)),
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
    DeclRule {} -> []
