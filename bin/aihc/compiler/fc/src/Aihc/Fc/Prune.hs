-- | Drop the value declarations of a System FC program that nothing
-- reaches.
module Aihc.Fc.Prune
  ( pruneProgram,
  )
where

import Aihc.Fc.Imports (declReferences)
import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | Keep every type, synonym, and axiom declaration, and every value
-- declaration that one of the roots reaches through the names in value
-- bodies. A root that the program does not declare has no effect.
pruneProgram :: [Name] -> Program -> Program
pruneProgram roots program =
  program {programDecls = filter keep (programDecls program)}
  where
    values = Map.fromList [(valName declaration, declaration) | DeclVal declaration <- programDecls program]
    reachable = close Set.empty (filter (`Map.member` values) roots)
    keep decl =
      case decl of
        DeclVal declaration -> valName declaration `Set.member` reachable
        _ -> True
    close :: Set Name -> [Name] -> Set Name
    close visited pending =
      case pending of
        [] -> visited
        name : rest
          | Set.member name visited -> close visited rest
          | otherwise ->
              let references =
                    case Map.lookup name values of
                      Just declaration -> [reference | reference <- Set.toList (declReferences (DeclVal declaration)), Map.member reference values]
                      Nothing -> []
               in close (Set.insert name visited) (references <> rest)
