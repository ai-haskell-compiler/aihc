-- | Drop the GRIN definitions that nothing outside the program can reach.
--
-- Reachability starts from the public globals: the names another unit can
-- link against. A separate build of one module keeps what that module
-- exports and what its exports use, and a whole-program build, which has
-- demoted every declaration but the entry to private before lowering, keeps
-- only what the entry uses. The same sweep serves both.
--
-- Two kinds of definition survive lowering with nothing left to reach them.
-- A static closure exists only because some use named a private function as
-- a value; when simplification replaces that use with a direct call, the
-- closure is orphaned. A dictionary is the same story one step further out:
-- once every field selection reads the field of a known static node, nothing
-- names the node. Neither is visible before GRIN, so System FC pruning
-- cannot see either one.
--
-- The sweep runs before the CPS transform, which appends continuation
-- functions that are reachable by construction from the functions that use
-- them.
module Aihc.Grin.Dce
  ( dceGrinProgram,
    sweptGrinProgram,
  )
where

import Aihc.Grin.Analysis (freeExprVars)
import Aihc.Grin.Syntax
import Data.List (sort)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | One definition of the program. A global and a function never collide:
-- the two live in separate namespaces, and a value can own one of each.
data Key
  = GlobalKey !Text
  | FunctionKey !FunctionName
  | ConstructorKey !Text
  deriving (Eq, Ord, Show)

-- | Sweep a program and check the result: no definition that survives may
-- name one that did not.
--
-- The reachability closure makes that true by construction, so a failure
-- means the reference reader below missed a place a name can occur — a node
-- kind or an expression form it does not walk. That is a silent bug
-- otherwise: the name goes missing and the object file loses a symbol or a
-- layout that something still needs. The check reads references back with
-- the program-wide walks in 'Aihc.Grin.Syntax', not with the per-definition
-- reader the sweep uses, so a gap in one is visible to the other.
sweptGrinProgram :: GrinProgram -> Either String GrinProgram
sweptGrinProgram program
  | null dangling = Right swept
  | otherwise =
      Left
        ( "GRIN dead-code elimination dropped definitions that the program still names: "
            <> T.unpack (T.intercalate ", " dangling)
        )
  where
    swept = dceGrinProgram program
    droppedGlobals = Set.difference (definedGlobals program) (definedGlobals swept)
    droppedConstructors = Set.difference (definedConstructors program) (definedConstructors swept)
    definedGlobals = Set.fromList . map grinGlobalName . grinGlobals
    definedConstructors = Set.fromList . map grinConstructorName . grinConstructors
    droppedFunctions = Set.difference (definedFunctions program) (definedFunctions swept)
    definedFunctions = Set.fromList . map grinFunctionName . grinFunctions
    dangling =
      sort
        ( Set.toList (Set.intersection droppedGlobals (Set.fromList (grinProgramGlobalReferences swept)))
            <> Set.toList (Set.intersection droppedConstructors (Set.fromList (referencedConstructors <> grinProgramGlobalReferences swept)))
            <> map unFunctionName (Set.toList (Set.intersection droppedFunctions (Set.fromList referencedFunctions)))
        )
    referencedConstructors =
      concatMap (grinNodeConstructorTags . grinGlobalNode) (grinGlobals swept)
        <> concatMap (grinExprConstructorTags . grinFunctionBody) (grinFunctions swept)
    referencedFunctions =
      concatMap (grinNodeFunctionNames . grinGlobalNode) (grinGlobals swept)
        <> concatMap (grinExprFunctionNames . grinFunctionBody) (grinFunctions swept)

-- | Keep the definitions that a public global reaches, and drop the rest.
dceGrinProgram :: GrinProgram -> GrinProgram
dceGrinProgram program =
  program
    { grinGlobals = filter (reaches . GlobalKey . grinGlobalName) (grinGlobals program),
      grinFunctions = filter (reaches . FunctionKey . grinFunctionName) (grinFunctions program),
      grinConstructors = filter (reaches . ConstructorKey . grinConstructorName) (grinConstructors program)
    }
  where
    reaches key = Set.member key reachable
    reachable =
      close
        Set.empty
        (map GlobalKey (grinGlobalRoots program) <> map ConstructorKey (grinConstructorRoots program))
    close :: Set Key -> [Key] -> Set Key
    close visited pending =
      case pending of
        [] -> visited
        key : rest
          | Set.member key visited -> close visited rest
          | otherwise ->
              close
                (Set.insert key visited)
                (Map.findWithDefault [] key references <> rest)
    -- What each definition names. A constructor names nothing: its layout
    -- holds runtime representations, not names.
    references :: Map Key [Key]
    references =
      Map.fromListWith
        (<>)
        ( [ ( GlobalKey (grinGlobalName global),
              concatMap staticKeys (grinNodeGlobalReferences node)
                <> map FunctionKey (grinNodeFunctionNames node)
                <> map ConstructorKey (grinNodeConstructorTags node)
            )
          | global <- grinGlobals program,
            let node = grinGlobalNode global
          ]
            <> [ ( FunctionKey (grinFunctionName function),
                   concatMap staticKeys (bodyGlobals function)
                     <> map FunctionKey (grinExprFunctionNames body)
                     <> map ConstructorKey (grinExprConstructorTags body)
                 )
               | function <- grinFunctions program,
                 let body = grinFunctionBody function
               ]
        )
    -- A global-table reference names a static object, and a nullary
    -- constructor has one without declaring a global: the backends give the
    -- constructor a shared object that stands for every value of it. Such a
    -- reference therefore reaches whichever of the two the program has.
    staticKeys name = [GlobalKey name, ConstructorKey name]
    -- A body names a global either as an explicit global-table reference or,
    -- where lowering left one behind, as a free variable. Reading both is
    -- what 'Aihc.Grin.Srt' does to find the static objects of a function,
    -- and a free variable that happens to share a name with a global only
    -- ever keeps a definition that is already there.
    bodyGlobals function =
      grinExprGlobalReferences body
        <> [ grinVarName var
           | var <- Set.toList (Set.difference (freeExprVars body) parameters)
           ]
      where
        body = grinFunctionBody function
        parameters = Set.fromList (grinFunctionParameters function)
