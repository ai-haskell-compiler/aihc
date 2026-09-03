-- | Static reference tables for GRIN functions.
--
-- The collector cannot keep every static object alive forever without also
-- keeping everything an evaluated CAF retains. A function's static reference
-- table names the static objects the function's code can reach without going
-- through a heap object, so the collector can mark exactly those objects when
-- the function is running and when one of its closures is traced.
--
-- Tables are chained instead of flattened: a table names the tables of the
-- functions it calls directly, and the runtime walks that graph. The reachable
-- set is therefore the same as a transitive closure, but the compiler never
-- computes one, every reference stays an ordinary relocation, and the analysis
-- does not need the whole program.
--
-- A node stored on the heap contributes no child. The stored object carries
-- its own info table, and that table carries the target function's static
-- reference table, so the collector reaches it only while the object lives.
module Aihc.Grin.Srt
  ( StaticObject (..),
    StaticReferenceTable (..),
    StaticReferences (..),
    lookupStaticReferenceTable,
    programStaticObjects,
    programStaticReferences,
    staticNodeIsTraced,
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

-- | One statically emitted object together with the collector's interest in
-- it. Untraced objects still exist in the data section; they simply never move
-- and never retain anything, so nothing has to mark them.
data StaticObject = StaticObject
  { staticObjectName :: !Text,
    staticObjectNode :: !GrinNode,
    staticObjectTraced :: !Bool
  }
  deriving (Eq, Show, Read)

-- | The static objects one function reaches directly, and the tables of the
-- functions it calls directly.
data StaticReferenceTable = StaticReferenceTable
  { srtObjects :: ![Text],
    srtChildren :: ![FunctionName]
  }
  deriving (Eq, Show, Read)

-- | Every non-empty table in one program, and the static objects that need
-- collector tracing at all.
data StaticReferences = StaticReferences
  { staticReferenceTables :: !(Map FunctionName StaticReferenceTable),
    staticReferenceTracedObjects :: !(Set Text)
  }
  deriving (Eq, Show, Read)

-- | The table of one function, or 'Nothing' when the function reaches no
-- traced static object. Backends emit no symbol for an absent table.
lookupStaticReferenceTable :: StaticReferences -> FunctionName -> Maybe StaticReferenceTable
lookupStaticReferenceTable references name =
  Map.lookup name (staticReferenceTables references)

-- | Every object the backends emit into the data section: the program's
-- declared globals followed by the nullary constructors that only appear as
-- references.
programStaticObjects :: GrinProgram -> [StaticObject]
programStaticObjects program =
  [ StaticObject name node (staticNodeIsTraced node)
  | (name, node) <- declaredGlobals <> implicitConstructors
  ]
  where
    declaredGlobals = grinGlobals program
    declaredNames = Set.fromList (map fst declaredGlobals)
    implicitConstructors =
      [ (name, GrinNode (GrinConstructor name 0) [])
      | (name, layouts) <- grinConstructors program,
        null layouts,
        not (name `Set.member` declaredNames)
      ]

-- | Whether the collector has to mark one static object.
--
-- Marking an object does two things: it scans the object's fields, and it
-- walks the static reference table on the object's info table. An object needs
-- marking when either of those can matter.
--
-- Closures and thunks always need it, because they carry code and therefore a
-- table. A static closure with no captured fields still names the static
-- objects its entry function reaches, and nothing else will walk that table
-- while the closure is only waiting to be entered.
--
-- A constructor has no code, so it matters only when it has fields at all.
-- That leaves nullary constructors, which can neither move nor retain
-- anything, out of the collector's bookkeeping entirely.
--
-- The constructor test is deliberately the field count rather than the field
-- representations. The collector follows the info table's pointer bitmap, and
-- a constructor's bitmap comes from the declared layout rather than from the
-- values this particular object stores. Marking every object that has fields
-- leaves that one decision with the bitmap, at the cost of scanning the
-- occasional static node whose fields are all unboxed.
staticNodeIsTraced :: GrinNode -> Bool
staticNodeIsTraced node =
  case grinNodeTag node of
    GrinThunk {} -> True
    GrinClosure {} -> True
    GrinConstructor {} -> not (null (grinNodeFields node))

programStaticReferences :: GrinProgram -> StaticReferences
programStaticReferences program =
  StaticReferences
    { staticReferenceTables =
        Map.fromList
          [ (name, StaticReferenceTable objects (children name))
          | name <- Set.toAscList reaching,
            let objects = Map.findWithDefault [] name directObjects
          ],
      staticReferenceTracedObjects = tracedNames
    }
  where
    localObjects = programStaticObjects program
    tracedNames =
      Set.fromList [staticObjectName object | object <- localObjects, staticObjectTraced object]
    untracedNames =
      Set.fromList [staticObjectName object | object <- localObjects, not (staticObjectTraced object)]
    definedNames = Set.fromList (map grinFunctionName (grinFunctions program))
    directObjects =
      Map.fromList
        [ (grinFunctionName function, objects)
        | function <- grinFunctions program,
          let objects = functionDirectObjects untracedNames function,
          not (null objects)
        ]
    directCalls =
      Map.fromList
        [ (grinFunctionName function, callees)
        | function <- grinFunctions program,
          let callees =
                Set.delete (grinFunctionName function) $
                  Set.intersection definedNames (bodyCalls (grinFunctionBody function)),
          not (Set.null callees)
        ]
    -- A function reaches a traced static object when it mentions one or when
    -- one of its known callees does. Iterating to a fixed point over the call
    -- graph drops the tables that would only ever be empty.
    reaching = fixpoint (Map.keysSet directObjects)
    fixpoint current
      | Set.size next == Set.size current = current
      | otherwise = fixpoint next
      where
        next =
          Set.union current $
            Set.fromList
              [ caller
              | (caller, callees) <- Map.toList directCalls,
                not (Set.disjoint callees current)
              ]
    children name =
      Set.toAscList (Set.intersection reaching (Map.findWithDefault Set.empty name directCalls))

-- | A function mentions a static object either through an explicit global
-- reference or through a free variable, which the backends materialize as a
-- global address. Both forms name a symbol the function's own code already
-- references, so naming them in a table adds no new link dependency.
--
-- Incremental compilation lowers one module at a time, so a function routinely
-- mentions a static object another module defines and this program knows
-- nothing about. Such an object must still be named, or nothing would keep an
-- imported CAF alive. A name is therefore dropped only when this program
-- defines the object and knows it needs no marking; anything else is named,
-- and the collector decides at run time whether the address is one it tracks.
functionDirectObjects :: Set Text -> GrinFunction -> [Text]
functionDirectObjects untracedNames function =
  sort (Set.toList (Set.difference mentioned untracedNames))
  where
    body = grinFunctionBody function
    freeNames =
      Set.map grinVarName $
        Set.difference (freeExprVars body) (Set.fromList (grinFunctionParameters function))
    mentioned = Set.union freeNames (Set.fromList (grinExprGlobalReferences body))

-- | Every function an expression calls by name. Calls through heap objects
-- are absent on purpose: the entered object carries the callee's table.
bodyCalls :: GrinExpr -> Set FunctionName
bodyCalls expression =
  case expression of
    GrinCall _ name _ -> Set.singleton name
    GrinBind _ valueExpression body -> bodyCalls valueExpression <> bodyCalls body
    GrinStoreRec _ body -> bodyCalls body
    GrinStoreRecUnchecked _ body -> bodyCalls body
    GrinCase _ _ alternatives -> foldMap (bodyCalls . grinAltRhs) alternatives
    GrinConstant {} -> Set.empty
    GrinStore {} -> Set.empty
    GrinEnsureHeap {} -> Set.empty
    GrinStoreUnchecked {} -> Set.empty
    GrinFetch {} -> Set.empty
    GrinUpdate {} -> Set.empty
    GrinEval {} -> Set.empty
    GrinCpsEval {} -> Set.empty
    GrinPrimitiveCall {} -> Set.empty
    GrinCpsPrimitiveCall {} -> Set.empty
    GrinApply {} -> Set.empty
    GrinCpsApply {} -> Set.empty
    GrinContinue {} -> Set.empty
    GrinCpsRaise {} -> Set.empty
    GrinUpdateBlackhole {} -> Set.empty
    GrinHalt {} -> Set.empty
    GrinExit {} -> Set.empty
    GrinThrow {} -> Set.empty
    GrinCatch {} -> Set.empty
    GrinForeignCallExpr {} -> Set.empty
