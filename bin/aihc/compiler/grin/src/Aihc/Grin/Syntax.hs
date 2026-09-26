-- | Strict graph-reduction intermediate language.
--
-- GRIN evaluation is strict: operands are values, and sequencing is explicit
-- through 'GrinBind'. Haskell laziness is represented by heap-allocated thunk
-- nodes and the explicit 'GrinEval' and 'GrinApply' operations.
module Aihc.Grin.Syntax
  ( GrinRep (..),
    GrinLevity (..),
    GrinVecCount (..),
    GrinVecElem (..),
    liftedGrinRep,
    GrinResultRep (..),
    liftedResultRep,
    resultRepComponents,
    grinApplyGroupLimit,
    GrinProgram (..),
    GrinVis (..),
    GrinConstructorDecl (..),
    pubConstructor,
    GrinGlobal (..),
    pubGlobal,
    grinGlobalRoots,
    grinConstructorRoots,
    GrinFunction (..),
    FunctionName (..),
    GrinVar (..),
    GrinExpr (..),
    GrinEvalUpdate (..),
    forwardedResultUses,
    GrinValue (..),
    GrinNode (..),
    GrinNodeTag (..),
    GrinAlt (..),
    GrinAltCon (..),
    GrinLiteral (..),
    GrinForeignCall (..),
    GrinForeignTarget (..),
    GrinForeignSignature (..),
    GrinForeignEffect (..),
    GrinForeignType (..),
    runtimeRepComponents,
    SumLayout (..),
    sumLayout,
    sumSlotRep,
    grinForeignOperandReps,
    grinForeignCallResultReps,
    foreignTypeRuntimeRep,
    GrinScope (..),
    grinScopedName,
    grinNameScope,
    grinProgramScopes,
    grinProgramLiterals,
    grinExprGlobalReferences,
    grinProgramGlobalReferences,
    grinNodeGlobalReferences,
    grinExprFunctionNames,
    grinNodeFunctionNames,
    grinExprConstructorTags,
    grinNodeConstructorTags,
    grinValueRuntimeRep,
    grinVarNameNeedsNumber,
    FunctionNames,
    functionNamesFrom,
    claimFunctionName,
    isLiftedRuntimeRep,
    isPointerRuntimeRep,
  )
where

import Data.ByteString (ByteString)
import Data.Char (isDigit)
import Data.List (mapAccumL)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | One runtime ABI layout. This data does not contain Haskell type information.
data GrinRep
  = BoxedRep !GrinLevity
  | IntRep
  | Int8Rep
  | Int16Rep
  | Int32Rep
  | Int64Rep
  | WordRep
  | Word8Rep
  | Word16Rep
  | Word32Rep
  | Word64Rep
  | AddrRep
  | FloatRep
  | DoubleRep
  | SumRep ![GrinRep]
  | TupleRep ![GrinRep]
  | VecRep !GrinVecCount !GrinVecElem
  deriving (Eq, Ord, Show, Read)

data GrinLevity = Lifted | Unlifted
  deriving (Eq, Ord, Show, Read)

data GrinVecCount = Vec16 | Vec2 | Vec32 | Vec4 | Vec64 | Vec8
  deriving (Eq, Ord, Show, Read)

data GrinVecElem
  = Int16ElemRep
  | Int32ElemRep
  | Int64ElemRep
  | Int8ElemRep
  | Word8ElemRep
  | Word16ElemRep
  | Word32ElemRep
  | Word64ElemRep
  | FloatElemRep
  | DoubleElemRep
  deriving (Eq, Ord, Show, Read)

liftedGrinRep :: GrinRep
liftedGrinRep = BoxedRep Lifted

-- | A concrete result layout or an abstract result that passes through unchanged.
--
-- A concrete layout gives the width and register class of each result component.
-- An abstract result has no local layout. The caller and the callee know the layout at their ends.
-- Intermediate functions pass their continuation to a callee or use final touches followed by 'GrinForward'.
-- Neither operation places individual result components.
--
-- 'GrinRep' describes machine values. An abstract result cannot occupy a binder, node field, case scrutinee, or GC root.
data GrinResultRep
  = ResultRep !GrinRep
  | ResultForwarded
  deriving (Eq, Ord, Show, Read)

liftedResultRep :: GrinResultRep
liftedResultRep = ResultRep liftedGrinRep

-- | The values a result occupies, when the function places them itself.
resultRepComponents :: GrinResultRep -> Maybe [GrinRep]
resultRepComponents resultRep =
  case resultRep of
    ResultRep runtimeRep -> Just (runtimeRepComponents runtimeRep)
    ResultForwarded -> Nothing

-- | The package and the module that a top-level GRIN name comes from.
data GrinScope = GrinScope
  { grinScopePackage :: !Text,
    grinScopeModule :: !Text
  }
  deriving (Eq, Ord, Show, Read)

-- | Join a package, a module, and a name into one GRIN name. Globals,
-- constructor tags, and foreign call names all use this encoding. The linker
-- splits the same name into its object symbol components.
grinScopedName :: Text -> Text -> Text -> Text
grinScopedName packageName moduleName baseName =
  T.intercalate "\0" [packageName, moduleName, baseName]

-- | Split a top-level GRIN name into its scope and its base name. A name that
-- does not come from a module, such as a primitive or a local name, gives
-- 'Nothing'.
grinNameScope :: Text -> Maybe (GrinScope, Text)
grinNameScope name =
  case T.splitOn "\0" name of
    [packageName, moduleName, baseName] -> Just (GrinScope packageName moduleName, baseName)
    _ -> Nothing

-- | A whole GRIN program.
data GrinProgram = GrinProgram
  { grinConstructors :: ![GrinConstructorDecl],
    grinPrimitives :: ![(GrinVar, Int)],
    grinForeignCalls :: ![GrinForeignCall],
    grinGlobals :: ![GrinGlobal],
    grinFunctions :: ![GrinFunction]
  }
  deriving (Eq, Show, Read)

-- | Whether another unit can name a definition. GRIN inherits this from the
-- export visibility of the System FC declaration it was lowered from, so
-- that reachability is well defined: the public globals are the roots of the
-- program, and anything they do not reach is dead.
--
-- A whole-program build demotes every declaration but the entry to
-- 'GrinPrivate' before lowering, which is what makes the same sweep serve a
-- whole-program build and a separate build of one module alike.
data GrinVis
  = GrinPub
  | GrinPrivate
  deriving (Eq, Ord, Show, Read)

-- | One constructor of the program: the runtime layout of each of its
-- logical fields, and whether another unit can name it.
--
-- A constructor is part of the interface of the module that declares it. Its
-- info table is named wherever a value of it is built or matched, and a
-- nullary one has a shared static object that stands for the single value it
-- has, so a public constructor is a root of reachability even when no value
-- of the declaring module mentions it.
data GrinConstructorDecl = GrinConstructorDecl
  { grinConstructorName :: !Text,
    grinConstructorLayouts :: ![[GrinRep]],
    grinConstructorVis :: !GrinVis
  }
  deriving (Eq, Show, Read)

-- | A public constructor. See 'pubGlobal'.
pubConstructor :: Text -> [[GrinRep]] -> GrinConstructorDecl
pubConstructor name layouts = GrinConstructorDecl name layouts GrinPub

-- | One static object of the global table.
--
-- Only a global carries visibility. Every public value gets one — a
-- declaration goes without only when it is a private function or a
-- constructor of one field or more — so the public globals name every entry
-- another unit can take into this one. A function is reached from the tag of
-- the node that names it and never on its own, so it needs no visibility of
-- its own and is always internal to the object that defines it.
data GrinGlobal = GrinGlobal
  { grinGlobalName :: !Text,
    grinGlobalNode :: !GrinNode,
    grinGlobalVis :: !GrinVis
  }
  deriving (Eq, Show, Read)

-- | A public global. Programs written by hand, in tests and in the runtime
-- support, are whole and their globals are the roots, so this is what they
-- want; code that lowers a declaration reads the visibility of that
-- declaration instead.
pubGlobal :: Text -> GrinNode -> GrinGlobal
pubGlobal name node = GrinGlobal name node GrinPub

-- | The globals that reachability starts from: the ones another unit can name.
grinGlobalRoots :: GrinProgram -> [Text]
grinGlobalRoots program =
  [grinGlobalName global | global <- grinGlobals program, grinGlobalVis global == GrinPub]

-- | The constructors that reachability starts from.
grinConstructorRoots :: GrinProgram -> [Text]
grinConstructorRoots program =
  [ grinConstructorName constructor
  | constructor <- grinConstructors program,
    grinConstructorVis constructor == GrinPub
  ]

-- | A first-order code definition. Closures and thunks refer to functions by
-- name and carry their environment as node fields.
data GrinFunction = GrinFunction
  { grinFunctionName :: !FunctionName,
    grinFunctionParameters :: ![GrinVar],
    grinFunctionResultRep :: !GrinResultRep,
    grinFunctionBody :: !GrinExpr
  }
  deriving (Eq, Show, Read)

newtype FunctionName = FunctionName
  { unFunctionName :: Text
  }
  deriving (Eq, Ord, Show, Read)

-- | GRIN erases source types but preserves the part of their kinds that
-- determines the runtime ABI.
data GrinVar = GrinVar
  { grinVarName :: !Text,
    grinVarUnique :: !Int,
    grinVarRuntimeRep :: !GrinRep
  }
  deriving (Show, Read)

instance Eq GrinVar where
  left == right =
    grinVarName left == grinVarName right
      && grinVarUnique left == grinVarUnique right

instance Ord GrinVar where
  compare left right =
    compare
      (grinVarName left, grinVarUnique left)
      (grinVarName right, grinVarUnique right)

-- | Strict expressions produce zero or more register values. 'GrinBind' names
-- those values for the following expression. 'GrinConstant' can only forward
-- atomic variables and literals; every dynamic node enters the heap explicitly
-- through 'GrinStore' or 'GrinStoreRec'. In particular, an unboxed tuple is
-- represented by its flattened components, never by a heap node.
data GrinExpr
  = GrinConstant ![GrinValue]
  | GrinBind ![GrinVar] !GrinExpr !GrinExpr
  | GrinStore !GrinNode
  | -- | Heap reservation whose size may be computed at runtime. Before GC
    -- lowering the root list is empty; afterward it contains precisely the
    -- live roots at the following allocation and returns their relocated
    -- values in the same order.
    GrinEnsureHeap !GrinValue ![GrinValue]
  | -- | A node allocation covered by a preceding 'GrinEnsureHeap'.
    GrinStoreUnchecked !GrinNode
  | GrinStoreRec ![(GrinVar, GrinNode)] !GrinExpr
  | -- | A recursive allocation group covered by one preceding
    -- 'GrinEnsureHeap'.
    GrinStoreRecUnchecked ![(GrinVar, GrinNode)] !GrinExpr
  | GrinUpdate !GrinValue !GrinValue
  | -- | Enter a heap pointer until it points to a node in weak-head normal
    -- form. The result remains a heap pointer; evaluation never returns the
    -- fetched node payload directly.
    GrinEval !GrinEvalUpdate !GrinRep !GrinValue
  | -- | CPS-only evaluation. With 'EvalUpdate', the runtime creates an update
    -- frame for each thunk it enters.
    GrinCpsEval !GrinEvalUpdate !GrinRep !GrinValue !GrinValue
  | -- | Read the fields of a node in weak-head normal form. The tag is the
    -- tag of that node. The compiler knows it, and nothing checks it at
    -- runtime. The results are the fields in their order.
    GrinFetch !GrinNodeTag !GrinValue
  | -- | CPS-only WHNF test. Neither the test nor the ready branch requires a heap frame.
    -- Indirections, thunks, and blackholes select the slow branch.
    GrinIfWhnf !GrinValue !GrinExpr !GrinExpr
  | -- | A saturated call to a statically known code entry. The result is
    -- the one this call site expects; see 'GrinResultRep' for when it is
    -- forwarded rather than placed.
    GrinCall !GrinResultRep !FunctionName ![GrinValue]
  | -- | A saturated call to a statically known primitive entry.
    GrinPrimitiveCall !GrinRep !Text ![GrinValue]
  | -- | A CPS-only primitive that may transfer execution to another thread.
    -- The continuation receives the primitive's logical result.
    GrinCpsPrimitiveCall !GrinRep !Text ![GrinValue] !GrinValue
  | -- | Apply one or more logical arguments to a heap pointer whose node is
    -- already in weak-head normal form. Each inner list is one argument
    -- group: the runtime values of one logical argument. A group can be
    -- empty for a zero-width argument such as @State# RealWorld@. The list
    -- of groups is never empty. The function can take fewer or more groups
    -- than the application supplies.
    GrinApply !GrinResultRep !GrinValue ![[GrinValue]]
  | -- | Return the abstract result of an enclosing empty bind without a concrete layout.
    -- Only final uses through @touch#@ can occur between that bind and this return.
    -- After CPS conversion, a forwarding continuation retains those uses as ordinary closure fields.
    GrinForward
  | -- | CPS-only application. Partial applications and saturated
    -- constructors transfer their result to the continuation; saturated
    -- closures enter their code with the continuation as the hidden final
    -- argument.
    GrinCpsApply !GrinResultRep !GrinValue ![[GrinValue]] !GrinValue
  | -- | Invoke an ordinary continuation closure with one logical result.
    -- Unlike 'GrinCpsApply', continuation entries do not themselves receive a
    -- return continuation.
    GrinContinue !GrinValue ![GrinValue]
  | -- | Raise a synchronous exception through the heap-resident continuation
    -- chain. This form exists only after CPS conversion.
    GrinCpsRaise !GrinValue !GrinValue
  | -- | Update a cell that was blackholed by 'GrinCpsEval'. This is separate
    -- from an ordinary explicit heap update so the runtime can enforce the
    -- thunk-update protocol.
    GrinUpdateBlackhole !GrinValue !GrinValue
  | -- | Terminate CPS execution with the supplied observable result values.
    GrinHalt ![GrinValue]
  | -- | Terminate the process with an already-unboxed machine status.
    GrinExit !GrinValue
  | -- | Match a value that is already in weak-head normal form.
    GrinCase !GrinValue !GrinVar ![GrinAlt]
  | GrinThrow !GrinValue
  | GrinCatch !GrinRep !GrinValue !GrinValue ![GrinValue]
  | -- | A saturated call whose operands are already strict primitive values.
    GrinForeignCallExpr !GrinForeignCall ![GrinValue]
  deriving (Eq, Show, Read)

-- | The largest number of argument groups that one 'GrinApply' or
-- 'GrinCpsApply' supplies. The backend generates an apply helper for each
-- shape of groups, and the code of a helper grows with the square of the
-- group count. A longer application is a chain of applications.
grinApplyGroupLimit :: Int
grinApplyGroupLimit = 4

-- | What an evaluation does with a thunk that it enters.
data GrinEvalUpdate
  = -- | Write the result of the thunk back into the thunk, so that each
    -- later evaluation of the same thunk gets that result.
    EvalUpdate
  | -- | Enter the thunk without an update frame. Use this mode only where
    -- no other evaluation can get to the same thunk. The heap points-to
    -- analysis finds these evaluations.
    EvalSingleEntry
  deriving (Eq, Ord, Show, Read)

-- | Atomic operands in the strict language.
data GrinValue
  = GrinVarValue !GrinVar
  | GrinGlobalValue !Text
  | GrinLitValue !GrinLiteral
  deriving (Eq, Show, Read)

data GrinNode = GrinNode
  { grinNodeTag :: !GrinNodeTag,
    grinNodeFields :: ![GrinValue]
  }
  deriving (Eq, Show, Read)

data GrinNodeTag
  = -- | A constructor with its remaining logical field count.
    GrinConstructor !Text !Int
  | -- | A function closure with the runtime layout of every remaining logical
    -- argument. Empty layouts are retained because they still count toward
    -- semantic arity even though they carry no runtime values.
    GrinClosure !FunctionName ![[GrinRep]]
  | -- | A suspended computation. Its target function must return exactly
    -- @BoxedRep Lifted@; unlifted computations are always evaluated strictly.
    GrinThunk !FunctionName
  deriving (Eq, Ord, Show, Read)

data GrinAlt = GrinAlt
  { grinAltCon :: !GrinAltCon,
    grinAltBinders :: ![GrinVar],
    grinAltRhs :: !GrinExpr
  }
  deriving (Eq, Show, Read)

data GrinAltCon
  = GrinDataAlt !Text
  | GrinLitAlt !GrinLiteral
  | GrinDefaultAlt
  deriving (Eq, Show, Read)

data GrinLiteral
  = GrinLitInt !GrinRep !Integer
  | GrinLitChar !GrinRep !Char
  | GrinLitAddr !ByteString
  deriving (Eq, Show, Read)

-- | Every literal embedded in a program, including node fields and case
-- alternatives. Native backends use this to build static literal pools.
-- | Every scope that the top-level names of one program come from, in a
-- stable order. The printer gives each of them a number, and it prints a name
-- from a numbered scope without its package and its module.
-- | The function names that are in use, and for each base name the first
-- number that 'claimFunctionName' has not tried yet.
data FunctionNames = FunctionNames
  { functionNamesUsed :: !(Set FunctionName),
    functionNamesNext :: !(Map.Map Text Int)
  }

-- | The names of a program's functions, before any name is claimed.
functionNamesFrom :: Set FunctionName -> FunctionNames
functionNamesFrom used = FunctionNames used Map.empty

-- | Claim the first name that is free, starting from @base@. A name that is
-- in use gets a number, so that no two functions of a program share a name.
--
-- The claimed names stay in use, so every number below the one claimed last
-- for a base is still taken. The search for the next name of that base
-- therefore starts after it. A module that names thousands of functions
-- after one value would otherwise try every number again for each of them.
claimFunctionName :: Text -> FunctionNames -> (FunctionName, FunctionNames)
claimFunctionName base names
  | Set.notMember (FunctionName base) used = (FunctionName base, names {functionNamesUsed = Set.insert (FunctionName base) used})
  | otherwise = search (Map.findWithDefault 2 base (functionNamesNext names))
  where
    used = functionNamesUsed names
    search index
      | Set.member candidate used = search (index + 1)
      | otherwise =
          ( candidate,
            FunctionNames
              { functionNamesUsed = Set.insert candidate used,
                functionNamesNext = Map.insert base (index + 1) (functionNamesNext names)
              }
          )
      where
        candidate = FunctionName (base <> "_" <> T.pack (show (index :: Int)))

grinProgramScopes :: GrinProgram -> [GrinScope]
grinProgramScopes program =
  Set.toAscList (Set.fromList (mapMaybe (fmap fst . grinNameScope) names))
  where
    names =
      map grinConstructorName (grinConstructors program)
        <> map grinGlobalName (grinGlobals program)
        <> map grinForeignCallName (grinForeignCalls program)
        <> grinProgramGlobalReferences program
        <> concatMap (nodeTagNames . grinGlobalNode) (grinGlobals program)
        <> concatMap (exprTagNames . grinFunctionBody) (grinFunctions program)

-- | The constructor tags that one expression names. Node tags and case
-- alternatives are the only places that name a constructor.
exprTagNames :: GrinExpr -> [Text]
exprTagNames expression =
  case expression of
    GrinBind _ valueExpression body -> exprTagNames valueExpression <> exprTagNames body
    GrinStore node -> nodeTagNames node
    GrinStoreUnchecked node -> nodeTagNames node
    GrinFetch tag _ -> nodeTagNames (GrinNode tag [])
    GrinStoreRec bindings body -> concatMap (nodeTagNames . snd) bindings <> exprTagNames body
    GrinStoreRecUnchecked bindings body -> concatMap (nodeTagNames . snd) bindings <> exprTagNames body
    GrinIfWhnf _ ready slow -> exprTagNames ready <> exprTagNames slow
    GrinCase _ _ alternatives -> concatMap altTagNames alternatives
    _ -> []
  where
    altTagNames alternative = altConTagNames (grinAltCon alternative) <> exprTagNames (grinAltRhs alternative)
    altConTagNames altCon =
      case altCon of
        GrinDataAlt name -> [name]
        _ -> []

nodeTagNames :: GrinNode -> [Text]
nodeTagNames node =
  case grinNodeTag node of
    GrinConstructor name _ -> [name]
    _ -> []

grinProgramLiterals :: GrinProgram -> [GrinLiteral]
grinProgramLiterals program =
  concatMap (nodeLiterals . grinGlobalNode) (grinGlobals program)
    <> concatMap (exprLiterals . grinFunctionBody) (grinFunctions program)
  where
    exprLiterals expression =
      case expression of
        GrinConstant values -> concatMap valueLiterals values
        GrinBind _ valueExpression body -> exprLiterals valueExpression <> exprLiterals body
        GrinStore node -> nodeLiterals node
        GrinEnsureHeap requiredWords roots -> valueLiterals requiredWords <> concatMap valueLiterals roots
        GrinStoreUnchecked node -> nodeLiterals node
        GrinStoreRec bindings body -> concatMap (nodeLiterals . snd) bindings <> exprLiterals body
        GrinStoreRecUnchecked bindings body -> concatMap (nodeLiterals . snd) bindings <> exprLiterals body
        GrinUpdate pointer value -> valueLiterals pointer <> valueLiterals value
        GrinEval _ _ value -> valueLiterals value
        GrinCpsEval _ _ value continuation ->
          valueLiterals value <> valueLiterals continuation
        GrinFetch _ value -> valueLiterals value
        GrinCall _ _ arguments -> concatMap valueLiterals arguments
        GrinPrimitiveCall _ _ arguments -> concatMap valueLiterals arguments
        GrinCpsPrimitiveCall _ _ arguments continuation ->
          concatMap valueLiterals arguments <> valueLiterals continuation
        GrinApply _ function arguments -> valueLiterals function <> concatMap valueLiterals (concat arguments)
        GrinForward -> []
        GrinCpsApply _ function arguments continuation ->
          valueLiterals function <> concatMap valueLiterals (concat arguments) <> valueLiterals continuation
        GrinContinue continuation values -> valueLiterals continuation <> concatMap valueLiterals values
        GrinCpsRaise exception continuation -> valueLiterals exception <> valueLiterals continuation
        GrinUpdateBlackhole pointer value -> valueLiterals pointer <> valueLiterals value
        GrinHalt values -> concatMap valueLiterals values
        GrinExit status -> valueLiterals status
        GrinIfWhnf value ready slow -> valueLiterals value <> exprLiterals ready <> exprLiterals slow
        GrinCase scrutinee _ alternatives -> valueLiterals scrutinee <> concatMap altLiterals alternatives
        GrinThrow exception -> valueLiterals exception
        GrinCatch _ action handler state ->
          valueLiterals action <> valueLiterals handler <> concatMap valueLiterals state
        GrinForeignCallExpr _ arguments -> concatMap valueLiterals arguments
    altLiterals alternative = altConLiterals (grinAltCon alternative) <> exprLiterals (grinAltRhs alternative)
    altConLiterals altCon =
      case altCon of
        GrinLitAlt literal -> [literal]
        _ -> []
    nodeLiterals = concatMap valueLiterals . grinNodeFields
    valueLiterals value =
      case value of
        GrinLitValue literal -> [literal]
        GrinVarValue {} -> []
        GrinGlobalValue {} -> []

-- | Every explicit global-table reference in one program.
grinProgramGlobalReferences :: GrinProgram -> [Text]
grinProgramGlobalReferences program =
  concatMap (nodeReferences . grinGlobalNode) (grinGlobals program)
    <> concatMap (grinExprGlobalReferences . grinFunctionBody) (grinFunctions program)

-- | Every explicit global-table reference in one expression.
grinExprGlobalReferences :: GrinExpr -> [Text]
grinExprGlobalReferences = exprReferences
  where
    exprReferences expression =
      case expression of
        GrinConstant values -> valuesReferences values
        GrinBind _ valueExpression body -> exprReferences valueExpression <> exprReferences body
        GrinStore node -> nodeReferences node
        GrinEnsureHeap requiredWords roots -> valueReferences requiredWords <> valuesReferences roots
        GrinStoreUnchecked node -> nodeReferences node
        GrinStoreRec bindings body -> concatMap (nodeReferences . snd) bindings <> exprReferences body
        GrinStoreRecUnchecked bindings body -> concatMap (nodeReferences . snd) bindings <> exprReferences body
        GrinUpdate pointer value -> valueReferences pointer <> valueReferences value
        GrinEval _ _ value -> valueReferences value
        GrinCpsEval _ _ value continuation -> valuesReferences [value, continuation]
        GrinFetch _ value -> valueReferences value
        GrinCall _ _ arguments -> valuesReferences arguments
        GrinPrimitiveCall _ _ arguments -> valuesReferences arguments
        GrinCpsPrimitiveCall _ _ arguments continuation -> valuesReferences arguments <> valueReferences continuation
        GrinApply _ function arguments -> valueReferences function <> valuesReferences (concat arguments)
        GrinForward -> []
        GrinCpsApply _ function arguments continuation -> valueReferences function <> valuesReferences (concat arguments) <> valueReferences continuation
        GrinContinue continuation values -> valueReferences continuation <> valuesReferences values
        GrinCpsRaise exception continuation -> valueReferences exception <> valueReferences continuation
        GrinUpdateBlackhole pointer value -> valueReferences pointer <> valueReferences value
        GrinHalt values -> valuesReferences values
        GrinExit status -> valueReferences status
        GrinIfWhnf value ready slow -> valueReferences value <> exprReferences ready <> exprReferences slow
        GrinCase scrutinee _ alternatives -> valueReferences scrutinee <> concatMap (exprReferences . grinAltRhs) alternatives
        GrinThrow exception -> valueReferences exception
        GrinCatch _ action handler state -> valuesReferences (action : handler : state)
        GrinForeignCallExpr _ arguments -> valuesReferences arguments
    valuesReferences = concatMap valueReferences

-- | Every explicit global-table reference in the fields of one node.
grinNodeGlobalReferences :: GrinNode -> [Text]
grinNodeGlobalReferences = nodeReferences

nodeReferences :: GrinNode -> [Text]
nodeReferences = concatMap valueReferences . grinNodeFields

-- | The functions one node names: the entry of a closure or a thunk.
grinNodeFunctionNames :: GrinNode -> [FunctionName]
grinNodeFunctionNames node =
  case grinNodeTag node of
    GrinClosure name _ -> [name]
    GrinThunk name -> [name]
    GrinConstructor {} -> []

-- | The functions one expression names: the target of a call, and the entry
-- of every node it builds.
grinExprFunctionNames :: GrinExpr -> [FunctionName]
grinExprFunctionNames = exprNames
  where
    exprNames expression =
      case expression of
        GrinCall _ name _ -> [name]
        GrinBind _ valueExpression body -> exprNames valueExpression <> exprNames body
        GrinStore node -> grinNodeFunctionNames node
        GrinFetch tag _ -> grinNodeFunctionNames (GrinNode tag [])
        GrinStoreUnchecked node -> grinNodeFunctionNames node
        GrinStoreRec bindings body -> concatMap (grinNodeFunctionNames . snd) bindings <> exprNames body
        GrinStoreRecUnchecked bindings body -> concatMap (grinNodeFunctionNames . snd) bindings <> exprNames body
        GrinIfWhnf _ ready slow -> exprNames ready <> exprNames slow
        GrinCase _ _ alternatives -> concatMap (exprNames . grinAltRhs) alternatives
        _ -> []

-- | The constructors one node names.
grinNodeConstructorTags :: GrinNode -> [Text]
grinNodeConstructorTags = nodeTagNames

-- | The constructors one expression names: the tag of every node it builds
-- and the constructor of every alternative it matches.
grinExprConstructorTags :: GrinExpr -> [Text]
grinExprConstructorTags = exprTagNames

valueReferences :: GrinValue -> [Text]
valueReferences value =
  case value of
    GrinGlobalValue name -> [name]
    GrinVarValue {} -> []
    GrinLitValue {} -> []

-- | Whether a printed variable must carry its number even when that number is
-- zero. A name shaped like an integer or a character would otherwise be read
-- back as a literal: @(0 :: IntRep)@ is the integer zero, not a variable.
grinVarNameNeedsNumber :: Text -> Bool
grinVarNameNeedsNumber name =
  case T.uncons name of
    Nothing -> False
    Just ('\'', _) -> True
    Just (character, rest)
      | character == '+' || character == '-' -> isIntegerShaped rest
      | otherwise -> isIntegerShaped name
  where
    isIntegerShaped digits = not (T.null digits) && T.all isDigit digits

grinValueRuntimeRep :: GrinValue -> GrinRep
grinValueRuntimeRep value =
  case value of
    GrinVarValue var -> grinVarRuntimeRep var
    GrinGlobalValue {} -> liftedGrinRep
    GrinLitValue literal ->
      case literal of
        GrinLitInt runtimeRep _ -> runtimeRep
        GrinLitChar runtimeRep _ -> runtimeRep
        GrinLitAddr {} -> AddrRep

isLiftedRuntimeRep :: GrinRep -> Bool
isLiftedRuntimeRep runtimeRep = runtimeRep == liftedGrinRep

-- | Flatten a source runtime representation into the values carried by GRIN's
-- calling convention. Tuple components compose recursively, and zero-width
-- tuples such as @State# RealWorld@ occupy no runtime slot.
runtimeRepComponents :: GrinRep -> [GrinRep]
runtimeRepComponents runtimeRep =
  case runtimeRep of
    TupleRep fieldReps -> concatMap runtimeRepComponents fieldReps
    SumRep alternatives -> IntRep : sumSlots (sumLayout alternatives)
    _ -> [runtimeRep]

-- | The tag precedes these slots. Alternative indices exclude the tag.
-- Each slot has one representation for all alternatives.
data SumLayout = SumLayout
  { sumSlots :: ![GrinRep],
    sumAlternativeSlots :: ![[Int]]
  }
  deriving (Eq, Show, Read)

-- | Share compatible slots across alternatives. Keep pointer levities separate.
sumLayout :: [GrinRep] -> SumLayout
sumLayout alternatives = SumLayout slots (map positions components)
  where
    components = map (map sumSlotRep . runtimeRepComponents) alternatives
    counts = Map.unionsWith max (map (Map.fromListWith (+) . map (,1 :: Int)) components)
    slots = concat [replicate count representation | (representation, count) <- Map.toAscList counts]
    offsets = Map.fromListWith (flip (<>)) [(representation, [index]) | (index, representation) <- zip [0 ..] slots]
    positions = snd . mapAccumL select offsets
    select available representation =
      case Map.findWithDefault [] representation available of
        index : rest -> (Map.insert representation rest available, index)
        [] -> error "sum layout has no slot for an alternative component"

-- | Integer values share a machine slot. Other classes remain separate.
sumSlotRep :: GrinRep -> GrinRep
sumSlotRep representation =
  case representation of
    Int8Rep -> IntRep
    Int16Rep -> IntRep
    Int32Rep -> IntRep
    Int64Rep -> IntRep
    WordRep -> IntRep
    Word8Rep -> IntRep
    Word16Rep -> IntRep
    Word32Rep -> IntRep
    Word64Rep -> IntRep
    _ -> representation

-- | Runtime reps carried in one pointer-sized slot.
isPointerRuntimeRep :: GrinRep -> Bool
isPointerRuntimeRep runtimeRep =
  case runtimeRep of
    BoxedRep {} -> True
    _ -> False

data GrinForeignCall = GrinForeignCall
  { grinForeignCallName :: !Text,
    grinForeignCallSymbol :: !Text,
    grinForeignCallTarget :: !GrinForeignTarget,
    grinForeignCallSignature :: !GrinForeignSignature
  }
  deriving (Eq, Show, Read)

-- | Whether the symbol is called, or is static data whose address is the
-- result (@foreign import ccall "&sym"@).
data GrinForeignTarget
  = GrinForeignFunction
  | GrinForeignAddress
  | GrinForeignDynamic
  | GrinForeignUnsafeFunction
  | GrinForeignUnsafeDynamic
  | GrinForeignWrapper !GrinForeignSignature
  deriving (Eq, Show, Read)

data GrinForeignSignature = GrinForeignSignature
  { grinForeignArgumentTypes :: ![GrinForeignType],
    grinForeignResultType :: !GrinForeignType,
    grinForeignEffect :: !GrinForeignEffect
  }
  deriving (Eq, Show, Read)

data GrinForeignEffect
  = GrinForeignPure
  | GrinForeignRealWorld
  deriving (Eq, Show, Read)

-- | The C ABI value of one foreign operand or result.
data GrinForeignType
  = GrinForeignInt
  | GrinForeignInt8
  | GrinForeignInt16
  | GrinForeignInt32
  | GrinForeignInt64
  | GrinForeignWord
  | GrinForeignWord8
  | GrinForeignWord16
  | GrinForeignWord32
  | GrinForeignWord64
  | GrinForeignFloat
  | GrinForeignDouble
  | GrinForeignAddr
  | GrinForeignClosure
  | -- | The result of a C procedure. It binds no GRIN value.
    GrinForeignVoid
  deriving (Eq, Show, Read, Enum, Bounded)

grinForeignOperandReps :: GrinForeignSignature -> [GrinRep]
grinForeignOperandReps signature =
  map foreignTypeRuntimeRep (grinForeignArgumentTypes signature)

grinForeignCallResultReps :: GrinForeignSignature -> [GrinRep]
grinForeignCallResultReps signature =
  runtimeRepComponents (foreignTypeRuntimeRep (grinForeignResultType signature))

foreignTypeRuntimeRep :: GrinForeignType -> GrinRep
foreignTypeRuntimeRep foreignType =
  case foreignType of
    GrinForeignInt -> IntRep
    GrinForeignInt8 -> Int8Rep
    GrinForeignInt16 -> Int16Rep
    GrinForeignInt32 -> Int32Rep
    GrinForeignInt64 -> Int64Rep
    GrinForeignWord -> WordRep
    GrinForeignWord8 -> Word8Rep
    GrinForeignWord16 -> Word16Rep
    GrinForeignWord32 -> Word32Rep
    GrinForeignWord64 -> Word64Rep
    GrinForeignFloat -> FloatRep
    GrinForeignDouble -> DoubleRep
    GrinForeignAddr -> AddrRep
    GrinForeignClosure -> liftedGrinRep
    GrinForeignVoid -> TupleRep []

-- | Final uses that leave an abstract result unchanged. No operation here can allocate or transfer control.
forwardedResultUses :: GrinExpr -> Maybe [GrinValue]
forwardedResultUses expr =
  case expr of
    GrinForward -> Just []
    GrinBind [] (GrinPrimitiveCall (TupleRep []) "touch#" [owner]) body ->
      (owner :) <$> forwardedResultUses body
    _ -> Nothing
