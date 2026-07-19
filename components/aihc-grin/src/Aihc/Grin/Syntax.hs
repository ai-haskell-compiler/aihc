{-# LANGUAGE OverloadedStrings #-}

-- | Strict graph-reduction intermediate language.
--
-- GRIN evaluation is strict: operands are values, and sequencing is explicit
-- through 'GrinBind'. Haskell laziness is represented by heap-allocated thunk
-- nodes and the explicit 'GrinEval' and 'GrinApply' operations.
module Aihc.Grin.Syntax
  ( GrinProgram (..),
    GrinCodeInfo (..),
    GrinFunction (..),
    FunctionName (..),
    GrinVar (..),
    GrinExpr (..),
    GrinValue (..),
    GrinNode (..),
    GrinNodeTag (..),
    GrinAlt (..),
    GrinAltCon (..),
    GrinLiteral (..),
    GrinForeignCall (..),
    GrinForeignSignature (..),
    GrinForeignEffect (..),
    GrinForeignType (..),
    runtimeRepComponents,
    grinForeignOperandReps,
    grinForeignCallResultReps,
    grinProgramLiterals,
    grinValueRuntimeRep,
    isLiftedRuntimeRep,
    isPointerRuntimeRep,
    builtinConstructorLayouts,
    builtinConstructors,
  )
where

import Aihc.Tc.Types (RuntimeRep (..), liftedRuntimeRep)
import Data.Text (Text)

-- | A whole GRIN program.
data GrinProgram = GrinProgram
  { grinConstructors :: ![(Text, [[RuntimeRep]])],
    grinPrimitives :: ![(GrinVar, Int)],
    grinForeignCalls :: ![GrinForeignCall],
    -- | Global slots supplied by dependency units and referenced by this unit.
    grinExternalGlobals :: ![Text],
    -- | Code entries supplied by other compilation units. Unlike runtime
    -- globals, these names never occupy global-table slots.
    grinExternalFunctions :: ![GrinCodeInfo],
    -- | Top-level values that are already in weak-head normal form. These are
    -- initialized directly rather than represented by updateable CAF cells.
    grinWhnfGlobals :: ![(GrinVar, GrinNode)],
    grinCafs :: ![(GrinVar, GrinNode)],
    grinFunctions :: ![GrinFunction]
  }
  deriving (Eq, Show, Read)

-- | Runtime calling information exported for a top-level code binding.
-- Parameter layouts retain source-argument boundaries because a Core term
-- argument such as @State# RealWorld@ may contribute no runtime values.
data GrinCodeInfo = GrinCodeInfo
  { grinCodeSourceName :: !Text,
    grinCodeFunctionName :: !FunctionName,
    grinCodeParameterLayouts :: ![[RuntimeRep]],
    grinCodeResultRep :: !RuntimeRep
  }
  deriving (Eq, Show, Read)

-- | A first-order code definition. Closures and thunks refer to functions by
-- name and carry their environment as node fields.
data GrinFunction = GrinFunction
  { grinFunctionName :: !FunctionName,
    -- | Source-level name when this entry is link-visible to other units.
    grinFunctionLinkName :: !(Maybe Text),
    grinFunctionParameters :: ![GrinVar],
    grinFunctionResultRep :: !RuntimeRep,
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
    grinVarRuntimeRep :: !RuntimeRep
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
  | GrinStoreRec ![(GrinVar, GrinNode)] !GrinExpr
  | GrinFetch !RuntimeRep !GrinValue
  | GrinUpdate !GrinValue !GrinValue
  | -- | Enter a heap pointer until it points to a node in weak-head normal
    -- form. The result remains a heap pointer; evaluation never returns the
    -- fetched node payload directly.
    GrinEval !RuntimeRep !GrinValue
  | -- | CPS-only evaluation. The first continuation receives a value already
    -- in weak-head normal form. The second continuation receives the result
    -- of an entered thunk and is responsible for updating its blackhole.
    GrinCpsEval !RuntimeRep !GrinValue !GrinValue !GrinValue
  | -- | A saturated call to a statically known code entry.
    GrinCall !RuntimeRep !FunctionName ![GrinValue]
  | -- | A saturated call to a statically known primitive entry.
    GrinPrimitiveCall !RuntimeRep !Text ![GrinValue]
  | -- | Apply exactly one logical argument to a heap pointer whose node is
    -- already in weak-head normal form. The list contains that argument's
    -- runtime values and may be empty for a zero-width argument such as
    -- @State# RealWorld@.
    GrinApply !RuntimeRep !GrinValue ![GrinValue]
  | -- | CPS-only application. Partial applications and saturated
    -- constructors transfer their result to the continuation; saturated
    -- closures enter their code with the continuation as the hidden final
    -- argument.
    GrinCpsApply !RuntimeRep !GrinValue ![GrinValue] !GrinValue
  | -- | Invoke an ordinary continuation closure with one logical result.
    -- Unlike 'GrinCpsApply', continuation entries do not themselves receive a
    -- return continuation.
    GrinContinue !GrinValue ![GrinValue]
  | -- | Update a cell that was blackholed by 'GrinCpsEval'. This is separate
    -- from an ordinary explicit heap update so the runtime can enforce the
    -- thunk-update protocol.
    GrinUpdateBlackhole !GrinValue !GrinValue
  | -- | Terminate CPS execution with the supplied observable result values.
    GrinHalt ![GrinValue]
  | -- | Match a value that is already in weak-head normal form.
    GrinCase !GrinValue !GrinVar ![GrinAlt]
  | GrinThrow !GrinValue
  | GrinCatch !RuntimeRep !GrinValue !GrinValue ![GrinValue]
  | -- | A saturated call whose operands are already strict primitive values.
    GrinForeignCallExpr !GrinForeignCall ![GrinValue]
  deriving (Eq, Show, Read)

-- | Atomic operands in the strict language.
data GrinValue
  = GrinVarValue !GrinVar
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
    GrinClosure !FunctionName ![[RuntimeRep]]
  | -- | A suspended computation. Its target function must return exactly
    -- @BoxedRep Lifted@; unlifted computations are always evaluated strictly.
    GrinThunk !FunctionName
  deriving (Eq, Show, Read)

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
  = GrinLitInt !RuntimeRep !Integer
  | GrinLitChar !RuntimeRep !Char
  | GrinLitString !Text
  | GrinLitAddr !Text
  deriving (Eq, Show, Read)

-- | Every literal embedded in a program, including node fields and case
-- alternatives. Native backends use this to build static literal pools.
grinProgramLiterals :: GrinProgram -> [GrinLiteral]
grinProgramLiterals program =
  concatMap (nodeLiterals . snd) (grinWhnfGlobals program <> grinCafs program)
    <> concatMap (exprLiterals . grinFunctionBody) (grinFunctions program)
  where
    exprLiterals expression =
      case expression of
        GrinConstant values -> concatMap valueLiterals values
        GrinBind _ valueExpression body -> exprLiterals valueExpression <> exprLiterals body
        GrinStore node -> nodeLiterals node
        GrinStoreRec bindings body -> concatMap (nodeLiterals . snd) bindings <> exprLiterals body
        GrinFetch _ pointer -> valueLiterals pointer
        GrinUpdate pointer value -> valueLiterals pointer <> valueLiterals value
        GrinEval _ value -> valueLiterals value
        GrinCpsEval _ value continuation updateContinuation ->
          valueLiterals value <> valueLiterals continuation <> valueLiterals updateContinuation
        GrinCall _ _ arguments -> concatMap valueLiterals arguments
        GrinPrimitiveCall _ _ arguments -> concatMap valueLiterals arguments
        GrinApply _ function arguments -> valueLiterals function <> concatMap valueLiterals arguments
        GrinCpsApply _ function arguments continuation ->
          valueLiterals function <> concatMap valueLiterals arguments <> valueLiterals continuation
        GrinContinue continuation values -> valueLiterals continuation <> concatMap valueLiterals values
        GrinUpdateBlackhole pointer value -> valueLiterals pointer <> valueLiterals value
        GrinHalt values -> concatMap valueLiterals values
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

grinValueRuntimeRep :: GrinValue -> RuntimeRep
grinValueRuntimeRep value =
  case value of
    GrinVarValue var -> grinVarRuntimeRep var
    GrinLitValue literal ->
      case literal of
        GrinLitInt runtimeRep _ -> runtimeRep
        GrinLitChar runtimeRep _ -> runtimeRep
        GrinLitString {} -> liftedRuntimeRep
        GrinLitAddr {} -> AddrRep

isLiftedRuntimeRep :: RuntimeRep -> Bool
isLiftedRuntimeRep runtimeRep = runtimeRep == liftedRuntimeRep

-- | Flatten a source runtime representation into the values carried by GRIN's
-- calling convention. Tuple components compose recursively, and zero-width
-- tuples such as @State# RealWorld@ occupy no runtime slot.
runtimeRepComponents :: RuntimeRep -> [RuntimeRep]
runtimeRepComponents runtimeRep =
  case runtimeRep of
    TupleRep fieldReps -> concatMap runtimeRepComponents fieldReps
    _ -> [runtimeRep]

-- | Runtime reps carried in one pointer-sized slot.
isPointerRuntimeRep :: RuntimeRep -> Bool
isPointerRuntimeRep runtimeRep =
  case runtimeRep of
    BoxedRep {} -> True
    SumRep {} -> True
    _ -> False

-- | Constructors supplied by the runtime rather than an FC data declaration.
-- Keeping their arities with the shared GRIN syntax makes lowering,
-- interpretation, linting, and native code generation agree on which global
-- constructor values exist before the program starts.
builtinConstructors :: [(Text, [[RuntimeRep]])]
builtinConstructors =
  [ ("C#", [[WordRep]]),
    ("[]", []),
    (":", [[liftedRuntimeRep], [liftedRuntimeRep]]),
    ("()", []),
    ("(,)", [[liftedRuntimeRep], [liftedRuntimeRep]])
  ]

-- | Flattened storage layouts for runtime-supplied constructors. Source-level
-- argument boundaries matter for arity, while heap snapshots describe the
-- individual machine values stored in each node.
builtinConstructorLayouts :: [(Text, [RuntimeRep])]
builtinConstructorLayouts =
  [(name, concat argumentLayouts) | (name, argumentLayouts) <- builtinConstructors]

data GrinForeignCall = GrinForeignCall
  { grinForeignCallName :: !Text,
    grinForeignCallSymbol :: !Text,
    grinForeignCallSignature :: !GrinForeignSignature
  }
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

data GrinForeignType
  = GrinForeignInt32
  | GrinForeignWord64
  | GrinForeignAddr
  deriving (Eq, Show, Read)

grinForeignOperandReps :: GrinForeignSignature -> [RuntimeRep]
grinForeignOperandReps signature =
  map foreignTypeRuntimeRep (grinForeignArgumentTypes signature)

grinForeignCallResultReps :: GrinForeignSignature -> [RuntimeRep]
grinForeignCallResultReps signature =
  [foreignTypeRuntimeRep (grinForeignResultType signature)]

foreignTypeRuntimeRep :: GrinForeignType -> RuntimeRep
foreignTypeRuntimeRep foreignType =
  case foreignType of
    GrinForeignInt32 -> Int32Rep
    GrinForeignWord64 -> Word64Rep
    GrinForeignAddr -> AddrRep
