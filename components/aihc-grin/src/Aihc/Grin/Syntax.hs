{-# LANGUAGE OverloadedStrings #-}

-- | Strict graph-reduction intermediate language.
--
-- GRIN evaluation is strict: operands are values, and sequencing is explicit
-- through 'GrinBind'. Haskell laziness is represented by heap-allocated thunk
-- nodes and the explicit 'GrinEval' and 'GrinApply' operations.
module Aihc.Grin.Syntax
  ( GrinProgram (..),
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
    grinValueRuntimeRep,
    isLiftedRuntimeRep,
    isPointerRuntimeRep,
    builtinConstructors,
    builtinConstructorLayouts,
  )
where

import Aihc.Tc.Types (RuntimeRep (..), liftedRuntimeRep)
import Data.Text (Text)

-- | A whole GRIN program.
data GrinProgram = GrinProgram
  { grinConstructors :: ![(Text, [RuntimeRep])],
    grinPrimitives :: ![(GrinVar, Int)],
    grinForeignCalls :: ![GrinForeignCall],
    -- | Top-level values that are already in weak-head normal form. These are
    -- initialized directly rather than represented by updateable CAF cells.
    grinWhnfGlobals :: ![(GrinVar, GrinNode)],
    grinCafs :: ![(GrinVar, GrinNode)],
    grinFunctions :: ![GrinFunction]
  }
  deriving (Eq, Show, Read)

-- | A first-order code definition. Closures and thunks refer to functions by
-- name and carry their environment as node fields.
data GrinFunction = GrinFunction
  { grinFunctionName :: !FunctionName,
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

-- | Strict expressions return zero or more register values. 'GrinBind' names
-- those values for the following expression. In particular, an unboxed tuple
-- is represented by its flattened components, never by a heap node.
data GrinExpr
  = GrinReturn ![GrinValue]
  | GrinBind ![GrinVar] !GrinExpr !GrinExpr
  | GrinStore !GrinNode
  | GrinStoreRec ![(GrinVar, GrinNode)] !GrinExpr
  | GrinFetch !RuntimeRep !GrinValue
  | GrinUpdate !GrinValue !GrinValue
  | GrinEval !RuntimeRep !GrinValue
  | GrinApply !RuntimeRep !GrinValue ![GrinValue]
  | GrinCase !GrinValue !GrinVar ![GrinAlt]
  | GrinThrow !GrinValue
  | GrinCatch !RuntimeRep !GrinValue !GrinValue ![GrinValue]
  | -- | A saturated call whose operands are already strict primitive values.
    GrinForeignCallExpr !GrinForeignCall ![GrinValue]
  deriving (Eq, Show, Read)

-- | Atomic operands in the strict language.
data GrinValue
  = GrinVarValue !GrinVar
  | GrinLitValue !GrinLiteral
  | GrinNodeValue !GrinNode
  deriving (Eq, Show, Read)

data GrinNode = GrinNode
  { grinNodeTag :: !GrinNodeTag,
    grinNodeFields :: ![GrinValue]
  }
  deriving (Eq, Show, Read)

data GrinNodeTag
  = GrinConstructor !Text
  | GrinClosure !FunctionName !Int
  | -- | A suspended computation. Its target function must return exactly
    -- @BoxedRep Lifted@; unlifted computations are always evaluated strictly.
    GrinThunk !FunctionName
  | GrinPrimitive !Text !Int
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
  deriving (Eq, Show, Read)

grinValueRuntimeRep :: GrinValue -> RuntimeRep
grinValueRuntimeRep value =
  case value of
    GrinVarValue var -> grinVarRuntimeRep var
    GrinLitValue literal ->
      case literal of
        GrinLitInt runtimeRep _ -> runtimeRep
        GrinLitChar runtimeRep _ -> runtimeRep
        GrinLitString {} -> liftedRuntimeRep
    GrinNodeValue {} -> liftedRuntimeRep

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
builtinConstructors :: [(Text, Int)]
builtinConstructors = [(name, length layout) | (name, layout) <- builtinConstructorLayouts]

builtinConstructorLayouts :: [(Text, [RuntimeRep])]
builtinConstructorLayouts =
  [ ("C#", [WordRep]),
    ("[]", []),
    (":", [liftedRuntimeRep, liftedRuntimeRep]),
    ("()", []),
    ("(,)", [liftedRuntimeRep, liftedRuntimeRep])
  ]

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
