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
    GrinProgram (..),
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
    GrinForeignTarget (..),
    GrinForeignSignature (..),
    GrinForeignEffect (..),
    GrinForeignType (..),
    runtimeRepComponents,
    grinForeignOperandReps,
    grinForeignCallResultReps,
    grinProgramLiterals,
    grinExprGlobalReferences,
    grinProgramGlobalReferences,
    grinValueRuntimeRep,
    grinVarNameNeedsNumber,
    isLiftedRuntimeRep,
    isPointerRuntimeRep,
  )
where

import Data.ByteString (ByteString)
import Data.Char (isDigit)
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

-- | A whole GRIN program.
data GrinProgram = GrinProgram
  { grinConstructors :: ![(Text, [[GrinRep]])],
    grinPrimitives :: ![(GrinVar, Int)],
    grinForeignCalls :: ![GrinForeignCall],
    grinGlobals :: ![(Text, GrinNode)],
    grinFunctions :: ![GrinFunction]
  }
  deriving (Eq, Show, Read)

-- | A first-order code definition. Closures and thunks refer to functions by
-- name and carry their environment as node fields.
data GrinFunction = GrinFunction
  { grinFunctionName :: !FunctionName,
    grinFunctionParameters :: ![GrinVar],
    grinFunctionResultRep :: !GrinRep,
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
    GrinEval !GrinRep !GrinValue
  | -- | CPS-only evaluation. The first continuation receives a value already
    -- in weak-head normal form. The second continuation receives the result
    -- of an entered thunk and is responsible for updating its blackhole.
    GrinCpsEval !GrinRep !GrinValue !GrinValue !GrinValue
  | -- | A saturated call to a statically known code entry.
    GrinCall !GrinRep !FunctionName ![GrinValue]
  | -- | A saturated call to a statically known primitive entry.
    GrinPrimitiveCall !GrinRep !Text ![GrinValue]
  | -- | A CPS-only primitive that may transfer execution to another thread.
    -- The continuation receives the primitive's logical result.
    GrinCpsPrimitiveCall !GrinRep !Text ![GrinValue] !GrinValue
  | -- | Apply exactly one logical argument to a heap pointer whose node is
    -- already in weak-head normal form. The list contains that argument's
    -- runtime values and may be empty for a zero-width argument such as
    -- @State# RealWorld@.
    GrinApply !GrinRep !GrinValue ![GrinValue]
  | -- | CPS-only application. Partial applications and saturated
    -- constructors transfer their result to the continuation; saturated
    -- closures enter their code with the continuation as the hidden final
    -- argument.
    GrinCpsApply !GrinRep !GrinValue ![GrinValue] !GrinValue
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
  = GrinLitInt !GrinRep !Integer
  | GrinLitChar !GrinRep !Char
  | GrinLitString !Text
  | GrinLitAddr !ByteString
  deriving (Eq, Show, Read)

-- | Every literal embedded in a program, including node fields and case
-- alternatives. Native backends use this to build static literal pools.
grinProgramLiterals :: GrinProgram -> [GrinLiteral]
grinProgramLiterals program =
  concatMap (nodeLiterals . snd) (grinGlobals program)
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
        GrinEval _ value -> valueLiterals value
        GrinCpsEval _ value continuation updateContinuation ->
          valueLiterals value <> valueLiterals continuation <> valueLiterals updateContinuation
        GrinCall _ _ arguments -> concatMap valueLiterals arguments
        GrinPrimitiveCall _ _ arguments -> concatMap valueLiterals arguments
        GrinCpsPrimitiveCall _ _ arguments continuation ->
          concatMap valueLiterals arguments <> valueLiterals continuation
        GrinApply _ function arguments -> valueLiterals function <> concatMap valueLiterals arguments
        GrinCpsApply _ function arguments continuation ->
          valueLiterals function <> concatMap valueLiterals arguments <> valueLiterals continuation
        GrinContinue continuation values -> valueLiterals continuation <> concatMap valueLiterals values
        GrinCpsRaise exception continuation -> valueLiterals exception <> valueLiterals continuation
        GrinUpdateBlackhole pointer value -> valueLiterals pointer <> valueLiterals value
        GrinHalt values -> concatMap valueLiterals values
        GrinExit status -> valueLiterals status
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
  concatMap (nodeReferences . snd) (grinGlobals program)
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
        GrinEval _ value -> valueReferences value
        GrinCpsEval _ value continuation updateContinuation -> valuesReferences [value, continuation, updateContinuation]
        GrinCall _ _ arguments -> valuesReferences arguments
        GrinPrimitiveCall _ _ arguments -> valuesReferences arguments
        GrinCpsPrimitiveCall _ _ arguments continuation -> valuesReferences arguments <> valueReferences continuation
        GrinApply _ function arguments -> valueReferences function <> valuesReferences arguments
        GrinCpsApply _ function arguments continuation -> valueReferences function <> valuesReferences arguments <> valueReferences continuation
        GrinContinue continuation values -> valueReferences continuation <> valuesReferences values
        GrinCpsRaise exception continuation -> valueReferences exception <> valueReferences continuation
        GrinUpdateBlackhole pointer value -> valueReferences pointer <> valueReferences value
        GrinHalt values -> valuesReferences values
        GrinExit status -> valueReferences status
        GrinCase scrutinee _ alternatives -> valueReferences scrutinee <> concatMap (exprReferences . grinAltRhs) alternatives
        GrinThrow exception -> valueReferences exception
        GrinCatch _ action handler state -> valuesReferences (action : handler : state)
        GrinForeignCallExpr _ arguments -> valuesReferences arguments
    valuesReferences = concatMap valueReferences

nodeReferences :: GrinNode -> [Text]
nodeReferences = concatMap valueReferences . grinNodeFields

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
        GrinLitString {} -> liftedGrinRep
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
    _ -> [runtimeRep]

-- | Runtime reps carried in one pointer-sized slot.
isPointerRuntimeRep :: GrinRep -> Bool
isPointerRuntimeRep runtimeRep =
  case runtimeRep of
    BoxedRep {} -> True
    SumRep {} -> True
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
  = GrinForeignInt
  | GrinForeignInt32
  | GrinForeignWord64
  | GrinForeignAddr
  deriving (Eq, Show, Read)

grinForeignOperandReps :: GrinForeignSignature -> [GrinRep]
grinForeignOperandReps signature =
  map foreignTypeRuntimeRep (grinForeignArgumentTypes signature)

grinForeignCallResultReps :: GrinForeignSignature -> [GrinRep]
grinForeignCallResultReps signature =
  [foreignTypeRuntimeRep (grinForeignResultType signature)]

foreignTypeRuntimeRep :: GrinForeignType -> GrinRep
foreignTypeRuntimeRep foreignType =
  case foreignType of
    GrinForeignInt -> IntRep
    GrinForeignInt32 -> Int32Rep
    GrinForeignWord64 -> Word64Rep
    GrinForeignAddr -> AddrRep
