-- | Lir: the low-level intermediate language between GC-GRIN and the machine
-- backends. See @docs/lir.md@ for the specification.
--
-- A module is a list of items. A function is a control-flow graph of blocks
-- in static single assignment form. Blocks carry parameters instead of phi
-- instructions.
module Aihc.Lir.Syntax
  ( Module (..),
    Item (..),
    Symbol (..),
    Var (..),
    Label (..),
    Type (..),
    typeBits,
    isIntegerType,
    isFloatType,
    CallingConvention (..),
    Linkage (..),
    Signature (..),
    Function (..),
    functionSignature,
    ExternFunction (..),
    Global (..),
    Constant (..),
    ConstantExpr (..),
    ConstantOp (..),
    DataItem (..),
    DataField (..),
    Block (..),
    Instruction (..),
    Operation (..),
    BinaryOp (..),
    UnaryOp (..),
    WideOp (..),
    CompareOp (..),
    FloatBinaryOp (..),
    FloatUnaryOp (..),
    ConvertOp (..),
    MemoryOrder (..),
    AtomicOp (..),
    Address (..),
    Region (..),
    regionName,
    inRegion,
    AddressConstant (..),
    byteAddress,
    addressByteOffset,
    Alignment (..),
    byteAlignment,
    wordAlignment,
    alignmentInBytes,
    Terminator (..),
    terminatorTargets,
    Target (..),
    SwitchCase (..),
    Operand (..),
    Literal (..),
    forOperationOperands,
    forTerminatorOperands,
    mapOperation,
    mapAddress,
    mapTerminator,
  )
where

import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.Text (Text)

-- | A whole Lir module. Item order is preserved by the pretty-printer.
newtype Module = Module
  { moduleItems :: [Item]
  }
  deriving (Eq, Show)

data Item
  = ItemFunction !Function
  | ItemExternFunction !ExternFunction
  | ItemGlobal !Global
  | ItemData !DataItem
  | ItemExternData !Symbol
  | ItemConstant !Constant
  | -- | @include "path"@: the constants of another file, whose path is
    -- relative to the directory of the including file.
    -- "Aihc.Lir.Resolve" replaces the item with those constants.
    ItemInclude !Text
  deriving (Eq, Show)

-- | A module-level name: a function, a global, or a data object.
newtype Symbol = Symbol {unSymbol :: Text}
  deriving (Eq, Ord, Show)

-- | A value name inside one function.
newtype Var = Var {unVar :: Text}
  deriving (Eq, Ord, Show)

-- | A block label inside one function.
newtype Label = Label {unLabel :: Text}
  deriving (Eq, Ord, Show)

-- | 'Ptr' is the address of data and 'Code' is the address of a function.
-- They have the same width but no operation converts between them.
data Type = I1 | I8 | I16 | I32 | I64 | F32 | F64 | Ptr | Code
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The width of a type in bits. The interpreter uses a 64-bit word.
typeBits :: Type -> Int
typeBits ty =
  case ty of
    I1 -> 1
    I8 -> 8
    I16 -> 16
    I32 -> 32
    I64 -> 64
    F32 -> 32
    F64 -> 64
    Ptr -> 64
    Code -> 64

-- | The integer types. 'I1' is not an integer type.
isIntegerType :: Type -> Bool
isIntegerType ty = ty `elem` [I8, I16, I32, I64]

isFloatType :: Type -> Bool
isFloatType ty = ty `elem` [F32, F64]

data CallingConvention = AihcConvention | CConvention
  deriving (Eq, Ord, Show)

-- | 'Inline' is a function-only linkage. An inline function has no symbol
-- of its own: "Aihc.Lir.Inline" splices its body into every call and drops
-- the definition, so no backend ever sees one. A data object is 'Internal'
-- or 'Export'; the linter rejects an inline one.
data Linkage = Internal | Export | Inline
  deriving (Eq, Ord, Show)

data Signature = Signature
  { signatureParameters :: ![Type],
    signatureResults :: ![Type],
    signatureConvention :: !CallingConvention
  }
  deriving (Eq, Ord, Show)

data Function = Function
  { functionName :: !Symbol,
    functionLinkage :: !Linkage,
    functionParameters :: ![(Var, Type)],
    functionResults :: ![Type],
    functionConvention :: !CallingConvention,
    functionBlocks :: ![Block]
  }
  deriving (Eq, Show)

functionSignature :: Function -> Signature
functionSignature function =
  Signature
    { signatureParameters = map snd (functionParameters function),
      signatureResults = functionResults function,
      signatureConvention = functionConvention function
    }

data ExternFunction = ExternFunction
  { externFunctionName :: !Symbol,
    externFunctionSignature :: !Signature
  }
  deriving (Eq, Show)

-- | One mutable cell without an address.
data Global = Global
  { globalName :: !Symbol,
    globalType :: !Type
  }
  deriving (Eq, Show)

-- | A named integer expression. Resolution uses the target word size.
data Constant = Constant
  { constantName :: !Symbol,
    constantValue :: !ConstantExpr
  }
  deriving (Eq, Show)

data ConstantExpr
  = ConstantInt !Integer
  | ConstantRef !Symbol
  | ConstantWords !ConstantExpr
  | ConstantNegate !ConstantExpr
  | ConstantBinary !ConstantOp !ConstantExpr !ConstantExpr
  deriving (Eq, Show)

data ConstantOp = ConstantAdd | ConstantSub | ConstantMul | ConstantQuot | ConstantRem
  deriving (Eq, Show)

data DataItem = DataItem
  { dataName :: !Symbol,
    dataLinkage :: !Linkage,
    dataMutable :: !Bool,
    dataAlignment :: !Integer,
    dataFields :: ![DataField]
  }
  deriving (Eq, Show)

data DataField
  = -- | An integer stored little-endian in the width of the type. The type
    -- is 'I1' or an integer type; the text format has no other form.
    DataInt !Type !Integer
  | -- | @i64 \@name@: a 'DataInt' whose value is a named constant.
    DataIntConstant !Type !Symbol
  | -- | A float stored little-endian in the width of the type. The type is
    -- 'F32' or 'F64'.
    DataFloat !Type !Double
  | -- | @ptr \@symbol + addend@: the address of a data object plus an addend.
    DataSymbol !Symbol !Integer
  | -- | @ptr null@: one word of zero bytes.
    DataNull
  | -- | @word n@: an integer stored little-endian in the target word width.
    -- Info-table counts and kinds use it, so one hand-written module suits
    -- both a 64-bit and a 32-bit target. The value fits 32 bits, so no
    -- target truncates it.
    DataWord !Integer
  | -- | @word \@name@: a 'DataWord' whose value is a named constant.
    DataWordConstant !Symbol
  | -- | @code \@symbol@ or @code null@: the address of a function, or one
    -- word of zero bytes.
    DataCode !(Maybe Symbol)
  | DataBytes !ByteString
  | DataZero !Integer
  deriving (Eq, Show)

data Block = Block
  { blockLabel :: !Label,
    blockParameters :: ![(Var, Type)],
    blockInstructions :: ![Instruction],
    blockTerminator :: !Terminator,
    -- | Whether the block is a slow path that runs rarely. A call in a cold
    -- block is a cold call: the register allocator keeps the values that
    -- live across it in the registers the fast path wants, and saves them
    -- around the call.
    blockCold :: !Bool
  }
  deriving (Eq, Show)

data Instruction = Instruction
  { instructionResults :: ![Var],
    instructionOperation :: !Operation
  }
  deriving (Eq, Show)

data Operation
  = Binary !BinaryOp !Type !Operand !Operand
  | -- | A bit-count operation on an integer type.
    Unary !UnaryOp !Type !Operand
  | -- | An operation with two results.
    Wide !WideOp !Type !Operand !Operand
  | -- | Unsigned 128-bit dividend divided by an i64 divisor.
    QuotRemWide !Type !Operand !Operand !Operand
  | Compare !CompareOp !Type !Operand !Operand
  | FloatBinary !FloatBinaryOp !Type !Operand !Operand
  | FloatUnary !FloatUnaryOp !Type !Operand
  | -- | @Convert op from operand to@.
    Convert !ConvertOp !Type !Operand !Type
  | PtrToInt !Operand
  | PtrFromInt !Operand
  | Select !Type !Operand !Operand !Operand
  | Load !Type !Address !Alignment
  | -- | A load of a location whose contents never change while the program
    -- runs, such as a field of an info table. A backend can treat the load
    -- as a pure function of its address. The header word of a heap object
    -- is not such a location, since an update rewrites it.
    LoadInvariant !Type !Address !Alignment
  | Store !Type !Operand !Address !Alignment
  | -- | An atomic load. The location has the natural alignment of the type.
    LoadAtomic !MemoryOrder !Type !Address
  | -- | An atomic store. The location has the natural alignment of the type.
    StoreAtomic !MemoryOrder !Type !Operand !Address
  | -- | A load that orders each later load through its result. A native
    -- backend keeps the address dependency. LLVM uses an acquire load.
    LoadDependent !Type !Address
  | -- | @cas order type expected, replacement, address@. The store occurs
    -- only when the location holds the expected value. The result is the
    -- value that the location held before the operation.
    CompareSwap !MemoryOrder !Type !Operand !Operand !Address
  | -- | An atomic read, modify, and write. The result is the value that the
    -- location held before the operation.
    AtomicRmw !AtomicOp !MemoryOrder !Type !Operand !Address
  | Fence !MemoryOrder
  | PtrAdd !Operand !Operand
  | StackAlloc !Integer !Alignment
  | GlobalGet !Symbol
  | GlobalSet !Symbol !Operand
  | Call !Symbol ![Operand]
  | CallIndirect !Operand ![Operand] !Signature
  deriving (Eq, Show)

-- | The memory order of an atomic operation, as in C11. 'Relaxed' gives
-- atomicity and no order.
data MemoryOrder = Relaxed | Acquire | Release | AcqRel | SeqCst
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The read-modify-write operations. 'AtomicXchg' stores the operand.
data AtomicOp = AtomicXchg | AtomicAdd | AtomicOr
  deriving (Eq, Ord, Show, Enum, Bounded)

data BinaryOp
  = Add
  | Sub
  | Mul
  | DivS
  | DivU
  | RemS
  | RemU
  | And
  | Or
  | Xor
  | Shl
  | ShrS
  | ShrU
  | -- | Rotate left. Reduce the count modulo the integer width.
    RotL
  | -- | Rotate right. Reduce the count modulo the integer width.
    RotR
  | -- | Parallel bit deposit: the low bits of the left operand move to the
    -- set bits of the right operand, the mask, in order.
    Pdep
  | -- | Parallel bit extract: the bits of the left operand at the set bits
    -- of the mask gather into the low bits of the result, in order.
    Pext
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The bit-count operations. Each counts bits of an @iN@ value and gives an
-- @iN@ result. @Clz@ and @Ctz@ give @N@ for a zero operand, so none of them
-- has an undefined case.
data UnaryOp = Clz | Ctz | Popcount
  deriving (Eq, Ord, Show, Enum, Bounded)

data WideOp = MulWideS | MulWideU | AddCarry | SubBorrow
  deriving (Eq, Ord, Show, Enum, Bounded)

data CompareOp
  = Eq
  | Ne
  | LtS
  | LtU
  | LeS
  | LeU
  | GtS
  | GtU
  | GeS
  | GeU
  | FLt
  | FLe
  | FGt
  | FGe
  deriving (Eq, Ord, Show, Enum, Bounded)

data FloatBinaryOp = FAdd | FSub | FMul | FDiv
  deriving (Eq, Ord, Show, Enum, Bounded)

data FloatUnaryOp = FNeg | FAbs | FSqrt
  deriving (Eq, Ord, Show, Enum, Bounded)

data ConvertOp
  = SExt
  | ZExt
  | Trunc
  | IToFS
  | IToFU
  | FToIS
  | FToIU
  | FpExt
  | FpTrunc
  | Bitcast
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A base pointer and a constant offset. The offset is @addressOffset@
-- bytes plus @addressWordOffset@ target words, so a module that walks a
-- table of word-sized fields is the same text on a 32-bit and on a 64-bit
-- target. Use 'addressByteOffset' to read the offset for a word size.
data Address = Address
  { addressBase :: !Operand,
    addressOffset :: !Integer,
    addressWordOffset :: !Integer,
    addressConstants :: ![AddressConstant],
    addressRegion :: !Region
  }
  deriving (Eq, Show)

-- | The region of memory an address points into. A location is in one
-- region for the whole life of the program, so an access to one region
-- never aliases an access to a different region. A continuation frame lives
-- in a stack chunk, which is a heap object, but no frame slot is a field of
-- a heap object that compiled code reads or writes. Thus a backend can move
-- a stack load before a heap store, and the reverse. An access with no
-- region may alias any access.
data Region
  = -- | Any memory.
    AnyRegion
  | -- | A field of a heap object: a node, a mutable variable, an array, or
    -- a byte array.
    HeapRegion
  | -- | A slot of a continuation frame on the stack of a thread.
    StackRegion
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The keyword of a region. The empty text names no region.
regionName :: Region -> Text
regionName region =
  case region of
    AnyRegion -> ""
    HeapRegion -> "heap"
    StackRegion -> "stack"

-- | A named address term. Resolution adds its signed value to the byte
-- offset or the target word offset before a backend receives the address.
data AddressConstant = AddressConstant
  { addressConstantName :: !Symbol,
    addressConstantNegative :: !Bool,
    addressConstantInWords :: !Bool
  }
  deriving (Eq, Show)

-- | The alignment an access promises: a fixed number of bytes, or a number
-- of target words. A word-scaled alignment states the exact alignment of a
-- word-sized field on every target, which a byte count cannot.
data Alignment
  = AlignBytes !Integer
  | AlignWords !Integer
  deriving (Eq, Show)

-- | An alignment of a fixed number of bytes.
byteAlignment :: Integer -> Alignment
byteAlignment = AlignBytes

-- | An alignment of a number of target words.
wordAlignment :: Integer -> Alignment
wordAlignment = AlignWords

-- | An alignment in bytes, given the size of a target word.
alignmentInBytes :: Integer -> Alignment -> Integer
alignmentInBytes wordBytes alignment =
  case alignment of
    AlignBytes value -> value
    AlignWords count -> count * wordBytes

-- | An address with a byte offset and no word offset.
byteAddress :: Operand -> Integer -> Address
byteAddress base offset = Address base offset 0 [] AnyRegion

-- | The same address in the given region.
inRegion :: Region -> Address -> Address
inRegion region address = address {addressRegion = region}

-- | The offset of an address in bytes, given the size of a target word.
addressByteOffset :: Integer -> Address -> Integer
addressByteOffset wordBytes address
  | null (addressConstants address) = addressOffset address + addressWordOffset address * wordBytes
  | otherwise = error "Lir address constants reached a backend unresolved"

data Terminator
  = Jump !Target
  | Branch !Operand !Target !Target
  | Switch !Type !Operand ![SwitchCase] !(Maybe Target)
  | Return ![Operand]
  | TailCall !Symbol ![Operand]
  | TailCallIndirect !Operand ![Operand] !Signature
  | Trap !Text
  deriving (Eq, Show)

-- | The blocks a terminator can continue at, in the order it names them.
terminatorTargets :: Terminator -> [Target]
terminatorTargets terminator =
  case terminator of
    Jump target -> [target]
    Branch _ whenTrue whenFalse -> [whenTrue, whenFalse]
    Switch _ _ cases fallback -> map switchCaseTarget cases <> maybe [] pure fallback
    _ -> []

data Target = Target
  { targetLabel :: !Label,
    targetArguments :: ![Operand]
  }
  deriving (Eq, Show)

data SwitchCase
  = SwitchCase
      { switchCaseValue :: !Integer,
        switchCaseTarget :: !Target
      }
  | SwitchCaseConstant
      { switchCaseConstant :: !Symbol,
        switchCaseTarget :: !Target
      }
  deriving (Eq, Show)

data Operand
  = OperandVar !Var
  | OperandLiteral !Literal
  deriving (Eq, Show)

-- | Literals are untyped. The operation or the block parameter gives the type.
data Literal
  = LitInt !Integer
  | LitFloat !Double
  | LitNull
  | LitSymbol !Symbol
  deriving (Eq, Show)

-- | Run an action on every operand one operation reads, in order and with
-- repeats.
{-# INLINE forOperationOperands #-}
forOperationOperands :: (Applicative f) => (Operand -> f ()) -> Operation -> f ()
forOperationOperands act operation =
  case operation of
    Binary _ _ left right -> act left *> act right
    Unary _ _ value -> act value
    Wide _ _ left right -> act left *> act right
    QuotRemWide _ high low divisor -> act high *> act low *> act divisor
    Compare _ _ left right -> act left *> act right
    FloatBinary _ _ left right -> act left *> act right
    FloatUnary _ _ value -> act value
    Convert _ _ value _ -> act value
    PtrToInt value -> act value
    PtrFromInt value -> act value
    Select _ condition left right -> act condition *> act left *> act right
    Load _ address _ -> act (addressBase address)
    LoadInvariant _ address _ -> act (addressBase address)
    Store _ value address _ -> act value *> act (addressBase address)
    LoadAtomic _ _ address -> act (addressBase address)
    StoreAtomic _ _ value address -> act value *> act (addressBase address)
    LoadDependent _ address -> act (addressBase address)
    CompareSwap _ _ expected replacement address -> act expected *> act replacement *> act (addressBase address)
    AtomicRmw _ _ _ value address -> act value *> act (addressBase address)
    Fence _ -> pure ()
    PtrAdd base offset -> act base *> act offset
    StackAlloc _ _ -> pure ()
    GlobalGet _ -> pure ()
    GlobalSet _ value -> act value
    Call _ arguments -> traverse_ act arguments
    CallIndirect callee arguments _ -> act callee *> traverse_ act arguments

-- | Run an action on every operand one terminator reads, in order and with
-- repeats.
{-# INLINE forTerminatorOperands #-}
forTerminatorOperands :: (Applicative f) => (Operand -> f ()) -> Terminator -> f ()
forTerminatorOperands act terminator =
  case terminator of
    Jump jump -> traverse_ act (targetArguments jump)
    Branch condition whenTrue whenFalse ->
      act condition *> traverse_ act (targetArguments whenTrue) *> traverse_ act (targetArguments whenFalse)
    Switch _ scrutinee cases fallback ->
      act scrutinee
        *> traverse_ (traverse_ act . targetArguments . switchCaseTarget) cases
        *> traverse_ (traverse_ act . targetArguments) fallback
    Return values -> traverse_ act values
    TailCall _ arguments -> traverse_ act arguments
    TailCallIndirect callee arguments _ -> act callee *> traverse_ act arguments
    Trap _ -> pure ()

-- | Rewrite the operands an operation reads.
mapOperation :: (Operand -> Operand) -> Operation -> Operation
mapOperation operand operation =
  case operation of
    Binary op ty left right -> Binary op ty (operand left) (operand right)
    Unary op ty value -> Unary op ty (operand value)
    Wide op ty left right -> Wide op ty (operand left) (operand right)
    QuotRemWide ty high low divisor -> QuotRemWide ty (operand high) (operand low) (operand divisor)
    Compare op ty left right -> Compare op ty (operand left) (operand right)
    FloatBinary op ty left right -> FloatBinary op ty (operand left) (operand right)
    FloatUnary op ty value -> FloatUnary op ty (operand value)
    Convert op from value to -> Convert op from (operand value) to
    PtrToInt value -> PtrToInt (operand value)
    PtrFromInt value -> PtrFromInt (operand value)
    Select ty condition whenTrue whenFalse -> Select ty (operand condition) (operand whenTrue) (operand whenFalse)
    Load ty address align -> Load ty (mapAddress operand address) align
    LoadInvariant ty address align -> LoadInvariant ty (mapAddress operand address) align
    Store ty value address align -> Store ty (operand value) (mapAddress operand address) align
    LoadAtomic order ty address -> LoadAtomic order ty (mapAddress operand address)
    StoreAtomic order ty value address -> StoreAtomic order ty (operand value) (mapAddress operand address)
    LoadDependent ty address -> LoadDependent ty (mapAddress operand address)
    CompareSwap order ty expected replacement address ->
      CompareSwap order ty (operand expected) (operand replacement) (mapAddress operand address)
    AtomicRmw op order ty value address -> AtomicRmw op order ty (operand value) (mapAddress operand address)
    Fence _ -> operation
    PtrAdd base offset -> PtrAdd (operand base) (operand offset)
    StackAlloc _ _ -> operation
    GlobalGet _ -> operation
    GlobalSet symbol value -> GlobalSet symbol (operand value)
    Call symbol arguments -> Call symbol (map operand arguments)
    CallIndirect callee arguments signature -> CallIndirect (operand callee) (map operand arguments) signature

-- | Rewrite the base of an address.
mapAddress :: (Operand -> Operand) -> Address -> Address
mapAddress operand address = address {addressBase = operand (addressBase address)}

-- | Rewrite the operands and the labels of a terminator. @onReturn@ gives
-- the terminator that a @return@ becomes, which is @Return@ everywhere
-- except at a call site that is waiting for the results.
mapTerminator :: (Operand -> Operand) -> (Label -> Label) -> ([Operand] -> Terminator) -> Terminator -> Terminator
mapTerminator operand label onReturn terminator =
  case terminator of
    Jump target -> Jump (mapTarget target)
    Branch condition whenTrue whenFalse -> Branch (operand condition) (mapTarget whenTrue) (mapTarget whenFalse)
    Switch ty scrutinee cases fallback -> Switch ty (operand scrutinee) (map mapCase cases) (fmap mapTarget fallback)
    Return values -> onReturn (map operand values)
    TailCall symbol arguments -> TailCall symbol (map operand arguments)
    TailCallIndirect callee arguments signature -> TailCallIndirect (operand callee) (map operand arguments) signature
    Trap message -> Trap message
  where
    mapTarget target = Target (label (targetLabel target)) (map operand (targetArguments target))
    mapCase switchCase =
      let target = mapTarget (switchCaseTarget switchCase)
       in case switchCase of
            SwitchCase value _ -> SwitchCase value target
            SwitchCaseConstant symbol _ -> SwitchCaseConstant symbol target
