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
    symbolText,
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
    Address (..),
    Terminator (..),
    terminatorTargets,
    Target (..),
    SwitchCase (..),
    Operand (..),
    Literal (..),
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text

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
--
-- It is bytes, not text. A symbol exists to be written into an object file
-- and read back by a linker, so it never leaves the byte world except to be
-- printed, and carrying it as 'Text' meant decoding every name on the way in
-- and encoding it again on the way out.
--
-- It is 'ShortByteString' rather than 'ByteString' because a Lir module holds
-- millions of these at once and what bounds the compiler is peak residency,
-- not allocation. A 'ByteString' is ten heap words -- a constructor, a
-- 'ForeignPtr', its contents box and the array header -- against 'Text' at six
-- and this at four, and measured on an @llvm@ install of @colour@ the
-- 'ByteString' spelling cost 117 MB of peak residency over 'Text'.
newtype Symbol = Symbol {unSymbol :: ShortByteString}
  deriving (Eq, Ord, Show)

-- | A symbol in a diagnostic.
--
-- Every symbol is ASCII: 'Aihc.Native.renderLinkedFunctionSymbol' escapes
-- each byte that is not an ASCII alphanumeric, and the symbols the compiler
-- builds itself are ASCII literals and decimal digits. So this widens the
-- bytes rather than decoding them -- one character per byte, total, and
-- never a replacement character. A message is text, which is why this
-- exists; the Lir text format spells a symbol out as bytes instead.
symbolText :: Symbol -> Text
symbolText = Text.decodeLatin1 . SBS.fromShort . unSymbol

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

data Linkage = Internal | Export
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

-- | @const \@name = value@: a named integer. A reference to it stands
-- wherever an integer literal does, and 'Aihc.Lir.Resolve' substitutes the
-- value before a backend sees the module.
data Constant = Constant
  { constantName :: !Symbol,
    constantValue :: !Integer
  }
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
    blockTerminator :: !Terminator
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
  | Compare !CompareOp !Type !Operand !Operand
  | FloatBinary !FloatBinaryOp !Type !Operand !Operand
  | FloatUnary !FloatUnaryOp !Type !Operand
  | -- | @Convert op from operand to@.
    Convert !ConvertOp !Type !Operand !Type
  | PtrToInt !Operand
  | PtrFromInt !Operand
  | Select !Type !Operand !Operand !Operand
  | Load !Type !Address !Integer
  | Store !Type !Operand !Address !Integer
  | PtrAdd !Operand !Operand
  | StackAlloc !Integer !Integer
  | GlobalGet !Symbol
  | GlobalSet !Symbol !Operand
  | Call !Symbol ![Operand]
  | CallIndirect !Operand ![Operand] !Signature
  deriving (Eq, Show)

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

-- | A base pointer and a constant byte offset.
data Address = Address
  { addressBase :: !Operand,
    addressOffset :: !Integer
  }
  deriving (Eq, Show)

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
