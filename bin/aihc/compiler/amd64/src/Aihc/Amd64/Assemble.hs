-- | Assemble the compiler AMD64 vocabulary without an external assembler.
module Aihc.Amd64.Assemble
  ( Amd64Statement,
    Amd64Instruction (..),
    Amd64Register (..),
    Amd64Memory (..),
    Amd64Address (..),
    Amd64Rm (..),
    Amd64MoveSource (..),
    Amd64StoreSource (..),
    Amd64BinarySource (..),
    Amd64JumpTarget (..),
    Amd64Condition (..),
    assembleElf,
    amd64Align,
    amd64Bytes,
    amd64Global,
    amd64Instruction,
    amd64Label,
    amd64Quad,
    amd64QuadSymbol,
    amd64Section,
  )
where

import Aihc.Native.Elf (writeAmd64Elf)
import Aihc.Native.Object
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word32, Word64, Word8)

data Amd64Statement
  = Amd64Section !SectionRole
  | Amd64Align !Int
  | Amd64Global !Text
  | Amd64Label !Text
  | Amd64Quad !Word64
  | Amd64QuadSymbol !Text
  | Amd64Bytes !ByteString
  | Amd64Instruction ![Item]

data Amd64Register
  = RAX
  | RCX
  | RDX
  | RBX
  | RSP
  | RBP
  | RSI
  | RDI
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  | EAX
  | ECX
  | EDX
  | EBX
  | ESP
  | EBP
  | ESI
  | EDI
  | R8D
  | R9D
  | R10D
  | R11D
  | R12D
  | R13D
  | R14D
  | R15D
  | AL
  | CL
  | DL
  | BL
  | SPL
  | BPL
  | SIL
  | DIL
  | R8B
  | R9B
  | R10B
  | R11B
  | R12B
  | R13B
  | R14B
  | R15B
  deriving (Eq, Ord, Show)

data Amd64Memory = Amd64Memory !Amd64Register !Int64
  deriving (Eq, Show)

data Amd64Address
  = Amd64MemoryAddress !Amd64Memory
  | Amd64RipAddress !Text
  deriving (Eq, Show)

data Amd64Rm
  = Amd64RmRegister !Amd64Register
  | Amd64RmMemory !Amd64Memory
  deriving (Eq, Show)

data Amd64MoveSource
  = Amd64MoveRegister !Amd64Register
  | Amd64MoveMemory !Amd64Memory
  | Amd64MoveImmediate !Integer
  deriving (Eq, Show)

data Amd64StoreSource
  = Amd64StoreRegister !Amd64Register
  | Amd64StoreImmediate !Integer
  deriving (Eq, Show)

data Amd64BinarySource
  = Amd64BinaryRegister !Amd64Register
  | Amd64BinaryImmediate !Integer
  deriving (Eq, Show)

data Amd64JumpTarget
  = Amd64JumpLabel !Text
  | Amd64JumpRegister !Amd64Register
  deriving (Eq, Show)

data Amd64Condition
  = AmdOverflow
  | AmdCarry
  | AmdBelow
  | AmdAboveOrEqual
  | AmdEqual
  | AmdNotEqual
  | AmdBelowOrEqual
  | AmdAbove
  | AmdLess
  | AmdGreaterOrEqual
  | AmdLessOrEqual
  | AmdGreater
  deriving (Eq, Ord, Show)

data Amd64Instruction
  = AmdRet
  | AmdUd2
  | AmdPush !Amd64Register
  | AmdPop !Amd64Register
  | AmdCall !Text
  | AmdJmp !Amd64JumpTarget
  | AmdJe !Text
  | AmdJne !Text
  | AmdMov !Amd64Register !Amd64MoveSource
  | AmdStore !Amd64Memory !Amd64StoreSource
  | AmdMovsxd !Amd64Register !Amd64Rm
  | -- | Sign-extend a byte register into a 64-bit register.
    AmdMovsxByte !Amd64Register !Amd64Rm
  | -- | Sign-extend the low 16 bits of a register into a 64-bit register.
    AmdMovsxWord !Amd64Register !Amd64Rm
  | AmdMovzx !Amd64Register !Amd64Rm
  | -- | Zero-extend the low 16 bits of a register into a 64-bit register.
    AmdMovzxWord !Amd64Register !Amd64Rm
  | AmdLea !Amd64Register !Amd64Address
  | AmdAdd !Amd64Rm !Amd64BinarySource
  | AmdSub !Amd64Rm !Amd64BinarySource
  | AmdAnd !Amd64Rm !Amd64BinarySource
  | AmdOr !Amd64Rm !Amd64BinarySource
  | AmdXor !Amd64Rm !Amd64BinarySource
  | AmdImul !Amd64Register !Amd64Rm
  | AmdCmp !Amd64Rm !Amd64BinarySource
  | AmdTest !Amd64Rm !Amd64Register
  | AmdShl !Amd64Rm
  | AmdShr !Amd64Rm
  | AmdNot !Amd64Rm
  | AmdMul !Amd64Rm
  | AmdDiv !Amd64Rm
  | AmdSet !Amd64Condition !Amd64Rm

assembleElf :: [Amd64Statement] -> Either ObjectError BL.ByteString
assembleElf statements = foldl' applyStatement (Right emptyDraft) statements >>= layoutDraft >>= writeAmd64Elf

amd64Section :: SectionRole -> Amd64Statement
amd64Section = Amd64Section

amd64Align :: Int -> Amd64Statement
amd64Align = Amd64Align

amd64Global :: Text -> Amd64Statement
amd64Global = Amd64Global

amd64Label :: Text -> Amd64Statement
amd64Label = Amd64Label

amd64Quad :: Word64 -> Amd64Statement
amd64Quad = Amd64Quad

amd64QuadSymbol :: Text -> Amd64Statement
amd64QuadSymbol = Amd64QuadSymbol

amd64Bytes :: ByteString -> Amd64Statement
amd64Bytes = Amd64Bytes

amd64Instruction :: Amd64Instruction -> Amd64Statement
amd64Instruction = Amd64Instruction . encodeInstruction

applyStatement :: Either ObjectError Draft -> Amd64Statement -> Either ObjectError Draft
applyStatement result statement = do
  draft <- result
  case statement of
    Amd64Section role -> pure (selectSection role draft)
    Amd64Align alignment -> addItem (Align alignment (alignmentFill draft)) draft
    Amd64Global symbol -> pure (addGlobal symbol draft)
    Amd64Label symbol -> addItem (Label symbol) draft
    Amd64Quad value -> addItem (Bytes (word64Bytes value)) draft
    Amd64QuadSymbol symbol -> addItem (Apply (Fixup Absolute64 symbol 0 (BS.replicate 8 0))) draft
    Amd64Bytes value
      | BS.null value -> pure draft
      | otherwise -> addItem (Bytes value) draft
    Amd64Instruction items -> foldl' (>>=) (pure draft) [addItem item | item <- items]

alignmentFill :: Draft -> ByteString
alignmentFill draft
  | draftCurrentSection draft == Just TextSection = BS.singleton 0x90
  | otherwise = BS.singleton 0

data Register = Register
  { registerNumber :: !Word8,
    registerWidth :: !Int
  }
  deriving (Eq, Show)

data Operand
  = RegisterOperand !Register
  | MemoryOperand !Register !Int64
  deriving (Eq, Show)

registerInfo :: Amd64Register -> Register
registerInfo register =
  case register of
    RAX -> q 0
    RCX -> q 1
    RDX -> q 2
    RBX -> q 3
    RSP -> q 4
    RBP -> q 5
    RSI -> q 6
    RDI -> q 7
    R8 -> q 8
    R9 -> q 9
    R10 -> q 10
    R11 -> q 11
    R12 -> q 12
    R13 -> q 13
    R14 -> q 14
    R15 -> q 15
    EAX -> d 0
    ECX -> d 1
    EDX -> d 2
    EBX -> d 3
    ESP -> d 4
    EBP -> d 5
    ESI -> d 6
    EDI -> d 7
    R8D -> d 8
    R9D -> d 9
    R10D -> d 10
    R11D -> d 11
    R12D -> d 12
    R13D -> d 13
    R14D -> d 14
    R15D -> d 15
    AL -> b 0
    CL -> b 1
    DL -> b 2
    BL -> b 3
    SPL -> b 4
    BPL -> b 5
    SIL -> b 6
    DIL -> b 7
    R8B -> b 8
    R9B -> b 9
    R10B -> b 10
    R11B -> b 11
    R12B -> b 12
    R13B -> b 13
    R14B -> b 14
    R15B -> b 15
  where
    q number = Register number 64
    d number = Register number 32
    b number = Register number 8

rmOperand :: Amd64Rm -> Operand
rmOperand value =
  case value of
    Amd64RmRegister register -> RegisterOperand (registerInfo register)
    Amd64RmMemory memory -> memoryOperand memory

memoryOperand :: Amd64Memory -> Operand
memoryOperand (Amd64Memory base offset) = MemoryOperand (registerInfo base) offset

encodeInstruction :: Amd64Instruction -> [Item]
encodeInstruction instruction =
  case instruction of
    AmdRet -> bytes [0xc3]
    AmdUd2 -> bytes [0x0f, 0x0b]
    AmdPush register -> encodePushPop False register
    AmdPop register -> encodePushPop True register
    AmdCall target -> relativeBranch [0xe8] X86Plt32 target
    AmdJmp (Amd64JumpLabel target) -> relativeBranch [0xe9] X86Pc32 target
    AmdJmp (Amd64JumpRegister register) -> encodeGroup True [0xff] 4 (RegisterOperand (registerInfo register)) []
    AmdJe target -> relativeBranch [0x0f, 0x84] X86Pc32 target
    AmdJne target -> relativeBranch [0x0f, 0x85] X86Pc32 target
    AmdMov destination source -> encodeMove destination source
    AmdStore destination source -> encodeStore destination source
    AmdMovsxd destination source -> encodeRegisterSource True [0x63] destination source
    AmdMovsxByte destination source -> encodeRegisterSource True [0x0f, 0xbe] destination source
    AmdMovsxWord destination source -> encodeRegisterSource True [0x0f, 0xbf] destination source
    AmdMovzx destination source -> encodeRegisterSource True [0x0f, 0xb6] destination source
    AmdMovzxWord destination source -> encodeRegisterSource True [0x0f, 0xb7] destination source
    AmdLea destination source -> encodeLea destination source
    AmdAdd destination source -> encodeBinary [0x01] 0 destination source
    AmdSub destination source -> encodeBinary [0x29] 5 destination source
    AmdAnd destination source -> encodeBinary [0x21] 4 destination source
    AmdOr destination source -> encodeBinary [0x09] 1 destination source
    AmdXor destination source -> encodeBinary [0x31] 6 destination source
    AmdImul destination source -> encodeRegisterSource True [0x0f, 0xaf] destination source
    AmdCmp left right -> encodeCompare left right
    AmdTest left right -> encodeRm (registerWidth register == 64) [0x85] (registerNumber register) (rmOperand left) False []
      where
        register = registerInfo right
    AmdShl destination -> encodeGroup True [0xd3] 4 (rmOperand destination) []
    AmdShr destination -> encodeGroup True [0xd3] 5 (rmOperand destination) []
    AmdNot destination -> encodeGroup True [0xf7] 2 (rmOperand destination) []
    AmdMul source -> encodeGroup True [0xf7] 4 (rmOperand source) []
    AmdDiv source -> encodeGroup True [0xf7] 6 (rmOperand source) []
    AmdSet condition destination -> encodeGroupWithWidth False [0x0f, 0x90 + conditionCode condition] 0 (rmOperand destination) [] True

encodePushPop :: Bool -> Amd64Register -> [Item]
encodePushPop popValue source =
  let register = registerInfo source
      prefix = [0x41 | registerNumber register >= 8]
      opcode = (if popValue then 0x58 else 0x50) + registerNumber register .&. 7
   in bytes (prefix <> [opcode])

encodeMove :: Amd64Register -> Amd64MoveSource -> [Item]
encodeMove destination source =
  case source of
    Amd64MoveImmediate immediate ->
      let number = registerNumber destinationRegister
          prefix
            | registerWidth destinationRegister == 64 = [rex True False False (number >= 8)]
            | number >= 8 = [rex False False False True]
            | otherwise = []
          immediateBytes = if registerWidth destinationRegister == 64 then word64List (fromIntegral immediate) else word32List (fromIntegral immediate)
       in bytes (prefix <> [0xb8 + number .&. 7] <> immediateBytes)
    Amd64MoveRegister sourceValue ->
      let sourceRegister = registerInfo sourceValue
       in encodeRm (registerWidth destinationRegister == 64) [0x89] (registerNumber sourceRegister) (RegisterOperand destinationRegister) False []
    Amd64MoveMemory memory ->
      encodeRm (registerWidth destinationRegister == 64) [0x8b] (registerNumber destinationRegister) (memoryOperand memory) False []
  where
    destinationRegister = registerInfo destination

encodeStore :: Amd64Memory -> Amd64StoreSource -> [Item]
encodeStore destination source =
  case source of
    Amd64StoreRegister sourceValue ->
      let sourceRegister = registerInfo sourceValue
       in encodeRm (registerWidth sourceRegister == 64) [0x89] (registerNumber sourceRegister) (memoryOperand destination) False []
    Amd64StoreImmediate immediate -> encodeGroup True [0xc7] 0 (memoryOperand destination) (word32List (fromIntegral immediate))

encodeRegisterSource :: Bool -> [Word8] -> Amd64Register -> Amd64Rm -> [Item]
encodeRegisterSource width64 destinationOpcode destinationSource source =
  let destination = registerInfo destinationSource
   in encodeRm width64 destinationOpcode (registerNumber destination) (rmOperand source) False []

encodeLea :: Amd64Register -> Amd64Address -> [Item]
encodeLea destinationSource source =
  case source of
    Amd64RipAddress target ->
      let prefix = rex True (registerNumber destination >= 8) False False
          modrm = ((registerNumber destination .&. 7) `shiftL` 3) .|. 5
       in [Bytes (BS.pack [prefix, 0x8d, modrm]), Apply (Fixup X86Pc32 target (-4) (BS.replicate 4 0))]
    Amd64MemoryAddress memory -> encodeRm True [0x8d] (registerNumber destination) (memoryOperand memory) False []
  where
    destination = registerInfo destinationSource

encodeBinary :: [Word8] -> Word8 -> Amd64Rm -> Amd64BinarySource -> [Item]
encodeBinary opcode immediateGroup destination source =
  case source of
    Amd64BinaryRegister registerSource ->
      let sourceRegister = registerInfo registerSource
       in encodeRm (registerWidth sourceRegister == 64) opcode (registerNumber sourceRegister) (rmOperand destination) False []
    Amd64BinaryImmediate immediate -> encodeGroup True [0x81] immediateGroup (rmOperand destination) (word32List (fromIntegral immediate))

encodeCompare :: Amd64Rm -> Amd64BinarySource -> [Item]
encodeCompare left right =
  case right of
    Amd64BinaryRegister source ->
      let register = registerInfo source
       in encodeRm (registerWidth register == 64) [0x39] (registerNumber register) (rmOperand left) False []
    Amd64BinaryImmediate immediate -> encodeGroup (operandUses64Bits (rmOperand left)) [0x81] 7 (rmOperand left) (word32List (fromIntegral immediate))

operandUses64Bits :: Operand -> Bool
operandUses64Bits operand =
  case operand of
    RegisterOperand register -> registerWidth register == 64
    MemoryOperand {} -> True

encodeGroup :: Bool -> [Word8] -> Word8 -> Operand -> [Word8] -> [Item]
encodeGroup width64 opcode group operand suffix = encodeGroupWithWidth width64 opcode group operand suffix False

encodeGroupWithWidth :: Bool -> [Word8] -> Word8 -> Operand -> [Word8] -> Bool -> [Item]
encodeGroupWithWidth width64 opcode group operand suffix forceByteRex = encodeRm width64 opcode group operand forceByteRex suffix

encodeRm :: Bool -> [Word8] -> Word8 -> Operand -> Bool -> [Word8] -> [Item]
encodeRm width64 opcode regField operand forceByteRex suffix =
  case operand of
    RegisterOperand register ->
      let rexByte = rex width64 (regField >= 8) False (registerNumber register >= 8)
          needRex = width64 || regField >= 8 || registerNumber register >= 8 || (forceByteRex && registerNumber register >= 4)
          modrm = 0xc0 .|. (regField .&. 7) `shiftL` 3 .|. registerNumber register .&. 7
       in bytes ([rexByte | needRex] <> opcode <> [modrm] <> suffix)
    MemoryOperand base displacement ->
      let baseNumber = registerNumber base
          baseLow = baseNumber .&. 7
          (mode, displacementBytes) :: (Word8, [Word8])
            | displacement == 0 && baseLow /= 5 = (0, [])
            | displacement >= -128 && displacement <= 127 = (1, [fromIntegral displacement])
            | otherwise = (2, word32List (fromIntegral displacement))
          actualDisplacement = if mode == 0 && baseLow == 5 then [0] else displacementBytes
          actualMode = if mode == 0 && baseLow == 5 then 1 else mode
          useSib = baseLow == 4
          rm = if useSib then 4 else baseLow
          modrm = fromIntegral actualMode `shiftL` 6 .|. (regField .&. 7) `shiftL` 3 .|. rm
          sib = [0x24 .|. baseLow | useSib]
          rexByte = rex width64 (regField >= 8) False (baseNumber >= 8)
       in bytes ([rexByte | width64 || regField >= 8 || baseNumber >= 8] <> opcode <> [modrm] <> sib <> actualDisplacement <> suffix)

relativeBranch :: [Word8] -> FixupKind -> Text -> [Item]
relativeBranch opcode kind target = [Bytes (BS.pack opcode), Apply (Fixup kind target (-4) (BS.replicate 4 0))]

conditionCode :: Amd64Condition -> Word8
conditionCode condition =
  case condition of
    AmdOverflow -> 0
    AmdCarry -> 2
    AmdBelow -> 2
    AmdAboveOrEqual -> 3
    AmdEqual -> 4
    AmdNotEqual -> 5
    AmdBelowOrEqual -> 6
    AmdAbove -> 7
    AmdLess -> 12
    AmdGreaterOrEqual -> 13
    AmdLessOrEqual -> 14
    AmdGreater -> 15

rex :: Bool -> Bool -> Bool -> Bool -> Word8
rex width register index base =
  0x40
    .|. (if width then 8 else 0)
    .|. (if register then 4 else 0)
    .|. (if index then 2 else 0)
    .|. (if base then 1 else 0)

bytes :: [Word8] -> [Item]
bytes value = [Bytes (BS.pack value)]

word32List :: Word32 -> [Word8]
word32List value =
  [ fromIntegral value,
    fromIntegral (value `shiftR` 8),
    fromIntegral (value `shiftR` 16),
    fromIntegral (value `shiftR` 24)
  ]

word64List :: Word64 -> [Word8]
word64List value = word32List (fromIntegral value) <> word32List (fromIntegral (value `shiftR` 32))

word64Bytes :: Word64 -> ByteString
word64Bytes = BS.pack . word64List
