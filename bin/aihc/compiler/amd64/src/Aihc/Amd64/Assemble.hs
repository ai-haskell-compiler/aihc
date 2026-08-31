{-# LANGUAGE OverloadedStrings #-}

-- | Assemble the compiler AMD64 vocabulary without an external assembler.
module Aihc.Amd64.Assemble
  ( Amd64Statement,
    Amd64Opcode (..),
    assembleElf,
    amd64Align,
    amd64Bytes,
    amd64Global,
    amd64Instruction,
    amd64Label,
    amd64Quad,
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
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32, Word64, Word8)
import Text.Read (readMaybe)

data Amd64Statement
  = Amd64Section !SectionRole
  | Amd64Align !Int
  | Amd64Global !Text
  | Amd64Label !Text
  | Amd64Quad !Text
  | Amd64Bytes !ByteString
  | Amd64Instruction (Either ObjectError [Item])

data Amd64Opcode
  = AmdRet
  | AmdUd2
  | AmdPush
  | AmdPop
  | AmdCall
  | AmdJmp
  | AmdJe
  | AmdJz
  | AmdJne
  | AmdMov
  | AmdMovsxd
  | AmdMovzx
  | AmdLea
  | AmdAdd
  | AmdSub
  | AmdAnd
  | AmdOr
  | AmdXor
  | AmdImul
  | AmdCmp
  | AmdTest
  | AmdShl
  | AmdShr
  | AmdNot
  | AmdMul
  | AmdDiv
  | AmdSeto
  | AmdSetc
  | AmdSetb
  | AmdSetae
  | AmdSete
  | AmdSetne
  | AmdSetbe
  | AmdSeta
  | AmdSetl
  | AmdSetge
  | AmdSetle
  | AmdSetg

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

amd64Quad :: Text -> Amd64Statement
amd64Quad = Amd64Quad

amd64Bytes :: ByteString -> Amd64Statement
amd64Bytes = Amd64Bytes

amd64Instruction :: Amd64Opcode -> [Text] -> Amd64Statement
amd64Instruction opcode = Amd64Instruction . encodeOperation opcode

applyStatement :: Either ObjectError Draft -> Amd64Statement -> Either ObjectError Draft
applyStatement result statement = do
  draft <- result
  case statement of
    Amd64Section role -> pure (selectSection role draft)
    Amd64Align alignment -> addItem (Align alignment (alignmentFill draft)) draft
    Amd64Global symbol -> pure (addGlobal symbol draft)
    Amd64Label symbol -> addItem (Label symbol) draft
    Amd64Quad value -> parseQuad value >>= \item -> addItem item draft
    Amd64Bytes value
      | BS.null value -> pure draft
      | otherwise -> addItem (Bytes value) draft
    Amd64Instruction encoded ->
      encoded >>= \items -> foldl' (>>=) (pure draft) [addItem item | item <- items]

opcodeText :: Amd64Opcode -> Text
opcodeText opcode =
  case opcode of
    AmdRet -> "ret"
    AmdUd2 -> "ud2"
    AmdPush -> "push"
    AmdPop -> "pop"
    AmdCall -> "call"
    AmdJmp -> "jmp"
    AmdJe -> "je"
    AmdJz -> "jz"
    AmdJne -> "jne"
    AmdMov -> "mov"
    AmdMovsxd -> "movsxd"
    AmdMovzx -> "movzx"
    AmdLea -> "lea"
    AmdAdd -> "add"
    AmdSub -> "sub"
    AmdAnd -> "and"
    AmdOr -> "or"
    AmdXor -> "xor"
    AmdImul -> "imul"
    AmdCmp -> "cmp"
    AmdTest -> "test"
    AmdShl -> "shl"
    AmdShr -> "shr"
    AmdNot -> "not"
    AmdMul -> "mul"
    AmdDiv -> "div"
    AmdSeto -> "seto"
    AmdSetc -> "setc"
    AmdSetb -> "setb"
    AmdSetae -> "setae"
    AmdSete -> "sete"
    AmdSetne -> "setne"
    AmdSetbe -> "setbe"
    AmdSeta -> "seta"
    AmdSetl -> "setl"
    AmdSetge -> "setge"
    AmdSetle -> "setle"
    AmdSetg -> "setg"

alignmentFill :: Draft -> ByteString
alignmentFill draft
  | draftCurrentSection draft == Just TextSection = BS.singleton 0x90
  | otherwise = BS.singleton 0

parseQuad :: Text -> Either ObjectError Item
parseQuad value =
  case readInteger value of
    Just integer -> pure (Bytes (word64Bytes (fromIntegral integer)))
    Nothing -> pure (Apply (Fixup Absolute64 value 0 (BS.replicate 8 0)))

data Register = Register
  { registerNumber :: !Word8,
    registerWidth :: !Int
  }
  deriving (Eq, Show)

data Operand
  = RegisterOperand !Register
  | MemoryOperand !Register !Int64
  | RipOperand !Text
  | ImmediateOperand !Integer
  deriving (Eq, Show)

parseRegister :: Text -> Maybe Register
parseRegister name = lookup name registerNames
  where
    registerNames =
      zipWith (\value number -> (value, Register number 64)) ["rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi"] [0 ..]
        <> [("r" <> T.pack (show number), Register (fromIntegral number) 64) | number <- [8 :: Int .. 15]]
        <> zipWith (\value number -> (value, Register number 32)) ["eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi"] [0 ..]
        <> [("r" <> T.pack (show number) <> "d", Register (fromIntegral number) 32) | number <- [8 :: Int .. 15]]
        <> zipWith (\value number -> (value, Register number 8)) ["al", "cl", "dl", "bl", "spl", "bpl", "sil", "dil"] [0 ..]
        <> [("r" <> T.pack (show number) <> "b", Register (fromIntegral number) 8) | number <- [8 :: Int .. 15]]

parseOperand :: Text -> Either ObjectError Operand
parseOperand source
  | Just register <- parseRegister value = pure (RegisterOperand register)
  | Just immediate <- readInteger value = pure (ImmediateOperand immediate)
  | Just inside <- T.stripPrefix "[" value >>= T.stripSuffix "]" = parseMemory inside
  | otherwise = Left (ObjectInvalidInput source)
  where
    value = fromMaybePrefix "DWORD PTR " (fromMaybePrefix "QWORD PTR " (T.strip source))
    fromMaybePrefix prefix text = fromMaybe text (T.stripPrefix prefix text)
    parseMemory inside =
      case map T.strip (T.splitOn "+" inside) of
        ["rip", symbol] -> pure (RipOperand symbol)
        [base] -> maybe (Left (ObjectInvalidInput source)) (\register -> pure (MemoryOperand register 0)) (parseRegister base)
        [base, offset] -> do
          register <- maybe (Left (ObjectInvalidInput source)) pure (parseRegister base)
          displacement <- maybe (Left (ObjectInvalidInput source)) pure (readInteger offset)
          pure (MemoryOperand register (fromIntegral displacement))
        _ -> Left (ObjectInvalidInput source)

encodeOperation :: Amd64Opcode -> [Text] -> Either ObjectError [Item]
encodeOperation operation operands =
  case (operation, operands) of
    (AmdRet, []) -> bytes [0xc3]
    (AmdUd2, []) -> bytes [0x0f, 0x0b]
    (AmdPush, [source]) -> encodePushPop False source
    (AmdPop, [source]) -> encodePushPop True source
    (AmdCall, [target]) -> relativeBranch [0xe8] X86Plt32 target
    (AmdJmp, [target])
      | Just register <- parseRegister target -> encodeGroup True [0xff] 4 (RegisterOperand register) []
      | otherwise -> relativeBranch [0xe9] X86Pc32 target
    (conditional, [target]) | Just condition <- jumpCondition conditional -> relativeBranch [0x0f, 0x80 + condition] X86Pc32 target
    (AmdMov, [destination, source]) -> do
      destinationOperand <- parseOperand destination
      sourceOperand <- parseOperand source
      encodeMove destinationOperand sourceOperand
    (AmdMovsxd, [destination, source]) -> encodeRegisterSource True [0x63] destination source
    (AmdMovzx, [destination, source]) -> encodeRegisterSource True [0x0f, 0xb6] destination source
    (AmdLea, [destination, source]) -> encodeLea destination source
    (AmdAdd, [destination, source]) -> encodeBinary [0x01] 0 destination source
    (AmdSub, [destination, source]) -> encodeBinary [0x29] 5 destination source
    (AmdAnd, [destination, source]) -> encodeBinary [0x21] 4 destination source
    (AmdOr, [destination, source]) -> encodeBinary [0x09] 1 destination source
    (AmdXor, [destination, source]) -> encodeBinary [0x31] 6 destination source
    (AmdImul, [destination, source]) -> encodeRegisterSource True [0x0f, 0xaf] destination source
    (AmdCmp, [left, right]) -> encodeCompare left right
    (AmdTest, [left, right]) -> encodeRegisterBinary [0x85] left right
    (AmdShl, [destination, "cl"]) -> parseOperand destination >>= \operand -> encodeGroup True [0xd3] 4 operand []
    (AmdShr, [destination, "cl"]) -> parseOperand destination >>= \operand -> encodeGroup True [0xd3] 5 operand []
    (AmdNot, [destination]) -> parseOperand destination >>= \operand -> encodeGroup True [0xf7] 2 operand []
    (AmdMul, [source]) -> parseOperand source >>= \operand -> encodeGroup True [0xf7] 4 operand []
    (AmdDiv, [source]) -> parseOperand source >>= \operand -> encodeGroup True [0xf7] 6 operand []
    (setOperation, [destination]) | Just code <- setCondition setOperation -> do
      operand <- parseOperand destination
      encodeGroupWithWidth False [0x0f, 0x90 + code] 0 operand [] True
    _ -> Left (ObjectInvalidInput (opcodeText operation <> " " <> T.intercalate ", " operands))

encodePushPop :: Bool -> Text -> Either ObjectError [Item]
encodePushPop popValue source =
  case parseRegister source of
    Nothing -> Left (ObjectInvalidInput source)
    Just register ->
      let prefix = [0x41 | registerNumber register >= 8]
          opcode = (if popValue then 0x58 else 0x50) + registerNumber register .&. 7
       in bytes (prefix <> [opcode])

encodeMove :: Operand -> Operand -> Either ObjectError [Item]
encodeMove destination source =
  case (destination, source) of
    (RegisterOperand destinationRegister, ImmediateOperand immediate) ->
      let number = registerNumber destinationRegister
          prefix
            | registerWidth destinationRegister == 64 = [rex True False False (number >= 8)]
            | number >= 8 = [rex False False False True]
            | otherwise = []
          immediateBytes = if registerWidth destinationRegister == 64 then word64List (fromIntegral immediate) else word32List (fromIntegral immediate)
       in bytes (prefix <> [0xb8 + number .&. 7] <> immediateBytes)
    (RegisterOperand destinationRegister, RegisterOperand sourceRegister) ->
      encodeRm (registerWidth destinationRegister == 64) [0x89] (registerNumber sourceRegister) destination False []
    (RegisterOperand destinationRegister, MemoryOperand {}) ->
      encodeRm (registerWidth destinationRegister == 64) [0x8b] (registerNumber destinationRegister) source False []
    (MemoryOperand {}, RegisterOperand sourceRegister) ->
      encodeRm (registerWidth sourceRegister == 64) [0x89] (registerNumber sourceRegister) destination False []
    (MemoryOperand {}, ImmediateOperand immediate) -> encodeGroup True [0xc7] 0 destination (word32List (fromIntegral immediate))
    _ -> Left (ObjectInvalidInput "mov operands")

encodeRegisterSource :: Bool -> [Word8] -> Text -> Text -> Either ObjectError [Item]
encodeRegisterSource width64 destinationOpcode destinationSource sourceSource = do
  destination <- maybe (Left (ObjectInvalidInput destinationSource)) pure (parseRegister destinationSource)
  source <- parseOperand sourceSource
  encodeRm width64 destinationOpcode (registerNumber destination) source False []

encodeLea :: Text -> Text -> Either ObjectError [Item]
encodeLea destinationSource sourceSource = do
  destination <- maybe (Left (ObjectInvalidInput destinationSource)) pure (parseRegister destinationSource)
  source <- parseOperand sourceSource
  case source of
    RipOperand target -> do
      let prefix = rex True (registerNumber destination >= 8) False False
          modrm = ((registerNumber destination .&. 7) `shiftL` 3) .|. 5
      pure [Bytes (BS.pack [prefix, 0x8d, modrm]), Apply (Fixup X86Pc32 target (-4) (BS.replicate 4 0))]
    _ -> encodeRm True [0x8d] (registerNumber destination) source False []

encodeBinary :: [Word8] -> Word8 -> Text -> Text -> Either ObjectError [Item]
encodeBinary opcode immediateGroup destinationSource sourceSource = do
  destination <- parseOperand destinationSource
  source <- parseOperand sourceSource
  case source of
    RegisterOperand sourceRegister -> encodeRm (registerWidth sourceRegister == 64) opcode (registerNumber sourceRegister) destination False []
    ImmediateOperand immediate -> encodeGroup True [0x81] immediateGroup destination (word32List (fromIntegral immediate))
    _ -> Left (ObjectInvalidInput (destinationSource <> ", " <> sourceSource))

encodeRegisterBinary :: [Word8] -> Text -> Text -> Either ObjectError [Item]
encodeRegisterBinary opcode destinationSource sourceSource = do
  destination <- parseOperand destinationSource
  source <- maybe (Left (ObjectInvalidInput sourceSource)) pure (parseRegister sourceSource)
  encodeRm (registerWidth source == 64) opcode (registerNumber source) destination False []

encodeCompare :: Text -> Text -> Either ObjectError [Item]
encodeCompare leftSource rightSource = do
  left <- parseOperand leftSource
  right <- parseOperand rightSource
  case right of
    RegisterOperand register -> encodeRm (registerWidth register == 64) [0x39] (registerNumber register) left False []
    ImmediateOperand immediate -> encodeGroup (operandUses64Bits left) [0x81] 7 left (word32List (fromIntegral immediate))
    _ -> Left (ObjectInvalidInput (leftSource <> ", " <> rightSource))

operandUses64Bits :: Operand -> Bool
operandUses64Bits operand =
  case operand of
    RegisterOperand register -> registerWidth register == 64
    MemoryOperand {} -> True
    _ -> False

encodeGroup :: Bool -> [Word8] -> Word8 -> Operand -> [Word8] -> Either ObjectError [Item]
encodeGroup width64 opcode group operand suffix = encodeGroupWithWidth width64 opcode group operand suffix False

encodeGroupWithWidth :: Bool -> [Word8] -> Word8 -> Operand -> [Word8] -> Bool -> Either ObjectError [Item]
encodeGroupWithWidth width64 opcode group operand suffix forceByteRex = encodeRm width64 opcode group operand forceByteRex suffix

encodeRm :: Bool -> [Word8] -> Word8 -> Operand -> Bool -> [Word8] -> Either ObjectError [Item]
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
    _ -> Left (ObjectInvalidInput "invalid ModRM operand")

relativeBranch :: [Word8] -> FixupKind -> Text -> Either ObjectError [Item]
relativeBranch opcode kind target = pure [Bytes (BS.pack opcode), Apply (Fixup kind target (-4) (BS.replicate 4 0))]

jumpCondition :: Amd64Opcode -> Maybe Word8
jumpCondition name =
  case name of
    AmdJe -> Just 4
    AmdJz -> Just 4
    AmdJne -> Just 5
    _ -> Nothing

setCondition :: Amd64Opcode -> Maybe Word8
setCondition opcode =
  case opcode of
    AmdSeto -> Just 0
    AmdSetc -> Just 2
    AmdSetb -> Just 2
    AmdSetae -> Just 3
    AmdSete -> Just 4
    AmdSetne -> Just 5
    AmdSetbe -> Just 6
    AmdSeta -> Just 7
    AmdSetl -> Just 12
    AmdSetge -> Just 13
    AmdSetle -> Just 14
    AmdSetg -> Just 15
    _ -> Nothing

rex :: Bool -> Bool -> Bool -> Bool -> Word8
rex width register index base =
  0x40
    .|. (if width then 8 else 0)
    .|. (if register then 4 else 0)
    .|. (if index then 2 else 0)
    .|. (if base then 1 else 0)

readInteger :: Text -> Maybe Integer
readInteger = readMaybe . T.unpack . T.strip

bytes :: [Word8] -> Either ObjectError [Item]
bytes value = pure [Bytes (BS.pack value)]

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
