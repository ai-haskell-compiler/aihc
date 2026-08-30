{-# LANGUAGE OverloadedStrings #-}

-- | Assemble the compiler AMD64 vocabulary without an external assembler.
module Aihc.Amd64.Assemble
  ( assembleElf,
  )
where

import Aihc.Native.Elf (writeAmd64Elf)
import Aihc.Native.Object
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32, Word64, Word8)
import Text.Read (readMaybe)

assembleElf :: Text -> Either ObjectError BL.ByteString
assembleElf source = parseAssembly source >>= layoutDraft >>= writeAmd64Elf

parseAssembly :: Text -> Either ObjectError Draft
parseAssembly = foldl' parseLine (Right emptyDraft) . T.lines

parseLine :: Either ObjectError Draft -> Text -> Either ObjectError Draft
parseLine result sourceLine = do
  draft <- result
  let line = T.strip sourceLine
  if T.null line || line == ".intel_syntax noprefix"
    then pure draft
    else
      if line == ".text"
        then pure (selectSection TextSection draft)
        else
          if ".section " `T.isPrefixOf` line
            then selectElfSection line draft
            else
              if ".p2align " `T.isPrefixOf` line
                then parseAlignment line >>= \alignment -> addItem (Align alignment (alignmentFill draft)) draft
                else
                  if ".globl " `T.isPrefixOf` line
                    then pure (addGlobal (T.drop 7 line) draft)
                    else
                      if ".quad " `T.isPrefixOf` line
                        then parseQuad (T.drop 6 line) >>= \item -> addItem item draft
                        else
                          if ".byte " `T.isPrefixOf` line
                            then parseBytes (T.drop 6 line) >>= \valueBytes -> addItem (Bytes valueBytes) draft
                            else
                              if ":" `T.isSuffixOf` line
                                then addItem (Label (T.dropEnd 1 line)) draft
                                else encodeInstruction line >>= \items -> foldl' (>>=) (pure draft) [addItem item | item <- items]

selectElfSection :: Text -> Draft -> Either ObjectError Draft
selectElfSection line draft
  | ".rodata" `T.isPrefixOf` name = pure (selectSection ReadOnlySection draft)
  | ".data" `T.isPrefixOf` name = pure (selectSection DataSection draft)
  | "aihc_roots" `T.isPrefixOf` name = pure (selectSection RootsSection draft)
  | "aihc_locals" `T.isPrefixOf` name = pure (selectSection LocalsSection draft)
  | ".note.GNU-stack" `T.isPrefixOf` name = pure (selectSection NoExecuteStackSection draft)
  | otherwise = Left (ObjectInvalidInput line)
  where
    name = T.strip (T.drop 9 line)

parseAlignment :: Text -> Either ObjectError Int
parseAlignment line = maybe (Left (ObjectInvalidInput line)) pure (readMaybe (T.unpack (T.drop 9 line)))

alignmentFill :: Draft -> ByteString
alignmentFill draft
  | draftCurrentSection draft == Just TextSection = BS.singleton 0x90
  | otherwise = BS.singleton 0

parseQuad :: Text -> Either ObjectError Item
parseQuad value =
  case readInteger value of
    Just integer -> pure (Bytes (word64Bytes (fromIntegral integer)))
    Nothing -> pure (Apply (Fixup Absolute64 value 0 (BS.replicate 8 0)))

parseBytes :: Text -> Either ObjectError ByteString
parseBytes source = BS.pack <$> mapM parseByte (T.splitOn "," source)
  where
    parseByte value =
      case readMaybe (T.unpack (T.strip value)) :: Maybe Integer of
        Just integer | integer >= 0 && integer <= 255 -> pure (fromIntegral integer)
        _ -> Left (ObjectInvalidInput source)

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

encodeInstruction :: Text -> Either ObjectError [Item]
encodeInstruction line =
  case T.break isSpace line of
    (operation, rest) -> encodeOperation operation (splitOperands (T.strip rest))

encodeOperation :: Text -> [Text] -> Either ObjectError [Item]
encodeOperation operation operands =
  case (operation, operands) of
    ("ret", []) -> bytes [0xc3]
    ("ud2", []) -> bytes [0x0f, 0x0b]
    ("push", [source]) -> encodePushPop False source
    ("pop", [source]) -> encodePushPop True source
    ("call", [target]) -> relativeBranch [0xe8] X86Plt32 target
    ("jmp", [target])
      | Just register <- parseRegister target -> encodeGroup True [0xff] 4 (RegisterOperand register) []
      | otherwise -> relativeBranch [0xe9] X86Pc32 target
    (conditional, [target]) | Just condition <- jumpCondition conditional -> relativeBranch [0x0f, 0x80 + condition] X86Pc32 target
    ("mov", [destination, source]) -> encodeMove destination source
    ("movsxd", [destination, source]) -> encodeRegisterSource True [0x63] destination source
    ("movzx", [destination, source]) -> encodeRegisterSource True [0x0f, 0xb6] destination source
    ("lea", [destination, source]) -> encodeLea destination source
    ("add", [destination, source]) -> encodeBinary [0x01] 0 destination source
    ("sub", [destination, source]) -> encodeBinary [0x29] 5 destination source
    ("and", [destination, source]) -> encodeBinary [0x21] 4 destination source
    ("or", [destination, source]) -> encodeBinary [0x09] 1 destination source
    ("xor", [destination, source]) -> encodeBinary [0x31] 6 destination source
    ("imul", [destination, source]) -> encodeRegisterSource True [0x0f, 0xaf] destination source
    ("cmp", [left, right]) -> encodeCompare left right
    ("test", [left, right]) -> encodeRegisterBinary [0x85] left right
    ("shl", [destination, "cl"]) -> parseOperand destination >>= \operand -> encodeGroup True [0xd3] 4 operand []
    ("shr", [destination, "cl"]) -> parseOperand destination >>= \operand -> encodeGroup True [0xd3] 5 operand []
    ("not", [destination]) -> parseOperand destination >>= \operand -> encodeGroup True [0xf7] 2 operand []
    ("mul", [source]) -> parseOperand source >>= \operand -> encodeGroup True [0xf7] 4 operand []
    ("div", [source]) -> parseOperand source >>= \operand -> encodeGroup True [0xf7] 6 operand []
    (setOperation, [destination]) | Just condition <- T.stripPrefix "set" setOperation -> do
      code <- conditionCode condition
      operand <- parseOperand destination
      encodeGroupWithWidth False [0x0f, 0x90 + code] 0 operand [] True
    _ -> Left (ObjectInvalidInput (operation <> " " <> T.intercalate ", " operands))

encodePushPop :: Bool -> Text -> Either ObjectError [Item]
encodePushPop popValue source =
  case parseRegister source of
    Nothing -> Left (ObjectInvalidInput source)
    Just register ->
      let prefix = [0x41 | registerNumber register >= 8]
          opcode = (if popValue then 0x58 else 0x50) + registerNumber register .&. 7
       in bytes (prefix <> [opcode])

encodeMove :: Text -> Text -> Either ObjectError [Item]
encodeMove destinationSource sourceSource = do
  destination <- parseOperand destinationSource
  source <- parseOperand sourceSource
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
    _ -> Left (ObjectInvalidInput (destinationSource <> ", " <> sourceSource))

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

jumpCondition :: Text -> Maybe Word8
jumpCondition name =
  case name of
    "je" -> Just 4
    "jz" -> Just 4
    "jne" -> Just 5
    _ -> Nothing

conditionCode :: Text -> Either ObjectError Word8
conditionCode name =
  case lookup name conditions of
    Just value -> pure value
    Nothing -> Left (ObjectInvalidInput name)
  where
    conditions =
      [ ("o", 0),
        ("c", 2),
        ("b", 2),
        ("ae", 3),
        ("e", 4),
        ("ne", 5),
        ("be", 6),
        ("a", 7),
        ("l", 12),
        ("ge", 13),
        ("le", 14),
        ("g", 15)
      ]

rex :: Bool -> Bool -> Bool -> Bool -> Word8
rex width register index base =
  0x40
    .|. (if width then 8 else 0)
    .|. (if register then 4 else 0)
    .|. (if index then 2 else 0)
    .|. (if base then 1 else 0)

splitOperands :: Text -> [Text]
splitOperands source
  | T.null source = []
  | otherwise = map T.strip (go (0 :: Int) "" [] (T.unpack source))
  where
    go _ current values [] = reverse (T.pack (reverse current) : values)
    go depth current values (character : rest)
      | character == '[' = go (depth + 1) (character : current) values rest
      | character == ']' = go (depth - 1) (character : current) values rest
      | character == ',' && depth == 0 = go depth "" (T.pack (reverse current) : values) rest
      | otherwise = go depth (character : current) values rest

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
