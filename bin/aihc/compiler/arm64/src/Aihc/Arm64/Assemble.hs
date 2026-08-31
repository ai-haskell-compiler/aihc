{-# LANGUAGE OverloadedStrings #-}

-- | Assemble the compiler ARM64 vocabulary without an external assembler.
module Aihc.Arm64.Assemble
  ( Arm64Statement,
    Arm64Opcode (..),
    assembleMachO,
    arm64Align,
    arm64Bytes,
    arm64Global,
    arm64Instruction,
    arm64Label,
    arm64Quad,
    arm64Section,
  )
where

import Aihc.Native.MachO (writeArm64MachO)
import Aihc.Native.Object
import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word32, Word64)
import Text.Read (readMaybe)

data Arm64Statement
  = Arm64Section !SectionRole
  | Arm64Align !Int
  | Arm64Global !Text
  | Arm64Label !Text
  | Arm64Quad !Text
  | Arm64Bytes !ByteString
  | Arm64Instruction (Either ObjectError [Item])

data Arm64Opcode
  = ArmRet
  | ArmBrk
  | ArmBr
  | ArmB
  | ArmBl
  | ArmBEq
  | ArmBNe
  | ArmBLo
  | ArmCbz
  | ArmCbnz
  | ArmAdr
  | ArmAdrp
  | ArmMov
  | ArmLdr
  | ArmStr
  | ArmLdp
  | ArmStp
  | ArmAdd
  | ArmAdds
  | ArmSub
  | ArmSubs
  | ArmCmp
  | ArmAnd
  | ArmOrr
  | ArmEor
  | ArmMvn
  | ArmMul
  | ArmUmulh
  | ArmUdiv
  | ArmMsub
  | ArmLsl
  | ArmLsr
  | ArmCset
  | ArmCsinv
  | ArmSxtw

instance IsString Arm64Statement where
  fromString source =
    case parseStatement (T.pack source) of
      Right statement -> statement
      Left objectError -> error (show objectError)

assembleMachO :: [Arm64Statement] -> Either ObjectError BL.ByteString
assembleMachO statements = foldl' applyStatement (Right emptyDraft) statements >>= layoutDraft >>= writeArm64MachO

arm64Section :: SectionRole -> Arm64Statement
arm64Section = Arm64Section

arm64Align :: Int -> Arm64Statement
arm64Align = Arm64Align

arm64Global :: Text -> Arm64Statement
arm64Global = Arm64Global

arm64Label :: Text -> Arm64Statement
arm64Label = Arm64Label

arm64Quad :: Text -> Arm64Statement
arm64Quad = Arm64Quad

arm64Bytes :: ByteString -> Arm64Statement
arm64Bytes = Arm64Bytes

arm64Instruction :: Arm64Opcode -> [Text] -> Arm64Statement
arm64Instruction opcode = Arm64Instruction . encodeOperation (opcodeText opcode)

applyStatement :: Either ObjectError Draft -> Arm64Statement -> Either ObjectError Draft
applyStatement result statement = do
  draft <- result
  case statement of
    Arm64Section role -> pure (selectSection role draft)
    Arm64Align alignment -> addItem (Align alignment (alignmentFill draft)) draft
    Arm64Global symbol -> pure (addGlobal symbol draft)
    Arm64Label symbol -> addItem (Label symbol) draft
    Arm64Quad value -> parseQuad value >>= \item -> addItem item draft
    Arm64Bytes value
      | BS.null value -> pure draft
      | otherwise -> addItem (Bytes value) draft
    Arm64Instruction encoded ->
      encoded >>= \items -> foldl' (>>=) (pure draft) [addItem item | item <- items]

opcodeText :: Arm64Opcode -> Text
opcodeText opcode =
  case opcode of
    ArmRet -> "ret"
    ArmBrk -> "brk"
    ArmBr -> "br"
    ArmB -> "b"
    ArmBl -> "bl"
    ArmBEq -> "b.eq"
    ArmBNe -> "b.ne"
    ArmBLo -> "b.lo"
    ArmCbz -> "cbz"
    ArmCbnz -> "cbnz"
    ArmAdr -> "adr"
    ArmAdrp -> "adrp"
    ArmMov -> "mov"
    ArmLdr -> "ldr"
    ArmStr -> "str"
    ArmLdp -> "ldp"
    ArmStp -> "stp"
    ArmAdd -> "add"
    ArmAdds -> "adds"
    ArmSub -> "sub"
    ArmSubs -> "subs"
    ArmCmp -> "cmp"
    ArmAnd -> "and"
    ArmOrr -> "orr"
    ArmEor -> "eor"
    ArmMvn -> "mvn"
    ArmMul -> "mul"
    ArmUmulh -> "umulh"
    ArmUdiv -> "udiv"
    ArmMsub -> "msub"
    ArmLsl -> "lsl"
    ArmLsr -> "lsr"
    ArmCset -> "cset"
    ArmCsinv -> "csinv"
    ArmSxtw -> "sxtw"

parseStatement :: Text -> Either ObjectError Arm64Statement
parseStatement sourceLine = do
  let line = T.strip sourceLine
  if T.null line
    then Left (ObjectInvalidInput sourceLine)
    else
      if ".section " `T.isPrefixOf` line
        then Arm64Section <$> selectMachSection line
        else
          if ".p2align " `T.isPrefixOf` line
            then Arm64Align <$> parseAlignment line
            else
              if ".globl " `T.isPrefixOf` line
                then pure (Arm64Global (T.drop 7 line))
                else
                  if ".quad " `T.isPrefixOf` line
                    then pure (Arm64Quad (T.drop 6 line))
                    else
                      if ".byte " `T.isPrefixOf` line
                        then Arm64Bytes <$> parseBytes (T.drop 6 line)
                        else
                          if line == ".ltorg"
                            then pure (Arm64Bytes BS.empty)
                            else
                              if ":" `T.isSuffixOf` line
                                then pure (Arm64Label (T.dropEnd 1 line))
                                else parseInstruction line

selectMachSection :: Text -> Either ObjectError SectionRole
selectMachSection line
  | "__TEXT,__text" `T.isInfixOf` line = pure TextSection
  | "__TEXT,__const" `T.isInfixOf` line = pure TextConstantsSection
  | "__DATA,__const" `T.isInfixOf` line = pure ReadOnlySection
  | "__DATA,__data" `T.isInfixOf` line = pure DataSection
  | "__DATA,__aihc_roots" `T.isInfixOf` line = pure RootsSection
  | "__DATA,__aihc_locals" `T.isInfixOf` line = pure LocalsSection
  | otherwise = Left (ObjectInvalidInput line)

parseAlignment :: Text -> Either ObjectError Int
parseAlignment line = maybe (Left (ObjectInvalidInput line)) pure (readMaybe (T.unpack (T.drop 9 line)))

alignmentFill :: Draft -> ByteString
alignmentFill draft
  | draftCurrentSection draft == Just TextSection = word32Bytes 0xd503201f
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
  { registerNumber :: !Word32,
    registerWidth :: !Int,
    registerSp :: !Bool
  }

parseRegister :: Text -> Either ObjectError Register
parseRegister name
  | name == "sp" = pure (Register 31 64 True)
  | name == "xzr" = pure (Register 31 64 False)
  | name == "wzr" = pure (Register 31 32 False)
  | Just number <- T.stripPrefix "x" name,
    Just value <- readMaybe (T.unpack number),
    value >= (0 :: Int),
    value <= 30 =
      pure (Register (fromIntegral value) 64 False)
  | Just number <- T.stripPrefix "w" name,
    Just value <- readMaybe (T.unpack number),
    value >= (0 :: Int),
    value <= 30 =
      pure (Register (fromIntegral value) 32 False)
  | otherwise = Left (ObjectInvalidInput name)

parseInstruction :: Text -> Either ObjectError Arm64Statement
parseInstruction line =
  case T.break isSpace line of
    (operation, rest) -> do
      opcode <- parseOpcode operation
      pure (arm64Instruction opcode (splitOperands (T.strip rest)))

parseOpcode :: Text -> Either ObjectError Arm64Opcode
parseOpcode operation =
  case lookup operation [(opcodeText opcode, opcode) | opcode <- allOpcodes] of
    Just opcode -> pure opcode
    Nothing -> Left (ObjectInvalidInput operation)
  where
    allOpcodes =
      [ ArmRet,
        ArmBrk,
        ArmBr,
        ArmB,
        ArmBl,
        ArmBEq,
        ArmBNe,
        ArmBLo,
        ArmCbz,
        ArmCbnz,
        ArmAdr,
        ArmAdrp,
        ArmMov,
        ArmLdr,
        ArmStr,
        ArmLdp,
        ArmStp,
        ArmAdd,
        ArmAdds,
        ArmSub,
        ArmSubs,
        ArmCmp,
        ArmAnd,
        ArmOrr,
        ArmEor,
        ArmMvn,
        ArmMul,
        ArmUmulh,
        ArmUdiv,
        ArmMsub,
        ArmLsl,
        ArmLsr,
        ArmCset,
        ArmCsinv,
        ArmSxtw
      ]

encodeOperation :: Text -> [Text] -> Either ObjectError [Item]
encodeOperation operation operands =
  case (operation, operands) of
    ("ret", []) -> words32 [0xd65f03c0]
    ("brk", [immediate]) -> do
      value <- parseImmediate immediate
      words32 [0xd4200000 .|. (fromIntegral value .&. 0xffff) `shiftL` 5]
    ("br", [source]) -> do
      register <- parseRegister source
      words32 [0xd61f0000 .|. registerNumber register `shiftL` 5]
    ("b", [target]) -> branchItem 0x14000000 Arm64Branch26 target
    ("bl", [target]) -> branchItem 0x94000000 Arm64Branch26 target
    (conditional, [target]) | Just condition <- T.stripPrefix "b." conditional -> do
      code <- conditionCode condition
      branchItem (0x54000000 .|. code) Arm64Branch19 target
    ("cbz", [source, target]) -> compareBranch 0x34000000 source target
    ("cbnz", [source, target]) -> compareBranch 0x35000000 source target
    ("adr", [destination, target]) -> do
      register <- parseRegister destination
      fixupItem (0x10000000 .|. registerNumber register) Arm64Adr21 target
    ("adrp", [destination, target]) -> do
      register <- parseRegister destination
      symbol <- requireSuffix "@PAGE" target
      fixupItem (0x90000000 .|. registerNumber register) Arm64Page21 symbol
    ("mov", [destination, source]) -> encodeMove destination source
    ("ldr", [destination, literal]) | "=" `T.isPrefixOf` literal -> do
      register <- parseRegister destination
      value <- maybe (Left (ObjectInvalidInput literal)) pure (readInteger (T.drop 1 literal))
      pure (map (Bytes . word32Bytes) (loadImmediate register value))
    ("ldr", memory) -> encodeLoadStore True memory
    ("str", memory) -> encodeLoadStore False memory
    ("ldp", memory) -> encodePair True memory
    ("stp", memory) -> encodePair False memory
    ("add", [destination, source, value]) | "@PAGEOFF" `T.isSuffixOf` value -> do
      destinationRegister <- parseRegister destination
      sourceRegister <- parseRegister source
      symbol <- requireSuffix "@PAGEOFF" value
      let instruction = 0x91000000 .|. registerNumber sourceRegister `shiftL` 5 .|. registerNumber destinationRegister
      fixupItem instruction Arm64PageOffset12 symbol
    ("add", [destination, source, value]) -> encodeAddSub False False destination source value
    ("adds", [destination, source, value]) -> encodeAddSub False True destination source value
    ("sub", [destination, source, value]) -> encodeAddSub True False destination source value
    ("subs", [destination, source, value]) -> encodeAddSub True True destination source value
    ("cmp", [left, right]) -> encodeCompare left right
    ("and", [destination, left, right]) -> encodeLogical 0x8a000000 destination left right
    ("orr", [destination, left, immediate]) | "#" `T.isPrefixOf` immediate -> encodeLogicalImmediate destination left immediate
    ("orr", [destination, left, right]) -> encodeLogical 0xaa000000 destination left right
    ("eor", [destination, left, right]) -> encodeLogical 0xca000000 destination left right
    ("mvn", [destination, source]) -> do
      destinationRegister <- parseRegister destination
      sourceRegister <- parseRegister source
      words32 [0xaa2003e0 .|. registerNumber sourceRegister `shiftL` 16 .|. registerNumber destinationRegister]
    ("mul", [destination, left, right]) -> encodeThreeRegister 0x9b007c00 destination left right
    ("umulh", [destination, left, right]) -> encodeThreeRegister 0x9bc07c00 destination left right
    ("udiv", [destination, left, right]) -> encodeThreeRegister 0x9ac00800 destination left right
    ("msub", [destination, left, right, accumulator]) -> do
      rd <- parseRegister destination
      rn <- parseRegister left
      rm <- parseRegister right
      ra <- parseRegister accumulator
      words32 [0x9b008000 .|. registerNumber rm `shiftL` 16 .|. registerNumber ra `shiftL` 10 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd]
    ("lsl", [destination, left, right]) -> encodeShift 0x9ac02000 True destination left right
    ("lsr", [destination, left, right]) -> encodeShift 0x9ac02400 False destination left right
    ("cset", [destination, condition]) -> do
      register <- parseRegister destination
      code <- conditionCode condition
      let inverted = code `xorWord32` 1
          base = if registerWidth register == 64 then 0x9a800400 else 0x1a800400
      words32 [base .|. 31 `shiftL` 16 .|. inverted `shiftL` 12 .|. 31 `shiftL` 5 .|. registerNumber register]
    ("csinv", [destination, trueValue, falseValue, condition]) -> do
      rd <- parseRegister destination
      rn <- parseRegister trueValue
      rm <- parseRegister falseValue
      code <- conditionCode condition
      let base = if registerWidth rd == 64 then 0xda800000 else 0x5a800000
      words32 [base .|. registerNumber rm `shiftL` 16 .|. code `shiftL` 12 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd]
    ("sxtw", [destination, source]) -> do
      rd <- parseRegister destination
      rn <- parseRegister source
      words32 [0x93407c00 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd]
    _ -> Left (ObjectInvalidInput (operation <> " " <> T.intercalate ", " operands))

xorWord32 :: Word32 -> Word32 -> Word32
xorWord32 left right = (left .|. right) .&. complement (left .&. right)

encodeMove :: Text -> Text -> Either ObjectError [Item]
encodeMove destination source = do
  destinationRegister <- parseRegister destination
  case T.stripPrefix "#" source >>= readInteger of
    Just value -> pure (map (Bytes . word32Bytes) (loadImmediate destinationRegister value))
    Nothing -> do
      sourceRegister <- parseRegister source
      if registerSp destinationRegister || registerSp sourceRegister
        then
          words32
            [ 0x91000000
                .|. registerNumber sourceRegister `shiftL` 5
                .|. registerNumber destinationRegister
            ]
        else do
          let base = if registerWidth destinationRegister == 64 then 0xaa0003e0 else 0x2a0003e0
          words32 [base .|. registerNumber sourceRegister `shiftL` 16 .|. registerNumber destinationRegister]

loadImmediate :: Register -> Integer -> [Word32]
loadImmediate register value
  | value >= 0 && value <= 65535 = [movz (fromIntegral value) 0]
  | value < 0 && value >= -65536 = [movn (fromIntegral (complement (fromIntegral value :: Word64) .&. 0xffff)) 0]
  | otherwise = movz low 0 : [movk part shift | shift <- shifts, let part = fromIntegral ((bits `shiftR` shift) .&. 0xffff), part /= 0]
  where
    bits = fromIntegral value :: Word64
    low = fromIntegral (bits .&. 0xffff) :: Word32
    shifts :: [Int]
    shifts = if registerWidth register == 64 then [16, 32, 48] else [16]
    widthBase :: Word32 -> Word32 -> Word32
    widthBase base64 base32 = if registerWidth register == 64 then base64 else base32
    movz :: Word32 -> Int -> Word32
    movz immediate shift = widthBase 0xd2800000 0x52800000 .|. fromIntegral (shift `div` 16) `shiftL` 21 .|. immediate `shiftL` 5 .|. registerNumber register
    movn :: Word32 -> Int -> Word32
    movn immediate shift = widthBase 0x92800000 0x12800000 .|. fromIntegral (shift `div` 16) `shiftL` 21 .|. immediate `shiftL` 5 .|. registerNumber register
    movk :: Word32 -> Int -> Word32
    movk immediate shift = widthBase 0xf2800000 0x72800000 .|. fromIntegral (shift `div` 16) `shiftL` 21 .|. immediate `shiftL` 5 .|. registerNumber register

encodeAddSub :: Bool -> Bool -> Text -> Text -> Text -> Either ObjectError [Item]
encodeAddSub subtractValue setFlags destination source value = do
  rd <- parseRegister destination
  rn <- parseRegister source
  case T.stripPrefix "#" value >>= readInteger of
    Just immediate
      | immediate >= 0 && immediate <= 4095 ->
          let base
                | registerWidth rd == 32 && subtractValue && setFlags = 0x71000000
                | registerWidth rd == 32 && setFlags = 0x31000000
                | registerWidth rd == 32 && subtractValue = 0x51000000
                | registerWidth rd == 32 = 0x11000000
                | subtractValue && setFlags = 0xf1000000
                | subtractValue = 0xd1000000
                | otherwise = 0x91000000
           in words32 [base .|. fromIntegral immediate `shiftL` 10 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd]
    _ -> do
      rm <- parseRegister value
      let useExtended = registerSp rd || registerSp rn
          base
            | useExtended && subtractValue = 0xcb206000
            | useExtended = 0x8b206000
            | subtractValue && setFlags = 0xeb000000
            | setFlags = 0xab000000
            | subtractValue = 0xcb000000
            | otherwise = 0x8b000000
      words32 [base .|. registerNumber rm `shiftL` 16 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd]

encodeCompare :: Text -> Text -> Either ObjectError [Item]
encodeCompare left right = do
  rn <- parseRegister left
  case T.stripPrefix "#" right >>= readInteger of
    Just immediate
      | immediate >= 0 && immediate <= 4095 ->
          let base = if registerWidth rn == 64 then 0xf100001f else 0x7100001f
           in words32 [base .|. fromIntegral immediate `shiftL` 10 .|. registerNumber rn `shiftL` 5]
    _ -> do
      rm <- parseRegister right
      let base = if registerWidth rn == 64 then 0xeb00001f else 0x6b00001f
      words32 [base .|. registerNumber rm `shiftL` 16 .|. registerNumber rn `shiftL` 5]

encodeLogical :: Word32 -> Text -> Text -> Text -> Either ObjectError [Item]
encodeLogical = encodeThreeRegister

encodeLogicalImmediate :: Text -> Text -> Text -> Either ObjectError [Item]
encodeLogicalImmediate destination left immediate = do
  rd <- parseRegister destination
  rn <- parseRegister left
  value <- parseImmediate immediate
  if registerWidth rd == 64 && value == 1
    then words32 [0xb2400000 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd]
    else Left (ObjectInvalidInput immediate)

encodeThreeRegister :: Word32 -> Text -> Text -> Text -> Either ObjectError [Item]
encodeThreeRegister base destination left right = do
  rd <- parseRegister destination
  rn <- parseRegister left
  rm <- parseRegister right
  words32 [base .|. registerNumber rm `shiftL` 16 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd]

encodeShift :: Word32 -> Bool -> Text -> Text -> Text -> Either ObjectError [Item]
encodeShift variableBase isLeft destination left right = do
  rd <- parseRegister destination
  rn <- parseRegister left
  case T.stripPrefix "#" right >>= readInteger of
    Just amount
      | amount >= 0 && amount < 64 ->
          let shift = fromIntegral amount
              immr = if isLeft then (64 - shift) `mod` 64 else shift
              imms = if isLeft then 63 - shift else 63
           in words32 [0xd3400000 .|. immr `shiftL` 16 .|. imms `shiftL` 10 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd]
    _ -> do
      rm <- parseRegister right
      words32 [variableBase .|. registerNumber rm `shiftL` 16 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd]

encodeLoadStore :: Bool -> [Text] -> Either ObjectError [Item]
encodeLoadStore load operands =
  case operands of
    [value, address] -> encodeAddress value address Nothing
    [value, address, postOffset] -> encodeAddress value address (Just postOffset)
    _ -> Left (ObjectInvalidInput (T.intercalate ", " operands))
  where
    encodeAddress value address postOffset = do
      rt <- parseRegister value
      (rn, offset, preIndex) <- parseMemory address
      case postOffset of
        Just source -> do
          immediate <- parseImmediate source
          let base = if load then 0xf8400400 else 0xf8000400
          words32 [base .|. fromIntegral (immediate .&. 0x1ff) `shiftL` 12 .|. registerNumber rn `shiftL` 5 .|. registerNumber rt]
        Nothing
          | preIndex -> do
              let base = if load then 0xf8400c00 else 0xf8000c00
              words32 [base .|. fromIntegral (offset .&. 0x1ff) `shiftL` 12 .|. registerNumber rn `shiftL` 5 .|. registerNumber rt]
          | offset >= 0,
            let scale = if registerWidth rt == 64 then 8 else 4,
            offset `mod` scale == 0,
            offset `div` scale <= 4095 -> do
              let base
                    | registerWidth rt == 32 && load = 0xb9400000
                    | registerWidth rt == 32 = 0xb9000000
                    | load = 0xf9400000
                    | otherwise = 0xf9000000
              words32 [base .|. fromIntegral (offset `div` scale) `shiftL` 10 .|. registerNumber rn `shiftL` 5 .|. registerNumber rt]
          | otherwise -> Left (ObjectInvalidInput address)

encodePair :: Bool -> [Text] -> Either ObjectError [Item]
encodePair load operands =
  case operands of
    [first, second, address] -> encode first second address Nothing
    [first, second, address, postOffset] -> encode first second address (Just postOffset)
    _ -> Left (ObjectInvalidInput (T.intercalate ", " operands))
  where
    encode first second address postOffset = do
      rt <- parseRegister first
      rt2 <- parseRegister second
      (rn, memoryOffset, preIndex) <- parseMemory address
      offset <- maybe (pure memoryOffset) parseImmediate postOffset
      if offset `mod` 8 /= 0 || offset < -512 || offset > 504
        then Left (ObjectInvalidInput address)
        else do
          let base
                | load && isJust postOffset = 0xa8c00000
                | not load && isJust postOffset = 0xa8800000
                | load && preIndex = 0xa9c00000
                | not load && preIndex = 0xa9800000
                | load = 0xa9400000
                | otherwise = 0xa9000000
          words32 [base .|. fromIntegral ((offset `div` 8) .&. 0x7f) `shiftL` 15 .|. registerNumber rt2 `shiftL` 10 .|. registerNumber rn `shiftL` 5 .|. registerNumber rt]

parseMemory :: Text -> Either ObjectError (Register, Int64, Bool)
parseMemory source = do
  let preIndex = "]!" `T.isSuffixOf` source
      withoutBang = if preIndex then T.dropEnd 1 source else source
  inside <-
    case (T.stripPrefix "[" withoutBang, T.stripSuffix "]" withoutBang) of
      (Just _, Just value) -> pure (T.drop 1 value)
      _ -> Left (ObjectInvalidInput source)
  case splitOperands inside of
    [base] -> do
      register <- parseRegister base
      pure (register, 0, preIndex)
    [base, offset] -> do
      register <- parseRegister base
      immediate <- parseImmediate offset
      pure (register, immediate, preIndex)
    _ -> Left (ObjectInvalidInput source)

compareBranch :: Word32 -> Text -> Text -> Either ObjectError [Item]
compareBranch base source target = do
  register <- parseRegister source
  let width = if registerWidth register == 64 then 0x80000000 else 0
  branchItem (base .|. width .|. registerNumber register) Arm64Branch19 target

branchItem :: Word32 -> FixupKind -> Text -> Either ObjectError [Item]
branchItem = fixupItem

fixupItem :: Word32 -> FixupKind -> Text -> Either ObjectError [Item]
fixupItem instruction kind target = pure [Apply (Fixup kind target 0 (word32Bytes instruction))]

conditionCode :: Text -> Either ObjectError Word32
conditionCode name =
  case lookup name conditions of
    Just value -> pure value
    Nothing -> Left (ObjectInvalidInput name)
  where
    conditions =
      [ ("eq", 0),
        ("ne", 1),
        ("cs", 2),
        ("hs", 2),
        ("cc", 3),
        ("lo", 3),
        ("mi", 4),
        ("pl", 5),
        ("vs", 6),
        ("vc", 7),
        ("hi", 8),
        ("ls", 9),
        ("ge", 10),
        ("lt", 11),
        ("gt", 12),
        ("le", 13)
      ]

parseImmediate :: Text -> Either ObjectError Int64
parseImmediate source =
  case T.stripPrefix "#" source >>= readInteger of
    Just value -> pure (fromIntegral value)
    Nothing -> Left (ObjectInvalidInput source)

readInteger :: Text -> Maybe Integer
readInteger = readMaybe . T.unpack . T.strip

requireSuffix :: Text -> Text -> Either ObjectError Text
requireSuffix suffix value = maybe (Left (ObjectInvalidInput value)) pure (T.stripSuffix suffix value)

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

words32 :: [Word32] -> Either ObjectError [Item]
words32 = pure . map (Bytes . word32Bytes)

word32Bytes :: Word32 -> ByteString
word32Bytes value =
  BS.pack
    [ fromIntegral value,
      fromIntegral (value `shiftR` 8),
      fromIntegral (value `shiftR` 16),
      fromIntegral (value `shiftR` 24)
    ]

word64Bytes :: Word64 -> ByteString
word64Bytes value = word32Bytes (fromIntegral value) <> word32Bytes (fromIntegral (value `shiftR` 32))
