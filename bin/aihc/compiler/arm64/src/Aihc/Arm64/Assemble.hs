-- | Assemble the compiler ARM64 vocabulary without an external assembler.
module Aihc.Arm64.Assemble
  ( Arm64Statement,
    Arm64Instruction (..),
    Arm64Register (..),
    Arm64Address (..),
    Arm64Value (..),
    Arm64Shift (..),
    Arm64Condition (..),
    assembleMachO,
    arm64Align,
    arm64Bytes,
    arm64Global,
    arm64Instruction,
    arm64Label,
    arm64Quad,
    arm64QuadSymbol,
    arm64Section,
  )
where

import Aihc.Native.MachO (writeArm64MachO)
import Aihc.Native.Object
import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Text (Text)
import Data.Word (Word32, Word64)

data Arm64Statement
  = Arm64Section !SectionRole
  | Arm64Align !Int
  | Arm64Global !Text
  | Arm64Label !Text
  | Arm64Quad !Word64
  | Arm64QuadSymbol !Text
  | Arm64Bytes !ByteString
  | Arm64Instruction ![Item]

data Arm64Register
  = X0
  | X1
  | X2
  | X3
  | X4
  | X5
  | X6
  | X7
  | X8
  | X9
  | X10
  | X11
  | X12
  | X13
  | X14
  | X15
  | X16
  | X17
  | X18
  | X19
  | X20
  | X21
  | X22
  | X23
  | X24
  | X25
  | X26
  | X27
  | X28
  | X29
  | X30
  | W0
  | W1
  | W2
  | W3
  | W4
  | W5
  | W6
  | W7
  | W8
  | W9
  | W10
  | W11
  | W12
  | W13
  | W14
  | W15
  | W16
  | W17
  | W18
  | W19
  | W20
  | W21
  | W22
  | W23
  | W24
  | W25
  | W26
  | W27
  | W28
  | W29
  | W30
  | SP
  | XZR
  | WZR
  deriving (Bounded, Enum, Eq, Ord, Show)

data Arm64Address
  = Arm64Offset !Arm64Register !Int64
  | Arm64PreIndex !Arm64Register !Int64
  | Arm64PostIndex !Arm64Register !Int64
  deriving (Eq, Show)

data Arm64Value
  = Arm64RegisterValue !Arm64Register
  | Arm64ImmediateValue !Integer
  deriving (Eq, Show)

data Arm64Shift
  = Arm64RegisterShift !Arm64Register
  | Arm64ImmediateShift !Word32
  deriving (Eq, Show)

data Arm64Condition
  = ArmEq
  | ArmNe
  | ArmCs
  | ArmCc
  | ArmMi
  | ArmPl
  | ArmVs
  | ArmVc
  | ArmHi
  | ArmLs
  | ArmGe
  | ArmLt
  | ArmGt
  | ArmLe
  deriving (Eq, Ord, Show)

data Arm64Instruction
  = ArmRet
  | ArmBrk !Word32
  | ArmBr !Arm64Register
  | ArmB !Text
  | ArmBl !Text
  | ArmBCond !Arm64Condition !Text
  | ArmCbz !Arm64Register !Text
  | ArmCbnz !Arm64Register !Text
  | ArmAdr !Arm64Register !Text
  | ArmAdrp !Arm64Register !Text
  | ArmMov !Arm64Register !Arm64Value
  | ArmLdr !Arm64Register !Arm64Address
  | ArmLdrImmediate !Arm64Register !Integer
  | ArmStr !Arm64Register !Arm64Address
  | ArmLdp !Arm64Register !Arm64Register !Arm64Address
  | ArmStp !Arm64Register !Arm64Register !Arm64Address
  | ArmAdd !Arm64Register !Arm64Register !Arm64Value
  | ArmAddPageOffset !Arm64Register !Arm64Register !Text
  | ArmAdds !Arm64Register !Arm64Register !Arm64Value
  | ArmSub !Arm64Register !Arm64Register !Arm64Value
  | ArmSubs !Arm64Register !Arm64Register !Arm64Value
  | ArmCmp !Arm64Register !Arm64Value
  | ArmAnd !Arm64Register !Arm64Register !Arm64Register
  | ArmOrr !Arm64Register !Arm64Register !Arm64Value
  | ArmEor !Arm64Register !Arm64Register !Arm64Register
  | ArmMvn !Arm64Register !Arm64Register
  | ArmMul !Arm64Register !Arm64Register !Arm64Register
  | ArmUmulh !Arm64Register !Arm64Register !Arm64Register
  | ArmUdiv !Arm64Register !Arm64Register !Arm64Register
  | ArmMsub !Arm64Register !Arm64Register !Arm64Register !Arm64Register
  | ArmLsl !Arm64Register !Arm64Register !Arm64Shift
  | ArmLsr !Arm64Register !Arm64Register !Arm64Shift
  | ArmCset !Arm64Register !Arm64Condition
  | ArmCsinv !Arm64Register !Arm64Register !Arm64Register !Arm64Condition
  | ArmSxtw !Arm64Register !Arm64Register

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

arm64Quad :: Word64 -> Arm64Statement
arm64Quad = Arm64Quad

arm64QuadSymbol :: Text -> Arm64Statement
arm64QuadSymbol = Arm64QuadSymbol

arm64Bytes :: ByteString -> Arm64Statement
arm64Bytes = Arm64Bytes

arm64Instruction :: Arm64Instruction -> Arm64Statement
arm64Instruction = Arm64Instruction . encodeInstruction

applyStatement :: Either ObjectError Draft -> Arm64Statement -> Either ObjectError Draft
applyStatement result statement = do
  draft <- result
  case statement of
    Arm64Section role -> pure (selectSection role draft)
    Arm64Align alignment -> addItem (Align alignment (alignmentFill draft)) draft
    Arm64Global symbol -> pure (addGlobal symbol draft)
    Arm64Label symbol -> addItem (Label symbol) draft
    Arm64Quad value -> addItem (Bytes (word64Bytes value)) draft
    Arm64QuadSymbol symbol -> addItem (Apply (Fixup Absolute64 symbol 0 (BS.replicate 8 0))) draft
    Arm64Bytes value
      | BS.null value -> pure draft
      | otherwise -> addItem (Bytes value) draft
    Arm64Instruction items -> foldl' (>>=) (pure draft) [addItem item | item <- items]

alignmentFill :: Draft -> ByteString
alignmentFill draft
  | draftCurrentSection draft == Just TextSection = word32Bytes 0xd503201f
  | otherwise = BS.singleton 0

data Register = Register
  { registerNumber :: !Word32,
    registerWidth :: !Int,
    registerSp :: !Bool
  }

registerInfo :: Arm64Register -> Register
registerInfo register
  | register <= X30 = Register (fromIntegral (fromEnum register)) 64 False
  | register <= W30 = Register (fromIntegral (fromEnum register - fromEnum W0)) 32 False
  | register == SP = Register 31 64 True
  | register == XZR = Register 31 64 False
  | otherwise = Register 31 32 False

encodeInstruction :: Arm64Instruction -> [Item]
encodeInstruction instruction =
  case instruction of
    ArmRet -> words32 [0xd65f03c0]
    ArmBrk value -> words32 [0xd4200000 .|. (value .&. 0xffff) `shiftL` 5]
    ArmBr source -> words32 [0xd61f0000 .|. registerNumber (registerInfo source) `shiftL` 5]
    ArmB target -> branchItem 0x14000000 Arm64Branch26 target
    ArmBl target -> branchItem 0x94000000 Arm64Branch26 target
    ArmBCond condition target -> branchItem (0x54000000 .|. conditionCode condition) Arm64Branch19 target
    ArmCbz source target -> compareBranch 0x34000000 source target
    ArmCbnz source target -> compareBranch 0x35000000 source target
    ArmAdr destination target -> fixupItem (0x10000000 .|. registerNumber (registerInfo destination)) Arm64Adr21 target
    ArmAdrp destination symbol -> fixupItem (0x90000000 .|. registerNumber (registerInfo destination)) Arm64Page21 symbol
    ArmMov destination source -> encodeMove (registerInfo destination) source
    ArmLdr destination address -> encodeLoadStore True destination address
    ArmLdrImmediate destination value -> map (Bytes . word32Bytes) (loadImmediate (registerInfo destination) value)
    ArmStr source address -> encodeLoadStore False source address
    ArmLdp first second address -> encodePair True first second address
    ArmStp first second address -> encodePair False first second address
    ArmAddPageOffset destination source symbol ->
      let rd = registerInfo destination
          rn = registerInfo source
       in fixupItem (0x91000000 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd) Arm64PageOffset12 symbol
    ArmAdd destination source value -> encodeAddSub False False destination source value
    ArmAdds destination source value -> encodeAddSub False True destination source value
    ArmSub destination source value -> encodeAddSub True False destination source value
    ArmSubs destination source value -> encodeAddSub True True destination source value
    ArmCmp left right -> encodeCompare left right
    ArmAnd destination left right -> encodeThreeRegister 0x8a000000 destination left right
    ArmOrr destination left (Arm64ImmediateValue value) -> encodeLogicalImmediate destination left value
    ArmOrr destination left (Arm64RegisterValue right) -> encodeThreeRegister 0xaa000000 destination left right
    ArmEor destination left right -> encodeThreeRegister 0xca000000 destination left right
    ArmMvn destination source ->
      let rd = registerInfo destination
          rn = registerInfo source
       in words32 [0xaa2003e0 .|. registerNumber rn `shiftL` 16 .|. registerNumber rd]
    ArmMul destination left right -> encodeThreeRegister 0x9b007c00 destination left right
    ArmUmulh destination left right -> encodeThreeRegister 0x9bc07c00 destination left right
    ArmUdiv destination left right -> encodeThreeRegister 0x9ac00800 destination left right
    ArmMsub destination left right accumulator -> encodeMsub destination left right accumulator
    ArmLsl destination left right -> encodeShift 0x9ac02000 True destination left right
    ArmLsr destination left right -> encodeShift 0x9ac02400 False destination left right
    ArmCset destination condition -> encodeCset destination condition
    ArmCsinv destination trueValue falseValue condition -> encodeCsinv destination trueValue falseValue condition
    ArmSxtw destination source ->
      let rd = registerInfo destination
          rn = registerInfo source
       in words32 [0x93407c00 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd]

xorWord32 :: Word32 -> Word32 -> Word32
xorWord32 left right = (left .|. right) .&. complement (left .&. right)

encodeMove :: Register -> Arm64Value -> [Item]
encodeMove destinationRegister source =
  case source of
    Arm64ImmediateValue value -> map (Bytes . word32Bytes) (loadImmediate destinationRegister value)
    Arm64RegisterValue sourceValue ->
      let sourceRegister = registerInfo sourceValue
       in if registerSp destinationRegister || registerSp sourceRegister
            then
              words32
                [ 0x91000000
                    .|. registerNumber sourceRegister `shiftL` 5
                    .|. registerNumber destinationRegister
                ]
            else
              let base = if registerWidth destinationRegister == 64 then 0xaa0003e0 else 0x2a0003e0
               in words32 [base .|. registerNumber sourceRegister `shiftL` 16 .|. registerNumber destinationRegister]

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

encodeAddSub :: Bool -> Bool -> Arm64Register -> Arm64Register -> Arm64Value -> [Item]
encodeAddSub subtractValue setFlags destination source value =
  case value of
    Arm64ImmediateValue immediate ->
      let base
            | registerWidth rd == 32 && subtractValue && setFlags = 0x71000000
            | registerWidth rd == 32 && setFlags = 0x31000000
            | registerWidth rd == 32 && subtractValue = 0x51000000
            | registerWidth rd == 32 = 0x11000000
            | subtractValue && setFlags = 0xf1000000
            | subtractValue = 0xd1000000
            | otherwise = 0x91000000
       in words32 [base .|. (fromIntegral immediate .&. 0xfff) `shiftL` 10 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd]
    Arm64RegisterValue register ->
      let rm = registerInfo register
          useExtended = registerSp rd || registerSp rn
          base
            | useExtended && subtractValue = 0xcb206000
            | useExtended = 0x8b206000
            | subtractValue && setFlags = 0xeb000000
            | setFlags = 0xab000000
            | subtractValue = 0xcb000000
            | otherwise = 0x8b000000
       in words32 [base .|. registerNumber rm `shiftL` 16 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd]
  where
    rd = registerInfo destination
    rn = registerInfo source

encodeCompare :: Arm64Register -> Arm64Value -> [Item]
encodeCompare left right =
  case right of
    Arm64ImmediateValue immediate ->
      let base = if registerWidth rn == 64 then 0xf100001f else 0x7100001f
       in words32 [base .|. (fromIntegral immediate .&. 0xfff) `shiftL` 10 .|. registerNumber rn `shiftL` 5]
    Arm64RegisterValue register ->
      let rm = registerInfo register
          base = if registerWidth rn == 64 then 0xeb00001f else 0x6b00001f
       in words32 [base .|. registerNumber rm `shiftL` 16 .|. registerNumber rn `shiftL` 5]
  where
    rn = registerInfo left

encodeLogicalImmediate :: Arm64Register -> Arm64Register -> Integer -> [Item]
encodeLogicalImmediate destination left _ =
  let rd = registerInfo destination
      rn = registerInfo left
   in words32 [0xb2400000 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd]

encodeThreeRegister :: Word32 -> Arm64Register -> Arm64Register -> Arm64Register -> [Item]
encodeThreeRegister base destination left right =
  let rd = registerInfo destination
      rn = registerInfo left
      rm = registerInfo right
   in words32 [base .|. registerNumber rm `shiftL` 16 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd]

encodeMsub :: Arm64Register -> Arm64Register -> Arm64Register -> Arm64Register -> [Item]
encodeMsub destination left right accumulator =
  let rd = registerInfo destination
      rn = registerInfo left
      rm = registerInfo right
      ra = registerInfo accumulator
   in words32 [0x9b008000 .|. registerNumber rm `shiftL` 16 .|. registerNumber ra `shiftL` 10 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd]

encodeShift :: Word32 -> Bool -> Arm64Register -> Arm64Register -> Arm64Shift -> [Item]
encodeShift variableBase isLeft destination left right =
  case right of
    Arm64ImmediateShift amount ->
      let shift = amount .&. 63
          immr = if isLeft then (64 - shift) `mod` 64 else shift
          imms = if isLeft then 63 - shift else 63
       in words32 [0xd3400000 .|. immr `shiftL` 16 .|. imms `shiftL` 10 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd]
    Arm64RegisterShift register ->
      let rm = registerInfo register
       in words32 [variableBase .|. registerNumber rm `shiftL` 16 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd]
  where
    rd = registerInfo destination
    rn = registerInfo left

encodeLoadStore :: Bool -> Arm64Register -> Arm64Address -> [Item]
encodeLoadStore load value address =
  case address of
    Arm64PostIndex baseRegister offset ->
      let rn = registerInfo baseRegister
          base = if load then 0xf8400400 else 0xf8000400
       in words32 [base .|. fromIntegral (offset .&. 0x1ff) `shiftL` 12 .|. registerNumber rn `shiftL` 5 .|. registerNumber rt]
    Arm64PreIndex baseRegister offset ->
      let rn = registerInfo baseRegister
          base = if load then 0xf8400c00 else 0xf8000c00
       in words32 [base .|. fromIntegral (offset .&. 0x1ff) `shiftL` 12 .|. registerNumber rn `shiftL` 5 .|. registerNumber rt]
    Arm64Offset baseRegister offset ->
      let rn = registerInfo baseRegister
          scale = if registerWidth rt == 64 then 8 else 4
          base
            | registerWidth rt == 32 && load = 0xb9400000
            | registerWidth rt == 32 = 0xb9000000
            | load = 0xf9400000
            | otherwise = 0xf9000000
       in words32 [base .|. fromIntegral ((offset `div` scale) .&. 0xfff) `shiftL` 10 .|. registerNumber rn `shiftL` 5 .|. registerNumber rt]
  where
    rt = registerInfo value

encodePair :: Bool -> Arm64Register -> Arm64Register -> Arm64Address -> [Item]
encodePair load first second address =
  let rt = registerInfo first
      rt2 = registerInfo second
      (rn, offset, mode) =
        case address of
          Arm64Offset register value -> (registerInfo register, value, 0 :: Int)
          Arm64PreIndex register value -> (registerInfo register, value, 1)
          Arm64PostIndex register value -> (registerInfo register, value, 2)
      base
        | load && mode == 2 = 0xa8c00000
        | not load && mode == 2 = 0xa8800000
        | load && mode == 1 = 0xa9c00000
        | not load && mode == 1 = 0xa9800000
        | load = 0xa9400000
        | otherwise = 0xa9000000
   in words32 [base .|. fromIntegral ((offset `div` 8) .&. 0x7f) `shiftL` 15 .|. registerNumber rt2 `shiftL` 10 .|. registerNumber rn `shiftL` 5 .|. registerNumber rt]

compareBranch :: Word32 -> Arm64Register -> Text -> [Item]
compareBranch base source target =
  let register = registerInfo source
      width = if registerWidth register == 64 then 0x80000000 else 0
   in branchItem (base .|. width .|. registerNumber register) Arm64Branch19 target

branchItem :: Word32 -> FixupKind -> Text -> [Item]
branchItem = fixupItem

fixupItem :: Word32 -> FixupKind -> Text -> [Item]
fixupItem instruction kind target = [Apply (Fixup kind target 0 (word32Bytes instruction))]

conditionCode :: Arm64Condition -> Word32
conditionCode condition =
  case condition of
    ArmEq -> 0
    ArmNe -> 1
    ArmCs -> 2
    ArmCc -> 3
    ArmMi -> 4
    ArmPl -> 5
    ArmVs -> 6
    ArmVc -> 7
    ArmHi -> 8
    ArmLs -> 9
    ArmGe -> 10
    ArmLt -> 11
    ArmGt -> 12
    ArmLe -> 13

encodeCset :: Arm64Register -> Arm64Condition -> [Item]
encodeCset destination condition =
  let register = registerInfo destination
      inverted = conditionCode condition `xorWord32` 1
      base = if registerWidth register == 64 then 0x9a800400 else 0x1a800400
   in words32 [base .|. 31 `shiftL` 16 .|. inverted `shiftL` 12 .|. 31 `shiftL` 5 .|. registerNumber register]

encodeCsinv :: Arm64Register -> Arm64Register -> Arm64Register -> Arm64Condition -> [Item]
encodeCsinv destination trueValue falseValue condition =
  let rd = registerInfo destination
      rn = registerInfo trueValue
      rm = registerInfo falseValue
      base = if registerWidth rd == 64 then 0xda800000 else 0x5a800000
   in words32 [base .|. registerNumber rm `shiftL` 16 .|. conditionCode condition `shiftL` 12 .|. registerNumber rn `shiftL` 5 .|. registerNumber rd]

words32 :: [Word32] -> [Item]
words32 = map (Bytes . word32Bytes)

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
