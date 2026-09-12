{-# LANGUAGE OverloadedStrings #-}

-- | Compile Lir modules to AArch64 Mach-O objects for Darwin.
--
-- Every value lives where the register allocator put it: in a register, or
-- in a frame slot when the registers ran out or the value lives across a
-- call that clobbers them all. Instruction selection reads a register
-- operand in place, loads a slot or a literal into a scratch register, and
-- writes the result straight into its home. A convention boundary is a
-- parallel move: the arguments of a call, the values of a return, and the
-- arguments of a jump are moved to their destinations at once, so a value
-- the allocator already placed where the convention wants it costs nothing.
--
-- The @aihc@ calling convention passes the first eight arguments in @x0@ to
-- @x7@ and the rest in a 16-byte aligned block on the stack. The callee pops
-- that block, so a tail call restores the stack of the caller before it
-- pushes its own block and the stack does not grow. Results come back in
-- @x0@ to @x7@. An aihc function preserves no register: every call clobbers
-- them all, so an aihc function that makes no call and spills nothing needs
-- no frame at all. A C function preserves @x19@ to @x28@ and saves the ones
-- it touches, and it saves all of them when it calls into aihc code.
--
-- Narrow integers are canonical: an @iN@ value is zero-extended to 64 bits
-- wherever it lives. A float is its IEEE bit pattern.
module Aihc.Arm64.Lir
  ( Arm64LirError (..),
    compileLirObject,
    compileLirObjectWith,
    compileLirStatements,
    arm64Backend,
    elideSlotReloads,
    lirSymbol,
  )
where

import Aihc.Arm64.Assemble
import Aihc.Lir.Convert (integerConversionBounds)
import Aihc.Lir.Lint (LintError)
import Aihc.Lir.RegAlloc (Registers (..))
import Aihc.Lir.Syntax
import Aihc.Native.Lir
import Aihc.Native.Lir qualified as Native
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import GHC.Float (castDoubleToWord64, castFloatToWord32, double2Float)

data Arm64LirError
  = Arm64LirLintErrors ![LintError]
  | Arm64LirUnsupported !Text
  | Arm64LirObjectError !Text
  deriving (Eq, Show)

-- | The object symbol of a Lir symbol. Darwin prefixes C symbols with an
-- underscore, and the module boundary uses the C symbol names.
lirSymbol :: Symbol -> Text
lirSymbol (Symbol name) = "_" <> name

compileLirObject :: Module -> Either Arm64LirError BL.ByteString
compileLirObject = compileLirObjectWith True

-- | Assemble the module, linting it first when asked to.
compileLirObjectWith :: Bool -> Module -> Either Arm64LirError BL.ByteString
compileLirObjectWith lint lirModule = do
  statements <- compileNativeStatementsWith lint arm64Backend lirModule
  either (Left . Arm64LirObjectError . T.pack . show) pure (assembleMachO statements)

compileLirStatements :: Module -> Either Arm64LirError [Arm64Statement]
compileLirStatements = compileNativeStatements arm64Backend

elideSlotReloads :: [Arm64Statement] -> [Arm64Statement]
elideSlotReloads = elideSlotReloadsWith arm64AsCode

type M = NativeM Arm64LirError

arm64AsCode :: Arm64Statement -> Maybe SlotEffect
arm64AsCode statement =
  case statement of
    Arm64Code instruction -> Just (instructionEffect instruction)
    _ -> Nothing

unsupportedText :: Text -> M value
unsupportedText = Native.unsupported arm64Backend

operandIn' :: Ctx Arm64Register -> Int -> Type -> Arm64Register -> Operand -> ([Arm64Statement], Arm64Register)
operandIn' = Native.operandIn arm64Backend

parallelMove' :: [(Location Arm64Register, MoveSource Arm64Register)] -> [Arm64Statement]
parallelMove' = Native.parallelMove arm64Backend

cArgumentMoves' :: Ctx Arm64Register -> [Type] -> [Operand] -> M [Arm64Statement]
cArgumentMoves' = Native.cArgumentMoves arm64Backend

overflowBytes' :: Int -> Int
overflowBytes' = Native.overflowBytes arm64Backend

frameBytes' :: Layout Arm64Register -> Int
frameBytes' = Native.frameBytes arm64Backend

arm64Backend :: NativeBackend Arm64Statement Arm64Register Arm64LirError
arm64Backend =
  NativeBackend
    { nbLintErrors = Arm64LirLintErrors,
      nbUnsupported = Arm64LirUnsupported,
      nbSymbol = lirSymbol,
      nbArgumentRegisters = argumentRegisters,
      nbResultRegisters = argumentRegisters,
      nbPreservedRegisters = preservedRegisters,
      nbScratchLeft = scratchLeft,
      nbScratchRight = scratchRight,
      nbCycleScratch = scratchExtra,
      nbSlotMoveScratch = scratchLeft,
      nbFloatArgCount = 8,
      nbCIntegerLimitWord = "eight",
      nbFrameOverhead = 16,
      nbReturnAddressGap = 0,
      nbMaxFrameBytes = Just 32000,
      nbCodeAlign = 2,
      nbAfterObject = [],
      nbRegistersFor = \convention _ -> registersFor convention,
      nbSection = arm64Section,
      nbAlign = arm64Align,
      nbGlobal = arm64Global,
      nbLabel = Arm64Label,
      nbBytes = arm64Bytes,
      nbWord = arm64Word,
      nbQuad = arm64Quad,
      nbQuadSymbol = arm64QuadSymbol,
      nbQuadSymbolAddend = arm64QuadSymbolAddend,
      nbAsCode = arm64AsCode,
      nbRenderTraps = renderTraps,
      -- A conditional branch reaches 1 MB. A whole-program object is
      -- larger, so each function branches to its own trampoline and the
      -- trampoline takes the 128 MB reach of an unconditional branch.
      nbTrapTrampoline = Just (\local stub -> [Arm64Label local, arm64Instruction (ArmB (SymbolName stub))]),
      nbPrologueFrame = prologueFrame,
      nbLeaveFrame = leaveFrame,
      nbSaveReg = storeSlot,
      nbZeroWord = arm64Instruction . ArmStr XZR . Arm64Offset SP . fromIntegral,
      nbReturn = \ctx -> adjustStack ArmAdd (ctxIncomingOverflow ctx) <> [arm64Instruction ArmRet],
      nbLoadSlot = loadSlot,
      nbStoreSlot = storeSlot,
      nbMove = move,
      nbLiteralInto = literalInto,
      nbStoreSlotImmediate = \_ _ -> Nothing,
      nbCanonicalize = canonicalizeRegister,
      nbFloatFromVec = \ty slot dest -> [arm64Instruction (ArmFmovFromFloat (ty == F64) dest slot)],
      nbFloatToVec = \ty register slot -> [arm64Instruction (ArmFmovToFloat (ty == F64) slot register)],
      nbCCallExtra = const [],
      nbJump = arm64Instruction . ArmB,
      nbCanFuseFloatCompare = const True,
      nbConditionTest = \ctx fused condition ->
        let (setup, test) = conditionTest ctx fused condition
         in pure (setup, BranchTest (\label -> pure [branchWhen test label]) (\label -> pure [branchUnless test label])),
      nbCompareAndBranchEqual = \ctx ty register value label ->
        compareWith ctx ty False register (OperandLiteral (LitInt value)) <> [arm64Instruction (ArmBCond ArmEq label)],
      nbCReturnFloat = \convention resultTypes ->
        case (convention, resultTypes) of
          (CConvention, [ty]) | isFloatType ty -> [arm64Instruction (ArmFmovToFloat (ty == F64) 0 X0)]
          _ -> [],
      nbBinary = arm64Binary,
      nbUnary = bitCount,
      nbWide = wide,
      nbCompare = compareResult,
      nbFloatBinary = \op ty dst a b -> [toFloat ty 16 a, toFloat ty 17 b, arm64Instruction (ArmFloat (floatOp op) (ty == F64) 16 16 17), fromFloat ty dst 16],
      nbFloatUnary = \op ty dst a -> [toFloat ty 16 a, arm64Instruction (ArmFloat (floatUnaryOp op) (ty == F64) 16 16 16), fromFloat ty dst 16],
      nbConvert = convert,
      nbSelect = arm64Select,
      nbLoad = arm64Load,
      nbStore = arm64Store,
      nbPtrAdd = arm64PtrAdd,
      nbStackAddr = arm64StackAddr,
      nbGlobalLoad = \dst symbol -> address scratchRight symbol <> [arm64Instruction (ArmLdr dst (Arm64Offset scratchRight 0))],
      nbGlobalStore = \value symbol -> address scratchRight symbol <> [arm64Instruction (ArmStr value (Arm64Offset scratchRight 0))],
      nbCall = arm64Call,
      nbCallIndirect = arm64CallIndirect,
      nbTailCall = arm64TailCall
    }

argumentRegisters :: [Arm64Register]
argumentRegisters = [X0, X1, X2, X3, X4, X5, X6, X7]

volatileRegisters :: [Arm64Register]
volatileRegisters = [X8, X9, X10, X11, X12, X13] <> argumentRegisters

preservedRegisters :: [Arm64Register]
preservedRegisters = [X19, X20, X21, X22, X23, X24, X25, X26, X27, X28]

scratchLeft, scratchRight, scratchExtra, scratchTarget :: Arm64Register
scratchLeft = X16
scratchRight = X17
scratchExtra = X15
scratchTarget = X14

registersFor :: CallingConvention -> Registers Arm64Register
registersFor convention =
  Registers
    { registersVolatile = volatileRegisters,
      registersPreserved = preservedRegisters,
      registersPreservedCost = convention == CConvention,
      registersArgument = argument,
      registersResult = argument
    }
  where
    argument index
      | index < length argumentRegisters = Just (argumentRegisters !! index)
      | otherwise = Nothing

renderTraps :: [(Text, Int)] -> [Arm64Statement]
renderTraps traps =
  let messageLabel index = ".Llir_trap_message_" <> tshow index
      stubs =
        concat
          [ [arm64Align 2, arm64Label (trapStubLabel index)]
              <> address X0 (messageLabel index)
              <> [immediate X1 (BS.length bytes), arm64Instruction (ArmB (SymbolName ".Llir_trap"))]
          | (message, index) <- traps,
            let bytes = Text.encodeUtf8 (message <> "\n")
          ]
      reporter =
        [ arm64Align 2,
          arm64Label ".Llir_trap",
          arm64Instruction (ArmMov X2 (Arm64RegisterValue X1)),
          arm64Instruction (ArmMov X1 (Arm64RegisterValue X0)),
          arm64Instruction (ArmMov X0 (Arm64ImmediateValue 2)),
          arm64Instruction (ArmBl "_write"),
          arm64Instruction (ArmMov X0 (Arm64ImmediateValue 1)),
          arm64Instruction (ArmBl "__exit"),
          arm64Instruction (ArmBrk 0)
        ]
      messages =
        concat
          [ [arm64Label (messageLabel index), arm64Bytes (Text.encodeUtf8 (message <> "\n"))]
          | (message, index) <- traps
          ]
   in [arm64Section TextSection] <> stubs <> reporter <> [arm64Section ReadOnlySection] <> messages

trapStubLabel :: Int -> Text
trapStubLabel index = ".Llir_trap_" <> tshow index

instructionEffect :: Arm64Instruction -> SlotEffect
instructionEffect instruction =
  case instruction of
    ArmRet -> Forgets
    ArmBrk _ -> Forgets
    ArmBr _ -> Forgets
    ArmB _ -> Forgets
    ArmBl _ -> Forgets
    ArmBlr _ -> Forgets
    ArmBCond _ _ -> Writes []
    ArmCbz _ _ -> Writes []
    ArmCbnz _ _ -> Writes []
    ArmCmp _ _ -> Writes []
    ArmFcmp {} -> Writes []
    ArmAdr destination _ -> writes [destination]
    ArmAdrp destination _ -> writes [destination]
    ArmMov destination value ->
      case value of
        Arm64RegisterValue source
          | doubleWord destination && doubleWord source -> MovesRegister (generalRegister destination) (generalRegister source)
        _ -> writes [destination]
    ArmLdrImmediate destination _ -> writes [destination]
    ArmAddPageOffset destination _ _ -> writes [destination]
    ArmAdd destination _ _ -> writes [destination]
    ArmAdds destination _ _ -> writes [destination]
    ArmSub destination _ _ -> writes [destination]
    ArmSubs destination _ _ -> writes [destination]
    ArmAnd destination _ _ -> writes [destination]
    ArmOrr destination _ _ -> writes [destination]
    ArmEor destination _ _ -> writes [destination]
    ArmMvn destination _ -> writes [destination]
    ArmMul destination _ _ -> writes [destination]
    ArmUmulh destination _ _ -> writes [destination]
    ArmSmulh destination _ _ -> writes [destination]
    ArmUdiv destination _ _ -> writes [destination]
    ArmSdiv destination _ _ -> writes [destination]
    ArmMsub destination _ _ _ -> writes [destination]
    ArmLsl destination _ _ -> writes [destination]
    ArmLsr destination _ _ -> writes [destination]
    ArmAsr destination _ _ -> writes [destination]
    ArmCset destination _ -> writes [destination]
    ArmCsinv destination _ _ _ -> writes [destination]
    ArmCsel destination _ _ _ -> writes [destination]
    ArmSxtw destination _ -> writes [destination]
    ArmSxtb destination _ -> writes [destination]
    ArmSxth destination _ -> writes [destination]
    ArmClz destination _ -> writes [destination]
    ArmRbit destination _ -> writes [destination]
    ArmCnt {} -> Writes []
    ArmAddv {} -> Writes []
    ArmAndMask destination _ _ -> writes [destination]
    ArmFmovFromFloat _ destination _ -> writes [destination]
    ArmFcvtzs _ destination _ -> writes [destination]
    ArmFcvtzu _ destination _ -> writes [destination]
    ArmFmovToFloat {} -> Writes []
    ArmFloat {} -> Writes []
    ArmFcvt {} -> Writes []
    ArmScvtf {} -> Writes []
    ArmUcvtf {} -> Writes []
    ArmLdr destination target ->
      case target of
        Arm64Offset SP offset -> LoadsSlot (generalRegister destination) offset
        Arm64Offset _ _ -> writes [destination]
        _ -> Forgets
    ArmStr source target ->
      case target of
        Arm64Offset SP offset -> StoresSlot (generalRegister source) offset
        Arm64Offset _ _ -> Writes []
        _ -> Forgets
    ArmLdrb destination base _ -> narrowAccess base (writes [destination])
    ArmLdrh destination base _ -> narrowAccess base (writes [destination])
    ArmStrb _ base _ -> narrowAccess base (Writes [])
    ArmStrh _ base _ -> narrowAccess base (Writes [])
    ArmLdp {} -> Forgets
    ArmStp {} -> Forgets
  where
    writes registers
      | SP `elem` registers = Forgets
      | otherwise = Writes (map generalRegister registers)
    narrowAccess base effect
      | base == SP = Forgets
      | otherwise = effect

doubleWord :: Arm64Register -> Bool
doubleWord register = (register >= X0 && register <= X30) || register == XZR

generalRegister :: Arm64Register -> Int
generalRegister register
  | register >= W0 && register <= W30 = fromEnum register - fromEnum W0
  | register == WZR = fromEnum XZR
  | otherwise = fromEnum register

prologueFrame :: Bool -> Int -> [Arm64Statement]
prologueFrame framed size
  | framed =
      [ arm64Instruction (ArmStp X29 X30 (Arm64PreIndex SP (-16))),
        arm64Instruction (ArmMov X29 (Arm64RegisterValue SP))
      ]
        <> adjustStack ArmSub size
  | otherwise = []

canonicalizeRegister :: Type -> Arm64Register -> [Arm64Statement]
canonicalizeRegister ty register =
  case ty of
    I1 -> [arm64Instruction (ArmAndMask register register 1)]
    I8 -> [arm64Instruction (ArmAndMask register register 8)]
    I16 -> [arm64Instruction (ArmAndMask register register 16)]
    I32 -> [arm64Instruction (ArmAndMask register register 32)]
    F32 -> [arm64Instruction (ArmAndMask register register 32)]
    _ -> []

adjustStack :: (Arm64Register -> Arm64Register -> Arm64Value -> Arm64Instruction) -> Int -> [Arm64Statement]
adjustStack operation bytes
  | bytes == 0 = []
  | bytes < 4096 = [arm64Instruction (operation SP SP (Arm64ImmediateValue (fromIntegral bytes)))]
  | otherwise = [immediate scratchExtra bytes, arm64Instruction (operation SP SP (Arm64RegisterValue scratchExtra))]

leaveFrame :: Ctx Arm64Register -> Int -> [Arm64Statement]
leaveFrame ctx displacement =
  restoreRegisters ctx displacement
    <> ( if layoutFramed (ctxLayout ctx)
           then
             [ arm64Instruction (ArmMov SP (Arm64RegisterValue X29)),
               arm64Instruction (ArmLdp X29 X30 (Arm64PostIndex SP 16))
             ]
           else adjustStack ArmAdd displacement
       )

restoreRegisters :: Ctx Arm64Register -> Int -> [Arm64Statement]
restoreRegisters ctx displacement =
  [ arm64Instruction (ArmLdr register (Arm64Offset SP (fromIntegral (offset + displacement))))
  | (register, offset) <- layoutSaved (ctxLayout ctx)
  ]

loadSlot :: Arm64Register -> Int -> Arm64Statement
loadSlot register offset = arm64Instruction (ArmLdr register (Arm64Offset SP (fromIntegral offset)))

storeSlot :: Arm64Register -> Int -> Arm64Statement
storeSlot register offset = arm64Instruction (ArmStr register (Arm64Offset SP (fromIntegral offset)))

literalInto :: Type -> Arm64Register -> Literal -> [Arm64Statement]
literalInto ty register literal =
  case (ty, literal) of
    (F32, LitFloat value) -> [immediate register (toInteger (castFloatToWord32 (double2Float value)))]
    (F32, LitInt value) -> [immediate register (toInteger (castFloatToWord32 (fromInteger value)))]
    (F64, LitInt value) -> [immediate register (toInteger (castDoubleToWord64 (fromInteger value)))]
    (_, LitFloat value) -> [immediate register (toInteger (castDoubleToWord64 value))]
    (_, LitInt value)
      | typeBits ty < 64 -> [immediate register (canonicalInteger ty value)]
      | otherwise -> [immediate register value]
    (_, LitNull) -> [arm64Instruction (ArmMov register (Arm64RegisterValue XZR))]
    (_, LitSymbol symbol) -> address register (lirSymbol symbol)

smallImmediate :: Type -> Operand -> Maybe Integer
smallImmediate ty operand =
  case operand of
    OperandLiteral (LitInt value)
      | not (isFloatType ty),
        let canonical = if typeBits ty < 64 then canonicalInteger ty value else value,
        canonical >= 0 && canonical < 4096 ->
          Just canonical
    _ -> Nothing

canonicalInteger :: Type -> Integer -> Integer
canonicalInteger ty value
  | typeBits ty >= 64 = value `mod` (2 ^ (64 :: Int))
  | otherwise = value `mod` (2 ^ typeBits ty)

address :: Arm64Register -> Text -> [Arm64Statement]
address register label =
  [ arm64Instruction (ArmAdrp register label),
    arm64Instruction (ArmAddPageOffset register register label)
  ]

immediate :: (Integral value) => Arm64Register -> value -> Arm64Statement
immediate register value
  | integer >= -65536 && integer <= 65535 = arm64Instruction (ArmMov register (Arm64ImmediateValue integer))
  | otherwise = arm64Instruction (ArmLdrImmediate register integer)
  where
    integer = toInteger value

move :: Arm64Register -> Arm64Register -> [Arm64Statement]
move destination source
  | destination == source = []
  | otherwise = [arm64Instruction (ArmMov destination (Arm64RegisterValue source))]

tshow :: (Show value) => value -> Text
tshow = T.pack . show

data Test
  = TestNonZero !Arm64Register
  | TestZero !Arm64Register
  | TestFlags !Arm64Condition

conditionTest :: Ctx Arm64Register -> Maybe Fused -> Operand -> ([Arm64Statement], Test)
conditionTest ctx fused condition =
  case fused of
    Just (Fused op ty left right)
      | op `elem` [Eq, Ne],
        not (isFloatType ty),
        isZero right ->
          let (loads, register) = operandIn' ctx 0 ty scratchLeft left
           in (loads, if op == Eq then TestZero register else TestNonZero register)
      | isFloatType ty ->
          let (leftLoads, leftRegister) = operandIn' ctx 0 ty scratchLeft left
              (rightLoads, rightRegister) = operandIn' ctx 0 ty scratchRight right
           in ( leftLoads
                  <> rightLoads
                  <> [toFloat ty 16 leftRegister, toFloat ty 17 rightRegister, arm64Instruction (ArmFcmp (ty == F64) 16 17)],
                TestFlags (floatCondition op)
              )
      | otherwise ->
          let (loads, register) = operandIn' ctx 0 ty scratchLeft left
              signed = op `elem` [LtS, LeS, GtS, GeS]
              (extendLeft, leftRegister) = if signed then signExtendInto ty scratchLeft register else ([], register)
           in (loads <> extendLeft <> compareWith ctx ty signed leftRegister right, TestFlags (integerCondition op))
    Nothing ->
      let (loads, register) = operandIn' ctx 0 I1 scratchLeft condition
       in (loads, TestNonZero register)
  where
    isZero operand = operand == OperandLiteral (LitInt 0)

compareWith :: Ctx Arm64Register -> Type -> Bool -> Arm64Register -> Operand -> [Arm64Statement]
compareWith ctx ty signed left right =
  case smallImmediate ty right of
    Just value
      | not signed || typeBits ty >= 64 || value < 2 ^ (typeBits ty - 1) ->
          [arm64Instruction (ArmCmp left (Arm64ImmediateValue value))]
    _ ->
      let (loads, register) = operandIn' ctx 0 ty scratchRight right
          (extend, rightRegister) = if signed then signExtendInto ty scratchRight register else ([], register)
       in loads <> extend <> [arm64Instruction (ArmCmp left (Arm64RegisterValue rightRegister))]

branchUnless, branchWhen :: Test -> Name -> Arm64Statement
branchUnless test label =
  case test of
    TestNonZero register -> arm64Instruction (ArmCbz register label)
    TestZero register -> arm64Instruction (ArmCbnz register label)
    TestFlags condition -> arm64Instruction (ArmBCond (inverseCondition condition) label)
branchWhen test label =
  case test of
    TestNonZero register -> arm64Instruction (ArmCbnz register label)
    TestZero register -> arm64Instruction (ArmCbz register label)
    TestFlags condition -> arm64Instruction (ArmBCond condition label)

arm64Binary :: Ctx Arm64Register -> BinaryOp -> Type -> Arm64Register -> Arm64Register -> Operand -> M [Arm64Statement]
arm64Binary ctx op ty dst a right =
  case op of
    Add ->
      let (loads, b) = rightValue ty right
       in pure (loads <> narrow ty dst [arm64Instruction (ArmAdd dst a b)])
    Sub ->
      let (loads, b) = rightValue ty right
       in pure (loads <> narrow ty dst [arm64Instruction (ArmSub dst a b)])
    Mul ->
      let (loads, b) = rightRegister ty right
       in pure (loads <> narrow ty dst [arm64Instruction (ArmMul dst a b)])
    DivS -> do
      zero <- trapLabel "integer division by zero"
      overflow <- trapLabel "integer overflow"
      skip <- freshLabel "div"
      let (loads, b) = rightRegister ty right
          (extendLeft, a') = signExtendInto ty scratchLeft a
          (extendRight, b') = signExtendInto ty scratchRight b
      pure
        ( loads
            <> extendLeft
            <> extendRight
            <> [ arm64Instruction (ArmCbz b' zero),
                 immediate scratchExtra (-1 :: Integer),
                 arm64Instruction (ArmCmp b' (Arm64RegisterValue scratchExtra)),
                 arm64Instruction (ArmBCond ArmNe skip),
                 immediate scratchExtra (minimumSigned ty),
                 arm64Instruction (ArmCmp a' (Arm64RegisterValue scratchExtra)),
                 arm64Instruction (ArmBCond ArmEq overflow),
                 Arm64Label skip
               ]
            <> narrow ty dst [arm64Instruction (ArmSdiv dst a' b')]
        )
    DivU -> do
      zero <- trapLabel "integer division by zero"
      let (loads, b) = rightRegister ty right
      pure (loads <> [arm64Instruction (ArmCbz b zero), arm64Instruction (ArmUdiv dst a b)])
    RemS -> do
      zero <- trapLabel "integer division by zero"
      let (loads, b) = rightRegister ty right
          (extendLeft, a') = signExtendInto ty scratchLeft a
          (extendRight, b') = signExtendInto ty scratchRight b
      pure
        ( loads
            <> extendLeft
            <> extendRight
            <> [arm64Instruction (ArmCbz b' zero), arm64Instruction (ArmSdiv scratchExtra a' b')]
            <> narrow ty dst [arm64Instruction (ArmMsub dst scratchExtra b' a')]
        )
    RemU -> do
      zero <- trapLabel "integer division by zero"
      let (loads, b) = rightRegister ty right
      pure (loads <> [arm64Instruction (ArmCbz b zero), arm64Instruction (ArmUdiv scratchExtra a b), arm64Instruction (ArmMsub dst scratchExtra b a)])
    And ->
      let (loads, b) = rightRegister ty right
       in pure (loads <> [arm64Instruction (ArmAnd dst a b)])
    Or ->
      let (loads, b) = rightRegister ty right
       in pure (loads <> [arm64Instruction (ArmOrr dst a (Arm64RegisterValue b))])
    Xor ->
      let (loads, b) = rightRegister ty right
       in pure (loads <> [arm64Instruction (ArmEor dst a b)])
    Shl ->
      let (loads, b) = rightRegister ty right
          (mask, b') = shiftCount ty b
       in pure (loads <> mask <> narrow ty dst [arm64Instruction (ArmLsl dst a (Arm64RegisterShift b'))])
    ShrS ->
      let (loads, b) = rightRegister ty right
          (extendLeft, a') = signExtendInto ty scratchLeft a
          (mask, b') = shiftCount ty b
       in pure (loads <> extendLeft <> mask <> narrow ty dst [arm64Instruction (ArmAsr dst a' (Arm64RegisterShift b'))])
    ShrU ->
      let (loads, b) = rightRegister ty right
          (mask, b') = shiftCount ty b
       in pure (loads <> mask <> [arm64Instruction (ArmLsr dst a (Arm64RegisterShift b'))])
  where
    rightValue operandTy operand =
      case smallImmediate operandTy operand of
        Just value -> ([], Arm64ImmediateValue value)
        Nothing ->
          let (loads, b) = operandIn' ctx 0 operandTy scratchRight operand
           in (loads, Arm64RegisterValue b)
    rightRegister operandTy = operandIn' ctx 0 operandTy scratchRight
    shiftCount operandTy b
      | typeBits operandTy == 64 = ([], b)
      | otherwise = ([arm64Instruction (ArmAndMask scratchRight b (log2 (toInteger (typeBits operandTy))))], scratchRight)
    narrow operandTy dest body = body <> narrowRegister operandTy dest

bitCount :: UnaryOp -> Type -> Arm64Register -> Arm64Register -> [Arm64Statement]
bitCount op ty dst a =
  let bits = typeBits ty
   in case op of
        Popcount ->
          [ toFloat F64 16 a,
            arm64Instruction (ArmCnt 16 16),
            arm64Instruction (ArmAddv 16 16),
            fromFloat F64 dst 16
          ]
        Clz ->
          arm64Instruction (ArmClz dst a)
            : [arm64Instruction (ArmSub dst dst (Arm64ImmediateValue (toInteger (64 - bits)))) | bits < 64]
        Ctz
          | bits < 64 ->
              [ immediate scratchExtra (2 ^ bits :: Integer),
                arm64Instruction (ArmOrr dst a (Arm64RegisterValue scratchExtra)),
                arm64Instruction (ArmRbit dst dst),
                arm64Instruction (ArmClz dst dst)
              ]
          | otherwise -> [arm64Instruction (ArmRbit dst a), arm64Instruction (ArmClz dst dst)]

wide :: WideOp -> Type -> Arm64Register -> Arm64Register -> Arm64Register -> Arm64Register -> [Arm64Statement]
wide op ty low high a b =
  case op of
    MulWideU
      | typeBits ty == 64 ->
          [arm64Instruction (ArmUmulh scratchExtra a b), arm64Instruction (ArmMul low a b)] <> move high scratchExtra
      | otherwise ->
          [arm64Instruction (ArmMul scratchExtra a b), arm64Instruction (ArmLsr high scratchExtra (Arm64ImmediateShift (fromIntegral (typeBits ty))))]
            <> narrowRegister ty high
            <> move low scratchExtra
            <> narrowRegister ty low
    MulWideS
      | typeBits ty == 64 ->
          [arm64Instruction (ArmSmulh scratchExtra a b), arm64Instruction (ArmMul low a b)] <> move high scratchExtra
      | otherwise ->
          let (extendLeft, a') = signExtendInto ty scratchLeft a
              (extendRight, b') = signExtendInto ty scratchRight b
           in extendLeft
                <> extendRight
                <> [arm64Instruction (ArmMul scratchExtra a' b'), arm64Instruction (ArmAsr high scratchExtra (Arm64ImmediateShift (fromIntegral (typeBits ty))))]
                <> narrowRegister ty high
                <> move low scratchExtra
                <> narrowRegister ty low
    AddCarry
      | typeBits ty == 64 -> [arm64Instruction (ArmAdds low a (Arm64RegisterValue b)), arm64Instruction (ArmCset high ArmCs)]
      | otherwise ->
          [arm64Instruction (ArmAdd low a (Arm64RegisterValue b)), arm64Instruction (ArmLsr high low (Arm64ImmediateShift (fromIntegral (typeBits ty))))]
            <> narrowRegister ty low
    SubBorrow
      | typeBits ty == 64 -> [arm64Instruction (ArmSubs low a (Arm64RegisterValue b)), arm64Instruction (ArmCset high ArmCc)]
      | otherwise ->
          [ arm64Instruction (ArmCmp a (Arm64RegisterValue b)),
            arm64Instruction (ArmCset scratchExtra ArmCc),
            arm64Instruction (ArmSub low a (Arm64RegisterValue b))
          ]
            <> narrowRegister ty low
            <> move high scratchExtra

compareResult :: Ctx Arm64Register -> CompareOp -> Type -> Arm64Register -> Operand -> Operand -> [Arm64Statement]
compareResult ctx op ty dst left right
  | isFloatType ty =
      let (loads, a) = operandIn' ctx 0 ty scratchLeft left
          (loads', b) = operandIn' ctx 0 ty scratchRight right
       in loads <> loads' <> [toFloat ty 16 a, toFloat ty 17 b, arm64Instruction (ArmFcmp (ty == F64) 16 17), arm64Instruction (ArmCset dst (floatCondition op))]
  | otherwise =
      let signed = op `elem` [LtS, LeS, GtS, GeS]
          (loads, a) = operandIn' ctx 0 ty scratchLeft left
          (extendLeft, a') = if signed then signExtendInto ty scratchLeft a else ([], a)
       in loads <> extendLeft <> compareWith ctx ty signed a' right <> [arm64Instruction (ArmCset dst (integerCondition op))]

convert :: Ctx Arm64Register -> ConvertOp -> Type -> Type -> Arm64Register -> Arm64Register -> M [Arm64Statement]
convert _ctx op from to dst a =
  case op of
    SExt -> pure (signExtendTo dst from a <> narrowRegister to dst)
    ZExt -> pure (move dst a)
    Trunc -> pure (truncateTo dst to a)
    IToFS ->
      let (extend, a') = signExtendInto from scratchLeft a
       in pure (extend <> [arm64Instruction (ArmScvtf (to == F64) 16 a'), fromFloat to dst 16])
    IToFU -> pure [arm64Instruction (ArmUcvtf (to == F64) 16 a), fromFloat to dst 16]
    FToIS -> floatToInteger True from to dst a
    FToIU -> floatToInteger False from to dst a
    FpExt -> pure [arm64Instruction (ArmFmovToFloat False 16 (wordRegister a)), arm64Instruction (ArmFcvt True 16 16), arm64Instruction (ArmFmovFromFloat True dst 16)]
    FpTrunc -> pure [arm64Instruction (ArmFmovToFloat True 16 a), arm64Instruction (ArmFcvt False 16 16), arm64Instruction (ArmFmovFromFloat False (wordRegister dst) 16)]
    Bitcast -> pure (move dst a)

floatToInteger :: Bool -> Type -> Type -> Arm64Register -> Arm64Register -> M [Arm64Statement]
floatToInteger signed from to dst a = do
  invalid <- trapLabel "invalid float to integer conversion"
  let widen = if from == F64 then [arm64Instruction (ArmFmovToFloat True 16 a)] else [arm64Instruction (ArmFmovToFloat False 16 (wordRegister a)), arm64Instruction (ArmFcvt True 16 16)]
      (lower, excludeLower, upper) = integerConversionBounds signed from to
      lowerCondition = if excludeLower then ArmLe else ArmMi
      convertOp = if signed then ArmFcvtzs True dst 16 else ArmFcvtzu True dst 16
  pure
    ( widen
        <> [ arm64Instruction (ArmFcmp True 16 16),
             arm64Instruction (ArmBCond ArmVs invalid),
             immediate scratchExtra (toInteger (castDoubleToWord64 lower)),
             arm64Instruction (ArmFmovToFloat True 17 scratchExtra),
             arm64Instruction (ArmFcmp True 16 17),
             arm64Instruction (ArmBCond lowerCondition invalid),
             immediate scratchExtra (toInteger (castDoubleToWord64 upper)),
             arm64Instruction (ArmFmovToFloat True 17 scratchExtra),
             arm64Instruction (ArmFcmp True 16 17),
             arm64Instruction (ArmBCond ArmGe invalid),
             arm64Instruction convertOp
           ]
        <> narrowRegister to dst
    )

arm64Select :: Ctx Arm64Register -> Type -> Arm64Register -> Operand -> Operand -> Operand -> [Arm64Statement]
arm64Select ctx ty dst condition left right =
  let (conditionLoads, c) = operandIn' ctx 0 I1 scratchExtra condition
      (loads, a) = operandIn' ctx 0 ty scratchLeft left
      (loads', b) = operandIn' ctx 0 ty scratchRight right
   in conditionLoads <> loads <> loads' <> [arm64Instruction (ArmCmp c (Arm64ImmediateValue 0)), arm64Instruction (ArmCsel dst a b ArmNe)]

arm64Load :: Ctx Arm64Register -> Type -> Operand -> Integer -> Arm64Register -> [Arm64Statement]
arm64Load ctx ty base offset dst =
  let (addressLines, baseRegister) = effectiveAddress ctx base offset ty
   in addressLines <> [loadMemory ty dst baseRegister (memoryOffset offset ty)] <> [mask | ty == I1, mask <- narrowRegister I1 dst]

arm64Store :: Ctx Arm64Register -> Type -> Operand -> Operand -> Integer -> [Arm64Statement]
arm64Store ctx ty value base offset =
  let (loads, a) = operandIn' ctx 0 ty scratchLeft value
      (addressLines, baseRegister) = effectiveAddress ctx base offset ty
   in loads <> addressLines <> [storeMemory ty a baseRegister (memoryOffset offset ty)]

memoryOffset :: Integer -> Type -> Int64
memoryOffset offset ty
  | fitsScaled offset ty = fromInteger offset
  | otherwise = 0

fitsScaled :: Integer -> Type -> Bool
fitsScaled offset ty =
  let size = toInteger (typeBytes ty)
   in offset >= 0 && offset `mod` size == 0 && offset `div` size < 4096

effectiveAddress :: Ctx Arm64Register -> Operand -> Integer -> Type -> ([Arm64Statement], Arm64Register)
effectiveAddress ctx base offset ty =
  let (loads, register) = operandIn' ctx 0 Ptr scratchRight base
   in if fitsScaled offset ty
        then (loads, register)
        else (loads <> [immediate scratchExtra offset, arm64Instruction (ArmAdd scratchRight register (Arm64RegisterValue scratchExtra))], scratchRight)

arm64PtrAdd :: Ctx Arm64Register -> Arm64Register -> Operand -> Arm64Register -> [Arm64Statement]
arm64PtrAdd ctx a offset dst =
  case smallImmediate I64 offset of
    Just value -> [arm64Instruction (ArmAdd dst a (Arm64ImmediateValue value))]
    Nothing ->
      let (loads', b) = operandIn' ctx 0 I64 scratchRight offset
       in loads' <> [arm64Instruction (ArmAdd dst a (Arm64RegisterValue b))]

arm64StackAddr :: Arm64Register -> Int -> [Arm64Statement]
arm64StackAddr dst offset
  | offset < 4096 = [arm64Instruction (ArmAdd dst SP (Arm64ImmediateValue (fromIntegral offset)))]
  | otherwise = [immediate scratchExtra offset, arm64Instruction (ArmAdd dst SP (Arm64RegisterValue scratchExtra))]

arm64Call :: Ctx Arm64Register -> Either Symbol Signature -> [Operand] -> [Var] -> M [Arm64Statement]
arm64Call ctx callee arguments results = do
  let (convention, resultTypes, parameterTypes) = Native.calleeSignature ctx callee
      outgoing = case convention of
        CConvention -> 0
        AihcConvention -> overflowBytes' (length arguments)
      types = parameterTypes <> repeat I64
      branch = case callee of
        Left symbol -> [arm64Instruction (ArmBl (lirSymbol symbol))]
        Right _ -> [arm64Instruction (ArmBlr scratchTarget)]
      resultMoves =
        concat [floatResult convention ty register <> canonicalResult convention ty register | (ty, register) <- zip resultTypes argumentRegisters]
          <> parallelMove' [(home ctx var, SourceLocation (LocRegister register)) | (var, register) <- zip results argumentRegisters]
  argumentMoves <-
    case convention of
      AihcConvention ->
        pure
          ( concat
              [ loads <> [storeSlot register (8 * position)]
              | (position, (ty, argument)) <- zip [0 :: Int ..] (drop (length argumentRegisters) (zip types arguments)),
                let (loads, register) = operandIn' ctx outgoing ty scratchLeft argument
              ]
              <> parallelMove'
                [ (LocRegister register, Native.displaceSource outgoing (operandSource ctx ty argument))
                | (register, (ty, argument)) <- zip argumentRegisters (zip types arguments)
                ]
          )
      CConvention -> cArgumentMoves' ctx parameterTypes arguments
  pure (adjustStack ArmSub outgoing <> argumentMoves <> branch <> resultMoves)
  where
    operandSource ctx' ty operand =
      case operand of
        OperandVar var -> SourceLocation (home ctx' var)
        OperandLiteral literal -> SourceLiteral ty literal
    floatResult convention ty register =
      case convention of
        CConvention | isFloatType ty -> [arm64Instruction (ArmFmovFromFloat (ty == F64) register 0)]
        _ -> []
    canonicalResult convention ty register =
      case convention of
        CConvention -> canonicalizeRegister ty register
        AihcConvention -> []

arm64CallIndirect :: Ctx Arm64Register -> Operand -> [Operand] -> Signature -> [Var] -> M [Arm64Statement]
arm64CallIndirect ctx target arguments signature results = do
  stub <- trapLabel "indirect call to a non-function"
  body <- arm64Call ctx (Right signature) arguments results
  let (loads, register) = operandIn' ctx 0 Code scratchTarget target
  pure (loads <> move scratchTarget register <> [arm64Instruction (ArmCbz scratchTarget stub)] <> body)

arm64TailCall :: Ctx Arm64Register -> Either Text Operand -> CallingConvention -> [Type] -> [Operand] -> M [Arm64Statement]
arm64TailCall ctx callee convention parameterTypes arguments =
  case convention of
    AihcConvention -> aihcTailCall
    CConvention -> do
      when (ctxIncomingOverflow ctx /= 0) $
        unsupportedText "C tail call from a function with an overflow parameter block"
      argumentMoves <- cArgumentMoves' ctx parameterTypes arguments
      targetLoad <- case callee of
        Left _ -> pure []
        Right operand -> do
          stub <- trapLabel "indirect call to a non-function"
          let (loads, register) = operandIn' ctx 0 Code scratchTarget operand
          pure (loads <> move scratchTarget register <> [arm64Instruction (ArmCbz scratchTarget stub)])
      let branch = case callee of
            Left label -> arm64Instruction (ArmB (SymbolName label))
            Right _ -> arm64Instruction (ArmBr scratchTarget)
      pure (targetLoad <> argumentMoves <> leaveFrame ctx 0 <> [branch])
  where
    layout = ctxLayout ctx
    aihcTailCall = do
      let outgoing = overflowBytes' (length arguments)
          incoming = ctxIncomingOverflow ctx
          types = parameterTypes <> repeat I64
          overflow = drop (length argumentRegisters) (zip types arguments)
          registerMoves displacement =
            parallelMove'
              [ (LocRegister register, Native.displaceSource displacement (operandSource ctx ty argument))
              | (register, (ty, argument)) <- zip argumentRegisters (zip types arguments)
              ]
          overflowStores displacement base =
            concat
              [ loads <> [storeSlot register (base + 8 * position)]
              | (position, (ty, argument)) <- zip [0 :: Int ..] overflow,
                let (loads, register) = operandIn' ctx displacement ty scratchLeft argument
              ]
          branch = case callee of
            Left label -> arm64Instruction (ArmB (SymbolName label))
            Right _ -> arm64Instruction (ArmBr scratchTarget)
          operandSource ctx' ty operand =
            case operand of
              OperandVar var -> SourceLocation (home ctx' var)
              OperandLiteral literal -> SourceLiteral ty literal
      stub <-
        case callee of
          Left _ -> pure Nothing
          Right _ -> Just <$> trapLabel "indirect call to a non-function"
      let targetLoad displacement =
            case (callee, stub) of
              (Right operand, Just label) ->
                let (loads, register) = operandIn' ctx displacement Code scratchTarget operand
                 in loads <> move scratchTarget register <> [arm64Instruction (ArmCbz scratchTarget label)]
              _ -> []
      pure $
        if outgoing <= incoming
          then
            overflowStores 0 (frameBytes' layout + incoming - outgoing)
              <> targetLoad 0
              <> registerMoves 0
              <> leaveFrame ctx 0
              <> adjustStack ArmAdd (incoming - outgoing)
              <> [branch]
          else
            if not (layoutFramed layout)
              then
                adjustStack ArmSub (outgoing - incoming)
                  <> overflowStores 0 0
                  <> targetLoad 0
                  <> registerMoves 0
                  <> [branch]
              else
                adjustStack ArmSub outgoing
                  <> overflowStores outgoing 0
                  <> targetLoad outgoing
                  <> registerMoves outgoing
                  <> restoreRegisters ctx outgoing
                  <> [ arm64Instruction (ArmMov scratchExtra (Arm64RegisterValue X29)),
                       arm64Instruction (ArmLdp X29 X30 (Arm64Offset scratchExtra 0))
                     ]
                  <> destination (16 + incoming - outgoing)
                  <> concat
                    [ [ loadSlot scratchRight (8 * position),
                        arm64Instruction (ArmStr scratchRight (Arm64Offset scratchLeft (fromIntegral (8 * position))))
                      ]
                    | position <- reverse [0 .. length overflow - 1]
                    ]
                  <> [arm64Instruction (ArmMov SP (Arm64RegisterValue scratchLeft)), branch]
    destination delta
      | delta >= 0 = [arm64Instruction (ArmAdd scratchLeft scratchExtra (Arm64ImmediateValue (fromIntegral delta)))]
      | otherwise = [arm64Instruction (ArmSub scratchLeft scratchExtra (Arm64ImmediateValue (fromIntegral (negate delta))))]

minimumSigned :: Type -> Integer
minimumSigned ty = negate (2 ^ (typeBits ty - 1))

narrowRegister :: Type -> Arm64Register -> [Arm64Statement]
narrowRegister ty register
  | typeBits ty >= 64 = []
  | otherwise = [arm64Instruction (ArmAndMask register register (typeBits ty))]

signExtendTo :: Arm64Register -> Type -> Arm64Register -> [Arm64Statement]
signExtendTo destination ty source =
  case ty of
    I1 -> [arm64Instruction (ArmSub destination XZR (Arm64RegisterValue source))]
    I8 -> [arm64Instruction (ArmSxtb destination source)]
    I16 -> [arm64Instruction (ArmSxth destination source)]
    I32 -> [arm64Instruction (ArmSxtw destination source)]
    _ -> move destination source

signExtendInto :: Type -> Arm64Register -> Arm64Register -> ([Arm64Statement], Arm64Register)
signExtendInto ty scratch source
  | typeBits ty >= 64 = ([], source)
  | otherwise = (signExtendTo scratch ty source, scratch)

truncateTo :: Arm64Register -> Type -> Arm64Register -> [Arm64Statement]
truncateTo destination ty source
  | typeBits ty >= 64 = move destination source
  | otherwise = [arm64Instruction (ArmAndMask destination source (typeBits ty))]

toFloat :: Type -> Int -> Arm64Register -> Arm64Statement
toFloat ty float general = arm64Instruction (ArmFmovToFloat (ty == F64) float (if ty == F64 then general else wordRegister general))

fromFloat :: Type -> Arm64Register -> Int -> Arm64Statement
fromFloat ty general float = arm64Instruction (ArmFmovFromFloat (ty == F64) (if ty == F64 then general else wordRegister general) float)

wordRegister :: Arm64Register -> Arm64Register
wordRegister register = toEnum (fromEnum register - fromEnum X0 + fromEnum W0)

floatOp :: FloatBinaryOp -> Arm64FloatOp
floatOp op =
  case op of
    FAdd -> ArmFAdd
    FSub -> ArmFSub
    FMul -> ArmFMul
    FDiv -> ArmFDiv

floatUnaryOp :: FloatUnaryOp -> Arm64FloatOp
floatUnaryOp op =
  case op of
    FNeg -> ArmFNeg
    FAbs -> ArmFAbs
    FSqrt -> ArmFSqrt

integerCondition :: CompareOp -> Arm64Condition
integerCondition op =
  case op of
    Eq -> ArmEq
    Ne -> ArmNe
    LtS -> ArmLt
    LtU -> ArmCc
    LeS -> ArmLe
    LeU -> ArmLs
    GtS -> ArmGt
    GtU -> ArmHi
    GeS -> ArmGe
    GeU -> ArmCs
    FLt -> ArmMi
    FLe -> ArmLs
    FGt -> ArmGt
    FGe -> ArmGe

inverseCondition :: Arm64Condition -> Arm64Condition
inverseCondition condition =
  case condition of
    ArmEq -> ArmNe
    ArmNe -> ArmEq
    ArmCs -> ArmCc
    ArmCc -> ArmCs
    ArmMi -> ArmPl
    ArmPl -> ArmMi
    ArmVs -> ArmVc
    ArmVc -> ArmVs
    ArmHi -> ArmLs
    ArmLs -> ArmHi
    ArmGe -> ArmLt
    ArmLt -> ArmGe
    ArmGt -> ArmLe
    ArmLe -> ArmGt

floatCondition :: CompareOp -> Arm64Condition
floatCondition op =
  case op of
    Eq -> ArmEq
    Ne -> ArmNe
    FLt -> ArmMi
    FLe -> ArmLs
    FGt -> ArmGt
    FGe -> ArmGe
    _ -> ArmEq

loadMemory :: Type -> Arm64Register -> Arm64Register -> Int64 -> Arm64Statement
loadMemory ty value base offset =
  case typeBytes ty of
    1 -> arm64Instruction (ArmLdrb (wordRegister value) base offset)
    2 -> arm64Instruction (ArmLdrh (wordRegister value) base offset)
    4 -> arm64Instruction (ArmLdr (wordRegister value) (Arm64Offset base offset))
    _ -> arm64Instruction (ArmLdr value (Arm64Offset base offset))

storeMemory :: Type -> Arm64Register -> Arm64Register -> Int64 -> Arm64Statement
storeMemory ty value base offset =
  case typeBytes ty of
    1 -> arm64Instruction (ArmStrb (wordRegister value) base offset)
    2 -> arm64Instruction (ArmStrh (wordRegister value) base offset)
    4 -> arm64Instruction (ArmStr (wordRegister value) (Arm64Offset base offset))
    _ -> arm64Instruction (ArmStr value (Arm64Offset base offset))
