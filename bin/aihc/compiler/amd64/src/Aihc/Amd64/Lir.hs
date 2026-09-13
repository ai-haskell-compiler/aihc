{-# LANGUAGE OverloadedStrings #-}

-- | Compile Lir modules to AMD64 ELF objects for Linux.
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
-- The @aihc@ calling convention passes the first six arguments in @rdi@,
-- @rsi@, @rdx@, @rcx@, @r8@, and @r9@ and the rest in a 16-byte aligned
-- block above the return address. The callee pops that block with
-- @ret imm16@, so a tail call moves the return address and the outgoing
-- block to the place of the incoming block and the stack does not grow.
-- Results come back in @rax@, @rdx@, @rcx@, @rsi@, @rdi@, @r8@, @r9@, and
-- @r10@. An aihc function preserves no register: every call clobbers them
-- all, so an aihc function that makes no call and spills nothing needs no
-- frame at all. The @c@ convention is the System V convention with at most
-- six integer and eight float arguments and one result. A C function
-- preserves @rbx@ and @r12@ to @r15@ and saves the ones it touches, and it
-- saves all of them when it calls into aihc code.
--
-- Narrow integers are canonical: an @iN@ value is zero-extended to 64 bits
-- wherever it lives. A float is its IEEE bit pattern.
module Aihc.Amd64.Lir
  ( Amd64LirError (..),
    compileLirObject,
    compileLirObjectWith,
    writeLirObjectWith,
    writeGrinObjectWith,
    compileLirStatements,
    elideSlotReloads,
    lirSymbol,
  )
where

import Aihc.Amd64.Assemble
import Aihc.Grin.Gc (GcGrinProgram)
import Aihc.Lir.Convert (integerConversionBounds)
import Aihc.Lir.Lint (LintError)
import Aihc.Lir.RegAlloc (Registers (..))
import Aihc.Lir.Syntax
import Aihc.Native.Elf (writeAmd64Elf)
import Aihc.Native.Emit qualified as Emit
import Aihc.Native.Lir
import Aihc.Native.Lir qualified as Native
import Control.Monad (when)
import Data.Bits (shiftR)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int64)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as Text
import Data.Word (Word64)
import GHC.Float (castDoubleToWord64)

data Amd64LirError
  = Amd64LirLintErrors ![LintError]
  | Amd64LirUnsupported !Text
  | Amd64LirObjectError !Text
  deriving (Eq, Show)

-- | The object symbol of a Lir symbol. Linux uses the C symbol names as they
-- are.
lirSymbol :: Symbol -> Text
lirSymbol = unSymbol

-- | Lint the module, then assemble it.
compileLirObject :: Module -> Either Amd64LirError BL.ByteString
compileLirObject = compileLirObjectWith True

-- | Assemble the module, linting it first when asked to.
compileLirObjectWith :: Bool -> Module -> Either Amd64LirError BL.ByteString
compileLirObjectWith = Emit.compileLirObjectWith objectBackend

-- | Write an object with bounded section buffers.
writeLirObjectWith :: Bool -> Module -> FilePath -> IO ()
writeLirObjectWith = Emit.writeLirObjectWith objectBackend

-- | Consume each LIR item as GC-GRIN conversion completes it.
writeGrinObjectWith :: Bool -> Bool -> Maybe FilePath -> GcGrinProgram -> FilePath -> IO ()
writeGrinObjectWith = Emit.writeGrinObjectWith objectBackend

objectBackend :: Emit.ObjectBackend Amd64Statement Amd64Register Amd64LirError
objectBackend =
  Emit.ObjectBackend
    { Emit.obNative = amd64Backend,
      Emit.obStatement = applyStatement,
      Emit.obImage = writeAmd64Elf,
      Emit.obError = Amd64LirObjectError . T.pack . show
    }

compileLirStatements :: Module -> Either Amd64LirError [Amd64Statement]
compileLirStatements = compileNativeStatements amd64Backend

elideSlotReloads :: [Amd64Statement] -> [Amd64Statement]
elideSlotReloads = elideSlotReloadsWith amd64AsCode

type M = NativeM Amd64LirError

amd64AsCode :: Amd64Statement -> Maybe SlotEffect
amd64AsCode statement =
  case statement of
    Amd64Code instruction -> Just (instructionEffect instruction)
    _ -> Nothing

unsupportedText :: Text -> M value
unsupportedText = Native.unsupported amd64Backend

operandIn' :: Ctx Amd64Register -> Int -> Type -> Amd64Register -> Operand -> ([Amd64Statement], Amd64Register)
operandIn' = Native.operandIn amd64Backend

operandTo' :: Ctx Amd64Register -> Type -> Amd64Register -> Operand -> [Amd64Statement]
operandTo' = Native.operandTo amd64Backend

parallelMove' :: [(Location Amd64Register, MoveSource Amd64Register)] -> [Amd64Statement]
parallelMove' = Native.parallelMove amd64Backend

cArgumentMoves' :: Ctx Amd64Register -> [Type] -> [Operand] -> M [Amd64Statement]
cArgumentMoves' = Native.cArgumentMoves amd64Backend

overflowBytes' :: Int -> Int
overflowBytes' = Native.overflowBytes amd64Backend

frameBytes' :: Layout Amd64Register -> Int
frameBytes' = Native.frameBytes amd64Backend

-- Encoding and register names. The shared walk calls these fields.

amd64Backend :: NativeBackend Amd64Statement Amd64Register Amd64LirError
amd64Backend =
  NativeBackend
    { nbLintErrors = Amd64LirLintErrors,
      nbUnsupported = Amd64LirUnsupported,
      nbSymbol = lirSymbol,
      nbArgumentRegisters = argumentRegisters,
      nbResultRegisters = resultRegisters,
      nbPreservedRegisters = preservedRegisters,
      nbScratchLeft = scratchLeft,
      nbScratchRight = scratchRight,
      nbCycleScratch = scratchLeft,
      nbSlotMoveScratch = scratchRight,
      nbFloatArgCount = 8,
      nbCIntegerLimitWord = "six",
      nbFrameOverhead = 8,
      nbReturnAddressGap = 8,
      nbMaxFrameBytes = Nothing,
      nbCodeAlign = 4,
      nbAfterObject = [amd64Section NoExecuteStackSection],
      nbRegistersFor = \convention function -> registersFor convention (functionScratch function),
      nbSection = amd64Section,
      nbAlign = amd64Align,
      nbGlobal = amd64Global,
      nbLabel = Amd64Label,
      nbBytes = amd64Bytes,
      nbWord = \width value -> amd64Bytes (littleEndian width value),
      nbQuad = amd64Quad,
      nbQuadSymbol = amd64QuadSymbol,
      nbQuadSymbolAddend = amd64QuadSymbolAddend,
      nbAsCode = amd64AsCode,
      nbRenderTraps = renderTraps,
      -- A conditional jump takes a 32-bit displacement, which reaches the
      -- stub from anywhere in the object.
      nbTrapTrampoline = Nothing,
      nbPrologueFrame = prologueFrame,
      nbLeaveFrame = leaveFrame,
      nbSaveReg = storeSlot,
      nbZeroWord = \offset -> amd64Instruction (AmdStore (slotMemory offset) (Amd64StoreImmediate 0)),
      nbReturn = returnInstruction,
      nbLoadSlot = loadSlot,
      nbStoreSlot = storeSlot,
      nbMove = move,
      nbLiteralInto = literalInto,
      nbStoreSlotImmediate = storeSlotImmediate,
      nbCanonicalize = canonicalizeRegister,
      nbFloatFromVec = \_ty xmm dest -> [amd64Instruction (AmdMovqFromXmm dest xmm)],
      nbFloatToVec = \ty register xmm -> [toFloat ty xmm register],
      nbCCallExtra = \count -> [immediate RAX count],
      nbJump = amd64Instruction . AmdJmp . Amd64JumpLabel,
      nbCanFuseFloatCompare = \op -> op `elem` [Eq, Ne, FLt, FLe, FGt, FGe],
      nbConditionTest = \ctx fused condition ->
        let (setup, test) = conditionTest ctx fused condition
         in pure (setup, BranchTest (branchWhen test) (branchUnless test)),
      nbCompareAndBranchEqual = \ctx ty register value label ->
        compareWith ctx ty False register (OperandLiteral (LitInt value)) <> [amd64Instruction (AmdJe label)],
      nbCReturnFloat = \convention resultTypes ->
        case (convention, resultTypes) of
          (CConvention, [F64]) -> [amd64Instruction (AmdMovqToXmm 0 RAX)]
          (CConvention, [F32]) -> [amd64Instruction (AmdMovdToXmm 0 EAX)]
          _ -> [],
      nbBinary = amd64Binary,
      nbUnary = bitCount,
      nbWide = wide,
      nbCompare = compareResult,
      nbFloatBinary = \op ty dst a b -> [toFloat ty 0 a, toFloat ty 1 b, amd64Instruction (AmdSse (floatOp op) (ty == F64) 0 1), fromFloat ty dst 0],
      nbFloatUnary = floatUnary,
      nbConvert = convert,
      nbSelect = select,
      nbLoad = amd64Load,
      nbStore = amd64Store,
      nbPtrAdd = amd64PtrAdd,
      nbStackAddr = \dst offset -> [amd64Instruction (AmdLea dst (Amd64MemoryAddress (slotMemory offset)))],
      nbGlobalLoad = \dst symbol -> [address scratchRight symbol, amd64Instruction (AmdMov dst (Amd64MoveMemory (Amd64Memory scratchRight 0)))],
      nbGlobalStore = \value symbol -> [address scratchRight symbol, amd64Instruction (AmdStore (Amd64Memory scratchRight 0) (Amd64StoreRegister value))],
      nbCall = amd64Call,
      nbCallIndirect = amd64CallIndirect,
      nbTailCall = amd64TailCall
    }

littleEndian :: Int -> Word64 -> BS.ByteString
littleEndian count value = BS.pack [fromIntegral (value `shiftR` (8 * index)) | index <- [0 .. count - 1]]

-- | One stub per message loads the message and enters the shared reporter.
-- The reporter writes the message to the standard error stream and exits
-- with status one.
renderTraps :: [(Text, Int)] -> [Amd64Statement]
renderTraps traps =
  let messageLabel index = ".Llir_trap_message_" <> tshow index
      stubs =
        concat
          [ [ amd64Align 4,
              amd64Label (trapStubLabel index),
              amd64Instruction (AmdLea RSI (Amd64RipAddress (messageLabel index))),
              amd64Instruction (AmdMov EDX (Amd64MoveImmediate (toInteger (BS.length bytes)))),
              amd64Instruction (AmdJmp (Amd64JumpLabel (SymbolName ".Llir_trap")))
            ]
          | (message, index) <- traps,
            let bytes = Text.encodeUtf8 (message <> "\n")
          ]
      reporter =
        [ amd64Align 4,
          amd64Label ".Llir_trap",
          amd64Instruction (AmdMov EDI (Amd64MoveImmediate 2)),
          amd64Instruction (AmdAnd (Amd64RmRegister RSP) (Amd64BinaryImmediate (-16))),
          amd64Instruction (AmdCall "write"),
          amd64Instruction (AmdMov EDI (Amd64MoveImmediate 1)),
          amd64Instruction (AmdCall "_exit"),
          amd64Instruction AmdUd2
        ]
      messages =
        concat
          [ [amd64Label (messageLabel index), amd64Bytes (Text.encodeUtf8 (message <> "\n"))]
          | (message, index) <- traps
          ]
   in [amd64Section TextSection] <> stubs <> reporter <> (amd64Section ReadOnlySection : messages)

trapStubLabel :: Int -> Text
trapStubLabel index = ".Llir_trap_" <> tshow index

argumentRegisters :: [Amd64Register]
argumentRegisters = [RDI, RSI, RDX, RCX, R8, R9]

resultRegisters :: [Amd64Register]
resultRegisters = [RAX, RDX, RCX, RSI, RDI, R8, R9, R10]

preservedRegisters :: [Amd64Register]
preservedRegisters = [RBX, R12, R13, R14, R15]

scratchLeft, scratchRight :: Amd64Register
scratchLeft = R11
scratchRight = R10

data Scratch = Scratch
  { scratchDivides :: !Bool,
    scratchShifts :: !Bool
  }

functionScratch :: Function -> Scratch
functionScratch function =
  Scratch
    { scratchDivides = any divides operations,
      scratchShifts = any shifts operations
    }
  where
    operations = [operation | block <- functionBlocks function, Instruction _ operation <- blockInstructions block]
    divides operation =
      case operation of
        Binary op _ _ _ -> op `elem` [DivS, DivU, RemS, RemU]
        Wide op ty _ _ -> op `elem` [MulWideU, MulWideS] && typeBits ty == 64
        _ -> False
    shifts operation =
      case operation of
        Binary op _ _ right -> op `elem` [Shl, ShrS, ShrU] && not (isLiteral right)
        _ -> False
    isLiteral operand = case operand of
      OperandLiteral _ -> True
      OperandVar _ -> False

volatileRegisters :: Scratch -> [Amd64Register]
volatileRegisters scratch =
  [R9, R8]
    <> [RCX | not (scratchShifts scratch)]
    <> [RDX | not (scratchDivides scratch)]
    <> [RAX | not (scratchDivides scratch)]
    <> [RSI, RDI]

registersFor :: CallingConvention -> Scratch -> Registers Amd64Register
registersFor convention scratch =
  Registers
    { registersVolatile = volatileRegisters scratch,
      registersPreserved = preservedRegisters,
      registersPreservedCost = convention == CConvention,
      registersArgument = carrier argumentRegisters,
      registersResult = carrier resultRegisters
    }
  where
    carrier registers index
      | index < length registers = Just (registers !! index)
      | otherwise = Nothing

instructionEffect :: Amd64Instruction -> SlotEffect
instructionEffect instruction =
  case instruction of
    AmdRet -> Forgets
    AmdRetImm _ -> Forgets
    AmdUd2 -> Forgets
    AmdPush _ -> Forgets
    AmdPop _ -> Forgets
    AmdCall _ -> Forgets
    AmdCallRegister _ -> Forgets
    AmdJmp _ -> Forgets
    AmdJe _ -> Writes []
    AmdJne _ -> Writes []
    AmdJcc _ _ -> Writes []
    AmdMov destination source ->
      case source of
        Amd64MoveRegister register
          | quadWord destination && quadWord register -> MovesRegister (registerKey destination) (registerKey register)
        Amd64MoveMemory (Amd64Memory RSP offset)
          | quadWord destination -> LoadsSlot (registerKey destination) offset
        _ -> writes [destination]
    AmdStore (Amd64Memory base offset) source ->
      case source of
        Amd64StoreRegister register
          | base == RSP && quadWord register -> StoresSlot (registerKey register) offset
        _
          | base == RSP -> WritesSlot offset
          | otherwise -> Writes []
    AmdStoreByte (Amd64Memory base _) _ -> narrowAccess base
    AmdStoreWord (Amd64Memory base _) _ -> narrowAccess base
    AmdMovsxd destination _ -> writes [destination]
    AmdMovsxByte destination _ -> writes [destination]
    AmdMovsxWord destination _ -> writes [destination]
    AmdMovzx destination _ -> writes [destination]
    AmdMovzxWord destination _ -> writes [destination]
    AmdLea destination _ -> writes [destination]
    AmdAdd destination _ -> modifies destination
    AmdSub destination _ -> modifies destination
    AmdAnd destination _ -> modifies destination
    AmdOr destination _ -> modifies destination
    AmdXor destination _ -> modifies destination
    AmdImul destination _ -> writes [destination]
    AmdCmp _ _ -> Writes []
    AmdTest _ _ -> Writes []
    AmdShl destination -> modifies destination
    AmdShr destination -> modifies destination
    AmdShlImmediate destination _ -> modifies destination
    AmdShrImmediate destination _ -> modifies destination
    AmdSarImmediate destination _ -> modifies destination
    AmdNot destination -> modifies destination
    AmdMul _ -> writes [RAX, RDX]
    AmdDiv _ -> writes [RAX, RDX]
    AmdSet _ destination -> modifies destination
    AmdCmov _ destination _ -> writes [destination]
    AmdNeg destination -> modifies destination
    AmdSar destination -> modifies destination
    AmdIdiv _ -> writes [RAX, RDX]
    AmdImulWide _ -> writes [RAX, RDX]
    AmdCqo -> writes [RDX]
    AmdMovqToXmm _ _ -> Writes []
    AmdMovdToXmm _ _ -> Writes []
    AmdSse {} -> Writes []
    AmdUcomis {} -> Writes []
    AmdCvtsi2s {} -> Writes []
    AmdMovqFromXmm destination _ -> writes [destination]
    AmdMovdFromXmm destination _ -> writes [destination]
    AmdCvtts2si _ destination _ -> writes [destination]
    AmdBitCount _ destination _ -> writes [destination]
  where
    writes registers
      | RSP `elem` registers = Forgets
      | otherwise = Writes (map registerKey registers)
    modifies destination =
      case destination of
        Amd64RmRegister register -> writes [register]
        Amd64RmMemory (Amd64Memory RSP offset) -> WritesSlot offset
        Amd64RmMemory _ -> Writes []
    narrowAccess base
      | base == RSP = Forgets
      | otherwise = Writes []

quadWord :: Amd64Register -> Bool
quadWord register = register `elem` quadRegisters && register /= RSP

quadRegisters, dwordRegisters, byteRegisters :: [Amd64Register]
quadRegisters = [RAX, RCX, RDX, RBX, RSP, RBP, RSI, RDI, R8, R9, R10, R11, R12, R13, R14, R15]
dwordRegisters = [EAX, ECX, EDX, EBX, ESP, EBP, ESI, EDI, R8D, R9D, R10D, R11D, R12D, R13D, R14D, R15D]
byteRegisters = [AL, CL, DL, BL, SPL, BPL, SIL, DIL, R8B, R9B, R10B, R11B, R12B, R13B, R14B, R15B]

registerKey :: Amd64Register -> Int
registerKey register =
  case elemIndex register quadRegisters of
    Just index -> index
    Nothing ->
      case elemIndex register dwordRegisters of
        Just index -> index
        Nothing -> fromMaybe 0 (elemIndex register byteRegisters)

prologueFrame :: Bool -> Int -> [Amd64Statement]
prologueFrame framed size
  | framed =
      [ amd64Instruction (AmdPush RBP),
        amd64Instruction (AmdMov RBP (Amd64MoveRegister RSP))
      ]
        <> adjustStack AmdSub size
  | otherwise = []

canonicalizeRegister :: Type -> Amd64Register -> [Amd64Statement]
canonicalizeRegister ty register =
  case ty of
    I1 -> [amd64Instruction (AmdMovzx register (Amd64RmRegister (byteRegister register)))]
    I8 -> [amd64Instruction (AmdMovzx register (Amd64RmRegister (byteRegister register)))]
    I16 -> [amd64Instruction (AmdMovzxWord register (Amd64RmRegister register))]
    I32 -> [amd64Instruction (AmdMov (dwordRegister register) (Amd64MoveRegister (dwordRegister register)))]
    F32 -> [amd64Instruction (AmdMov (dwordRegister register) (Amd64MoveRegister (dwordRegister register)))]
    _ -> []

adjustStack :: (Amd64Rm -> Amd64BinarySource -> Amd64Instruction) -> Int -> [Amd64Statement]
adjustStack operation bytes
  | bytes == 0 = []
  | otherwise = [amd64Instruction (operation (Amd64RmRegister RSP) (Amd64BinaryImmediate (toInteger bytes)))]

leaveFrame :: Ctx Amd64Register -> Int -> [Amd64Statement]
leaveFrame ctx displacement =
  restoreRegisters ctx displacement
    <> ( if layoutFramed (ctxLayout ctx)
           then [amd64Instruction (AmdMov RSP (Amd64MoveRegister RBP)), amd64Instruction (AmdPop RBP)]
           else adjustStack AmdAdd displacement
       )

returnInstruction :: Ctx Amd64Register -> [Amd64Statement]
returnInstruction ctx =
  [amd64Instruction (if ctxIncomingOverflow ctx == 0 then AmdRet else AmdRetImm (ctxIncomingOverflow ctx))]

restoreRegisters :: Ctx Amd64Register -> Int -> [Amd64Statement]
restoreRegisters ctx displacement =
  [ amd64Instruction (AmdMov register (Amd64MoveMemory (Amd64Memory RSP (fromIntegral (offset + displacement)))))
  | (register, offset) <- layoutSaved (ctxLayout ctx)
  ]

loadSlot :: Amd64Register -> Int -> Amd64Statement
loadSlot register offset = amd64Instruction (AmdMov register (Amd64MoveMemory (slotMemory offset)))

storeSlot :: Amd64Register -> Int -> Amd64Statement
storeSlot register offset = amd64Instruction (AmdStore (slotMemory offset) (Amd64StoreRegister register))

slotMemory :: Int -> Amd64Memory
slotMemory offset = Amd64Memory RSP (fromIntegral offset)

storeSlotImmediate :: Int -> Integer -> Maybe Amd64Statement
storeSlotImmediate offset bits =
  case signedImmediate bits of
    Just value -> Just (amd64Instruction (AmdStore (slotMemory offset) (Amd64StoreImmediate value)))
    Nothing -> Nothing

literalInto :: Type -> Amd64Register -> Literal -> [Amd64Statement]
literalInto ty register literal =
  case Native.literalBits ty literal of
    Just bits -> [immediate register bits]
    Nothing ->
      case literal of
        LitSymbol symbol -> [address register (lirSymbol symbol)]
        _ -> []

smallImmediate :: Type -> Operand -> Maybe Integer
smallImmediate ty operand =
  case operand of
    OperandLiteral (LitInt value)
      | not (isFloatType ty) -> signedImmediate (canonicalInteger ty value)
    _ -> Nothing

signedImmediate :: Integer -> Maybe Integer
signedImmediate bits
  | bits < 2 ^ (31 :: Int) = Just bits
  | bits >= 2 ^ (64 :: Int) - 2 ^ (31 :: Int) = Just (bits - 2 ^ (64 :: Int))
  | otherwise = Nothing

canonicalInteger :: Type -> Integer -> Integer
canonicalInteger ty value
  | typeBits ty >= 64 = value `mod` (2 ^ (64 :: Int))
  | otherwise = value `mod` (2 ^ typeBits ty)

address :: Amd64Register -> Text -> Amd64Statement
address register label = amd64Instruction (AmdLea register (Amd64RipAddress label))

immediate :: (Integral value) => Amd64Register -> value -> Amd64Statement
immediate register value
  | integer >= 0 && integer <= 0xffffffff = amd64Instruction (AmdMov (dwordRegister register) (Amd64MoveImmediate integer))
  | otherwise = amd64Instruction (AmdMov register (Amd64MoveImmediate (integer `mod` (2 ^ (64 :: Int)))))
  where
    integer = toInteger value

move :: Amd64Register -> Amd64Register -> [Amd64Statement]
move destination source
  | destination == source = []
  | otherwise = [amd64Instruction (AmdMov destination (Amd64MoveRegister source))]

tshow :: (Show value) => value -> Text
tshow = T.pack . show

data Test
  = TestNonZero !Amd64Register
  | TestZero !Amd64Register
  | TestFlags !Amd64Condition
  | TestFloatEqual
  | TestFloatNotEqual

conditionTest :: Ctx Amd64Register -> Maybe Fused -> Operand -> ([Amd64Statement], Test)
conditionTest ctx fused condition =
  case fused of
    Just (Fused op ty left right)
      | op `elem` [Eq, Ne],
        not (isFloatType ty),
        right == OperandLiteral (LitInt 0) ->
          let (loads, register) = operandIn' ctx 0 ty scratchLeft left
           in (loads, if op == Eq then TestZero register else TestNonZero register)
      | isFloatType ty ->
          let (loads, flags) = floatFlags ctx op ty left right
           in ( loads,
                case op of
                  Eq -> TestFloatEqual
                  Ne -> TestFloatNotEqual
                  _ -> TestFlags flags
              )
      | otherwise ->
          let (loads, register) = operandIn' ctx 0 ty scratchLeft left
              signed = op `elem` [LtS, LeS, GtS, GeS]
              (extendLeft, leftRegister) = if signed then signExtendInto ty scratchLeft register else ([], register)
           in (loads <> extendLeft <> compareWith ctx ty signed leftRegister right, TestFlags (integerCondition op))
    Nothing ->
      let (loads, register) = operandIn' ctx 0 I1 scratchLeft condition
       in (loads, TestNonZero register)

floatFlags :: Ctx Amd64Register -> CompareOp -> Type -> Operand -> Operand -> ([Amd64Statement], Amd64Condition)
floatFlags ctx op ty left right =
  let (loads, a) = operandIn' ctx 0 ty scratchLeft left
      (loads', b) = operandIn' ctx 0 ty scratchRight right
      ucomis first second = [toFloat ty 0 first, toFloat ty 1 second, amd64Instruction (AmdUcomis (ty == F64) 0 1)]
      (order, flags) =
        case op of
          FLt -> (ucomis b a, AmdAbove)
          FLe -> (ucomis b a, AmdAboveOrEqual)
          FGt -> (ucomis a b, AmdAbove)
          FGe -> (ucomis a b, AmdAboveOrEqual)
          Ne -> (ucomis a b, AmdNotEqual)
          _ -> (ucomis a b, AmdEqual)
   in (loads <> loads' <> order, flags)

compareWith :: Ctx Amd64Register -> Type -> Bool -> Amd64Register -> Operand -> [Amd64Statement]
compareWith ctx ty signed left right =
  case smallImmediate ty right of
    Just value
      | not signed || typeBits ty >= 64 || value < 2 ^ (typeBits ty - 1) ->
          [amd64Instruction (AmdCmp (Amd64RmRegister left) (Amd64BinaryImmediate value))]
    _ ->
      let (loads, register) = operandIn' ctx 0 ty scratchRight right
          (extend, rightRegister) = if signed then signExtendInto ty scratchRight register else ([], register)
       in loads <> extend <> [amd64Instruction (AmdCmp (Amd64RmRegister left) (Amd64BinaryRegister rightRegister))]

branchUnless :: Test -> Name -> M [Amd64Statement]
branchUnless test label =
  case test of
    TestNonZero register -> pure [testZero register, amd64Instruction (AmdJe label)]
    TestZero register -> pure [testZero register, amd64Instruction (AmdJne label)]
    TestFlags condition -> pure [amd64Instruction (AmdJcc (inverseCondition condition) label)]
    TestFloatEqual -> pure [amd64Instruction (AmdJne label), amd64Instruction (AmdJcc AmdParity label)]
    TestFloatNotEqual -> do
      over <- freshLabel "unordered"
      pure [amd64Instruction (AmdJcc AmdParity over), amd64Instruction (AmdJe label), Amd64Label over]

branchWhen :: Test -> Name -> M [Amd64Statement]
branchWhen test label =
  case test of
    TestNonZero register -> pure [testZero register, amd64Instruction (AmdJne label)]
    TestZero register -> pure [testZero register, amd64Instruction (AmdJe label)]
    TestFlags condition -> pure [amd64Instruction (AmdJcc condition label)]
    TestFloatEqual -> do
      over <- freshLabel "unordered"
      pure [amd64Instruction (AmdJcc AmdParity over), amd64Instruction (AmdJe label), Amd64Label over]
    TestFloatNotEqual -> pure [amd64Instruction (AmdJne label), amd64Instruction (AmdJcc AmdParity label)]

data RightOperand
  = RightRegister !Amd64Register
  | RightImmediate !Integer

binarySource :: RightOperand -> Amd64BinarySource
binarySource operand =
  case operand of
    RightRegister register -> Amd64BinaryRegister register
    RightImmediate value -> Amd64BinaryImmediate value

amd64Binary :: Ctx Amd64Register -> BinaryOp -> Type -> Amd64Register -> Amd64Register -> Operand -> M [Amd64Statement]
amd64Binary ctx op ty dst a right =
  case op of
    Add ->
      let (loads, b) = rightValue ty right
       in pure
            ( loads
                <> narrow
                  ty
                  dst
                  ( case b of
                      RightImmediate value -> addImmediate dst a value
                      RightRegister _ -> arith True AmdAdd dst a b
                  )
            )
    Sub ->
      let (loads, b) = rightValue ty right
       in pure
            ( loads
                <> narrow
                  ty
                  dst
                  ( case b of
                      RightImmediate value | value > negate (2 ^ (31 :: Int)) -> addImmediate dst a (negate value)
                      _ -> arith False AmdSub dst a b
                  )
            )
    Mul ->
      let (loads, b) = rightRegister ty right
       in pure (loads <> narrow ty dst (multiply dst a b))
    DivS -> do
      checks <- signedDivisionChecks op ty a right
      pure (checks <> [amd64Instruction AmdCqo, amd64Instruction (AmdIdiv (Amd64RmRegister scratchRight))] <> narrow ty dst (move dst RAX))
    DivU -> do
      zero <- trapLabel "integer division by zero"
      let (loads, b) = rightRegister ty right
      pure (loads <> move RAX a <> [testZero b, amd64Instruction (AmdJe zero), clearRdx, amd64Instruction (AmdDiv (Amd64RmRegister b))] <> move dst RAX)
    RemS -> do
      checks <- signedDivisionChecks op ty a right
      pure (checks <> [amd64Instruction AmdCqo, amd64Instruction (AmdIdiv (Amd64RmRegister scratchRight))] <> narrow ty dst (move dst RDX))
    RemU -> do
      zero <- trapLabel "integer division by zero"
      let (loads, b) = rightRegister ty right
      pure (loads <> move RAX a <> [testZero b, amd64Instruction (AmdJe zero), clearRdx, amd64Instruction (AmdDiv (Amd64RmRegister b))] <> move dst RDX)
    And ->
      let (loads, b) = rightValue ty right
       in pure (loads <> arith True AmdAnd dst a b)
    Or ->
      let (loads, b) = rightValue ty right
       in pure (loads <> arith True AmdOr dst a b)
    Xor ->
      let (loads, b) = rightValue ty right
       in pure (loads <> arith True AmdXor dst a b)
    Shl -> pure (shift ty dst (move dst a) AmdShl AmdShlImmediate right)
    ShrS -> pure (shift ty dst (signExtendTo dst ty a) AmdSar AmdSarImmediate right)
    ShrU -> pure (shift ty dst (move dst a) AmdShr AmdShrImmediate right)
  where
    rightValue operandTy operand =
      case smallImmediate operandTy operand of
        Just value -> ([], RightImmediate value)
        Nothing ->
          let (loads, b) = operandIn' ctx 0 operandTy scratchRight operand
           in (loads, RightRegister b)
    rightRegister operandTy = operandIn' ctx 0 operandTy scratchRight
    arith commutative instruction dest left b
      | dest == left = [amd64Instruction (instruction (Amd64RmRegister dest) (binarySource b))]
      | RightRegister register <- b,
        register == dest =
          if commutative
            then [amd64Instruction (instruction (Amd64RmRegister dest) (Amd64BinaryRegister left))]
            else move scratchLeft left <> [amd64Instruction (instruction (Amd64RmRegister scratchLeft) (binarySource b))] <> move dest scratchLeft
      | otherwise = move dest left <> [amd64Instruction (instruction (Amd64RmRegister dest) (binarySource b))]
    addImmediate dest left value
      | dest /= left = [amd64Instruction (AmdLea dest (Amd64MemoryAddress (Amd64Memory left (fromInteger value))))]
      | otherwise = arith True AmdAdd dest left (RightImmediate value)
    multiply dest left b
      | dest == left = [amd64Instruction (AmdImul dest (Amd64RmRegister b))]
      | dest == b = [amd64Instruction (AmdImul dest (Amd64RmRegister left))]
      | otherwise = move dest left <> [amd64Instruction (AmdImul dest (Amd64RmRegister b))]
    shift operandTy dest prepare byCount byImmediate operand =
      case operand of
        OperandLiteral (LitInt count) ->
          prepare <> narrowShift operandTy dest [amd64Instruction (byImmediate (Amd64RmRegister dest) (fromInteger (count `mod` toInteger (typeBits operandTy))))]
        _ ->
          let (loads, b) = rightRegister operandTy operand
           in loads
                <> move RCX b
                <> [amd64Instruction (AmdAnd (Amd64RmRegister RCX) (Amd64BinaryImmediate (toInteger (typeBits operandTy - 1)))) | typeBits operandTy /= 64]
                <> prepare
                <> narrowShift operandTy dest [amd64Instruction (byCount (Amd64RmRegister dest))]
    narrowShift operandTy dest body = body <> narrowRegister operandTy dest
    signedDivisionChecks divisionOp operandTy left operand = do
      zero <- trapLabel "integer division by zero"
      skip <- freshLabel "div"
      minusOne <-
        if divisionOp == DivS
          then do
            overflow <- trapLabel "integer overflow"
            pure
              [ immediate scratchLeft (minimumSigned operandTy),
                amd64Instruction (AmdCmp (Amd64RmRegister RAX) (Amd64BinaryRegister scratchLeft)),
                amd64Instruction (AmdJe overflow)
              ]
          else pure [immediate scratchRight (1 :: Integer)]
      let (loads, b) = rightRegister operandTy operand
      pure
        ( loads
            <> signExtendTo RAX operandTy left
            <> signExtendTo scratchRight operandTy b
            <> [ testZero scratchRight,
                 amd64Instruction (AmdJe zero),
                 amd64Instruction (AmdCmp (Amd64RmRegister scratchRight) (Amd64BinaryImmediate (-1))),
                 amd64Instruction (AmdJne skip)
               ]
            <> minusOne
            <> [Amd64Label skip]
        )
    narrow operandTy dest body = body <> narrowRegister operandTy dest

bitCount :: UnaryOp -> Type -> Amd64Register -> Amd64Register -> [Amd64Statement]
bitCount op ty dst a =
  let bits = typeBits ty
   in case op of
        Popcount -> [amd64Instruction (AmdBitCount AmdPopcnt dst (Amd64RmRegister a))]
        Clz ->
          [amd64Instruction (AmdBitCount AmdLzcnt dst (Amd64RmRegister a))]
            <> [amd64Instruction (AmdSub (Amd64RmRegister dst) (Amd64BinaryImmediate (toInteger (64 - bits)))) | bits < 64]
        Ctz
          | bits < 64 ->
              move scratchLeft a
                <> ( case signedImmediate (2 ^ bits) of
                       Just value -> [amd64Instruction (AmdOr (Amd64RmRegister scratchLeft) (Amd64BinaryImmediate value))]
                       Nothing -> [immediate scratchRight (2 ^ bits :: Integer), amd64Instruction (AmdOr (Amd64RmRegister scratchLeft) (Amd64BinaryRegister scratchRight))]
                   )
                <> [amd64Instruction (AmdBitCount AmdTzcnt dst (Amd64RmRegister scratchLeft))]
          | otherwise -> [amd64Instruction (AmdBitCount AmdTzcnt dst (Amd64RmRegister a))]

wide :: WideOp -> Type -> Amd64Register -> Amd64Register -> Amd64Register -> Amd64Register -> [Amd64Statement]
wide op ty low high a b =
  case op of
    MulWideU
      | typeBits ty == 64 -> move RAX a <> [amd64Instruction (AmdMul (Amd64RmRegister b))] <> move low RAX <> move high RDX
      | otherwise ->
          move scratchLeft a
            <> [amd64Instruction (AmdImul scratchLeft (Amd64RmRegister b))]
            <> move high scratchLeft
            <> [amd64Instruction (AmdShrImmediate (Amd64RmRegister high) (typeBits ty))]
            <> narrowRegister ty high
            <> narrowRegister ty scratchLeft
            <> move low scratchLeft
    MulWideS
      | typeBits ty == 64 -> move RAX a <> [amd64Instruction (AmdImulWide (Amd64RmRegister b))] <> move low RAX <> move high RDX
      | otherwise ->
          signExtendTo scratchLeft ty a
            <> signExtendTo scratchRight ty b
            <> [amd64Instruction (AmdImul scratchLeft (Amd64RmRegister scratchRight))]
            <> move high scratchLeft
            <> [amd64Instruction (AmdSarImmediate (Amd64RmRegister high) (typeBits ty))]
            <> narrowRegister ty high
            <> narrowRegister ty scratchLeft
            <> move low scratchLeft
    AddCarry
      | typeBits ty == 64 ->
          move scratchLeft a
            <> [amd64Instruction (AmdAdd (Amd64RmRegister scratchLeft) (Amd64BinaryRegister b))]
            <> setFlag AmdCarry high
            <> move low scratchLeft
      | otherwise ->
          move scratchLeft a
            <> [amd64Instruction (AmdAdd (Amd64RmRegister scratchLeft) (Amd64BinaryRegister b))]
            <> move high scratchLeft
            <> [amd64Instruction (AmdShrImmediate (Amd64RmRegister high) (typeBits ty))]
            <> narrowRegister ty scratchLeft
            <> move low scratchLeft
    SubBorrow ->
      move scratchLeft a
        <> [amd64Instruction (AmdSub (Amd64RmRegister scratchLeft) (Amd64BinaryRegister b))]
        <> setFlag AmdCarry high
        <> narrowRegister ty scratchLeft
        <> move low scratchLeft

compareResult :: Ctx Amd64Register -> CompareOp -> Type -> Amd64Register -> Operand -> Operand -> [Amd64Statement]
compareResult ctx op ty dst left right
  | isFloatType ty = floatCompare ctx op ty dst left right
  | otherwise =
      let signed = op `elem` [LtS, LeS, GtS, GeS]
          (loads, a) = operandIn' ctx 0 ty scratchLeft left
          (extendLeft, a') = if signed then signExtendInto ty scratchLeft a else ([], a)
       in loads <> extendLeft <> compareWith ctx ty signed a' right <> setFlag (integerCondition op) dst

floatCompare :: Ctx Amd64Register -> CompareOp -> Type -> Amd64Register -> Operand -> Operand -> [Amd64Statement]
floatCompare ctx op ty dst left right =
  let (loads, flags) = floatFlags ctx op ty left right
   in loads
        <> case op of
          Eq -> setFlag AmdEqual dst <> setFlag AmdNotParity scratchRight <> [amd64Instruction (AmdAnd (Amd64RmRegister dst) (Amd64BinaryRegister scratchRight))]
          Ne -> setFlag AmdNotEqual dst <> setFlag AmdParity scratchRight <> [amd64Instruction (AmdOr (Amd64RmRegister dst) (Amd64BinaryRegister scratchRight))]
          _ | op `elem` [FLt, FLe, FGt, FGe] -> setFlag flags dst
          _ -> [immediate dst (0 :: Integer)]

floatUnary :: FloatUnaryOp -> Type -> Amd64Register -> Amd64Register -> [Amd64Statement]
floatUnary op ty dst a =
  case op of
    FNeg -> [immediate scratchRight (signBit ty)] <> move dst a <> [amd64Instruction (AmdXor (Amd64RmRegister dst) (Amd64BinaryRegister scratchRight))]
    FAbs -> [immediate scratchRight (signBit ty - 1)] <> move dst a <> [amd64Instruction (AmdAnd (Amd64RmRegister dst) (Amd64BinaryRegister scratchRight))]
    FSqrt -> [toFloat ty 0 a, amd64Instruction (AmdSse SseSqrt (ty == F64) 0 0), fromFloat ty dst 0]

select :: Ctx Amd64Register -> Type -> Amd64Register -> Operand -> Operand -> Operand -> [Amd64Statement]
select ctx ty dst condition left right =
  case condition of
    OperandLiteral literal -> operandTo' ctx ty dst (if literal == LitInt 0 || literal == LitNull then right else left)
    OperandVar var ->
      let (conditionLoads, test) =
            case home ctx var of
              LocRegister register
                | register == dst -> (move scratchRight register, testZero scratchRight)
                | otherwise -> ([], testZero register)
              LocSlot offset -> ([], amd64Instruction (AmdCmp (Amd64RmMemory (slotMemory offset)) (Amd64BinaryImmediate 0)))
          other = if dst == scratchLeft then scratchRight else scratchLeft
       in case right of
            OperandVar rightVar
              | home ctx rightVar == LocRegister dst ->
                  let (loads, a) = operandIn' ctx 0 ty other left
                   in conditionLoads <> loads <> [test, amd64Instruction (AmdCmov AmdNotEqual dst (Amd64RmRegister a))]
            _ ->
              let (loads, b) = operandIn' ctx 0 ty other right
               in conditionLoads <> loads <> operandTo' ctx ty dst left <> [test, amd64Instruction (AmdCmov AmdEqual dst (Amd64RmRegister b))]

convert :: Ctx Amd64Register -> ConvertOp -> Type -> Type -> Amd64Register -> Amd64Register -> M [Amd64Statement]
convert _ctx op from to dst a =
  case op of
    SExt -> pure (signExtendTo dst from a <> narrowRegister to dst)
    ZExt -> pure (move dst a)
    Trunc -> pure (truncateTo dst to a)
    IToFS ->
      let (extend, a') = signExtendInto from scratchLeft a
       in pure (extend <> [amd64Instruction (AmdCvtsi2s (to == F64) 0 a'), fromFloat to dst 0])
    IToFU
      | typeBits from == 64 -> unsignedToFloat to dst a
      | otherwise -> pure [amd64Instruction (AmdCvtsi2s (to == F64) 0 a), fromFloat to dst 0]
    FToIS -> floatToInteger True from to dst a
    FToIU -> floatToInteger False from to dst a
    FpExt -> pure [amd64Instruction (AmdMovdToXmm 0 (dwordRegister a)), amd64Instruction (AmdSse SseConvertWidth False 0 0), amd64Instruction (AmdMovqFromXmm dst 0)]
    FpTrunc -> pure [amd64Instruction (AmdMovqToXmm 0 a), amd64Instruction (AmdSse SseConvertWidth True 0 0), amd64Instruction (AmdMovdFromXmm (dwordRegister dst) 0)]
    Bitcast -> pure (move dst a)

unsignedToFloat :: Type -> Amd64Register -> Amd64Register -> M [Amd64Statement]
unsignedToFloat to dst a = do
  large <- freshLabel "utof_large"
  done <- freshLabel "utof_done"
  pure
    ( [ testZero a,
        amd64Instruction (AmdJcc AmdSign large),
        amd64Instruction (AmdCvtsi2s (to == F64) 0 a),
        amd64Instruction (AmdJmp (Amd64JumpLabel done)),
        Amd64Label large
      ]
        <> move scratchRight a
        <> [amd64Instruction (AmdShrImmediate (Amd64RmRegister scratchRight) 1)]
        <> move scratchLeft a
        <> [ amd64Instruction (AmdAnd (Amd64RmRegister scratchLeft) (Amd64BinaryImmediate 1)),
             amd64Instruction (AmdOr (Amd64RmRegister scratchRight) (Amd64BinaryRegister scratchLeft)),
             amd64Instruction (AmdCvtsi2s (to == F64) 0 scratchRight),
             amd64Instruction (AmdSse SseAdd (to == F64) 0 0),
             Amd64Label done,
             fromFloat to dst 0
           ]
    )

floatToInteger :: Bool -> Type -> Type -> Amd64Register -> Amd64Register -> M [Amd64Statement]
floatToInteger signed from to dst a = do
  invalid <- trapLabel "invalid float to integer conversion"
  let widen = if from == F64 then [amd64Instruction (AmdMovqToXmm 0 a)] else [amd64Instruction (AmdMovdToXmm 0 (dwordRegister a)), amd64Instruction (AmdSse SseConvertWidth False 0 0)]
      bits = typeBits to
      (lower, excludeLower, upper) = integerConversionBounds signed from to
      bound value = [immediate scratchRight (toInteger (castDoubleToWord64 value)), amd64Instruction (AmdMovqToXmm 1 scratchRight), amd64Instruction (AmdUcomis True 0 1)]
      lowerCheck = bound lower <> [amd64Instruction (AmdJcc (if excludeLower then AmdBelowOrEqual else AmdBelow) invalid)]
      upperCheck = bound upper <> [amd64Instruction (AmdJcc AmdAboveOrEqual invalid)]
  body <-
    if signed || bits < 64
      then pure [amd64Instruction (AmdCvtts2si True dst 0)]
      else do
        large <- freshLabel "ftou_large"
        done <- freshLabel "ftou_done"
        pure
          [ immediate scratchRight (toInteger (castDoubleToWord64 (2 ^^ (63 :: Int)))),
            amd64Instruction (AmdMovqToXmm 1 scratchRight),
            amd64Instruction (AmdUcomis True 0 1),
            amd64Instruction (AmdJcc AmdAboveOrEqual large),
            amd64Instruction (AmdCvtts2si True dst 0),
            amd64Instruction (AmdJmp (Amd64JumpLabel done)),
            Amd64Label large,
            amd64Instruction (AmdSse SseSub True 0 1),
            amd64Instruction (AmdCvtts2si True dst 0),
            immediate scratchRight (signBit F64),
            amd64Instruction (AmdXor (Amd64RmRegister dst) (Amd64BinaryRegister scratchRight)),
            Amd64Label done
          ]
  pure
    ( widen
        <> [amd64Instruction (AmdUcomis True 0 0), amd64Instruction (AmdJcc AmdParity invalid)]
        <> lowerCheck
        <> upperCheck
        <> body
        <> narrowRegister to dst
    )

amd64Load :: Ctx Amd64Register -> Type -> Operand -> Integer -> Amd64Register -> [Amd64Statement]
amd64Load ctx ty base offset dst =
  let (loads, baseRegister) = operandIn' ctx 0 Ptr scratchRight base
   in loads <> [loadMemory ty dst (Amd64Memory baseRegister (fromInteger offset))] <> [mask | ty == I1, mask <- narrowRegister I1 dst]

amd64Store :: Ctx Amd64Register -> Type -> Operand -> Operand -> Integer -> [Amd64Statement]
amd64Store ctx ty value base offset =
  let (loads, a) = operandIn' ctx 0 ty scratchLeft value
      (loads', baseRegister) = operandIn' ctx 0 Ptr scratchRight base
   in loads <> loads' <> [storeMemory ty (Amd64Memory baseRegister (fromInteger offset)) a]

amd64PtrAdd :: Ctx Amd64Register -> Amd64Register -> Operand -> Amd64Register -> [Amd64Statement]
amd64PtrAdd ctx a offset dst =
  case smallImmediate I64 offset of
    Just value -> [amd64Instruction (AmdLea dst (Amd64MemoryAddress (Amd64Memory a (fromInteger value))))]
    Nothing ->
      let (loads', b) = operandIn' ctx 0 I64 scratchRight offset
       in loads' <> arithAdd dst a b
  where
    arithAdd dest left right
      | dest == left = [amd64Instruction (AmdAdd (Amd64RmRegister dest) (Amd64BinaryRegister right))]
      | right == dest = [amd64Instruction (AmdAdd (Amd64RmRegister dest) (Amd64BinaryRegister left))]
      | otherwise = move dest left <> [amd64Instruction (AmdAdd (Amd64RmRegister dest) (Amd64BinaryRegister right))]

amd64Call :: Ctx Amd64Register -> Either Symbol Signature -> [Operand] -> [Var] -> M [Amd64Statement]
amd64Call ctx callee arguments results = do
  let (convention, resultTypes, parameterTypes) = Native.calleeSignature ctx callee
      outgoing = case convention of
        CConvention -> 0
        AihcConvention -> overflowBytes' (length arguments)
      types = parameterTypes <> repeat I64
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
  let branch = case callee of
        Left symbol -> [amd64Instruction (AmdCall (lirSymbol symbol))]
        Right _ -> [amd64Instruction (AmdCallRegister scratchRight)]
      resultMoves =
        case convention of
          AihcConvention -> parallelMove' [(home ctx var, SourceLocation (LocRegister register)) | (var, register) <- zip results resultRegisters]
          CConvention ->
            concat
              [ floatResult ty <> canonicalizeRegister ty RAX <> parallelMove' [(home ctx var, SourceLocation (LocRegister RAX))]
              | (var, ty) <- zip results resultTypes
              ]
  pure (adjustStack AmdSub outgoing <> argumentMoves <> branch <> resultMoves)
  where
    operandSource ctx' ty operand =
      case operand of
        OperandVar var -> SourceLocation (home ctx' var)
        OperandLiteral literal -> SourceLiteral ty literal
    floatResult ty =
      case ty of
        F64 -> [amd64Instruction (AmdMovqFromXmm RAX 0)]
        F32 -> [amd64Instruction (AmdMovdFromXmm EAX 0)]
        _ -> []

amd64CallIndirect :: Ctx Amd64Register -> Operand -> [Operand] -> Signature -> [Var] -> M [Amd64Statement]
amd64CallIndirect ctx target arguments signature results = do
  stub <- trapLabel "indirect call to a non-function"
  body <- amd64Call ctx (Right signature) arguments results
  pure (operandTo' ctx Code scratchRight target <> [testZero scratchRight, amd64Instruction (AmdJe stub)] <> body)

amd64TailCall :: Ctx Amd64Register -> Either Text Operand -> CallingConvention -> [Type] -> [Operand] -> M [Amd64Statement]
amd64TailCall ctx callee convention parameterTypes arguments =
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
          pure (operandTo' ctx Code scratchRight operand <> [testZero scratchRight, amd64Instruction (AmdJe stub)])
      let branch = case callee of
            Left label -> jump label
            Right _ -> amd64Instruction (AmdJmp (Amd64JumpRegister scratchRight))
      pure (targetLoad <> argumentMoves <> leaveFrame ctx 0 <> [branch])
  where
    layout = ctxLayout ctx
    jump label = amd64Instruction (AmdJmp (Amd64JumpLabel (SymbolName label)))
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
            Left label -> jump label
            Right _ -> amd64Instruction (AmdJmp (Amd64JumpRegister scratchRight))
          returnSlot = frameBytes' layout
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
                let (loads, register) = operandIn' ctx displacement Code scratchRight operand
                 in loads <> move scratchRight register <> [testZero scratchRight, amd64Instruction (AmdJe label)]
              _ -> []
      pure $
        if outgoing <= incoming
          then
            overflowStores 0 (returnSlot + 8 + incoming - outgoing)
              <> targetLoad 0
              <> registerMoves 0
              <> ( if outgoing == incoming
                     then []
                     else
                       [ loadSlot scratchLeft returnSlot,
                         storeSlot scratchLeft (returnSlot + incoming - outgoing)
                       ]
                 )
              <> leaveFrame ctx 0
              <> adjustStack AmdAdd (incoming - outgoing)
              <> [branch]
          else
            if not (layoutFramed layout)
              then
                [loadSlot scratchLeft 0]
                  <> adjustStack AmdSub (outgoing - incoming)
                  <> [storeSlot scratchLeft 0]
                  <> overflowStores 0 8
                  <> targetLoad 0
                  <> registerMoves 0
                  <> [branch]
              else
                let temporary = 8 + outgoing
                    delta = fromIntegral (8 + incoming - outgoing) :: Int64
                 in adjustStack AmdSub temporary
                      <> overflowStores temporary 8
                      <> targetLoad temporary
                      <> registerMoves temporary
                      <> restoreRegisters ctx temporary
                      <> [ amd64Instruction (AmdMov RAX (Amd64MoveRegister RBP)),
                           amd64Instruction (AmdMov scratchLeft (Amd64MoveMemory (Amd64Memory RAX 8))),
                           storeSlot scratchLeft 0,
                           amd64Instruction (AmdMov RBP (Amd64MoveMemory (Amd64Memory RAX 0)))
                         ]
                      <> concat
                        [ [ loadSlot scratchLeft (8 * position),
                            amd64Instruction (AmdStore (Amd64Memory RAX (delta + fromIntegral (8 * position))) (Amd64StoreRegister scratchLeft))
                          ]
                        | position <- reverse [0 .. length overflow]
                        ]
                      <> [amd64Instruction (AmdLea RSP (Amd64MemoryAddress (Amd64Memory RAX delta))), branch]

testZero :: Amd64Register -> Amd64Statement
testZero register = amd64Instruction (AmdTest (Amd64RmRegister register) register)

clearRdx :: Amd64Statement
clearRdx = amd64Instruction (AmdXor (Amd64RmRegister EDX) (Amd64BinaryRegister EDX))

setFlag :: Amd64Condition -> Amd64Register -> [Amd64Statement]
setFlag condition register =
  [ amd64Instruction (AmdSet condition (Amd64RmRegister (byteRegister register))),
    amd64Instruction (AmdMovzx register (Amd64RmRegister (byteRegister register)))
  ]

minimumSigned :: Type -> Integer
minimumSigned ty = negate (2 ^ (typeBits ty - 1))

signBit :: Type -> Integer
signBit ty = 2 ^ (typeBits ty - 1)

narrowRegister :: Type -> Amd64Register -> [Amd64Statement]
narrowRegister ty register =
  case ty of
    I1 -> [amd64Instruction (AmdAnd (Amd64RmRegister register) (Amd64BinaryImmediate 1))]
    I8 -> [amd64Instruction (AmdMovzx register (Amd64RmRegister (byteRegister register)))]
    I16 -> [amd64Instruction (AmdMovzxWord register (Amd64RmRegister register))]
    I32 -> [amd64Instruction (AmdMov (dwordRegister register) (Amd64MoveRegister (dwordRegister register)))]
    F32 -> [amd64Instruction (AmdMov (dwordRegister register) (Amd64MoveRegister (dwordRegister register)))]
    _ -> []

truncateTo :: Amd64Register -> Type -> Amd64Register -> [Amd64Statement]
truncateTo destination ty source =
  case ty of
    I1 -> move destination source <> [amd64Instruction (AmdAnd (Amd64RmRegister destination) (Amd64BinaryImmediate 1))]
    I8 -> [amd64Instruction (AmdMovzx destination (Amd64RmRegister (byteRegister source)))]
    I16 -> [amd64Instruction (AmdMovzxWord destination (Amd64RmRegister source))]
    I32 -> [amd64Instruction (AmdMov (dwordRegister destination) (Amd64MoveRegister (dwordRegister source)))]
    _ -> move destination source

signExtendTo :: Amd64Register -> Type -> Amd64Register -> [Amd64Statement]
signExtendTo destination ty source =
  case ty of
    I1 -> move destination source <> [amd64Instruction (AmdNeg (Amd64RmRegister destination))]
    I8 -> [amd64Instruction (AmdMovsxByte destination (Amd64RmRegister (byteRegister source)))]
    I16 -> [amd64Instruction (AmdMovsxWord destination (Amd64RmRegister source))]
    I32 -> [amd64Instruction (AmdMovsxd destination (Amd64RmRegister (dwordRegister source)))]
    _ -> move destination source

signExtendInto :: Type -> Amd64Register -> Amd64Register -> ([Amd64Statement], Amd64Register)
signExtendInto ty scratch source
  | typeBits ty >= 64 = ([], source)
  | otherwise = (signExtendTo scratch ty source, scratch)

toFloat :: Type -> Int -> Amd64Register -> Amd64Statement
toFloat ty xmm general
  | ty == F64 = amd64Instruction (AmdMovqToXmm xmm general)
  | otherwise = amd64Instruction (AmdMovdToXmm xmm (dwordRegister general))

fromFloat :: Type -> Amd64Register -> Int -> Amd64Statement
fromFloat ty general xmm
  | ty == F64 = amd64Instruction (AmdMovqFromXmm general xmm)
  | otherwise = amd64Instruction (AmdMovdFromXmm (dwordRegister general) xmm)

byteRegister :: Amd64Register -> Amd64Register
byteRegister register =
  case register of
    RAX -> AL
    RCX -> CL
    RDX -> DL
    RBX -> BL
    RSP -> SPL
    RBP -> BPL
    RSI -> SIL
    RDI -> DIL
    R8 -> R8B
    R9 -> R9B
    R10 -> R10B
    R11 -> R11B
    R12 -> R12B
    R13 -> R13B
    R14 -> R14B
    R15 -> R15B
    other -> other

dwordRegister :: Amd64Register -> Amd64Register
dwordRegister register =
  case register of
    RAX -> EAX
    RCX -> ECX
    RDX -> EDX
    RBX -> EBX
    RSP -> ESP
    RBP -> EBP
    RSI -> ESI
    RDI -> EDI
    R8 -> R8D
    R9 -> R9D
    R10 -> R10D
    R11 -> R11D
    R12 -> R12D
    R13 -> R13D
    R14 -> R14D
    R15 -> R15D
    other -> other

floatOp :: FloatBinaryOp -> Amd64SseOp
floatOp op =
  case op of
    FAdd -> SseAdd
    FSub -> SseSub
    FMul -> SseMul
    FDiv -> SseDiv

integerCondition :: CompareOp -> Amd64Condition
integerCondition op =
  case op of
    Eq -> AmdEqual
    Ne -> AmdNotEqual
    LtS -> AmdLess
    LtU -> AmdBelow
    LeS -> AmdLessOrEqual
    LeU -> AmdBelowOrEqual
    GtS -> AmdGreater
    GtU -> AmdAbove
    GeS -> AmdGreaterOrEqual
    GeU -> AmdAboveOrEqual
    FLt -> AmdBelow
    FLe -> AmdBelowOrEqual
    FGt -> AmdAbove
    FGe -> AmdAboveOrEqual

inverseCondition :: Amd64Condition -> Amd64Condition
inverseCondition condition =
  case condition of
    AmdOverflow -> AmdNotOverflow
    AmdNotOverflow -> AmdOverflow
    AmdCarry -> AmdAboveOrEqual
    AmdBelow -> AmdAboveOrEqual
    AmdAboveOrEqual -> AmdBelow
    AmdEqual -> AmdNotEqual
    AmdNotEqual -> AmdEqual
    AmdBelowOrEqual -> AmdAbove
    AmdAbove -> AmdBelowOrEqual
    AmdLess -> AmdGreaterOrEqual
    AmdGreaterOrEqual -> AmdLess
    AmdLessOrEqual -> AmdGreater
    AmdGreater -> AmdLessOrEqual
    AmdSign -> AmdNotSign
    AmdNotSign -> AmdSign
    AmdParity -> AmdNotParity
    AmdNotParity -> AmdParity

loadMemory :: Type -> Amd64Register -> Amd64Memory -> Amd64Statement
loadMemory ty value memory =
  case typeBytes ty of
    1 -> amd64Instruction (AmdMovzx value (Amd64RmMemory memory))
    2 -> amd64Instruction (AmdMovzxWord value (Amd64RmMemory memory))
    4 -> amd64Instruction (AmdMov (dwordRegister value) (Amd64MoveMemory memory))
    _ -> amd64Instruction (AmdMov value (Amd64MoveMemory memory))

storeMemory :: Type -> Amd64Memory -> Amd64Register -> Amd64Statement
storeMemory ty memory value =
  case typeBytes ty of
    1 -> amd64Instruction (AmdStoreByte memory (byteRegister value))
    2 -> amd64Instruction (AmdStoreWord memory value)
    4 -> amd64Instruction (AmdStore memory (Amd64StoreRegister (dwordRegister value)))
    _ -> amd64Instruction (AmdStore memory (Amd64StoreRegister value))
