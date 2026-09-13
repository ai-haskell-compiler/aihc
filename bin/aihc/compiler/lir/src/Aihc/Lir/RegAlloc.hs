-- | A linear-scan register allocator over Lir functions.
--
-- The allocator is target-independent. A backend describes the registers it
-- is willing to give away and receives, for every value of the function,
-- either one of those registers or the verdict that the value stays in a
-- frame slot.
--
-- The registers come in two classes. A volatile register is clobbered by
-- every call and costs nothing to use. A preserved register survives a C
-- call, because the C callee saves it, and is clobbered by an aihc call,
-- because an aihc function saves nothing. So a value that lives across a C
-- call takes a preserved register, a value that lives across an aihc call
-- goes to a frame slot, and everything else takes whatever is free. That is
-- the whole of the interaction between calls and registers: no interval is
-- ever split, and no register is ever pre-colored.
--
-- The intervals are conservative. A value gets one contiguous interval from
-- the lowest to the highest position at which it is live, with no holes and
-- no splitting, so a value that dies and revives inside the span keeps its
-- register throughout. That costs registers on a wide function and buys
-- independence from the block order: the result is correct whatever order
-- the blocks arrive in and whatever the loops look like.
--
-- A hint is a register the scan tries first. Parameters, call arguments,
-- call results, and returned values are hinted with the register the
-- convention puts them in. The argument of a jump and the block parameter
-- it reaches are partners: each prefers the register the other already has,
-- and failing that the register the other was hinted with. Last, a result
-- prefers the register of an operand of its own instruction, which is free
-- exactly when the operand dies there; on a two-operand machine that is the
-- difference between one instruction and two. A hint that is not free at
-- the time is dropped, so hints cost nothing in correctness and buy most of
-- the moves that a convention would otherwise need.
--
-- The allocator names every value and every block by a number while it
-- works: values by their rank among the names of the function, so that the
-- number order is the name order, and blocks by their position. The names
-- come back only in the result.
--
-- Memory is the constraint the design obeys. The compiler runs the
-- allocator on every function of every module, so a pass that builds one
-- small container for each instruction costs more than the scan itself. No
-- pass holds a list or a persistent map: 'encodeFunction' flattens the
-- function into unboxed arrays in one walk, liveness holds one bit for each
-- value of each block, the intervals are two arrays of positions, and the
-- scan keeps its free registers, its active intervals, and its result in
-- mutable arrays.
module Aihc.Lir.RegAlloc
  ( Allocation (..),
    Registers (..),
    allocateRegistersFor,
    Interval (..),
    functionIntervals,
    readCounts,
  )
where

import Aihc.Lir.Syntax
import Control.Applicative (Const (..))
import Control.Monad (unless, when)
import Control.Monad.ST (ST, runST)
import Data.Array.Base (getNumElements)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.Array.Unboxed (Array, UArray, bounds, elems, listArray, (!))
import Data.Array.Unsafe (unsafeFreeze)
import Data.Bits (complement, countTrailingZeros, shiftL, (.&.), (.|.))
import Data.Foldable (traverse_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Word (Word64)

-- | Where every value of one function lives.
data Allocation register = Allocation
  { -- | The values that live in a register.
    allocationRegisters :: !(Map Var register),
    -- | The values that live in a frame slot, in the order the function
    -- defines them. The backend gives each one a slot.
    allocationSpills :: ![Var],
    -- | The registers the allocator handed out, in pool order. The backend
    -- saves the preserved ones among them when its convention asks for it.
    allocationUsed :: ![register]
  }
  deriving (Eq, Show)

-- | The live interval of one value: the lowest and the highest position at
-- which it is live. Positions number the function in block order.
data Interval = Interval
  { intervalVar :: !Var,
    intervalStart :: !Int,
    intervalEnd :: !Int
  }
  deriving (Eq, Show)

-- | The registers a backend offers and what the calls of a function do to
-- them.
data Registers register = Registers
  { -- | The registers every call clobbers, in preference order.
    registersVolatile :: ![register],
    -- | The registers a C call preserves and an aihc call clobbers, in
    -- preference order.
    registersPreserved :: ![register],
    -- | Whether a preserved register costs the function a save and a
    -- restore. It does under the C convention, where the caller expects the
    -- register back, and it does not under the aihc convention.
    registersPreservedCost :: !Bool,
    -- | The register that carries parameter and argument number @i@ under
    -- the conventions of the target, when one does.
    registersArgument :: !(Int -> Maybe register),
    -- | The register that carries result number @i@ of a call and of a
    -- return, when one does.
    registersResult :: !(Int -> Maybe register)
  }

-- | Assign the registers of the target to the values of the function. The
-- signatures resolve the convention of every direct call.
allocateRegistersFor :: (Ord register) => Registers register -> Map Symbol Signature -> Function -> Allocation register
allocateRegistersFor target signatures function =
  Allocation
    { allocationRegisters =
        Map.fromDistinctAscList
          [ (encNames encoded ! value, registerArray ! register)
          | value <- [0 .. encValues encoded - 1],
            let register = assigned ! value,
            register >= 0
          ],
      allocationSpills = [encNames encoded ! value | value <- elems (encDefinitions encoded), assigned ! value < 0],
      allocationUsed = [register | (index, register) <- zip [0 ..] pool, used ! index]
    }
  where
    encoded = encodeFunction signatures function
    volatile = registersVolatile target
    pool = volatile <> registersPreserved target
    poolSize = length pool
    registerArray = boxedArray poolSize pool
    -- The pool is small, so a register finds its index by a walk, once
    -- per argument or result position.
    indexed = zip pool [0 ..]
    indexOf register = fromMaybe (-1) (lookup register indexed)
    carriers =
      Carriers
        { carrierArgument = memoIndex (registersArgument target),
          carrierResult = memoIndex (registersResult target)
        }
    -- The pool index of the carrier of a position, or -1 when the
    -- convention names no register for it.
    memoIndex carrier =
      let table = listArray (0, memoLimit) [maybe (-1) indexOf (carrier index) | index <- [0 .. memoLimit]] :: UArray Int Int
       in \index -> if index <= memoLimit then table ! index else maybe (-1) indexOf (carrier index)
    memoLimit = 15
    (assigned, used) = runAllocation encoded carriers poolSize (length volatile) (registersPreservedCost target)

-- | The live interval of every value of the function, in name order.
functionIntervals :: Function -> [Interval]
functionIntervals function =
  [ Interval {intervalVar = encNames encoded ! value, intervalStart = starts ! value, intervalEnd = ends ! value}
  | value <- [0 .. encValues encoded - 1]
  ]
  where
    encoded = encodeFunction Map.empty function
    (starts, ends) = runST (functionSpans encoded)

-- Arrays

-- | These wrappers fix the array type, which the class of the operation
-- leaves open.
boxedArray :: Int -> [e] -> Array Int e
boxedArray size = listArray (0, size - 1)

{-# INLINE newIntArray #-}
newIntArray :: Int -> Int -> ST s (STUArray s Int Int)
newIntArray size = newArray (0, size - 1)

{-# INLINE newBoolArray #-}
newBoolArray :: Int -> Bool -> ST s (STUArray s Int Bool)
newBoolArray size = newArray (0, size - 1)

{-# INLINE newWordArray #-}
newWordArray :: Int -> ST s (STUArray s Int Word64)
newWordArray size = newArray (0, size - 1) 0

{-# INLINE freezeInts #-}
freezeInts :: STUArray s Int Int -> ST s (UArray Int Int)
freezeInts = unsafeFreeze

{-# INLINE freezeBools #-}
freezeBools :: STUArray s Int Bool -> ST s (UArray Int Bool)
freezeBools = unsafeFreeze

-- | The number of elements of an array that starts at index zero.
{-# INLINE lengthOf #-}
lengthOf :: UArray Int Int -> Int
lengthOf array = snd (bounds array) + 1

-- | A growable array of integers. The encoder pushes into one of these and
-- freezes it once, so no pass builds a list.
data IntBuffer s = IntBuffer !(STRef s (STUArray s Int Int)) !(STRef s Int)

newIntBuffer :: Int -> ST s (IntBuffer s)
newIntBuffer capacity = do
  array <- newIntArray (max 4 capacity) 0
  IntBuffer <$> newSTRef array <*> newSTRef 0

{-# INLINE pushInt #-}
pushInt :: IntBuffer s -> Int -> ST s ()
pushInt (IntBuffer arrayRef countRef) value = do
  array <- readSTRef arrayRef
  count <- readSTRef countRef
  capacity <- getNumElements array
  target <-
    if count < capacity
      then pure array
      else do
        bigger <- newIntArray (capacity * 2) 0
        copyInts array bigger capacity
        writeSTRef arrayRef bigger
        pure bigger
  writeArray target count value
  writeSTRef countRef (count + 1)

{-# INLINE bufferLength #-}
bufferLength :: IntBuffer s -> ST s Int
bufferLength (IntBuffer _ countRef) = readSTRef countRef

{-# INLINE bufferAt #-}
bufferAt :: IntBuffer s -> Int -> ST s Int
bufferAt (IntBuffer arrayRef _) index = do
  array <- readSTRef arrayRef
  readArray array index

clearBuffer :: IntBuffer s -> ST s ()
clearBuffer (IntBuffer _ countRef) = writeSTRef countRef 0

freezeBuffer :: IntBuffer s -> ST s (UArray Int Int)
freezeBuffer (IntBuffer arrayRef countRef) = do
  array <- readSTRef arrayRef
  count <- readSTRef countRef
  result <- newIntArray count 0
  copyInts array result count
  freezeInts result

{-# INLINE copyInts #-}
copyInts :: STUArray s Int Int -> STUArray s Int Int -> Int -> ST s ()
copyInts from to count = go 0
  where
    go index =
      when (index < count) $ do
        value <- readArray from index
        writeArray to index value
        go (index + 1)

-- | Replace every value in an array with its rank.
{-# INLINE renameArray #-}
renameArray :: UArray Int Int -> STUArray s Int Int -> Int -> ST s ()
renameArray ranks array count = go 0
  where
    go index =
      when (index < count) $ do
        value <- readArray array index
        writeArray array index (ranks ! value)
        go (index + 1)

-- | Replace every value in a buffer with its rank. The encoder numbers the
-- values as it meets them and moves them to name order at the end.
renameBuffer :: UArray Int Int -> Int -> Int -> IntBuffer s -> ST s ()
renameBuffer ranks stride start buffer@(IntBuffer arrayRef _) = do
  count <- bufferLength buffer
  array <- readSTRef arrayRef
  let go index =
        when (index < count) $ do
          value <- readArray array index
          when (value >= 0) (writeArray array index (ranks ! value))
          go (index + stride)
  go start

-- Encoding

-- | A function with its values, blocks, instructions, and positions
-- numbered, and every list of the function flattened into an array.
--
-- A field named @...Offset@ holds one element more than the rows it
-- describes: row @i@ occupies the elements from @offset ! i@ up to
-- @offset ! (i + 1)@ of the array it indexes.
data Encoded = Encoded
  { -- | Every value of the function by its rank among the names.
    encNames :: !(Array Int Var),
    encValues :: !Int,
    -- | One position more than the highest position of the function. A
    -- block ends one position after its terminator, so the last block gives
    -- the highest position.
    encPositions :: !Int,
    -- | The parameters of the function, in order.
    encParameters :: !(UArray Int Int),
    -- | Every value the function defines, in the order the text defines it.
    encDefinitions :: !(UArray Int Int),
    encBlocks :: !Int,
    -- | Every position is distinct and the positions of a block are a
    -- contiguous run, so the whole block sits between its start and its
    -- end.
    encBlockStart :: !(UArray Int Int),
    -- | A block ends one position after its terminator.
    encBlockTerminator :: !(UArray Int Int),
    -- | Whether the terminator restores the saved registers: a return or a
    -- tail call. A trap does not return, so it restores nothing.
    encBlockExit :: !(UArray Int Bool),
    encBlockParameterOffset :: !(UArray Int Int),
    encBlockParameter :: !(UArray Int Int),
    -- | The instructions of a block, as a range of instruction numbers.
    encBlockInstructionOffset :: !(UArray Int Int),
    -- | Every read of a terminator, in order and with repeats.
    encTerminatorReadOffset :: !(UArray Int Int),
    encTerminatorRead :: !(UArray Int Int),
    -- | The blocks a terminator jumps to.
    encTargetOffset :: !(UArray Int Int),
    encTargetBlock :: !(UArray Int Int),
    -- | The value each jump argument carries, or -1 for a literal.
    encTargetArgumentOffset :: !(UArray Int Int),
    encTargetArgument :: !(UArray Int Int),
    encInstructions :: !Int,
    encInstructionPosition :: !(UArray Int Int),
    -- | The convention of the callee: 'callNone', 'callAihc', or 'callC'.
    encInstructionCall :: !(UArray Int Int),
    -- | Every read of an instruction, in order and with repeats.
    encReadOffset :: !(UArray Int Int),
    encRead :: !(UArray Int Int),
    encResultOffset :: !(UArray Int Int),
    encResult :: !(UArray Int Int),
    -- | The instruction that defines each value, or -1 for a parameter.
    encDefiner :: !(UArray Int Int),
    -- | The values a call hands to and takes from the convention, as
    -- triples of a kind, a position, and a value. The kind is
    -- 'hintArgument' or 'hintResult'.
    encInstructionHint :: !(UArray Int Int),
    -- | The same for the values a terminator hands to the convention: a
    -- tail-call argument, or a returned value.
    encTerminatorHint :: !(UArray Int Int)
  }

callNone, callAihc, callC :: Int
callNone = 0
callAihc = 1
callC = 2

hintArgument, hintResult :: Int
hintArgument = 0
hintResult = 1

-- | Number the function and flatten it, in one walk of the blocks.
encodeFunction :: Map Symbol Signature -> Function -> Encoded
encodeFunction signatures function = runST $ do
  let blocks = functionBlocks function
      blockCount = length blocks
      instructionCount = foldl' (\total block -> total + length (blockInstructions block)) 0 blocks
      parameterCount = length (functionParameters function)
      labels = Map.fromList (zip (map blockLabel blocks) [0 :: Int ..])
  identifiers <- newSTRef (Map.empty :: Map Var Int)
  -- The number the next new value takes. An array holds it unboxed.
  nextIdentifier <- newIntArray 1 0
  let intern var = do
        known <- readSTRef identifiers
        case Map.lookup var known of
          Just identifier -> pure identifier
          Nothing -> do
            identifier <- readArray nextIdentifier 0
            writeSTRef identifiers (Map.insert var identifier known)
            writeArray nextIdentifier 0 (identifier + 1)
            pure identifier
      internOperand operand =
        case operand of
          OperandVar var -> intern var
          OperandLiteral _ -> pure (-1)
  blockStart <- newIntArray blockCount 0
  blockTerminatorPosition <- newIntArray blockCount 0
  blockExit <- newBoolArray blockCount False
  blockParameterOffset <- newIntArray (blockCount + 1) 0
  blockInstructionOffset <- newIntArray (blockCount + 1) 0
  terminatorReadOffset <- newIntArray (blockCount + 1) 0
  targetOffset <- newIntArray (blockCount + 1) 0
  instructionPosition <- newIntArray instructionCount 0
  instructionCall <- newIntArray instructionCount callNone
  readOffset <- newIntArray (instructionCount + 1) 0
  resultOffset <- newIntArray (instructionCount + 1) 0
  parameters <- newIntArray parameterCount 0
  blockParameter <- newIntBuffer 4
  terminatorRead <- newIntBuffer 8
  targetBlock <- newIntBuffer 4
  targetArgumentOffset <- newIntBuffer 4
  targetArgument <- newIntBuffer 4
  instructionRead <- newIntBuffer (2 * instructionCount + 4)
  instructionResult <- newIntBuffer (instructionCount + 4)
  instructionHint <- newIntBuffer 4
  terminatorHint <- newIntBuffer 4
  let defineTyped _ [] = pure ()
      defineTyped buffer ((var, _) : rest) = (intern var >>= pushInt buffer) >> defineTyped buffer rest
      define _ [] = pure ()
      define buffer (var : rest) = (intern var >>= pushInt buffer) >> define buffer rest
      pushRead buffer operand = do
        value <- internOperand operand
        when (value >= 0) (pushInt buffer value)
      pushArgument operand = internOperand operand >>= pushInt targetArgument
      pushArguments [] = pure ()
      pushArguments (operand : rest) = pushArgument operand >> pushArguments rest
      pushTargets [] = pure ()
      pushTargets (jump : rest) = do
        pushInt targetBlock (labels Map.! targetLabel jump)
        pushInt targetArgumentOffset =<< bufferLength targetArgument
        pushArguments (targetArguments jump)
        pushTargets rest
      pushHint buffer kind position value = do
        pushInt buffer kind
        pushInt buffer position
        pushInt buffer value
      pushOperandHints buffer kind = go 0
        where
          go _ [] = pure ()
          go position (operand : rest) = do
            case operand of
              OperandVar var -> intern var >>= pushHint buffer kind position
              OperandLiteral _ -> pure ()
            go (position + 1) rest
      pushResultHints buffer = go 0
        where
          go _ [] = pure ()
          go position (var : rest) = do
            intern var >>= pushHint buffer hintResult position
            go (position + 1) rest
  let goParameter _ [] = pure ()
      goParameter position ((var, _) : rest) = do
        value <- intern var
        writeArray parameters position value
        goParameter (position + 1) rest
  goParameter 0 (functionParameters function)
  let goBlock _ _ [] = pure ()
      goBlock index start (block : rest) = do
        writeArray blockStart index start
        writeArray blockParameterOffset index =<< bufferLength blockParameter
        defineTyped blockParameter (blockParameters block)
        firstInstruction <- readArray blockInstructionOffset index
        let goInstruction number _ [] = pure number
            goInstruction number position (Instruction results operation : more) = do
              writeArray instructionPosition number position
              writeArray resultOffset number =<< bufferLength instructionResult
              define instructionResult results
              writeArray readOffset number =<< bufferLength instructionRead
              forOperationOperands (pushRead instructionRead) operation
              case operation of
                Call symbol arguments -> do
                  writeArray instructionCall number (conventionOf (maybe AihcConvention signatureConvention (Map.lookup symbol signatures)))
                  pushOperandHints instructionHint hintArgument arguments
                  pushResultHints instructionHint results
                CallIndirect _ arguments signature -> do
                  writeArray instructionCall number (conventionOf (signatureConvention signature))
                  pushOperandHints instructionHint hintArgument arguments
                  pushResultHints instructionHint results
                _ -> pure ()
              goInstruction (number + 1) (position + 1) more
        afterInstructions <- goInstruction firstInstruction (start + 1) (blockInstructions block)
        writeArray blockInstructionOffset (index + 1) afterInstructions
        let terminatorPosition = start + 1 + (afterInstructions - firstInstruction)
            terminator = blockTerminator block
        writeArray blockTerminatorPosition index terminatorPosition
        writeArray terminatorReadOffset index =<< bufferLength terminatorRead
        forTerminatorOperands (pushRead terminatorRead) terminator
        writeArray targetOffset index =<< bufferLength targetBlock
        pushTargets (terminatorTargets terminator)
        writeArray blockExit index (exitTerminator terminator)
        case terminator of
          TailCall _ arguments -> pushOperandHints terminatorHint hintArgument arguments
          TailCallIndirect _ arguments _ -> pushOperandHints terminatorHint hintArgument arguments
          Return values -> pushOperandHints terminatorHint hintResult values
          _ -> pure ()
        goBlock (index + 1) (terminatorPosition + 2) rest
  goBlock 0 1 blocks
  writeArray blockParameterOffset blockCount =<< bufferLength blockParameter
  writeArray terminatorReadOffset blockCount =<< bufferLength terminatorRead
  writeArray targetOffset blockCount =<< bufferLength targetBlock
  writeArray readOffset instructionCount =<< bufferLength instructionRead
  writeArray resultOffset instructionCount =<< bufferLength instructionResult
  pushInt targetArgumentOffset =<< bufferLength targetArgument
  -- The values take their rank in name order, so that the number order and
  -- the name order agree.
  known <- readSTRef identifiers
  let valueCount = Map.size known
  rankArray <- newIntArray valueCount 0
  let rankOf rank (identifier : more) = writeArray rankArray identifier rank >> rankOf (rank + 1) more
      rankOf _ [] = pure ()
  rankOf 0 (Map.elems known)
  ranks <- freezeInts rankArray
  renameBuffer ranks 1 0 blockParameter
  renameBuffer ranks 1 0 terminatorRead
  renameBuffer ranks 1 0 targetArgument
  renameBuffer ranks 1 0 instructionRead
  renameBuffer ranks 1 0 instructionResult
  renameBuffer ranks 3 2 instructionHint
  renameBuffer ranks 3 2 terminatorHint
  -- Every value the function defines, in the order the text defines it.
  definitions <- newIntArray valueCount 0
  cursor <- newIntArray 1 0
  let takeDefinition value = do
        at <- readArray cursor 0
        writeArray definitions at value
        writeArray cursor 0 (at + 1)
      takeRow buffer offsets index = do
        from <- readArray offsets index
        to <- readArray offsets (index + 1)
        let go at = when (at < to) (bufferAt buffer at >>= takeDefinition >> go (at + 1))
        go from
      takeBlock index =
        when (index < blockCount) $ do
          takeRow blockParameter blockParameterOffset index
          firstInstruction <- readArray blockInstructionOffset index
          afterInstructions <- readArray blockInstructionOffset (index + 1)
          let goResults number =
                when (number < afterInstructions) $ do
                  takeRow instructionResult resultOffset number
                  goResults (number + 1)
          goResults firstInstruction
          takeBlock (index + 1)
  renameArray ranks parameters parameterCount
  let goParameters at = when (at < parameterCount) (readArray parameters at >>= takeDefinition >> goParameters (at + 1))
  goParameters 0
  takeBlock 0
  definer <- newIntArray valueCount (-1)
  let noteDefiner number =
        when (number < instructionCount) $ do
          from <- readArray resultOffset number
          to <- readArray resultOffset (number + 1)
          let go index =
                when (index < to) $ do
                  value <- bufferAt instructionResult index
                  writeArray definer value number
                  go (index + 1)
          go from
          noteDefiner (number + 1)
  noteDefiner 0
  lastTerminator <- if blockCount == 0 then pure (-1) else readArray blockTerminatorPosition (blockCount - 1)
  parametersFrozen <- freezeInts parameters
  definitionsFrozen <- freezeInts definitions
  blockStartFrozen <- freezeInts blockStart
  blockTerminatorFrozen <- freezeInts blockTerminatorPosition
  blockExitFrozen <- freezeBools blockExit
  blockParameterOffsetFrozen <- freezeInts blockParameterOffset
  blockParameterFrozen <- freezeBuffer blockParameter
  blockInstructionOffsetFrozen <- freezeInts blockInstructionOffset
  terminatorReadOffsetFrozen <- freezeInts terminatorReadOffset
  terminatorReadFrozen <- freezeBuffer terminatorRead
  targetOffsetFrozen <- freezeInts targetOffset
  targetBlockFrozen <- freezeBuffer targetBlock
  targetArgumentOffsetFrozen <- freezeBuffer targetArgumentOffset
  targetArgumentFrozen <- freezeBuffer targetArgument
  instructionPositionFrozen <- freezeInts instructionPosition
  instructionCallFrozen <- freezeInts instructionCall
  readOffsetFrozen <- freezeInts readOffset
  instructionReadFrozen <- freezeBuffer instructionRead
  resultOffsetFrozen <- freezeInts resultOffset
  instructionResultFrozen <- freezeBuffer instructionResult
  definerFrozen <- freezeInts definer
  instructionHintFrozen <- freezeBuffer instructionHint
  terminatorHintFrozen <- freezeBuffer terminatorHint
  pure
    Encoded
      { encNames = listArray (0, valueCount - 1) (Map.keys known),
        encValues = valueCount,
        encPositions = lastTerminator + 2,
        encParameters = parametersFrozen,
        encDefinitions = definitionsFrozen,
        encBlocks = blockCount,
        encBlockStart = blockStartFrozen,
        encBlockTerminator = blockTerminatorFrozen,
        encBlockExit = blockExitFrozen,
        encBlockParameterOffset = blockParameterOffsetFrozen,
        encBlockParameter = blockParameterFrozen,
        encBlockInstructionOffset = blockInstructionOffsetFrozen,
        encTerminatorReadOffset = terminatorReadOffsetFrozen,
        encTerminatorRead = terminatorReadFrozen,
        encTargetOffset = targetOffsetFrozen,
        encTargetBlock = targetBlockFrozen,
        encTargetArgumentOffset = targetArgumentOffsetFrozen,
        encTargetArgument = targetArgumentFrozen,
        encInstructions = instructionCount,
        encInstructionPosition = instructionPositionFrozen,
        encInstructionCall = instructionCallFrozen,
        encReadOffset = readOffsetFrozen,
        encRead = instructionReadFrozen,
        encResultOffset = resultOffsetFrozen,
        encResult = instructionResultFrozen,
        encDefiner = definerFrozen,
        encInstructionHint = instructionHintFrozen,
        encTerminatorHint = terminatorHintFrozen
      }
  where
    conventionOf convention =
      case convention of
        AihcConvention -> callAihc
        CConvention -> callC
    exitTerminator terminator =
      case terminator of
        Return _ -> True
        TailCall _ _ -> True
        TailCallIndirect {} -> True
        _ -> False

-- | Run an action on every operand one operation reads, in order and with
-- repeats.
forOperationOperands :: (Applicative f) => (Operand -> f ()) -> Operation -> f ()
{-# INLINE forOperationOperands #-}
forOperationOperands act operation =
  case operation of
    Binary _ _ left right -> act left *> act right
    Unary _ _ value -> act value
    Wide _ _ left right -> act left *> act right
    Compare _ _ left right -> act left *> act right
    FloatBinary _ _ left right -> act left *> act right
    FloatUnary _ _ value -> act value
    Convert _ _ value _ -> act value
    PtrToInt value -> act value
    PtrFromInt value -> act value
    Select _ condition left right -> act condition *> act left *> act right
    Load _ address _ -> act (addressBase address)
    Store _ value address _ -> act value *> act (addressBase address)
    PtrAdd base offset -> act base *> act offset
    StackAlloc _ _ -> pure ()
    GlobalGet _ -> pure ()
    GlobalSet _ value -> act value
    Call _ arguments -> traverse_ act arguments
    CallIndirect callee arguments _ -> act callee *> traverse_ act arguments

-- | Run an action on every operand one terminator reads, in order and with
-- repeats.
forTerminatorOperands :: (Applicative f) => (Operand -> f ()) -> Terminator -> f ()
{-# INLINE forTerminatorOperands #-}
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

-- Liveness

-- | The values that are live at the start and at the end of each block, as
-- one bit for each value of each block.
--
-- A jump argument is a read of the block that jumps, and a block parameter
-- is a write of the block that receives it, made before anything in the
-- block reads it.
liveness :: Encoded -> Int -> ST s (Maybe (STUArray s Int Word64, STUArray s Int Word64))
liveness encoded stride
  -- A function whose blocks jump nowhere has nothing live across a block
  -- boundary: a block parameter is a definition, and a use of something a
  -- block does not define is a parameter, which is defined before the
  -- first block.
  | encTargetOffset encoded ! encBlocks encoded == 0 = pure Nothing
  | otherwise = do
      let blockCount = encBlocks encoded
      liveIn <- newWordArray (blockCount * stride)
      liveOut <- newWordArray (blockCount * stride)
      scratch <- newWordArray stride
      -- The values a block reads before it writes them.
      upward <- newIntBuffer 16
      upwardOffset <- newIntArray (blockCount + 1) 0
      let clearDefinitions index = do
            forRange (encBlockParameterOffset encoded) index (\at -> clearWord scratch (encBlockParameter encoded ! at))
            let go number =
                  when (number < encBlockInstructionOffset encoded ! (index + 1)) $ do
                    forRange (encResultOffset encoded) number (\at -> clearWord scratch (encResult encoded ! at))
                    go (number + 1)
            go (encBlockInstructionOffset encoded ! index)
          collectUpward index =
            when (index < blockCount) $ do
              clearWords scratch 0 stride
              forRange (encTerminatorReadOffset encoded) index (\at -> setWord scratch (encTerminatorRead encoded ! at))
              let back number =
                    when (number >= encBlockInstructionOffset encoded ! index) $ do
                      forRange (encResultOffset encoded) number (\at -> clearWord scratch (encResult encoded ! at))
                      forRange (encReadOffset encoded) number (\at -> setWord scratch (encRead encoded ! at))
                      back (number - 1)
              back (encBlockInstructionOffset encoded ! (index + 1) - 1)
              forRange (encBlockParameterOffset encoded) index (\at -> clearWord scratch (encBlockParameter encoded ! at))
              writeArray upwardOffset index =<< bufferLength upward
              forWords scratch 0 stride (pushInt upward)
              collectUpward (index + 1)
      collectUpward 0
      writeArray upwardOffset blockCount =<< bufferLength upward
      upwardValue <- freezeBuffer upward
      upwardRange <- freezeInts upwardOffset
      let update index = do
            let base = index * stride
            clearWords liveOut base stride
            forRange (encTargetOffset encoded) index $ \at ->
              unionWords liveOut base liveIn (encTargetBlock encoded ! at * stride) stride
            copyWords liveOut base scratch 0 stride
            clearDefinitions index
            forRange upwardRange index (\at -> setWord scratch (upwardValue ! at))
            different <- differentWords scratch 0 liveIn base stride
            when different (copyWords scratch 0 liveIn base stride)
            pure different
          sweep index changed
            | index < 0 = pure changed
            | otherwise = do
                here <- update index
                sweep (index - 1) (changed || here)
          converge = do
            changed <- sweep (blockCount - 1) False
            when changed converge
      converge
      pure (Just (liveIn, liveOut))

-- Intervals

-- | The lowest and the highest position at which each value is live.
--
-- A value is relevant at its definition, at each of its uses, at the start of
-- every block it is live into, and at the end of every block it is live out
-- of. The interval spans the lowest to the highest of those positions, which
-- covers every point at which the value is live whatever the block order.
functionSpans :: Encoded -> ST s (UArray Int Int, UArray Int Int)
functionSpans encoded = do
  let valueCount = encValues encoded
      blockCount = encBlocks encoded
      stride = max 1 ((valueCount + 63) `div` 64)
  starts <- newIntArray valueCount maxBound
  ends <- newIntArray valueCount minBound
  let touch position value = do
        start <- readArray starts value
        when (position < start) (writeArray starts value position)
        end <- readArray ends value
        when (position > end) (writeArray ends value position)
  -- A parameter is defined before the first block.
  forAll (encParameters encoded) (touch 0)
  live <- liveness encoded stride
  let goBlock index =
        when (index < blockCount) $ do
          let start = encBlockStart encoded ! index
          forRange (encBlockParameterOffset encoded) index (\at -> touch start (encBlockParameter encoded ! at))
          let goInstruction number =
                when (number < encBlockInstructionOffset encoded ! (index + 1)) $ do
                  let position = encInstructionPosition encoded ! number
                  forRange (encResultOffset encoded) number (\at -> touch position (encResult encoded ! at))
                  forRange (encReadOffset encoded) number (\at -> touch position (encRead encoded ! at))
                  goInstruction (number + 1)
          goInstruction (encBlockInstructionOffset encoded ! index)
          forRange
            (encTerminatorReadOffset encoded)
            index
            (\at -> touch (encBlockTerminator encoded ! index) (encTerminatorRead encoded ! at))
          case live of
            Nothing -> pure ()
            Just (liveIn, liveOut) -> do
              forWords liveIn (index * stride) stride (touch start)
              forWords liveOut (index * stride) stride (touch (encBlockTerminator encoded ! index + 1))
          goBlock (index + 1)
  goBlock 0
  (,) <$> freezeInts starts <*> freezeInts ends

-- Allocation

-- | The pool index of the register that carries argument or result number
-- @i@, or -1 when the convention names none.
data Carriers = Carriers
  { carrierArgument :: Int -> Int,
    carrierResult :: Int -> Int
  }

-- | Run an action on the value and the suggested pool index of every hint
-- of a table. The pool index is -1 when the convention names no register.
forHintTable :: Carriers -> UArray Int Int -> (Int -> Int -> ST s ()) -> ST s ()
{-# INLINE forHintTable #-}
forHintTable carriers table act = go 0
  where
    limit = lengthOf table
    go at =
      when (at < limit) $ do
        let position = table ! (at + 1)
        act
          (table ! (at + 2))
          (if table ! at == hintArgument then carrierArgument carriers position else carrierResult carriers position)
        go (at + 3)

-- | Which registers an interval may take, given the calls it lives across.
-- A call at the start of the interval defines it and a call at its end
-- consumes it; neither clobbers it.
reachAny, reachPreserved, reachNone :: Int
reachAny = 0
reachPreserved = 1
reachNone = 2

-- | Walk the intervals in order of their start and hand out registers, by
-- pool index. The result gives the pool index of every value, or -1 when the
-- value stays in a frame slot, and the registers the scan handed out.
--
-- An interval that outlives another may take its register once that one has
-- expired. A hint of the value that is free is taken first, then the
-- register of a partner already placed, then a hint of a partner, then the
-- register of an operand that just died, then the first free register of
-- the pool. When nothing acceptable is free, the acceptable interval that
-- reaches furthest goes to a frame slot; it is the one whose register would
-- sit idle the longest.
runAllocation :: Encoded -> Carriers -> Int -> Int -> Bool -> (UArray Int Int, UArray Int Bool)
runAllocation encoded carriers poolSize volatileCount preservedCost = runST $ do
  let valueCount = encValues encoded
      blockCount = encBlocks encoded
      positionCount = encPositions encoded
  (starts, ends) <- functionSpans encoded
  aihcCalls <- callsBefore encoded callAihc
  cCalls <- callsBefore encoded callC
  earns <- earnedRegisters encoded preservedCost
  -- The pool indexes the conventions suggest for each value.
  hintOffset <- newIntArray (valueCount + 1) 0
  hintCursor <- newIntArray (valueCount + 1) 0
  let forHints act = do
        forIndexed (encParameters encoded) (\position value -> act value (carrierArgument carriers position))
        forHintTable carriers (encInstructionHint encoded) act
        forHintTable carriers (encTerminatorHint encoded) act
  forHints (\value register -> when (register >= 0) (bump hintOffset (value + 1)))
  scanSums hintOffset valueCount
  copyInts hintOffset hintCursor (valueCount + 1)
  hintTotal <- readArray hintOffset valueCount
  hintValue <- newIntArray hintTotal 0
  forHints
    ( \value register ->
        when (register >= 0) $ do
          at <- readArray hintCursor value
          writeArray hintCursor value (at + 1)
          writeArray hintValue at register
    )
  hints <- freezeInts hintValue
  hintRange <- freezeInts hintOffset
  -- The values a jump copies to or from each other.
  partnerOffset <- newIntArray (valueCount + 1) 0
  partnerCursor <- newIntArray (valueCount + 1) 0
  let forPairs act = goBlock 0
        where
          goBlock index =
            when (index < blockCount) $ do
              forRange (encTargetOffset encoded) index $ \at -> do
                let successor = encTargetBlock encoded ! at
                    argumentFrom = encTargetArgumentOffset encoded ! at
                    parameterFrom = encBlockParameterOffset encoded ! successor
                    count =
                      min
                        (encTargetArgumentOffset encoded ! (at + 1) - argumentFrom)
                        (encBlockParameterOffset encoded ! (successor + 1) - parameterFrom)
                    go step =
                      when (step < count) $ do
                        let value = encTargetArgument encoded ! (argumentFrom + step)
                        when (value >= 0) (act value (encBlockParameter encoded ! (parameterFrom + step)))
                        go (step + 1)
                go 0
              goBlock (index + 1)
  forPairs (\value parameter -> bump partnerOffset (value + 1) >> bump partnerOffset (parameter + 1))
  scanSums partnerOffset valueCount
  copyInts partnerOffset partnerCursor (valueCount + 1)
  partnerTotal <- readArray partnerOffset valueCount
  partnerValue <- newIntArray partnerTotal 0
  let place value partner = do
        at <- readArray partnerCursor value
        writeArray partnerCursor value (at + 1)
        writeArray partnerValue at partner
  forPairs place
  forPairs (flip place)
  partners <- freezeInts partnerValue
  partnerRange <- freezeInts partnerOffset
  -- The scan visits the values in order of their start. A value with a hint
  -- of its own, or with a partner placed before it, has a claim on a
  -- register; it goes before the values defined at the same position that
  -- have none.
  leads <- newBoolArray valueCount False
  let noteLeads value =
        when (value < valueCount) $ do
          let earlier at =
                at < partnerRange ! (value + 1)
                  && (starts ! (partners ! at) < starts ! value || earlier (at + 1))
          writeArray
            leads
            value
            (hintRange ! (value + 1) > hintRange ! value || earlier (partnerRange ! value))
          noteLeads (value + 1)
  noteLeads 0
  leading <- freezeBools leads
  order <- orderByStart valueCount positionCount starts leading
  assigned <- newIntArray valueCount (-1)
  used <- newBoolArray poolSize False
  free <- newBoolArray poolSize True
  activeStart <- newIntArray (max 1 poolSize) 0
  activeEnd <- newIntArray (max 1 poolSize) 0
  activeValue <- newIntArray (max 1 poolSize) 0
  activeRegister <- newIntArray (max 1 poolSize) 0
  activeCount <- newSTRef (0 :: Int)
  let reachOf value =
        let start = starts ! value
            end = ends ! value
            crosses table = end > start + 1 && table ! end > table ! (start + 1)
         in if crosses aihcCalls
              then reachNone
              else if crosses cCalls then reachPreserved else reachAny
      -- Whether a candidate may live in the register at a pool index.
      accepts value reach register = do
        earned <- readArray earns value
        let preserved = register >= volatileCount
        pure $ case () of
          _
            | reach == reachNone -> False
            | reach == reachPreserved -> preserved && earned
            | otherwise -> not preserved || earned
      -- A register a value may take now: free, and acceptable.
      available value reach register
        | register < 0 = pure False
        | otherwise = do
            isFree <- readArray free register
            if isFree then accepts value reach register else pure False
      -- The active intervals that end before this one starts give their
      -- registers back. An interval that ends exactly where the next begins
      -- does so too: the value an instruction consumes hands its register to
      -- the value the instruction defines, which every instruction a backend
      -- selects has to tolerate. A value that never lived past its own
      -- definition keeps its register, so two values one instruction defines
      -- never share.
      expire position = do
        count <- readSTRef activeCount
        let countDone index
              | index >= count = pure index
              | otherwise = do
                  end <- readArray activeEnd index
                  start <- readArray activeStart index
                  if end < position || (end == position && start < end)
                    then do
                      register <- readArray activeRegister index
                      writeArray free register True
                      countDone (index + 1)
                    else pure index
        done <- countDone 0
        when (done > 0) $ do
          let shift index =
                when (index + done < count) $ do
                  moveActive (index + done) index
                  shift (index + 1)
          shift 0
          writeSTRef activeCount (count - done)
      moveActive from to = do
        readArray activeStart from >>= writeArray activeStart to
        readArray activeEnd from >>= writeArray activeEnd to
        readArray activeValue from >>= writeArray activeValue to
        readArray activeRegister from >>= writeArray activeRegister to
      -- The active list stays sorted by end; a new interval goes before the
      -- ones that end where it ends.
      activate value register = do
        count <- readSTRef activeCount
        let end = ends ! value
            place' index
              | index >= count = pure index
              | otherwise = do
                  other <- readArray activeEnd index
                  if end <= other then pure index else place' (index + 1)
        at <- place' 0
        let shift index = when (index > at) (moveActive (index - 1) index >> shift (index - 1))
        shift count
        writeArray activeStart at (starts ! value)
        writeArray activeEnd at end
        writeArray activeValue at value
        writeArray activeRegister at register
        writeSTRef activeCount (count + 1)
        writeArray free register False
        writeArray used register True
        writeArray assigned value register
      remove at = do
        count <- readSTRef activeCount
        let shift index = when (index + 1 < count) (moveActive (index + 1) index >> shift (index + 1))
        shift at
        writeSTRef activeCount (count - 1)
      firstHint value reach owner at
        | at >= hintRange ! (owner + 1) = pure (-1)
        | otherwise = do
            ok <- available value reach (hints ! at)
            if ok then pure (hints ! at) else firstHint value reach owner (at + 1)
      firstPartner value reach at
        | at >= partnerRange ! (value + 1) = pure (-1)
        | otherwise = do
            register <- readArray assigned (partners ! at)
            ok <- available value reach register
            if ok then pure register else firstPartner value reach (at + 1)
      firstWeakHint value reach at
        | at >= partnerRange ! (value + 1) = pure (-1)
        | otherwise = do
            register <- firstHint value reach (partners ! at) (hintRange ! (partners ! at))
            if register >= 0 then pure register else firstWeakHint value reach (at + 1)
      -- A result prefers the register of an operand of its own instruction.
      firstOperand value reach =
        let number = encDefiner encoded ! value
            go at
              | at >= encReadOffset encoded ! (number + 1) = pure (-1)
              | otherwise = do
                  register <- readArray assigned (encRead encoded ! at)
                  ok <- available value reach register
                  if ok then pure register else go (at + 1)
         in if number < 0 then pure (-1) else go (encReadOffset encoded ! number)
      firstFree value reach register
        | register >= poolSize = pure (-1)
        | otherwise = do
            ok <- available value reach register
            if ok then pure register else firstFree value reach (register + 1)
      preferred value reach =
        firstHint value reach value (hintRange ! value)
          `orElse` firstPartner value reach (partnerRange ! value)
          `orElse` firstWeakHint value reach (partnerRange ! value)
          `orElse` firstOperand value reach
          `orElse` firstFree value reach 0
      -- The furthest-reaching acceptable interval loses its register. The
      -- active list is sorted by end, so it is the last acceptable one.
      spill value reach = do
        count <- readSTRef activeCount
        let victimOf index
              | index < 0 = pure (-1)
              | otherwise = do
                  register <- readArray activeRegister index
                  ok <- accepts value reach register
                  if ok then pure index else victimOf (index - 1)
        at <- victimOf (count - 1)
        when (at >= 0) $ do
          end <- readArray activeEnd at
          when (end > ends ! value) $ do
            register <- readArray activeRegister at
            victim <- readArray activeValue at
            writeArray assigned victim (-1)
            remove at
            writeArray free register True
            activate value register
      step at =
        when (at < valueCount) $ do
          let value = order ! at
              reach = reachOf value
          expire (starts ! value)
          register <- preferred value reach
          if register >= 0 then activate value register else spill value reach
          step (at + 1)
  step 0
  (,) <$> freezeInts assigned <*> freezeBools used

-- | The first of two searches that finds a register.
orElse :: ST s Int -> ST s Int -> ST s Int
orElse first second = do
  register <- first
  if register >= 0 then pure register else second

-- | The number of calls of one convention before each position.
callsBefore :: Encoded -> Int -> ST s (UArray Int Int)
callsBefore encoded convention = do
  counts <- newIntArray (encPositions encoded + 2) 0
  let count number =
        when (number < encInstructions encoded) $ do
          when
            (encInstructionCall encoded ! number == convention)
            (bump counts (encInstructionPosition encoded ! number + 1))
          count (number + 1)
  count 0
  scanSums counts (encPositions encoded + 1)
  freezeInts counts

-- | Whether a value earns the register it would take.
--
-- A value in a frame slot costs one memory access per definition and per use.
-- A value in a register costs none of those, and instead the prologue saves
-- the register once and every exit restores it. So the register pays for
-- itself once the value is touched more often than the function has exits
-- plus the one save.
--
-- Several values that share a register pay the save and the restores once
-- between them, so a value that clears the bar alone is never a loss and a
-- register that several values share is a gain beyond what the bar counts.
earnedRegisters :: Encoded -> Bool -> ST s (STUArray s Int Bool)
earnedRegisters encoded preservedCost = do
  earns <- newBoolArray (encValues encoded) True
  when preservedCost $ do
    counts <- accessCounts encoded
    -- The number of exits: the terminators that restore the saved registers.
    let countExits index total
          | index >= encBlocks encoded = pure total
          | otherwise = countExits (index + 1) (if encBlockExit encoded ! index then total + 1 else total)
    exits <- countExits 0 0
    let note value =
          when (value < encValues encoded) $ do
            count <- readArray counts value
            writeArray earns value (count > 1 + exits)
            note (value + 1)
    note 0
  pure earns

-- | The values in the order the scan visits them: by the start of the
-- interval, then the values that lead, then the number of the value.
orderByStart :: Int -> Int -> UArray Int Int -> UArray Int Bool -> ST s (UArray Int Int)
orderByStart valueCount positionCount starts leading = do
  leadCursor <- newIntArray (positionCount + 1) 0
  restCursor <- newIntArray (positionCount + 1) 0
  let cursorFor value = if leading ! value then leadCursor else restCursor
      count value =
        when (value < valueCount) $ do
          bump (cursorFor value) (starts ! value)
          count (value + 1)
  count 0
  let sums position total =
        when (position <= positionCount) $ do
          leaders <- readArray leadCursor position
          rest <- readArray restCursor position
          writeArray leadCursor position total
          writeArray restCursor position (total + leaders)
          sums (position + 1) (total + leaders + rest)
  sums 0 0
  order <- newIntArray valueCount 0
  let place value =
        when (value < valueCount) $ do
          let cursor = cursorFor value
          at <- readArray cursor (starts ! value)
          writeArray cursor (starts ! value) (at + 1)
          writeArray order at value
          place (value + 1)
  place 0
  freezeInts order

-- | How often the function touches each value, weighted by the loops that
-- enclose the touch: once where it defines it, and once for every place it
-- reads it. A value read twice by one instruction counts twice, because
-- instruction selection reads it twice.
accessCounts :: Encoded -> ST s (STUArray s Int Int)
accessCounts encoded = do
  depths <- loopDepths encoded
  counts <- newIntArray (encValues encoded) 0
  let add weight value = readArray counts value >>= writeArray counts value . (+ weight)
      goBlock index =
        when (index < encBlocks encoded) $ do
          depth <- readArray depths index
          -- A touch inside a loop happens once for every turn of the loop, so
          -- it counts for more. The weight is a power of ten per loop that
          -- encloses the block, which is the usual guess in the absence of a
          -- profile, and it is capped so that a deep nest cannot overflow the
          -- count.
          let weight = 10 ^ min 3 depth
          forRange (encBlockParameterOffset encoded) index (\at -> add weight (encBlockParameter encoded ! at))
          let goInstruction number =
                when (number < encBlockInstructionOffset encoded ! (index + 1)) $ do
                  forRange (encResultOffset encoded) number (\at -> add weight (encResult encoded ! at))
                  forRange (encReadOffset encoded) number (\at -> add weight (encRead encoded ! at))
                  goInstruction (number + 1)
          goInstruction (encBlockInstructionOffset encoded ! index)
          forRange (encTerminatorReadOffset encoded) index (\at -> add weight (encTerminatorRead encoded ! at))
          goBlock (index + 1)
  goBlock 0
  -- A parameter arrives before the first block.
  forAll (encParameters encoded) (add 1)
  pure counts

-- | How many loops enclose each block. A loop is a back edge and the blocks
-- that reach it without leaving through its header, which is the natural loop
-- of the edge.
loopDepths :: Encoded -> ST s (STUArray s Int Int)
loopDepths encoded = do
  let blockCount = encBlocks encoded
      targetCount = encTargetOffset encoded ! blockCount
  depths <- newIntArray blockCount 0
  predecessorOffset <- newIntArray (blockCount + 1) 0
  predecessorCursor <- newIntArray (blockCount + 1) 0
  let forEdges act = goBlock 0
        where
          goBlock index =
            when (index < blockCount) $ do
              forRange (encTargetOffset encoded) index (\at -> act index (encTargetBlock encoded ! at))
              goBlock (index + 1)
  forEdges (\_ successor -> bump predecessorOffset (successor + 1))
  scanSums predecessorOffset blockCount
  copyInts predecessorOffset predecessorCursor (blockCount + 1)
  predecessorValue <- newIntArray targetCount 0
  forEdges
    ( \index successor -> do
        at <- readArray predecessorCursor successor
        writeArray predecessorCursor successor (at + 1)
        writeArray predecessorValue at index
    )
  predecessors <- freezeInts predecessorValue
  predecessorRange <- freezeInts predecessorOffset
  done <- newBoolArray blockCount False
  onPath <- newBoolArray blockCount False
  stamps <- newIntArray blockCount (-1)
  generation <- newSTRef (0 :: Int)
  worklist <- newIntBuffer 8
  let deepen index = readArray depths index >>= writeArray depths index . (+ 1)
      -- The blocks of the natural loop of a back edge: its header, its
      -- source, and everything that reaches the source without passing the
      -- header.
      naturalLoop header from = do
        stamp <- readSTRef generation
        writeSTRef generation (stamp + 1)
        writeArray stamps header stamp
        deepen header
        unless (from == header) $ do
          writeArray stamps from stamp
          deepen from
        pushInt worklist from
        let grow at = do
              count <- bufferLength worklist
              when (at < count) $ do
                index <- bufferAt worklist at
                unless (index == header) $
                  forRange predecessorRange index $ \edge -> do
                    let source = predecessors ! edge
                    seen <- readArray stamps source
                    unless (seen == stamp) $ do
                      writeArray stamps source stamp
                      deepen source
                      pushInt worklist source
                grow (at + 1)
        grow 0
        clearBuffer worklist
      -- The edges that close a loop: an edge whose target is already on the
      -- path the search took to reach its source.
      visit index = do
        seen <- readArray done index
        unless seen $ do
          writeArray done index True
          writeArray onPath index True
          forRange (encTargetOffset encoded) index $ \at -> do
            let successor = encTargetBlock encoded ! at
            back <- readArray onPath successor
            if back then naturalLoop successor index else visit successor
          writeArray onPath index False
  when (blockCount > 0) (visit 0)
  pure depths

-- Arrays and bits

-- | Run an action on every element of a row that an offset array describes.
forRange :: UArray Int Int -> Int -> (Int -> ST s ()) -> ST s ()
{-# INLINE forRange #-}
forRange offsets index act = go (offsets ! index)
  where
    limit = offsets ! (index + 1)
    go at
      | at >= limit = pure ()
      | otherwise = act at >> go (at + 1)

-- | Run an action on every element of an array.
forAll :: UArray Int Int -> (Int -> ST s ()) -> ST s ()
{-# INLINE forAll #-}
forAll array act = go 0
  where
    limit = lengthOf array
    go at
      | at >= limit = pure ()
      | otherwise = act (array ! at) >> go (at + 1)

-- | The same, with the position of each element.
forIndexed :: UArray Int Int -> (Int -> Int -> ST s ()) -> ST s ()
{-# INLINE forIndexed #-}
forIndexed array act = go 0
  where
    limit = lengthOf array
    go at
      | at >= limit = pure ()
      | otherwise = act at (array ! at) >> go (at + 1)

{-# INLINE bump #-}
bump :: STUArray s Int Int -> Int -> ST s ()
bump array index = readArray array index >>= writeArray array index . (+ 1)

-- | Turn counts into offsets, in place.
{-# INLINE scanSums #-}
scanSums :: STUArray s Int Int -> Int -> ST s ()
scanSums array count = go 1
  where
    go index =
      when (index <= count) $ do
        previous <- readArray array (index - 1)
        here <- readArray array index
        writeArray array index (previous + here)
        go (index + 1)

{-# INLINE setWord #-}
setWord :: STUArray s Int Word64 -> Int -> ST s ()
setWord bits value = do
  let (word, bit) = value `quotRem` 64
  current <- readArray bits word
  writeArray bits word (current .|. (1 `shiftL` bit))

{-# INLINE clearWord #-}
clearWord :: STUArray s Int Word64 -> Int -> ST s ()
clearWord bits value = do
  let (word, bit) = value `quotRem` 64
  current <- readArray bits word
  writeArray bits word (current .&. complement (1 `shiftL` bit))

{-# INLINE clearWords #-}
clearWords :: STUArray s Int Word64 -> Int -> Int -> ST s ()
clearWords bits base stride = go 0
  where
    go index = when (index < stride) (writeArray bits (base + index) 0 >> go (index + 1))

{-# INLINE copyWords #-}
copyWords :: STUArray s Int Word64 -> Int -> STUArray s Int Word64 -> Int -> Int -> ST s ()
copyWords from fromBase to toBase stride = go 0
  where
    go index =
      when (index < stride) $ do
        word <- readArray from (fromBase + index)
        writeArray to (toBase + index) word
        go (index + 1)

{-# INLINE unionWords #-}
unionWords :: STUArray s Int Word64 -> Int -> STUArray s Int Word64 -> Int -> Int -> ST s ()
unionWords into base from fromBase stride = go 0
  where
    go index =
      when (index < stride) $ do
        here <- readArray into (base + index)
        there <- readArray from (fromBase + index)
        writeArray into (base + index) (here .|. there)
        go (index + 1)

{-# INLINE differentWords #-}
differentWords :: STUArray s Int Word64 -> Int -> STUArray s Int Word64 -> Int -> Int -> ST s Bool
differentWords left leftBase right rightBase stride = go 0
  where
    go index
      | index >= stride = pure False
      | otherwise = do
          here <- readArray left (leftBase + index)
          there <- readArray right (rightBase + index)
          if here /= there then pure True else go (index + 1)

-- | Run an action on every value one row of bits holds, lowest first.
{-# INLINE forWords #-}
forWords :: STUArray s Int Word64 -> Int -> Int -> (Int -> ST s ()) -> ST s ()
forWords bits base stride act = go 0
  where
    go index =
      when (index < stride) $ do
        word <- readArray bits (base + index)
        bitsOf (index * 64) word
        go (index + 1)
    bitsOf shift word
      | word == 0 = pure ()
      | otherwise = do
          act (shift + countTrailingZeros word)
          bitsOf shift (word .&. (word - 1))

-- Uses

-- | How many times the function reads each value, unweighted.
readCounts :: Function -> Map Var Int
readCounts function =
  Map.fromListWith
    (+)
    [ (var, 1)
    | block <- functionBlocks function,
      var <- concatMap (operationReads . instructionOperation) (blockInstructions block) <> terminatorReads (blockTerminator block)
    ]

-- | Every read of a value by one operation, in order and with repeats.
operationReads :: Operation -> [Var]
operationReads = getConst . forOperationOperands (Const . operandVar)

-- | Every read of a value by one terminator, in order and with repeats.
terminatorReads :: Terminator -> [Var]
terminatorReads = getConst . forTerminatorOperands (Const . operandVar)

operandVar :: Operand -> [Var]
operandVar operand =
  case operand of
    OperandVar var -> [var]
    OperandLiteral _ -> []
