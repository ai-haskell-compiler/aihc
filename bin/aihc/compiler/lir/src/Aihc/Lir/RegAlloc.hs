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
-- Memory is the constraint the design obeys, because the compiler runs the
-- allocator on every function of every module. A pass that builds one small
-- container for each instruction costs more than the scan itself. So
-- 'encodeFunction' numbers the values and the blocks and flattens the
-- function into unboxed arrays in one walk. After that no pass holds a list
-- or a persistent map: a set of values is a row of bits, an interval is two
-- positions in an array, and the scan keeps its state in mutable arrays.
-- The names come back only in the result.
module Aihc.Lir.RegAlloc
  ( Allocation (..),
    Registers (..),
    allocateRegistersFor,
    Interval (..),
    functionIntervals,
    readCounts,
  )
where

import Aihc.Lir.Flat
import Aihc.Lir.Syntax
import Control.Applicative (Const (..))
import Control.Monad (when, (>=>))
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, readArray, writeArray)
import Data.Array.Unboxed (Array, UArray, elems, listArray, (!))
import Data.Array.Unsafe (unsafeFreeze)
import Data.Foldable (traverse_)
import Data.List (elemIndex)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.STRef (newSTRef, readSTRef, writeSTRef)

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
          [ (encNames encoded ! value, registers ! register)
          | value <- [0 .. encValues encoded - 1],
            let register = assigned ! value,
            register >= 0
          ],
      allocationSpills = [encNames encoded ! value | value <- elems (encDefinitions encoded), assigned ! value < 0],
      allocationUsed = [register | (index, register) <- zip [0 ..] pool, used ! index]
    }
  where
    pool = registersVolatile target <> registersPreserved target
    registers = boxedArray (length pool) pool
    -- The pool is small, so a register finds its index by a walk, and a
    -- table then holds the answer for the positions a convention uses.
    carrier which = memoise (maybe (-1) indexOf . which target)
    indexOf register = fromMaybe (-1) (elemIndex register pool)
    encoded = encodeFunction (carrier registersArgument) (carrier registersResult) signatures function
    (assigned, used) =
      runAllocation encoded (length pool) (length (registersVolatile target)) (registersPreservedCost target)

-- | The live interval of every value of the function, in name order.
functionIntervals :: Function -> [Interval]
functionIntervals function =
  [ Interval {intervalVar = encNames encoded ! value, intervalStart = starts ! value, intervalEnd = ends ! value}
  | value <- [0 .. encValues encoded - 1]
  ]
  where
    encoded = encodeFunction unhinted unhinted Map.empty function
    unhinted _ = -1
    (starts, ends) = runST (functionSpans encoded)

-- | The answer of a function on the first positions, in a table.
memoise :: (Int -> Int) -> Int -> Int
memoise carrier =
  let table = listArray (0, limit) (map carrier [0 .. limit]) :: UArray Int Int
   in \position -> if position <= limit then table ! position else carrier position
  where
    limit = 15

-- Encoding

-- | A function with its values, blocks, instructions, and positions
-- numbered, and every list of the function flattened into an array.
data Encoded = Encoded
  { -- | Every value of the function by its rank among the names, so that the
    -- number order is the name order.
    encNames :: !(Array Int Var),
    encValues :: !Int,
    -- | One position more than the highest position of the function.
    encPositions :: !Int,
    encBlocks :: !Int,
    -- | How many terminators restore the saved registers: a return or a tail
    -- call. A trap does not return, so it restores nothing.
    encExits :: !Int,
    -- | The parameters of the function, in order.
    encParameters :: !(UArray Int Int),
    -- | Every value the function defines, in the order the text defines it.
    encDefinitions :: !(UArray Int Int),
    -- | Every position is distinct and the positions of a block are a
    -- contiguous run, so the whole block sits between its start and its end.
    -- A block ends one position after its terminator.
    encBlockStart :: !(UArray Int Int),
    encBlockTerminator :: !(UArray Int Int),
    -- | The instructions of block @i@ are the numbers from
    -- @encBlockInstructions ! i@ up to @encBlockInstructions ! (i + 1)@.
    encBlockInstructions :: !(UArray Int Int),
    -- | The convention of the callee of a call: 'callNone', 'callAihc', or
    -- 'callC'.
    encInstructionCall :: !(UArray Int Int),
    -- | One row for each block.
    encBlockParameters :: !Rows,
    encTerminatorReads :: !Rows,
    encTargets :: !Rows,
    -- | One row for each instruction. A read appears in order and with
    -- repeats.
    encResults :: !Rows,
    encReads :: !Rows,
    -- | One row for each target: the value each jump argument carries, or -1
    -- for a literal.
    encTargetArguments :: !Rows,
    -- | The instruction that defines each value, or -1 for a parameter.
    encDefiner :: !(UArray Int Int),
    -- | The register a convention suggests for a value, as pairs of a value
    -- and a pool index. The parameters and the calls come first, and the
    -- terminators follow.
    encCallHints :: !(UArray Int Int),
    encExitHints :: !(UArray Int Int)
  }

callNone, callAihc, callC :: Int
callNone = 0
callAihc = 1
callC = 2

-- | Run an action on every block, and on every instruction of one block.
forBlocks :: Encoded -> (Int -> ST s ()) -> ST s ()
forBlocks encoded = forUpTo (encBlocks encoded)

-- | The action receives the number of the instruction and its position.
{-# INLINE forBlockInstructions #-}
forBlockInstructions :: Encoded -> Int -> (Int -> Int -> ST s ()) -> ST s ()
forBlockInstructions encoded index act = go (encBlockInstructions encoded ! index) (encBlockStart encoded ! index + 1)
  where
    go number position =
      when
        (number < encBlockInstructions encoded ! (index + 1))
        (act number position >> go (number + 1) (position + 1))

-- | Run an action on every hint: a value, and the pool index the convention
-- suggests for it.
forHints :: Encoded -> (Int -> Int -> ST s ()) -> ST s ()
forHints encoded act = pairs (encCallHints encoded) >> pairs (encExitHints encoded)
  where
    pairs table = forUpTo (lengthOf table `div` 2) (\at -> act (table ! (2 * at)) (table ! (2 * at + 1)))

-- | Run an action on every value a jump copies, with the block parameter it
-- reaches.
forJumpPairs :: Encoded -> (Int -> Int -> ST s ()) -> ST s ()
forJumpPairs encoded act =
  forBlocks encoded $ \index ->
    forUpTo (rowTo targets index - rowFrom targets index) $ \step -> do
      let at = rowFrom targets index + step
          successor = rowAt targets at
          count = min (rowTo arguments at - rowFrom arguments at) (rowTo parameters successor - rowFrom parameters successor)
      forUpTo count $ \position -> do
        let value = rowAt arguments (rowFrom arguments at + position)
        when (value >= 0) (act value (rowAt parameters (rowFrom parameters successor + position)))
  where
    targets = encTargets encoded
    arguments = encTargetArguments encoded
    parameters = encBlockParameters encoded

-- | Number the function and flatten it, in one walk of the blocks. The
-- carriers give the pool index the convention puts an argument or a result
-- in, or -1 when it names no register.
encodeFunction :: (Int -> Int) -> (Int -> Int) -> Map Symbol Signature -> Function -> Encoded
encodeFunction argumentCarrier resultCarrier signatures function = runST $ do
  let blocks = functionBlocks function
      blockCount = length blocks
      instructionCount = foldl' (\total block -> total + length (blockInstructions block)) 0 blocks
      parameterCount = length (functionParameters function)
      labels = Map.fromList (zip (map blockLabel blocks) [0 :: Int ..])
  identifiers <- newSTRef (Map.empty :: Map Var Int)
  nextIdentifier <- newInts 1 0
  exitCount <- newInts 1 0
  blockStart <- newInts blockCount 0
  terminatorPositions <- newInts blockCount 0
  blockRanges <- newInts (blockCount + 1) 0
  conventions <- newInts instructionCount callNone
  parameters <- newInts parameterCount 0
  parameterTable <- newTable blockCount 4
  terminatorReadTable <- newTable blockCount 8
  targetTable <- newTable blockCount 4
  resultTable <- newTable instructionCount (instructionCount + 4)
  readTable <- newTable instructionCount (2 * instructionCount + 4)
  argumentTable <- newTable 4 4
  callHints <- newIntBuffer 4
  exitHints <- newIntBuffer 4
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
      pushRead table operand = do
        value <- internOperand operand
        when (value >= 0) (pushValue table value)
      pushHint buffer carrier position var = intern var >>= pushHintValue buffer carrier position
      hintOperands buffer carrier operands =
        forEach operands $ \position operand ->
          case operand of
            OperandVar var -> pushHint buffer carrier position var
            OperandLiteral _ -> pure ()
      goInstruction first offset (Instruction results operation) = do
        let number = first + offset
        openRow resultTable
        traverse_ (intern >=> pushValue resultTable) results
        openRow readTable
        forOperationOperands (pushRead readTable) operation
        let callee arguments convention = do
              writeArray conventions number convention
              hintOperands callHints argumentCarrier arguments
              forEach results (pushHint callHints resultCarrier)
        case operation of
          Call symbol arguments -> callee arguments (conventionOf (maybe AihcConvention signatureConvention (Map.lookup symbol signatures)))
          CallIndirect _ arguments signature -> callee arguments (conventionOf (signatureConvention signature))
          _ -> pure ()
      goBlock index start block = do
        writeArray blockStart index start
        openRow parameterTable
        traverse_ (\(var, _) -> intern var >>= pushValue parameterTable) (blockParameters block)
        first <- readArray blockRanges index
        forEach (blockInstructions block) (goInstruction first)
        writeArray blockRanges (index + 1) (first + length (blockInstructions block))
        let terminator = blockTerminator block
            terminatorPosition = start + 1 + length (blockInstructions block)
        writeArray terminatorPositions index terminatorPosition
        openRow terminatorReadTable
        forTerminatorOperands (pushRead terminatorReadTable) terminator
        openRow targetTable
        traverse_
          ( \jump -> do
              pushValue targetTable (labels Map.! targetLabel jump)
              openRow argumentTable
              traverse_ (internOperand >=> pushValue argumentTable) (targetArguments jump)
          )
          (terminatorTargets terminator)
        when (restoresRegisters terminator) (addTo exitCount 0 1)
        case terminator of
          TailCall _ arguments -> hintOperands exitHints argumentCarrier arguments
          TailCallIndirect _ arguments _ -> hintOperands exitHints argumentCarrier arguments
          Return values -> hintOperands exitHints resultCarrier values
          _ -> pure ()
        pure (terminatorPosition + 2)
      goBlocks _ start [] = pure start
      goBlocks index start (block : rest) = goBlock index start block >>= \next -> goBlocks (index + 1) next rest
  forEach (functionParameters function) $ \position (var, _) -> do
    value <- intern var
    writeArray parameters position value
    pushHintValue callHints argumentCarrier position value
  _ <- goBlocks 0 1 blocks
  -- The values take their rank in name order.
  known <- readSTRef identifiers
  let valueCount = Map.size known
  rankArray <- newInts valueCount 0
  forEach (Map.elems known) (flip (writeArray rankArray))
  ranks <- freezeInts rankArray
  let rename value = if value < 0 then value else ranks ! value
      renamePairs at value = if even at then rename value else value
  forUpTo parameterCount (\at -> readArray parameters at >>= writeArray parameters at . rename)
  blockParameterRows <- freezeTableWith rename parameterTable
  terminatorReadRows <- freezeTableWith rename terminatorReadTable
  resultRows <- freezeTableWith rename resultTable
  readRows <- freezeTableWith rename readTable
  argumentRows <- freezeTableWith rename argumentTable
  targetRows <- freezeTableWith id targetTable
  callHintPairs <- freezeBufferWith renamePairs callHints
  exitHintPairs <- freezeBufferWith renamePairs exitHints
  blockRangesFrozen <- freezeInts blockRanges
  -- The values the function defines, in the order the text defines them,
  -- and the instruction that defines each one.
  definitions <- newInts valueCount 0
  definitionCount <- newInts 1 0
  definer <- newInts valueCount (-1)
  let take' value = readArray definitionCount 0 >>= \at -> writeArray definitions at value >> writeArray definitionCount 0 (at + 1)
  forUpTo parameterCount (readArray parameters >=> take')
  forUpTo blockCount $ \index -> do
    forRow blockParameterRows index take'
    forUpTo (blockRangesFrozen ! (index + 1) - blockRangesFrozen ! index) $ \offset ->
      forRow resultRows (blockRangesFrozen ! index + offset) $ \value -> do
        take' value
        writeArray definer value (blockRangesFrozen ! index + offset)
  lastTerminator <- if blockCount == 0 then pure (-1) else readArray terminatorPositions (blockCount - 1)
  Encoded (listArray (0, valueCount - 1) (Map.keys known)) valueCount (lastTerminator + 2) blockCount
    <$> readArray exitCount 0
    <*> freezeInts parameters
    <*> freezeInts definitions
    <*> freezeInts blockStart
    <*> freezeInts terminatorPositions
    <*> pure blockRangesFrozen
    <*> freezeInts conventions
    <*> pure blockParameterRows
    <*> pure terminatorReadRows
    <*> pure targetRows
    <*> pure resultRows
    <*> pure readRows
    <*> pure argumentRows
    <*> freezeInts definer
    <*> pure callHintPairs
    <*> pure exitHintPairs
  where
    conventionOf callee =
      case callee of
        AihcConvention -> callAihc
        CConvention -> callC
    restoresRegisters terminator =
      case terminator of
        Return _ -> True
        TailCall _ _ -> True
        TailCallIndirect {} -> True
        _ -> False

-- | Remember that the convention suggests a register for a value.
pushHintValue :: IntBuffer s -> (Int -> Int) -> Int -> Int -> ST s ()
pushHintValue buffer carrier position value =
  when (carrier position >= 0) (pushInt buffer value >> pushInt buffer (carrier position))

-- | Run an action on every operand one operation reads, in order and with
-- repeats.
{-# INLINE forOperationOperands #-}
forOperationOperands :: (Applicative f) => (Operand -> f ()) -> Operation -> f ()
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

-- Intervals

-- | The lowest and the highest position at which each value is live.
--
-- A value is relevant at its definition, at each of its uses, at the start of
-- every block it is live into, and at the end of every block it is live out
-- of. The interval spans the lowest to the highest of those positions, which
-- covers every point at which the value is live whatever the block order.
functionSpans :: Encoded -> ST s (UArray Int Int, UArray Int Int)
functionSpans encoded = do
  starts <- newInts (encValues encoded) maxBound
  ends <- newInts (encValues encoded) minBound
  let touch position value = do
        start <- readArray starts value
        when (position < start) (writeArray starts value position)
        end <- readArray ends value
        when (position > end) (writeArray ends value position)
  -- A parameter is defined before the first block.
  forUpTo (lengthOf (encParameters encoded)) (touch 0 . (encParameters encoded !))
  live <- liveness encoded
  forBlocks encoded $ \index -> do
    let start = encBlockStart encoded ! index
    forRow (encBlockParameters encoded) index (touch start)
    forBlockInstructions encoded index $ \number position -> do
      forRow (encResults encoded) number (touch position)
      forRow (encReads encoded) number (touch position)
    forRow (encTerminatorReads encoded) index (touch (encBlockTerminator encoded ! index))
    case live of
      Nothing -> pure ()
      Just (liveIn, liveOut) -> do
        forBits liveIn index (touch start)
        forBits liveOut index (touch (encBlockTerminator encoded ! index + 1))
  (,) <$> freezeInts starts <*> freezeInts ends

-- | The values that are live at the start and at the end of each block.
--
-- A jump argument is a read of the block that jumps, and a block parameter
-- is a write of the block that receives it, made before anything in the
-- block reads it.
liveness :: Encoded -> ST s (Maybe (Bits s, Bits s))
liveness encoded
  -- A function whose blocks jump nowhere has nothing live across a block
  -- boundary: a block parameter is a definition, and a use of something a
  -- block does not define is a parameter, which is defined before the first
  -- block.
  | rowsSize (encTargets encoded) == 0 = pure Nothing
  | otherwise = do
      let blockCount = encBlocks encoded
      liveIn <- newBits blockCount (encValues encoded)
      liveOut <- newBits blockCount (encValues encoded)
      scratch <- newBits 1 (encValues encoded)
      -- The values a block reads before it writes them.
      upward <- newTable blockCount 8
      forBlocks encoded $ \index -> do
        clearRow scratch 0
        forRow (encTerminatorReads encoded) index (setBit scratch 0)
        forDownFrom (encBlockInstructions encoded ! (index + 1) - 1) (encBlockInstructions encoded ! index) $ \number -> do
          forRow (encResults encoded) number (clearBit scratch 0)
          forRow (encReads encoded) number (setBit scratch 0)
        forRow (encBlockParameters encoded) index (clearBit scratch 0)
        openRow upward
        forBits scratch 0 (pushValue upward)
      upwardRows <- freezeTableWith id upward
      let update index = do
            clearRow liveOut index
            forRow (encTargets encoded) index (unionRow liveOut index liveIn)
            _ <- replaceRow scratch 0 liveOut index
            forRow (encBlockParameters encoded) index (clearBit scratch 0)
            forBlockInstructions encoded index (\number _ -> forRow (encResults encoded) number (clearBit scratch 0))
            forRow upwardRows index (setBit scratch 0)
            replaceRow liveIn index scratch 0
          sweep index changed
            | index < 0 = pure changed
            | otherwise = update index >>= \here -> sweep (index - 1) (changed || here)
          converge = sweep (blockCount - 1) False >>= \changed -> when changed converge
      converge
      pure (Just (liveIn, liveOut))

-- Allocation

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
runAllocation :: Encoded -> Int -> Int -> Bool -> (UArray Int Int, UArray Int Bool)
runAllocation encoded poolSize volatileCount preservedCost = runST $ do
  let valueCount = encValues encoded
  (starts, ends) <- functionSpans encoded
  aihcCalls <- callsBefore encoded callAihc
  cCalls <- callsBefore encoded callC
  earns <- earnedRegisters encoded preservedCost
  hints <- buildRows valueCount (forHints encoded)
  partners <- buildRows valueCount (\act -> forJumpPairs encoded act >> forJumpPairs encoded (flip act))
  order <- orderByStart encoded starts hints partners
  assigned <- newInts valueCount (-1)
  used <- newBools poolSize False
  -- The value in each register, or -1, and the registers in use, in the
  -- order their intervals end.
  holder <- newInts poolSize (-1)
  active <- newInts poolSize 0
  activeCount <- newInts 1 0
  let reachOf value
        | crosses aihcCalls (starts ! value) (ends ! value) = reachNone
        | crosses cCalls (starts ! value) (ends ! value) = reachPreserved
        | otherwise = reachAny
      -- Whether a value may live in the register at a pool index.
      {-# INLINE accepts #-}
      accepts value register = do
        earned <- readArray earns value
        let preserved = register >= volatileCount
        pure $ case reachOf value of
          reach
            | reach == reachNone -> False
            | reach == reachPreserved -> preserved && earned
            | otherwise -> not preserved || earned
      -- The same, for a register that must also be free.
      {-# INLINE usable #-}
      usable value register
        | register < 0 = pure False
        | otherwise = do
            taken <- readArray holder register
            if taken >= 0 then pure False else accepts value register
      valueIn index = readArray active index >>= readArray holder
      -- The active intervals that end before this one starts give their
      -- registers back. An interval that ends exactly where the next begins
      -- does so too: the value an instruction consumes hands its register to
      -- the value the instruction defines, which every instruction a backend
      -- selects has to tolerate. A value that never lived past its own
      -- definition keeps its register, so two values one instruction defines
      -- never share.
      expire position = do
        count <- readArray activeCount 0
        let finished index
              | index >= count = pure index
              | otherwise = do
                  value <- valueIn index
                  if ends ! value < position || (ends ! value == position && starts ! value < ends ! value)
                    then readArray active index >>= \register -> writeArray holder register (-1) >> finished (index + 1)
                    else pure index
        done <- finished 0
        when (done > 0) (dropActive 0 done count)
      -- Take a run of registers out of the active list, and close the gap.
      dropActive at gone count = do
        forUpTo (count - at - gone) (\step -> readArray active (at + gone + step) >>= writeArray active (at + step))
        writeArray activeCount 0 (count - gone)
      -- The active list stays sorted by end; a new interval goes before the
      -- ones that end where it ends.
      activate value register = do
        count <- readArray activeCount 0
        let place index
              | index >= count = pure index
              | otherwise = valueIn index >>= \other -> if ends ! value <= ends ! other then pure index else place (index + 1)
        at <- place 0
        forDownFrom count (at + 1) (\index -> readArray active (index - 1) >>= writeArray active index)
        writeArray active at register
        writeArray activeCount 0 (count + 1)
        writeArray holder register value
        writeArray used register True
        writeArray assigned value register
      -- The first register of a row that the value may take now. The row
      -- gives a register through @pick@, which is the identity for a hint of
      -- the value and the assignment for a partner or an operand.
      {-# INLINE searchRow #-}
      searchRow rows index pick value
        | index < 0 = pure (-1)
        | otherwise = go (rowFrom rows index)
        where
          go at
            | at >= rowTo rows index = pure (-1)
            | otherwise = do
                register <- pick (rowAt rows at)
                ok <- usable value register
                if ok then pure register else go (at + 1)
      firstFree value register
        | register >= poolSize = pure (-1)
        | otherwise = usable value register >>= \ok -> if ok then pure register else firstFree value (register + 1)
      preferred value =
        searchRow hints value pure value
          `orElse` searchRow partners value (readArray assigned) value
          `orElse` searchRow partners value (\partner -> searchRow hints partner pure value) value
          `orElse` searchRow (encReads encoded) (encDefiner encoded ! value) (readArray assigned) value
          `orElse` firstFree value 0
      -- The furthest-reaching acceptable interval loses its register. The
      -- active list is sorted by end, so it is the last acceptable one.
      spill value = do
        count <- readArray activeCount 0
        let victim index
              | index < 0 = pure (-1)
              | otherwise = do
                  register <- readArray active index
                  accepts value register >>= \ok -> if ok then pure index else victim (index - 1)
        at <- victim (count - 1)
        when (at >= 0) $ do
          register <- readArray active at
          loser <- readArray holder register
          when (ends ! loser > ends ! value) $ do
            writeArray assigned loser (-1)
            writeArray holder register (-1)
            dropActive at 1 count
            activate value register
  forUpTo valueCount $ \at -> do
    let value = order ! at
    expire (starts ! value)
    register <- preferred value
    if register >= 0 then activate value register else spill value
  (,) <$> freezeInts assigned <*> unsafeFreeze used

-- | Whether a call sits inside an interval. A call at the start of the
-- interval defines it and a call at its end consumes it.
{-# INLINE crosses #-}
crosses :: UArray Int Int -> Int -> Int -> Bool
crosses calls start end = end > start + 1 && calls ! end > calls ! (start + 1)

-- | The first of two searches that finds a register.
{-# INLINE orElse #-}
orElse :: ST s Int -> ST s Int -> ST s Int
orElse first second = first >>= \register -> if register >= 0 then pure register else second

-- | How many calls of one convention come before each position.
callsBefore :: Encoded -> Int -> ST s (UArray Int Int)
callsBefore encoded convention = do
  counts <- newInts (encPositions encoded + 2) 0
  forBlocks encoded $ \index ->
    forBlockInstructions encoded index $ \number position ->
      when (encInstructionCall encoded ! number == convention) (addTo counts (position + 1) 1)
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
  earns <- newBools (encValues encoded) True
  when preservedCost $ do
    counts <- accessCounts encoded
    forUpTo (encValues encoded) $ \value -> do
      count <- readArray counts value
      writeArray earns value (count > 1 + encExits encoded)
  pure earns

-- | How often the function touches each value, weighted by the loops that
-- enclose the touch: once where it defines it, and once for every place it
-- reads it. A value read twice by one instruction counts twice, because
-- instruction selection reads it twice.
accessCounts :: Encoded -> ST s (STUArray s Int Int)
accessCounts encoded = do
  depths <- loopDepths encoded
  counts <- newInts (encValues encoded) 0
  forBlocks encoded $ \index -> do
    depth <- readArray depths index
    -- A touch inside a loop happens once for every turn of the loop, so it
    -- counts for more. The weight is a power of ten per loop that encloses
    -- the block, which is the usual guess in the absence of a profile, and
    -- it is capped so that a deep nest cannot overflow the count.
    let add value = addTo counts value (10 ^ min 3 depth)
    forRow (encBlockParameters encoded) index add
    forBlockInstructions encoded index $ \number _ -> do
      forRow (encResults encoded) number add
      forRow (encReads encoded) number add
    forRow (encTerminatorReads encoded) index add
  -- A parameter arrives before the first block.
  forUpTo (lengthOf (encParameters encoded)) (\at -> addTo counts (encParameters encoded ! at) 1)
  pure counts

-- | How many loops enclose each block, as a guess from the block order. An
-- edge whose target does not come after its source closes a loop, and the
-- blocks between the two are inside that loop. A loop that the block order
-- breaks apart counts for less, which costs a little code quality and no
-- correctness: the depth only weights the count of touches.
loopDepths :: Encoded -> ST s (STUArray s Int Int)
loopDepths encoded = do
  let blockCount = encBlocks encoded
  edges <- newInts (blockCount + 1) 0
  forBlocks encoded $ \index ->
    forRow (encTargets encoded) index $ \successor ->
      when (successor <= index) (addTo edges successor 1 >> addTo edges (index + 1) (-1))
  depths <- newInts blockCount 0
  forUpTo blockCount $ \index -> do
    enclosing <- if index == 0 then pure 0 else readArray depths (index - 1)
    opened <- readArray edges index
    writeArray depths index (enclosing + opened)
  pure depths

-- | The values in the order the scan visits them: by the start of the
-- interval, then the values that lead, then the number of the value.
--
-- A value with a hint of its own, or with a partner placed before it, has a
-- claim on a register. It leads, so it goes before the values that are
-- defined at the same position and have no claim.
orderByStart :: Encoded -> UArray Int Int -> Rows -> Rows -> ST s (UArray Int Int)
orderByStart encoded starts hints partners = do
  let positionCount = encPositions encoded
      earlier value at
        | at >= rowTo partners value = False
        | otherwise = starts ! rowAt partners at < starts ! value || earlier value (at + 1)
      leads value = rowTo hints value > rowFrom hints value || earlier value (rowFrom partners value)
  leadCursor <- newInts (positionCount + 1) 0
  restCursor <- newInts (positionCount + 1) 0
  let cursorFor value = if leads value then leadCursor else restCursor
  forUpTo (encValues encoded) (\value -> addTo (cursorFor value) (starts ! value) 1)
  total <- newInts 1 0
  forUpTo (positionCount + 1) $ \position -> do
    leaders <- readArray leadCursor position
    rest <- readArray restCursor position
    placed <- readArray total 0
    writeArray leadCursor position placed
    writeArray restCursor position (placed + leaders)
    writeArray total 0 (placed + leaders + rest)
  order <- newInts (encValues encoded) 0
  forUpTo (encValues encoded) $ \value -> do
    at <- readArray (cursorFor value) (starts ! value)
    writeArray (cursorFor value) (starts ! value) (at + 1)
    writeArray order at value
  freezeInts order

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
