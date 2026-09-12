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
-- number order is the name order, and blocks by their position. Every pass
-- then runs on 'IntMap' and 'IntSet', and the names come back only in the
-- result.
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
import Data.Array (Array, listArray, (!))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (nub, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set

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
    { allocationRegisters = Map.mapMaybe (\value -> (registerArray !) <$> IntMap.lookup value assigned) (encodedValues encoded),
      allocationSpills = [encodedNames encoded ! value | value <- definitionOrder encoded, not (IntMap.member value assigned)],
      allocationUsed = [register | (index, register) <- zip [0 ..] pool, IntSet.member index used]
    }
  where
    encoded = encodeFunction signatures function
    volatile = registersVolatile target
    pool = volatile <> registersPreserved target
    poolSize = length pool
    registerArray = listArray (0, poolSize - 1) pool
    -- The pool is small, so a register finds its index by a walk, once
    -- per argument or result position.
    indexed = zip pool [0 ..]
    indexOf register = lookup register indexed
    carriers =
      Carriers
        { carrierArgument = memoIndex (registersArgument target),
          carrierResult = memoIndex (registersResult target)
        }
    memoIndex carrier =
      let table = listArray (0, memoLimit) [carrier index >>= indexOf | index <- [0 .. memoLimit]] :: Array Int (Maybe Int)
       in \index -> if index <= memoLimit then table ! index else carrier index >>= indexOf
    memoLimit = 15
    config =
      Config
        { configPoolSize = poolSize,
          -- The preserved registers follow the volatile ones.
          configPreserved = IntSet.fromDistinctAscList [length volatile .. poolSize - 1]
        }
    counts = accessCounts encoded
    exits = exitCount encoded
    calls = callPositions encoded
    (fixedHints, partners) = hints carriers encoded
    operandsOf = resultOperands encoded
    spans = functionSpans encoded
    starts = IntMap.fromList [(spanValue s, spanStart s) | s <- spans]
    candidates =
      [ Candidate
          { candidateSpan = s,
            candidateReach = reach calls s,
            candidateEarnsPreserved = not (registersPreservedCost target) || profitable counts exits s,
            candidateHints = direct,
            candidatePartners = ours,
            candidateWeakHints = nub (concatMap (\partner -> IntMap.findWithDefault [] partner fixedHints) ours),
            candidateOperands = IntMap.findWithDefault [] value operandsOf,
            -- A value with a hint of its own, or with a partner placed
            -- before it, has a claim on a register; it goes before the
            -- values defined at the same position that have none.
            candidateLeads = not (null direct) || any (\partner -> IntMap.lookup partner starts < Just (spanStart s)) ours
          }
      | s <- spans,
        let value = spanValue s,
        let direct = IntMap.findWithDefault [] value fixedHints,
        let ours = IntMap.findWithDefault [] value partners
      ]
    assigned = linearScan config candidates
    used = IntSet.fromList (IntMap.elems assigned)

-- Encoding

-- | A function with its values and blocks numbered.
data Encoded = Encoded
  { -- | Every value of the function by its rank among the names.
    encodedValues :: !(Map Var Int),
    encodedNames :: !(Array Int Var),
    -- | The parameters of the function, in order.
    encodedParameters :: ![Int],
    encodedBlocks :: ![EBlock]
  }

data EBlock = EBlock
  { ebIndex :: !Int,
    -- | Every position is distinct and the positions of a block are a
    -- contiguous run, so the whole block sits between its start and its
    -- end.
    ebStart :: !Int,
    ebTerminatorPosition :: !Int,
    ebEnd :: !Int,
    ebParameters :: ![Int],
    ebInstructions :: ![EInstruction],
    -- | Every read of the terminator, in order and with repeats.
    ebTerminatorReads :: ![Int],
    -- | The blocks the terminator jumps to, with the value each argument
    -- carries when it is a value.
    ebTargets :: ![(Int, [Maybe Int])],
    -- | Whether the terminator restores the saved registers: a return or a
    -- tail call. A trap does not return, so it restores nothing.
    ebExit :: !Bool,
    -- | The values the terminator hands to the convention: a tail-call
    -- argument by its index, or a returned value by its index.
    ebTerminatorHints :: ![Hint]
  }

data EInstruction = EInstruction
  { eiPosition :: !Int,
    eiResults :: ![Int],
    -- | Every read, in order and with repeats.
    eiReads :: ![Int],
    -- | The convention of the callee, for a call.
    eiCall :: !(Maybe CallingConvention),
    -- | The values a call hands to and takes from the convention.
    eiHints :: ![Hint]
  }

-- | A value the convention places: an argument by its index, or a result by
-- its index.
data Hint
  = ArgumentHint !Int !Int
  | ResultHint !Int !Int

encodeFunction :: Map Symbol Signature -> Function -> Encoded
encodeFunction signatures function =
  Encoded
    { encodedValues = values,
      encodedNames = listArray (0, Map.size values - 1) (Map.keys values),
      encodedParameters = map (valueOf . fst) (functionParameters function),
      encodedBlocks = go 0 1 blocks
    }
  where
    blocks = functionBlocks function
    names =
      Set.fromList
        ( map fst (functionParameters function)
            <> concat
              [ map fst (blockParameters block)
                  <> concat [instructionResults instruction <> operationReads (instructionOperation instruction) | instruction <- blockInstructions block]
                  <> terminatorReads (blockTerminator block)
              | block <- blocks
              ]
        )
    values = Map.fromDistinctAscList (zip (Set.toAscList names) [0 ..])
    valueOf var = values Map.! var
    operandValue operand =
      case operand of
        OperandVar var -> Just (valueOf var)
        OperandLiteral _ -> Nothing
    blockIndex = Map.fromList (zip (map blockLabel blocks) [0 ..])
    argumentHints arguments = [ArgumentHint position (valueOf var) | (position, OperandVar var) <- zip [0 ..] arguments]
    resultHints vars = [ResultHint position (valueOf var) | (position, var) <- zip [0 ..] vars]
    go _ _ [] = []
    go index start (block : rest) =
      let instructions = zipWith encodeInstruction [start + 1 ..] (blockInstructions block)
          terminatorPosition = start + 1 + length (blockInstructions block)
          terminator = blockTerminator block
          encodedBlock =
            EBlock
              { ebIndex = index,
                ebStart = start,
                ebTerminatorPosition = terminatorPosition,
                ebEnd = terminatorPosition + 1,
                ebParameters = map (valueOf . fst) (blockParameters block),
                ebInstructions = instructions,
                ebTerminatorReads = map valueOf (terminatorReads terminator),
                ebTargets = [(blockIndex Map.! targetLabel t, map operandValue (targetArguments t)) | t <- terminatorTargets terminator],
                ebExit = case terminator of
                  Return _ -> True
                  TailCall _ _ -> True
                  TailCallIndirect {} -> True
                  _ -> False,
                ebTerminatorHints = case terminator of
                  TailCall _ arguments -> argumentHints arguments
                  TailCallIndirect _ arguments _ -> argumentHints arguments
                  Return results -> [ResultHint i (valueOf var) | (i, OperandVar var) <- zip [0 ..] results]
                  _ -> []
              }
       in encodedBlock : go (index + 1) (terminatorPosition + 2) rest
    encodeInstruction position (Instruction results operation) =
      EInstruction
        { eiPosition = position,
          eiResults = map valueOf results,
          eiReads = map valueOf (operationReads operation),
          eiCall = case operation of
            Call symbol _ -> Just (maybe AihcConvention signatureConvention (Map.lookup symbol signatures))
            CallIndirect _ _ signature -> Just (signatureConvention signature)
            _ -> Nothing,
          eiHints = case operation of
            Call _ arguments -> argumentHints arguments <> resultHints results
            CallIndirect _ arguments _ -> argumentHints arguments <> resultHints results
            _ -> []
        }

-- | Every value the function defines, in the order the text defines it.
definitionOrder :: Encoded -> [Int]
definitionOrder encoded =
  encodedParameters encoded
    <> concat
      [ ebParameters block <> concatMap eiResults (ebInstructions block)
      | block <- encodedBlocks encoded
      ]

-- Calls

-- | The positions of the calls of a function, by the convention of the
-- callee.
data Calls = Calls
  { callsC :: !IntSet,
    callsAihc :: !IntSet
  }

callPositions :: Encoded -> Calls
callPositions encoded =
  Calls
    { callsC = IntSet.fromList [position | (position, CConvention) <- calls],
      callsAihc = IntSet.fromList [position | (position, AihcConvention) <- calls]
    }
  where
    calls =
      [ (eiPosition instruction, convention)
      | block <- encodedBlocks encoded,
        instruction <- ebInstructions block,
        Just convention <- [eiCall instruction]
      ]

-- | Which registers an interval may take, given the calls it lives across.
-- A call at the start of the interval defines it and a call at its end
-- consumes it; neither clobbers it.
data Reach
  = ReachAny
  | ReachPreserved
  | ReachNone

reach :: Calls -> Span -> Reach
reach calls s
  | crosses (callsAihc calls) = ReachNone
  | crosses (callsC calls) = ReachPreserved
  | otherwise = ReachAny
  where
    crosses positions =
      maybe False (< spanEnd s) (IntSet.lookupGT (spanStart s) positions)

-- Hints

-- | The pool index of the register that carries argument or result number
-- @i@, when one does.
data Carriers = Carriers
  { carrierArgument :: Int -> Maybe Int,
    carrierResult :: Int -> Maybe Int
  }

-- | The pool indexes the convention suggests for each value, and the values
-- each value is copied to or from by a jump.
hints :: Carriers -> Encoded -> (IntMap [Int], IntMap [Int])
hints carriers encoded = (fixed, partners)
  where
    blocks = encodedBlocks encoded
    parameters = IntMap.fromList [(ebIndex block, ebParameters block) | block <- blocks]
    placed hint =
      case hint of
        ArgumentHint index value -> [(value, [register]) | Just register <- [carrierArgument carriers index]]
        ResultHint index value -> [(value, [register]) | Just register <- [carrierResult carriers index]]
    fixed =
      IntMap.fromListWith
        (flip (<>))
        ( [(value, [register]) | (index, value) <- zip [0 ..] (encodedParameters encoded), Just register <- [carrierArgument carriers index]]
            <> concat [placed hint | block <- blocks, instruction <- ebInstructions block, hint <- eiHints instruction]
            <> concat [placed hint | block <- blocks, hint <- ebTerminatorHints block]
        )
    pairs =
      [ (value, parameter)
      | block <- blocks,
        (successor, arguments) <- ebTargets block,
        (Just value, parameter) <- zip arguments (IntMap.findWithDefault [] successor parameters)
      ]
    partners = IntMap.fromListWith (flip (<>)) ([(value, [parameter]) | (value, parameter) <- pairs] <> [(parameter, [value]) | (value, parameter) <- pairs])

-- | The operands each instruction result is computed from.
resultOperands :: Encoded -> IntMap [Int]
resultOperands encoded =
  IntMap.fromList
    [ (result, nub (eiReads instruction))
    | block <- encodedBlocks encoded,
      instruction <- ebInstructions block,
      result <- eiResults instruction
    ]

-- | Whether a value earns the register it would take.
--
-- A value in a frame slot costs one memory access per definition and per use.
-- A value in a register costs none of those, and instead the prologue saves
-- the register once and every exit restores it. So the register pays for
-- itself once the value is touched more often than the function has exits
-- plus the one save.
--
-- A touch inside a loop happens once for every turn of the loop, so it counts
-- for more. The weight is a power of ten per loop that encloses the block,
-- which is the usual guess in the absence of a profile, and it is capped so
-- that a deep nest cannot overflow the count.
--
-- Several values that share a register pay the save and the restores once
-- between them, so a value that clears the bar alone is never a loss and a
-- register that several values share is a gain beyond what the bar counts.
profitable :: IntMap Int -> Int -> Span -> Bool
profitable counts exits s =
  IntMap.findWithDefault 0 (spanValue s) counts > 1 + exits

-- | The number of exits: the terminators that restore the saved registers.
exitCount :: Encoded -> Int
exitCount encoded = length [() | block <- encodedBlocks encoded, ebExit block]

-- | How often the function touches each value, weighted by the loops that
-- enclose the touch: once where it defines it, and once for every place it
-- reads it. A value read twice by one instruction counts twice, because
-- instruction selection reads it twice.
accessCounts :: Encoded -> IntMap Int
accessCounts encoded =
  IntMap.fromListWith
    (+)
    ( [ (value, weightOf (ebIndex block))
      | block <- encodedBlocks encoded,
        value <-
          ebParameters block
            <> concatMap eiResults (ebInstructions block)
            <> concatMap eiReads (ebInstructions block)
            <> ebTerminatorReads block
      ]
        -- A parameter arrives before the first block.
        <> [(value, 1) | value <- encodedParameters encoded]
    )
  where
    depths = loopDepths encoded
    weightOf index = 10 ^ min 3 (IntMap.findWithDefault 0 index depths)

-- | How many loops enclose each block. A loop is a back edge and the blocks
-- that reach it without leaving through its header, which is the natural loop
-- of the edge.
loopDepths :: Encoded -> IntMap Int
loopDepths encoded =
  IntMap.fromListWith
    (+)
    [ (index, 1)
    | (tail', header) <- edges,
      index <- IntSet.toList (naturalLoop predecessors header tail')
    ]
  where
    successors = blockSuccessors encoded
    predecessors =
      IntMap.fromListWith
        (<>)
        [ (target, [source])
        | (source, targets) <- IntMap.toList successors,
          target <- targets
        ]
    edges = case encodedBlocks encoded of
      [] -> []
      entry : _ -> backEdges successors (ebIndex entry)

blockSuccessors :: Encoded -> IntMap [Int]
blockSuccessors encoded = IntMap.fromList [(ebIndex block, map fst (ebTargets block)) | block <- encodedBlocks encoded]

-- | The edges that close a loop: an edge whose target is already on the path
-- the search took to reach its source.
backEdges :: IntMap [Int] -> Int -> [(Int, Int)]
backEdges successors entry = snd (visit (IntSet.empty, []) IntSet.empty entry)
  where
    visit (done, found) path index
      | IntSet.member index done = (done, found)
      | otherwise = foldl' step (IntSet.insert index done, found) (IntMap.findWithDefault [] index successors)
      where
        path' = IntSet.insert index path
        step (seen, edges) target
          | IntSet.member target path' = (seen, (index, target) : edges)
          | otherwise = visit (seen, edges) path' target

-- | The blocks of the natural loop of a back edge: its header, its source,
-- and everything that reaches the source without passing the header.
naturalLoop :: IntMap [Int] -> Int -> Int -> IntSet
naturalLoop predecessors header tail' = grow (IntSet.fromList [header, tail']) [tail']
  where
    grow seen [] = seen
    grow seen (index : rest)
      | index == header = grow seen rest
      | otherwise =
          let fresh = [source | source <- IntMap.findWithDefault [] index predecessors, not (IntSet.member source seen)]
           in grow (foldr IntSet.insert seen fresh) (fresh <> rest)

-- Liveness

-- | The values a block reads before it writes them, and the values it
-- writes. A jump argument is a read of the block that jumps, and a block
-- parameter is a write of the block that receives it, made before anything
-- in the block reads it.
data BlockFlow = BlockFlow
  { flowUpwardUses :: !IntSet,
    flowDefinitions :: !IntSet
  }

blockFlow :: EBlock -> BlockFlow
blockFlow block =
  BlockFlow
    { flowUpwardUses = foldr IntSet.delete (foldl' step (IntSet.fromList (ebTerminatorReads block)) (reverse (ebInstructions block))) (ebParameters block),
      flowDefinitions = IntSet.fromList (ebParameters block <> concatMap eiResults (ebInstructions block))
    }
  where
    step live instruction =
      IntSet.union
        (IntSet.fromList (eiReads instruction))
        (foldr IntSet.delete live (eiResults instruction))

liveness :: Encoded -> IntMap (IntSet, IntSet)
liveness encoded
  -- A function whose blocks jump nowhere has nothing live across a block
  -- boundary: a block parameter is a definition, and a use of something a
  -- block does not define is a parameter, which is defined before the
  -- first block.
  | all (null . ebTargets) blocks = initial
  | otherwise = converge initial
  where
    blocks = encodedBlocks encoded
    flows = IntMap.fromList [(ebIndex block, blockFlow block) | block <- blocks]
    successors = blockSuccessors encoded
    initial = IntMap.fromList [(ebIndex block, (IntSet.empty, IntSet.empty)) | block <- blocks]
    converge current =
      let next = foldl' update current (reverse (map ebIndex blocks))
       in if next == current then current else converge next
    update current index =
      let flow = flows IntMap.! index
          liveOut = IntSet.unions [fst (current IntMap.! successor) | successor <- successors IntMap.! index]
          liveIn = IntSet.union (flowUpwardUses flow) (IntSet.difference liveOut (flowDefinitions flow))
       in IntMap.insert index (liveIn, liveOut) current

-- Intervals

-- | The live interval of a numbered value.
data Span = Span
  { spanValue :: !Int,
    spanStart :: !Int,
    spanEnd :: !Int
  }

-- | The live interval of every value of the function, in name order.
functionIntervals :: Function -> [Interval]
functionIntervals function =
  [ Interval {intervalVar = encodedNames encoded ! spanValue s, intervalStart = spanStart s, intervalEnd = spanEnd s}
  | s <- functionSpans encoded
  ]
  where
    encoded = encodeFunction Map.empty function

-- | The live interval of every value of the function, in value order.
--
-- A value is relevant at its definition, at each of its uses, at the start of
-- every block it is live into, and at the end of every block it is live out
-- of. The interval spans the lowest to the highest of those positions, which
-- covers every point at which the value is live whatever the block order.
functionSpans :: Encoded -> [Span]
functionSpans encoded =
  [ Span {spanValue = value, spanStart = start, spanEnd = end}
  | (value, (start, end)) <- IntMap.toAscList bounds
  ]
  where
    blocks = encodedBlocks encoded
    live = liveness encoded
    bounds = foldl' note IntMap.empty relevant
    note current (value, position) = IntMap.insertWith merge value (position, position) current
    merge (newStart, newEnd) (oldStart, oldEnd) = (min newStart oldStart, max newEnd oldEnd)
    relevant =
      -- A parameter is defined before the first block.
      [(value, 0) | value <- encodedParameters encoded]
        <> concat
          [ [(value, ebStart block) | value <- ebParameters block]
              <> concat
                [ [(result, eiPosition instruction) | result <- eiResults instruction]
                    <> [(value, eiPosition instruction) | value <- IntSet.toList (IntSet.fromList (eiReads instruction))]
                | instruction <- ebInstructions block
                ]
              <> [(value, ebTerminatorPosition block) | value <- IntSet.toList (IntSet.fromList (ebTerminatorReads block))]
              <> [(value, ebStart block) | value <- IntSet.toList liveIn]
              <> [(value, ebEnd block) | value <- IntSet.toList liveOut]
          | block <- blocks,
            let (liveIn, liveOut) = live IntMap.! ebIndex block
          ]

-- Linear scan

data Config = Config
  { configPoolSize :: !Int,
    -- | The pool indexes of the preserved registers.
    configPreserved :: !IntSet
  }

data Candidate = Candidate
  { candidateSpan :: !Span,
    candidateReach :: !Reach,
    -- | Whether the value may take a preserved register.
    candidateEarnsPreserved :: !Bool,
    -- | The pool indexes the conventions suggest for the value itself.
    candidateHints :: ![Int],
    -- | The values a jump copies this one to or from.
    candidatePartners :: ![Int],
    -- | The pool indexes the conventions suggest for the partners.
    candidateWeakHints :: ![Int],
    -- | The operands the value is computed from.
    candidateOperands :: ![Int],
    -- | Whether the value goes before the others defined at its position.
    candidateLeads :: !Bool
  }

-- | Whether a candidate may live in the register at a pool index.
accepts :: Config -> Candidate -> Int -> Bool
accepts config candidate index =
  case candidateReach candidate of
    ReachNone -> False
    ReachPreserved -> preserved && candidateEarnsPreserved candidate
    ReachAny -> not preserved || candidateEarnsPreserved candidate
  where
    preserved = IntSet.member index (configPreserved config)

-- | Walk the intervals in order of their start and hand out registers, by
-- pool index.
--
-- An interval that outlives another may take its register once that one has
-- expired. A hint of the value that is free is taken first, then the
-- register of a partner already placed, then a hint of a partner, then the
-- register of an operand that just died, then the first free register of
-- the pool. When nothing acceptable is free, the acceptable interval that
-- reaches furthest goes to a frame slot; it is the one whose register would
-- sit idle the longest.
linearScan :: Config -> [Candidate] -> IntMap Int
linearScan config candidates = scanState (foldl' step initial ordered)
  where
    initial = ScanState [] (IntSet.fromDistinctAscList [0 .. configPoolSize config - 1]) IntMap.empty
    ordered = sortOn (\candidate -> (spanStart (candidateSpan candidate), not (candidateLeads candidate), spanValue (candidateSpan candidate))) candidates
    step state candidate =
      let s = candidateSpan candidate
          expired = expire (spanStart s) state
          free = scanFree expired
          preferred =
            candidateHints candidate
              <> mapMaybe (`IntMap.lookup` scanState expired) (candidatePartners candidate)
              <> candidateWeakHints candidate
              <> mapMaybe (`IntMap.lookup` scanState expired) (candidateOperands candidate)
          choice =
            case [index | index <- preferred, IntSet.member index free, accepts config candidate index] of
              index : _ -> Just index
              [] -> firstFree candidate (IntSet.toAscList free)
       in case choice of
            Just index -> activate s index expired
            Nothing -> spill candidate expired
    firstFree candidate indexes =
      case indexes of
        [] -> Nothing
        index : rest
          | accepts config candidate index -> Just index
          | otherwise -> firstFree candidate rest
    -- The active intervals that end before this one starts give their
    -- registers back. An interval that ends exactly where the next begins
    -- does so too: the value an instruction consumes hands its register to
    -- the value the instruction defines, which every instruction a backend
    -- selects has to tolerate. A value that never lived past its own
    -- definition keeps its register, so two values one instruction defines
    -- never share.
    expire position state =
      let finished active =
            spanEnd active < position
              || (spanEnd active == position && spanStart active < spanEnd active)
          (done, alive) = span (finished . fst) (scanActive state)
       in case done of
            [] -> state
            _ ->
              state
                { scanActive = alive,
                  scanFree = foldl' (\free (_, index) -> IntSet.insert index free) (scanFree state) done
                }
    -- The active list stays sorted by end; a new interval goes before the
    -- ones that end where it ends.
    activate s index state =
      state
        { scanActive = insertActive (s, index) (scanActive state),
          scanFree = IntSet.delete index (scanFree state),
          scanState = IntMap.insert (spanValue s) index (scanState state)
        }
    insertActive entry active =
      case active of
        [] -> [entry]
        first : rest
          | spanEnd (fst entry) <= spanEnd (fst first) -> entry : active
          | otherwise -> first : insertActive entry rest
    -- The furthest-reaching acceptable interval loses its register. The
    -- active list is sorted by end, so it is the last acceptable one.
    spill candidate state =
      let s = candidateSpan candidate
       in case reverse [(active, index) | (active, index) <- scanActive state, accepts config candidate index] of
            (victim, index) : _
              | spanEnd victim > spanEnd s ->
                  activate
                    s
                    index
                    state
                      { scanActive = filter ((/= spanValue victim) . spanValue . fst) (scanActive state),
                        scanFree = IntSet.insert index (scanFree state),
                        scanState = IntMap.delete (spanValue victim) (scanState state)
                      }
            _ -> state

data ScanState = ScanState
  { -- | The intervals holding a register, sorted by their end.
    scanActive :: ![(Span, Int)],
    -- | The pool indexes nothing holds.
    scanFree :: !IntSet,
    scanState :: !(IntMap Int)
  }

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
operationReads operation =
  case operation of
    Binary _ _ left right -> operands [left, right]
    Unary _ _ value -> operands [value]
    Wide _ _ left right -> operands [left, right]
    Compare _ _ left right -> operands [left, right]
    FloatBinary _ _ left right -> operands [left, right]
    FloatUnary _ _ value -> operands [value]
    Convert _ _ value _ -> operands [value]
    PtrToInt value -> operands [value]
    PtrFromInt value -> operands [value]
    Select _ condition left right -> operands [condition, left, right]
    Load _ address _ -> operands [addressBase address]
    Store _ value address _ -> operands [value, addressBase address]
    PtrAdd base offset -> operands [base, offset]
    StackAlloc _ _ -> []
    GlobalGet _ -> []
    GlobalSet _ value -> operands [value]
    Call _ arguments -> operands arguments
    CallIndirect callee arguments _ -> operands (callee : arguments)

-- | Every read of a value by one terminator, in order and with repeats.
terminatorReads :: Terminator -> [Var]
terminatorReads terminator =
  case terminator of
    Jump target -> operands (targetArguments target)
    Branch condition whenTrue whenFalse -> operands (condition : targetArguments whenTrue <> targetArguments whenFalse)
    Switch _ scrutinee cases fallback ->
      operands
        ( scrutinee
            : concatMap (targetArguments . switchCaseTarget) cases
              <> concatMap targetArguments fallback
        )
    Return values -> operands values
    TailCall _ arguments -> operands arguments
    TailCallIndirect callee arguments _ -> operands (callee : arguments)
    Trap _ -> []

operands :: [Operand] -> [Var]
operands values = [var | OperandVar var <- values]
