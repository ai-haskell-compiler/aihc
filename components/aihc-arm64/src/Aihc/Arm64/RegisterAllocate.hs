-- | Fast linear-scan allocation for straight-line AArch64 LIR blocks.
--
-- Haskell spill slots are abstract frame slots, never native-stack offsets.
-- Code generation materializes them relative to the current heap frame.
module Aihc.Arm64.RegisterAllocate
  ( Location (..),
    Allocation (..),
    allocateBlock,
    allocatableRegisters,
  )
where

import Aihc.Arm64.Lir
import Data.List (maximumBy, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Set qualified as Set

data Location
  = InRegister !PhysicalReg
  | InHeapSpill !Int
  deriving (Eq, Show)

data Allocation = Allocation
  { allocationLocations :: !(Map VirtualReg Location),
    allocationSpillCount :: !Int
  }
  deriving (Eq, Show)

data Interval = Interval
  { intervalRegister :: !VirtualReg,
    intervalStart :: !Int,
    intervalEnd :: !Int,
    intervalCrossesCall :: !Bool
  }
  deriving (Eq, Show)

data Active = Active
  { activeInterval :: !Interval,
    activePhysical :: !PhysicalReg
  }

data ScanState = ScanState
  { scanActive :: ![Active],
    scanLocations :: !(Map VirtualReg Location),
    scanNextSpill :: !Int
  }

-- | Registers which are caller-clobbered by C and not reserved for the linker,
-- platform, frame pointer, link register, or runtime state. Values live across
-- a 'Call' are assigned heap spill slots instead.
allocatableRegisters :: [PhysicalReg]
allocatableRegisters = [X9, X10, X11, X12, X13, X14, X15]

allocateBlock :: [Instruction] -> Allocation
allocateBlock instructions =
  Allocation
    { allocationLocations = scanLocations finalState,
      allocationSpillCount = scanNextSpill finalState
    }
  where
    intervals = sortOn intervalStart (buildIntervals instructions)
    finalState = foldl' allocateInterval initialState intervals
    initialState = ScanState [] Map.empty 0

allocateInterval :: ScanState -> Interval -> ScanState
allocateInterval state interval =
  if intervalCrossesCall interval
    then assignSpill stateAfterExpiry interval
    else case freeRegisters of
      physical : _ ->
        stateAfterExpiry
          { scanActive = Active interval physical : scanActive stateAfterExpiry,
            scanLocations = Map.insert (intervalRegister interval) (InRegister physical) (scanLocations stateAfterExpiry)
          }
      [] -> spillInterval stateAfterExpiry interval
  where
    (_, remaining) = span ((< intervalStart interval) . intervalEnd . activeInterval) (sortOn (intervalEnd . activeInterval) (scanActive state))
    stateAfterExpiry = state {scanActive = remaining}
    occupied = Set.fromList (map activePhysical remaining)
    freeRegisters = filter (`Set.notMember` occupied) allocatableRegisters

spillInterval :: ScanState -> Interval -> ScanState
spillInterval state interval =
  case scanActive state of
    [] -> assignSpill state interval
    active
      | intervalEnd (activeInterval spillCandidate) > intervalEnd interval ->
          let stateWithOldSpilled = assignSpill state (activeInterval spillCandidate)
              activeWithoutCandidate = filter ((/= intervalRegister (activeInterval spillCandidate)) . intervalRegister . activeInterval) (scanActive stateWithOldSpilled)
              physical = activePhysical spillCandidate
           in stateWithOldSpilled
                { scanActive = Active interval physical : activeWithoutCandidate,
                  scanLocations = Map.insert (intervalRegister interval) (InRegister physical) (scanLocations stateWithOldSpilled)
                }
      | otherwise -> assignSpill state interval
      where
        spillCandidate = maximumBy (comparing (intervalEnd . activeInterval)) active

assignSpill :: ScanState -> Interval -> ScanState
assignSpill state interval =
  state
    { scanActive = filter ((/= intervalRegister interval) . intervalRegister . activeInterval) (scanActive state),
      scanLocations = Map.insert (intervalRegister interval) (InHeapSpill spill) (scanLocations state),
      scanNextSpill = spill + 1
    }
  where
    spill = scanNextSpill state

buildIntervals :: [Instruction] -> [Interval]
buildIntervals instructions =
  [ Interval register first lastPosition (any (\position -> first < position && position < lastPosition) callPositions)
  | (register, (first, lastPosition)) <- Map.toList positions
  ]
  where
    callPositions = [position | (position, Call {}) <- zip [0 ..] instructions]
    positions =
      foldl'
        ( \accumulator (position, instruction) ->
            foldl'
              (recordPosition position)
              accumulator
              (Set.toList (instructionDefs instruction <> instructionUses instruction))
        )
        Map.empty
        (zip [0 ..] instructions)
    recordPosition position accumulator register =
      Map.insertWith
        (\(newFirst, newLast) (oldFirst, oldLast) -> (min newFirst oldFirst, max newLast oldLast))
        register
        (position, position)
        accumulator
