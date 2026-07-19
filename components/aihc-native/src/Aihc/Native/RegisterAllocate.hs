-- | Architecture-neutral linear-scan allocation for straight-line native LIR
-- blocks. Spill slots are abstract heap-frame slots, never native-stack
-- offsets.
module Aihc.Native.RegisterAllocate
  ( AllocatorConfig (..),
    Location (..),
    Allocation (..),
    allocateBlock,
  )
where

import Aihc.Native.Lir
import Data.List (maximumBy, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Set qualified as Set

newtype AllocatorConfig physical = AllocatorConfig
  { allocatorRegisters :: [physical]
  }

data Location physical
  = InRegister !physical
  | InHeapSpill !Int
  deriving (Eq, Show)

data Allocation physical = Allocation
  { allocationLocations :: !(Map VirtualReg (Location physical)),
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

data Active physical = Active
  { activeInterval :: !Interval,
    activePhysical :: !physical
  }

data ScanState physical = ScanState
  { scanActive :: ![Active physical],
    scanLocations :: !(Map VirtualReg (Location physical)),
    scanNextSpill :: !Int
  }

allocateBlock :: (Ord physical) => AllocatorConfig physical -> [Instruction physical] -> Allocation physical
allocateBlock config instructions =
  Allocation
    { allocationLocations = scanLocations finalState,
      allocationSpillCount = scanNextSpill finalState
    }
  where
    intervals = sortOn intervalStart (buildIntervals instructions)
    finalState = foldl' (allocateInterval (allocatorRegisters config)) initialState intervals
    initialState = ScanState [] Map.empty 0

allocateInterval :: (Ord physical) => [physical] -> ScanState physical -> Interval -> ScanState physical
allocateInterval registers state interval =
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
    freeRegisters = filter (`Set.notMember` occupied) registers

spillInterval :: ScanState physical -> Interval -> ScanState physical
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

assignSpill :: ScanState physical -> Interval -> ScanState physical
assignSpill state interval =
  state
    { scanActive = filter ((/= intervalRegister interval) . intervalRegister . activeInterval) (scanActive state),
      scanLocations = Map.insert (intervalRegister interval) (InHeapSpill spill) (scanLocations state),
      scanNextSpill = spill + 1
    }
  where
    spill = scanNextSpill state

buildIntervals :: [Instruction physical] -> [Interval]
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
