-- | Architecture-neutral trace layout for generated native basic blocks.
--
-- Instructions remain backend-owned. This module only orders labeled blocks
-- along their preferred unconditional successors and omits jumps when that
-- successor is laid out immediately afterward.
module Aihc.Native.BlockLayout
  ( Block (..),
    Terminator (..),
    layoutBlocks,
    renderBlocks,
  )
where

import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Set qualified as Set

data Block label instruction = Block
  { blockLabel :: !label,
    blockInstructions :: ![instruction],
    blockTerminator :: !(Terminator label)
  }
  deriving (Eq, Show)

data Terminator label
  = Jump !label
  | Exit
  deriving (Eq, Show)

-- | Lay out the entry trace first, following each block's preferred successor.
-- Once a trace ends, begin the next trace in the original source order.
layoutBlocks :: (Ord label) => label -> [Block label instruction] -> [Block label instruction]
layoutBlocks entry blocks = go Set.empty (Just entry)
  where
    blocksByLabel = Map.fromList [(blockLabel block, block) | block <- blocks]

    go placed requested =
      case requested >>= selectUnplaced placed of
        Just block -> block : go (Set.insert (blockLabel block) placed) (successor (blockTerminator block))
        Nothing ->
          case listToMaybe [block | block <- blocks, blockLabel block `Set.notMember` placed] of
            Just block -> block : go (Set.insert (blockLabel block) placed) (successor (blockTerminator block))
            Nothing -> []

    selectUnplaced placed label
      | label `Set.member` placed = Nothing
      | otherwise = Map.lookup label blocksByLabel

    successor terminator =
      case terminator of
        Jump label -> Just label
        Exit -> Nothing

-- | Render blocks, suppressing an unconditional jump to the next block.
renderBlocks :: (Eq label) => (label -> instruction) -> (label -> instruction) -> [Block label instruction] -> [instruction]
renderBlocks renderLabel renderJump blocks =
  concat
    [ renderLabel (blockLabel block)
        : blockInstructions block
          <> case blockTerminator block of
            Jump successor | Just successor /= nextLabel -> [renderJump successor]
            _ -> []
    | (block, nextLabel) <- zip blocks (map (Just . blockLabel) (drop 1 blocks) <> [Nothing])
    ]
