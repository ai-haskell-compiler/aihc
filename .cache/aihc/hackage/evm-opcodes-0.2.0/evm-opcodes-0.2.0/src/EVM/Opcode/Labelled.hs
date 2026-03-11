{-# LANGUAGE ViewPatterns #-}

-- |
-- Module: EVM.Opcode.Labelled
-- Copyright: 2018-2024 Simon Shine
-- Maintainer: Simon Shine <simon@simonshine.dk>
-- License: MIT
--
-- This module exposes the 'LabelledOpcode' type for expressing Ethereum VM
-- opcodes with labelled jumps. Plain Ethereum VM Opcodes are not so ergonomic
-- because one has to know the exact byte offset of the target 'JUMPDEST'.
--
-- With 'EVM.Opcode.Opcode' the byte offset is pushed to the stack via 'PUSH',
-- but the offset to the 'JUMPDEST' depends on all occurrences of 'PUSH' prior
-- to the label, including the 'PUSH' to the label itself.

module EVM.Opcode.Labelled
  ( Label
  , LabelledOpcode
  , TranslateError(..)
  , translate
  , labelPositions
  ) where

import Data.Function (fix)
import Data.List (foldl', group, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import EVM.Opcode (Opcode' (..), concrete, jumpAnnot, jumpdest, jumpdestAnnot, opcodeSize)
import EVM.Opcode.Positional (Position, PositionalOpcode, jumpSize)
import EVM.Opcode.Traversal (OpcodeMapper (..), mapOpcodeM)

-- | For now, all labels are 'Text'.
type Label = Text

-- | 'LabelledOpcode's use 'Label' to represent jumps.
--
-- In particular, @'JUMP' "name"@, @'JUMPI' "name"@ and @'JUMPDEST' "name"@.
--
-- All other opcodes remain the same.
type LabelledOpcode = Opcode' Label

-- | Translation of 'LabelledOpcode's into 'PositionalOpcode's may fail if
-- a jump is made to a non-occurring 'JUMPDEST' or a 'JUMPDEST' occurs twice.

data TranslateError = TranslateError
  { translateErrorMissingJumpdests   :: [Label]
  , translateErrorDuplicateJumpdests :: [Label]
  } deriving (Eq, Show)

-- | Replace all labels with absolute positions.
--
-- Positions are calculated by fixed-point iteration to account for variable
-- sizes of jumps. Labelled jumps don't have a size defined, the size of a
-- positional jump depends on the address being jumped to.
--
-- For example, if jumping to the 'JUMPDEST' on the 256th position in a
-- @['LabelledOpcode']@, this requires a @PUSH2@ instruction which uses an
-- additional byte, which pushes the 'JUMPDEST' one byte ahead.

translate :: [LabelledOpcode] -> Either TranslateError [PositionalOpcode]
translate opcodes = do
  labelMap <- labelPositions opcodes
  traverse (replaceLabel labelMap) opcodes
  where
    replaceLabel = mapOpcodeM . jumpMapper . lookupLabel

    -- Apply @f@ to the parameter of 'JUMP's, 'JUMPI's and 'JUMPDEST's
    jumpMapper f = OpcodeMapper
      { mapOnJump = fmap JUMP . f
      , mapOnJumpi = fmap JUMPI . f
      , mapOnJumpdest = fmap JUMPDEST . f
      , mapOnOther = const (pure Nothing)
      }

    -- Let @f@ be @'lookupLabel' labelMap@.
    lookupLabel labelMap label =
      case Map.lookup label labelMap of
        Just pos -> Right pos
        Nothing -> Left (TranslateError [label] [])


-- | Extract a @'Map' 'Label' 'Position'@ that describes where each 'JUMPDEST'
-- is located, taking into account the sizes of all prior opcodes.

labelPositions :: [LabelledOpcode] -> Either TranslateError (Map Label Position)
labelPositions opcodes
  | null wildJumps && null duplicateDests = Right (fixpoint opcodes)
  | otherwise = Left (TranslateError wildJumps duplicateDests)
  where
    wildJumps :: [Label]
    wildJumps = jumps `missing` dests

    duplicateDests :: [Label]
    duplicateDests = duplicate dests

    jumps :: [Label]
    jumps = mapMaybe jumpAnnot opcodes

    dests :: [Label]
    dests = mapMaybe jumpdestAnnot opcodes

    missing :: Ord a => [a] -> [a] -> [a]
    missing xs ys = Set.toList (Set.difference (Set.fromList xs) (Set.fromList ys))

    duplicate :: Ord a => [a] -> [a]
    duplicate = concatMap (take 1) . filter ((> 1) . length) . group . sort

-- | Extract a 'Map' the position of every 'JUMPDEST'.
--
-- Do this by keeping track of the current position.
--
-- This function may not terminate for all inputs!

fixpoint :: [LabelledOpcode] -> Map Label Position
fixpoint opcodes = flip fix Map.empty $ \go labelMap ->
  case step labelMap opcodes of
    (True, _, labelMap') -> labelMap'
    (False, _, labelMap') -> go labelMap'

-- | A single step in the fixpoint function is going over every opcode and
-- checking if its position is already aligned and updating the map of
-- positions otherwise.
step :: Map Label Position
     -> [LabelledOpcode]
     -> (Bool, Position, Map Label Position)
step labelMap = foldl' align (True, 0, labelMap)

align :: (Bool, Position, Map Label Position)
      -> LabelledOpcode
      -> (Bool, Position, Map Label Position)

-- When encountering a 'JUMPDEST' that was either not seen before, or was seen
-- at another offset, it hasn't been aligned. In that case, update the 'Map'
-- and signal that another iteration of 'fixpoint' is necessary.
align (done, currentBytePos, labelMap) (JUMPDEST label) =
  let aligned = Map.lookup label labelMap == Just currentBytePos
  in ( done && aligned
     , currentBytePos + opcodeSize jumpdest
     , Map.insert label currentBytePos labelMap
     )

-- When encountering a 'JUMP' or a 'JUMPI', check if the destination 'JUMPDEST'
-- was seen before. If so, increment the running offset with the size of a jump
-- to that 'JUMPDEST'. If not, the offset is still approximate.
align (done, currentBytePos, labelMap) (jumpAnnot -> Just label) =
  case Map.lookup label labelMap of
    Just bytePos -> ( done, currentBytePos + jumpSize bytePos, labelMap )
    Nothing      -> ( False, currentBytePos + jumpSize 0,      labelMap )

-- For any straight-line opcode, just increment the offset with its size.
align (done, currentBytePos, labelMap) opcode =
  ( done, currentBytePos + opcodeSize (concrete opcode), labelMap )
