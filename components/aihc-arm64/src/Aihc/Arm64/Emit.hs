{-# LANGUAGE OverloadedStrings #-}

-- | Render register-allocated AArch64 LIR. Spills are loaded from and stored
-- into the current Haskell heap frame in @x19@.
module Aihc.Arm64.Emit
  ( EmitError (..),
    renderAllocatedBlock,
    renderPhysicalReg,
  )
where

import Aihc.Arm64.Lir
import Aihc.Arm64.RegisterAllocate
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data EmitError
  = EmitMissingAllocation !VirtualReg
  | EmitTooManySpilledOperands !(Instruction PhysicalReg)
  deriving (Eq, Show)

renderAllocatedBlock :: Int -> [Instruction PhysicalReg] -> Either EmitError ([Text], Int)
renderAllocatedBlock spillBase instructions = do
  rendered <- mapM (renderInstruction spillBase allocation) instructions
  pure (concat rendered, allocationSpillCount allocation)
  where
    allocation = allocateBlock instructions

renderInstruction :: Int -> Allocation PhysicalReg -> Instruction PhysicalReg -> Either EmitError [Text]
renderInstruction spillBase allocation instruction = do
  let virtuals = Set.toAscList (instructionUses instruction <> instructionDefs instruction)
      spilled =
        [ register
        | register <- virtuals,
          Just InHeapSpill {} <- [Map.lookup register locations]
        ]
  scratchAssignments <-
    if length spilled <= length spillScratchRegisters
      then pure (Map.fromList (zip spilled spillScratchRegisters))
      else Left (EmitTooManySpilledOperands instruction)
  before <- fmap concat . mapM (loadSpill scratchAssignments) $ Set.toAscList (instructionUses instruction)
  core <- renderCore (resolveRegister scratchAssignments) instruction
  after <- fmap concat . mapM (storeSpill scratchAssignments) $ Set.toAscList (instructionDefs instruction)
  pure (before <> [core] <> after)
  where
    locations = allocationLocations allocation
    resolveRegister scratchAssignments register =
      case register of
        Physical physical -> Right physical
        Virtual virtual ->
          case Map.lookup virtual locations of
            Just (InRegister physical) -> Right physical
            Just InHeapSpill {} ->
              maybe (Left (EmitMissingAllocation virtual)) Right (Map.lookup virtual scratchAssignments)
            Nothing -> Left (EmitMissingAllocation virtual)
    loadSpill scratchAssignments virtual =
      case Map.lookup virtual locations of
        Just (InHeapSpill slot) -> do
          scratch <- maybe (Left (EmitMissingAllocation virtual)) Right (Map.lookup virtual scratchAssignments)
          pure [heapLoad scratch (spillBase + slot)]
        Just InRegister {} -> pure []
        Nothing -> Left (EmitMissingAllocation virtual)
    storeSpill scratchAssignments virtual =
      case Map.lookup virtual locations of
        Just (InHeapSpill slot) -> do
          scratch <- maybe (Left (EmitMissingAllocation virtual)) Right (Map.lookup virtual scratchAssignments)
          pure [heapStore scratch (spillBase + slot)]
        Just InRegister {} -> pure []
        Nothing -> Left (EmitMissingAllocation virtual)

renderCore :: (Register PhysicalReg -> Either EmitError PhysicalReg) -> Instruction PhysicalReg -> Either EmitError Text
renderCore resolve instruction =
  case instruction of
    Move destination source -> binary "mov" <$> resolve destination <*> resolve source
    MoveImmediate destination integer -> do
      destination' <- resolve destination
      pure ("  ldr " <> renderPhysicalReg destination' <> ", =" <> tshow integer)
    Load destination base offset -> do
      destination' <- resolve destination
      base' <- resolve base
      pure (memory "ldr" destination' base' offset)
    Store source base offset -> do
      source' <- resolve source
      base' <- resolve base
      pure (memory "str" source' base' offset)
    Add destination left right -> do
      destination' <- resolve destination
      left' <- resolve left
      right' <- resolve right
      pure (ternary "add" destination' left' right')
    Compare left right -> binary "cmp" <$> resolve left <*> resolve right
    CompareImmediate value integer -> do
      value' <- resolve value
      pure ("  cmp " <> renderPhysicalReg value' <> ", #" <> tshow integer)
    LoadAddress destination label -> do
      destination' <- resolve destination
      let rendered = renderPhysicalReg destination'
      pure ("  adrp " <> rendered <> ", " <> label <> "@PAGE\n  add " <> rendered <> ", " <> rendered <> ", " <> label <> "@PAGEOFF")
    Call symbol -> pure ("  bl " <> symbol)

binary :: Text -> PhysicalReg -> PhysicalReg -> Text
binary opcode destination source =
  "  " <> opcode <> " " <> renderPhysicalReg destination <> ", " <> renderPhysicalReg source

ternary :: Text -> PhysicalReg -> PhysicalReg -> PhysicalReg -> Text
ternary opcode destination left right =
  "  " <> opcode <> " " <> renderPhysicalReg destination <> ", " <> renderPhysicalReg left <> ", " <> renderPhysicalReg right

memory :: Text -> PhysicalReg -> PhysicalReg -> Int -> Text
memory opcode value base offset =
  "  " <> opcode <> " " <> renderPhysicalReg value <> ", [" <> renderPhysicalReg base <> ", #" <> tshow offset <> "]"

heapLoad :: PhysicalReg -> Int -> Text
heapLoad destination slot = memory "ldr" destination X19 (slot * 8)

heapStore :: PhysicalReg -> Int -> Text
heapStore source slot = memory "str" source X19 (slot * 8)

spillScratchRegisters :: [PhysicalReg]
spillScratchRegisters = [X8, X7, X6]

renderPhysicalReg :: PhysicalReg -> Text
renderPhysicalReg register =
  case register of
    X0 -> "x0"
    X1 -> "x1"
    X2 -> "x2"
    X3 -> "x3"
    X4 -> "x4"
    X5 -> "x5"
    X6 -> "x6"
    X7 -> "x7"
    X8 -> "x8"
    X9 -> "x9"
    X10 -> "x10"
    X11 -> "x11"
    X12 -> "x12"
    X13 -> "x13"
    X14 -> "x14"
    X15 -> "x15"
    X19 -> "x19"
    X20 -> "x20"
    X21 -> "x21"
    X22 -> "x22"

tshow :: (Show value) => value -> Text
tshow = T.pack . show
