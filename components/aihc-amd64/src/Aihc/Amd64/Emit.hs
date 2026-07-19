{-# LANGUAGE OverloadedStrings #-}

-- | Render register-allocated AMD64 LIR. Spills are loaded from and stored
-- into the current Haskell heap frame in @r14@.
module Aihc.Amd64.Emit
  ( EmitError (..),
    renderAllocatedBlock,
    renderPhysicalReg,
  )
where

import Aihc.Amd64.Lir
import Aihc.Amd64.RegisterAllocate
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
      pure ("  mov " <> renderPhysicalReg destination' <> ", " <> tshow integer)
    Load destination base offset -> do
      destination' <- resolve destination
      base' <- resolve base
      pure (memory "mov" destination' base' offset)
    Store source base offset -> do
      source' <- resolve source
      base' <- resolve base
      pure (memoryStore source' base' offset)
    Add destination left right -> do
      destination' <- resolve destination
      left' <- resolve left
      right' <- resolve right
      pure (ternary "add" destination' left' right')
    Compare left right -> binary "cmp" <$> resolve left <*> resolve right
    CompareImmediate value integer -> do
      value' <- resolve value
      pure ("  cmp " <> renderPhysicalReg value' <> ", " <> tshow integer)
    LoadAddress destination label -> do
      destination' <- resolve destination
      pure ("  lea " <> renderPhysicalReg destination' <> ", [rip + " <> label <> "]")
    Call symbol -> pure ("  call " <> symbol)

binary :: Text -> PhysicalReg -> PhysicalReg -> Text
binary opcode destination source =
  "  " <> opcode <> " " <> renderPhysicalReg destination <> ", " <> renderPhysicalReg source

ternary :: Text -> PhysicalReg -> PhysicalReg -> PhysicalReg -> Text
ternary opcode destination left right =
  "  mov " <> renderPhysicalReg destination <> ", " <> renderPhysicalReg left <> "\n  " <> opcode <> " " <> renderPhysicalReg destination <> ", " <> renderPhysicalReg right

memory :: Text -> PhysicalReg -> PhysicalReg -> Int -> Text
memory opcode value base offset =
  "  " <> opcode <> " " <> renderPhysicalReg value <> ", QWORD PTR [" <> renderPhysicalReg base <> offsetText offset <> "]"

memoryStore :: PhysicalReg -> PhysicalReg -> Int -> Text
memoryStore source base offset =
  "  mov QWORD PTR [" <> renderPhysicalReg base <> offsetText offset <> "], " <> renderPhysicalReg source

offsetText :: Int -> Text
offsetText offset
  | offset == 0 = ""
  | offset > 0 = " + " <> tshow offset
  | otherwise = " - " <> tshow (abs offset)

heapLoad :: PhysicalReg -> Int -> Text
heapLoad destination slot = memory "mov" destination R14 (slot * 8)

heapStore :: PhysicalReg -> Int -> Text
heapStore source slot = memoryStore source R14 (slot * 8)

spillScratchRegisters :: [PhysicalReg]
spillScratchRegisters = [R10, R11, R12]

renderPhysicalReg :: PhysicalReg -> Text
renderPhysicalReg register =
  case register of
    Rax -> "rax"
    Rdi -> "rdi"
    Rsi -> "rsi"
    Rdx -> "rdx"
    Rcx -> "rcx"
    R8 -> "r8"
    R9 -> "r9"
    R10 -> "r10"
    R11 -> "r11"
    R12 -> "r12"
    R13 -> "r13"
    R14 -> "r14"
    R15 -> "r15"

tshow :: (Show value) => value -> Text
tshow = T.pack . show
