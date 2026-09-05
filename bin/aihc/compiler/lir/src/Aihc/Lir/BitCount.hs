{-# LANGUAGE OverloadedStrings #-}

-- | Rewrite the Lir bit-count operations into ordinary integer arithmetic.
--
-- @clz@, @ctz@, and @popcount@ have a machine instruction on WebAssembly and
-- an intrinsic on LLVM, so those backends emit them directly. The AArch64 and
-- the AMD64 backends assemble their own machine code, and the instructions
-- that would serve them are not part of the baseline of either architecture:
-- AMD64 has @popcnt@ only with SSE4.2 and @lzcnt@ only with BMI1, and AArch64
-- counts bits of a general register through the vector unit. Rather than
-- teach both assemblers a CPU feature test, they run this pass first.
--
-- Every expansion is straight-line arithmetic. It introduces no block and no
-- edge, so a rewritten module has the control-flow graph of the original one
-- and passes the same linter.
module Aihc.Lir.BitCount
  ( expandBitCounts,
  )
where

import Aihc.Lir.Syntax
import Control.Monad (foldM)
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

-- | Replace every 'Unary' operation by the arithmetic that computes it.
expandBitCounts :: Module -> Module
expandBitCounts (Module items) = Module (map expandItem items)

expandItem :: Item -> Item
expandItem item =
  case item of
    ItemFunction function -> ItemFunction (expandFunction function)
    _ -> item

expandFunction :: Function -> Function
expandFunction function =
  function {functionBlocks = evalState (traverse expandBlock (functionBlocks function)) (Fresh (definedVars function) 0)}

expandBlock :: Block -> M Block
expandBlock block = do
  instructions <- concat <$> traverse expandInstruction (blockInstructions block)
  pure block {blockInstructions = instructions}

expandInstruction :: Instruction -> M [Instruction]
expandInstruction instruction =
  case instruction of
    Instruction [result] (Unary op ty value) -> expand op ty value result
    _ -> pure [instruction]

-- | The names a function already defines. A fresh name avoids all of them.
definedVars :: Function -> Set Var
definedVars function =
  Set.fromList
    ( map fst (functionParameters function)
        <> concatMap blockVars (functionBlocks function)
    )
  where
    blockVars block =
      map fst (blockParameters block) <> concatMap instructionResults (blockInstructions block)

data Fresh = Fresh !(Set Var) !Int

type M = State Fresh

fresh :: M Var
fresh = do
  Fresh used counter <- get
  put (Fresh used (counter + 1))
  let candidate = Var ("bitcount." <> T.pack (show counter))
  if Set.member candidate used then fresh else pure candidate

-- | Emit one instruction and give back the operand that names its result.
emit :: Operation -> M (Instruction, Operand)
emit operation = do
  var <- fresh
  pure (Instruction [var] operation, OperandVar var)

expand :: UnaryOp -> Type -> Operand -> Var -> M [Instruction]
expand op ty value result =
  case op of
    Popcount -> do
      (instructions, final) <- populationCount ty value
      pure (instructions <> [Instruction [result] final])
    -- The lowest set bit of a value is @x .&. negate x@. One less than that
    -- has exactly the trailing zeros of @x@ set. A zero operand isolates
    -- nothing, so the mask becomes all ones and the count becomes the width.
    Ctz -> do
      (negateStep, negated) <- emit (Binary Sub ty (literal 0) value)
      (lowestStep, lowest) <- emit (Binary And ty value negated)
      (maskStep, masked) <- emit (Binary Sub ty lowest (literal 1))
      (instructions, final) <- populationCount ty masked
      pure ([negateStep, lowestStep, maskStep] <> instructions <> [Instruction [result] final])
    -- Smearing every set bit to the right leaves a value whose population
    -- count is the number of significant bits. The leading zeros are the rest
    -- of the width. A zero operand smears to zero and counts the full width.
    Clz -> do
      (smearSteps, smeared) <- foldM smear ([], value) (takeWhile (<= toInteger bits `div` 2) [2 ^ stage | stage <- [0 :: Int ..]])
      (instructions, final) <- populationCount ty smeared
      (countStep, count) <- emit final
      pure (smearSteps <> instructions <> [countStep, Instruction [result] (Binary Sub ty (literal (toInteger bits)) count)])
  where
    bits = typeBits ty
    literal = OperandLiteral . LitInt
    smear (steps, current) distance = do
      (shiftStep, shifted) <- emit (Binary ShrU ty current (literal distance))
      (orStep, merged) <- emit (Binary Or ty current shifted)
      pure (steps <> [shiftStep, orStep], merged)

-- | The instructions of a population count, and the operation that produces
-- the count itself. The caller names that last result, so the expansion adds
-- no copy.
--
-- This is the usual divide-and-conquer count: pairs of bits, then nibbles,
-- then bytes, and finally one multiplication that sums the bytes into the
-- top one. Every constant repeats a byte pattern over the width of the type,
-- so one shape serves @i8@ through @i64@.
populationCount :: Type -> Operand -> M ([Instruction], Operation)
populationCount ty value = do
  (pairShift, shiftedPairs) <- emit (Binary ShrU ty value (literal 1))
  (pairMask, maskedPairs) <- emit (Binary And ty shiftedPairs (literal (repeatByte 0x55)))
  (pairs, paired) <- emit (Binary Sub ty value maskedPairs)
  (lowNibbleMask, lowNibbles) <- emit (Binary And ty paired (literal (repeatByte 0x33)))
  (nibbleShift, shiftedNibbles) <- emit (Binary ShrU ty paired (literal 2))
  (highNibbleMask, highNibbles) <- emit (Binary And ty shiftedNibbles (literal (repeatByte 0x33)))
  (nibbles, nibbled) <- emit (Binary Add ty lowNibbles highNibbles)
  (byteShift, shiftedBytes) <- emit (Binary ShrU ty nibbled (literal 4))
  (bytes, summed) <- emit (Binary Add ty nibbled shiftedBytes)
  let prefix = [pairShift, pairMask, pairs, lowNibbleMask, nibbleShift, highNibbleMask, nibbles, byteShift, bytes]
      byteCounts = Binary And ty summed (literal (repeatByte 0x0f))
  if bits == 8
    then pure (prefix, byteCounts)
    else do
      (mask, masked) <- emit byteCounts
      (multiply, spread) <- emit (Binary Mul ty masked (literal (repeatByte 0x01)))
      pure (prefix <> [mask, multiply], Binary ShrU ty spread (literal (toInteger bits - 8)))
  where
    bits = typeBits ty
    literal = OperandLiteral . LitInt
    repeatByte byte = sum [byte * 256 ^ index | index <- [0 .. bits `div` 8 - 1]]
