
module OpcodeGenerators where

import Prelude hiding (LT, EQ, GT)

import           Data.Bits (shift)
import           Data.Containers.ListUtils (nubOrd)
import           Data.DoubleWord (Word256)
import           Data.Text (Text)
import qualified Data.Text as Text

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           EVM.Opcode as Opcode
import           EVM.Opcode.Positional
import           EVM.Opcode.Labelled
import           EVM.Opcode.Traversal

-- | Here are the three main strategies for generating testable opcodes:
--
--  1. 'genOpcode' will generate any 'Opcode'.
--
--  2. 'genOpcode'1' will generate any jump-free 'Opcode'' of 'opcodeSize' 1.
--
--  3. 'genLabelledOpcodes' will generate a sequence of opcodes for which
--     all jumps are labelled and all jumps have valid jumpdests. Many jumps
--     to the same jumpdest may occur, but the same jumpdest may not occur
--     twice.
--

-- | Generate any 'Opcode'.
genOpcode :: Gen Opcode
genOpcode = Opcode.concrete <$> Gen.frequency
  [ (3, genOpcode'1)
  , (1, genPushOpcode)
  , (1, genJumpyOpcode)
  ]

-- | Generate a jump-free 'Opcode'' of 'opcodeSize' 1.
--
-- That means, no 'JUMP', 'JUMPI', 'JUMPDEST' or 'PUSH'.
--
-- Only jumps of type 'Opcode' have a clearly defined size.
genOpcode'1 :: Gen (Opcode' a)
genOpcode'1 = Gen.element opcode1

-- | Generate a list of 'LabelledOpcode' where all 'JUMP' and 'JUMPI' have a 'JUMPDEST'.
genLabelledOpcodes :: Gen [LabelledOpcode]
genLabelledOpcodes = do
  opcodes <- Gen.list (Range.linear 0 40) genLabelledOpcode
  let jumpdests = JUMPDEST <$> (nubOrd . foldMap extractLabel) opcodes
  Gen.shuffle (opcodes <> jumpdests)
  where
    extractLabel :: LabelledOpcode -> [Label]
    extractLabel (JUMP label) = [label]
    extractLabel (JUMPI label) = [label]
    extractLabel _ = []

-- | Generate a 'LabelledOpcode' that isn't 'JUMPDEST'.
genLabelledOpcode :: Gen LabelledOpcode
genLabelledOpcode = Gen.frequency
  [ (3, genOpcode'1)
  , (1, genPushOpcode)
  , (1, genLabelledJumpOpcode)
  ]

-- | Generate any jump-free 'Opcode''.
--
-- That means, no 'JUMP', 'JUMPI', 'JUMPDEST'.
genOpcodeN :: Gen (Opcode' a)
genOpcodeN = Gen.frequency
  [ (length opcode1, genOpcode'1)
  , (1, genPushOpcode)
  ]

-- | A list of opcodes of byte size 1.
--
-- That means, no 'JUMP', 'JUMPI', 'JUMPDEST' or 'PUSH'.
opcode1 :: [Opcode' a]
opcode1 =
  [ STOP, ADD, MUL, SUB, DIV, SDIV, MOD, SMOD, ADDMOD, MULMOD, EXP, SIGNEXTEND
  , LT, GT, SLT, SGT, EQ, ISZERO, AND, OR, XOR, NOT, BYTE, SHL, SHR, SAR
  , KECCAK256

  , ADDRESS, BALANCE, ORIGIN
  , CALLER, CALLVALUE, CALLDATALOAD, CALLDATASIZE, CALLDATACOPY
  , CODESIZE, CODECOPY
  , GASPRICE, EXTCODESIZE, EXTCODECOPY, RETURNDATASIZE, RETURNDATACOPY, EXTCODEHASH

  , BLOCKHASH, COINBASE, TIMESTAMP, NUMBER, PREVRANDAO, GASLIMIT, CHAINID, SELFBALANCE, BASEFEE

  , POP, MLOAD, MSTORE, MSTORE8, SLOAD, SSTORE {- , JUMP, JUMPI -}, PC, MSIZE, GAS {- , JUMPDEST -}
    {- , PUSH -}
  , CREATE, CALL, CALLCODE, RETURN, DELEGATECALL, CREATE2, STATICCALL, REVERT, INVALID, SELFDESTRUCT
  ] <> map DUP [minBound..maxBound]
    <> map SWAP [minBound..maxBound]
    <> map LOG [minBound..maxBound]

genPushOpcode :: Gen (Opcode' a)
genPushOpcode = PUSH <$> genWord256

genJumpyOpcode :: Gen Opcode
genJumpyOpcode = Gen.element
  [ jump
  , jumpi
  , jumpdest
  ]

genLabelledJumpOpcode :: Gen LabelledOpcode
genLabelledJumpOpcode = Gen.choice
  [ JUMP <$> genLabel
  , JUMPI <$> genLabel
  ]

genLabel :: Gen Label
genLabel = Gen.text (Range.singleton 5) (Gen.element "aeiomnr")

-- | Generate a 'Word256' that needs N (1-32) bytes to represent itself, where
-- N is linearly dependent on the generator's size. The value is distributed
-- uniformly in that range.
--
-- For example, size 0 gives a value in the range 0-255, size 1 gives a value
-- in the range 256-65535, and so on.
genWord256 :: Gen Word256
genWord256 = snd <$> genWord256'

-- | Generate a @(n, k)@ pair where @n@ is 0-31 and @k@ is a 'Word256' that is
-- @n + 1@ bytes.
genWord256' :: Gen (Int, Word256)
genWord256' = do
  n <- Gen.integral (Range.linear 0 31)
  let lo = (1 `shift` (8 * n)) - 1
      hi = (1 `shift` (8 * (n + 1))) - 1
  k <- Gen.integral_ (Range.constant lo hi)
  pure (n, k)
