{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module: EVM.Opcode.Internal
-- Copyright: 2018-2024 Simon Shine
-- Maintainer: Simon Shine <simon@simonshine.dk>
-- License: MIT
--
-- This module exposes the 'Opcode'' abstract type.

module EVM.Opcode.Internal where

import Control.Monad (void)
import Data.Bits (shift)
import Data.DoubleWord (Word256)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Prelude hiding (EQ, GT, LT)

-- | An 'Opcode'' is an Ethereum VM Opcode with parameterised jumps.
--
-- For a plain opcode using the basic EVM stack-based jumps, use
-- 'EVM.Opcode.Opcode' instead.
--
-- This type is used for defining and translating from annotated opcodes, e.g.
-- with labelled jumps.
data Opcode' j
  -- 0s: Stop and Arithmetic Operations
  = STOP              -- ^ 0x00
  | ADD               -- ^ 0x01
  | MUL               -- ^ 0x02
  | SUB               -- ^ 0x03
  | DIV               -- ^ 0x04
  | SDIV              -- ^ 0x05
  | MOD               -- ^ 0x06
  | SMOD              -- ^ 0x07
  | ADDMOD            -- ^ 0x08
  | MULMOD            -- ^ 0x09
  | EXP               -- ^ 0x0a
  | SIGNEXTEND        -- ^ 0x0b

  -- 10s: Comparison & Bitwise Logic Operations
  | LT                -- ^ 0x10
  | GT                -- ^ 0x11
  | SLT               -- ^ 0x12
  | SGT               -- ^ 0x13
  | EQ                -- ^ 0x14
  | ISZERO            -- ^ 0x15
  | AND               -- ^ 0x16
  | OR                -- ^ 0x17
  | XOR               -- ^ 0x18
  | NOT               -- ^ 0x19
  | BYTE              -- ^ 0x1a
  | SHL               -- ^ 0x1b, https://eips.ethereum.org/EIPS/eip-145
  | SHR               -- ^ 0x1c, https://eips.ethereum.org/EIPS/eip-145
  | SAR               -- ^ 0x1d, https://eips.ethereum.org/EIPS/eip-145

  -- 20s: KECCAK256
  | KECCAK256         -- ^ 0x20, https://eips.ethereum.org/EIPS/eip-1803

  -- 30s: Environmental Information
  | ADDRESS           -- ^ 0x30
  | BALANCE           -- ^ 0x31
  | ORIGIN            -- ^ 0x32
  | CALLER            -- ^ 0x33
  | CALLVALUE         -- ^ 0x34
  | CALLDATALOAD      -- ^ 0x35
  | CALLDATASIZE      -- ^ 0x36
  | CALLDATACOPY      -- ^ 0x37
  | CODESIZE          -- ^ 0x38
  | CODECOPY          -- ^ 0x39
  | GASPRICE          -- ^ 0x3a
  | EXTCODESIZE       -- ^ 0x3b
  | EXTCODECOPY       -- ^ 0x3c
  | RETURNDATASIZE    -- ^ 0x3d, https://eips.ethereum.org/EIPS/eip-211
  | RETURNDATACOPY    -- ^ 0x3e, https://eips.ethereum.org/EIPS/eip-211
  | EXTCODEHASH       -- ^ 0x3f, https://eips.ethereum.org/EIPS/eip-1052

  -- 40s: Block Information
  | BLOCKHASH         -- ^ 0x40
  | COINBASE          -- ^ 0x41
  | TIMESTAMP         -- ^ 0x42
  | NUMBER            -- ^ 0x43
  | PREVRANDAO        -- ^ 0x44, https://eips.ethereum.org/EIPS/eip-4399
  | GASLIMIT          -- ^ 0x45
  | CHAINID           -- ^ 0x46, https://eips.ethereum.org/EIPS/eip-1344
  | SELFBALANCE       -- ^ 0x47, https://eips.ethereum.org/EIPS/eip-1884
  | BASEFEE           -- ^ 0x48, https://eips.ethereum.org/EIPS/eip-3198

  -- 50s: Stack, Memory, Storage and Flow Operations
  | POP               -- ^ 0x50
  | MLOAD             -- ^ 0x51
  | MSTORE            -- ^ 0x52
  | MSTORE8           -- ^ 0x53
  | SLOAD             -- ^ 0x54
  | SSTORE            -- ^ 0x55
  | JUMP j            -- ^ 0x56
  | JUMPI j           -- ^ 0x57
  | PC                -- ^ 0x58
  | MSIZE             -- ^ 0x59
  | GAS               -- ^ 0x5a
  | JUMPDEST j        -- ^ 0x5b

  -- 60s & 70s: Push Operations
  | PUSH !Word256     -- ^ 0x60 - 0x7f (PUSH1 - PUSH32)

  -- 80s: Duplication Operations (DUP)
  | DUP !Ord16         -- ^ 0x80 - 0x8f ('DUP1' - 'DUP16')

  -- 90s: Exchange operations (SWAP)
  | SWAP !Ord16        -- ^ 0x90 - 0x9f ('SWAP1' - 'SWAP16')

  -- a0s: Logging Operations
  | LOG !Ord5         -- ^ 0xa0 - 0xa4 ('LOG0' - 'LOG4')

  -- f0s: System Operations
  | CREATE            -- ^ 0xf0
  | CALL              -- ^ 0xf1
  | CALLCODE          -- ^ 0xf2
  | RETURN            -- ^ 0xf3
  | DELEGATECALL      -- ^ 0xf4, https://eips.ethereum.org/EIPS/eip-7
  | CREATE2           -- ^ 0xf5, https://eips.ethereum.org/EIPS/eip-1014
  | STATICCALL        -- ^ 0xfa
  | REVERT            -- ^ 0xfd, https://eips.ethereum.org/EIPS/eip-140
  | INVALID           -- ^ 0xfe, https://eips.ethereum.org/EIPS/eip-141
  | SELFDESTRUCT      -- ^ 0xff, https://eips.ethereum.org/EIPS/eip-6
  deriving (Eq, Ord, Functor)

-- | Convert any @'Opcode'' a@ into an @'Opcode'' ()@.
concrete :: Opcode' a -> Opcode' ()
concrete = void

-- | Extract the @a@ from a @'JUMP' a@ or a @'JUMPI' a@.
jumpAnnot :: Opcode' a -> Maybe a
jumpAnnot = \case
  JUMP a -> Just a
  JUMPI a -> Just a
  _ -> Nothing

-- | Extract the @a@ from a @'JUMPDEST' a@.
jumpdestAnnot :: Opcode' a -> Maybe a
jumpdestAnnot = \case
  JUMPDEST a -> Just a
  _ -> Nothing

-- | Convert an 'Ord5' or an 'Ord16' to a 'Word8'.
toWord8 :: Enum e => e -> Word8
toWord8 = fromIntegral . fromEnum

-- | Convert a 'Word8' to an 'Ord5' or an 'Ord16'.
fromWord8 :: Enum e => Word8 -> e
fromWord8 = toEnum . fromIntegral

-- | Convenience type of cardinality 5 for 'LOG'.
data Ord5
  = Ord5_0
  | Ord5_1
  | Ord5_2
  | Ord5_3
  | Ord5_4
  deriving (Eq, Ord, Enum, Bounded)

-- | Convenience type of cardinality 16 for 'DUP' and 'SWAP'.
data Ord16
  = Ord16_1
  | Ord16_2
  | Ord16_3
  | Ord16_4
  | Ord16_5
  | Ord16_6
  | Ord16_7
  | Ord16_8
  | Ord16_9
  | Ord16_10
  | Ord16_11
  | Ord16_12
  | Ord16_13
  | Ord16_14
  | Ord16_15
  | Ord16_16
  deriving (Eq, Ord, Enum, Bounded)

-- | An 'OpcodeSpec' for a given 'EVM.Opcode.Opcode' contains the numeric
-- encoding of the opcode, the number of items that this opcode removes
-- from the stack (α), and the number of items added to the stack (δ).
-- These values are documented in the Ethereum Yellow Paper.
--
-- Examples of 'OpcodeSpec's:
--
-- > --         Hex  α δ
-- > OpcodeSpec 0x01 2 1 "add"
-- > OpcodeSpec 0x60 0 1 "push1 255"
-- > OpcodeSpec 0x61 0 1 "push2 256"
data OpcodeSpec = OpcodeSpec
  { opcodeEncoding :: !Word8 -- ^ Numeric encoding of opcode
  , opcodeAlpha    :: !Word8 -- ^ Number of items opcode places on stack (α)
  , opcodeDelta    :: !Word8 -- ^ Number of items opcode removes from stack (δ)
  , opcodeName     :: !Text  -- ^ Printable name for opcode, e.g. @"add"@
  } deriving (Eq, Show)

-- | Given an 'Opcode'', produce its 'OpcodeSpec'.
--
-- For 'DUP', 'SWAP' and 'LOG' this depends on the specific variant, and for
-- 'PUSH' it depends on the size of the constant being pushed.
opcodeSpec :: Opcode' j -> OpcodeSpec
opcodeSpec opcode = case opcode of
  -- 0s: Stop and Arithmetic Operations
  --                   Hex  α δ
  STOP       -> OpcodeSpec 0x00 0 0 "stop"
  ADD        -> OpcodeSpec 0x01 2 1 "add"
  MUL        -> OpcodeSpec 0x02 2 1 "mul"
  SUB        -> OpcodeSpec 0x03 2 1 "sub"
  DIV        -> OpcodeSpec 0x04 2 1 "div"
  SDIV       -> OpcodeSpec 0x05 2 1 "sdiv"
  MOD        -> OpcodeSpec 0x06 2 1 "mod"
  SMOD       -> OpcodeSpec 0x07 2 1 "smod"
  ADDMOD     -> OpcodeSpec 0x08 3 1 "addmod"
  MULMOD     -> OpcodeSpec 0x09 3 1 "mulmod"
  EXP        -> OpcodeSpec 0x0a 2 1 "exp"
  SIGNEXTEND -> OpcodeSpec 0x0b 2 1 "signextend"

  -- 10s: Comparison & Bitwise Logic Operations
  --                Hex  α δ
  LT         -> OpcodeSpec 0x10 2 1 "lt"
  GT         -> OpcodeSpec 0x11 2 1 "gt"
  SLT        -> OpcodeSpec 0x12 2 1 "slt"
  SGT        -> OpcodeSpec 0x13 2 1 "sgt"
  EQ         -> OpcodeSpec 0x14 2 1 "eq"
  ISZERO     -> OpcodeSpec 0x15 1 1 "iszero"
  AND        -> OpcodeSpec 0x16 2 1 "and"
  OR         -> OpcodeSpec 0x17 2 1 "or"
  XOR        -> OpcodeSpec 0x18 2 1 "xor"
  NOT        -> OpcodeSpec 0x19 1 1 "not"
  BYTE       -> OpcodeSpec 0x1a 2 1 "byte"
  SHL        -> OpcodeSpec 0x1b 2 1 "shl"
  SHR        -> OpcodeSpec 0x1c 2 1 "shr"
  SAR        -> OpcodeSpec 0x1d 2 1 "sar"

  -- 20s: KECCAK256
  --               Hex  α δ
  KECCAK256  -> OpcodeSpec 0x20 2 1 "keccak256"

  -- 30s: Environmental Information
  --     Opcode            Hex  α δ
  ADDRESS        -> OpcodeSpec 0x30 0 1 "address"
  BALANCE        -> OpcodeSpec 0x31 1 1 "balance"
  ORIGIN         -> OpcodeSpec 0x32 0 1 "origin"
  CALLER         -> OpcodeSpec 0x33 0 1 "caller"
  CALLVALUE      -> OpcodeSpec 0x34 0 1 "callvalue"
  CALLDATALOAD   -> OpcodeSpec 0x35 1 1 "calldataload"
  CALLDATASIZE   -> OpcodeSpec 0x36 0 1 "calldatasize"
  CALLDATACOPY   -> OpcodeSpec 0x37 3 0 "calldatacopy"
  CODESIZE       -> OpcodeSpec 0x38 0 1 "codesize"
  CODECOPY       -> OpcodeSpec 0x39 3 0 "codecopy"
  GASPRICE       -> OpcodeSpec 0x3a 0 1 "gasprice"
  EXTCODESIZE    -> OpcodeSpec 0x3b 1 1 "extcodesize"
  EXTCODECOPY    -> OpcodeSpec 0x3c 4 0 "extcodecopy"
  RETURNDATASIZE -> OpcodeSpec 0x3d 0 1 "returndatasize"
  RETURNDATACOPY -> OpcodeSpec 0x3e 3 0 "returndatacopy"
  EXTCODEHASH    -> OpcodeSpec 0x3f 1 1 "extcodehash"

  -- 40s: Block Information
  --                    Hex  α δ
  BLOCKHASH      -> OpcodeSpec 0x40 1 1 "blockhash"
  COINBASE       -> OpcodeSpec 0x41 0 1 "coinbase"
  TIMESTAMP      -> OpcodeSpec 0x42 0 1 "timestamp"
  NUMBER         -> OpcodeSpec 0x43 0 1 "number"
  PREVRANDAO     -> OpcodeSpec 0x44 0 1 "prevrandao"
  GASLIMIT       -> OpcodeSpec 0x45 0 1 "gaslimit"
  CHAINID        -> OpcodeSpec 0x46 0 1 "chainid"
  SELFBALANCE    -> OpcodeSpec 0x47 0 1 "selfbalance"
  BASEFEE        -> OpcodeSpec 0x48 0 1 "basefee"

  -- 50s: Stack, Memory, Storage and Flow Operations
  --                    Hex  α δ
  POP            -> OpcodeSpec 0x50 1 0 "pop"
  MLOAD          -> OpcodeSpec 0x51 1 1 "mload"
  MSTORE         -> OpcodeSpec 0x52 2 0 "mstore"
  MSTORE8        -> OpcodeSpec 0x53 2 0 "mstore8"
  SLOAD          -> OpcodeSpec 0x54 1 1 "sload"
  SSTORE         -> OpcodeSpec 0x55 2 0 "sstore"
  JUMP{}         -> OpcodeSpec 0x56 1 0 "jump"
  JUMPI{}        -> OpcodeSpec 0x57 2 0 "jumpi"
  PC             -> OpcodeSpec 0x58 0 1 "pc"
  MSIZE          -> OpcodeSpec 0x59 0 1 "msize"
  GAS            -> OpcodeSpec 0x5a 0 1 "gas"
  JUMPDEST{}     -> OpcodeSpec 0x5b 0 0 "jumpdest"

  -- 60s & 70s: Push Operations
  PUSH n ->
    let (pushHex, pushConst) = push' n
    in OpcodeSpec { opcodeEncoding = pushHex
                  , opcodeAlpha    = 0
                  , opcodeDelta    = 1
                  , opcodeName     = Text.concat
                    [ "push"
                    , Text.pack (show (List.length pushConst))
                    , " "
                    , Text.pack (show n) ]
                  }

  -- 80s: Duplication Operations (DUP)
  DUP i ->
    let wi = toWord8 i
    in OpcodeSpec { opcodeEncoding = 0x80 + wi
                  , opcodeAlpha    = wi + 1
                  , opcodeDelta    = wi + 2
                  , opcodeName     = "dup" <> Text.pack (show (wi + 1))
                  }

  -- 90s: Exchange operations (SWAP)
  SWAP i ->
    let wi = toWord8 i
    in OpcodeSpec { opcodeEncoding = 0x90 + wi
                  , opcodeAlpha    = wi + 1
                  , opcodeDelta    = wi + 1
                  , opcodeName     = "swap" <> Text.pack (show (wi + 1))
                  }

  -- a0s: Logging Operations (LOG)
  LOG i ->
    let wi = toWord8 i
    in OpcodeSpec { opcodeEncoding = 0xa0 + wi
                  , opcodeAlpha    = wi + 2
                  , opcodeDelta    = 0
                  , opcodeName     = "log" <> Text.pack (show wi)
                  }

  -- f0s: System Operations
  --                      Hex  α δ
  CREATE       -> OpcodeSpec 0xf0 3 1 "create"
  CALL         -> OpcodeSpec 0xf1 7 1 "call"
  CALLCODE     -> OpcodeSpec 0xf2 7 1 "callcode"
  RETURN       -> OpcodeSpec 0xf3 2 0 "return"
  DELEGATECALL -> OpcodeSpec 0xf4 6 1 "delegatecall"
  CREATE2      -> OpcodeSpec 0xf5 4 1 "create2"
  STATICCALL   -> OpcodeSpec 0xfa 6 1 "staticcall"
  REVERT       -> OpcodeSpec 0xfd 2 0 "revert"
  INVALID      -> OpcodeSpec 0xfe 0 0 "invalid" -- α, δ are ∅
  SELFDESTRUCT -> OpcodeSpec 0xff 1 0 "selfdestruct"

instance Show a => Show (Opcode' a) where
  show (PUSH n) = "PUSH " <> show n
  show (JUMP j) = "JUMP " <> show j
  show (JUMPI j) = "JUMPI " <> show j
  show (JUMPDEST j) = "JUMPDEST " <> show j
  show opcode = Text.unpack (Text.toUpper (opcodeName (opcodeSpec opcode)))

-- | Convert the constant argument of a 'PUSH' to the opcode encoding
-- (0x60--0x7f) and its constant split into 'Word8' segments.
push' :: Word256 -> (Word8, [Word8])
push' i | i < 256 = (0x60, [fromIntegral i])
push' i = (opcode + 1, arg <> [fromIntegral i])
  where
    (opcode, arg) = push' (i `shift` (-8))

-- | Provide both 'SHA3' and 'KECCAK256' patterns for backwards-compatibility
-- 
-- Note that 'KECCAK256' replaces 'SHA3' because it is more precise.
pattern SHA3 :: forall j. Opcode' j
pattern SHA3 = KECCAK256

-- | Use 'DUP1' instead of @'DUP' 'Ord16_1'@.
pattern DUP1 :: forall j. Opcode' j
pattern DUP1 = DUP Ord16_1

-- | Use 'DUP2' instead of @'DUP' 'Ord16_2'@.
pattern DUP2 :: forall j. Opcode' j
pattern DUP2 = DUP Ord16_2

-- | Use 'DUP3' instead of @'DUP' 'Ord16_3'@.
pattern DUP3 :: forall j. Opcode' j
pattern DUP3 = DUP Ord16_3

-- | Use 'DUP4' instead of @'DUP' 'Ord16_4'@.
pattern DUP4 :: forall j. Opcode' j
pattern DUP4 = DUP Ord16_4

-- | Use 'DUP5' instead of @'DUP' 'Ord16_5'@.
pattern DUP5 :: forall j. Opcode' j
pattern DUP5 = DUP Ord16_5

-- | Use 'DUP6' instead of @'DUP' 'Ord16_6'@.
pattern DUP6 :: forall j. Opcode' j
pattern DUP6 = DUP Ord16_6

-- | Use 'DUP7' instead of @'DUP' 'Ord16_7'@.
pattern DUP7 :: forall j. Opcode' j
pattern DUP7 = DUP Ord16_7

-- | Use 'DUP8' instead of @'DUP' 'Ord16_8'@.
pattern DUP8 :: forall j. Opcode' j
pattern DUP8 = DUP Ord16_8

-- | Use 'DUP9' instead of @'DUP' 'Ord16_9'@.
pattern DUP9 :: forall j. Opcode' j
pattern DUP9 = DUP Ord16_9

-- | Use 'DUP10' instead of @'DUP' 'Ord16_10'@.
pattern DUP10 :: forall j. Opcode' j
pattern DUP10 = DUP Ord16_10

-- | Use 'DUP11' instead of @'DUP' 'Ord16_11'@.
pattern DUP11 :: forall j. Opcode' j
pattern DUP11 = DUP Ord16_11

-- | Use 'DUP12' instead of @'DUP' 'Ord16_12'@.
pattern DUP12 :: forall j. Opcode' j
pattern DUP12 = DUP Ord16_12

-- | Use 'DUP13' instead of @'DUP' 'Ord16_13'@.
pattern DUP13 :: forall j. Opcode' j
pattern DUP13 = DUP Ord16_13

-- | Use 'DUP14' instead of @'DUP' 'Ord16_14'@.
pattern DUP14 :: forall j. Opcode' j
pattern DUP14 = DUP Ord16_14

-- | Use 'DUP15' instead of @'DUP' 'Ord16_15'@.
pattern DUP15 :: forall j. Opcode' j
pattern DUP15 = DUP Ord16_15

-- | Use 'DUP16' instead of @'DUP' 'Ord16_16'@.
pattern DUP16 :: forall j. Opcode' j
pattern DUP16 = DUP Ord16_16

-- | Use 'SWAP1' instead of @'SWAP' 'Ord16_1'@, etc.
pattern SWAP1 :: forall j. Opcode' j
pattern SWAP1 = SWAP Ord16_1

-- | Use 'SWAP2' instead of @'SWAP' 'Ord16_2'@, etc.
pattern SWAP2 :: forall j. Opcode' j
pattern SWAP2 = SWAP Ord16_2

-- | Use 'SWAP3' instead of @'SWAP' 'Ord16_3'@, etc.
pattern SWAP3 :: forall j. Opcode' j
pattern SWAP3 = SWAP Ord16_3

-- | Use 'SWAP4' instead of @'SWAP' 'Ord16_4'@, etc.
pattern SWAP4 :: forall j. Opcode' j
pattern SWAP4 = SWAP Ord16_4

-- | Use 'SWAP5' instead of @'SWAP' 'Ord16_5'@, etc.
pattern SWAP5 :: forall j. Opcode' j
pattern SWAP5 = SWAP Ord16_5

-- | Use 'SWAP6' instead of @'SWAP' 'Ord16_6'@, etc.
pattern SWAP6 :: forall j. Opcode' j
pattern SWAP6 = SWAP Ord16_6

-- | Use 'SWAP7' instead of @'SWAP' 'Ord16_7'@, etc.
pattern SWAP7 :: forall j. Opcode' j
pattern SWAP7 = SWAP Ord16_7

-- | Use 'SWAP8' instead of @'SWAP' 'Ord16_8'@, etc.
pattern SWAP8 :: forall j. Opcode' j
pattern SWAP8 = SWAP Ord16_8

-- | Use 'SWAP9' instead of @'SWAP' 'Ord16_9'@, etc.
pattern SWAP9 :: forall j. Opcode' j
pattern SWAP9 = SWAP Ord16_9

-- | Use 'SWAP10' instead of @'SWAP' 'Ord16_10'@, etc.
pattern SWAP10 :: forall j. Opcode' j
pattern SWAP10 = SWAP Ord16_10

-- | Use 'SWAP11' instead of @'SWAP' 'Ord16_11'@, etc.
pattern SWAP11 :: forall j. Opcode' j
pattern SWAP11 = SWAP Ord16_11

-- | Use 'SWAP12' instead of @'SWAP' 'Ord16_12'@, etc.
pattern SWAP12 :: forall j. Opcode' j
pattern SWAP12 = SWAP Ord16_12

-- | Use 'SWAP13' instead of @'SWAP' 'Ord16_13'@, etc.
pattern SWAP13 :: forall j. Opcode' j
pattern SWAP13 = SWAP Ord16_13

-- | Use 'SWAP14' instead of @'SWAP' 'Ord16_14'@, etc.
pattern SWAP14 :: forall j. Opcode' j
pattern SWAP14 = SWAP Ord16_14

-- | Use 'SWAP15' instead of @'SWAP' 'Ord16_15'@, etc.
pattern SWAP15 :: forall j. Opcode' j
pattern SWAP15 = SWAP Ord16_15

-- | Use 'SWAP16' instead of @'SWAP' 'Ord16_16'@, etc.
pattern SWAP16 :: forall j. Opcode' j
pattern SWAP16 = SWAP Ord16_16

-- | Use 'LOG0' instead of @'LOG' 'Ord5_0'@.
pattern LOG0 :: forall j. Opcode' j
pattern LOG0 = LOG Ord5_0

-- | Use 'LOG1' instead of @'LOG' 'Ord5_1'@.
pattern LOG1 :: forall j. Opcode' j
pattern LOG1 = LOG Ord5_1

-- | Use 'LOG2' instead of @'LOG' 'Ord5_2'@.
pattern LOG2 :: forall j. Opcode' j
pattern LOG2 = LOG Ord5_2

-- | Use 'LOG3' instead of @'LOG' 'Ord5_3'@.
pattern LOG3 :: forall j. Opcode' j
pattern LOG3 = LOG Ord5_3

-- | Use 'LOG4' instead of @'LOG' 'Ord5_4'@.
pattern LOG4 :: forall j. Opcode' j
pattern LOG4 = LOG Ord5_4
