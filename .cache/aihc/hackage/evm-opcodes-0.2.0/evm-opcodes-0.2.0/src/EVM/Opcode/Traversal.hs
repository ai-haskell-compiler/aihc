{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: EVM.Opcode.Traversal
-- Copyright: 2018-2024 Simon Shine
-- Maintainer: Simon Shine <simon@simonshine.dk>
-- License: MIT
--
-- This module exposes a generic method of traversing 'Opcode''s.

module EVM.Opcode.Traversal
  ( OpcodeMapper(..)
  , mapOpcodeM
  ) where

import EVM.Opcode
import Prelude hiding (EQ, GT, LT)

-- | An 'OpcodeMapper' is a collection of four mapping functions that can
-- map any @'Opcode'' a@ to an @'Opcode'' b@. For each of the three opcodes
-- that are annotated, 'JUMP', 'JUMPI' and 'JUMPDEST', a separate mapping
-- function is specified, and for any other opcode, a general mapping function
-- is specified that falls back to the same opcode of type @'Opcode'' b@.
--
-- See 'EVM.Opcode.Labelled.translate' for an example of usage.
data OpcodeMapper m a b = OpcodeMapper
  { mapOnJump     :: a -> m (Opcode' b)
  , mapOnJumpi    :: a -> m (Opcode' b)
  , mapOnJumpdest :: a -> m (Opcode' b)
  , mapOnOther    :: Opcode' a -> m (Maybe (Opcode' b))
  }

-- | Given an 'OpcodeMapper' and an @'Opcode'' a@, produce @m ('Opcode'' b)@.
mapOpcodeM :: forall m a b. Monad m => OpcodeMapper m a b -> Opcode' a -> m (Opcode' b)
mapOpcodeM mapper opcode = case opcode of
  JUMP a     -> mapOnJump mapper a
  JUMPI a    -> mapOnJumpi mapper a
  JUMPDEST a -> mapOnJumpdest mapper a

  -- 0s: Stop and Arithmetic Operations
  STOP       -> mapOnOther' STOP STOP
  ADD        -> mapOnOther' ADD ADD
  MUL        -> mapOnOther' MUL MUL
  SUB        -> mapOnOther' SUB SUB
  DIV        -> mapOnOther' DIV DIV
  SDIV       -> mapOnOther' SDIV SDIV
  MOD        -> mapOnOther' MOD MOD
  SMOD       -> mapOnOther' SMOD SMOD
  ADDMOD     -> mapOnOther' ADDMOD ADDMOD
  MULMOD     -> mapOnOther' MULMOD MULMOD
  EXP        -> mapOnOther' EXP EXP
  SIGNEXTEND -> mapOnOther' SIGNEXTEND SIGNEXTEND

  -- 10s: Comparison & Bitwise Logic Operations
  LT      -> mapOnOther' LT LT
  GT      -> mapOnOther' GT GT
  SLT     -> mapOnOther' SLT SLT
  SGT     -> mapOnOther' SGT SGT
  EQ      -> mapOnOther' EQ EQ
  ISZERO  -> mapOnOther' ISZERO ISZERO
  AND     -> mapOnOther' AND AND
  OR      -> mapOnOther' OR OR
  XOR     -> mapOnOther' XOR XOR
  NOT     -> mapOnOther' NOT NOT
  BYTE    -> mapOnOther' BYTE BYTE
  SHL     -> mapOnOther' SHL SHL
  SHR     -> mapOnOther' SHR SHR
  SAR     -> mapOnOther' SAR SAR

  -- 20s: KECCAK256
  KECCAK256 -> mapOnOther' KECCAK256 KECCAK256

  -- 30s: Environmental Information
  ADDRESS        -> mapOnOther' ADDRESS ADDRESS
  BALANCE        -> mapOnOther' BALANCE BALANCE
  ORIGIN         -> mapOnOther' ORIGIN ORIGIN
  CALLER         -> mapOnOther' CALLER CALLER
  CALLVALUE      -> mapOnOther' CALLVALUE CALLVALUE
  CALLDATALOAD   -> mapOnOther' CALLDATALOAD CALLDATALOAD
  CALLDATASIZE   -> mapOnOther' CALLDATASIZE CALLDATASIZE
  CALLDATACOPY   -> mapOnOther' CALLDATACOPY CALLDATACOPY
  CODESIZE       -> mapOnOther' CODESIZE CODESIZE
  CODECOPY       -> mapOnOther' CODECOPY CODECOPY
  GASPRICE       -> mapOnOther' GASPRICE GASPRICE
  EXTCODESIZE    -> mapOnOther' EXTCODESIZE EXTCODESIZE
  EXTCODECOPY    -> mapOnOther' EXTCODECOPY EXTCODECOPY
  RETURNDATASIZE -> mapOnOther' RETURNDATASIZE RETURNDATASIZE
  RETURNDATACOPY -> mapOnOther' RETURNDATACOPY RETURNDATACOPY
  EXTCODEHASH    -> mapOnOther' EXTCODEHASH EXTCODEHASH

  -- 40s: Block Information
  BLOCKHASH   -> mapOnOther' BLOCKHASH BLOCKHASH
  COINBASE    -> mapOnOther' COINBASE COINBASE
  TIMESTAMP   -> mapOnOther' TIMESTAMP TIMESTAMP
  NUMBER      -> mapOnOther' NUMBER NUMBER
  PREVRANDAO  -> mapOnOther' PREVRANDAO PREVRANDAO
  GASLIMIT    -> mapOnOther' GASLIMIT GASLIMIT
  CHAINID     -> mapOnOther' CHAINID CHAINID
  SELFBALANCE -> mapOnOther' SELFBALANCE SELFBALANCE
  BASEFEE     -> mapOnOther' BASEFEE BASEFEE

  -- 50s: Stack, Memory, Storage and Flow Operations
  POP       -> mapOnOther' POP POP
  MLOAD     -> mapOnOther' MLOAD MLOAD
  MSTORE    -> mapOnOther' MSTORE MSTORE
  MSTORE8   -> mapOnOther' MSTORE8 MSTORE8
  SLOAD     -> mapOnOther' SLOAD SLOAD
  SSTORE    -> mapOnOther' SSTORE SSTORE
  PC        -> mapOnOther' PC PC
  MSIZE     -> mapOnOther' MSIZE MSIZE
  GAS       -> mapOnOther' GAS GAS

  -- 60s & 70s: Push Operations
  PUSH n    -> mapOnOther' (PUSH n) (PUSH n)

  -- 80s: Duplication Operations (DUP)
  DUP i     -> mapOnOther' (DUP i) (DUP i)

  -- 90s: Exchange operations (SWAP)
  SWAP i    -> mapOnOther' (SWAP i) (SWAP i)

  -- a0s: Logging Operations (LOG)
  LOG i     -> mapOnOther' (LOG i) (LOG i)

  -- f0s: System Operations
  CREATE       -> mapOnOther' CREATE CREATE
  CALL         -> mapOnOther' CALL CALL
  CALLCODE     -> mapOnOther' CALLCODE CALLCODE
  RETURN       -> mapOnOther' RETURN RETURN
  DELEGATECALL -> mapOnOther' DELEGATECALL DELEGATECALL
  CREATE2      -> mapOnOther' CREATE2 CREATE2
  STATICCALL   -> mapOnOther' STATICCALL STATICCALL
  REVERT       -> mapOnOther' REVERT REVERT
  INVALID      -> mapOnOther' INVALID INVALID
  SELFDESTRUCT -> mapOnOther' SELFDESTRUCT SELFDESTRUCT
  where
    mapOnOther' :: Opcode' a -> Opcode' b -> m (Opcode' b)
    mapOnOther' opa opbDefault = do
      res <- mapOnOther mapper opa
      case res of
        Just opb -> return opb
        Nothing -> return opbDefault
