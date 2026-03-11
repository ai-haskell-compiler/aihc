-- |
-- Module: EVM.Opcode.Positional
-- Copyright: 2018-2024 Simon Shine
-- Maintainer: Simon Shine <simon@simonshine.dk>
-- License: MIT
--
-- This module exposes the `PositionalOpcode` type for expressing Ethereum
-- VM opcodes where jumps and jumpdests are annotated with the byte position
-- of the translated opcode.
--
-- This representation is useful for when generating code that refers to the
-- size of itself or other chunks of code.  E.g. the CODECOPY segment of an
-- Ethereum contract must refer to the size of the code being copied, and
-- determining the size of a jump is trivial when it's annotated with the
-- destination address.

module EVM.Opcode.Positional
  ( Position
  , PositionalOpcode
  , translate
  , jumpSize
  ) where

import EVM.Opcode (Opcode, Opcode' (..), concrete, jump, jumpdest, jumpi, opcodeSize)

-- | The position of an Opcode.
type Position = Word

-- | A 'PositionalOpcode' has byte positions annotated at 'JUMP', 'JUMPI'
-- and 'JUMPDEST'; on 'JUMP' and 'JUMPI' the positions denote where they
-- jump to, and on 'JUMPDEST' they denote the position of the opcode itself.
type PositionalOpcode = Opcode' Position

-- | Translate a 'PositionalOpcode' into an 'Opcode' by converting the position
-- into a 'PUSH' instruction.
translate :: [PositionalOpcode] -> [Opcode]
translate = concatMap inline
  where
    inline :: PositionalOpcode -> [Opcode]
    inline (JUMP addr) = [ PUSH (fromIntegral addr), jump ]
    inline (JUMPI addr) = [ PUSH (fromIntegral addr), jumpi ]
    inline (JUMPDEST _) = [ jumpdest ]
    inline opcode = [ concrete opcode ]

-- | The size of a jump to some absolute position.
jumpSize :: Num i => Position -> i
jumpSize pos = opcodeSize (PUSH (fromIntegral pos)) + opcodeSize jump
