{-# LANGUAGE OverloadedStrings #-}

module OpcodeTest where

import Prelude hiding (LT, EQ, GT)

import           Data.Char (isSpace)
import qualified Data.ByteString as BS
import           Data.DoubleWord (Word256)
import           Data.Foldable (for_)
import           Data.List (permutations)
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Test.Tasty.Hspec
import           Test.Hspec

import EVM.Opcode as Opcode
import EVM.Opcode.Positional as P
import EVM.Opcode.Labelled as L

import OpcodeGenerators

-- Property: Jump-free non-PUSH opcodes have size 1.
hprop_opcodeSize_1 :: Property
hprop_opcodeSize_1 = property $ do
  opcode <- forAll genOpcode'1
  opcodeSize opcode === 1

-- Property: When n is 0-31, PUSH opcodes have size n + 2.
hprop_opcodeSize_PUSH :: Property
hprop_opcodeSize_PUSH = property $ do
  (n, k) <- forAll genWord256'
  opcodeSize (PUSH k) === n + 1 + 1

hprop_opcodeText_for_PUSH_matches :: Property
hprop_opcodeText_for_PUSH_matches = property $ do
  (n, k) <- forAll genWord256'
  let text = opcodeText (PUSH k)
      exp = "push" <> Text.pack (show (n + 1))
      got = Text.takeWhile (not . isSpace) text
  got === exp

hprop_opcodeName_unique :: Property
hprop_opcodeName_unique = property $ do
  opcode1 <- forAll genOpcode
  opcode2 <- forAll genOpcode
  if opcode1 == opcode2
    then opcodeName (opcodeSpec opcode1) === opcodeName (opcodeSpec opcode2)
    else opcodeName (opcodeSpec opcode1) /== opcodeName (opcodeSpec opcode2)

hprop_pack_readOp_inverses :: Property
hprop_pack_readOp_inverses = property $ do
  opcode1 <- forAll genOpcode
  let bytecode = pack [opcode1]

  -- Property: Opcodes are packed to /non-empty/ ByteStrings.
  (c, cs) <- evalMaybe (BS.uncons bytecode)

  -- Property: 'pack' and 'readOp' are inverses.
  opcode2 <- evalMaybe (readOp c cs)
  opcode1 === opcode2

hprop_translate_LabelledOpcode :: Property
hprop_translate_LabelledOpcode = withTests 10000 $ property $ do
  labelledOpcodes <- forAll genLabelledOpcodes

  -- Property: Labelled opcodes, for which valid jumpdests occur, translate.
  positionalOpcodes <- evalEither (L.translate labelledOpcodes)

  -- Property: Translating labels to positions is structure-preserving.
  let pairs = zip labelledOpcodes positionalOpcodes
  fmap Opcode.concrete labelledOpcodes === fmap Opcode.concrete positionalOpcodes

  -- Property: For every positional jump the corresponding index in the translated
  -- bytecode is a JUMPDEST. FIXME: bytestring-0.11.0.0 has `indexMaybe` / `!?`.
  let positions = mapMaybe jumpAnnot positionalOpcodes
  let opcodes = P.translate positionalOpcodes
  let bytecode = Opcode.pack opcodes
  for_ (fromIntegral <$> positions) $ \pos ->
    [ bytecode `BS.index` pos ] === toBytes jumpdest

-- Negative tests to assert that broken labels don't cause infinite recursion

spec_EVM_Opcode_Labelled :: Spec
spec_EVM_Opcode_Labelled = do
  describe "translate" $ do
    it "handles empty lists" $
      L.translate [] `shouldBe` Right []

    it "handles instructions without jumps" $ do
      L.translate [STOP] `shouldBe` Right [STOP]
      L.translate [PUSH 2, PUSH 2, ADD] `shouldBe` Right [PUSH 2, PUSH 2, ADD]

    it "handles empty labels" $
      L.translate [JUMP "", JUMPDEST ""] `shouldBe` Right [JUMP 3, JUMPDEST 3]

    it "handles jumpdests with no pointers to it" $
      L.translate [JUMPDEST "foo"] `shouldBe` Right [JUMPDEST 0]

    it "fails on jumps without destinations" $ do
      L.translate [JUMP "off"] `shouldMissErr` ["off"]
      L.translate [JUMPI "off"] `shouldMissErr` ["off"]
      L.translate [JUMP "a", JUMPI "b"] `shouldMissErr` ["a", "b"]

      for_ (permutations [JUMP "a", JUMPDEST "a", JUMP "b"]) $
        \instructions -> L.translate instructions `shouldMissErr` ["b"]

    it "fails on single duplicate destination" $
      L.translate [JUMPDEST "foo", JUMPDEST "foo"] `shouldDupErr` ["foo"]

    it "fails on duplicate destination in presence of non-duplicate destination" $
      for_ (permutations [JUMPDEST "foo", JUMPDEST "foo", JUMPDEST "bar"]) $
        \instructions -> L.translate instructions `shouldDupErr` ["foo"]

    it "fails on multiple duplicate destinations" $
      let instructions = [JUMPDEST "foo", JUMPDEST "bar", JUMPDEST "foo", JUMPDEST "bar"]
      in L.translate instructions `shouldDupErr` ["bar", "foo"]

    it "fails and reports both jumps without destinations and duplicate destinations" $
      let instructions = [JUMP "foo", JUMPDEST "bar", JUMPDEST "bar"]
      in L.translate instructions `shouldBe` Left (TranslateError ["foo"] ["bar"])

    it "fails and reports multiple jumps without destination and multiple duplicate destinations" $
      let instructions =
            [ JUMP "a"
            , JUMP "b"
            , JUMP "good"
            , JUMPI "c"
            , JUMPI "d"
            , JUMPI "good"
            , JUMPDEST "x"
            , JUMPDEST "x"
            , JUMPDEST "y"
            , JUMPDEST "y"
            , JUMPDEST "good"
            ]
          wildJumps = [ "a", "b", "c", "d" ]
          duplicateDests = [ "x", "y" ]

      in L.translate instructions `shouldBe` Left (TranslateError wildJumps duplicateDests)

spec_Show_for_Opcode :: Spec
spec_Show_for_Opcode =
  describe "show" $ do
    -- 0s: Stop and Arithmetic Operations
    it "shows STOP" $ show' STOP `shouldBe` "STOP"
    it "shows ADD" $ show' ADD `shouldBe` "ADD"
    it "shows MUL" $ show' MUL `shouldBe` "MUL"
    it "shows SUB" $ show' SUB `shouldBe` "SUB"
    it "shows DIV" $ show' DIV `shouldBe` "DIV"
    it "shows SDIV" $ show' SDIV `shouldBe` "SDIV"
    it "shows MOD" $ show' MOD `shouldBe` "MOD"
    it "shows SMOD" $ show' SMOD `shouldBe` "SMOD"
    it "shows ADDMOD" $ show' ADDMOD `shouldBe` "ADDMOD"
    it "shows MULMOD" $ show' MULMOD `shouldBe` "MULMOD"
    it "shows EXP" $ show' EXP `shouldBe` "EXP"
    it "shows SIGNEXTEND" $ show' SIGNEXTEND `shouldBe` "SIGNEXTEND"

    -- 10s: Comparison & Bitwise Logic Operations
    it "shows LT" $ show' LT `shouldBe` "LT"
    it "shows GT" $ show' GT `shouldBe` "GT"
    it "shows SLT" $ show' SLT `shouldBe` "SLT"
    it "shows SGT" $ show' SGT `shouldBe` "SGT"
    it "shows EQ" $ show' EQ `shouldBe` "EQ"
    it "shows ISZERO" $ show' ISZERO `shouldBe` "ISZERO"
    it "shows AND" $ show' AND `shouldBe` "AND"
    it "shows OR" $ show' OR `shouldBe` "OR"
    it "shows XOR" $ show' XOR `shouldBe` "XOR"
    it "shows NOT" $ show' NOT `shouldBe` "NOT"
    it "shows BYTE" $ show' BYTE `shouldBe` "BYTE"
    it "shows SHL" $ show' SHL `shouldBe` "SHL"
    it "shows SHR" $ show' SHR `shouldBe` "SHR"
    it "shows SAR" $ show' SAR `shouldBe` "SAR"

    -- 20s: KECCAK256
    it "shows KECCAK256" $ show' KECCAK256 `shouldBe` "KECCAK256"

    -- 30s: Environmental Information
    it "shows ADDRESS" $ show' ADDRESS `shouldBe` "ADDRESS"
    it "shows BALANCE" $ show' BALANCE `shouldBe` "BALANCE"
    it "shows ORIGIN" $ show' ORIGIN `shouldBe` "ORIGIN"
    it "shows CALLER" $ show' CALLER `shouldBe` "CALLER"
    it "shows CALLVALUE" $ show' CALLVALUE `shouldBe` "CALLVALUE"
    it "shows CALLDATALOAD" $ show' CALLDATALOAD `shouldBe` "CALLDATALOAD"
    it "shows CALLDATASIZE" $ show' CALLDATASIZE `shouldBe` "CALLDATASIZE"
    it "shows CALLDATACOPY" $ show' CALLDATACOPY `shouldBe` "CALLDATACOPY"
    it "shows CODESIZE" $ show' CODESIZE `shouldBe` "CODESIZE"
    it "shows CODECOPY" $ show' CODECOPY `shouldBe` "CODECOPY"
    it "shows GASPRICE" $ show' GASPRICE `shouldBe` "GASPRICE"
    it "shows EXTCODESIZE" $ show' EXTCODESIZE `shouldBe` "EXTCODESIZE"
    it "shows EXTCODECOPY" $ show' EXTCODECOPY `shouldBe` "EXTCODECOPY"
    it "shows RETURNDATASIZE" $ show' RETURNDATASIZE `shouldBe` "RETURNDATASIZE"
    it "shows RETURNDATACOPY" $ show' RETURNDATACOPY `shouldBe` "RETURNDATACOPY"
    it "shows EXTCODEHASH" $ show' EXTCODEHASH `shouldBe` "EXTCODEHASH"

    -- 40s: Block Information
    it "shows BLOCKHASH" $ show' BLOCKHASH `shouldBe` "BLOCKHASH"
    it "shows COINBASE" $ show' COINBASE `shouldBe` "COINBASE"
    it "shows TIMESTAMP" $ show' TIMESTAMP `shouldBe` "TIMESTAMP"
    it "shows NUMBER" $ show' NUMBER `shouldBe` "NUMBER"
    it "shows PREVRANDAO" $ show' PREVRANDAO `shouldBe` "PREVRANDAO"
    it "shows GASLIMIT" $ show' GASLIMIT `shouldBe` "GASLIMIT"
    it "shows CHAINID" $ show' CHAINID `shouldBe` "CHAINID"
    it "shows SELFBALANCE" $ show' SELFBALANCE `shouldBe` "SELFBALANCE"
    it "shows BASEFEE" $ show' BASEFEE `shouldBe` "BASEFEE"

    -- 50s: Stack, Memory, Storage and Flow Operations
    it "shows POP" $ show' POP `shouldBe` "POP"
    it "shows MLOAD" $ show' MLOAD `shouldBe` "MLOAD"
    it "shows MSTORE" $ show' MSTORE `shouldBe` "MSTORE"
    it "shows MSTORE8" $ show' MSTORE8 `shouldBe` "MSTORE8"
    it "shows SLOAD" $ show' SLOAD `shouldBe` "SLOAD"
    it "shows SSTORE" $ show' SSTORE `shouldBe` "SSTORE"
    it "shows JUMP" $ show' (JUMP ()) `shouldBe` "JUMP ()"
    it "shows JUMPI" $ show' (JUMPI ()) `shouldBe` "JUMPI ()"
    it "shows PC" $ show' PC `shouldBe` "PC"
    it "shows MSIZE" $ show' MSIZE `shouldBe` "MSIZE"
    it "shows GAS" $ show' GAS `shouldBe` "GAS"
    it "shows JUMPDEST" $ show' (JUMPDEST ()) `shouldBe` "JUMPDEST ()"

    -- 60s & 70s: Push Operations
    for_ [0, 255, 256, 65535, 65536] $ \i ->
      it ("shows PUSH " <> show i) $ show' (PUSH i) `shouldBe` "PUSH " <> show i

    -- 80s: Duplication Operations (DUP)
    for_ [minBound..maxBound] $ \nth -> do
      let i = fromEnum nth + 1
      it ("shows DUP" <> show i) $ show' (DUP nth) `shouldBe` ("DUP" <> show i)

    -- 90s: Exchange operations (SWAP)
    for_ [minBound..maxBound] $ \nth -> do
      let i = fromEnum nth + 1
      it ("shows DUP" <> show i) $ show' (SWAP nth) `shouldBe` ("SWAP" <> show i)

    -- a0s: Logging Operations (LOG)
    for_ [minBound..maxBound] $ \nth -> do
      let i = fromEnum nth
      it ("shows DUP" <> show i) $ show' (LOG nth) `shouldBe` ("LOG" <> show i)

    -- f0s: System Operations
    it "shows CREATE" $ show' CREATE `shouldBe` "CREATE"
    it "shows CALL" $ show' CALL `shouldBe` "CALL"
    it "shows CALLCODE" $ show' CALLCODE `shouldBe` "CALLCODE"
    it "shows RETURN" $ show' RETURN `shouldBe` "RETURN"
    it "shows DELEGATECALL" $ show' DELEGATECALL `shouldBe` "DELEGATECALL"
    it "shows CREATE2" $ show' CREATE2 `shouldBe` "CREATE2"
    it "shows STATICCALL" $ show' STATICCALL `shouldBe` "STATICCALL"
    it "shows REVERT" $ show' REVERT `shouldBe` "REVERT"
    it "shows INVALID" $ show' INVALID `shouldBe` "INVALID"
    it "shows SELFDESTRUCT" $ show' SELFDESTRUCT `shouldBe` "SELFDESTRUCT"
  where
    show' :: Opcode -> String
    show' = show

shouldMissErr :: (Show b, Eq b) => Either TranslateError b -> [Label] -> Expectation
shouldMissErr x y = x `shouldBe` Left (TranslateError y [])

shouldDupErr :: (Show b, Eq b) => Either TranslateError b -> [Label] -> Expectation
shouldDupErr x y = x `shouldBe` Left (TranslateError [] y)
