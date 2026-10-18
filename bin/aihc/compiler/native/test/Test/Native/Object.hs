{-# LANGUAGE OverloadedStrings #-}

-- | The names an object file publishes.
module Test.Native.Object (tests) where

import Aihc.Amd64.Assemble qualified as Amd64
import Aihc.Arm64.Assemble qualified as Arm64
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

-- | A name this object defines and does not export. Only the relocation
-- beside it names it, so the object writer is free to number it instead.
privateName :: Text
privateName = "aihc_f_aihc_entry___sfIntegralCSUSeconds__uthunk"

-- | A name another object matches against, which must survive intact.
exportedName :: Text
exportedName = "aihc_base_4_21_2_0_hash_Foreign_C_Types_CSUSeconds"

-- | A name this object leaves to the linker, which must survive intact.
undefinedName :: Text
undefinedName = "aihc_base_4_21_2_0_hash_GHC_Real_toInteger"

-- | An object that defines both names and points an absolute relocation at
-- each. 'Absolute64' is the kind the assembler cannot patch in place, so
-- every target here reaches the symbol table.
assembled :: (Show error) => Either error BL.ByteString -> IO ByteString
assembled result =
  case result of
    Left failure -> assertFailure ("the object failed to assemble: " <> show failure)
    Right bytes -> pure (BL.toStrict bytes)

machO :: IO ByteString
machO =
  assembled . Arm64.assembleMachO $
    [ Arm64.arm64Section Arm64.DataSection,
      Arm64.arm64Label privateName,
      Arm64.arm64Quad 0,
      Arm64.arm64Global exportedName,
      Arm64.arm64Label exportedName,
      Arm64.arm64QuadSymbol privateName,
      Arm64.arm64QuadSymbol undefinedName
    ]

elf :: IO ByteString
elf =
  assembled . Amd64.assembleElf $
    [ Amd64.amd64Section Amd64.DataSection,
      Amd64.amd64Label privateName,
      Amd64.amd64Quad 0,
      Amd64.amd64Global exportedName,
      Amd64.amd64Label exportedName,
      Amd64.amd64QuadSymbol privateName,
      Amd64.amd64QuadSymbol undefinedName
    ]

contains :: Text -> ByteString -> Bool
contains name object = Text.encodeUtf8 name `BS.isInfixOf` object

format :: String -> IO ByteString -> TestTree
format name object =
  testGroup
    name
    [ testCase "a private name is not written" $ do
        bytes <- object
        assertBool "the private name is still in the object" (not (contains privateName bytes)),
      testCase "an exported name is written" $ do
        bytes <- object
        assertBool "the exported name is missing" (contains exportedName bytes),
      testCase "an undefined name is written" $ do
        bytes <- object
        assertBool "the undefined name is missing" (contains undefinedName bytes)
    ]

-- | Filler code that a jump goes over.
filler :: Int -> Amd64.Amd64Statement
filler count = Amd64.amd64Bytes (BS.replicate count 0xcc)

local :: Int -> Amd64.Name
local identifier = Amd64.LocalName identifier "jump"

jumpTo :: Int -> Amd64.Amd64Statement
jumpTo = Amd64.amd64Instruction . Amd64.AmdJmp . Amd64.Amd64JumpLabel . local

-- | Jumps at the limits of a signed byte displacement. The outer jump of
-- the last pair reaches its label only when the inner jump is short too.
jumps :: IO ByteString
jumps =
  assembled . Amd64.assembleElf $
    [ Amd64.amd64Section Amd64.TextSection,
      jumpTo 0,
      filler 127,
      Amd64.Amd64Label (local 0),
      Amd64.amd64Instruction (Amd64.AmdJcc Amd64.AmdEqual (local 1)),
      filler 128,
      Amd64.Amd64Label (local 1),
      Amd64.Amd64Label (local 2),
      filler 126,
      jumpTo 2,
      jumpTo 3,
      jumpTo 4,
      filler 125,
      Amd64.Amd64Label (local 4),
      Amd64.Amd64Label (local 3),
      Amd64.amd64Instruction Amd64.AmdRet
    ]

-- | The jumps a seal makes short, and those it keeps long.
shortJumps :: TestTree
shortJumps =
  testGroup
    "short jumps"
    [ check "a forward jump of 127 bytes is short" (BS.pack [0xeb, 0x7f] <> BS.replicate 127 0xcc <> BS.pack [0x0f, 0x84]),
      check "a backward jump of 128 bytes is short" (BS.replicate 126 0xcc <> BS.pack [0xeb, 0x80]),
      check "a jump over a short jump is short" (BS.pack [0xeb, 0x80, 0xeb, 0x7f, 0xeb, 0x7d] <> BS.replicate 125 0xcc <> BS.pack [0xc3])
    ]
  where
    check name expected = testCase name $ do
      bytes <- jumps
      assertBool "the object does not hold the expected code" (expected `BS.isInfixOf` bytes)

tests :: TestTree
tests = testGroup "object symbols" [format "mach-o" machO, format "elf" elf, shortJumps]
