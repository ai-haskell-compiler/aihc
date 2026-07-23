{-# LANGUAGE OverloadedStrings #-}

module Test.Native.Primitive
  ( tests,
  )
where

import Aihc.Grin.Syntax (grinForeignCallSymbol)
import Aihc.Native (nativeRuntimePrimitiveCall, supportedNativePrimitiveNames)
import Data.Text (Text)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

tests :: TestTree
tests =
  testGroup
    "native primitives"
    [ testCase "maps byte-array primitives to the shared runtime ABI" $
        mapM_
          ( \(primitive, symbol) ->
              assertEqual
                ("runtime call for " <> show primitive)
                (Just symbol)
                (grinForeignCallSymbol <$> nativeRuntimePrimitiveCall primitive)
          )
          byteArrayRuntimeSymbols,
      testCase "keeps freeze and thaw representation-preserving" $
        mapM_
          (\primitive -> assertEqual ("runtime call for " <> show primitive) Nothing (nativeRuntimePrimitiveCall primitive))
          ["unsafeFreezeByteArray#", "unsafeThawByteArray#"],
      testCase "accepts the complete byte-array API in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          (map fst byteArrayRuntimeSymbols <> ["unsafeFreezeByteArray#", "unsafeThawByteArray#"]),
      testCase "accepts the Integer arithmetic primitive API" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          integerPrimitiveNames
    ]

byteArrayRuntimeSymbols :: [(Text, Text)]
byteArrayRuntimeSymbols =
  [ ("newByteArray#", "aihc_byte_array_new"),
    ("newPinnedByteArray#", "aihc_byte_array_new_pinned"),
    ("newAlignedPinnedByteArray#", "aihc_byte_array_new_aligned_pinned"),
    ("isMutableByteArrayPinned#", "aihc_byte_array_is_pinned"),
    ("isByteArrayPinned#", "aihc_byte_array_is_pinned"),
    ("byteArrayContents#", "aihc_byte_array_contents"),
    ("mutableByteArrayContents#", "aihc_byte_array_contents"),
    ("shrinkMutableByteArray#", "aihc_byte_array_shrink"),
    ("resizeMutableByteArray#", "aihc_byte_array_resize"),
    ("sizeofByteArray#", "aihc_byte_array_get_size"),
    ("getSizeofMutableByteArray#", "aihc_byte_array_get_size"),
    ("copyAddrToByteArray#", "aihc_byte_array_copy_from_addr"),
    ("indexWordArray#", "aihc_byte_array_index_word"),
    ("readWordArray#", "aihc_byte_array_read_word"),
    ("writeWordArray#", "aihc_byte_array_write_word"),
    ("copyByteArray#", "aihc_byte_array_copy")
  ]

integerPrimitiveNames :: [Text]
integerPrimitiveNames =
  [ "+#",
    "-#",
    "*#",
    "<#",
    "==#",
    "addIntC#",
    "subIntC#",
    "plusWord#",
    "addWordC#",
    "subWordC#",
    "timesWord2#",
    "quotWord#",
    "int2Word#",
    "word2Int#",
    "eqWord#",
    "ltWord#"
  ]
