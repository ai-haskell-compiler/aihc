{-# LANGUAGE OverloadedStrings #-}

module Test.Native.Primitive
  ( tests,
  )
where

import Aihc.Grin.Syntax (grinForeignCallSymbol)
import Aihc.Native
  ( NativeCpsCall (..),
    NativeCpsTransfer (..),
    NativeRuntimeCall (..),
    nativeCpsPrimitiveCall,
    nativeRuntimePrimitiveCall,
    supportedNativePrimitiveNames,
  )
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
                (runtimeCallSymbol <$> nativeRuntimePrimitiveCall primitive)
          )
          byteArrayRuntimeSymbols,
      testCase "keeps the sized conversion and floating point primitives out of the runtime ABI" $
        mapM_
          (\primitive -> assertEqual ("runtime call for " <> show primitive) Nothing (nativeRuntimePrimitiveCall primitive))
          numericInlineNames,
      testCase "accepts the sized conversion and floating point primitives in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          numericInlineNames,
      testCase "keeps the address indexing primitives out of the runtime ABI" $
        mapM_
          (\primitive -> assertEqual ("runtime call for " <> show primitive) Nothing (nativeRuntimePrimitiveCall primitive))
          addressIndexInlineNames,
      testCase "accepts the address indexing primitives in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          addressIndexInlineNames,
      testCase "keeps the runtime object accesses out of the runtime ABI" $
        mapM_
          (\primitive -> assertEqual ("runtime call for " <> show primitive) Nothing (nativeRuntimePrimitiveCall primitive))
          objectInlineNames,
      testCase "accepts the runtime object accesses in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          objectInlineNames,
      testCase "maps boxed-array primitives to the shared runtime ABI" $
        mapM_
          ( \(primitive, symbol) ->
              assertEqual
                ("runtime call for " <> show primitive)
                (Just symbol)
                (runtimeCallSymbol <$> nativeRuntimePrimitiveCall primitive)
          )
          arrayRuntimeSymbols,
      testCase "maps mutable-reference primitives to the shared runtime ABI" $
        mapM_
          ( \(primitive, symbol) ->
              assertEqual
                ("runtime call for " <> show primitive)
                (Just symbol)
                (runtimeCallSymbol <$> nativeRuntimePrimitiveCall primitive)
          )
          mutVarRuntimeSymbols,
      testCase "maps stable-name primitives to the shared runtime ABI" $
        mapM_
          ( \(primitive, symbol) ->
              assertEqual
                ("runtime call for " <> show primitive)
                (Just symbol)
                (runtimeCallSymbol <$> nativeRuntimePrimitiveCall primitive)
          )
          stableNameRuntimeSymbols,
      testCase "keeps the IEEE 754 bit-pattern casts representation-preserving" $
        mapM_
          (\primitive -> assertEqual ("runtime call for " <> show primitive) Nothing (nativeRuntimePrimitiveCall primitive))
          bitPatternCastNames,
      testCase "accepts the IEEE 754 bit-pattern casts in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          bitPatternCastNames,
      testCase "gives the bit counts the Lir operations of the backend" $
        mapM_
          ( \primitive -> do
              assertEqual ("runtime call for " <> show primitive) Nothing (nativeRuntimePrimitiveCall primitive)
              assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames)
          )
          ["clz#", "ctz#", "popCnt#"],
      testCase "gives timesInt2# the wide multiplication of the backend" $ do
        assertEqual
          "runtime call for timesInt2#"
          Nothing
          (nativeRuntimePrimitiveCall "timesInt2#")
        assertEqual
          "native support for timesInt2#"
          True
          ("timesInt2#" `elem` supportedNativePrimitiveNames),
      testCase "keeps freeze and thaw representation-preserving" $
        mapM_
          (\primitive -> assertEqual ("runtime call for " <> show primitive) Nothing (nativeRuntimePrimitiveCall primitive))
          ["unsafeFreezeByteArray#", "unsafeThawByteArray#", "unsafeFreezeArray#", "unsafeThawArray#"],
      testCase "accepts the complete byte-array API in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          (map fst byteArrayRuntimeSymbols <> ["unsafeFreezeByteArray#", "unsafeThawByteArray#"]),
      testCase "accepts the complete boxed-array API in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          (map fst arrayRuntimeSymbols <> ["unsafeFreezeArray#", "unsafeThawArray#"]),
      testCase "accepts the complete mutable-reference API in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          ["newMutVar#", "readMutVar#", "writeMutVar#", "casMutVar#", "sameMutVar#"],
      testCase "accepts the complete stable-name API in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          ["makeStableName#", "eqStableName#", "stableNameToInt#"],
      testCase "accepts the Integer arithmetic primitive API" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          integerPrimitiveNames,
      testCase "describes CPS primitive runtime signatures" $
        mapM_
          ( \(primitive, runtimeCall) ->
              assertEqual
                ("CPS runtime call for " <> show primitive)
                (Just runtimeCall)
                (nativeCpsPrimitiveCall primitive)
          )
          cpsRuntimeCalls,
      testCase "accepts the Prelude Int# primitive API in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          ["+#", "-#", "*#", "compareInt#", "<#", "==#", ">#", ">=#", "<=#", "/=#", "ord#", "chr#"],
      testCase "keeps the address arithmetic primitives out of the runtime ABI" $
        mapM_
          (\primitive -> assertEqual ("runtime call for " <> show primitive) Nothing (nativeRuntimePrimitiveCall primitive))
          addressArithmeticInlineNames,
      testCase "accepts the address arithmetic primitives in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          addressArithmeticInlineNames,
      testCase "accepts the Word64# comparison and conversion primitives in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          ["eqWord64#", "neWord64#", "ltWord64#", "leWord64#", "gtWord64#", "geWord64#", "wordToWord64#", "word16ToWord#"],
      testCase "accepts the sized word shift primitives in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          [ "uncheckedShiftLWord16#",
            "uncheckedShiftRLWord16#",
            "uncheckedShiftLWord32#",
            "uncheckedShiftRLWord32#",
            "uncheckedShiftL64#",
            "uncheckedShiftRL64#"
          ],
      testCase "accepts the Word8# comparison primitives in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          ["eqWord8#", "word8ToWord#", "wordToWord8#"],
      testCase "accepts the Char# comparison and Int# division primitives in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          ["eqChar#", "neChar#", "ltChar#", "leChar#", "gtChar#", "geChar#", "quotInt#", "remInt#"]
    ]

runtimeCallSymbol :: NativeRuntimeCall -> Text
runtimeCallSymbol = grinForeignCallSymbol . nativeRuntimeCallForeignCall

byteArrayRuntimeSymbols :: [(Text, Text)]
byteArrayRuntimeSymbols =
  [ ("newByteArray#", "aihc_byte_array_new"),
    ("newPinnedByteArray#", "aihc_byte_array_new_pinned"),
    ("newAlignedPinnedByteArray#", "aihc_byte_array_new_aligned_pinned"),
    ("shrinkMutableByteArray#", "aihc_byte_array_shrink"),
    ("resizeMutableByteArray#", "aihc_byte_array_resize"),
    ("copyAddrToByteArray#", "aihc_byte_array_copy_from_addr"),
    ("copyByteArray#", "aihc_byte_array_copy"),
    ("copyMutableByteArray#", "aihc_byte_array_copy"),
    ("copyMutableByteArrayNonOverlapping#", "aihc_byte_array_copy"),
    ("fetchAddIntArray#", "aihc_byte_array_fetch_add_word"),
    ("fetchSubIntArray#", "aihc_byte_array_fetch_sub_word"),
    ("fetchAndIntArray#", "aihc_byte_array_fetch_and_word"),
    ("fetchNandIntArray#", "aihc_byte_array_fetch_nand_word"),
    ("fetchOrIntArray#", "aihc_byte_array_fetch_or_word"),
    ("fetchXorIntArray#", "aihc_byte_array_fetch_xor_word"),
    ("casIntArray#", "aihc_byte_array_compare_and_swap_word"),
    ("copyByteArrayToAddr#", "aihc_byte_array_copy_to_addr"),
    ("copyMutableByteArrayToAddr#", "aihc_byte_array_copy_to_addr"),
    ("compareByteArrays#", "aihc_byte_array_compare"),
    ("setByteArray#", "aihc_byte_array_set")
  ]

bitPatternCastNames :: [Text]
bitPatternCastNames =
  ["castFloatToWord32#", "castWord32ToFloat#", "castDoubleToWord64#", "castWord64ToDouble#"]

arrayRuntimeSymbols :: [(Text, Text)]
arrayRuntimeSymbols =
  [ ("newArray#", "aihc_array_new"),
    ("copyArray#", "aihc_array_copy"),
    ("copyMutableArray#", "aihc_array_copy"),
    ("cloneArray#", "aihc_array_clone"),
    ("cloneMutableArray#", "aihc_array_clone"),
    ("freezeArray#", "aihc_array_clone"),
    ("thawArray#", "aihc_array_clone"),
    ("shrinkSmallMutableArray#", "aihc_array_shrink"),
    ("resizeSmallMutableArray#", "aihc_array_resize")
  ]

mutVarRuntimeSymbols :: [(Text, Text)]
mutVarRuntimeSymbols =
  [("newMutVar#", "aihc_mutvar_new")]

-- | Accesses of runtime objects that the Lir lowering gives as Lir
-- operations: identity tests, field reads, and element accesses of boxed
-- arrays, mutable references, byte arrays, MVars, and stable names. None
-- has an entry in the runtime ABI.
objectInlineNames :: [Text]
objectInlineNames =
  [ "sameMutableArray#",
    "sameSmallMutableArray#",
    "sameMutVar#",
    "readMutVar#",
    "writeMutVar#",
    "casMutVar#",
    "eqStableName#",
    "stableNameToInt#",
    "indexArray#",
    "readArray#",
    "writeArray#",
    "sizeofArray#",
    "sizeofMutableArray#",
    "indexSmallArray#",
    "readSmallArray#",
    "writeSmallArray#",
    "sizeofSmallArray#",
    "sizeofSmallMutableArray#",
    "getSizeofSmallMutableArray#",
    "sameMVar#",
    "isEmptyMVar#",
    "tryReadMVar#",
    "isMutableByteArrayPinned#",
    "isByteArrayPinned#",
    "byteArrayContents#",
    "mutableByteArrayContents#",
    "sizeofByteArray#",
    "getSizeofMutableByteArray#",
    "sizeofMutableByteArray#",
    "indexWordArray#",
    "readWordArray#",
    "writeWordArray#",
    "atomicReadIntArray#",
    "atomicWriteIntArray#",
    "indexCharArray#",
    "readCharArray#",
    "writeCharArray#",
    "indexWord8ArrayAsWord16#",
    "indexWord8ArrayAsWord32#",
    "indexWord8ArrayAsWord64#",
    "indexWord8Array#",
    "readWord8Array#",
    "writeWord8Array#",
    "indexWord16Array#",
    "readWord16Array#",
    "writeWord16Array#",
    "indexWord32Array#",
    "readWord32Array#",
    "writeWord32Array#",
    "indexWord64Array#",
    "readWord64Array#",
    "writeWord64Array#"
  ]

-- | Primitives that the Lir lowering gives as Lir operations. They have no
-- entry in the runtime ABI.
addressIndexInlineNames :: [Text]
addressIndexInlineNames =
  [ "indexWord8OffAddr#",
    "indexWord16OffAddr#",
    "indexWord32OffAddr#",
    "indexWord64OffAddr#",
    "readWord8OffAddr#",
    "readWord16OffAddr#",
    "readWord32OffAddr#",
    "readWord64OffAddr#",
    "writeWord8OffAddr#",
    "writeWord16OffAddr#",
    "writeWord32OffAddr#",
    "writeWord64OffAddr#",
    "indexWord8OffAddrAsWord16#",
    "indexWord8OffAddrAsWord32#",
    "indexWord8OffAddrAsWord64#",
    "readWord8OffAddrAsWord16#",
    "readWord8OffAddrAsWord32#",
    "readWord8OffAddrAsWord64#",
    "indexWord8OffAddrAsFloat#",
    "indexWord8OffAddrAsDouble#",
    "readWord8OffAddrAsFloat#",
    "readWord8OffAddrAsDouble#",
    "writeWord8OffAddrAsFloat#",
    "writeWord8OffAddrAsDouble#",
    "writeWord8OffAddrAsWord16#",
    "writeWord8OffAddrAsWord32#",
    "writeWord8OffAddrAsWord64#",
    "indexInt8OffAddr#",
    "readInt8OffAddr#",
    "writeInt8OffAddr#",
    "indexInt16OffAddr#",
    "readInt16OffAddr#",
    "writeInt16OffAddr#",
    "indexInt32OffAddr#",
    "readInt32OffAddr#",
    "writeInt32OffAddr#",
    "indexInt64OffAddr#",
    "readInt64OffAddr#",
    "writeInt64OffAddr#",
    "indexIntOffAddr#",
    "readIntOffAddr#",
    "writeIntOffAddr#",
    "indexWordOffAddr#",
    "readWordOffAddr#",
    "writeWordOffAddr#",
    "indexAddrOffAddr#",
    "readAddrOffAddr#",
    "writeAddrOffAddr#",
    "indexFloatOffAddr#",
    "readFloatOffAddr#",
    "writeFloatOffAddr#",
    "indexDoubleOffAddr#",
    "readDoubleOffAddr#",
    "writeDoubleOffAddr#",
    "indexWideCharOffAddr#",
    "readWideCharOffAddr#",
    "writeWideCharOffAddr#",
    "indexStablePtrOffAddr#",
    "readStablePtrOffAddr#",
    "writeStablePtrOffAddr#"
  ]

addressArithmeticInlineNames :: [Text]
addressArithmeticInlineNames =
  [ "plusAddr#",
    "minusAddr#",
    "eqAddr#",
    "neAddr#",
    "ltAddr#",
    "leAddr#",
    "gtAddr#",
    "geAddr#",
    "addr2Int#",
    "int2Addr#",
    "cstringLength#",
    "touch#",
    "wordToWord8#",
    "wordToWord16#",
    "wordToWord32#"
  ]

numericInlineNames :: [Text]
numericInlineNames =
  [ "intToInt8#",
    "int8ToInt#",
    "intToInt16#",
    "int16ToInt#",
    "intToInt32#",
    "int32ToInt#",
    "intToInt64#",
    "int64ToInt#",
    "plusFloat#",
    "minusFloat#",
    "timesFloat#",
    "negateFloat#",
    "fabsFloat#",
    "int2Float#",
    "float2Int#",
    "gtFloat#",
    "ltFloat#",
    "eqFloat#",
    "+##",
    "-##",
    "*##",
    "negateDouble#",
    "fabsDouble#",
    "int2Double#",
    "double2Int#",
    ">##",
    "<##",
    "==##",
    "float2Double#",
    "double2Float#",
    "byteSwap#",
    "byteSwap16#",
    "byteSwap32#",
    "byteSwap64#"
  ]

stableNameRuntimeSymbols :: [(Text, Text)]
stableNameRuntimeSymbols =
  [("makeStableName#", "aihc_stable_name_make")]

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

cpsRuntimeCalls :: [(Text, NativeCpsCall)]
cpsRuntimeCalls =
  [ enters "fork#" "aihc_fork" 1,
    enters "newMVar#" "aihc_mvar_new" 0,
    resumes "readMVar#" "aihc_mvar_read" 1,
    resumes "takeMVar#" "aihc_mvar_take" 1,
    resumes "putMVar#" "aihc_mvar_put" 2,
    resumes "yield#" "aihc_yield" 0,
    resumes "awaitIO#" "aihc_await_io" 1
  ]
  where
    enters primitive symbol operands =
      (primitive, NativeCpsCall symbol operands False NativeCpsEnterContinuation)
    resumes primitive symbol operands =
      (primitive, NativeCpsCall symbol operands True NativeCpsResumeScheduler)
