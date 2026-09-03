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
      testCase "maps sized conversion and floating point primitives to the shared runtime ABI" $
        mapM_
          ( \(primitive, symbol) ->
              assertEqual
                ("runtime call for " <> show primitive)
                (Just symbol)
                (runtimeCallSymbol <$> nativeRuntimePrimitiveCall primitive)
          )
          numericRuntimeSymbols,
      testCase "maps address indexing primitives to the shared runtime ABI" $
        mapM_
          ( \(primitive, symbol) ->
              assertEqual
                ("runtime call for " <> show primitive)
                (Just symbol)
                (runtimeCallSymbol <$> nativeRuntimePrimitiveCall primitive)
          )
          addressIndexRuntimeSymbols,
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
          (map fst arrayRuntimeSymbols <> ["newArray#", "unsafeFreezeArray#", "unsafeThawArray#"]),
      testCase "accepts the complete mutable-reference API in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          ("newMutVar#" : map fst mutVarRuntimeSymbols),
      testCase "accepts the complete stable-name API in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          ("makeStableName#" : map fst stableNameRuntimeSymbols),
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
      testCase "maps address arithmetic primitives to the shared runtime ABI" $
        mapM_
          ( \(primitive, symbol) ->
              assertEqual
                ("runtime call for " <> show primitive)
                (Just symbol)
                (runtimeCallSymbol <$> nativeRuntimePrimitiveCall primitive)
          )
          (addressArithmeticRuntimeSymbols <> wordNarrowRuntimeSymbols),
      testCase "accepts the Word64# comparison and conversion primitives in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          ["eqWord64#", "neWord64#", "ltWord64#", "leWord64#", "gtWord64#", "geWord64#", "wordToWord64#", "word16ToWord#"]
    ]

runtimeCallSymbol :: NativeRuntimeCall -> Text
runtimeCallSymbol = grinForeignCallSymbol . nativeRuntimeCallForeignCall

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
    ("copyByteArray#", "aihc_byte_array_copy"),
    ("copyMutableByteArray#", "aihc_byte_array_copy"),
    ("copyByteArrayToAddr#", "aihc_byte_array_copy_to_addr"),
    ("copyMutableByteArrayToAddr#", "aihc_byte_array_copy_to_addr"),
    ("compareByteArrays#", "aihc_byte_array_compare")
  ]

addressIndexRuntimeSymbols :: [(Text, Text)]
addressIndexRuntimeSymbols =
  [ ("indexWord8OffAddr#", "aihc_addr_index_word8"),
    ("indexWord16OffAddr#", "aihc_addr_index_word16"),
    ("indexWord32OffAddr#", "aihc_addr_index_word32"),
    ("indexWord64OffAddr#", "aihc_addr_index_word64"),
    ("readWord8OffAddr#", "aihc_addr_index_word8"),
    ("readWord16OffAddr#", "aihc_addr_index_word16"),
    ("readWord32OffAddr#", "aihc_addr_index_word32"),
    ("readWord64OffAddr#", "aihc_addr_index_word64"),
    ("writeWord8OffAddr#", "aihc_addr_write_word8"),
    ("writeWord16OffAddr#", "aihc_addr_write_word16"),
    ("writeWord32OffAddr#", "aihc_addr_write_word32"),
    ("writeWord64OffAddr#", "aihc_addr_write_word64"),
    ("indexWord8OffAddrAsWord16#", "aihc_addr_index_byte_word16"),
    ("indexWord8OffAddrAsWord32#", "aihc_addr_index_byte_word32"),
    ("indexWord8OffAddrAsWord64#", "aihc_addr_index_byte_word64"),
    ("readWord8OffAddrAsWord16#", "aihc_addr_index_byte_word16"),
    ("readWord8OffAddrAsWord32#", "aihc_addr_index_byte_word32"),
    ("readWord8OffAddrAsWord64#", "aihc_addr_index_byte_word64"),
    ("writeWord8OffAddrAsWord16#", "aihc_addr_write_byte_word16"),
    ("writeWord8OffAddrAsWord32#", "aihc_addr_write_byte_word32"),
    ("writeWord8OffAddrAsWord64#", "aihc_addr_write_byte_word64")
  ]

addressArithmeticRuntimeSymbols :: [(Text, Text)]
addressArithmeticRuntimeSymbols =
  [ ("plusAddr#", "aihc_addr_plus"),
    ("minusAddr#", "aihc_addr_minus"),
    ("eqAddr#", "aihc_addr_eq"),
    ("neAddr#", "aihc_addr_ne"),
    ("ltAddr#", "aihc_addr_lt"),
    ("leAddr#", "aihc_addr_le"),
    ("gtAddr#", "aihc_addr_gt"),
    ("geAddr#", "aihc_addr_ge"),
    ("addr2Int#", "aihc_addr_to_int"),
    ("int2Addr#", "aihc_int_to_addr"),
    ("cstringLength#", "aihc_addr_cstring_length"),
    ("touch#", "aihc_touch")
  ]

wordNarrowRuntimeSymbols :: [(Text, Text)]
wordNarrowRuntimeSymbols =
  [ ("wordToWord8#", "aihc_word_to_word8"),
    ("wordToWord16#", "aihc_word_to_word16"),
    ("wordToWord32#", "aihc_word_to_word32")
  ]

arrayRuntimeSymbols :: [(Text, Text)]
arrayRuntimeSymbols =
  [ ("indexArray#", "aihc_array_index"),
    ("readArray#", "aihc_array_index"),
    ("writeArray#", "aihc_array_write"),
    ("sameMutableArray#", "aihc_array_same")
  ]

mutVarRuntimeSymbols :: [(Text, Text)]
mutVarRuntimeSymbols =
  [ ("readMutVar#", "aihc_mutvar_read"),
    ("writeMutVar#", "aihc_mutvar_write"),
    ("casMutVar#", "aihc_mutvar_compare_and_swap"),
    ("sameMutVar#", "aihc_mutvar_same")
  ]

numericRuntimeSymbols :: [(Text, Text)]
numericRuntimeSymbols =
  [ ("intToInt8#", "aihc_int_to_int8"),
    ("int8ToInt#", "aihc_int8_to_int"),
    ("intToInt16#", "aihc_int_to_int16"),
    ("int16ToInt#", "aihc_int16_to_int"),
    ("intToInt32#", "aihc_int_to_int32"),
    ("int32ToInt#", "aihc_int32_to_int"),
    ("intToInt64#", "aihc_int_to_int64"),
    ("int64ToInt#", "aihc_int64_to_int"),
    ("plusFloat#", "aihc_float_plus"),
    ("minusFloat#", "aihc_float_minus"),
    ("timesFloat#", "aihc_float_times"),
    ("negateFloat#", "aihc_float_negate"),
    ("fabsFloat#", "aihc_float_abs"),
    ("int2Float#", "aihc_int_to_float"),
    ("float2Int#", "aihc_float_to_int"),
    ("gtFloat#", "aihc_float_gt"),
    ("ltFloat#", "aihc_float_lt"),
    ("eqFloat#", "aihc_float_eq"),
    ("+##", "aihc_double_plus"),
    ("-##", "aihc_double_minus"),
    ("*##", "aihc_double_times"),
    ("negateDouble#", "aihc_double_negate"),
    ("fabsDouble#", "aihc_double_abs"),
    ("int2Double#", "aihc_int_to_double"),
    ("double2Int#", "aihc_double_to_int"),
    (">##", "aihc_double_gt"),
    ("<##", "aihc_double_lt"),
    ("==##", "aihc_double_eq")
  ]

stableNameRuntimeSymbols :: [(Text, Text)]
stableNameRuntimeSymbols =
  [ ("eqStableName#", "aihc_stable_name_equal"),
    ("stableNameToInt#", "aihc_stable_name_hash")
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
