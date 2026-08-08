{-# LANGUAGE OverloadedStrings #-}

module Test.Native.Primitive
  ( tests,
  )
where

import Aihc.Grin.Syntax (GrinForeignType (..), grinForeignArgumentTypes, grinForeignCallSignature, grinForeignCallSymbol)
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
      testCase "describes allocating primitive runtime signatures" $
        mapM_
          ( \(primitive, symbol, operands) ->
              assertEqual
                ("allocating runtime call for " <> show primitive)
                (Just (symbol, operands, True, 1))
                (runtimeCallDescription <$> nativeRuntimePrimitiveCall primitive)
          )
          [ ("newArray#", "aihc_array_new", [GrinForeignWord64, GrinForeignWord64]),
            ("newMutVar#", "aihc_mutvar_new", [GrinForeignWord64]),
            ("makeStableName#", "aihc_stable_name_make", [GrinForeignAddr])
          ],
      testCase "accepts the Prelude Int# primitive API in native programs" $
        mapM_
          (\primitive -> assertEqual ("native support for " <> show primitive) True (primitive `elem` supportedNativePrimitiveNames))
          ["+#", "-#", "*#", "compareInt#", "<#", "==#", "ord#", "chr#"]
    ]

runtimeCallSymbol :: NativeRuntimeCall -> Text
runtimeCallSymbol = grinForeignCallSymbol . nativeRuntimeCallForeignCall

runtimeCallDescription :: NativeRuntimeCall -> (Text, [GrinForeignType], Bool, Int)
runtimeCallDescription runtimeCall =
  ( runtimeCallSymbol runtimeCall,
    grinForeignArgumentTypes (grinForeignCallSignature (nativeRuntimeCallForeignCall runtimeCall)),
    nativeRuntimeCallPassMachine runtimeCall,
    nativeRuntimeCallResultCount runtimeCall
  )

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

addressIndexRuntimeSymbols :: [(Text, Text)]
addressIndexRuntimeSymbols =
  [ ("indexWord8OffAddr#", "aihc_addr_index_word8"),
    ("indexWord32OffAddr#", "aihc_addr_index_word32"),
    ("indexWord64OffAddr#", "aihc_addr_index_word64")
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
    ("aihcCasMutVarFlag", "aihc_mutvar_compare_and_swap"),
    ("sameMutVar#", "aihc_mutvar_same")
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
