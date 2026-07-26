{-# LANGUAGE OverloadedStrings #-}

module Test.Native.Primitive
  ( tests,
  )
where

import Aihc.Grin.Syntax (grinForeignCallSymbol)
import Aihc.Native
  ( NativeCpsCall (..),
    NativeCpsTransfer (..),
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
          ["+#", "-#", "*#", "compareInt#", "<#", "==#", "charToInt#", "intToChar#"]
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
    ("copyAddrToByteArray#", "aihc_byte_array_copy_from_addr")
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
