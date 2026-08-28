module Main (main) where

import Test.Aihc.Spec qualified as Aihc
import Test.Amd64.Spec qualified as Amd64
import Test.Arm64.Spec qualified as Arm64
import Test.Fc.Spec qualified as Fc
import Test.Grin.Spec qualified as Grin
import Test.Llvm.Spec qualified as Llvm
import Test.Native.Spec qualified as Native
import Test.Tasty (defaultMain, testGroup)
import Test.Wasm.Spec qualified as Wasm

main :: IO ()
main = do
  fc <- Fc.tests
  grin <- Grin.tests
  defaultMain
    ( testGroup
        "aihc"
        [ testGroup "spec" [Aihc.tests],
          testGroup "fc-spec" [fc],
          testGroup "grin-spec" [grin],
          testGroup "native-spec" [Native.tests],
          testGroup "amd64-spec" [Amd64.tests],
          testGroup "arm64-spec" [Arm64.tests],
          testGroup "llvm-spec" [Llvm.tests],
          testGroup "wasm-spec" [Wasm.tests]
        ]
    )
