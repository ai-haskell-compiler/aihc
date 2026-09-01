module Main (main) where

import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (lookupEnv, setEnv)
import System.FilePath (takeDirectory, (</>))
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
  configureTestRoot
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

configureTestRoot :: IO ()
configureTestRoot = do
  configured <- lookupEnv "AIHC_TEST_ROOT"
  case configured of
    Just _ -> pure ()
    Nothing -> getCurrentDirectory >>= findRoot >>= setEnv "AIHC_TEST_ROOT"
  where
    findRoot directory = do
      exists <- doesFileExist (directory </> "bin" </> "aihc" </> "aihc.cabal")
      if exists
        then pure directory
        else do
          let parent = takeDirectory directory
          if parent == directory
            then fail "Cannot find the test source root."
            else findRoot parent
