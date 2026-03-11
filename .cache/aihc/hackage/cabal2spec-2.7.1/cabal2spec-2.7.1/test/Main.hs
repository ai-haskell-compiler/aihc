module Main ( main ) where

import Cabal2Spec

import Distribution.Compiler
import Distribution.System
import Distribution.Version
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden

main :: IO ()
main = do
  testCases <- findByExtension [".cabal"] "test/golden-test-cases"
  defaultMain $ testGroup "regression-tests" (map regressionTest testCases)

regressionTest :: String -> TestTree
regressionTest cabalFile = do
  let specFile = cabalFile `replaceExtension` "spec"
      goldenFile = specFile `addExtension` "golden"
      pid = Platform X86_64 Linux
      cid = CompilerId GHC (mkVersion [8,2])
  goldenVsFileDiff specFile
                   (\ref new -> ["diff", "-u", ref, new])
                   goldenFile
                   specFile
                   (cabal2spec pid cid mempty True False (Just 2020) cabalFile specFile)
