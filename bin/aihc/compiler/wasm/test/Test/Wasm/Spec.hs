module Test.Wasm.Spec (tests) where

import Test.Tasty (TestTree)
import Test.Wasm.Suite qualified as Suite

tests :: TestTree
tests = Suite.tests
