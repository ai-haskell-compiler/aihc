{-# LANGUAGE OverloadedStrings #-}

module Test.Arm64.Spec (tests) where

import Aihc.Arm64.Assemble
import Aihc.Arm64.Lir (compileLirObject, elideSlotReloads)
import Aihc.Cli.Backend (BackendOutput (..))
import Aihc.Lir.Lower (posixTarget64)
import Aihc.Native (NativeTarget (AppleArm64))
import System.Info (arch, os)
import Test.Lir.NativeSuite (NativeBackend (..))
import Test.Lir.NativeSuite qualified as NativeSuite
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

tests :: IO TestTree
tests = do
  suite <-
    NativeSuite.tests
      NativeBackend
        { backendName = "aihc-arm64",
          backendTarget = AppleArm64,
          backendLowerTarget = posixTarget64,
          backendClangArguments = ["--target=arm64-apple-darwin"],
          backendRuns = arch == "aarch64" && os == "darwin",
          backendAllocationKey = "macos-arm64",
          backendSourceExtension = ".o",
          backendCompile = either (Left . show) (Right . BackendObject) . compileLirObject
        }
  pure (testGroup "arm64" [suite, slotReloadTests])

slotReloadTests :: TestTree
slotReloadTests =
  testGroup
    "slot reloads"
    [ testCase "a register that holds the slot skips the load" $
        elideSlotReloads [store X9 0, load X9 0] @?= [store X9 0],
      testCase "a load into another register stays" $
        elideSlotReloads [store X9 0, load X10 0] @?= [store X9 0, load X10 0],
      testCase "a write to the register ends the run" $
        elideSlotReloads [store X9 0, add X9, load X9 0] @?= [store X9 0, add X9, load X9 0],
      testCase "a store through a register base keeps the run" $
        elideSlotReloads [load X10 8, storeThrough X9 X10, load X10 8]
          @?= [load X10 8, storeThrough X9 X10],
      testCase "a new value in the slot ends the run" $
        elideSlotReloads [load X9 0, store X10 0, load X9 0] @?= [load X9 0, store X10 0, load X9 0],
      testCase "a call ends the run" $
        elideSlotReloads [store X9 0, call, load X9 0] @?= [store X9 0, call, load X9 0],
      testCase "a moved stack pointer ends the run" $
        elideSlotReloads [store X9 0, dropStack, load X9 0] @?= [store X9 0, dropStack, load X9 0],
      testCase "a label ends the run" $
        elideSlotReloads [store X9 0, arm64Label "block", load X9 0]
          @?= [store X9 0, arm64Label "block", load X9 0],
      testCase "the narrow name of the register ends the run" $
        elideSlotReloads [store X9 0, add W9, load X9 0] @?= [store X9 0, add W9, load X9 0]
    ]
  where
    load register offset = arm64Instruction (ArmLdr register (Arm64Offset SP offset))
    store register offset = arm64Instruction (ArmStr register (Arm64Offset SP offset))
    storeThrough source base = arm64Instruction (ArmStr source (Arm64Offset base 0))
    add register = arm64Instruction (ArmAdd register register (Arm64ImmediateValue 1))
    call = arm64Instruction (ArmBl "_target")
    dropStack = arm64Instruction (ArmSub SP SP (Arm64ImmediateValue 16))
