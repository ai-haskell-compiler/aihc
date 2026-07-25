module Test.Native.RuntimePlan (tests) where

import Aihc.Native
  ( NativeTarget (..),
    RuntimeGarbageCollector (..),
    RuntimePlan (runtimeSources),
    runtimePlan,
  )
import System.FilePath (takeFileName)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck qualified as QC

tests :: TestTree
tests =
  testGroup
    "runtime source plans"
    [ sourcePlan
        "native calloc"
        AppleArm64
        RuntimeGcCalloc
        ["aihc_runtime.c", "aihc_gc_calloc.c", "aihc_host_posix.c"],
      sourcePlan
        "native semispace"
        LinuxAmd64
        RuntimeGcSemispace
        ["aihc_runtime.c", "aihc_gc_semispace.c", "aihc_host_posix.c"],
      sourcePlan
        "portable C"
        PortableC
        RuntimeGcCalloc
        [ "aihc_runtime.c",
          "aihc_gc_calloc.c",
          "aihc_host_posix.c",
          "aihc_runtime_trampoline.c"
        ],
      sourcePlan
        "WebAssembly"
        Wasm32Wasip3
        RuntimeGcCalloc
        [ "aihc_runtime.c",
          "aihc_gc_calloc.c",
          "aihc_host_wasip3.c",
          "aihc_runtime_trampoline.c"
        ],
      QC.testProperty "selects exactly one collector and host" sourcePlanIsComplete
    ]

sourcePlan ::
  String ->
  NativeTarget ->
  RuntimeGarbageCollector ->
  [FilePath] ->
  TestTree
sourcePlan name target garbageCollector expected =
  testCase name $ do
    plan <- runtimePlan target garbageCollector
    assertEqual "selected runtime sources" expected (map takeFileName (runtimeSources plan))

sourcePlanIsComplete :: QC.Property
sourcePlanIsComplete =
  QC.forAll (QC.elements [minBound .. maxBound]) $ \target ->
    QC.forAll (QC.elements [RuntimeGcCalloc, RuntimeGcSemispace]) $ \garbageCollector ->
      QC.ioProperty $ do
        plan <- runtimePlan target garbageCollector
        let names = map takeFileName (runtimeSources plan)
            collectorCount = length (filter ("aihc_gc_" `prefixOf`) names)
            hostCount = length (filter ("aihc_host_" `prefixOf`) names)
        pure $
          QC.counterexample ("runtime sources: " <> show names) $
            QC.conjoin [collectorCount QC.=== 1, hostCount QC.=== 1]
  where
    prefix `prefixOf` value = take (length prefix) value == prefix
