{-# LANGUAGE OverloadedStrings #-}

module Test.Native.RegisterAllocate
  ( tests,
  )
where

import Aihc.Native.Lir
import Aihc.Native.RegisterAllocate
import Data.Map.Strict qualified as Map
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

data TestReg = First | Second
  deriving (Eq, Ord, Show)

tests :: TestTree
tests =
  testGroup
    "register allocator"
    [ testCase "reuses an expired register" $ do
        let first = VirtualReg 0
            second = VirtualReg 1
            allocation =
              allocate
                [ MoveImmediate (Virtual first) 1,
                  Move (Physical First) (Virtual first),
                  MoveImmediate (Virtual second) 2,
                  Move (Physical First) (Virtual second)
                ]
        assertEqual
          "locations"
          (Map.fromList [(first, InRegister First), (second, InRegister First)])
          (allocationLocations allocation),
      testCase "spills the interval ending farthest from the scan point" $ do
        let longLived = VirtualReg 0
            shortLived = VirtualReg 1
            allocation =
              allocateWith
                [First]
                [ MoveImmediate (Virtual longLived) 1,
                  MoveImmediate (Virtual shortLived) 2,
                  Move (Physical First) (Virtual shortLived),
                  Move (Physical First) (Virtual longLived)
                ]
        assertEqual "long-lived interval" (Just (InHeapSpill 0)) (Map.lookup longLived (allocationLocations allocation))
        assertEqual "short-lived interval" (Just (InRegister First)) (Map.lookup shortLived (allocationLocations allocation)),
      testCase "spills values live across calls" $ do
        let register = VirtualReg 0
            allocation =
              allocate
                [ MoveImmediate (Virtual register) 1,
                  Call "external",
                  Move (Physical First) (Virtual register)
                ]
        assertEqual "call-spanning location" (Just (InHeapSpill 0)) (Map.lookup register (allocationLocations allocation)),
      testCase "uses only registers supplied by the backend" $ do
        let registers = map VirtualReg [0 .. 2]
            definitions = [MoveImmediate (Virtual register) (fromIntegral index) | (index, register) <- zip [0 :: Int ..] registers]
            uses = [Move (Physical First) (Virtual register) | register <- registers]
            allocation = allocate (definitions <> uses)
        assertBool "second backend register used" (InRegister Second `elem` Map.elems (allocationLocations allocation))
        assertEqual "one heap spill" 1 (allocationSpillCount allocation)
    ]
  where
    allocate = allocateWith [First, Second]
    allocateWith registers = allocateBlock AllocatorConfig {allocatorRegisters = registers}
