{-# LANGUAGE OverloadedStrings #-}

module Redact.Monad.Terminal.Mock (tests) where

-- https://hackage.haskell.org/package/HMock
import Test.HMock ((|->), expect, inSequence, runMockT)

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit (testCase)

-- (redact)
import qualified Redact.Monad.Terminal as RMT
import Redact.Monad.Terminal (resetSGRs)
import Redact.Types (Line(NormalLine, RedactLine), Part(Redact, Stet))

-- (redact:test)
import TestLib

------------------------------------------------------------------------------

testReset :: TestTree
testReset = testCase "reset" . runMockT $ do
    inSequence
      [ expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      ]
    RMT.reset

------------------------------------------------------------------------------

testPutLinesNone :: TestTree
testPutLinesNone = testCase "none" . runMockT $ RMT.putLines redactSGRs []

------------------------------------------------------------------------------

testPutLinesNormalEmpty :: TestTree
testPutLinesNormalEmpty = testCase "normal_empty" . runMockT $ do
    expect $ PutStrLn "" |-> ()
    RMT.putLines redactSGRs [NormalLine []]

------------------------------------------------------------------------------

testPutLinesNormalStet :: TestTree
testPutLinesNormalStet = testCase "normal_stet" . runMockT $ do
    inSequence
      [ expect $ PutStrLn "one" |-> ()
      , expect $ PutStrLn "two" |-> ()
      ]
    RMT.putLines redactSGRs
      [ NormalLine [Stet "one"]
      , NormalLine [Stet "two"]
      ]

------------------------------------------------------------------------------

testPutLinesNormalRedact :: TestTree
testPutLinesNormalRedact = testCase "normal_redact" . runMockT $ do
    inSequence
      [ expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStr "one" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStr "two" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      ]
    RMT.putLines redactSGRs
      [ NormalLine [Redact "one"]
      , NormalLine [Redact "two"]
      ]

------------------------------------------------------------------------------

testPutLinesNormalMixed :: TestTree
testPutLinesNormalMixed = testCase "normal_mixed" . runMockT $ do
    inSequence
      [ expect $ PutStrLn "one" |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ PutStr "two" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStr "three" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStr "four" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStr "five" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStr "six" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStr "seven" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      ]
    RMT.putLines redactSGRs
      [ NormalLine [Stet "one"]
      , NormalLine []
      , NormalLine [Stet "two", Redact "three"]
      , NormalLine [Redact "four"]
      , NormalLine []
      , NormalLine [Redact "five", Stet "six", Redact "seven"]
      ]

------------------------------------------------------------------------------

testPutLinesRedactStet :: TestTree
testPutLinesRedactStet = testCase "redact_stet" . runMockT $ do
    inSequence
      [ expect $ PutStr "one" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ PutStr "two" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ PutStrLn "three" |-> ()
      ]
    RMT.putLines redactSGRs
      [ NormalLine [Stet "one"]
      , RedactLine "two"
      , NormalLine [Stet "three"]
      ]

------------------------------------------------------------------------------

testPutLinesRedactRedact :: TestTree
testPutLinesRedactRedact = testCase "redact_redact" . runMockT $ do
    inSequence
      [ expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStrLn "one" |-> ()
      , expect $ PutStr "two" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStr "three" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      ]
    RMT.putLines redactSGRs
      [ NormalLine [Redact "one"]
      , RedactLine "two"
      , NormalLine [Redact "three"]
      ]

------------------------------------------------------------------------------

testPutLinesRedactMultiple :: TestTree
testPutLinesRedactMultiple = testCase "redact_multiple" . runMockT $ do
    inSequence
      [ expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStrLn "one" |-> ()
      , expect $ PutStrLn "two" |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ PutStr "three" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ PutStrLn "four" |-> ()
      ]
    RMT.putLines redactSGRs
      [ NormalLine [Redact "one"]
      , RedactLine "two"
      , RedactLine ""
      , RedactLine "three"
      , NormalLine [Stet "four"]
      ]

------------------------------------------------------------------------------

testPutLinesRedactBegin :: TestTree
testPutLinesRedactBegin = testCase "redact_begin" . runMockT $ do
    inSequence
      [ expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ PutStr "one" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ PutStrLn "two" |-> ()
      ]
    RMT.putLines redactSGRs
      [ RedactLine "one"
      , NormalLine [Stet "two"]
      ]

------------------------------------------------------------------------------

testPutLinesRedactEnd :: TestTree
testPutLinesRedactEnd = testCase "redact_end" . runMockT $ do
    inSequence
      [ expect $ PutStr "one" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ PutStr "two" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      ]
    RMT.putLines redactSGRs
      [ NormalLine [Stet "one"]
      , RedactLine "two"
      ]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Redact.Monad.Terminal:Mock"
    [ testReset
    , testGroup "putLines"
        [ testPutLinesNone
        , testPutLinesNormalEmpty
        , testPutLinesNormalStet
        , testPutLinesNormalRedact
        , testPutLinesNormalMixed
        , testPutLinesRedactStet
        , testPutLinesRedactRedact
        , testPutLinesRedactMultiple
        , testPutLinesRedactBegin
        , testPutLinesRedactEnd
        ]
    ]
