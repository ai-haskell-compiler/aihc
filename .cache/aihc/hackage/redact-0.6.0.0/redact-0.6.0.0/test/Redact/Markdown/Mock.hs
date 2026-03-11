{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Redact.Markdown.Mock (tests) where

-- https://hackage.haskell.org/package/base
import System.IO (IOMode(ReadMode), stdin)

-- https://hackage.haskell.org/package/explainable-predicates
import Test.Predicates (anything, eq)

-- https://hackage.haskell.org/package/HMock
import Test.HMock ((|->), (|=>), expect, inSequence, runMockT)

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- (redact)
import qualified Redact.Markdown as Redact
import Redact.Monad.Handle (MonadHandle)
import Redact.Monad.Terminal (resetSGRs)

-- (redact:test)
import TestLib

------------------------------------------------------------------------------

testTextToTerminalStrictEmpty :: TestTree
testTextToTerminalStrictEmpty = testCase "empty" . runMockT $
    assertSuccess =<< Redact.textToTerminal redactSGRs ""

------------------------------------------------------------------------------

testTextToTerminalStrictOK :: TestTree
testTextToTerminalStrictOK = testCase "OK" . runMockT $ do
    inSequence
      [ expect $ PutStr "one`" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStr "two" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "`" |-> ()
      , expect $ PutStr "```" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ PutStr "three" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ PutStrLn "```" |-> ()
      ]
    assertSuccess =<< Redact.textToTerminal redactSGRs
      "one`two`\n```\nthree\n```\n"

------------------------------------------------------------------------------

testTextToTerminalStrictErrorInline :: TestTree
testTextToTerminalStrictErrorInline = testCase "error_inline" $ do
    Left err <- runMockT $
      Redact.textToTerminal redactSGRs "one\ntwo`three\nfour`five`six\n"
    err @=? Redact.RedactError "inline code not terminated (line 2)"

------------------------------------------------------------------------------

testTextToTerminalStrictErrorFenced :: TestTree
testTextToTerminalStrictErrorFenced = testCase "error_fenced" $ do
    Left err <- runMockT $
      Redact.textToTerminal redactSGRs "one\n```\ntwo\n"
    err @=? Redact.RedactError "fenced code not terminated (line 4)"

------------------------------------------------------------------------------

testTextToTerminalLenientEmpty :: TestTree
testTextToTerminalLenientEmpty = testCase "empty" . runMockT $
    Redact.textToTerminal' redactSGRs ""

------------------------------------------------------------------------------

testTextToTerminalLenientOK :: TestTree
testTextToTerminalLenientOK = testCase "OK" . runMockT $ do
    inSequence
      [ expect $ PutStr "one`" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStr "two" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "`" |-> ()
      , expect $ PutStr "```" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ PutStr "three" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ PutStrLn "```" |-> ()
      ]
    Redact.textToTerminal' redactSGRs "one`two`\n```\nthree\n```\n"

------------------------------------------------------------------------------

testTextToTerminalLenientErrorInline :: TestTree
testTextToTerminalLenientErrorInline = testCase "error_inline" . runMockT $ do
    inSequence
      [ expect $ PutStrLn "one" |-> ()
      , expect $ PutStr "two`" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStr "three" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ PutStr "four`" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStr "five" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "`six" |-> ()
      ]
    Redact.textToTerminal' redactSGRs "one\ntwo`three\nfour`five`six\n"

------------------------------------------------------------------------------

testTextToTerminalLenientErrorFenced :: TestTree
testTextToTerminalLenientErrorFenced = testCase "error_fenced" . runMockT $ do
    inSequence
      [ expect $ PutStrLn "one" |-> ()
      , expect $ PutStr "```" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ PutStr "two" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      ]
    Redact.textToTerminal' redactSGRs "one\n```\ntwo\n"

------------------------------------------------------------------------------

testHandleToTerminalStrictNone :: TestTree
testHandleToTerminalStrictNone = testCase "none" . runMockT $ do
    expect $ HIsEOF stdin |-> True
    assertSuccess =<< Redact.handleToTerminal redactSGRs stdin

------------------------------------------------------------------------------

testHandleToTerminalStrictOK :: TestTree
testHandleToTerminalStrictOK = testCase "OK" . runMockT $ do
    inSequence
      [ expect $ HIsEOF stdin |-> False
      , expect $ HGetLine stdin |-> "one`two`"
      , expect $ HIsEOF stdin |-> False
      , expect $ HGetLine stdin |-> "```"
      , expect $ PutStr "one`" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStr "two" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "`" |-> ()
      , expect $ HIsEOF stdin |-> False
      , expect $ HGetLine stdin |-> "three"
      , expect $ PutStr "```" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ HIsEOF stdin |-> False
      , expect $ HGetLine stdin |-> "```"
      , expect $ PutStr "three" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ HIsEOF stdin |-> True
      , expect $ PutStrLn "```" |-> ()
      ]
    assertSuccess =<< Redact.handleToTerminal redactSGRs stdin

------------------------------------------------------------------------------

testHandleToTerminalStrictErrorInline :: TestTree
testHandleToTerminalStrictErrorInline = testCase "error_inline" $ do
    Left err <- runMockT $ do
      inSequence
        [ expect $ HIsEOF stdin |-> False
        , expect $ HGetLine stdin |-> "one"
        , expect $ HIsEOF stdin |-> False
        , expect $ HGetLine stdin |-> "two`three"
        , expect $ PutStrLn "one"
        ]
      Redact.handleToTerminal redactSGRs stdin
    err @=? Redact.RedactError "inline code not terminated (line 2)"

------------------------------------------------------------------------------

testHandleToTerminalStrictErrorFenced :: TestTree
testHandleToTerminalStrictErrorFenced = testCase "error_fenced" $ do
    Left err <- runMockT $ do
      inSequence
        [ expect $ HIsEOF stdin |-> False
        , expect $ HGetLine stdin |-> "one"
        , expect $ HIsEOF stdin |-> False
        , expect $ HGetLine stdin |-> "```"
        , expect $ PutStrLn "one" |-> ()
        , expect $ HIsEOF stdin |-> False
        , expect $ HGetLine stdin |-> "two"
        , expect $ PutStr "```" |-> ()
        , expect $ SetSGR redactSGRs |-> ()
        , expect $ PutStrLn "" |-> ()
        , expect $ HIsEOF stdin |-> True
        , expect $ PutStr "two" |-> ()
        , expect $ SetSGR resetSGRs |-> ()
        , expect $ PutStrLn "" |-> ()
        ]
      Redact.handleToTerminal redactSGRs stdin
    err @=? Redact.RedactError "fenced code not terminated (line 4)"

------------------------------------------------------------------------------

testHandleToTerminalLenientNone :: TestTree
testHandleToTerminalLenientNone = testCase "none" . runMockT $ do
    expect $ HIsEOF stdin |-> True
    Redact.handleToTerminal' redactSGRs stdin

------------------------------------------------------------------------------

testHandleToTerminalLenientOK :: TestTree
testHandleToTerminalLenientOK = testCase "OK" . runMockT $ do
    inSequence
      [ expect $ HIsEOF stdin |-> False
      , expect $ HGetLine stdin |-> "one`two`"
      , expect $ HIsEOF stdin |-> False
      , expect $ HGetLine stdin |-> "```"
      , expect $ PutStr "one`" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStr "two" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "`" |-> ()
      , expect $ HIsEOF stdin |-> False
      , expect $ HGetLine stdin |-> "three"
      , expect $ PutStr "```" |-> ()
      , expect $ SetSGR redactSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ HIsEOF stdin |-> False
      , expect $ HGetLine stdin |-> "```"
      , expect $ PutStr "three" |-> ()
      , expect $ SetSGR resetSGRs |-> ()
      , expect $ PutStrLn "" |-> ()
      , expect $ HIsEOF stdin |-> True
      , expect $ PutStrLn "```" |-> ()
      ]
    Redact.handleToTerminal' redactSGRs stdin

------------------------------------------------------------------------------

testHandleToTerminalLenientErrorInline :: TestTree
testHandleToTerminalLenientErrorInline =
    testCase "error_inline" . runMockT $ do
      inSequence
        [ expect $ HIsEOF stdin |-> False
        , expect $ HGetLine stdin |-> "one"
        , expect $ HIsEOF stdin |-> False
        , expect $ HGetLine stdin |-> "two`three"
        , expect $ PutStrLn "one"
        , expect $ HIsEOF stdin |-> False
        , expect $ HGetLine stdin |-> "four`five`six"
        , expect $ PutStr "two`" |-> ()
        , expect $ SetSGR redactSGRs |-> ()
        , expect $ PutStr "three" |-> ()
        , expect $ SetSGR resetSGRs |-> ()
        , expect $ PutStrLn "" |-> ()
        , expect $ HIsEOF stdin |-> True
        , expect $ PutStr "four`" |-> ()
        , expect $ SetSGR redactSGRs |-> ()
        , expect $ PutStr "five" |-> ()
        , expect $ SetSGR resetSGRs |-> ()
        , expect $ PutStrLn "`six" |-> ()
        ]
      Redact.handleToTerminal' redactSGRs stdin

------------------------------------------------------------------------------

testHandleToTerminalLenientErrorFenced :: TestTree
testHandleToTerminalLenientErrorFenced =
    testCase "error_fenced" . runMockT $ do
      inSequence
        [ expect $ HIsEOF stdin |-> False
        , expect $ HGetLine stdin |-> "one"
        , expect $ HIsEOF stdin |-> False
        , expect $ HGetLine stdin |-> "```"
        , expect $ PutStrLn "one" |-> ()
        , expect $ HIsEOF stdin |-> False
        , expect $ HGetLine stdin |-> "two"
        , expect $ PutStr "```" |-> ()
        , expect $ SetSGR redactSGRs |-> ()
        , expect $ PutStrLn "" |-> ()
        , expect $ HIsEOF stdin |-> True
        , expect $ PutStr "two" |-> ()
        , expect $ SetSGR resetSGRs |-> ()
        , expect $ PutStrLn "" |-> ()
        ]
      Redact.handleToTerminal' redactSGRs stdin

------------------------------------------------------------------------------

testFileToTerminalStrictOK :: TestTree
testFileToTerminalStrictOK = testCase "OK" . runMockT $ do
    expect $
      ( WithFile_ (eq "test.md") (eq ReadMode) anything
          :: Matcher MonadHandle "withFile" IO
              (Either IOError (Either Redact.Error ()))
      )
      |=> \(WithFile _path _mode _action) -> pure . Right $ Right ()
    assertSuccess =<< Redact.fileToTerminal redactSGRs "test.md"

------------------------------------------------------------------------------

testFileToTerminalStrictRedactError :: TestTree
testFileToTerminalStrictRedactError = testCase "error_redact" $ do
    let err = Redact.RedactError "inline code not terminated (line 42)"
    Left err' <- runMockT $ do
      expect $
        ( WithFile_ (eq "test.md") (eq ReadMode) anything
            :: Matcher MonadHandle "withFile" IO
                (Either IOError (Either Redact.Error ()))
        )
        |=> \(WithFile _path _mode _action) -> pure . Right $ Left err
      Redact.fileToTerminal redactSGRs "test.md"
    err' @=? err

------------------------------------------------------------------------------

testFileToTerminalStrictIOError :: TestTree
testFileToTerminalStrictIOError = testCase "error_io" $ do
    let err = userError "nope"
    Left err' <- runMockT $ do
      expect $
        ( WithFile_ (eq "test.md") (eq ReadMode) anything
            :: Matcher MonadHandle "withFile" IO
                (Either IOError (Either Redact.Error ()))
        )
        |=> \(WithFile _path _mode _action) -> pure $ Left err
      Redact.fileToTerminal redactSGRs "test.md"
    err' @=? Redact.IOError err

------------------------------------------------------------------------------

testFileToTerminalLenientOK :: TestTree
testFileToTerminalLenientOK = testCase "OK" . runMockT $ do
    expect $
      ( WithFile_ (eq "test.md") (eq ReadMode) anything
          :: Matcher MonadHandle "withFile" IO (Either IOError ())
      )
      |=> \(WithFile _path _mode _action) -> pure $ Right ()
    assertSuccess =<< Redact.fileToTerminal' redactSGRs "test.md"

------------------------------------------------------------------------------

testFileToTerminalLenientIOError :: TestTree
testFileToTerminalLenientIOError = testCase "error_io" $ do
    let err = userError "nope"
    Left err' <- runMockT $ do
      expect $
        ( WithFile_ (eq "test.md") (eq ReadMode) anything
            :: Matcher MonadHandle "withFile" IO (Either IOError ())
        )
        |=> \(WithFile _path _mode _action) -> pure $ Left err
      Redact.fileToTerminal' redactSGRs "test.md"
    err' @=? Redact.IOError err

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Redact.Markdown:Mock"
    [ testGroup "textToTerminal"
        [ testTextToTerminalStrictEmpty
        , testTextToTerminalStrictOK
        , testTextToTerminalStrictErrorInline
        , testTextToTerminalStrictErrorFenced
        ]
    , testGroup "textToTerminal'"
        [ testTextToTerminalLenientEmpty
        , testTextToTerminalLenientOK
        , testTextToTerminalLenientErrorInline
        , testTextToTerminalLenientErrorFenced
        ]
    , testGroup "handleToTerminal"
        [ testHandleToTerminalStrictNone
        , testHandleToTerminalStrictOK
        , testHandleToTerminalStrictErrorInline
        , testHandleToTerminalStrictErrorFenced
        ]
    , testGroup "handleToTerminal'"
        [ testHandleToTerminalLenientNone
        , testHandleToTerminalLenientOK
        , testHandleToTerminalLenientErrorInline
        , testHandleToTerminalLenientErrorFenced
        ]
    , testGroup "fileToTerminal"
        [ testFileToTerminalStrictOK
        , testFileToTerminalStrictRedactError
        , testFileToTerminalStrictIOError
        ]
    , testGroup "fileToTerminal'"
        [ testFileToTerminalLenientOK
        , testFileToTerminalLenientIOError
        ]
    ]
