{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import qualified Data.ByteString                              as BS
import           Data.Monoid
import           Network.Wai.Middleware.FilterLogger.Internal
import           Test.HUnit.Base
import           Test.HUnit.Text

main :: IO ()
main = void . runTestTT . TestList $ basicTests ++ advancedTests

-- basic tests for log filtering functionality
basicTests :: [Test]
basicTests = map TestCase [
    noFilter
  , simpleFilterKeep
  , simpleFilterDiscard
  ]

-- slightly more complicated filters
advancedTests :: [Test]
advancedTests = map TestCase [
    doubleFilterKeep
  , doubleFilterDiscard
  ]

runFilterUnitTest :: Maybe BS.ByteString -> BS.ByteString -> LogFilter BS.ByteString -> Assertion
runFilterUnitTest expectation input lf = expectation @=? lf input

invert :: LogFilter a -> LogFilter a
invert lf a = maybe (Just a) (const Nothing) $ lf a

contains :: BS.ByteString -> BS.ByteString -> Bool
contains t = not . BS.null . snd . BS.breakSubstring t

simpleFilter :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
simpleFilter t bs
  | contains t bs = Just bs
  | otherwise         = Nothing

noFilter :: Assertion
noFilter = runFilterUnitTest (Just "MyByteString") "MyByteString" return

simpleFilterKeep :: Assertion
simpleFilterKeep = runFilterUnitTest (Just "MyByteString") "MyByteString" testFilter
  where testFilter = simpleFilter "Str"

simpleFilterDiscard :: Assertion
simpleFilterDiscard = runFilterUnitTest Nothing "MyByteString" testFilter
  where testFilter = invert (simpleFilter "Str")

doubleFilter :: LogFilter BS.ByteString
doubleFilter = simpleFilter "Test" >=> lenCheck
  where lenCheck s
          | BS.length s > 4 = Just s
          | otherwise       = Nothing

doubleFilterKeep :: Assertion
doubleFilterKeep = runFilterUnitTest (Just "UnitTest") "UnitTest" doubleFilter

doubleFilterDiscard :: Assertion
doubleFilterDiscard = runFilterUnitTest Nothing "UnitTest" (invert doubleFilter)
