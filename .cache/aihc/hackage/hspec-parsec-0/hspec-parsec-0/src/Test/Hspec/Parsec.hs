-- | Utility functions for testing Parsec parsers with Hspec.
module Test.Hspec.Parsec
    ( shouldParse
    , shouldFailOn
    ) where

import GHC.Stack (HasCallStack)
import Text.Parsec
import Test.Hspec.Expectations

-- | Create an expectation by saying what the result should be.
--
-- @
-- 'parse' 'letter' "" "x" ``shouldParse`` \'x\'
-- @
shouldParse
  :: (HasCallStack, Show a, Eq a)
  => Either ParseError a -- ^ Result of parsing as returned by function like 'parse'
  -> a                   -- ^ Desired result
  -> Expectation
r `shouldParse` v = case r of
  Left e -> expectationFailure $ "expected: " ++ show v ++
    "\nbut parsing failed with error:\n" ++ show e
  Right x -> x `shouldBe` v

-- | Check that a parser fails on a given input.
--
-- @
-- 'parse' ('char' \'x\') "" ``shouldFailOn`` "a"
-- @
shouldFailOn
  :: (HasCallStack, Show a)
  => (s -> Either ParseError a)
     -- ^ Parser that takes stream and produces result or error message
  -> s                 -- ^ Input that the parser should fail on
  -> Expectation
p `shouldFailOn` s = case p s of
  Left _ -> return ()
  Right v -> expectationFailure $
    "the parser is expected to fail, but it parsed: " ++ show v
