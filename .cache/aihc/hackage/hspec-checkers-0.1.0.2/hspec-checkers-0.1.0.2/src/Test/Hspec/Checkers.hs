{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | This module bridges between
-- <http://hackage.haskell.org/package/checkers checkers> and
-- <http://hackage.haskell.org/package/hspec hspec>. It makes it easy to
-- integrate tests for class laws provided by checkers into test suites written
-- with hspec.
--
-- Here's an example testing that lists satisfy the 'Monoid' laws:
--
-- @
--import Test.Hspec -- 'Test.Hspec'
--import Test.Hspec.Checkers -- 'Test.Hspec.Checkers'
--import Test.QuickCheck.Classes -- 'Test.QuickCheck.Classes'
--
--main :: IO ()
--main = 'hspec' spec
--
--spec :: Spec
--spec = do
--  'testBatch' ('monoid' (undefined :: [Int]))
-- @
module Test.Hspec.Checkers (testBatch) where


import           Test.Hspec
import           Test.QuickCheck.Checkers

-- to make haddock links work
import           Test.QuickCheck.Classes  (monoid)


-- | Allows to insert a 'TestBatch' into a Spec.
testBatch :: TestBatch -> Spec
testBatch (batchName, tests) = describe ("laws for: " ++ batchName) $
    foldr (>>) (return ()) (map (uncurry it) tests)
