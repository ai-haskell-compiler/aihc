-- |
-- Module      : Test.Tasty.LeanCheck
-- Copyright   : (c) 2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- LeanCheck support for the Tasty test framework.
--
-- Here's how your @test.hs@ might look like:
--
-- > import Test.Tasty
-- > import Test.Tasty.LeanCheck as LC
-- > import Data.List
-- >
-- > main :: IO ()
-- > main = defaultMain tests
-- >
-- > tests :: TestTree
-- > tests = testGroup "Test properties checked by LeanCheck"
-- >   [ LC.testProperty "sort == sort . reverse" $
-- >       \list -> sort (list :: [Int]) == sort (reverse list)
-- >   , LC.testProperty "Fermat's little theorem" $
-- >       \x -> ((x :: Integer)^7 - x) `mod` 7 == 0
-- >   -- the following property do not hold
-- >   , LC.testProperty "Fermat's last theorem" $
-- >       \x y z n ->
-- >         (n :: Integer) >= 3 LC.==> x^n + y^n /= (z^n :: Integer)
-- >   ]
--
-- The output for the above program is:
--
-- > $ ./test
-- > Test properties checked by LeanCheck
-- >   sort == sort . reverse:  OK
-- >     +++ OK, passed 200 tests.
-- >   Fermat's little theorem: OK
-- >     +++ OK, passed 200 tests.
-- >   Fermat's last theorem:   FAIL
-- >     *** Failed! Falsifiable (after 71 tests):
-- >     0 0 0 3
-- >
-- > 1 out of 3 tests failed (0.00s)
--
-- Use @--leancheck-tests@ to configure the maximum number of tests for each
-- property.
--
-- Please see the documentation of
-- "Test.LeanCheck" and Tasty
-- for more details.
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ == 708
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif
module Test.Tasty.LeanCheck
  ( testProperty
  , LeanCheckTests (..)
  , module Test.LeanCheck
  )
where

import Test.Tasty.Providers hiding (Result)
import Test.Tasty.Options
import Test.LeanCheck
import Data.Proxy
import Control.Applicative (pure)
import Control.Exception (SomeException, catch, evaluate)
#if __GLASGOW_HASKELL__ == 708
import Data.Typeable (Typeable)

deriving instance Typeable Results
deriving instance Typeable Result
deriving instance Typeable LeanCheckTests
#endif

newtype Results = Results [([String],Bool)]

data Result = OK        Int
            | Falsified Int [String]
            | Exception Int [String] String
  deriving (Eq, Show)

-- | Create a 'Test' for a LeanCheck 'Testable' property.
-- Example:
--
-- > LC.testProperty "sort is idempotent" $ \xs -> sort (sort xs :: [Int]) == sort xs
testProperty :: Testable a => TestName -> a -> TestTree
testProperty name = singleTest name . Results . results

-- | Number of test cases for LeanCheck to generate.
newtype LeanCheckTests = LeanCheckTests Int
  deriving (Show, Eq, Ord)

instance IsOption LeanCheckTests where
  defaultValue = LeanCheckTests 200
  parseValue = fmap LeanCheckTests . safeRead
  optionName = return "leancheck-tests"
  optionHelp = return "Depth to use for leancheck tests"

instance IsTest Results where
  testOptions = return [Option (Proxy :: Proxy LeanCheckTests)]
  run opts results _ = resultIO m results >>= \r -> pure $
    case r of
    OK n             -> testPassed $ "+++ OK, passed " ++ show n ++ " tests"
                                  ++ takeWhile (\_ -> n < m) " (exhausted)"
                                  ++ "."
    Falsified i ce   -> testFailed $ "*** Failed! Falsifiable (after "
                                  ++ show i ++ " tests):\n" ++ joinArgs ce
    Exception i ce e -> testFailed $ "*** Failed! Exception '" ++ e
                                  ++ "' (after " ++ show i ++ " tests):\n"
                                  ++ joinArgs ce
    where
    LeanCheckTests m = lookupOption opts

resultsIO :: Int -> Results -> [IO Result]
resultsIO n (Results results) = zipWith torio [1..] $ take n results
  where
    tor i (_,True) = OK i
    tor i (as,False) = Falsified i as
    torio i r@(as,_) = evaluate (tor i r)
       `catch` \e -> let _ = e :: SomeException
                     in return (Exception i as (show e))

resultIO :: Int -> Results -> IO Result
resultIO n = computeResult . resultsIO n
  where
  computeResult []  = error "resultIO: no results, empty Listable enumeration?"
  computeResult [r] = r
  computeResult (r:rs) = r >>= \r -> case r of
                                     (OK _) -> computeResult rs
                                     _      -> return r

-- joins the counter-example arguments
joinArgs :: [String] -> String
joinArgs ce | any ('\n' `elem`) ce = unlines $ map chopBreak ce
            | otherwise            = unwords ce

-- chops a line break at the end if there is any
chopBreak :: String -> String
chopBreak [] = []
chopBreak ['\n'] = []
chopBreak (x:xs) = x:chopBreak xs
