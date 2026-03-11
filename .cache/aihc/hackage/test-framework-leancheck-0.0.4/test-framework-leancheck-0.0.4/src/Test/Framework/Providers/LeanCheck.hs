{-# LANGUAGE MultiParamTypeClasses, CPP #-}
#if __GLASGOW_HASKELL__ == 708
{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
#endif
-- |
-- Module      : Test.Framework.Providers.LeanCheck
-- Copyright   : (c) 2018 Rudy Matela
-- License     : 3-Clause BSD  (see the file LICENSE)
-- Maintainer  : Rudy Matela <rudy@matela.com.br>
--
-- LeanCheck support for test-framework (Test.Framework).
--
-- Here's how your @test.hs@ might look like:
--
-- > import Test.Framework
-- > import Test.Framework.Providers.LeanCheck as LC
-- > import Data.List
-- >
-- > main :: IO ()
-- > main = defaultMain tests
-- >
-- > tests :: [Test]
-- > tests =
-- >   [ LC.testProperty "sort . sort == sort"
-- >       $ \xs -> sort (sort xs :: [Int]) == sort xs
-- >   , LC.testProperty "sort == id" -- not really, should fail
-- >       $ \xs -> sort (xs :: [Int]) == xs
-- >   ]
--
-- The output for the above program is:
--
-- > ./eg/test
-- > sort . sort == sort: [OK, passed 100 tests.]
-- > sort == id: [Failed]
-- > *** Failed! Falsifiable (after 7 tests):
-- > [1,0]
-- >
-- >          Properties  Total
-- >  Passed  1           1
-- >  Failed  1           1
-- >  Total   2           2
--
-- Use @-a@ or @--maximum-generated-tests@ to configure
-- the maximum number of tests for each property.
--
-- > $ ./eg/test -a5
-- > sort . sort == sort: [OK, passed 5 tests.]
-- > sort == id: [OK, passed 5 tests.]
-- >
-- >          Properties  Total      
-- >  Passed  2           2          
-- >  Failed  0           0          
-- >  Total   2           2          
--
-- Since LeanCheck is enumerative,
-- you may want to increase the default number of tests (100).
-- Arbitrary rule of thumb:
--
-- * between 200 to 500 on a developer machine;
-- * between 1000 and 5000 on the CI.
--
-- Your mileage may vary.
--
-- Please see the documentation of
-- "Test.LeanCheck" and "Test.Framework.Providers"
-- for more details.
module Test.Framework.Providers.LeanCheck
  ( testProperty
  )
where

import Test.Framework.Providers.API
import Test.LeanCheck
import Control.Exception (SomeException, catch, evaluate)
#if __GLASGOW_HASKELL__ == 708
import Data.Typeable (Typeable)
#endif

-- | List of test results for a given property
newtype Results = Results [([String],Bool)]

#if __GLASGOW_HASKELL__ == 708
deriving instance Typeable Results
#endif

-- | The ultimate test result for a given property
data Result = OK        Int
            | Falsified Int [String]
            | Exception Int [String] String
  deriving Eq

instance Show Result where
  show (OK n)              =  "OK, passed " ++ show n ++ " tests."
  show (Falsified i ce)    =  "*** Failed! Falsifiable (after "
                           ++ show i ++ " tests):\n" ++ joinArgs ce
  show (Exception i ce e)  =  "*** Failed! Exception '" ++ e
                           ++ "' (after " ++ show i ++ " tests):\n"
                           ++ joinArgs ce

-- | Given a 'Testable' property, returns a test-framework test.
--   For example, place the following in a 'TestGroup' list:
--
-- > testProperty "sort . sort == sort" $
-- >   \xs -> sort (sort xs :: [Int]) == sort xs
--
-- You may want to import this module qualified and use @LC.TestProperty@
-- if mixing "Test.LeanCheck" tests
-- with those of other property testing libraries.
testProperty :: Testable a => TestName -> a -> Test
testProperty name = Test name . Results . results

-- | The current version does not 'runImprovingIO'.
--   So progress is only seen between properties, but not within properties.
instance Testlike Int Result Results where
  runTest topts results = do
    let m = unK $ topt_maximum_generated_tests topts
    result <- resultIO m results
    return (Finished result, return ())
  testTypeName _ = "Properties"

instance TestResultlike Int Result where
  testSucceeded (OK _)  =  True
  testSucceeded _       =  False

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
