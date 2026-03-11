-------------------------------------------------------------------------------
-- network-wait
-- Copyright 2022 Michael B. Gale (github@michael-gale.co.uk)
-------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad.Catch
import Control.Retry
-- Only needed for base < 4.11, redundant otherwise
import Data.Semigroup

import Database.PostgreSQL.Simple

import Test.Tasty
import Test.Tasty.HUnit

import Network.Wait.PostgreSQL

-------------------------------------------------------------------------------

-- | Essentially the same as `retryPolicyDefault`, but here for compatibility
-- with older versions of `retry`.
testRetryPolicy :: Monad m => RetryPolicyM m
testRetryPolicy = constantDelay 50000 <> limitRetries 5

tests :: TestTree
tests = testGroup "Network.Wait.PostgreSQL"
    [ testCase "Can't connect to server that doesn't exist" $ do
        res <- try @IO @SomeException $
            waitPostgreSql testRetryPolicy defaultConnectInfo{
                connectHost = "doesnotexist"
            }

        case res of
            Left _ -> pure ()
            Right _ -> assertFailure "`waitPostgreSql` did not fail"
    , testCase "Can connect to server that does exist" $ do
            let policy = limitRetries 5 <> exponentialBackoff (2*1000*1000)
            res <- try @IO @SomeException $
                waitPostgreSqlVerbose putStrLn policy defaultConnectInfo

            case res of
                Left ex -> assertFailure $
                    "`waitPostgreSql` caused an exception: " <> show ex
                Right _ -> pure ()
    ]

-- | `main` is the entry point to this test suite.
main :: IO ()
main = defaultMain tests

-------------------------------------------------------------------------------
